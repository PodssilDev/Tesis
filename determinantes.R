# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 7: # ANALISIS DE DETERMINANTES USANDO RANDOM 
# FOREST
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(purrr)
library(randomForest)
library(caret)
library(Metrics)
library(ggplot2)

# ===================================================
# PREPARACIÓN DE DATOS
# ===================================================

# Se seleccionan solo las columnas mínimas necesarias de los datos procesados:
#   - idEstablecimiento: identificador único del hospital
#   - eff_global: eficiencia global calculada anteriormente
# Luego se asegura que cada combinación idEstablecimiento-eff_global sea única.
datos_min <- map(
  datos_procesados,                           
  ~ .x %>%                                  
    select(idEstablecimiento = IdEstablecimiento,
           eff_global) %>%
    distinct()                             
)

# ===================================================
# RANDOM FOREST
# ===================================================

# ---------------------------------------------------
# 2) FUNCIÓN PRINCIPAL PARA RANDOM FOREST POR AÑO
# ---------------------------------------------------
analize_rf <- function(year, resultados_in, n_top, tipo){
  print("---------------------------")
  print(paste0("AÑO: ", year))
  
  # 2.1 Leer datos consolidados originales
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  colnames(datos_consolidados) <- gsub("\\.0$", "", colnames(datos_consolidados))  # Limpieza nombres
  
  # 2.2 Convertir todas las columnas a enteros
  df <- datos_consolidados
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # 2.3 Filtrar solo hospitales que tienen resultados de eficiencia
  df_vrs <- resultados_in[[as.character(year)]]
  df_w_vrs <- df %>%
    filter(idEstablecimiento %in% df_vrs$idEstablecimiento)
  
  # 2.4 Combinar datos de entrada con eficiencia global (left join)
  df_merged_original <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)
  
  # Elimina columnas con NA (mantiene solo variables con datos completos)
  df_merged_clean <- df_merged_original[, colSums(is.na(df_merged_original)) == 0]
  
  # 2.5 Calcular correlaciones con la variable objetivo (tipo, por defecto eff_global)
  correlaciones <- cor(df_merged_clean[,-1])[tipo, ]        # correlación de cada variable con la eficiencia
  correlaciones <- correlaciones[!names(correlaciones) %in% tipo] # elimina la autocorrelación
  correlaciones_ordenadas <- sort(abs(correlaciones), decreasing = TRUE) # ordenar por |correlación|
  
  # Seleccionar las n_top variables más correlacionadas
  top_correlacion <- head(correlaciones_ordenadas, n=n_top)
  top_variables <- names(top_correlacion)
  
  # Variables a incluir en el modelo: eficiencia global + top correlacionadas
  columnas_a_incluir <- c(tipo, top_variables)
  df_top <- df_merged_clean[, columnas_a_incluir]
  
  # 2.6 Preparar conjunto de entrenamiento y prueba
  set.seed(123)  # Para reproducibilidad
  trainIndex <- createDataPartition(df_top[[tipo]], p = 0.7, list = FALSE)
  trainData <- df_top[trainIndex, ]
  testData  <- df_top[-trainIndex, ]
  
  # 2.7 Configurar validación cruzada
  control <- trainControl(method = "cv", number = 10)  # 10-fold CV
  formula <- as.formula(paste(tipo, "~ ."))
  
  # 2.8 Ajustar el modelo Random Forest
  modelo_rf <- randomForest(formula, 
                            data = trainData, 
                            importance = TRUE, 
                            trControl = control, 
                            ntree = 700,
                            do.trace = 100 )
  
  # 2.9 Predicciones y métricas de error
  predicciones <- predict(modelo_rf, newdata = testData)
  r2 <- R2(predicciones, testData[[tipo]])
  rmse <- rmse(predicciones, testData[[tipo]])
  cat("R²:", r2, "\nRMSE:", rmse)
  
  # 2.10 Importancia de variables
  importancia <- importance(modelo_rf)  # métricas como IncMSE y NodePurity
  
  print("---------------------------")
  print("---------------------------")
  
  # 2.11 Retornar resultados
  return(list(importancia = importancia,
              modelo = modelo_rf,
              correlaciones = top_correlacion))
}

# ---------------------------------------------------
# 3) APLICAR RANDOM FOREST A TODOS LOS AÑOS
# ---------------------------------------------------
random_forest <- lapply(anios, function(anio) {
  analize_rf(anio, resultados_in = datos_min, 500, "eff_global")
})
names(random_forest) <- paste0(anios)  # nombrar la lista por año


# ===================================================
# PROCESAR IMPORTANCIAS POR AÑO
# ===================================================

# Función auxiliar: calcula cuántos años tiene datos y su mediana por fila
calcular_estadisticas <- function(df) {
  df$Frecuencia <- rowSums(!is.na(df[,-1]))  # cuenta número de años con datos
  df$Mediana <- apply(df[, -c(1, ncol(df))], 1, median, na.rm = TRUE)  # mediana por fila
  return(df)
}

# Función auxiliar: une dataframes de distintos años con Variable como índice
crear_dataframe <- function(lista_metrica) {
  df <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), lista_metrica)
  colnames(df)[-1] <- names(lista_metrica)  # renombrar columnas con los años
  return(df)
}


# Procesa el resultado de Random Forest por año:
#  - Combina importancia y correlación
#  - Obtiene top 50 y top 10 por año
#  - Crea dataframes anuales consolidados
importancia_dataframe <- function(random_forest) {
  
  # Listas de resultados por año
  lista_incmse <- list()
  lista_incmse_10 <- list()
  lista_corr <- list()
  lista_todos <- list()
  lista_top50_incmse <- list()
  
  # Procesar para cada año
  for (anio in 2014:2023) {
    
    # 1. Importancia (IncMSE) del Random Forest
    importancia <- data.frame(
      Variable = rownames(random_forest[[as.character(anio)]][["importancia"]]), 
      IncMSE = random_forest[[as.character(anio)]][["importancia"]][,1]
    )
    
    # 2. Correlaciones asociadas a esas variables
    corr <- as.data.frame(random_forest[[as.character(anio)]][["correlaciones"]])
    corr$Variable <- rownames(corr)
    corr$Corr <- corr[,1]
    corr <- corr[, c("Variable", "Corr")]
    rownames(corr) <- NULL
    
    # 3. Combinar importancia y correlación en un solo dataframe
    df_final <- merge(importancia, corr, by = "Variable")
    
    # 4. Filtrar Top 50 y Top 10 por IncMSE
    df_top50 <- df_final[order(-df_final$IncMSE), ][1:50, ]
    df_top10 <- df_final[order(-df_final$IncMSE), ][1:10, ]
    
    # Guardar en listas para uso posterior
    lista_incmse[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE)
    lista_incmse_10[[as.character(anio)]] <- data.frame(Variable = df_top10$Variable, IncMSE = df_top10$IncMSE)
    lista_corr[[as.character(anio)]] <- data.frame(Variable = df_top10$Variable, Corr = df_top10$Corr)
    lista_todos[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE, Corr = df_top50$Corr)
    
    # Etiqueta del año para la lista de top 50
    df_top50$Año <- anio
    lista_top50_incmse[[as.character(anio)]] <- df_top50
  }
  
  # Combinar resultados en un solo dataframe por métrica
  df_incmse <- crear_dataframe(lista_incmse)
  df_incmse_10  <- crear_dataframe(lista_incmse_10)
  df_corr <- crear_dataframe(lista_corr)
  df_todos <- crear_dataframe(lista_todos)
  
  # Devolver lista con todos los resultados
  return(list(
    top_50 = lista_top50_incmse,
    df_incmse = df_incmse,
    df_incmse_10 = df_incmse_10,
    df_incmse_est = calcular_estadisticas(df_incmse),
    df_incmse_est_10 = calcular_estadisticas(df_incmse_10),
    df_corr = df_corr,
    df_todos = df_todos
  ))
}

# ---------------------------------------------------
# 4) OBTENER TABLAS FINALES DE IMPORTANCIA
# ---------------------------------------------------
resultados_importancia <- importancia_dataframe(random_forest)



# ===================================================
# BARPLOT: Variables más frecuentes en el Top 50
# ===================================================

# 1) Combinar todas las variables top 50 de todos los años
top50_todos <- bind_rows(resultados_importancia$top_50)

# 2) Contar cuántas veces aparece cada variable en el top 50 (todos los años)
frecuencia_vars <- top50_todos %>%
  group_by(Variable) %>%
  summarise(Frecuencia = n(),
            Mediana_IncMSE = median(IncMSE, na.rm = TRUE)) %>%
  arrange(desc(Frecuencia))

# 3) Seleccionar las 10 variables más frecuentes
top10_vars <- frecuencia_vars %>% slice_max(Frecuencia, n = 10)

# 4) Ordenar factores para el gráfico
top10_vars$Variable <- factor(top10_vars$Variable, levels = top10_vars$Variable[order(top10_vars$Frecuencia)])

# 5) Generar barplot
ggplot(top10_vars, aes(x = Variable, y = Frecuencia, fill = Mediana_IncMSE)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variables más frecuentes en el Top 50 (2014–2023)",
       subtitle = "Frecuencia de aparición y mediana de importancia IncMSE",
       x = "Variable", y = "Frecuencia en el Top 50",
       fill = "Mediana IncMSE") +
  theme_minimal(base_size = 13) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")
