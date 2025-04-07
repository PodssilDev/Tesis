#source("functions.R")
#source("graphics.R")

library(frontier)
library(dplyr)
library(openxlsx)
library(readxl)
library(scatterplot3d)
library(rgl)
library(tidyr)
library(purrr)
library(randomForest)
library(caret)
library(Metrics)
library(RColorBrewer)

# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================
consolidar_datos_por_anio <- function(anio) {
  
  
  # Definir rutas de archivos utilizando el año como variable
  path_hospitales <- paste0("data/", anio, "/", anio, "_hospitals.csv")
  path_hospitales_complejidades <- paste0("data/hospitales.csv")
  path_predicciones_grd <- paste0("data/", anio, "/", anio, "_prediciones_grd.txt")
  path_datos_consolidados <- paste0("data/", anio, "/", anio, "_consolidated_data.csv")
  path_financiero <- paste0("data/", anio, "/", anio, "_financial_data.csv")
  path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2023.xlsx"
  path_consultas <- paste0("data/", anio, "/variables/", anio, "_consultas.txt")
  path_quirofano <- paste0("data/", anio, "/variables/", anio, "_quirofano.txt")
  #browser()
  # Cargar datos
  hospitales <- read.csv(path_hospitales) %>% rename("IdEstablecimiento" = "hospital_id")
  
  hospitales_complejidades <- read.csv(path_hospitales_complejidades) %>% rename("IdEstablecimiento" = "hospital_id")
  
  hospitales <- hospitales %>%
    left_join(hospitales_complejidades %>% select(IdEstablecimiento, complejidad), 
              by = "IdEstablecimiento")
  
  
  predicciones_grd <- read.csv(path_predicciones_grd, sep=",")
  datos_consolidados <- read.table(path_datos_consolidados, sep=";", header=TRUE)
  financiero <- read.csv(path_financiero) %>% 
    select(hospital_id, X21_value, X22_value) %>% rename("IdEstablecimiento" = "hospital_id")
  financiero$X21_value <- as.numeric(financiero$X21_value)
  financiero$X22_value <- as.numeric(financiero$X22_value)
  
  financiero <- financiero[rowSums(is.na(financiero)) < 2, ]
  
  
  estadisticas <- read_excel(path_estadisticas, sheet = (anio - 2014) + 1, skip = 1)  %>% 
    rename("IdEstablecimiento" = "Cód. Estab.", "Region" = "Nombre SS/SEREMI") %>%
    filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
    select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
    semi_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(1:5)
  
  # Procesar estadísticas
  dias_cama_disponibles <- estadisticas %>% 
    filter(Glosa == "Dias Cama Disponibles") %>%  
    select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)
  
  egresos <- estadisticas %>% 
    filter(Glosa == "Numero de Egresos") %>%  
    select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa)
  
  
  consultas <- unlist(strsplit(readLines(path_consultas), ","))
  # Seleccionar y convertir columnas válidas
  columnas_validas <- intersect(unlist(consultas), colnames(datos_consolidados))
  
  consultas_data <- subset(datos_consolidados, select = columnas_validas)
  
  # Identificar columnas tipo character
  cols_char <- sapply(consultas_data, is.character)
  
  # Convertir columnas character a numeric
  consultas_data[, cols_char] <- lapply(consultas_data[, cols_char], function(x) as.numeric(x))
  
  # Crear suma total de consultas
  consultas_data$sumaTotal <- rowSums(consultas_data[, -which(names(consultas_data) == "idEstablecimiento")], na.rm = TRUE)
  
  consultas <- data.frame(idEstablecimiento = consultas_data$idEstablecimiento, 
                          Consultas = consultas_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Consultas)
  
  
  quirofano <- unlist(strsplit(readLines(path_quirofano), ","))
  
  columnas_validas_q <- intersect(unlist(quirofano), colnames(datos_consolidados))
  
  quirofano_data <- subset(datos_consolidados, select = unlist(columnas_validas_q))
  
  
  # Reemplazar comas por puntos y convertir a numérico
  quirofano_data <- quirofano_data %>%
    mutate(across(-idEstablecimiento, ~ as.integer(floor(as.numeric(gsub(",", ".", .))))))
  
  # Crear suma total de quirofano
  quirofano_data$sumaTotal <- rowSums(select(quirofano_data, -idEstablecimiento), na.rm = TRUE)
  
  quirofano <- data.frame(idEstablecimiento = quirofano_data$idEstablecimiento, 
                          Quirofano = quirofano_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Quirofano)
  
  # Procesar egresos y predicciones GRD
  intermediate_df <- egresos %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    mutate(Egresos.GRD = Prediction * egresos) %>%
    select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
  
  # Combinar datos financieros y días cama disponibles
  input <- left_join(financiero, dias_cama_disponibles %>% 
                       select(IdEstablecimiento, dias_cama_disponible), by = "IdEstablecimiento")
  
  # Combinar todas las salidas
  output <- intermediate_df %>%
    left_join(consultas, by = "IdEstablecimiento") %>%
    left_join(quirofano, by = "IdEstablecimiento")
  
  # Consolidar todos los datos
  all <- inner_join(output, input, by = "IdEstablecimiento") %>%
    left_join(hospitales %>% select(IdEstablecimiento, region_id, latitud, longitud,complejidad), by = "IdEstablecimiento") %>%
    relocate(region_id, .after = Region)
  
  all_sin_duplicados <- distinct(all)
  return(all_sin_duplicados)
}

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023

datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])

# Hasta aca tenemos 178 hospitales con las variables de entrada y salida necesarias

# ==============================================
#  MODELOS SFA PARA TODOS LOS AÑOS
# ==============================================

procesar_sfa <- function(df) {
  # ---- Modelo Egresos ----
  mod_egresos <- sfa(
    formula = log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    data    = df
  )
  eff_egresos <- efficiencies(mod_egresos)
  
  # ---- Modelo Consultas ----
  mod_consultas <- sfa(
    formula = log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    data    = df
  )
  eff_consultas <- efficiencies(mod_consultas)
  
  # ---- Modelo Quirofano ----
  mod_quirofano <- sfa(
    formula = log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    data    = df
  )
  eff_quirofano <- efficiencies(mod_quirofano)
  
  # Agregamos esas columnas al data frame
  df_nuevo <- df %>%
    mutate(
      eff_egresos   = eff_egresos,
      eff_consultas = eff_consultas,
      eff_quirofano = eff_quirofano,
      dist_ideal = sqrt(
        (1 - eff_egresos)^2 +
          (1 - eff_consultas)^2 +
          (1 - eff_quirofano)^2
      ),
      eff_global = 1 - dist_ideal / sqrt(3)
    )
  
  return(df_nuevo)
}

# Consultar: Calcular residuals y ruido?
# datos_procesados tienen los resultados de eficiencia por año
datos_procesados <- lapply(datos, procesar_sfa)

# ===================================================
# PASAR RESULTADOS A EXCEL
# ===================================================

df_ref <- datos_procesados[["2014"]] %>%
  select(IdEstablecimiento, NombreHospital = `Nombre Establecimiento`) %>%
  distinct() 


df_long <- bind_rows(
  lapply(names(datos_procesados), function(year_name) {
    df_year <- datos_procesados[[year_name]]
    
    df_year %>%
      select(IdEstablecimiento, eff_global) %>%
      mutate(Anio = year_name)
  })
)


df_long <- df_long %>%
  left_join(df_ref, by = "IdEstablecimiento")

df_wide <- df_long %>%
  pivot_wider(
    id_cols      = c(IdEstablecimiento, NombreHospital),
    names_from   = Anio,
    values_from  = eff_global,
    names_prefix = "Eficiencia_"
  )

df_wide <- df_wide %>%
  mutate(
    across(
      starts_with("Eficiencia_"),
      ~ round(.x, 3)
    )
  )

# Exportar a Excel
wb <- createWorkbook()
addWorksheet(wb, "Eficiencias")
writeData(wb, "Eficiencias", df_wide, rowNames = FALSE)
saveWorkbook(wb, "Resultados_eficiencias.xlsx", overwrite = TRUE)

##########################################################

# ===================================================
# ANALISIS DE DETERMINANTES
# ===================================================

datos_min <- map(
  datos_procesados,                           
  ~ .x %>%                                  
    select(idEstablecimiento = IdEstablecimiento,
           eff_global) %>%
    distinct()                             
)


# =======================
#  RANDOM FOREST
# =======================

analize_rf <- function(year, resultados_in, n_top, tipo){
  #year <- 2014
  print("---------------------------")
  print(paste0("AÑO: ", year))
  
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  
  #print(data_path)
  # Leer los datos consolidados
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  colnames(datos_consolidados) <- gsub("\\.0$", "", colnames(datos_consolidados))
  
  df <- datos_consolidados
  
  
  # Convertir columnas a enteros
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # Filtrar los resultados de VRS
  df_vrs <- resultados_in[[as.character(year)]]
  
  
  df_w_vrs <- df %>%
    filter(idEstablecimiento %in% df_vrs$idEstablecimiento)
  
  # Combinar los DataFrames
  df_merged_original <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)
  df_merged_clean <- df_merged_original[, colSums(is.na(df_merged_original)) == 0]
  
  
  correlaciones <- cor(df_merged_clean[,-1])[tipo, ]
  correlaciones <- correlaciones[!names(correlaciones) %in% tipo]
  correlaciones_ordenadas <- sort(abs(correlaciones), decreasing = TRUE)
  
  top_correlacion <- head(correlaciones_ordenadas, n=n_top)
  top_variables <- names(top_correlacion)
  
  
  columnas_a_incluir <- c(tipo, top_variables)
  
  # Crear el DataFrame con las variables seleccionadas
  df_top <- df_merged_clean[, columnas_a_incluir]
  
  
  # PROBANDO RANDOM FOREST
  
  set.seed(123)  # Para reproducibilidad
  
  trainIndex <- createDataPartition(df_top[[tipo]], p = 0.7, list = FALSE)
  
  trainData <- df_top[trainIndex, ]
  testData <- df_top[-trainIndex, ]
  
  control <- trainControl(method = "cv", number = 10)  # 10-fold CV
  formula <- as.formula(paste(tipo, "~ ."))
  
  # Ajustar el modelo de Random Forest
  modelo_rf <- randomForest(formula, 
                            data = trainData, 
                            importance = TRUE, 
                            trControl = control, 
                            ntree = 700,
                            do.trace = 100 )
  
  
  # Predicciones en el conjunto de prueba
  predicciones <- predict(modelo_rf, newdata = testData)
  
  # Evaluar el rendimiento
  r2 <- R2(predicciones, testData[[tipo]])
  rmse <- rmse(predicciones, testData[[tipo]])
  cat("R²:", r2, "\nRMSE:", rmse)
  
  # Importancia de las variables
  importancia <- importance(modelo_rf)
  
  
  print("---------------------------")
  print("---------------------------")
  
  return(list(importancia = importancia,
              modelo = modelo_rf,
              correlaciones = top_correlacion))
  print("Completado!")
}

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = datos_min, 500, "eff_global")})
# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)

# ===============================
#  EXTRACCIÓN DE VARIABLES POR AÑO
# ===============================

# Función para calcular frecuencia y mediana
calcular_estadisticas <- function(df) {
  df$Frecuencia <- rowSums(!is.na(df[,-1]))  # Cuenta cuántos años tiene datos
  df$Mediana <- apply(df[, -c(1, ncol(df))], 1, median, na.rm = TRUE)  # Mediana ignorando NA
  return(df)
}

# Función para generar el dataframe con años como columnas
crear_dataframe <- function(lista_metrica) {
  df <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), lista_metrica)
  colnames(df)[-1] <- names(lista_metrica)  # Renombrar columnas con los años
  return(df)
}

importancia_dataframe <- function(random_forest) {
  
  # Crear listas para almacenar cada métrica por año
  lista_incmse <- list()
  lista_incmse_10 <- list()
  lista_incnp <- list()
  lista_corr <- list()
  lista_todos <- list()
  lista_top50_incmse <- list()
  
  # Iterar sobre los años del 2014 al 2023
  for (anio in 2014:2023) {
    
    # Extraer importancia y correlaciones
    importancia <- data.frame(
      Variable = rownames(random_forest[[as.character(anio)]][["importancia"]]), 
      IncMSE = random_forest[[as.character(anio)]][["importancia"]][,1]
    )
    
    # Extraer correlaciones
    corr <- as.data.frame(random_forest[[as.character(anio)]][["correlaciones"]])
    
    # Agregar nombres de fila como columna y limpiar formato
    corr$Variable <- rownames(corr)
    corr$Corr <- corr[,1]
    corr <- corr[, c("Variable", "Corr")]
    rownames(corr) <- NULL
    
    # Unir las tablas por la columna 'Variable'
    df_final <- merge(importancia, corr, by = "Variable")
    
    df_top50 <- df_final[order(-df_final$IncMSE), ][1:50, ]
    df_top10 <- df_final[order(-df_final$IncMSE), ][1:10, ]
    
    # Guardar cada métrica en listas separadas con Variable como índice
    lista_incmse[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE)
    lista_incmse_10[[as.character(anio)]] <- data.frame(Variable = df_top10$Variable, IncMSE = df_top10$IncMSE)
    lista_corr[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, Corr = df_top50$Corr)
    lista_todos[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE, Corr = df_top50$Corr)
    
    # Filtrar las 50 variables con mayor IncMSE en ese año
    df_top50$Año <- anio
    
    # Guardar en la lista de Top 50
    lista_top50_incmse[[as.character(anio)]] <- df_top50
    
  }
  
  
  
  # Crear dataframes con años como columnas
  df_incmse <- crear_dataframe(lista_incmse)
  df_incmse_10  <- crear_dataframe(lista_incmse_10)
  
  return(list(top_50 = lista_top50_incmse,
              df_incmse = df_incmse,
              df_incmse_10 = df_incmse_10,
              df_incmse_est = calcular_estadisticas(df_incmse),
              df_incmse_est_10 = calcular_estadisticas(df_incmse_10) ))
}

resultados_importancia <- importancia_dataframe(random_forest)

# =======================
#  DETERMINANTES A EXCEL
# =======================

wb <- createWorkbook()

addWorksheet(wb, "Determinantes")
writeData(
  wb,
  sheet = "Determinantes",
  x = resultados_importancia[["df_incmse_est_10"]],
  startRow = 1,
  startCol = 1,
  headerStyle = createStyle(textDecoration = "bold")
)

setColWidths(wb, sheet = "Determinantes", cols = 1:50, widths = "auto")


# =================================
#  DETERMINANTES A TABLA DE VALORES
# =================================

saveWorkbook(wb, "Determinantes_eficiencia.xlsx", overwrite = TRUE)

df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_long_all_comp <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")

grafica <- ggplot(df_long_all_comp, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_bw() +  # Cambiar a theme_bw() para fondo blanco
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_all_comp$Valor, na.rm = TRUE)  # Escala de valores automática
  ) +
  theme(
    panel.background = element_blank(),  # Fondo del panel en blanco
    plot.background = element_blank(),   # Fondo del gráfico en blanco
    panel.grid.major = element_blank(),  # Eliminar líneas de la cuadrícula principales
    panel.grid.minor = element_blank()   # Eliminar líneas de la cuadrícula secundarias
  )


ggsave(paste0("determinantes.jpg"), plot = grafica, width = 15, height = 20, dpi = 500)

# ==============================================
#  AÑO 2014 (DESDE ACA HASTA ABAJO, PRUEBAS)
# ==============================================
d2014 = datos[["2014"]]

# ================
#  EGRESOS (178 eficiencias)
# ================

# 1. Estimación
mod_egresos <- sfa(
  formula = log(Egresos.GRD) ~ log(dias_cama_disponible) + log(X21_value) + log(X22_value),
  data    = d2014
)

# 2. Resumen de resultados
summary(mod_egresos)

# 3. Extraer eficiencia
eff_egresos <- efficiencies(mod_egresos)   # vector con 178 valores, uno por hospital
head(eff_egresos)

# ================
#  CONSULTAS (167 eficiencias)
# ================

mod_consultas <- sfa(
  formula = log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
  data    = d2014
)
summary(mod_consultas)

eff_consultas <- efficiencies(mod_consultas)
head(eff_consultas)

# ================
#  QUIROFANOS (154 eficiencias)
# ================

mod_quirofano <- sfa(
  formula = log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
  data    = d2014
)
summary(mod_quirofano)

eff_quirofano <- efficiencies(mod_quirofano)
head(eff_quirofano)

# ------------------------------
d2014$eff_egresos   <- eff_egresos
d2014$eff_consultas <- eff_consultas
d2014$eff_quirofano <- eff_quirofano

# Puedes inspeccionar
head(d2014)



# Idea de Manuel: Distancia -> (1,1,1) punto ideal

d2014 <- d2014 %>%
  mutate(
    dist_ideal = sqrt(
      (1 - eff_egresos)^2 +
        (1 - eff_consultas)^2 +
        (1 - eff_quirofano)^2
    ),
    eff_global = 1 - dist_ideal / sqrt(3)
  )

#scatterplot3d(
#  x = d2014$eff_egresos,
#  y = d2014$eff_consultas,
#  z = d2014$eff_quirofano,
#  pch = 16,
#  main = "Eficiencias (por variable) año 2014"
#)


#resid_sfa <- residuals(mod_quirofano)

# Ineficiencia estimada (u_i)
#ineff_sfa <- -log(eff_quirofano)

# Termino de ruido estimado: v_i = e_i + u_i
#noise_sfa <- resid_sfa + ineff_sfa