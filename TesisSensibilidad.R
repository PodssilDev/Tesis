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
library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)
library(gridExtra)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(sf)

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
      eff_global = round(1 - dist_ideal / sqrt(3),3)
    )
  
  return(df_nuevo)
}

# Consultar: Calcular residuals y ruido?
# datos_procesados tienen los resultados de eficiencia por año
datos_procesados <- lapply(datos, procesar_sfa)



es_outlier <- function(x) {
  x <- as.vector(x)                       # evita el warning de dplyr
  q  <- quantile(x, c(.25, .75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  low  <- q[1] - 1.5 * iqr
  high <- q[2] + 1.5 * iqr
  x < low | x > high
}

df_long <- bind_rows(
  imap(datos_procesados, ~
         mutate(.x,
                Año = .y,
                idEstablecimiento = IdEstablecimiento) %>%
         select(idEstablecimiento, Año, eff_global))
)

df_mean <- df_long %>%
  group_by(idEstablecimiento) %>%
  summarise(mean_eff = mean(eff_global, na.rm = TRUE), .groups = "drop")


# ▸ Vector con todos los IDs que alguna vez son outlier
outlier_ids <- map_dfr(datos_procesados, ~
                         .x %>%
                         mutate(outlier = es_outlier(eff_global)) %>%
                         filter(outlier) %>%
                         select(IdEstablecimiento)
) %>% 
  distinct() %>% 
  pull(IdEstablecimiento)

df_mean <- df_mean %>%
  mutate(outlier = es_outlier(mean_eff))

ids_fuera <- df_mean %>%
  filter(outlier) %>%
  pull(idEstablecimiento)


datos_tag <- map(datos_procesados, ~
                   .x %>%
                   mutate(outlier = es_outlier(eff_global))
)

df_plot <- bind_rows(
  imap(datos_tag, ~ mutate(.x, Año = .y))
)

ggplot(df_plot, aes(x = factor(Año), y = eff_global)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Eff_global – boxplot por año",
       x = "Año", y = "Eficiencia global")

sens_resumen <- map_dfr(datos_tag, ~
                          .x %>%
                          summarise(
                            Año          = unique(anios),                       # si ya existía
                            n_total      = n(),
                            n_outliers   = sum(outlier),
                            media_full   = mean(eff_global, na.rm = TRUE),
                            media_sinOL  = mean(eff_global[!outlier], na.rm = TRUE),
                            mediana_full = median(eff_global, na.rm = TRUE),
                            mediana_sinOL= median(eff_global[!outlier], na.rm = TRUE),
                            sd_full      = sd(eff_global, na.rm = TRUE),
                            sd_sinOL     = sd(eff_global[!outlier], na.rm = TRUE)
                          )
)

print(sens_resumen)

datos_limpios <- map(datos_tag, ~ filter(.x, !outlier))
# mantiene mismas columnas pero excluye filas outlier




##################################################
##################################################
##################################################
##################################################
##################################################

sensibilidad_parametro_general <- function(data, data_original, mayor, valor) {
  # Determinar la columna a trabajar (vrs o crs)
  #browser()
  # Filtrar los datos en función del parámetro `mayor` y el valor dado
  if (mayor) {
    data_filtrada <- subset(data_original, data_original[["eff_global"]] > valor)
  } else {
    data_filtrada <- subset(data_original, data_original[["eff_global"]] < valor)
  }
  
  # Filtrar el dataset por IdEstablecimiento
  data_set <- data[data$IdEstablecimiento %in% data_filtrada$IdEstablecimiento, ]
  
  # Aplicar el análisis DEA
  resultados_in  <- procesar_sfa(data_set)
  
  return (resultados = resultados_in)
}

aplicar_sensibilidad <- function(datos, resultados, umbral, mayor) {
  mapply(function(data, resultado, anio) {
    # Mostrar el nombre del año en pantalla
    print(paste("Aplicando sensibilidad para el año:", anio))
    #browser()
    # Ejecutar la función principal
    sensibilidad_parametro_general(data, resultado, mayor, umbral)
  },
  datos, resultados, names(datos), SIMPLIFY = FALSE) # Pasar los nombres de los datos como argumento
}


combinar_resultados_iteraciones <- function(resultados_in, resultados_in_2, resultados_in_3) {
  #browser()
  # Crear una lista de dataframes, uno por cada año, con valores de VRS y CRS
  lista_resultados_combinados <- lapply(unique(names(resultados_in)), function(anio) {
    # Seleccionar los datos de las iteraciones de VRS
    df <- resultados_in[[anio]] %>%
      select(IdEstablecimiento, "eff_global") %>%
      rename(iteracion_1 = "eff_global" )
    
    df_2 <- resultados_in_2[[anio]] %>%
      select(IdEstablecimiento, "eff_global") %>%
      rename(iteracion_2 = "eff_global")
    
    df_3 <- resultados_in_3[[anio]] %>%
      select(IdEstablecimiento, "eff_global") %>%
      rename(iteracion_3 = "eff_global")
    
    # Unir los dataframes de VRS por IdEstablecimiento
    df_combinado <- df %>%
      full_join(df_2, by = "IdEstablecimiento") %>%
      full_join(df_3, by = "IdEstablecimiento") %>%
      mutate(
        iteracion_1 = ifelse(is.na(iteracion_1), "NO APLICA", iteracion_1),
        iteracion_2 = ifelse(is.na(iteracion_2), "NO APLICA", iteracion_2),
        iteracion_3 = ifelse(is.na(iteracion_3), "NO APLICA", iteracion_3)
      )
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}


calcular_correlaciones_all <- function(lista_resultados_combinados_in) {
  #browser()
  # Calcular las matrices de correlación para cada dataframe en la lista
  correlaciones_lista <- lapply(lista_resultados_combinados_in, function(df) {
    df_num <- df %>%
      select(-IdEstablecimiento) %>%
      mutate(across(starts_with("iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA))))
    
    cor(df_num[, sapply(df_num, is.numeric)], use = "pairwise.complete.obs")
  })
  
  # Nombrar la lista con los años para identificación
  names(correlaciones_lista) <- names(lista_resultados_combinados_in)
  
  # Sumar todas las matrices con `Reduce`:
  suma_matrices <- Reduce("+", correlaciones_lista)
  
  # Calcular el promedio dividiendo por la cantidad de matrices
  n <- length(correlaciones_lista)
  promedio_matriz <- suma_matrices / n
  
  # Retornar resultados de correlación entre matrices de distintos años
  return(list(correlaciones_lista = correlaciones_lista,
              promedio_correlacion = promedio_matriz))
}


resultados_iteracion <- function(datos, original){
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
  iteracion_1 <- aplicar_sensibilidad(datos, datos_procesados, 0.99, FALSE)
  iteracion_2 <- aplicar_sensibilidad(datos, iteracion_1, 0.99, FALSE)
  
    
  resultados_combinados <- combinar_resultados_iteraciones(datos_procesados, iteracion_1, iteracion_2)
  resultados_correlacion <- calcular_correlaciones_all(resultados_combinados)
  
  
  lista_outliers_vrs <- list()
  vector_outliers_vrs <- c()
  
  lista_outliers_crs <- list()
  vector_outliers_crs <- c()
  
  lista_outliers_esc <- list()
  vector_outliers_esc <- c()
  
  # Especificar los años que quieres iterar
  for (anio in anios) {
    
    # Generar y almacenar los valores atípicos de VRS
    outliers_vrs <- boxplot.stats(original[[anio]][["data"]]$vrs)$out
    
    if (length(outliers_vrs)  > 0 ){
      ids_outliers_vrs <- original[[anio]][["data"]] %>%
        filter(vrs %in% outliers_vrs) %>%
        select(IdEstablecimiento, vrs)
      lista_outliers_vrs[[anio]] <- ids_outliers_vrs
      vector_outliers_vrs <- unique(c(vector_outliers_vrs, ids_outliers_vrs$IdEstablecimiento))
      
    }else{
      lista_outliers_vrs[[anio]] <- list()
      vector_outliers_vrs <- list()
    }
    
    # ----------------- #
    # Generar y almacenar los valores atípicos de CRS
    outliers_crs <- boxplot.stats(original[[anio]][["data"]]$crs)$out
    
    if (length(outliers_crs)  > 0){
      ids_outliers_crs <- original[[anio]][["data"]] %>%
        filter(crs %in% outliers_crs) %>%
        select(IdEstablecimiento, crs)
      lista_outliers_crs[[anio]] <- ids_outliers_crs
      vector_outliers_crs <- unique(c(vector_outliers_crs, ids_outliers_crs$IdEstablecimiento))
      
    }else{
      lista_outliers_crs[[anio]] <- list()
      vector_outliers_crs <- list()
    }
    
    # ----------------- #
    # Generar y almacenar los valores atípicos de escala
    outliers_esc <- boxplot.stats(original[[anio]][["data"]]$escala)$out
    
    if (length(outliers_esc)  > 0){
      ids_outliers_esc <- original[[anio]][["data"]] %>%
        filter(escala %in% outliers_esc) %>%
        select(IdEstablecimiento, escala)
      lista_outliers_esc[[anio]] <- ids_outliers_esc
      vector_outliers_esc <- unique(c(vector_outliers_esc, ids_outliers_esc$IdEstablecimiento))
      
    }else{
      lista_outliers_esc[[anio]] <- list()
      vector_outliers_esc <- list()
    }
    
  }
  
  list(
    original =  original,
    iteracion_1_vrs = iteracion_1_vrs,
    iteracion_2_vrs = iteracion_2_vrs,
    iteracion_1_crs = iteracion_1_crs,
    iteracion_2_crs = iteracion_2_crs,
    
    iteracion_1_esc = iteracion_1_esc,
    iteracion_2_esc = iteracion_2_esc,
    
    resultados_combinados = resultados_combinados,
    resultados_correlacion = resultados_correlacion,
    
    lista_outliers_vrs = lista_outliers_vrs,
    vector_outliers_vrs = vector_outliers_vrs,
    lista_outliers_crs = lista_outliers_crs,
    vector_outliers_crs = vector_outliers_crs,
    lista_outliers_esc = lista_outliers_esc,
    vector_outliers_esc = vector_outliers_esc
  )
}
