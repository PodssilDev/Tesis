# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 1: CONSOLIDADO, LIMPIEZA Y NORMALIZACIÓN
# DE DATOS.
# ===================================================


# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(here)
library(dplyr)
library(openxlsx)
library(readxl)

# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================

#  ENTRADA:
#     anio (numeric): año específico para el cual se consolidarán los datos
#
#  SALIDA:
#     data.frame: conjunto de datos consolidados con información hospitalaria,
#                 financiera, consultas, quirófano y egresos ajustados por GRD
#
#  DESCRIPCIÓN:
#     Esta función lee múltiples fuentes de datos (CSV, TXT, Excel) correspondientes 
#     a un año determinado, realiza uniones y transformaciones para consolidar 
#     información relevante de los hospitales, sus consultas, actividades de quirófano, 
#     egresos hospitalarios ajustados por predicciones GRD y datos financieros. 
#     Retorna un data.frame sin duplicados con todos los datos integrados.
# ==============================================
consolidar_datos_por_anio <- function(anio) {
  
  # ============================
  # Definición de rutas de archivos según el año
  # ============================
  path_hospitales <- paste0("data/", anio, "/", anio, "_hospitals.csv")
  path_hospitales_complejidades <- paste0("data/hospitales.csv")
  path_predicciones_grd <- paste0("data/", anio, "/", anio, "_prediciones_grd.txt")
  path_datos_consolidados <- paste0("data/", anio, "/", anio, "_consolidated_data.csv")
  path_financiero <- paste0("data/", anio, "/", anio, "_financial_data.csv")
  path_estadisticas <- "data/Consolidado estadísticas hospitalarias 2014-2023.xlsx"
  path_consultas <- paste0("data/", anio, "/variables/", anio, "_consultas.txt")
  path_quirofano <- paste0("data/", anio, "/variables/", anio, "_quirofano.txt")
  
  # ============================
  # Carga de datos de hospitales y complejidad
  # ============================
  hospitales <- read.csv(path_hospitales) %>% rename("IdEstablecimiento" = "hospital_id")
  hospitales_complejidades <- read.csv(path_hospitales_complejidades) %>% rename("IdEstablecimiento" = "hospital_id")
  
  # Unión con información de complejidad hospitalaria
  hospitales <- hospitales %>%
    left_join(hospitales_complejidades %>% select(IdEstablecimiento, complejidad), 
              by = "IdEstablecimiento")
  
  # ============================
  # Carga de predicciones GRD, datos consolidados y financieros
  # ============================
  predicciones_grd <- read.csv(path_predicciones_grd, sep=",")
  datos_consolidados <- read.table(path_datos_consolidados, sep=";", header=TRUE)
  
  # Se seleccionan solo las columnas relevantes del archivo financiero
  financiero <- read.csv(path_financiero) %>% 
    select(hospital_id, X21_value, X22_value) %>% 
    rename("IdEstablecimiento" = "hospital_id")
  
  # Conversión a numérico y eliminación de filas con datos financieros incompletos
  financiero$X21_value <- as.numeric(financiero$X21_value)
  financiero$X22_value <- as.numeric(financiero$X22_value)
  financiero <- financiero[rowSums(is.na(financiero)) < 2, ]
  
  # ============================
  # Carga de estadísticas hospitalarias desde Excel
  # ============================
  estadisticas <- read_excel(path_estadisticas, sheet = (anio - 2014) + 1, skip = 1)  %>% 
    rename("IdEstablecimiento" = "Cód. Estab.", "Region" = "Nombre SS/SEREMI") %>%
    filter(`Nombre Nivel Cuidado` == "Datos Establecimiento") %>% 
    select(-"Cód. Nivel Cuidado", -"Cód. SS/SEREMI", -"Nombre Nivel Cuidado") %>%  
    semi_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(1:5)
  
  # ============================
  # Extracción de variables específicas: días cama y egresos
  # ============================
  dias_cama_disponibles <- estadisticas %>% 
    filter(Glosa == "Dias Cama Disponibles") %>%  
    select(1:5) %>% rename("dias_cama_disponible" = "Acum") %>% select(-Glosa)
  
  egresos <- estadisticas %>% 
    filter(Glosa == "Numero de Egresos") %>%  
    select(1:5) %>% rename("egresos" = "Acum") %>% select(-Glosa)
  
  # ============================
  # Procesamiento de archivo de consultas
  # ============================
  consultas <- unlist(strsplit(readLines(path_consultas), ","))
  columnas_validas <- intersect(unlist(consultas), colnames(datos_consolidados))
  consultas_data <- subset(datos_consolidados, select = columnas_validas)
  
  # Conversión de columnas tipo character a numérico
  cols_char <- sapply(consultas_data, is.character)
  consultas_data[, cols_char] <- lapply(consultas_data[, cols_char], function(x) as.numeric(x))
  
  # Creación de suma total de consultas por establecimiento
  consultas_data$sumaTotal <- rowSums(consultas_data[, -which(names(consultas_data) == "idEstablecimiento")], na.rm = TRUE)
  consultas <- data.frame(idEstablecimiento = consultas_data$idEstablecimiento, 
                          Consultas = consultas_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Consultas)
  
  # ============================
  # Procesamiento de archivo de quirófano
  # ============================
  quirofano <- unlist(strsplit(readLines(path_quirofano), ","))
  columnas_validas_q <- intersect(unlist(quirofano), colnames(datos_consolidados))
  quirofano_data <- subset(datos_consolidados, select = unlist(columnas_validas_q))
  
  # Conversión de comas a puntos y redondeo a enteros
  quirofano_data <- quirofano_data %>%
    mutate(across(-idEstablecimiento, ~ as.integer(floor(as.numeric(gsub(",", ".", .))))))
  
  # Creación de suma total de uso de quirófano
  quirofano_data$sumaTotal <- rowSums(select(quirofano_data, -idEstablecimiento), na.rm = TRUE)
  quirofano <- data.frame(idEstablecimiento = quirofano_data$idEstablecimiento, 
                          Quirofano = quirofano_data$sumaTotal) %>%
    rename("IdEstablecimiento" = "idEstablecimiento") %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    select(IdEstablecimiento, Quirofano)
  
  # ============================
  # Ajuste de egresos con predicciones GRD
  # ============================
  intermediate_df <- egresos %>%
    inner_join(predicciones_grd, by = "IdEstablecimiento") %>%
    mutate(Egresos.GRD = Prediction * egresos) %>%
    select("Region", IdEstablecimiento, "Nombre Establecimiento", Egresos.GRD)
  
  # ============================
  # Combinación de datos de entrada (financiero + camas) 
  # y salidas (egresos ajustados, consultas, quirófano)
  # ============================
  input <- left_join(financiero, dias_cama_disponibles %>% 
                       select(IdEstablecimiento, dias_cama_disponible), by = "IdEstablecimiento")
  output <- intermediate_df %>%
    left_join(consultas, by = "IdEstablecimiento") %>%
    left_join(quirofano, by = "IdEstablecimiento")
  
  # ============================
  # Unión final con datos geográficos y de complejidad
  # ============================
  all <- inner_join(output, input, by = "IdEstablecimiento") %>%
    left_join(hospitales %>% select(IdEstablecimiento, region_id, latitud, longitud, complejidad), 
              by = "IdEstablecimiento") %>%
    relocate(region_id, .after = Region)
  
  # Eliminación de duplicados
  all_sin_duplicados <- distinct(all)
  
  return(all_sin_duplicados)
}

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

# Lista de años a procesar
anios <- 2014:2023

# Consolidación de datos para cada año usando la función definida
datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Identificación de DMUs (hospitales) comunes en todos los años
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))

# Filtrado de datos por año para incluir solo las DMUs comunes
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])

# Adición manual de datos faltantes para el año 2021
datos[["2021"]][["complejidad"]][[149]] <- "Baja"
datos[["2021"]][["complejidad"]][[56]] <- "Alta"
datos[["2021"]][["latitud"]][[149]] <- -40.5785
datos[["2021"]][["latitud"]][[56]] <- -33.4442
datos[["2021"]][["longitud"]][[149]] <- -73.3772
datos[["2021"]][["longitud"]][[56]] <- -70.6385
datos[["2021"]][["region_id"]][[56]] <- 13
