#source("functions.R")
#source("graphics.R")

library(frontier)
library(dplyr)
library(openxlsx)
library(readxl)
library(scatterplot3d)
library(rgl)
library(tidyr)


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