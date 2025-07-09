# ===================================================
# IMPORTACIÓN DE LIBRERÍAS
# ===================================================

library(dplyr)
library(openxlsx)
library(readxl)
library(scatterplot3d)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(sf)
library(tibble)
library(deaR)

consolidar_datos_por_anio <- function(anio) {
  
  #anio <- 2014
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

datos[["2021"]][["complejidad"]][[149]] <- "Baja"
datos[["2021"]][["complejidad"]][[56]] <- "Alta"
datos[["2021"]][["latitud"]][[149]] <- -40.5785
datos[["2021"]][["latitud"]][[56]] <- -33.4442
datos[["2021"]][["longitud"]][[149]] <- -73.3772
datos[["2021"]][["longitud"]][[56]] <- -70.6385
datos[["2021"]][["region_id"]][[56]] <- 13





# ==============================================
#  CÁLCULO DEA
# ==============================================

datos_2014 <- datos[["2014"]]

# Selecciona entradas y salidas
inputs <- datos_2014 %>%
  select(dias_cama_disponible, X21_value, X22_value) %>%
  as.matrix()

outputs <- datos_2014 %>%
  select(Egresos.GRD, Consultas, Quirofano) %>%
  as.matrix()

data_dea <- make_deadata(datadea = datos_2014,
                         dmus = "IdEstablecimiento",
                         inputs = c("dias_cama_disponible", "X21_value", "X22_value"),
                         outputs = c("Egresos.GRD", "Consultas", "Quirofano"))

resultados_dea <- model_basic(data_dea, 
                              orientation = "oo", 
                              rts = "vrs")

eficiencias <- efficiencies(resultados_dea)
eficiencias_normalizadas <- 1 / eficiencias
# Puedes agregar los nombres de hospitales si quieres
eficiencias <- data.frame(IdEstablecimiento = datos_2014$IdEstablecimiento,
                          eficiencia = eficiencias_normalizadas)



Q1 <- quantile(eficiencias$eficiencia, 0.25)
Q3 <- quantile(eficiencias$eficiencia, 0.75)
IQR_val <- Q3 - Q1

lim_inf <- quantile(eficiencias$eficiencia, 0.05)
lim_sup <- quantile(eficiencias$eficiencia, 0.95)

outliers <- eficiencias %>% filter(eficiencia < 0.15)  # Menores al 15%




# Filtra los hospitales SIN outliers
sin_outliers <- eficiencias %>%
  filter(eficiencia >= 0.15)


# Filtra el tibble original para quitar los outliers
datos_2014_sin_outliers <- datos_2014 %>%
  filter(IdEstablecimiento %in% sin_outliers$IdEstablecimiento)

# Prepara de nuevo el objeto de datos para DEA
data_dea_sin_outliers <- make_deadata(datadea = datos_2014_sin_outliers,
                                      dmus = "IdEstablecimiento",
                                      inputs = c("dias_cama_disponible", "X21_value", "X22_value"),
                                      outputs = c("Egresos.GRD", "Consultas", "Quirofano"))

# Corre DEA nuevamente
resultados_dea_sin_outliers <- model_basic(data_dea_sin_outliers, 
                                           orientation = "oo", 
                                           rts = "vrs")

# Obtén nuevas eficiencias
eficiencias_sin_outliers <- efficiencies(resultados_dea_sin_outliers)
eficiencias_finales <- 1/ eficiencias_sin_outliers


eficiencias_sensibilidad <- data.frame(IdEstablecimiento = datos_2014_sin_outliers$IdEstablecimiento,
                          eficiencia = eficiencias_finales)


df_sfa <- read_excel("Eficiencias_SFA_2014-2023.xlsx", sheet = 1)
df_dea <- read_excel("RESULTADOS.xlsx", sheet = 1)
df_dea <- eficiencias

colnames(df_sfa)[colnames(df_sfa) == "ID Establecimiento"] <- "IdEstablecimiento"
df_sfa$IdEstablecimiento <- as.integer(df_sfa$IdEstablecimiento)
df_dea$IdEstablecimiento <- as.integer(df_dea$IdEstablecimiento)

anio <- 2014

euclid_resultados <- data.frame()

for (anio in anios) {
  col_sfa <- paste0("Eficiencia ", anio)
  col_dea <- as.character("eficiencia")
  
  df <- inner_join(
    df_sfa %>% select(IdEstablecimiento, Complejidad, SFA = !!sym(col_sfa)),
    df_dea %>% select(IdEstablecimiento, DEA = !!sym(col_dea)),
    by = "IdEstablecimiento"
  ) %>% filter(!is.na(SFA) & !is.na(DEA) & !is.na(Complejidad))
  for (nivel in unique(df$Complejidad)) {
    sub <- df %>% filter(Complejidad == nivel)
    sub$SFA <- as.numeric(sub$SFA)
    sub$DEA <- as.numeric(sub$DEA)
    
    if (nrow(sub) > 0) {
      dist_eucl <- cor(sub$SFA, sub$DEA)
      euclid_resultados <- bind_rows(
        euclid_resultados,
        tibble(
          Año = anio,
          Complejidad = nivel,
          N_Hospitales = nrow(sub),
          Distancia_Euclidiana = dist_eucl
        )
      )
    }
  }
}
print(euclid_resultados)

sfa <- df$SFA

correlacion <- cor(as.numeric(df$SFA), as.numeric(df$DEA))



# Cambia el path si lo tienes en otro directorio
archivo <- "SFA VS DEA.xlsx"
df <- read_excel(archivo, sheet = 2)

head(df)

# Distancia euclidiana total
df$DEA <- as.numeric(df$DEA)
df$SFA <- as.numeric(df$SFA)
dist_total <- sqrt(sum((df$DEA - df$SFA)^2))

# Correlación total
corr_total <- cor(df$DEA, df$SFA, method = "pearson")

# Imprimir resultados
cat("Distancia euclidiana total:", dist_total, "\n")
cat("Correlación total:", corr_total, "\n")


resultados <- df %>%
  group_by(Complejidad) %>%
  summarise(
    N = n(),
    Distancia_Euclidiana = sqrt(sum((DEA - SFA)^2)),
    Correlacion = cor(DEA, SFA, method = "pearson")
  )

print(resultados)



dea_df <- read_excel("RESULTADOS.xlsx", sheet = 1) # Cambia el sheet si corresponde

sfa_df <- read_excel("SFA VS DEA.xlsx", sheet = 2)

# Si los nombres de columna son iguales
merged <- dea_df %>%
  inner_join(sfa_df %>% select(IdEstablecimiento, Complejidad, SFA), by = "IdEstablecimiento")

merged$`2014` <- as.numeric(merged$`2014`)
merged$SFA <- as.numeric(merged$SFA)

# Distancia euclidiana total
dist_total <- sqrt(sum((merged$`2014` - merged$SFA)^2, na.rm = TRUE))

# Correlación total
corr_total <- cor(merged$`2014`, merged$SFA, use = "pairwise.complete.obs", method = "pearson")

cat("Distancia euclidiana total:", dist_total, "\n")
cat("Correlación total:", corr_total, "\n")


resultados_con_domi <- merged %>%
  group_by(Complejidad) %>%
  summarise(
    N = n(),
    Distancia_Euclidiana = sqrt(sum((`2014` - SFA)^2, na.rm = TRUE)),
    Correlacion = cor(`2014`, SFA, use = "pairwise.complete.obs", method = "pearson")
  )

print(resultados)
