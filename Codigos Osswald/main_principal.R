source("functions.R")
source("graphics.R")


# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# ==============================================
#  CÁLCULO DEA
# ==============================================

#  CALCULO DE EFICIENCIA EN TODOS LOS AÑOS Y REVISIÓN DE
#  SENSIBILIDAD - ELIMINACION EFICIENTES
resultados <- list(io = resultados_iteracion(datos, "io"),
                   oo = resultados_iteracion(datos, "oo"))

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
resultados_combinaciones <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])
correlacion_todos_metodos <- calcular_correlaciones_all(resultados_combinaciones)


#  NUEVO CONJUNTO DE DATOS A PARTIR DE ELIMINACIÓN DE ATÍPICOS 

datos_sin_atipicos <- datos_filtrados_atipicos(datos,resultados)

