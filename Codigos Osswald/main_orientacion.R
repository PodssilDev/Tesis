setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")


orientacion <- "oo"
retorno <- "vrs"
columna <- "vrs_oo"

resultados_usar <- resultados_sin_atipicos[[columna]]

# ---- SOLO VRS OO

# Vector de años de interés
years <- 2014:2023
# --------------------------- #

# Extraemos la submatriz de correlaciones vrs para cada año
vrs_cor_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl("^vrs", rownames(cor_matriz)),
                               grepl("^vrs", colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_cor_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_cor_list

resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]] <- vrs_cor_list



correlaciones_eficiencia_grafica(resultados_usar[[orientacion]][["resultados_correlacion"]][["correlaciones_lista"]], orientacion, c("VRS iteracion 1", "VRS iteracion 2", "VRS iteracion 3"),  "Sensibilidad por eliminación de DMU eficientes", "iteraciones")
# --------------------------- #


# Extraemos la submatriz de correlaciones vrs para cada año
vrs_atp_list <- lapply(years, function(y) {
  # Accedemos a la matriz de correlaciones del año y
  cor_matriz <- correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]][[as.character(y)]]
  
  # Filtramos filas y columnas que inician con "vrs"
  cor_matriz_vrs <- cor_matriz[grepl("^vrs", rownames(cor_matriz)),
                               grepl("^vrs", colnames(cor_matriz))]
  
  return(cor_matriz_vrs)
})

# Asignamos nombres a cada elemento de la lista para identificar el año
names(vrs_atp_list) <- years

# Ahora vrs_cor_list es una lista en la que cada elemento es la submatriz vrs de cada año
vrs_atp_list

correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]] <- vrs_atp_list


correlaciones_eficiencia_grafica(correlacion_todos_metodos_atipicos[[columna]][["original_vs_sin_atipicos"]][[orientacion]][["correlaciones_lista"]], "ambos", c("VRS original", "VRS sin atipicos"), "Comparación Original vs sin atipicos - Orientación Outputs VRS", "sin_atipicos")



# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

eficiencias_grafica(resultados_usar)


# ==============================================
#  MALMQUIST 
# ==============================================

# DATOS COMPLETOS / ORIGINALES

datos_usar <- datos_sin_atipicos[[columna]]

# DATOS SIN ATIPICOS PARA VRS OO
malmquist_indices <- malmquist(datos_usar,retorno, "out")

generar_graficas_malmquist(malmquist_indices$index,"out_vrs")


# ==============================================
#  DETERMINANTES
# ==============================================

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados_usar[[orientacion]], 500, retorno, "Entradas")})

# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)


# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- importancia_dataframe(random_forest)
#resultados_importancia <- determinantes_importancia_single(random_forest, anios_pre_pandemia, anios_pandemia)
# ==============================================
#  RESULTADOS
# ==============================================



# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

#resultados_usar <- resultados

lapply(anios, function(anio) {
  eficiencias_chile_grafica(resultados_usar[[orientacion]][["original"]][[as.character(anio)]][["data"]], anio, retorno, "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})



# ---------------------------------------- #
# ---------------------------------------- #
# ---------------------------------------- #
# ==============================================
#    PRUEBAS ESTADISTICAS
# ==============================================


path_hospitales_complejidades <- paste0("data/hospitales.csv")
hospitales_complejidades <- read.csv(path_hospitales_complejidades) %>% rename("IdEstablecimiento" = "hospital_id")

for (year in 2014:2023) {
  resultados_usar[[orientacion]]$original[[as.character(year)]]$data <- resultados_usar[[orientacion]]$original[[as.character(year)]]$data %>%
    left_join(hospitales_complejidades %>% select(IdEstablecimiento, complejidad), 
              by = "IdEstablecimiento")
}


# ANÁLISIS DE EFICIENCIA A HOSPITALES

# ALTA COMPLEJIDAD
ef_tec_alta <- guardar_dataframe_por_columna(resultados_usar[[orientacion]], retorno)


# MEDIANA

# BAJA





# ==============================================
#    EFICIENCIA
# ==============================================


ef_tec <- guardar_dataframe_por_columna(resultados_usar[["oo"]], retorno)

# NO SON NORMALES LOS DATOS





# PARA VER SIHAY DIFERENCIAS DE MEDIANA ENTRE PRE Y POST
grupo_1_medianas <- apply(ef_tec[, 3:8], 1, median)
grupo_2_medianas <- apply(ef_tec[, 9:12], 1, median)

# Calcular la mediana de cada grupo
mediana_grupo1 <- median(grupo_1_medianas, na.rm = TRUE)
mediana_grupo2 <- median(grupo_2_medianas, na.rm = TRUE)


variacion <- 100 * ( (mediana_grupo2-mediana_grupo1)/mediana_grupo1 )

# Comparación
if (mediana_grupo1 > mediana_grupo2) {
  print("La mediana del grupo 1 es mayor que la del grupo 2.")
} else if (mediana_grupo1 < mediana_grupo2) {
  print("La mediana del grupo 2 es mayor que la del grupo 1.")
} else {
  print("Las medianas de ambos grupos son iguales.")
}



# REVISION DE LOS DOS GRUPOS PRE Y POT
wilcox.test(grupo_1_medianas, grupo_2_medianas, paired = TRUE, alternative = "two.sided")


# COMPARACION DE HOSPITALES



ef_tec <- guardar_dataframe_por_columna(resultados_usar[[orientacion]], retorno)
ef_tec <- ef_tec[,-2]

hospitals_long_ef <- ef_tec%>%
  pivot_longer(cols = -c(IdEstablecimiento), names_to = "Año", values_to = "Valor")

df <- data.frame(
  IdEstablecimiento = rep(paste("Hospital", 1:10), each = 10),  # 10 hospitales
  Año = rep(2014:2023, times = 10),                    # Años de 2014 a 2023
  Valor = runif(100, min = 10, max = 100)              # Valores aleatorios
)


#KRUSKAL PARA REVISAR POR AÑO SI HAY DIFERENCIAS:


resultados_kruskal <- by(hospitals_long_ef, hospitals_long_ef$Año, function(subset) {
  kruskal_result <- kruskal.test(Valor ~ IdEstablecimiento, data = subset)
  return(kruskal_result)
})

#NO HAY DIFERENCIAS SIGNIFICATIVAS (NO HAY HOSPITALES QUE TENGAN VALORES MAS SIGNIFICATIVOS POR AÑO)








#FRIEDMAN

friedman_result <- friedman.test(Valor ~ IdEstablecimiento | Año, data = hospitals_long_ef)


print(friedman_result)

# HAY DIFERENCIAS SIGNIFICATIVAS

#library(FSA)


#posthoc_dunn <- dunnTest(Valor ~ Año, data = hospitals_long_ef, method = "holm")

#print(posthoc_dunn)
#library(ggplot2)
#ggplot(hospitals_long_ef, aes(x = as.factor(Año), y = Valor)) +
#  geom_boxplot() +
#  theme_minimal() +
#  labs(title = "Distribución de valores por año", x = "Año", y = "Valor")




#if (!requireNamespace("rstatix", quietly = TRUE)) install.packages("rstatix")

# Cargar librería
library(rstatix)

posthoc_wilcoxon <- hospitals_long_ef %>%
  pairwise_wilcox_test(Valor ~ Año, paired = TRUE, p.adjust.method = "holm")

# Mostrar resultados
print(n=50,posthoc_wilcoxon)







# ==============================================
#    MALMQUIST
# ==============================================



  
mal_tec <- malmquist_indices[["index"]][,c(1:10)]

normalidad_malm <- verificar_normalidad(mal_tec,c(1:9))
print(normalidad_malm)


# REVISAR SI HAY DIFERENCIAS EN LAS MEDIANAS
df_long_malm <- mal_tec %>%        # Identificador del DMU
  pivot_longer(
    cols = starts_with("20"),          # Ajustar si tus columnas se llaman "2014", "2015", etc.
    names_to = "year",
    values_to = "index"
  ) %>%
  mutate(
    year = as.factor(year)  # o as.numeric si prefieres, pero factor para la prueba
  )

# Aplicar la prueba de Kruskal-Wallis
resultado_kruskal_malm <- kruskal.test(index ~ year, data = df_long_malm)

# Mostrar el resultado
print(resultado_kruskal_malm)



library(dunn.test)

# Aplicar la prueba de Dunn
resultado_dunn <- dunn.test(df_long_malm$index, df_long_malm$year, method = "holm")

# Mostrar el resultado
print(resultado_dunn)

posthoc_wilcoxon_mal <- df_long_malm %>%
  pairwise_wilcox_test(index ~ year, paired = TRUE, p.adjust.method = "holm")

# Mostrar resultados
print(n=50,posthoc_wilcoxon)






# ==============================================
#    DETERMINANTES
# ==============================================
# REVISAR SI HAY SIGNIFICANCIA POR AÑO EN DETERMINANTES:


df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_incmse_pre <- df_incmse[,c(1:7)]
df_incmse_post <- df_incmse[,c(1,8:11)]


df_long_all_comp <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_all_comp <- na.omit(df_long_all_comp)

# Calcular la mediana de los valores en el periodo "pre" (columnas 2 a 7)
df_median_pre <- df_incmse_pre %>%
  mutate(Mediana_Pre = apply(df_incmse_pre[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Pre)  # Seleccionar solo Variable y la mediana

# Calcular la mediana de los valores en el periodo "post" (columnas 8 a 11)
df_median_post <- df_incmse_post %>%
  mutate(Mediana_Post = apply(df_incmse_post[, -1], 1, median, na.rm = TRUE)) %>%
  select(Variable, Mediana_Post)  # Seleccionar solo Variable y la mediana


# Unir ambos dataframes en un único dataframe con las medianas
df_median <- left_join(df_median_pre, df_median_post, by = "Variable")
df_median <- calcular_estadisticas(df_median) %>% filter(Frecuencia > 1) 
df_median <- df_median[,-c(3,4)]

df_incmse_all_est <- calcular_estadisticas(df_incmse_all) %>% filter(Frecuencia > 1)
df_incmse_all <- df_incmse_all_est[,c(1:11)]

df_incmse_pre_est <- calcular_estadisticas(df_incmse_pre) %>% filter(Frecuencia > 1) 
df_incmse_pre <- df_incmse_pre_est[,c(1:7)]

df_incmse_post_est <- calcular_estadisticas(df_incmse_post) %>% filter(Frecuencia > 1)
df_incmse_post <- df_incmse_post_est[,c(1:5)]




# Convertir a formato largo (tidy)
df_long_all <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_all <- na.omit(df_long_all)

df_long_pre <- df_incmse_pre %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_pre <- na.omit(df_long_pre)

df_long_post <- df_incmse_post %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")
df_long_post <- na.omit(df_long_post)

df_long_pre_post <- df_median %>% pivot_longer(-Variable, names_to = "Periodo", values_to = "Valor")
df_long_pre_post <- na.omit(df_long_pre_post)


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_all <- df_long_all %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre <- df_long_pre %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_post <- df_long_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Año)$p.value
  )

# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_pre_post <- df_long_pre_post %>%
  group_by(Variable) %>%
  summarise(
    p_value = kruskal.test(Valor ~ Periodo)$p.value
  )



print(n=100,kruskal_results_all)
print(n=100,kruskal_results_pre)
print(n=100,kruskal_results_post)
print(n=100,kruskal_results_pre_post)


df_long_kruskal_all <- df_long_all %>% filter(Variable %in% kruskal_results_all$Variable)
df_long_kruskal_pre <- df_long_pre %>% filter(Variable %in% kruskal_results_pre$Variable)
df_long_kruskal_post <- df_long_post %>% filter(Variable %in% kruskal_results_post$Variable)

# Gráfico de evolución de las variables analizadas










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


library(ggplot2)
library(RColorBrewer)

grafica <- ggplot(df_long_kruskal_all, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_all$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","todos",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)



grafica <- ggplot(df_long_kruskal_pre, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_pre$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","pre",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)





grafica <- ggplot(df_long_kruskal_post, aes(x = Año, y = Variable, fill = Valor)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Matriz de valores por variable y año",
       x = "Año", y = "Variable", fill = "Valor") +
  scale_fill_gradientn(
    colors = brewer.pal(11, "RdYlGn"),  # Paleta de colores RdYlGn
    limits = range(df_long_kruskal_post$Valor, na.rm = TRUE)  # Escala de valores automática
  )



ggsave(paste0("determinantes","_","post",".jpg"), plot = grafica, width = 10, height = 8, dpi = 300)




df_incmse_est <- resultados_importancia[["df_incmse_est_10"]]



df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_all, by = "Variable") %>%
  rename(P_value_all = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_pre, by = "Variable") %>%
  rename(P_value_pre = p_value)  # Cambia el nombre de la columna

df_incmse_est <- df_incmse_est %>%
  left_join(kruskal_results_post, by = "Variable") %>%
  rename(P_value_post = p_value)  # Cambia el nombre de la columna



























# Almacenar resultados en excel
guardar_resultados(
  dataframes = resultados_usar[[orientacion]],
  retorno,
  df_incmse_est,
  malmquist = malmquist_indices$index,
  carpeta="results/oo_vrs",
  archivo_salida = "RESULTADOS.xlsx",
  prefijo = orientacion
)



# Gráfico de evolución de las 10 variables
ggplot(df_long_10, aes(x = Año, y = Valor, group = Variable, color = Variable)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() +
  labs(title = "Evolución de las primeras 10 variables (2014-2023)",
       x = "Año", y = "Valor de la Variable")

