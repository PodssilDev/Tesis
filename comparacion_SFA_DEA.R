# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 8: COMPARACIÓN SFA VS DEA
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(readxl)
library(writexl)

# ===================================================
# SFA VS DEA
# ===================================================

# ENTRADAS:
#   archivo_sfa   -> Ruta al archivo Excel que contiene las eficiencias SFA
#                    (por defecto "Eficiencias_SFA_2014-2023.xlsx")
#   archivo_dea   -> Ruta al archivo Excel que contiene las eficiencias DEA
#   sheet_sfa     -> Índice o nombre de la hoja del archivo SFA (por defecto 1)
#   sheet_dea     -> Índice o nombre de la hoja del archivo DEA (por defecto 1)
#   anios         -> Vector con los años a analizar (por defecto 2014:2023)
#   output        -> Nombre del archivo de salida Excel con los resultados
#                    (por defecto "Resultados_Testing.xlsx")
#
# DESCRIPCIÓN:
#   Compara los resultados de eficiencia técnica obtenidos por los modelos
#   SFA (Stochastic Frontier Analysis) y DEA (Data Envelopment Analysis) para
#   los mismos hospitales y periodos de tiempo.
#   - Calcula la **distancia euclidiana** entre las eficiencias SFA y DEA.
#   - Calcula la **correlación de Pearson** entre ambas eficiencias.
#   - Entrega resultados globales por año y también desagregados por nivel
#     de complejidad hospitalaria.
#   Finalmente, exporta los resultados a un archivo Excel con dos hojas:
#   1) "Por_Complejidad"
#   2) "Por_Año"
#
# SALIDA:
#   lista con dos data.frames:
#     - Por_Complejidad: distancia y correlación por año y complejidad
#     - Por_Año: distancia y correlación global por año
#   Además, genera un archivo Excel con el resumen.
# ===================================================
calcula_distancia_correlacion <- function(
    archivo_sfa = "Eficiencias_SFA_2014-2023.xlsx",
    archivo_dea,
    sheet_sfa = 1,
    sheet_dea = 1,
    anios = 2014:2023,
    output = "Resultados_Testing.xlsx"
) {
  # -------------------------------
  # 1) Cargar datos desde archivos Excel
  # -------------------------------
  df_sfa <- read_excel(archivo_sfa, sheet = sheet_sfa)
  df_dea <- read_excel(archivo_dea, sheet = sheet_dea)
  
  # -------------------------------
  # 2) Normalizar nombres y tipos
  # -------------------------------
  # Cambiar nombre de la columna "ID Establecimiento" por "IdEstablecimiento"
  colnames(df_sfa)[colnames(df_sfa) == "ID Establecimiento"] <- "IdEstablecimiento"
  
  # Asegurar que el identificador sea entero en ambos data.frames
  df_sfa$IdEstablecimiento <- as.integer(df_sfa$IdEstablecimiento)
  df_dea$IdEstablecimiento <- as.integer(df_dea$IdEstablecimiento)
  
  # Inicializar data.frames donde se almacenarán resultados
  resultados_por_complejidad <- data.frame()
  resultados_por_anio <- data.frame()
  
  # ===================================================
  # 3) Iterar por cada año de análisis
  # ===================================================
  for (anio in anios) {
    # Nombres de columnas específicas para el año
    col_sfa <- paste0("Eficiencia ", anio)   # en archivo SFA
    col_dea <- as.character(anio)           # en archivo DEA
    
    # Unir eficiencias de ambos métodos usando IdEstablecimiento
    df <- inner_join(
      df_sfa %>% select(IdEstablecimiento, Complejidad, SFA = !!sym(col_sfa)),
      df_dea %>% select(IdEstablecimiento, DEA = !!sym(col_dea)),
      by = "IdEstablecimiento"
    ) %>% filter(!is.na(SFA) & !is.na(DEA)) # eliminar NA
    
    # -------------------------------
    # 3.1 Análisis por nivel de complejidad
    # -------------------------------
    for (nivel in unique(df$Complejidad)) {
      sub <- df %>% filter(Complejidad == nivel)
      sub$SFA <- as.numeric(sub$SFA)
      sub$DEA <- as.numeric(sub$DEA)
      
      if (nrow(sub) > 0) {
        # Distancia euclidiana entre vectores SFA y DEA
        dist_eucl <- sqrt(sum((sub$SFA - sub$DEA)^2))
        # Correlación de Pearson
        correlacion <- cor(sub$SFA, sub$DEA)
        
        # Agregar resultados de este grupo de complejidad
        resultados_por_complejidad <- bind_rows(
          resultados_por_complejidad,
          tibble(
            Año = anio,
            Complejidad = nivel,
            N_Hospitales = nrow(sub),
            Distancia_Euclidiana = dist_eucl,
            Correlacion = correlacion
          )
        )
      }
    }
    
    # -------------------------------
    # 3.2 Análisis global por año (sin distinguir complejidad)
    # -------------------------------
    sub_total <- df
    sub_total$SFA <- as.numeric(sub_total$SFA)
    sub_total$DEA <- as.numeric(sub_total$DEA)
    
    if (nrow(sub_total) > 0) {
      dist_eucl_total <- sqrt(sum((sub_total$SFA - sub_total$DEA)^2))
      correlacion_total <- cor(sub_total$SFA, sub_total$DEA)
      
      # Agregar resultado global del año
      resultados_por_anio <- bind_rows(
        resultados_por_anio,
        tibble(
          Año = anio,
          N_Hospitales = nrow(sub_total),
          Distancia_Euclidiana = dist_eucl_total,
          Correlacion = correlacion_total
        )
      )
    }
  }
  
  # ===================================================
  # 4) Exportar resultados a Excel
  # ===================================================
  write_xlsx(
    list(
      "Por_Complejidad" = resultados_por_complejidad,
      "Por_Año" = resultados_por_anio
    ),
    path = output
  )
  
  # ===================================================
  # 5) Retornar resultados como lista invisible
  # ===================================================
  invisible(list(
    Por_Complejidad = resultados_por_complejidad,
    Por_Año = resultados_por_anio
  ))
}

# ===================================================
# EJEMPLO DE USO DE LA FUNCIÓN
# ---------------------------------------------------
# Compara eficiencias SFA vs DEA y genera archivo Excel "Testing_Results.xlsx"
# con resultados por complejidad y globales por año.
calcula_distancia_correlacion(
  archivo_dea = "RESULTADOS_OOVRS.xlsx",
  output = "Testing_Results.xlsx"
)

# ===================================================
# GRAFICOS SEPARADOS: CORRELACION Y DISTANCIA
# ===================================================
library(ggplot2)
library(readxl)
library(dplyr)

graficar_correlacion_y_distancia <- function(archivo_resultados = "Testing_Results.xlsx") {
  
  # 1) Leer hoja "Por_Año" desde el archivo
  df <- read_excel(archivo_resultados, sheet = "Por_Año")
  df$Año <- as.numeric(df$Año)
  
  # =====================
  # Gráfico de Correlación
  # =====================
  grafico_cor <- ggplot(df, aes(x = factor(Año), y = Correlacion)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = round(Correlacion, 3)), vjust = -0.3, size = 3.5) +
    ylim(0, 1) +
    labs(title = "Correlación SFA vs DEA por Año",
         x = "Año", y = "Correlación de Pearson") +
    theme_minimal(base_size = 13)
  
  ggsave("Correlacion_SFA_DEA.png", plot = grafico_cor, width = 8, height = 5, dpi = 300)
  
  # ============================
  # Gráfico de Distancia Euclidiana
  # ============================
  grafico_dist <- ggplot(df, aes(x = factor(Año), y = Distancia_Euclidiana)) +
    geom_col(fill = "darkorange") +
    geom_text(aes(label = round(Distancia_Euclidiana, 3)), vjust = -0.3, size = 3.5) +
    labs(title = "Distancia Euclidiana SFA vs DEA por Año",
         x = "Año", y = "Distancia Euclidiana") +
    theme_minimal(base_size = 13)
  
  ggsave("Distancia_Euclidiana_SFA_DEA.png", plot = grafico_dist, width = 8, height = 5, dpi = 300)
  
  # Mostrar ambos gráficos en pantalla
  print(grafico_cor)
  print(grafico_dist)
  
  invisible(list(correlacion = grafico_cor, distancia = grafico_dist))
}

graficar_correlacion_y_distancia("Testing_Results.xlsx")

