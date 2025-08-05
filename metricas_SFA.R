# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 4: MÉTRICAS, IDENTIFICACIÓN DE OUTLIERS Y
# PRUEBA DE WILCOXON
# ===================================================


# ===================================================
# OUTLIERS + METRICAS
# ===================================================

# ENTRADAS:
#   datos_lista:  lista de data.frames (uno por año) con la variable eff_global
#   nombre_categoria: nombre de la columna categórica usada para agrupar (por defecto "complejidad")
#
# SALIDA:
#   lista con:
#     - resumen_metricas: tabla con métricas antes y después de eliminar outliers
#     - datos_sin_outliers: lista de data.frames por año sin outliers
#
# DESCRIPCIÓN:
#   1) Detecta outliers en la variable eff_global usando el método IQR
#   2) Calcula métricas estadísticas (media, mediana, varianza, desviación estándar)
#      por categoría antes y después de eliminar outliers.
#   3) Devuelve los datos limpios y el resumen de métricas.
# ===================================================
analisis_sensibilidad_outliers_por_anno <- function(datos_lista, nombre_categoria = "complejidad") {
  
  # --- Función interna para detectar outliers ---
  es_outlier <- function(x) {
    x <- as.vector(x)
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE) # cuartiles
    iqr <- q[2] - q[1]                            # rango intercuartílico
    lim_inf <- q[1] - 1.5 * iqr                   # límite inferior
    lim_sup <- q[2] + 1.5 * iqr                   # límite superior
    x < lim_inf | x > lim_sup                     # devuelve vector lógico
  }
  
  resumen_metricas <- list()      # almacenará métricas por año
  datos_sin_outliers <- list()    # almacenará data.frames sin outliers por año
  
  # --- Procesar cada año ---
  for (anio in names(datos_lista)) {
    
    df <- datos_lista[[anio]]
    
    # Detectar outliers (TRUE/FALSE por fila)
    df <- df %>%
      mutate(outlier = es_outlier(eff_global))
    
    # Crear versión limpia sin outliers
    df_clean <- df %>% filter(!outlier)
    datos_sin_outliers[[anio]] <- df_clean
    
    # Calcular métricas agrupadas por la categoría (ej: complejidad)
    resumen_ano <- df %>%
      group_by(.data[[nombre_categoria]]) %>%
      summarise(
        Año = anio,
        n_total = n(),                                 # total de observaciones
        n_outliers = sum(outlier),                     # cantidad de outliers
        media_original = mean(eff_global, na.rm = TRUE),     # media original
        media_limpia   = mean(eff_global[!outlier], na.rm = TRUE), # media sin outliers
        mediana_original = median(eff_global, na.rm = TRUE),       # mediana original
        mediana_limpia   = median(eff_global[!outlier], na.rm = TRUE), # mediana sin outliers
        var_original = var(eff_global, na.rm = TRUE),   # varianza original
        var_limpia = var(eff_global[!outlier], na.rm = TRUE), # varianza sin outliers
        sd_original = sd(eff_global, na.rm = TRUE),     # desviación estándar original
        sd_limpia   = sd(eff_global[!outlier], na.rm = TRUE), # desviación estándar sin outliers
        .groups = "drop"
      )
    
    resumen_metricas[[anio]] <- resumen_ano
  }
  
  # Combinar todos los años en un solo data.frame
  resumen_final <- bind_rows(resumen_metricas)
  
  # Devolver lista con métricas y datos limpios
  list(
    resumen_metricas = resumen_final,
    datos_sin_outliers = datos_sin_outliers
  )
}

# Ejecución de la función
resultados_metricas <- analisis_sensibilidad_outliers_por_anno(
  datos_procesados, 
  nombre_categoria = "complejidad"
)

# Lista de datos sin outliers por año
datos_sin_outlier <- resultados_metricas$datos_sin_outliers

# (Opcional) Recalcular modelos SFA sobre datos sin outliers
# nuevos_datos_procesados <- lapply(datos_sin_outlier, procesar_sfa)


# ===================================================
# PRUEBA DE WILCOXON
# ===================================================

# ENTRADAS:
#   df_metricas: data.frame con métricas originales y limpias por categoría
#
# SALIDA:
#   data.frame con p-values de la prueba de Wilcoxon y su interpretación
#
# DESCRIPCIÓN:
#   Aplica una prueba de Wilcoxon pareada para comparar medias originales
#   vs medias sin outliers, verificando si la diferencia es significativa.
# ===================================================
aplicar_wilcoxon_por_categoria <- function(datos_lista, nombre_categoria = "complejidad") {
  
  resultados <- list()
  
  # Recorremos cada año
  for (anio in names(datos_lista)) {
    df <- datos_lista[[anio]]
    
    # Detectamos outliers en este año (de nuevo, para poder separar datos)
    es_outlier <- function(x) {
      q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2] - q[1]
      lim_inf <- q[1] - 1.5 * iqr
      lim_sup <- q[2] + 1.5 * iqr
      x < lim_inf | x > lim_sup
    }
    df <- df %>% mutate(outlier = es_outlier(eff_global))
    
    # Agrupamos por categoría (ej: complejidad)
    categorias <- unique(df[[nombre_categoria]])
    
    for (cat in categorias) {
      datos_cat <- df[df[[nombre_categoria]] == cat, ]
      original <- datos_cat$eff_global
      limpio   <- datos_cat$eff_global[!datos_cat$outlier]
      
      # Solo ejecutar prueba si hay outliers eliminados
      if (length(original) != length(limpio)) {
        # Wilcoxon para muestras independientes (porque tamaños distintos)
        p_value <- tryCatch(
          wilcox.test(original, limpio, paired = FALSE)$p.value,
          error = function(e) NA
        )
      } else {
        p_value <- NA
      }
      
      resultados[[length(resultados)+1]] <- data.frame(
        Año = anio,
        Categoria = cat,
        n_original = length(original),
        n_limpio   = length(limpio),
        media_original = mean(original, na.rm = TRUE),
        media_limpia   = mean(limpio, na.rm = TRUE),
        p_value = p_value,
        interpretacion = ifelse(is.na(p_value), 
                                "No aplica (sin outliers)", 
                                ifelse(p_value < 0.05, 
                                       "Diferencia significativa", 
                                       "Sin diferencia significativa"))
      )
    }
  }
  
  # Combinar resultados en un data.frame
  resultado_final <- do.call(rbind, resultados)
  return(resultado_final)
}

resultado_wilcoxon <- aplicar_wilcoxon_por_categoria(datos_procesados, "complejidad")
print(resultado_wilcoxon)
