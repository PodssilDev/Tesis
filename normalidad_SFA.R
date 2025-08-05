# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 3: NORMALIDAD DE EFICIENCIA TÉCNICA HOSPITALARIA
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(tidyverse)
library(patchwork)  

# ==============================================
# ANÁLISIS DE NORMALIDAD DE ET HOSPITALARIA
# ==============================================

# ENTRADAS:
#   datos_lista -> lista de data.frames (uno por año) que contiene la variable eff_global
#   ncol        -> número de columnas para organizar los histogramas (por defecto 2)
#   guardar     -> booleano que indica si guardar la imagen de los histogramas
#   archivo     -> nombre del archivo de salida (si guardar = TRUE)
#   ancho, alto -> dimensiones de la imagen guardada
#   dpi         -> resolución de la imagen
#
# SALIDA:
#   lista con:
#     - tabla_pvalues: tabla con los p-values de la prueba de Shapiro-Wilk
#     - grafico: objeto patchwork con los histogramas
#
# DESCRIPCIÓN:
#   Genera histogramas de la eficiencia técnica global por año y aplica la prueba
#   de normalidad de Shapiro-Wilk (si el tamaño de muestra está entre 3 y 5000).
#   Devuelve tanto la tabla de resultados de p-values como una imagen que junta
#   los histogramas obtenidos

# ==============================================
analizar_normalidad_eff_con_QQ <- function(datos_lista, ncol = 2, guardar = TRUE,
                                           archivo = "histogramas_eficiencia.png",
                                           ancho = 18, alto = 10, dpi = 300) {
  
  grafs_hist <- list()   # lista para almacenar los histogramas individuales
  resultados <- list()   # lista para almacenar los p-values de Shapiro-Wilk
  
  for (anio in names(datos_lista)) {
    
    df <- datos_lista[[anio]]  # extraer data.frame de un año
    
    # --- Histograma de eficiencia ---
    p_hist <- ggplot(df, aes(x = eff_global)) +
      geom_histogram(bins = 30, color = "black", fill = "lightblue") +
      labs(title = paste("Histograma eficiencia –", anio),
           x = "Eficiencia", y = "Frecuencia") +
      theme_minimal(base_size = 12)
    
    grafs_hist[[anio]] <- p_hist   # almacenar gráfico
    
    # --- Prueba de Shapiro-Wilk ---
    # Sólo se aplica si el tamaño muestral está entre 3 y 5000
    if (dplyr::between(nrow(df), 3, 5000)) {
      pvalor <- shapiro.test(df$eff_global)$p.value
    } else {
      pvalor <- NA  # no aplicable
    }
    resultados[[anio]] <- pvalor
  }
  
  # --- Combinar todos los histogramas en un solo gráfico usando patchwork ---
  plot_final <- wrap_plots(grafs_hist, ncol = ncol) +
    plot_annotation(title = "Distribución de eficiencia por año",
                    theme = theme(plot.title = element_text(size = 18, face = "bold")))
  
  # --- Guardar imagen si se indicó ---
  if (guardar) {
    ggsave(archivo, plot_final, width = ancho, height = alto, dpi = dpi)
  }
  
  # --- Resultado: tabla de p-values y gráfico final ---
  lista_salida <- list(
    tabla_pvalues = tibble(
      Año = names(resultados),
      p_value_Shapiro = unlist(resultados)
    ),
    grafico = plot_final
  )
  return(lista_salida)
}


# ============================
# HISTOGRAMAS Y SHAPIRO-WILK
# ============================

tabla_resultados <- analizar_normalidad_eff_con_QQ(datos_procesados)


# Tabla con p-values de Shapiro-Wilk
tabla_resultados$tabla_pvalues

# Visualización del gráfico directamente en la sesión de R
tabla_resultados$grafico

# Los datos NO son normales en ninguno de sus años.
