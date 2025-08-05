# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 9: PRUEBAS ESTADÍSTICAS (FRIEDMAN Y WILCOXON
# CON AJUSTE PAREADO HOLM)
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(dplyr)
library(tidyr)
library(rstatix)
library(writexl)


# ENTRADAS:
#   datos_largos -> Dataframe con columnas:
#       IdEstablecimiento, anio, eff_global, complejidad
#   output_file  -> Nombre del archivo Excel de salida
# SALIDA:
#   Exporta resultados a Excel y retorna lista con:
#     - friedman_global
#     - wilcoxon_global
#     - friedman_por_complejidad
#     - wilcoxon_por_complejidad
# ===================================================
pruebas_friedman_wilcoxon <- function(datos_largos, output_file = "Resultados_Friedman_Wilcoxon.xlsx") {
  
  # =====================
  # FUNCIONES INTERNAS
  # =====================
  
  # ---- Función para aplicar Friedman con datos wide ----
  aplicar_friedman <- function(df) {
    df_wide <- df %>%
      select(IdEstablecimiento, anio, eff_global) %>%
      pivot_wider(names_from = anio, values_from = eff_global) %>%
      drop_na() # solo hospitales con datos de todos los años
    
    if(nrow(df_wide) == 0) return(NULL)
    
    matriz <- as.matrix(df_wide[,-1])
    friedman.test(matriz)
  }
  
  # ---- Función para aplicar Wilcoxon pareado con ajuste Holm ----
  aplicar_wilcoxon <- function(df, ids_validos) {
    df %>%
      filter(IdEstablecimiento %in% ids_validos) %>%
      pairwise_wilcox_test(eff_global ~ anio, paired = TRUE, p.adjust.method = "holm")
  }
  
  # =====================
  # ANÁLISIS GLOBAL
  # =====================
  friedman_global <- aplicar_friedman(datos_largos)
  
  # IdEstablecimientos válidos (con datos de todos los años)
  ids_validos_global <- datos_largos %>%
    select(IdEstablecimiento, anio) %>%
    distinct() %>%
    group_by(IdEstablecimiento) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n == length(unique(datos_largos$anio))) %>%
    pull(IdEstablecimiento)
  
  wilcoxon_global <- aplicar_wilcoxon(datos_largos, ids_validos_global)
  
  # =====================
  # ANÁLISIS POR COMPLEJIDAD
  # =====================
  niveles_complejidad <- unique(datos_largos$complejidad)
  
  friedman_por_complejidad <- list()
  wilcoxon_por_complejidad <- list()
  
  for (nivel in niveles_complejidad) {
    datos_nivel <- datos_largos %>% filter(complejidad == nivel)
    
    # Friedman
    friedman_result <- aplicar_friedman(datos_nivel)
    friedman_por_complejidad[[nivel]] <- friedman_result
    
    # IdEstablecimientos válidos
    ids_validos_nivel <- datos_nivel %>%
      select(IdEstablecimiento, anio) %>%
      distinct() %>%
      group_by(IdEstablecimiento) %>%
      summarise(n = n(), .groups = "drop") %>%
      filter(n == length(unique(datos_nivel$anio))) %>%
      pull(IdEstablecimiento)
    
    # Wilcoxon
    wilcoxon_result <- aplicar_wilcoxon(datos_nivel, ids_validos_nivel)
    wilcoxon_por_complejidad[[nivel]] <- wilcoxon_result
  }
  
  # =====================
  # EXPORTAR RESULTADOS
  # =====================
  # Convertir resultados a dataframes exportables
  friedman_export <- data.frame(
    Test = "Friedman Global",
    p_value = ifelse(is.null(friedman_global), NA, friedman_global$p.value)
  )
  
  wilcoxon_export <- if (!is.null(wilcoxon_global)) wilcoxon_global else data.frame()
  
  # Crear lista para writexl
  export_list <- list(
    "Friedman_Global" = friedman_export,
    "Wilcoxon_Global" = wilcoxon_export
  )
  
  # Agregar resultados por complejidad
  for (nivel in niveles_complejidad) {
    fried <- friedman_por_complejidad[[nivel]]
    wilco <- wilcoxon_por_complejidad[[nivel]]
    
    export_list[[paste0("Friedman_", nivel)]] <- 
      data.frame(Test = paste0("Friedman ", nivel),
                 p_value = ifelse(is.null(fried), NA, fried$p.value))
    export_list[[paste0("Wilcoxon_", nivel)]] <- 
      if (!is.null(wilco)) wilco else data.frame()
  }
  
  write_xlsx(export_list, path = output_file)
  
  # =====================
  # RETORNAR RESULTADOS
  # =====================
  return(list(
    friedman_global = friedman_global,
    wilcoxon_global = wilcoxon_global,
    friedman_por_complejidad = friedman_por_complejidad,
    wilcoxon_por_complejidad = wilcoxon_por_complejidad
  ))
}

# ===================================================
# USO DE LA FUNCIÓN
# ---------------------------------------------------
resultados_pruebas <- pruebas_friedman_wilcoxon(datos_largos)
