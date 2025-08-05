# ===================================================
# MENÚ INTERACTIVO – ANÁLISIS DE EFICIENCIA TÉCNICA
# HOSPITALES PÚBLICOS EN CHILE (SFA vs DEA)
# JOHN SERRANO CARRASCO, 2025
# ===================================================

# --- Cargar funciones de prueba ---
source("pruebas.R")

# --- Variables de control de ejecución ---
ejecuciones <- list(
  procesamiento = FALSE,
  sfa = FALSE,
  normalidad = FALSE,
  metricas = FALSE,
  sensibilidad = FALSE,
  pruebas = FALSE,
  mapas = FALSE,
  determinantes = FALSE,
  comparacion = FALSE
)

# --- Función auxiliar para medir tiempo y ejecutar pruebas ---
ejecutar_con_tiempo <- function(script, mensaje, pruebas = NULL) {
  start_time <- Sys.time()
  source(script)
  end_time <- Sys.time()
  tiempo <- round(difftime(end_time, start_time, units = "secs"), 2)
  cat(paste0(mensaje, " (Tiempo: ", tiempo, " segundos)\n"))
  
  # Ejecutar pruebas si se definieron
  if (!is.null(pruebas)) pruebas()
}

# --- Función para verificar qué pasos faltan ---
pasos_faltantes <- function() {
  faltan <- names(ejecuciones)[!unlist(ejecuciones)]
  if (length(faltan) == 0) return(NULL)
  return(faltan)
}

# --- Menú principal ---
menu_principal <- function() {
  repeat {
    cat("\n",
        "==============================================\n",
        "  ANÁLISIS DE EFICIENCIA TÉCNICA - MENU\n",
        "==============================================\n",
        "1. Instalar dependencias\n",
        "2. Procesamiento de datos (con pruebas)\n",
        "3. Modelos SFA (con pruebas)\n",
        "4. Normalidad de eficiencias\n",
        "5. Métricas SFA (con pruebas)\n",
        "6. Análisis de sensibilidad\n",
        "7. Pruebas estadísticas (Friedman / Wilcoxon) (con pruebas)\n",
        "8. Mapas de eficiencia (con pruebas)\n",
        "9. Determinantes de eficiencia (Random Forest) (con pruebas)\n",
        "10. Comparación SFA vs DEA (con pruebas)\n",
        "11. Generar informe PDF (requiere todas las opciones previas)\n",
        "0. Salir\n",
        "----------------------------------------------\n")
    
    opcion <- readline(prompt = "Seleccione una opción (0-11): ")
    
    if (opcion == "0") {
      cat("Saliendo del programa...\n")
      break
    }
    
    switch(opcion,
           "1"  = ejecutar_con_tiempo("dependencias.R", 
                                      "Instalación de dependencias completada."),
           "2"  = { ejecutar_con_tiempo("procesamiento_datos.R", 
                                        "Procesamiento de datos completado.", 
                                        pruebas = pruebas_datos)
             ejecuciones$procesamiento <- TRUE },
           "3"  = { ejecutar_con_tiempo("SFA.R", 
                                        "Modelos SFA ejecutados.", 
                                        pruebas = pruebas_sfa)
             ejecuciones$sfa <- TRUE },
           "4"  = { ejecutar_con_tiempo("normalidad_SFA.R", 
                                        "Análisis de normalidad completado.")
             ejecuciones$normalidad <- TRUE },
           "5"  = { ejecutar_con_tiempo("metricas_SFA.R", 
                                        "Métricas de eficiencia calculadas.", 
                                        pruebas = pruebas_metricas)
             ejecuciones$metricas <- TRUE },
           "6"  = { ejecutar_con_tiempo("sensibilidad.R", 
                                        "Análisis de sensibilidad completado.")
             ejecuciones$sensibilidad <- TRUE },
           "7"  = { ejecutar_con_tiempo("pruebas_estadisticas.R", 
                                        "Pruebas estadísticas ejecutadas.", 
                                        pruebas = pruebas_pruebas_estadisticas)
             ejecuciones$pruebas <- TRUE },
           "8"  = { ejecutar_con_tiempo("mapas_eficiencia.R", 
                                        "Mapas de eficiencia generados.", 
                                        pruebas = pruebas_mapas)
             ejecuciones$mapas <- TRUE },
           "9"  = { ejecutar_con_tiempo("determinantes.R", 
                                        "Determinantes de eficiencia calculados.", 
                                        pruebas = pruebas_determinantes)
             ejecuciones$determinantes <- TRUE },
           "10" = { ejecutar_con_tiempo("comparacion_SFA_DEA.R", 
                                        "Comparación SFA vs DEA completada.", 
                                        pruebas = pruebas_comparacion_sfa_dea)
             ejecuciones$comparacion <- TRUE },
           "11" = {
             faltan <- pasos_faltantes()
             if (is.null(faltan)) {
               cat("Generando informe... (esto puede tardar unos segundos)\n")
               # Guardar datos para el informe
               save(datos_procesados,
                    datos,
                    resultado_corregido,
                    resultados_importancia,
                    random_forest,
                    resultados_por_anio,
                    file = "informe.RDATA")
               cat("Datos guardados en informe.RDATA\n")
               # Renderizar informe
               rmarkdown::render("informe_eficiencia.Rmd", output_file = "Informe_Eficiencia.pdf")
               cat("Informe generado correctamente: Informe_Eficiencia.pdf\n")
             } else {
               cat("No puedes generar el informe aún. Faltan ejecutar:\n")
               cat(paste("-", faltan, collapse="\n"), "\n")
             }
           },
           cat("Opción no válida. Por favor intente nuevamente.\n")
    )
  }
}

# Ejecutar el menú
menu_principal()
