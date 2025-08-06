# ===================================================
# ANÁLISIS DE EFICIENCIA TÉCNICA DE HOSPITALES PÚBLICOS 
# EN CHILE USANDO SFA Y DEA
# JOHN SERRANO CARRASCO, 2025
# ===================================================

source("src/pruebas.R")

# ===================================================
# TIEMPO DE EJECUCIÓN Y PRUEBAS DE VALIDACIÓN
# ===================================================
ejecutar_con_tiempo <- function(script, mensaje, pruebas = NULL) {
  start_time <- Sys.time()
  source(script)
  end_time <- Sys.time()
  tiempo <- round(difftime(end_time, start_time, units = "secs"), 2)
  cat(paste0(mensaje, " (Tiempo: ", tiempo, " segundos)\n"))
  if (!is.null(pruebas)) pruebas()
}


# ===================================================
# MENÚ INTERACTIVO POR CONSOLA
# ===================================================
menu_principal <- function() {
  repeat {
    cat("\n",
        "==============================================\n",
        "  ANÁLISIS DE EFICIENCIA TÉCNICA - MENU\n",
        "==============================================\n",
        "1. Instalar dependencias\n",
        "2. Procesamiento de datos\n",
        "3. Creación y ejecución de modelos SFA\n",
        "4. Normalidad de eficiencias\n",
        "5. Cálcular métricas de SFA\n",
        "6. Análisis de sensibilidad\n",
        "7. Pruebas estadísticas (Friedman / Wilcoxon)\n",
        "8. Mapas de eficiencia de Chile\n",
        "9. Determinantes de eficiencia (Random Forest)\n",
        "10. Comparación SFA vs DEA\n",
        "11. Generar informe PDF con resultados\n",
        "0. Salir\n",
        "----------------------------------------------\n")
    
    opcion <- readline(prompt = "Seleccione una opción (0-11): ")
    
    if (opcion == "0") {
      cat("Saliendo del programa...\n")
      break
    } else if (opcion == "1") {
      ejecutar_con_tiempo("src/dependencias.R", "Instalación de dependencias completada.")
    } else if (opcion == "2") {
      ejecutar_con_tiempo("src/procesamiento_datos.R", "Procesamiento de datos completado.", pruebas = pruebas_datos)
    } else if (opcion == "3") {
      ejecutar_con_tiempo("src/SFA.R", "Modelos SFA ejecutados.", pruebas = pruebas_sfa)
    } else if (opcion == "4") {
      ejecutar_con_tiempo("src/normalidad_SFA.R", "Análisis de normalidad completado.")
    } else if (opcion == "5") {
      ejecutar_con_tiempo("src/metricas_SFA.R", "Métricas de eficiencia calculadas.", pruebas = pruebas_metricas)
    } else if (opcion == "6") {
      ejecutar_con_tiempo("src/sensibilidad.R", "Análisis de sensibilidad completado.")
    } else if (opcion == "7") {
      ejecutar_con_tiempo("src/pruebas_estadisticas.R", "Pruebas estadísticas ejecutadas.")
    } else if (opcion == "8") {
      ejecutar_con_tiempo("src/mapas_eficiencia.R", "Mapas de eficiencia generados.", pruebas = pruebas_mapas)
    } else if (opcion == "9") {
      ejecutar_con_tiempo("src/determinantes.R", "Determinantes de eficiencia calculados.", pruebas = pruebas_determinantes)
    } else if (opcion == "10") {
      archivo_dea_global   <<- readline(prompt = "Ingrese el nombre del archivo DEA (con extensión): ")
      archivo_salida_global <<- readline(prompt = "Ingrese el nombre del archivo de salida (por defecto Resultados.xlsx): ")
      if (archivo_salida_global == "") archivo_salida_global <- "Resultados/Comparacion/Resultados.xlsx"
      
      ejecutar_con_tiempo("src/comparacion_SFA_DEA.R", 
                          paste("Comparación SFA vs DEA usando", archivo_dea_global, "y guardando en", archivo_salida_global),
                          pruebas = pruebas_comparacion_sfa_dea)
    } else if (opcion == "11") {
      cat("Generando informe... (esto puede tardar unos segundos)\n")
      save(datos_procesados, datos, resultados_metricas,
           resultados_importancia, random_forest, tabla_resultados, datos_largos,
           file = "Resultados/Informes/informe.RDATA")
      cat("Datos guardados en informe.RDATA\n")
      rmarkdown::render("informe_eficiencia.Rmd", output_file = "Resultados/Informes/Informe_Eficiencia.pdf")
      cat("Informe generado correctamente: Informe_Eficiencia.pdf\n")
    } else {
      cat("Opción no válida. Por favor intente nuevamente.\n")
    }
  }
}


# Ejecutar el menú
menu_principal()
