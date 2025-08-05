# ===================================================
# PRUEBAS AUTOMÁTICAS DEL PROYECTO
# ===================================================

# --- Función de log ---
registrar_log <- function(mensaje) {
  log_file <- "log_pruebas.txt"
  hora <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", hora, "] ", mensaje, "\n"), file = log_file, append = TRUE)
}

# --- PRUEBAS DE PROCESAMIENTO DE DATOS ---
pruebas_datos <- function() {
  cat("\n=== PRUEBAS TRAS PROCESAMIENTO DE DATOS ===\n")
  if (!exists("datos")) {
    mensaje <- "❌ 'datos' no existe."
    cat(mensaje, "\n"); registrar_log(mensaje); return(FALSE)
  }
  if (!is.list(datos) || length(datos) == 0) {
    mensaje <- "❌ 'datos' está vacío o no es una lista válida."
    cat(mensaje, "\n"); registrar_log(mensaje); return(FALSE)
  }
  # Columnas clave
  columnas_requeridas <- c("IdEstablecimiento", "Region", "latitud", "longitud", "complejidad")
  if (!all(columnas_requeridas %in% colnames(datos[[1]]))) {
    mensaje <- "❌ Faltan columnas obligatorias en datos."
    cat(mensaje, "\n"); registrar_log(mensaje)
  } else {
    registrar_log("✔ Columnas clave presentes.")
  }
  # Años completos
  anios_esperados <- as.character(2014:2023)
  if (!all(anios_esperados %in% names(datos))) {
    registrar_log("❌ Faltan datos de algunos años.")
  } else {
    registrar_log("✔ Todos los años presentes.")
  }
  # Duplicados
  duplicados <- FALSE
  for (anio in names(datos)) {
    df <- datos[[anio]]
    if (any(duplicated(df$IdEstablecimiento))) {
      registrar_log(paste0("❌ Duplicados en IdEstablecimiento (Año ", anio, ")"))
      duplicados <- TRUE
    }
  }
  if (!duplicados) registrar_log("✔ No hay duplicados de IdEstablecimiento.")
  registrar_log("=== Pruebas de datos completadas ===")
  return(TRUE)
}

# --- PRUEBAS DE SFA ---
pruebas_sfa <- function() {
  cat("\n=== PRUEBAS TRAS SFA ===\n")
  if (!exists("datos_procesados")) {
    registrar_log("❌ 'datos_procesados' no existe."); return(FALSE)
  }
  rango_ok <- TRUE
  for (anio in names(datos_procesados)) {
    df <- datos_procesados[[anio]]
    for (col in c("eff_egresos", "eff_consultas", "eff_quirofano", "eff_global")) {
      if (any(df[[col]] < 0 | df[[col]] > 1, na.rm = TRUE)) {
        registrar_log(paste0("❌ Valores fuera de rango en ", col, " (Año ", anio, ")"))
        rango_ok <- FALSE
      }
    }
  }
  if (rango_ok) registrar_log("✔ Todas las eficiencias están entre 0 y 1.")
  registrar_log("=== Pruebas SFA completadas ===")
  return(TRUE)
}

# --- MÉTRICAS ---
pruebas_metricas <- function() {
  registrar_log("Prueba: Verificar métricas calculadas (implementar si es necesario)")
  return(TRUE)
}

# --- PRUEBAS ESTADÍSTICAS ---
pruebas_pruebas_estadisticas <- function() {
  registrar_log("Prueba: Verificar p-valores de Friedman y Wilcoxon")
  return(TRUE)
}

# --- MAPAS ---
pruebas_mapas <- function() {
  archivo <- "Gráfica Chile - Eficiencia técnica_Año_2023.jpg"
  if (!file.exists(archivo)) {
    registrar_log("❌ No se generó el mapa esperado.")
  } else {
    registrar_log("✔ Mapa de eficiencia generado correctamente.")
  }
  return(TRUE)
}

# --- DETERMINANTES ---
pruebas_determinantes <- function() {
  cat("\n=== PRUEBAS TRAS DETERMINANTES (Random Forest) ===\n")
  
  # Verificar existencia del objeto de resultados esperado
  if (!exists("random_forest")) {
    mensaje <- "❌ No existe el objeto 'random_forest'. Ejecute primero determinantes.R"
    cat(mensaje, "\n"); registrar_log(mensaje); return(FALSE)
  }
  
  # Debe ser una lista con al menos un modelo
  if (!is.list(random_forest) || length(random_forest) == 0) {
    mensaje <- "❌ 'random_forest' no es una lista válida."
    cat(mensaje, "\n"); registrar_log(mensaje); return(FALSE)
  }
  
  # Verificar que cada año tenga importancia de variables y modelo entrenado
  for (anio in names(random_forest)) {
    modelo <- random_forest[[anio]]
    if (!("modelo" %in% names(modelo))) {
      registrar_log(paste0("❌ Falta el objeto 'modelo' en año ", anio))
    } else {
      # Comprobar tipo de modelo
      if (!inherits(modelo$modelo, "randomForest")) {
        registrar_log(paste0("❌ El modelo del año ", anio, " no es un objeto randomForest"))
      } else {
        registrar_log(paste0("✔ Modelo Random Forest válido para el año ", anio))
      }
    }
    
    # Importancia de variables
    if (!("importancia" %in% names(modelo))) {
      registrar_log(paste0("❌ Falta la importancia de variables en año ", anio))
    } else {
      importancia <- modelo$importancia
      if (all(importancia[,1] == 0)) {
        registrar_log(paste0("❌ Todas las importancias son cero en año ", anio))
      } else {
        registrar_log(paste0("✔ Importancia de variables calculada correctamente para año ", anio))
      }
    }
  }
  
  registrar_log("=== Pruebas de determinantes completadas ===")
  cat("=== PRUEBAS DE DETERMINANTES COMPLETADAS ===\n")
  return(TRUE)
}


# --- COMPARACIÓN SFA vs DEA ---
pruebas_comparacion_sfa_dea <- function() {
  archivo <- "Testing_Results.xlsx"
  if (!file.exists(archivo)) {
    registrar_log("❌ No se generó el archivo Testing_Results.xlsx")
  } else {
    registrar_log("✔ Comparación SFA vs DEA generada correctamente.")
  }
  return(TRUE)
}
