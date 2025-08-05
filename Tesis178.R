# Tesis: Análisis de eficiencia técnica de hospitales públicos en Chile usando
# SFA y DEA
# Autor: John Serrano Carrasco

# ===================================================
# IMPORTACIÓN DE LIBRERÍAS
# ===================================================

library(frontier)
library(dplyr)
library(openxlsx)
library(readxl)
library(scatterplot3d)
library(rgl)
library(tidyr)
library(purrr)
library(randomForest)
library(caret)
library(Metrics)
library(RColorBrewer)
library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)
library(gridExtra)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(tibble)
#library(deaR)
# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================

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


# ============================================================
#  Función:  check_sfa_diagnostics()
#  Qué hace:
#    1) Calcula   ê  (residuals)
#    2) Calcula   û  (ineficiencia estimada)   y  v̂ = ê + û
#    3) Muestra:
#         • Histograma de ê
#         • QQ-plot + Shapiro de v̂   (≈ ruido)
#         • Histograma de û           (debe ser positiva y sesgada dcha.)
#         • Valor medio de ê
# ============================================================
# ============================================================
#  Función: check_sfa_diagnostics()
#  Diagnósticos básicos para un modelo SFA de 'frontier'
#     • Histograma de ê  (residual total)
#     • Histograma de û   (ineficiencia)
#     • QQ-plot + Shapiro de v̂  (ruido ≈ Normal)
#     • Promedio de ê
# ============================================================
check_sfa_diagnostics <- function(mod_sfa) {

  # ---------- 1. residuales totales ê ----------
  e_hat <- residuals(mod_sfa)
  
  # ---------- 2. ineficiencia û  ----------------
  eff_hat <- as.numeric(efficiencies(mod_sfa))   # exp(−û)
  u_hat   <- -log(eff_hat)                       # û ≥ 0
  
  # ---------- 3. ruido v̂  ----------------------
  v_hat <- e_hat + u_hat                         # v̂ debería ~ N(0,σ_v²)
  
  # ---------- 4. gráficos -----------------------
  p_e <- ggplot(data.frame(e_hat = e_hat),
                aes(x = e_hat)) +
    geom_histogram(fill = "skyblue", colour = "black", bins = 30) +
    labs(title = "Histograma ê (residual total)",
         x = "ê", y = "Frecuencia") +
    theme_minimal()
  
  p_u <- ggplot(data.frame(u_hat = u_hat),
                aes(x = u_hat)) +
    geom_histogram(fill = "orange", colour = "black", bins = 30) +
    labs(title = "Histograma û (ineficiencia)",
         x = "û", y = "Frecuencia") +
    theme_minimal()
  
  p_qq <- ggplot(data.frame(v_hat = v_hat),
                 aes(sample = v_hat)) +        #  <-- sample = v_hat
    stat_qq() +
    stat_qq_line(colour = "red") +
    labs(title = "QQ-plot v̂ (ruido)",
         x = "Cuantiles teóricos N(0,1)",
         y = "Cuantiles v̂") +
    theme_minimal()
  
  # ---------- 5. Shapiro-Wilk para v̂ -----------
  if (length(v_hat) <= 5000) {
    cat("\n--- Shapiro-Wilk para v̂ (n =", length(v_hat), ") ---\n")
    print(shapiro.test(v_hat))
  } else {
    cat("\n--- Muestra > 5000: Shapiro-Wilk no aplicable ---\n")
  }
  
  # ---------- 6. Promedio de ê -----------------
  cat("\nPromedio(ê) = ", mean(e_hat), "\n")
  
  # ---------- 7. Mostrar plots juntos ----------
  gridExtra::grid.arrange(p_e, p_u, p_qq, ncol = 2)
  
  invisible(list(e_hat = e_hat, u_hat = u_hat, v_hat = v_hat))
}

# --------------------- Ejemplo ---------------------
# mod_sfa <- sfa(log(Y) ~ log(X1) + log(X2), data = df)
# check_sfa_diagnostics(mod_sfa)


# ----------------- Ejemplo de uso -----------------
# mod_sfa <- sfa(log(Y) ~ log(X1) + log(X2), data = df)
f <- check_sfa_diagnostics(mod_egresos)


# ==============================================
#  MODELOS SFA PARA TODOS LOS AÑOS
# ==============================================

df <- datos[["2014"]]

procesar_sfa <- function(df) {
  # ---- Modelo Egresos ----
  mod_egresos <- sfa(
    #formula = log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    formula = log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) + I(0.5 * log(X21_value + 1)^2) + I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) + I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1)),
    data    = df
  )
  eff_egresos <- efficiencies(mod_egresos)
  
  
  #logLik(mod_egresos)
  #AIC(mod_egresos)
  #BIC(mod_egresos)
  
  #resid_sfa <- residuals(mod_egresos)   # vector, una entrada por observación
  
  #head(resid_sfa)p
  
  # ---- Modelo Consultas ----
  mod_consultas <- sfa(
    #formula = log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    formula = log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) + I(0.5 * log(X21_value + 1)^2) + I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) + I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1)),
    data    = df
  )
  eff_consultas <- efficiencies(mod_consultas)
  
  # ---- Modelo Quirofano ----
  mod_quirofano <- sfa(
    #formula = log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
    formula = log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) + I(0.5 * log(X21_value + 1)^2) + I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) + I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1)),
    data    = df
  )
  eff_quirofano <- efficiencies(mod_quirofano)
  
  # Agregamos esas columnas al data frame
  df_nuevo <- df %>%
    mutate(
      eff_egresos   = eff_egresos,
      eff_consultas = eff_consultas,
      eff_quirofano = eff_quirofano,
      dist_ideal = sqrt(
        (1 - eff_egresos)^2 +
          (1 - eff_consultas)^2 +
          (1 - eff_quirofano)^2
      ),
      eff_global = round(1 - dist_ideal / sqrt(3),3)
    )
  
  return(df_nuevo)
}

# datos_procesados tienen los resultados de eficiencia por año
datos_procesados <- lapply(datos, procesar_sfa)

# ===================================================
# VERIFICAR NORMALIDAD DE EFICIENCIAS
# ===================================================

library(tidyverse)
library(patchwork)   # instalar con install.packages("patchwork")

analizar_normalidad_eff_con_QQ <- function(datos_lista, ncol = 2, guardar = TRUE,
                                           archivo = "histogramas_eficiencia.png",
                                           ancho = 18, alto = 10, dpi = 300) {
  
  grafs_hist <- list()   # aquí se irán guardando los histogramas
  resultados <- list()   # p-values
  
  for (anio in names(datos_lista)) {
    
    df <- datos_lista[[anio]]
    
    # Histograma
    p_hist <- ggplot(df, aes(x = eff_global)) +
      geom_histogram(bins = 30, color = "black", fill = "lightblue") +
      labs(title = paste("Histograma eficiencia –", anio),
           x = "Eficiencia", y = "Frecuencia") +
      theme_minimal(base_size = 12)
    
    grafs_hist[[anio]] <- p_hist   # guardar
    
    # Shapiro–Wilk si procede
    if (dplyr::between(nrow(df), 3, 5000)) {
      pvalor <- shapiro.test(df$eff_global)$p.value
    } else {
      pvalor <- NA
    }
    resultados[[anio]] <- pvalor
  }
  
  # --- Juntar todos los histogramas ---
  plot_final <- wrap_plots(grafs_hist, ncol = ncol) +
    plot_annotation(title = "Distribución de eficiencia por año",
                    theme = theme(plot.title = element_text(size = 18, face = "bold")))
  
  # Guardar si se pidió
  if (guardar) {
    ggsave(archivo, plot_final, width = ancho, height = alto, dpi = dpi)
  }
  
  # Devolver p-values y el plot por si se quiere mostrar en consola
  lista_salida <- list(
    tabla_pvalues = tibble(
      Año = names(resultados),
      p_value_Shapiro = unlist(resultados)
    ),
    grafico = plot_final
  )
  return(lista_salida)
}

res <- analizar_normalidad_eff_con_QQ(
  datos_lista     = datos_procesados,
  ncol            = 3,                 # número de columnas en la cuadrícula
  archivo         = "histos_normalidad.png",
  ancho           = 20,                # aumenta para más separación
  alto            = 12,
  dpi             = 300
)

# Tabla de p-values
res$tabla_pvalues

# Ver el gráfico en la sesión interactiva
res$grafico


analizar_normalidad_eff_con_QQ <- function(datos_lista) {
  
  resultados <- list()
  
  for (anio in names(datos_lista)) {
    
    df <- datos_lista[[anio]]
    
    # Histograma
    p_hist <- ggplot(df, aes(x = eff_global)) +
      geom_histogram(bins = 30, color = "black", fill = "lightblue") +
      labs(title = paste("Histograma Eficiencia -", anio),
           x = "Eficiencia", y = "Frecuencia") +
      theme_minimal()
    
    # QQ-Plot
    p_qq <- ggplot(df, aes(sample = eff_global)) +
      stat_qq() +
      stat_qq_line(col = "red") +
      labs(title = paste("QQ-Plot Eficiencia -", anio),
           x = "Cuantiles teóricos (Normal)", y = "Cuantiles de Eficiencia") +
      theme_minimal()
    
    # Mostrar ambos juntos
    gridExtra::grid.arrange(p_hist, p_qq, ncol = 2)
    
    # Prueba de Shapiro-Wilk
    if (nrow(df) >= 3 && nrow(df) <= 5000) {
      prueba <- shapiro.test(df$eff_global)
      pvalor <- prueba$p.value
    } else {
      pvalor <- NA
    }
    
    resultados[[anio]] <- pvalor
  }
  
  # Devolver tabla de resultados
  tibble::tibble(
    Año = names(resultados),
    p_value_Shapiro = unlist(resultados)
  )
}

tabla_resultados <- analizar_normalidad_eff_con_QQ(datos_procesados)

# Los datos NO son normales en ninguno de sus años

# ===================================================
# OUTLIERS + ESTADISTICAS OUTLIERS
# ===================================================

analisis_sensibilidad_outliers_por_anno <- function(datos_lista, nombre_categoria = "complejidad") {
  
  es_outlier <- function(x) {
    x <- as.vector(x)
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lim_inf <- q[1] - 1.5 * iqr
    lim_sup <- q[2] + 1.5 * iqr
    x < lim_inf | x > lim_sup
  }
  
  resumen_metricas <- list()
  datos_sin_outliers <- list()
  
  for (anio in names(datos_lista)) {
    
    df <- datos_lista[[anio]]
    
    # Detectar outliers por todo el año
    df <- df %>%
      mutate(outlier = es_outlier(eff_global))
    
    # Guardar versión limpia
    df_clean <- df %>% filter(!outlier)
    datos_sin_outliers[[anio]] <- df_clean
    
    # Calcular métricas antes y después por categoría
    resumen_ano <- df %>%
      group_by(.data[[nombre_categoria]]) %>%
      summarise(
        Año = anio,
        n_total = n(),
        n_outliers = sum(outlier),
        media_original = mean(eff_global, na.rm = TRUE),
        media_limpia   = mean(eff_global[!outlier], na.rm = TRUE),
        mediana_original = median(eff_global, na.rm = TRUE),
        mediana_limpia   = median(eff_global[!outlier], na.rm = TRUE),
        var_original = var(eff_global, na.rm = TRUE),
        var_limpia = var(eff_global[!outlier], na.rm = TRUE),
        sd_original = sd(eff_global, na.rm = TRUE),
        sd_limpia   = sd(eff_global[!outlier], na.rm = TRUE),
        .groups = "drop"
      )
    
    resumen_metricas[[anio]] <- resumen_ano
  }
  
  resumen_final <- bind_rows(resumen_metricas)
  
  list(
    resumen_metricas = resumen_final,
    datos_sin_outliers = datos_sin_outliers
  )
}

resultado_corregido <- analisis_sensibilidad_outliers_por_anno(datos_procesados, nombre_categoria = "complejidad")

datos_sin_outlier <- resultado_corregido$datos_sin_outliers

#nuevos_datos_procesados <- lapply(datos_sin_outlier, procesar_sfa)

# ===================================================
# PRUEBA DE WILCOXON PARA SABER SI OUTLIERS AFECTAN
# ===================================================

aplicar_wilcoxon_por_categoria <- function(df_metricas) {
  
  df_resultado <- df_metricas %>%
    rowwise() %>%
    mutate(
      p_value = tryCatch(
        wilcox.test(
          x = c(media_original),
          y = c(media_limpia),
          paired = TRUE
        )$p.value,
        error = function(e) NA
      ),
      interpretacion = case_when(
        is.na(p_value) ~ "No aplica (sin outliers)",
        p_value < 0.05 ~ "Diferencia significativa",
        TRUE           ~ "Sin diferencia significativa"
      )
    ) %>%
    ungroup()
  
  return(df_resultado)
}

resultado_wilcoxon <- aplicar_wilcoxon_por_categoria(resultado_corregido$resumen_metricas)
print(resultado_wilcoxon)

# ===================================================
# ANALISIS DE SENSIBILIDAD
# ===================================================

# Fórmulas para cada salida
formula_egresos   <- log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)
formula_consultas <- log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)
formula_quirofano <- log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)

sensibilidad_resultados <- list()

for (anio in names(datos)) {
  cat("\n=======\nAÑO", anio, "\n=======\n")
  
  df_ori <- datos[[anio]]
  df_proc <- datos_procesados[[anio]]
  
  # Extraer eficiencias originales
  eff_egresos_ori   <- df_proc$eff_egresos
  eff_consultas_ori <- df_proc$eff_consultas
  eff_quirofano_ori <- df_proc$eff_quirofano
  eff_global_ori    <- df_proc$eff_global
  
  # Identificar outliers en eficiencia global
  Q1 <- quantile(eff_global_ori, 0.25, na.rm = TRUE)
  Q3 <- quantile(eff_global_ori, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  no_outliers <- eff_global_ori >= lim_inf & eff_global_ori <= lim_sup
  
  cat("N hospitales:", nrow(df_proc), " | Outliers:", sum(!no_outliers), "\n")
  
  # Filtrar sin outliers
  df_sin_out <- df_ori[no_outliers, ]
  
  # Recalcular eficiencias SFA (sin outliers)
  mod_egresos   <- sfa(formula_egresos,   data = df_sin_out)
  mod_consultas <- sfa(formula_consultas, data = df_sin_out)
  mod_quirofano <- sfa(formula_quirofano, data = df_sin_out)
  
  eff_egresos_new    <- as.numeric(efficiencies(mod_egresos))
  eff_consultas_new  <- as.numeric(efficiencies(mod_consultas))
  eff_quirofano_new  <- as.numeric(efficiencies(mod_quirofano))
  eff_global_new     <- 1 - sqrt((1-eff_egresos_new)^2 + (1-eff_consultas_new)^2 + (1-eff_quirofano_new)^2) / sqrt(3)
  
  # Emparejar por IdEstablecimiento
  comp_df <- data.frame(
    IdEstablecimiento = df_sin_out$IdEstablecimiento,
    #eff_egresos_ori = eff_egresos_ori[no_outliers],
    #eff_egresos_new = eff_egresos_new,
    #eff_consultas_ori = eff_consultas_ori[no_outliers],
    #ff_consultas_new = eff_consultas_new,
    #eff_quirofano_ori = eff_quirofano_ori[no_outliers],
    # eff_quirofano_new = eff_quirofano_new,
    eff_global_ori = eff_global_ori[no_outliers],
    eff_global_new = eff_global_new
  )
  
  # Calcular correlaciones
  #cor_egresos   <- cor(comp_df$eff_egresos_ori,   comp_df$eff_egresos_new)
  #cor_consultas <- cor(comp_df$eff_consultas_ori, comp_df$eff_consultas_new)
  #  cor_quirofano <- cor(comp_df$eff_quirofano_ori, comp_df$eff_quirofano_new)
  cor_global    <- cor(comp_df$eff_global_ori,    comp_df$eff_global_new)
  
  cat("Correlaciones:\n",
    #  "- Egresos:   ", round(cor_egresos,   3), "\n",
    #  "- Consultas: ", round(cor_consultas, 3), "\n",
    #  "- Quirófano: ", round(cor_quirofano, 3), "\n",
      "- Global:    ", round(cor_global,    3), "\n")
  
  sensibilidad_resultados[[anio]] <- list(
    outliers = which(!no_outliers),
    n_outliers = sum(!no_outliers),
    comp_df = comp_df,
    correlacion_global = cor_global,
    #correlacion_egresos = cor_egresos,
    #correlacion_consultas = cor_consultas,
    #correlacion_quirofano = cor_quirofano,
    summary_ori = summary(comp_df$eff_global_ori),
    summary_new = summary(comp_df$eff_global_new)
  )
}

# Paso 1: Extrae por año el IdEstablecimiento y la eficiencia recalculada
efic_global_new_por_ano <- lapply(names(sensibilidad_resultados), function(anio) {
  df <- sensibilidad_resultados[[anio]]$comp_df %>%
    select(IdEstablecimiento, eff_global_new)
  df$Anio <- anio
  df
})

names(efic_global_new_por_ano)  <- paste0(anios)

# Paso 2: Junta todos los años en un solo dataframe largo
#efic_global_new_long <- bind_rows(efic_global_new_por_ano)

# Paso 3: Llévalo a formato wide (hospitales como filas, años como columnas)
#efic_global_new_wide <- efic_global_new_long %>%
#  pivot_wider(
#    names_from = Anio,
#    values_from = eff_global_new,
#    names_prefix = "Eficiencia_"
#  )

# Suponiendo que ejecutaste el ciclo anterior...
correlaciones_por_anio <- data.frame(
  Anio = names(sensibilidad_resultados),
  Correlacion_Global = sapply(sensibilidad_resultados, function(x) x$correlacion_global)
)

print(correlaciones_por_anio)


eficiencia_por_hospital_ano <- lapply(names(sensibilidad_resultados), function(anio) {
  comp_df <- sensibilidad_resultados[[anio]]$comp_df
  comp_df$Anio <- anio
  # Selecciona solo ID y los dos valores de eficiencia global
  comp_df %>%
    select(IdEstablecimiento, Anio, eff_global_ori, eff_global_new)
})

# Unimos todo en un solo dataframe
eficiencia_por_hospital_ano <- bind_rows(eficiencia_por_hospital_ano)

#correlacion_por_hospital <- eficiencia_por_hospital_ano %>%
#  group_by(IdEstablecimiento) %>%
#  summarise(
#    n_anos = n(),
#    correlacion = ifelse(
#      n_anos > 1,
#      cor(eff_global_ori, eff_global_new, use = "complete.obs"),
#      NA_real_
#    )
#  )

# ===================================================
# PASAR EFICIENCIAS A EXCEL
# ===================================================

df_ref <- datos_procesados[["2014"]] %>%
  select(IdEstablecimiento, NombreHospital = `Nombre Establecimiento`, complejidad) %>%
  distinct() 


df_long <- bind_rows(
  lapply(names(datos_procesados), function(year_name) {
    df_year <- datos_procesados[[year_name]]
    
    df_year %>%
      select(IdEstablecimiento, eff_global) %>%
      mutate(Anio = year_name)
  })
)


df_long <- df_long %>%
  left_join(df_ref, by = "IdEstablecimiento")

df_wide <- df_long %>%
  pivot_wider(
    id_cols      = c(IdEstablecimiento, NombreHospital, complejidad),
    names_from   = Anio,
    values_from  = eff_global,
    names_prefix = "Eficiencia_"
  )


# Exportar a Excel
wb <- createWorkbook()
addWorksheet(wb, "Eficiencias")
writeData(wb, "Eficiencias", df_wide, rowNames = FALSE)
setColWidths(wb, sheet = "Eficiencias", cols = 1:50, widths = "auto")
saveWorkbook(wb, "Eficiencias_OtraFuncion.xlsx", overwrite = TRUE)

##########################################################

# ===================================================
# ANALISIS DE DETERMINANTES
# ===================================================

# USAR PARA LA VERSIÓN CON OUTLIERS
datos_min <- map(
  datos_procesados,                           
  ~ .x %>%                                  
    select(idEstablecimiento = IdEstablecimiento,
           eff_global) %>%
    distinct()                             
)

# USAR PARA LA VERSIÓN SIN OUTLIERS
#resultados_outliers <- resultado_corregido[["datos_sin_outliers"]]

datos_min <- map(
  efic_global_new_por_ano,                           
  ~ .x %>%                                  
    select(idEstablecimiento = IdEstablecimiento,
           eff_global_new) %>%
    distinct()                             
)

# =======================
#  RANDOM FOREST
# =======================

analize_rf <- function(year, resultados_in, n_top, tipo){
  #year <- 2014
  print("---------------------------")
  print(paste0("AÑO: ", year))
  
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  
  #print(data_path)
  # Leer los datos consolidados
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  colnames(datos_consolidados) <- gsub("\\.0$", "", colnames(datos_consolidados))
  
  df <- datos_consolidados
  
  
  # Convertir columnas a enteros
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # Filtrar los resultados de VRS
  df_vrs <- resultados_in[[as.character(year)]]
  
  
  df_w_vrs <- df %>%
    filter(idEstablecimiento %in% df_vrs$idEstablecimiento)
  
  # Combinar los DataFrames
  df_merged_original <- merge(df_w_vrs, df_vrs, by = "idEstablecimiento", all.x = TRUE)
  df_merged_clean <- df_merged_original[, colSums(is.na(df_merged_original)) == 0]
  
  
  correlaciones <- cor(df_merged_clean[,-1])[tipo, ]
  correlaciones <- correlaciones[!names(correlaciones) %in% tipo]
  correlaciones_ordenadas <- sort(abs(correlaciones), decreasing = TRUE)
  
  top_correlacion <- head(correlaciones_ordenadas, n=n_top)
  top_variables <- names(top_correlacion)
  
  
  columnas_a_incluir <- c(tipo, top_variables)
  
  # Crear el DataFrame con las variables seleccionadas
  df_top <- df_merged_clean[, columnas_a_incluir]
  
  
  # PROBANDO RANDOM FOREST
  
  set.seed(123)  # Para reproducibilidad
  
  trainIndex <- createDataPartition(df_top[[tipo]], p = 0.7, list = FALSE)
  
  trainData <- df_top[trainIndex, ]
  testData <- df_top[-trainIndex, ]
  
  control <- trainControl(method = "cv", number = 10)  # 10-fold CV
  formula <- as.formula(paste(tipo, "~ ."))
  
  # Ajustar el modelo de Random Forest
  modelo_rf <- randomForest(formula, 
                            data = trainData, 
                            importance = TRUE, 
                            trControl = control, 
                            ntree = 700,
                            do.trace = 100 )
  
  
  # Predicciones en el conjunto de prueba
  predicciones <- predict(modelo_rf, newdata = testData)
  
  # Evaluar el rendimiento
  r2 <- R2(predicciones, testData[[tipo]])
  rmse <- rmse(predicciones, testData[[tipo]])
  cat("R²:", r2, "\nRMSE:", rmse)
  
  # Importancia de las variables
  importancia <- importance(modelo_rf)
  #importancia_2 = varImp(modelo_rf)
  
  
  print("---------------------------")
  print("---------------------------")
  
  return(list(importancia = importancia,
  #            importancia_2 = importancia_2,
              modelo = modelo_rf,
              correlaciones = top_correlacion))
  print("Completado!")
}

# Aplicar Random Forest para cada año
random_forest <- lapply(anios, function(anio) {analize_rf(anio, resultados_in = datos_min, 500, "eff_global")})
# Asignar nombres a la lista de modelos
names(random_forest) <- paste0(anios)

# ===============================
#  EXTRACCIÓN DE VARIABLES POR AÑO
# ===============================

# Función para calcular frecuencia y mediana
calcular_estadisticas <- function(df) {
  df$Frecuencia <- rowSums(!is.na(df[,-1]))  # Cuenta cuántos años tiene datos
  df$Mediana <- apply(df[, -c(1, ncol(df))], 1, median, na.rm = TRUE)  # Mediana ignorando NA
  return(df)
}

# Función para generar el dataframe con años como columnas
crear_dataframe <- function(lista_metrica) {
  df <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), lista_metrica)
  colnames(df)[-1] <- names(lista_metrica)  # Renombrar columnas con los años
  return(df)
}

importancia_dataframe <- function(random_forest) {
  
  # Crear listas para almacenar cada métrica por año
  lista_incmse <- list()
  lista_incmse_10 <- list()
  lista_incnp <- list()
  lista_corr <- list()
  lista_todos <- list()
  lista_top50_incmse <- list()
  
  # Iterar sobre los años del 2014 al 2023
  for (anio in 2014:2023) {
    
    # Extraer importancia y correlaciones
    importancia <- data.frame(
      Variable = rownames(random_forest[[as.character(anio)]][["importancia"]]), 
      IncMSE = random_forest[[as.character(anio)]][["importancia"]][,1]
    )
    
    # Extraer correlaciones
    corr <- as.data.frame(random_forest[[as.character(anio)]][["correlaciones"]])
    
    # Agregar nombres de fila como columna y limpiar formato
    corr$Variable <- rownames(corr)
    corr$Corr <- corr[,1]
    corr <- corr[, c("Variable", "Corr")]
    rownames(corr) <- NULL
    
    # Unir las tablas por la columna 'Variable'
    df_final <- merge(importancia, corr, by = "Variable")
    
    df_top50 <- df_final[order(-df_final$IncMSE), ][1:50, ]
    df_top10 <- df_final[order(-df_final$IncMSE), ][1:10, ]
    
    # Guardar cada métrica en listas separadas con Variable como índice
    lista_incmse[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE)
    lista_incmse_10[[as.character(anio)]] <- data.frame(Variable = df_top10$Variable, IncMSE = df_top10$IncMSE)
    lista_corr[[as.character(anio)]] <- data.frame(Variable = df_top10$Variable, Corr = df_top10$Corr)
    lista_todos[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE, Corr = df_top50$Corr)
    
    # Filtrar las 50 variables con mayor IncMSE en ese año
    df_top50$Año <- anio
    
    # Guardar en la lista de Top 50
    lista_top50_incmse[[as.character(anio)]] <- df_top50
    
  }
  
  
  
  # Crear dataframes con años como columnas
  df_incmse <- crear_dataframe(lista_incmse)
  df_incmse_10  <- crear_dataframe(lista_incmse_10)
  df_corr <- crear_dataframe(lista_corr)
  df_todos <- crear_dataframe(lista_todos)
  
  return(list(top_50 = lista_top50_incmse,
              df_incmse = df_incmse,
              df_incmse_10 = df_incmse_10,
              df_incmse_est = calcular_estadisticas(df_incmse),
              df_incmse_est_10 = calcular_estadisticas(df_incmse_10),
              df_corr = df_corr,
              df_todos = df_todos))
}

resultados_importancia <- importancia_dataframe(random_forest)

# =======================
#  DETERMINANTES A EXCEL
# =======================

wb <- createWorkbook()

addWorksheet(wb, "Determinantes")
writeData(
  wb,
  sheet = "Determinantes",
  x = resultados_importancia[["df_incmse_est_10"]],
  startRow = 1,
  startCol = 1,
  headerStyle = createStyle(textDecoration = "bold")
)

setColWidths(wb, sheet = "Determinantes", cols = 1:50, widths = "auto")

saveWorkbook(wb, "Determinantes_new_out_eficiencia_SFA_2014-2023.xlsx", overwrite = TRUE)

# =================================
#  DETERMINANTES A TABLA DE VALORES
# =================================

df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_long_all_comp <- df_incmse_all %>% pivot_longer(-Variable, names_to = "Año", values_to = "Valor")

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


ggsave(paste0("Determinantes_SFA.jpg"), plot = grafica, width = 15, height = 20, dpi = 500)


# ==============================================
#  GRAFICAS CHILE MAPAS EFICIENCIA POR AÑO
# ==============================================

world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas

eficiencias_chile_grafica <- function(hospitales_df, anio, titulo) {
  subtitulo_paste <-  paste0("Año ", anio)
  
  grafico <- ggplot(data = chile) +
    geom_sf() +
    ggtitle(titulo,  subtitle = subtitulo_paste) +
    geom_point(
      data = hospitales_df,
      aes_string(
        x = "longitud",
        y = "latitud",
        color = "eff_global"
      ),
      alpha = 0.7
    ) +
    scale_color_gradientn(
      colors = RColorBrewer::brewer.pal(11, "RdYlGn"), # Asignar colores del 1 al 9 de la paleta "Spectral"
      limits = c(0, 1)  # Escala de valores
    ) +
    labs(
      x = "Longitud", y= "Latitud",
      #title = paste(tipo, "- Año", anio),
      color = "Valor",
      size = "Valor"
    ) +
    theme_minimal()  +
    theme(legend.position = "right",
          legend.box = "vertical",                
          plot.margin = unit(c(2, 2, 2, 2), "cm"),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),)
  
  print(grafico)
  
  ggsave(paste0(titulo,"_",subtitulo_paste,".jpg"), plot = grafico, width = 10, height = 8, dpi = 300)
}

lapply(anios, function(anio) {
  eficiencias_chile_grafica(datos_procesados[[as.character(anio)]], anio, "Gráfica Chile - Eficiencia técnica (HR)")
})


# polígonos de Chile continental
world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas

# -------------------------------------------------------------------
#   Función: mapa de eficiencia – solo Chile continental
eficiencias_chile_grafica <- function(hospitales_df,
                                      anio,
                                      titulo,
                                      ancho_px_extra = TRUE) {
  
  subt <- paste0("Año ", anio)
  hosp_cont <- hospitales_df %>% dplyr::filter(longitud >= -80)
  
  # ── mapa ──────────────────────────────────────────
  grafico <- ggplot(chile) +
    geom_sf(fill = "grey95") +
    geom_point(data   = hosp_cont,
               aes(longitud, latitud, colour = eff_global),
               alpha = .7, size = 2) +
    scale_colour_gradientn(colours = RColorBrewer::brewer.pal(11, "RdYlGn"),
                           limits  = c(0, 1), name = "Valor") +
    scale_x_continuous(breaks = c(-80,-75,-70,-65),
                       labels = c("80° W","75° W","70° W","65° W")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    # A) ventana longitudinal más ancha  (−82…−64)
    coord_sf(xlim = c(-82, -64), ylim = c(-56, -17)) +
    
    labs(title = titulo, subtitle = subt,
         x = "Longitud", y = "Latitud") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 12))
  
  print(grafico)
  
  # B) exportar con ancho/alto más generoso si se pide
  if (ancho_px_extra) {
    ggsave(paste0(titulo, "_Año_", anio, ".jpg"),
           plot   = grafico,
           width  = 14,   # ← más ancho
           height = 8,
           dpi    = 300)
  } else {
    ggsave(paste0(titulo, "_Año_", anio, ".jpg"),
           plot   = grafico,
           width  = 10,
           height = 8,
           dpi    = 300)
  }
}

# -------- genera para todos los años --------
lapply(anios, function(a)
  eficiencias_chile_grafica(
    datos_procesados[[as.character(a)]],
    anio   = a,
    titulo = "Gráfica Chile - Eficiencia técnica",
    ancho_px_extra = TRUE          # pon FALSE si no quieres la opción B
  ))


# ==============================================
#  SFA VS DEA
# ==============================================

# Cargar los datos
df_sfa <- read_excel("Eficiencias_SFA_2014-2023.xlsx", sheet = 1)
df_dea <- read_excel("RESULTADOS.xlsx", sheet = 1)

colnames(df_sfa)[colnames(df_sfa) == "ID Establecimiento"] <- "IdEstablecimiento"
df_sfa$IdEstablecimiento <- as.integer(df_sfa$IdEstablecimiento)
df_dea$IdEstablecimiento <- as.integer(df_dea$IdEstablecimiento)



euclid_resultados <- data.frame()

for (anio in anios) {
  col_sfa <- paste0("Eficiencia ", anio)
  col_dea <- as.character(anio)
  
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
      dist_eucl <- sqrt(sum((sub$SFA - sub$DEA)^2))
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


wb <- createWorkbook()
addWorksheet(wb, "Euclidiana")
writeData(wb, "Euclidiana", euclid_resultados, rowNames = FALSE)
setColWidths(wb, sheet = "Euclidiana", cols = 1:50, widths = "auto")
saveWorkbook(wb, "Distancia_Euclidiana.xlsx", overwrite = TRUE)




euclid_resultados <- data.frame()

for (anio in anios) {
  col_sfa <- paste0("Eficiencia ", anio)
  col_dea <- as.character(anio)
  
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


wb <- createWorkbook()
addWorksheet(wb, "Correlacion")
writeData(wb, "Correlacion", corr_resultados, rowNames = FALSE)
setColWidths(wb, sheet = "Correlacion", cols = 1:50, widths = "auto")
saveWorkbook(wb, "Correlaciones.xlsx", overwrite = TRUE)



euclid_resultados <- data.frame()

for (anio in anios) {
  col_sfa <- paste0("Eficiencia ", anio)
  col_dea <- as.character(anio)
  
  df <- inner_join(
    df_sfa %>% select(IdEstablecimiento, Complejidad, SFA = !!sym(col_sfa)),
    df_dea %>% select(IdEstablecimiento, DEA = !!sym(col_dea)),
    by = "IdEstablecimiento"
  ) %>% filter(!is.na(SFA) & !is.na(DEA) & !is.na(Complejidad))
  df$SFA <- as.numeric(df$SFA)
  df$DEA <- as.numeric(df$DEA)
      dist_eucl <- cor(df$SFA, df$DEA)
      euclid_resultados <- bind_rows(
        euclid_resultados,
        tibble(
          Año = anio,
          Complejidad = nivel,
          N_Hospitales = nrow(df),
          Distancia_Euclidiana = dist_eucl
        )
      )
    }
print(euclid_resultados)



# ==============================================
#  AÑO 2014 (DESDE ACA HASTA ABAJO, PRUEBAS)
# ==============================================

d2014 = datos[["2014"]]

# ================
#  EGRESOS (178 eficiencias)
# ================

# 1. Estimación
mod_egresos <- sfa(
  formula = log(Egresos.GRD) ~ log(dias_cama_disponible) + log(X21_value) + log(X22_value),
  data    = d2014
)

# 2. Resumen de resultados
summary(mod_egresos)

# 3. Extraer eficiencia
eff_egresos <- efficiencies(mod_egresos)   # vector con 178 valores, uno por hospital
head(eff_egresos)

# ================
#  CONSULTAS (167 eficiencias)
# ================

mod_consultas <- sfa(
  formula = log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
  data    = d2014
)
summary(mod_consultas)

eff_consultas <- efficiencies(mod_consultas)
head(eff_consultas)

# ================
#  QUIROFANOS (154 eficiencias)
# ================

mod_quirofano <- sfa(
  formula = log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1),
  data    = d2014
)
summary(mod_quirofano)

eff_quirofano <- efficiencies(mod_quirofano)
head(eff_quirofano)

# ------------------------------
d2014$eff_egresos   <- eff_egresos
d2014$eff_consultas <- eff_consultas
d2014$eff_quirofano <- eff_quirofano

# Puedes inspeccionar
head(d2014)



# Idea de Manuel: Distancia -> (1,1,1) punto ideal

#d2014 <- d2014 %>%
#  mutate(
#    dist_ideal = sqrt(
#      (1 - eff_egresos)^2 +
#        (1 - eff_consultas)^2 +
#        (1 - eff_quirofano)^2
#    ),
#    eff_global = 1 - dist_ideal / sqrt(3)
#  )

#analisis_sensibilidad_outliers <- function(datos_lista) {
  
  # Función para detectar outliers usando 1.5 x IQR
#  es_outlier <- function(x) {
#    x <- as.vector(x)
#    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
#    iqr <- q[2] - q[1]
#    lim_inf <- q[1] - 1.5 * iqr
#   lim_sup <- q[2] + 1.5 * iqr
#    x < lim_inf | x > lim_sup
#  }
  
  # Inicializar listas para guardar resultados
#  resumen_sensibilidad <- list()
#  datos_limpios <- list()
  
  # Recorrer cada año
#  for (anio in names(datos_lista)) {
    
#    df <- datos_lista[[anio]]
    
    # Etiquetar outliers
#    df <- df %>%
#      mutate(outlier = es_outlier(eff_global))
    
    # Dataset limpio
 #   df_clean <- df %>%
#      filter(!outlier)
    
    # Guardar dataset limpio
#    datos_limpios[[anio]] <- df_clean
    
#    # Calcular métricas antes y después
#    resumen_sensibilidad[[anio]] <- tibble(
#      Año = anio,
#      n_total = nrow(df),
#      n_outliers = sum(df$outlier),
#      media_original = mean(df$eff_global, na.rm = TRUE),
#      media_limpia   = mean(df_clean$eff_global, na.rm = TRUE),
#      mediana_original = median(df$eff_global, na.rm = TRUE),
#      mediana_limpia   = median(df_clean$eff_global, na.rm = TRUE),
#      sd_original = sd(df$eff_global, na.rm = TRUE),
#      sd_limpia   = sd(df_clean$eff_global, na.rm = TRUE)
#    )
#  }
  
  # Combinar resumen en una tabla
#  resumen_final <- bind_rows(resumen_sensibilidad)
  
  # Devolver todo junto
#  list(
#    resumen_metricas = resumen_final,
#    datos_sin_outliers = datos_limpios
#  )
#}

#resultado_sensibilidaddd <- analisis_sensibilidad_outliers(datos_procesados)

#scatterplot3d(
#  x = d2014$eff_egresos,
#  y = d2014$eff_consultas,
#  z = d2014$eff_quirofano,
#  pch = 16,
#  main = "Eficiencias (por variable) año 2014"
#)


#resid_sfa <- residuals(mod_quirofano)

# Ineficiencia estimada (u_i)
#ineff_sfa <- -log(eff_quirofano)

# Termino de ruido estimado: v_i = e_i + u_i
#noise_sfa <- resid_sfa + ineff_sfa

df <- datos[["2014"]]
mod_egresos <- sfa(
  formula = f_translog <- log(Egresos.GRD + 1) ~ 
    log(dias_cama_disponible + 1) + 
    log(X21_value + 1) + 
    log(X22_value + 1) +
    I(0.5 * log(dias_cama_disponible + 1)^2) +
    I(0.5 * log(X21_value + 1)^2) +
    I(0.5 * log(X22_value + 1)^2) +
    I(log(dias_cama_disponible + 1) * log(X21_value + 1)) +
    I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
    I(log(X21_value + 1) * log(X22_value + 1)),
  data = df
)
summary(mod_egresos)
logLik(mod_egresos)

f_translog <- log(Egresos.GRD + 1) ~ 
  log(dias_cama_disponible + 1) + 
  log(X21_value + 1) + 
  log(X22_value + 1) +
  I(0.5 * log(dias_cama_disponible + 1)^2) +
  I(0.5 * log(X21_value + 1)^2) +
  I(0.5 * log(X22_value + 1)^2) +
  I(log(dias_cama_disponible + 1) * log(X21_value + 1)) +
  I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
  I(log(X21_value + 1) * log(X22_value + 1))






# --- Cargar los datos
df_sfa <- read_excel("Eficiencias_SFA_2014-2023.xlsx", sheet = 1)
df_dea <- read_excel("RESULTADOS.xlsx", sheet = 1)

# --- Normalizar nombres y tipos
colnames(df_sfa)[colnames(df_sfa) == "ID Establecimiento"] <- "IdEstablecimiento"
df_sfa$IdEstablecimiento <- as.integer(df_sfa$IdEstablecimiento)
df_dea$IdEstablecimiento <- as.integer(df_dea$IdEstablecimiento)

# --- Define los años de interés (ajusta según tu caso)
anios <- 2014:2023

# --- DataFrames para resultados
resultados_por_complejidad <- data.frame()
resultados_por_anio <- data.frame()

for (anio in anios) {
  col_sfa <- paste0("Eficiencia ", anio)
  col_dea <- as.character(anio)
  
  # --- Unir datos del año correspondiente
  df <- inner_join(
    df_sfa %>% select(IdEstablecimiento, Complejidad, SFA = !!sym(col_sfa)),
    df_dea %>% select(IdEstablecimiento, DEA = !!sym(col_dea)),
    by = "IdEstablecimiento"
  ) %>% filter(!is.na(SFA) & !is.na(DEA))
  
  # --- Calculo por grupo de complejidad
  for (nivel in unique(df$Complejidad)) {
    sub <- df %>% filter(Complejidad == nivel)
    sub$SFA <- as.numeric(sub$SFA)
    sub$DEA <- as.numeric(sub$DEA)
    
    if (nrow(sub) > 0) {
      dist_eucl <- sqrt(sum((sub$SFA - sub$DEA)^2))
      correlacion <- cor(sub$SFA, sub$DEA)
      
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
  
  # --- Calculo global por año (sin separar por complejidad)
  sub_total <- df
  sub_total$SFA <- as.numeric(sub_total$SFA)
  sub_total$DEA <- as.numeric(sub_total$DEA)
  
  if (nrow(sub_total) > 0) {
    dist_eucl_total <- sqrt(sum((sub_total$SFA - sub_total$DEA)^2))
    correlacion_total <- cor(sub_total$SFA, sub_total$DEA)
    
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

# --- Exportar a Excel con dos hojas
write_xlsx(
  list(
    "Por_Complejidad" = resultados_por_complejidad,
    "Por_Año" = resultados_por_anio
  ),
  path = "Resultados_Distancia_Correlacion.xlsx"
)




calcula_distancia_correlacion <- function(
    archivo_sfa = "Eficiencias_OtraFuncion.xlsx",
    archivo_dea,
    sheet_sfa = 1,
    sheet_dea = 1,
    anios = 2014:2023,
    output = "Resultados_HP1.xlsx"
) {
  # Cargar los datos
  df_sfa <- read_excel(archivo_sfa, sheet = sheet_sfa)
  df_dea <- read_excel(archivo_dea, sheet = sheet_dea)
  
  # Normalizar nombres y tipos
  colnames(df_sfa)[colnames(df_sfa) == "ID Establecimiento"] <- "IdEstablecimiento"
  df_sfa$IdEstablecimiento <- as.integer(df_sfa$IdEstablecimiento)
  df_dea$IdEstablecimiento <- as.integer(df_dea$IdEstablecimiento)
  
  resultados_por_complejidad <- data.frame()
  resultados_por_anio <- data.frame()
  
  for (anio in anios) {
    col_sfa <- paste0("Eficiencia ", anio)
    col_dea <- as.character(anio)
    
    # Unir datos del año correspondiente
    df <- inner_join(
      df_sfa %>% select(IdEstablecimiento, Complejidad, SFA = !!sym(col_sfa)),
      df_dea %>% select(IdEstablecimiento, DEA = !!sym(col_dea)),
      by = "IdEstablecimiento"
    ) %>% filter(!is.na(SFA) & !is.na(DEA))
    
    # Por grupo de complejidad
    for (nivel in unique(df$Complejidad)) {
      sub <- df %>% filter(Complejidad == nivel)
      sub$SFA <- as.numeric(sub$SFA)
      sub$DEA <- as.numeric(sub$DEA)
      
      if (nrow(sub) > 0) {
        dist_eucl <- sqrt(sum((sub$SFA - sub$DEA)^2))
        correlacion <- cor(sub$SFA, sub$DEA)
        
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
    
    # Global por año
    sub_total <- df
    sub_total$SFA <- as.numeric(sub_total$SFA)
    sub_total$DEA <- as.numeric(sub_total$DEA)
    
    if (nrow(sub_total) > 0) {
      dist_eucl_total <- sqrt(sum((sub_total$SFA - sub_total$DEA)^2))
      correlacion_total <- cor(sub_total$SFA, sub_total$DEA)
      
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
  
  # Exportar a Excel
  write_xlsx(
    list(
      "Por_Complejidad" = resultados_por_complejidad,
      "Por_Año" = resultados_por_anio
    ),
    path = output
  )
  
  # Retornar los dataframes si necesitas usarlos en R
  invisible(list(
    Por_Complejidad = resultados_por_complejidad,
    Por_Año = resultados_por_anio
  ))
}

# Solo cambia el archivo de DEA, lo demás se mantiene
calcula_distancia_correlacion(
  archivo_dea = "RESULTADOS_OOVRS.xlsx",
  output = "Comparacion_HP1.xlsx"
)

calcula_distancia_correlacion(
  archivo_dea = "RESULTADOS_DEA_CRS.xlsx",
  output = "Resultados_CRS.xlsx"
)




library(tidyverse)

# Suponiendo que tu lista se llama datos_procesados y tiene nombres de cada año ("2014", ..., "2023")
datos_largos <- imap_dfr(
  datos_procesados,
  ~ .x %>% mutate(anio = .y)
)

# Si anio quedó como carácter, conviértelo a factor ordenado:
datos_largos <- datos_largos %>%
  mutate(anio = factor(anio, levels = names(datos_procesados)))


library(ggplot2)

library(ggplot2)
library(tidyverse)

# Supongamos que tu dataframe largo es datos_largos
ggplot(datos_largos, aes(x = anio, y = eff_global)) +
  geom_violin(fill = "#aee8a3", alpha = 0.3, color = "#58b858", width = 1) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  stat_summary(fun = mean, geom = "line", aes(group=1), linetype = "dashed", color = "black", size = 1) +
  labs(
    x = "Año",
    y = "Eficiencia técnica",
  ) +
  theme_minimal(base_size = 18) +      # Agranda los textos y separa
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.7, line = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold")
  )

ggsave("violinplot_eficiencia.png", width = 15, height = 5, dpi = 300)






library(dplyr)

# 1. Quedarse sólo con las columnas clave -----------------------------
datos_reduc <- datos_largos %>%                    # o datos_completos, según tu objeto
  select(anio, IdEstablecimiento, eff_global, complejidad)

#datos_reduc <- datos_reduc %>% filter(complejidad == "Mediana")

# 2. Mantener SOLO los hospitales que tienen datos para TODOS los años --------
años_totales <- n_distinct(datos_reduc$anio)

datos_balanceados <- datos_reduc %>% 
  group_by(IdEstablecimiento) %>% 
  filter(n() == años_totales) %>%   # deja los bloques completos
  ungroup() %>% 
  mutate(
    anio              = factor(anio),              # convertir a factor
    IdEstablecimiento = factor(IdEstablecimiento)
  )

library(tidyverse)

# --- 1.  MATRIZ → FORMATO LARGO ----------------------------------------------
long_eff <- datos_balanceados %>%
  as.data.frame() %>%                     # cada fila = hospital
  rownames_to_column("IdEstablecimiento") %>%
  pivot_longer(
    cols      = -IdEstablecimiento,
    names_to  = "anio",
    values_to = "eff_global"
  )

# aseguramos que los factores queden ordenados cronológicamente
long_eff <- long_eff %>% 
  mutate(anio = factor(anio, levels = sort(unique(anio))))

library(rstatix)

cat("\n==== POST-HOC WILCOXON PAREADO (two-sided) ====\n")
posthoc_wilcoxon <- datos_balanceados %>%
  pairwise_wilcox_test(eff_global ~ anio, paired = TRUE, p.adjust.method = "holm")
print(posthoc_wilcoxon, n = 50)

# --- 3. Post-hoc Wilcoxon pareado con alternativa "less"
cat("\n==== POST-HOC WILCOXON PAREADO (alternative = 'less') ====\n")
posthoc_wilcoxon_less <- datos_balanceados %>%
  pairwise_wilcox_test(eff_global ~ anio, paired = TRUE, p.adjust.method = "holm", alternative = "less")
print(posthoc_wilcoxon_less, n = 50)

# --- 2.  PRUEBA DE WILCOXON PAREADA (alternativa: menor, ajuste Holm) ---------
wilc_res <- pairwise.wilcox.test(
  x                 = long_eff$eff_global,
  g                 = long_eff$anio,
  paired            = TRUE,
  alternative       = "less",
  p.adjust.method   = "holm"
)

# --- 3.  MATRIZ DE P-VALUES FORMATEADA ----------------------------------------
p_mat <- wilc_res$p.value          # matriz triangular superior
p_mat[is.na(p_mat)] <- 1           # opcional: NA como 1 para mostrar '-'

# función de asteriscos de significancia
sig_stars <- function(p) cut(
  p,
  breaks = c(-Inf, .0001, .001, .01, .05, 1),
  labels = c("****", "***", "**", "*", "")
)

p_fmt <- ifelse(
  p_mat < 1,
  paste0(formatC(p_mat, digits = 3, format = "f"), " ", sig_stars(p_mat)),
  "-"
)

print(p_fmt)      # tabla lista para inspeccionar o exportar




# 1. Prueba de Friedman (eficiencia ~ año, bloque = hospital)
cat("==== PRUEBA DE FRIEDMAN ====\n")
friedman_result <- friedman.test(eff_global ~ IdEstablecimiento | anio, data = datos_largos)
print(friedman_result)

# 2. Wilcoxon pareado año-a-año  (alternativa: menor, ajuste Holm)
wilc_all <- pairwise.wilcox.test(
  x       = eficiencia_long$eff_global,
  g       = eficiencia_long$anio,
  p.adjust.method = "holm",
  paired  = TRUE,
  alternative = "less"
)

# 3. Convertir a matriz triangular superior y formatear
p_mat <- wilc_all$p.value           # obtiene la matriz
p_mat[is.na(p_mat)] <- 1            # opcional: rellenar NA con 1 para mostrar '-'
stars <- function(p) {              # asigna asteriscos
  cut(p,
      breaks = c(-Inf, .0001, .001, .01, .05, 1),
      labels = c("****", "***", "**", "*", ""))
}

p_fmt <- formatC(p_mat, digits = 3, format = "f")
p_fmt <- ifelse(p_mat < .05, paste(p_fmt, stars(p_mat)), p_fmt)
p_fmt[p_mat == 1] <- "-"            # muestra guión donde p = 1

# 4. Crear data frame para imprimir o exportar
tabla_final <- as.data.frame(p_fmt)
tabla_final <- tibble::rownames_to_column(tabla_final, "Año")
print(tabla_final)

# 5. (Optativo) Tabla en LaTeX con kableExtra
# install.packages("kableExtra")
library(kableExtra)

kbl(tabla_final, booktabs = TRUE,
    caption = "Prueba de Wilcoxon pareada con ajuste Holm (alternativa: menor)") |>
  kable_styling(latex_options = c("striped", "hold_position"))




df_incmse <- resultados_importancia[["df_incmse_10"]]
df_incmse_all <- df_incmse
df_incmse_pre <- df_incmse[,c(1:6)]
df_incmse_post <- df_incmse[,c(1,7:10)]


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

df_long_pre_post <- df_median %>% pivot_longer(-Variable, names_to = "Periodo", values_to = "Valor")
df_long_pre_post <- na.omit(df_long_pre_post)


# Aplicar Kruskal-Wallis para cada fila (variable)
kruskal_results_all <- df_long_all %>%
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
print(n=100,kruskal_results_pre_post)



library(rstatix)    # friedman.test() y pairwise_wilcox_test()

# ──────────────────────────────────────────────────────────────
# 1. Pasar la lista a un único data frame largo
#    Quedan las columnas: IdEstablecimiento, Año, complejidad, Valor (=eff_global)
# ──────────────────────────────────────────────────────────────
df <- imap_dfr(
  datos_procesados,
  ~ .x %>%                           # .x = tibble del año
    mutate(Año = .y) %>%             # .y = nombre de la lista (2014, 2015,…)
    select(IdEstablecimiento, Año, complejidad, Valor = eff_global)
)

# Aseguramos que Año sea factor ordenado
df <- df %>%
  mutate(
    Año         = factor(Año, levels = sort(unique(Año))),
    complejidad = factor(complejidad, levels = c("Alta", "Mediana", "Baja"))
  )

# ──────────────────────────────────────────────────────────────
# 2. Función auxiliar que corre Friedman + Wilcoxon para un nivel
# ──────────────────────────────────────────────────────────────
analisis_por_nivel <- function(datos, nivel){
  
  # --- 1.  MATRIZ → FORMATO LARGO ----------------------------------------------
  long_eff <- datos %>%
    as.data.frame() %>%                     # cada fila = hospital
    rownames_to_column("IdEstablecimiento") %>%
    pivot_longer(
      cols      = IdEstablecimiento,
      names_to  = "Año",
      values_to = "Valor"
    )
  
  # aseguramos que los factores queden ordenados cronológicamente
  long_eff <- long_eff %>% 
    mutate(Año = factor(Año, levels = sort(unique(Año))))
  
  datos_nivel <- long_eff %>% filter(complejidad == nivel)
  
  
  cat("\n==== POST-HOC WILCOXON PAREADO (two-sided) ====\n")
  posthoc_wilcoxon <- long_eff %>%
    pairwise_wilcox_test(Valor ~ Año, paired = TRUE, p.adjust.method = "holm")
  print(posthoc_wilcoxon, n = 50)
  
  cat("\n==============================\n")
  cat(">>> COMPLEJIDAD:", nivel, "\n")
  
  
  

  invisible(list(wilcoxon = wilcoxon_res))
}

# ──────────────────────────────────────────────────────────────
# 3. Ejecutar para Alta, Mediana y Baja
# ──────────────────────────────────────────────────────────────
resultados <- map(
  levels(df$complejidad),
  ~ analisis_por_nivel(df, .x)
)
names(resultados) <- levels(df$complejidad)

# Ahora `resultados$Alta`, `resultados$Mediana`, `resultados$Baja`
# contienen sendas listas con los objetos de Friedman y Wilcoxon.