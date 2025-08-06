# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 5: ANÁLISIS DE SENSIBILIDAD
# ===================================================

# ===================================================
# ANALISIS DE SENSIBILIDAD
# ===================================================

# --- Fórmulas para cada salida (modelo Cobb-Douglas) ---
formula_egresos   <- log(Egresos.GRD + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)
formula_consultas <- log(Consultas + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)
formula_quirofano <- log(Quirofano + 1) ~ log(dias_cama_disponible + 1) + log(X21_value + 1) + log(X22_value + 1)

sensibilidad_resultados <- list()

for (anio in names(datos)) {
  cat("\n=======\nAÑO", anio, "\n=======\n")
  
  df_ori <- datos[[anio]]          # datos originales
  df_proc <- datos_procesados[[anio]]  # datos con eficiencias
  
  # --- Extraer eficiencias originales ---
  eff_egresos_ori   <- df_proc$eff_egresos
  eff_consultas_ori <- df_proc$eff_consultas
  eff_quirofano_ori <- df_proc$eff_quirofano
  eff_global_ori    <- df_proc$eff_global
  
  # --- Detección de outliers en eficiencia global ---
  Q1 <- quantile(eff_global_ori, 0.25, na.rm = TRUE)
  Q3 <- quantile(eff_global_ori, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  no_outliers <- eff_global_ori >= lim_inf & eff_global_ori <= lim_sup
  
  cat("N hospitales:", nrow(df_proc), " | Outliers:", sum(!no_outliers), "\n")
  
  # --- Crear versión limpia de datos ---
  df_sin_out <- df_ori[no_outliers, ]
  
  # --- Recalcular modelos SFA con datos sin outliers ---
  mod_egresos   <- sfa(formula_egresos,   data = df_sin_out)
  mod_consultas <- sfa(formula_consultas, data = df_sin_out)
  mod_quirofano <- sfa(formula_quirofano, data = df_sin_out)
  
  # --- Eficiencias recalculadas ---
  eff_egresos_new    <- as.numeric(efficiencies(mod_egresos))
  eff_consultas_new  <- as.numeric(efficiencies(mod_consultas))
  eff_quirofano_new  <- as.numeric(efficiencies(mod_quirofano))
  eff_global_new     <- 1 - sqrt((1-eff_egresos_new)^2 + 
                                   (1-eff_consultas_new)^2 + 
                                   (1-eff_quirofano_new)^2) / sqrt(3)
  
  # --- Emparejar original vs nuevo ---
  comp_df <- data.frame(
    IdEstablecimiento = df_sin_out$IdEstablecimiento,
    eff_global_ori = eff_global_ori[no_outliers],
    eff_global_new = eff_global_new
  )
  
  # === diferencias absoluta y relativa ===
  comp_df <- comp_df %>%
    mutate(
      diff_abs = eff_global_new - eff_global_ori,                  # diferencia absoluta
      diff_rel = 100 * diff_abs / ifelse(eff_global_ori == 0, 1, eff_global_ori) # diferencia relativa (%)
    )
  
  # --- Calcular correlación global ---
  cor_global <- cor(comp_df$eff_global_ori, comp_df$eff_global_new)
  
  cat("Correlación global: ", round(cor_global, 3), "\n")
  
  # --- Guardar resultados del año ---
  sensibilidad_resultados[[anio]] <- list(
    outliers = which(!no_outliers),
    n_outliers = sum(!no_outliers),
    comp_df = comp_df,
    correlacion_global = cor_global,
    summary_ori = summary(comp_df$eff_global_ori),
    summary_new = summary(comp_df$eff_global_new),
    resumen_diferencias = summary(comp_df$diff_abs)
  )
}

# ===================================================
# Consolidar diferencias por año
# ===================================================
efic_global_new_por_ano <- lapply(names(sensibilidad_resultados), function(anio) {
  df <- sensibilidad_resultados[[anio]]$comp_df %>%
    select(IdEstablecimiento, eff_global_new, diff_abs, diff_rel)
  df$Anio <- anio
  df
})
names(efic_global_new_por_ano) <- names(sensibilidad_resultados)

# ===================================================
# Tabla de correlaciones globales por año
# ===================================================
correlaciones_por_anio <- data.frame(
  Anio = names(sensibilidad_resultados),
  Correlacion_Global = sapply(sensibilidad_resultados, function(x) x$correlacion_global)
)

print(correlaciones_por_anio)
