# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 2: Modelos SFA Y EFICIENCIA TÉCNICA HOSPITALARIA
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(here)
library(frontier)
library(purrr)
library(ggplot2)

# ==============================================
#  MODELOS SFA PARA TODOS LOS AÑOS
# ==============================================

# ENTRADAS:
#   df        -> data.frame con los datos de un año (hospitales y variables)
#   egresos   -> tipo de modelo para egresos (1 = Cobb-Douglas, otro = Translog)
#   consultas -> tipo de modelo para consultas (1 = Cobb-Douglas, otro = Translog)
#   quirofano -> tipo de modelo para quirófano (1 = Cobb-Douglas, otro = Translog)
#
# SALIDA:
#   data.frame con columnas originales + eficiencias por modelo y eficiencia global
#
# DESCRIPCIÓN:
#   La función ejecuta 3 modelos SFA (para Egresos, Consultas y Quirófano) 
#   dependiendo de la opción seleccionada para cada variable. 
#   Calcula las eficiencias individuales y una eficiencia global, esta última
#   mediante la distancia euclidiana considerando un punto óptimo.
# ==============================================
procesar_sfa <- function(df, egresos, consultas, quirofano) {
  
  # ============================
  # 1. Definición de fórmula para egresos
  # ============================
  if (egresos == 1) {
    # Modelo simple Cobb-Douglas
    formula_egresos <- log(Egresos.GRD + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1)
  } else {
    # Modelo extendido Translog
    formula_egresos <- log(Egresos.GRD + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) +
      I(0.5 * log(X21_value + 1)^2) +
      I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) +
      I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1))
  }
  
  # ============================
  # 2. Ajuste del modelo SFA para egresos
  # ============================
  mod_egresos <- sfa(
    formula = formula_egresos,
    data    = df
  )
  # Cálculo de eficiencias técnicas para egresos
  eff_egresos <- efficiencies(mod_egresos)
  
  # ============================
  # 3. Definición de fórmula para consultas
  # ============================
  if (consultas == 1) {
    # Modelo simple Cobb-Douglas
    formula_consultas <- log(Consultas + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1)
  } else {
    # Modelo extendido Translog
    formula_consultas <- log(Consultas + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) +
      I(0.5 * log(X21_value + 1)^2) +
      I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) +
      I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1))
  }
  
  # ============================
  # 4. Ajuste del modelo SFA para consultas
  # ============================
  mod_consultas <- sfa(
    formula = formula_consultas,
    data    = df
  )
  # Cálculo de eficiencias técnicas para consultas
  eff_consultas <- efficiencies(mod_consultas)
  
  # ============================
  # 5. Definición de fórmula para quirófano
  # ============================
  if (quirofano == 1) {
    # Modelo simple Cobb-Douglas
    formula_quirofano <- log(Quirofano + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1)
  } else {
    # Modelo extendido Translog
    formula_quirofano <- log(Quirofano + 1) ~ 
      log(dias_cama_disponible + 1) +
      log(X21_value + 1) +
      log(X22_value + 1) +
      I(0.5 * log(dias_cama_disponible + 1)^2) +
      I(0.5 * log(X21_value + 1)^2) +
      I(0.5 * log(X22_value + 1)^2) +
      I(log(dias_cama_disponible + 1) * log(X21_value + 1)) +
      I(log(dias_cama_disponible + 1) * log(X22_value + 1)) +
      I(log(X21_value + 1) * log(X22_value + 1))
  }
  
  # ============================
  # 6. Ajuste del modelo SFA para quirófano
  # ============================
  mod_quirofano <- sfa(
    formula = formula_quirofano,
    data    = df
  )
  # Cálculo de eficiencias técnicas para quirófano
  eff_quirofano <- efficiencies(mod_quirofano)
  
  # ============================
  # 7. Agregación de eficiencias al data.frame original
  #    y cálculo de eficiencia global
  # ============================
  df_nuevo <- df %>%
    mutate(
      eff_egresos   = eff_egresos,
      eff_consultas = eff_consultas,
      eff_quirofano = eff_quirofano,
      # Distancia euclidiana al punto ideal (1,1,1)
      dist_ideal = sqrt(
        (1 - eff_egresos)^2 +
          (1 - eff_consultas)^2 +
          (1 - eff_quirofano)^2
      ),
      # Eficiencia global normalizada
      eff_global = round(1 - dist_ideal / sqrt(3), 3)
    )
  
  # Retorna el data.frame con nuevas columnas
  return(df_nuevo)
}

# ==============================================
# PROCESAMIENTO DE TODOS LOS AÑOS
# ==============================================
datos_procesados <- lapply(
  datos, 
  procesar_sfa, 
  egresos   = 1, 
  consultas = 1, 
  quirofano = 1
)


# Se transforma el data.frame a largo
datos_largos <- imap_dfr(
  datos_procesados,
  ~ .x %>% mutate(anio = .y)
)

# Año a factor
datos_largos <- datos_largos %>%
  mutate(anio = factor(anio, levels = names(datos_procesados)))

# ==============================================
# VIOLIN PLOT GLOBAL
# ==============================================

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


# ==============================================
# VIOLIN PLOT POR COMPLEJIDAD HOSPITALARIA
# ==============================================

ggplot(datos_largos, aes(x = anio, y = eff_global)) +
  # Violin plot global
  geom_violin(fill = "#aee8a3", alpha = 0.3, color = "#58b858", width = 1) +
  
  # Puntos de la media por año y complejidad
  stat_summary(fun = mean,
               geom = "point",
               aes(color = complejidad),    # color según complejidad
               size = 3,
               position = position_dodge(width = 0.5)) +
  
  # Líneas conectando las medias de cada complejidad
  stat_summary(fun = mean,
               geom = "line",
               aes(group = complejidad, color = complejidad),
               size = 1,
               position = position_dodge(width = 0.5)) +
  
  labs(
    x = "Año",
    y = "Eficiencia técnica",
    color = "Complejidad"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 30, vjust = 0.7),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold")
  )

ggsave("violinplot_eficiencia_complejidad.png", width = 15, height = 5, dpi = 300)
