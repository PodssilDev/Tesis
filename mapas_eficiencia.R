# ===================================================
# ANÁLISIS Y COMPARACIÓN DE LA EFICIENCIA TÉCNICA DE
# HOSPITALES PÚBLICOS EN CHILE USANDO SFA Y DEA
#
# JOHN SERRANO CARRASCO, 2025.
#
# SCRIPT 6: MAPAS DE EFICIENCIA TÉCNICA HOSPITALARIA
# EN CHILE
# ===================================================

# ===================================================
# IMPORTACIÓN DE PAQUETES
# ===================================================
library(RColorBrewer)
library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)


# ===================================================
# POLÍGONOS DE CHILE CONTINENTAL
# ===================================================
# world: obtiene polígonos de todos los países usando datos naturales (Natural Earth)
world <- ne_countries(scale = "medium", returnclass = "sf")

# chile: filtra solo la geometría de Chile
chile <- world[world$name == "Chile", ]

# comunas_sf: carga shapefile de comunas de Chile usando el paquete chilemapas
# (aunque en esta función no se usa, queda cargado por si se requiere)
comunas_sf <- chilemapas::mapa_comunas

# ENTRADAS:
#   hospitales_df   -> data.frame con columnas 'longitud', 'latitud', 'eff_global'
#   anio            -> año de los datos (para subtítulo)
#   titulo          -> título principal del gráfico
#   ancho_px_extra  -> TRUE para usar dimensiones más anchas en el archivo exportado
#
# SALIDA:
#   Mapa de Chile continental con puntos que representan hospitales,
#   coloreados según eficiencia global. También exporta el gráfico a un archivo .jpg.
# ===================================================
eficiencias_chile_grafica <- function(hospitales_df,
                                      anio,
                                      titulo,
                                      ancho_px_extra = TRUE) {
  
  # Subtítulo dinámico basado en el año
  subt <- paste0("Año ", anio)
  
  # Filtrar solo hospitales con coordenadas válidas (longitud >= -80 evita islas alejadas)
  hosp_cont <- hospitales_df %>% dplyr::filter(longitud >= -80)
  
  # ===================================================
  # CONSTRUCCIÓN DEL MAPA
  # ===================================================
  grafico <- ggplot(chile) +
    # Geometría base: mapa de Chile
    geom_sf(fill = "grey95") +
    
    # Puntos: hospitales con color según eficiencia global
    geom_point(data   = hosp_cont,
               aes(longitud, latitud, colour = eff_global),
               alpha = .7, size = 2) +
    
    # Escala de color: paleta "RdYlGn" de RColorBrewer (rojo a verde)
    scale_colour_gradientn(colours = RColorBrewer::brewer.pal(11, "RdYlGn"),
                           limits  = c(0, 1), name = "Valor") +
    
    # Etiquetas del eje X con grados de longitud personalizados
    scale_x_continuous(breaks = c(-80,-75,-70,-65),
                       labels = c("80° W","75° W","70° W","65° W")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    # Vista ajustada: se amplía ventana longitudinal (−82 a −64) y latitudinal (−56 a −17)
    coord_sf(xlim = c(-82, -64), ylim = c(-56, -17)) +
    
    # Etiquetas de títulos y ejes
    labs(title = titulo, subtitle = subt,
         x = "Longitud", y = "Latitud") +
    
    # Tema estético minimalista
    theme_minimal() +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 12))
  
  # Mostrar gráfico en la sesión
  print(grafico)
  
  # ===================================================
  # EXPORTAR EL GRÁFICO A ARCHIVO
  # ===================================================
  if (ancho_px_extra) {
    # Opción A: imagen más ancha (ideal para presentaciones)
    ggsave(paste0(titulo, "_Año_", anio, ".jpg"),
           plot   = grafico,
           width  = 14,   # más ancho
           height = 8,
           dpi    = 300)
  } else {
    # Opción B: imagen con dimensiones estándar
    ggsave(paste0(titulo, "_Año_", anio, ".jpg"),
           plot   = grafico,
           width  = 10,
           height = 8,
           dpi    = 300)
  }
}


# ===================================================
# GENERAR MAPAS PARA TODOS LOS AÑOS
# ===================================================
# Se itera sobre cada año en 'anios' y se genera un mapa para ese año
# usando la función anterior.
lapply(anios, function(a)
  eficiencias_chile_grafica(
    datos_procesados[[as.character(a)]],  # datos del año a
    anio   = a,
    titulo = "Gráfica Chile - Eficiencia técnica",
    ancho_px_extra = TRUE                 
  ))
