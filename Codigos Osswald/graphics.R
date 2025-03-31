library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)
library(gridExtra)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(sf)
library(RColorBrewer)

# Cargar los datos de Chile
world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas


# -------------------------------------- #
# Grafica de distribución
# -------------------------------------- #
eficiencias_grafica <- function(resultados) {
  # Parámetros para iterar
  tipos <- c("io", "oo")
  periodos <- list(
    "todos" = "2014 - 2023",
    "pre" = "2014 - 2019",
    "post" = "2020 - 2023"
  )
  
  # Generar gráficos para cada tipo y período
  for (tipo in tipos) {
    for (periodo in names(periodos)) {
      # Crear dataframe
      df <- eficiencias_dataframe(resultados, tipo, periodo)
      
      titulo1 <- paste("Distribución de Eficiencia Técnica Orientación", ifelse(tipo == "io", "Entradas", "Salidas"), "(VRS)")
      subtitulo1 <- paste("Período", periodos[[periodo]])
      titulo2 <- paste("Distribución de Eficiencia Técnica Orientación", ifelse(tipo == "io", "Entradas", "Salidas"), "(CRS)")
      subtitulo2 <- paste("Período", periodos[[periodo]])
      
      # Graficar VRS
      grafica1 <- graficar_boxplots(
        df,
        "vrs",
        titulo1,
        subtitulo1
      )
      
      # Graficar CRS
      grafica2 <-graficar_boxplots(
        df,
        "crs",
        titulo2,
        subtitulo2
      )
      
      ggsave(paste0(titulo1,"_",subtitulo1,".jpg"), plot = grafica1, width = 13, height = 5, dpi = 300)
      ggsave(paste0(titulo2,"_",subtitulo2,".jpg"), plot = grafica2, width = 13, height = 5, dpi = 300)
      
      print(grafica1)
      print(grafica2)
    }
  }
  
}




# Función para generar gráficos de un solo key
generar_graficas_malmquist <- function(index, key) {
  #browser()
  print(paste("Generando gráfico para:", key))
  
  key_name <- switch(key,
                     "out_vrs" = "modelo orientado a salidas VRS",
                     "out_crs" = "modelo orientado a salidas CRS",
                     "in_vrs" = "modelo orientado a entradas VRS",
                     "in_crs" = "modelo orientado a entradas CRS",
                     key)
  
  comparativa_pre <- colnames(index)[c(2, 3, 4, 5, 6)]
  comparativa_2020_2021 <- colnames(index)[c(7,8)]
  comparativa_post <- colnames(index)[c(9, 10)]
  
  
  # Crear un dataframe vacío con el mismo número de filas que index
  datos_resumen <- data.frame(matrix(nrow = nrow(index), ncol = 0))
  
  # Calcular el promedio y asignarlo
  datos_resumen$pre_pandemia <- rowMeans(index[, comparativa_pre], na.rm = TRUE)
  datos_resumen$critico <- rowMeans(index[, comparativa_2020_2021], na.rm = TRUE)
  datos_resumen$post_pandemia <- rowMeans(index[, comparativa_post], na.rm = TRUE)
  
  columnas <- colnames(datos_resumen)
  # Crear un dataframe combinado para todas las columnas
  datos_comb <- data.frame()
  for (col in columnas) {
    datos_temp <- na.omit(datos_resumen[[col]])  # Omitir NAs
    datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
  }
  
  
  #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
  rango_x <- c(-2, 5)  
  
  titulo_comparativa <- paste("Índice Malmquist para", key_name)
  subtitulo_comparativa <- "Comparativa periodo crítico COVID-19"
  
  # Crear el gráfico combinado
  grafico_comparativa <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
    geom_density(alpha = 0.3) +  # Añadir transparencia
    ggtitle(titulo_comparativa,  subtitle = subtitulo_comparativa) +
    
    xlab("Índice Malmquist") +
    ylab("Densidad") +
    theme_minimal() +
    ylim(0, 8) + 
    theme(legend.position = "right",
          legend.box = "vertical",                # Orden vertical fijo
          #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
          #legend.key.width = unit(5, "cm"),
          plot.margin = unit(c(2, 2, 2, 2), "cm"),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)) + 
    
    scale_x_continuous(
      breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
      limits = rango_x  # Limitar los valores al rango simétrico
    ) +
    
    # Leyenda para colores
    scale_color_manual(
      values = brewer.pal(11, "RdYlGn")[c(1,5,11)],                         # Paleta de colores
      labels = c("2014-2019", "2019-2021","2021-2023")    # Etiquetas personalizadas
    ) +
    scale_fill_manual(
      values = brewer.pal(11, "RdYlGn")[c(1,5,11)],                             # Usar la misma paleta para rellenos
      labels = c("2014-2019", "2019-2021","2021-2023")
    ) +
    
    labs(
      color = "Años",    # Cambia el nombre para colores
      fill = "Años"      # Cambia el nombre para rellenos
    )
  
  
  print(grafico_comparativa)
  ggsave(paste0(titulo_comparativa,"_",subtitulo_comparativa,".jpg"), plot = grafico_comparativa, width = 13, height = 5, dpi = 300)
  
  
  # Seleccionar columnas excepto la primera
  columnas <- colnames(index)[-c(1, 7, 8, 9, 10, 11, 12)]
  
  # Crear un dataframe combinado para todas las columnas
  datos_comb <- data.frame()
  for (col in columnas) {
    datos_temp <- na.omit(index[[col]])  # Omitir NAs
    datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
  }
  
  #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
  rango_x <- c(-2, 5)  
  
  titulo_pre <- paste("Índice Malmquist para", key_name)
  subtitulo_pre <- "Periodo pre pandemia por COVID-19"
  
  # Crear el gráfico combinado
  grafico_pre_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
    geom_density(alpha = 0.3) +  # Añadir transparencia
    ggtitle(titulo_pre,  subtitle = subtitulo_pre) +
    
    xlab("Índice Malmquist") +
    ylab("Densidad") +
    theme_minimal() +
    ylim(0, 6) + 
    theme(legend.position = "right",
          legend.box = "vertical",                # Orden vertical fijo
          #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
          #legend.key.width = unit(5, "cm"),
          plot.margin = unit(c(2, 2, 2, 2), "cm"),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12)) + 
    
    scale_x_continuous(
      breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
      limits = rango_x  # Limitar los valores al rango simétrico
    ) +
    
    # Leyenda para colores
    scale_color_manual(
      values = brewer.pal(11, "RdYlGn")[1:5],                         # Paleta de colores
      labels = c("2014 - 2015", "2015 - 2016", 
                 "2016 - 2017", "2017 - 2018", 
                 "2018 - 2019")    # Etiquetas personalizadas
    ) +
    scale_fill_manual(
      values = brewer.pal(11, "RdYlGn")[1:5],                             # Usar la misma paleta para rellenos
      labels = c("2014 - 2015", "2015 - 2016", 
                 "2016 - 2017", "2017 - 2018", 
                 "2018 - 2019")
    ) +
    
    labs(
      color = "Años",    # Cambia el nombre para colores
      fill = "Años"      # Cambia el nombre para rellenos
    )
  
  
  print(grafico_pre_pandemia)
  ggsave(paste0(titulo_pre,"_",subtitulo_pre,".jpg"), plot = grafico_pre_pandemia, width = 13, height = 5, dpi = 300)
  
  
  
  # Seleccionar columnas excepto la primera
  columnas <- colnames(index)[-c(1, 2, 3, 4, 5, 6, 11, 12)]
  
  # Crear un dataframe combinado para todas las columnas
  datos_comb <- data.frame()
  for (col in columnas) {
    datos_temp <- na.omit(index[[col]])  # Omitir NAs
    datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
  }
  
  #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
  rango_x <- c(-2, 5)  
  
  titulo_post <- paste("Índice Malmquist para", key_name)
  subtitulo_post <- "Periodo de pandemia por COVID-19"
  # Crear el gráfico combinado
  grafico_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
    geom_density(alpha = 0.3) +  # Añadir transparencia
    ggtitle(titulo_post, subtitle = subtitulo_post) +
    xlab("Índice Malmquist") +
    ylab("Densidad") +
    theme_minimal() +
    ylim(0, 6) + 
    theme(
      legend.position = "right",
      legend.box = "vertical",                # Orden vertical fijo
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)
    ) +
    scale_x_continuous(
      breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
      limits = rango_x  # Limitar los valores al rango simétrico
    ) +
    # Leyenda para colores
    scale_color_manual(
      values = brewer.pal(11, "RdYlGn")[7:11],  # Asignar colores personalizados
      labels = c("2019 - 2020", 
                 "2020 - 2021", 
                 "2021 - 2022", "2022 - 2023")  # Etiquetas personalizadas
    ) +
    scale_fill_manual(
      values = brewer.pal(11, "RdYlGn")[7:11],  # Usar los mismos colores personalizados para rellenos
      labels = c("2019 - 2020", 
                 "2020 - 2021",
                 "2021 - 2022", "2022 - 2023")
    ) +
    labs(
      color = "Años",    # Cambia el nombre para colores
      fill = "Años"      # Cambia el nombre para rellenos
    )
  
  
  print(grafico_pandemia)
  ggsave(paste0(titulo_post,"_",subtitulo_post,".jpg"), plot = grafico_pandemia, width = 13, height = 5, dpi = 300)
  
  
  # Seleccionar columnas excepto la primera
  columnas_2 <- colnames(index)[11:12]
  
  # Crear un dataframe combinado para todas las columnas
  datos_comb <- data.frame()
  for (col in columnas_2) {
    datos_temp <- na.omit(index[[col]])  # Omitir NAs
    datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
  }
  
  titulo_tasas <- paste("Índice Malmquist para", key_name)
  subtitulo_tasas <- "Promedio por periodo"
  
  grafico_tasas <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
    geom_density(alpha = 0.3) +  # Añadir transparencia
    ggtitle(titulo_tasas, subtitle = subtitulo_tasas) +
    xlab("Índice Malmquist") +
    ylab("Densidad") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12)# Márgenes
    ) +
    scale_x_continuous(
      breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
      limits = rango_x  # Limitar los valores al rango simétrico
    ) +
    # Leyenda para colores
    scale_color_manual(
      #values = brewer.pal(11, "RdYlGn")[7:11]
      values = c( brewer.pal(11, "RdYlGn")[1],  brewer.pal(11, "RdYlGn")[11]),                    # Colores personalizados
      labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
    ) +
    # Leyenda para rellenos
    scale_fill_manual(
      values = c( brewer.pal(11, "RdYlGn")[1],  brewer.pal(11, "RdYlGn")[11]),              # Colores de relleno
      labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
    ) +
    # Cambiar nombres de las leyendas
    labs(
      color = "Periodo",    # Cambia el nombre para colores
      fill = "Periodo"      # Cambia el nombre para rellenos
    )
  
  
  print(grafico_tasas)
  ggsave(paste0(titulo_tasas,"_",subtitulo_tasas,".jpg"), plot = grafico_tasas, width = 13, height = 5, dpi = 300)
}

# Función para generar gráficos de manera masiva
malmquist_graficas_masiva <- function(malmquist_indices) {
  for (key in names(malmquist_indices)) {
    index <- malmquist_indices[[key]][["index"]]
    generar_graficas_malmquist(index, key)
  }
}












# -------------------------------------- #
# Grafica de densidad de malmquist
# -------------------------------------- #


malmquist_graficas <- function(malmquist_indices) {
  for (key in names(malmquist_indices)) {
    index <- malmquist_indices[[key]][["index"]]
    print(paste("Generando gráfico para:", key))
    
    if (key == "out_vrs"){
      key_name = "modelo orientado a salidas VRS"
    }
    if (key == "out_crs"){
      key_name = "modelo orientado a salidas CRS"
    }
    if (key == "in_vrs"){
      key_name = "modelo orientado a entradas VRS"
    }
    if (key == "in_crs"){
      key_name = "modelo orientado a entradas CRS"
    }
    
    comparativa_pre <- colnames(index)[c(2, 3, 4, 5, 6)]
    comparativa_2020_2021 <- colnames(index)[c(7)]
    comparativa_post <- colnames(index)[c(8, 9, 10)]
    
    
    # Crear un dataframe vacío con el mismo número de filas que index
    datos_resumen <- data.frame(matrix(nrow = nrow(index), ncol = 0))
    
    # Calcular el promedio y asignarlo
    datos_resumen$pre_pandemia <- rowMeans(index[, comparativa_pre], na.rm = TRUE)
    datos_resumen$critico <- rowMeans(index[, comparativa_2020_2021], na.rm = TRUE)
    datos_resumen$post_pandemia <- rowMeans(index[, comparativa_post], na.rm = TRUE)
    
    columnas <- colnames(datos_resumen)
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas) {
      datos_temp <- na.omit(datos_resumen[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    

    #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
    rango_x <- c(-2, 5)  
    
    titulo_comparativa <- paste("Índice Malmquist para", key_name)
    subtitulo_comparativa <- "Comparativa periodo crítico COVID-19"
    
    # Crear el gráfico combinado
    grafico_comparativa <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_comparativa,  subtitle = subtitulo_comparativa) +
      
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 8) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12)) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_manual(
        values = brewer.pal(11, "RdYlGn")[c(1,5,11)],                         # Paleta de colores
        labels = c("Pre critico", "Críticos","Post critico")    # Etiquetas personalizadas
      ) +
      scale_fill_manual(
        values = brewer.pal(11, "RdYlGn")[c(1,5,11)],                             # Usar la misma paleta para rellenos
        labels = c("Pre critico", "Críticos","Post critico")
      ) +
      
      labs(
        color = "Años",    # Cambia el nombre para colores
        fill = "Años"      # Cambia el nombre para rellenos
      )
    
    
    print(grafico_comparativa)
    ggsave(paste0(titulo_comparativa,"_",subtitulo_comparativa,".jpg"), plot = grafico_comparativa, width = 13, height = 5, dpi = 300)
    
    
    # Seleccionar columnas excepto la primera
    columnas <- colnames(index)[-c(1, 7, 8, 9, 10, 11, 12)]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
    rango_x <- c(-2, 5)  
    
    titulo_pre <- paste("Índice Malmquist para", key_name)
    subtitulo_pre <- "Periodo pre pandemia por COVID-19"
    
    # Crear el gráfico combinado
    grafico_pre_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_pre,  subtitle = subtitulo_pre) +
      
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(2, 2, 2, 2), "cm"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12)) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_manual(
        values = brewer.pal(11, "RdYlGn")[1:5],                         # Paleta de colores
        labels = c("2014 - 2015", "2015 - 2016", 
                   "2016 - 2017", "2017 - 2018", 
                   "2018 - 2019")    # Etiquetas personalizadas
      ) +
      scale_fill_manual(
        values = brewer.pal(11, "RdYlGn")[1:5],                             # Usar la misma paleta para rellenos
        labels = c("2014 - 2015", "2015 - 2016", 
                   "2016 - 2017", "2017 - 2018", 
                   "2018 - 2019")
      ) +

      labs(
        color = "Años",    # Cambia el nombre para colores
        fill = "Años"      # Cambia el nombre para rellenos
      )
    
    
    print(grafico_pre_pandemia)
    ggsave(paste0(titulo_pre,"_",subtitulo_pre,".jpg"), plot = grafico_pre_pandemia, width = 13, height = 5, dpi = 300)

    
    
    # Seleccionar columnas excepto la primera
    columnas <- colnames(index)[-c(1, 2, 3, 4, 5, 6, 11, 12)]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
    rango_x <- c(-2, 5)  
    
    titulo_post <- paste("Índice Malmquist para", key_name)
    subtitulo_post <- "Periodo de pandemia por COVID-19"
    # Crear el gráfico combinado
    grafico_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_post, subtitle = subtitulo_post) +
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(
        legend.position = "right",
        legend.box = "vertical",                # Orden vertical fijo
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)
      ) +
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      # Leyenda para colores
      scale_color_manual(
        values = brewer.pal(11, "RdYlGn")[7:11],  # Asignar colores personalizados
        labels = c("2019 - 2020", 
                   "2020 - 2021", 
                   "2021 - 2022", "2022 - 2023")  # Etiquetas personalizadas
      ) +
      scale_fill_manual(
        values = brewer.pal(11, "RdYlGn")[7:11],  # Usar los mismos colores personalizados para rellenos
        labels = c("2019 - 2020", 
                   "2020 - 2021",
                   "2021 - 2022", "2022 - 2023")
      ) +
      labs(
        color = "Años",    # Cambia el nombre para colores
        fill = "Años"      # Cambia el nombre para rellenos
      )
    
    
    print(grafico_pandemia)
    ggsave(paste0(titulo_post,"_",subtitulo_post,".jpg"), plot = grafico_pandemia, width = 13, height = 5, dpi = 300)

    
    # Seleccionar columnas excepto la primera
    columnas_2 <- colnames(index)[11:12]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas_2) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    titulo_tasas <- paste("Índice Malmquist para", key_name)
    subtitulo_tasas <- "Promedio por periodo"
    
    grafico_tasas <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_tasas, subtitle = subtitulo_tasas) +
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)# Márgenes
      ) +
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      # Leyenda para colores
      scale_color_manual(
        #values = brewer.pal(11, "RdYlGn")[7:11]
        values = c( brewer.pal(11, "RdYlGn")[1],  brewer.pal(11, "RdYlGn")[11]),                    # Colores personalizados
        labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
      ) +
      # Leyenda para rellenos
      scale_fill_manual(
        values = c( brewer.pal(11, "RdYlGn")[1],  brewer.pal(11, "RdYlGn")[11]),              # Colores de relleno
        labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
      ) +
      # Cambiar nombres de las leyendas
      labs(
        color = "Periodo",    # Cambia el nombre para colores
        fill = "Periodo"      # Cambia el nombre para rellenos
      )
    

    print(grafico_tasas)
    ggsave(paste0(titulo_tasas,"_",subtitulo_tasas,".jpg"), plot = grafico_tasas, width = 13, height = 5, dpi = 300)
    
  }
  
}




# -------------------------------------- #
# Graficar Chile con sus eficiencias según el criterio de orientacion y variacion
# -------------------------------------- #
eficiencias_chile_grafica <- function(hospitales_df, anio, tipo, titulo, subtitulo) {
    subtitulo_paste <-  paste0(subtitulo, " Año ",anio)
    
    grafico <- ggplot(data = chile) +
      geom_sf() +
      ggtitle(titulo,  subtitle = subtitulo_paste) +
      geom_point(
        data = hospitales_df,
        aes_string(
          x = "longitud",
          y = "latitud",
          color = tipo,
          text = paste0("paste('Hospital:', Nombre, '<br>", tipo, ":', ", tipo, ", '<br>Region:', region_id)")
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


# -------------------------------------- #
# Correlaciones de eficiencia técnica
# -------------------------------------- #
correlaciones_eficiencia_grafica <- function(correlaciones_lista, orientacion, etiquetas = c(), subtitulo = "", nombre_archivo) {
  # Definir colores personalizados
  #browser()
  colores_personalizados <- colorRampPalette(brewer.pal(9, "RdYlGn"))(200)
  
  # Determinar la cantidad de gráficos
  num_graficos <- length(correlaciones_lista)
  graficos_por_pagina <- 10  # Máximo 10 gráficos por página (2x5)
  paginas <- ceiling(num_graficos / graficos_por_pagina)
  
  # Crear lista para almacenar los gráficos
  lista_graficos <- list()
  
  # Iterar sobre cada página
  for (pagina in 1:paginas) {
    # Definir el rango de años a mostrar en la página actual
    inicio <- (pagina - 1) * graficos_por_pagina + 1
    fin <- min(pagina * graficos_por_pagina, num_graficos)
    años_actuales <- names(correlaciones_lista)[inicio:fin]
    
    # Definir el nombre del archivo para guardar la página como imagen
    archivo_salida <- paste0(nombre_archivo, ".png")
    
    # Abrir un dispositivo gráfico para guardar como imagen
    png(archivo_salida, width = 7000, height = 4000, res = 300)  # Más ancho y achatado
    
    # Ajustar la ventana gráfica para 2x5 con márgenes reducidos
    #par(mfrow = c(2, 5), mar = c(1, 1, 2, 1), oma = c(2, 2, 2, 2)) 
    par(mfrow = c(2, 5), mar = c(2, 2, 2, 2), oma = c(2, 2, 2, 2))  
    
    # Crear las gráficas para los años actuales
    for (anio in años_actuales) {
      corr_matrix <- correlaciones_lista[[anio]]
      
      if (length(etiquetas) != 0) {
        rownames(corr_matrix) <- etiquetas
        colnames(corr_matrix) <- etiquetas
      }
      
      # Enmascarar la diagonal superior
      corr_matrix[upper.tri(corr_matrix)] <- NA
      
      format_num <- function(x) {
        ifelse(x == 1, "1", format(round(x, 3), nsmall = 3))
      }
      
      corrplot(
        corr_matrix, 
        col = colores_personalizados, 
        method = "color", 
        mar = c(1, 1, 2, 1),  # Reducir margen superior del gráfico
        addCoef.col = "black",  # Color de los coeficientes
        number.cex = 1.5,  # Aumenta el tamaño de los números dentro de las celdas
        tl.cex = 1.5,  # Aumenta el tamaño de las etiquetas de los ejes (filas y columnas)
        cl.cex = 1.5,  # Aumenta el tamaño de la escala de colores
        cl.ratio = 0.4,
        cl.align = "r", 
        tl.col = "black",
        number.format = format_num,  
        na.label = " "
      )
      
      
      
      # Acercar el título al gráfico
      mtext(paste("Año", anio), side = 3, line = -1, cex = 1.5, font = 2)  
      
    }
    
    # Determinar título según orientación
    if (orientacion == "io") {
      texto <- "Matrices de correlación de métodos orientado a entradas por año"
    } else if (orientacion == "oo") {
      texto <- "Matrices de correlación de métodos orientado a salidas por año"
    } else {
      texto <- "Matrices de correlación de métodos por año"
    }
    
    # Reducir espacio entre el título general y los gráficos
    mtext(
      texto, 
      outer = TRUE, 
      cex = 1.5,  # Tamaño del texto
      font = 2,   # Estilo en negrita
      line = 0.5  # Acercar más el título a los gráficos
    )
    
    # Subtítulo centrado y más cerca del título principal
    if (subtitulo != "") {
      mtext(
        subtitulo,
        outer = TRUE, 
        cex = 1.2,  
        font = 3,   
        line = -0.5  # Subtítulo aún más cerca
      )
    }
    
    # Cerrar el dispositivo gráfico
    dev.off()
    
    # Restablecer configuración gráfica
    par(mfrow = c(1, 1))
  }
  
  # Retornar lista de gráficos
  return(lista_graficos)
}






correlaciones_single_eficiencia_grafica <- function(correlaciones_lista, orientacion, etiquetas = c(), subtitulo = "") {
  # Definir colores personalizados
  colores_personalizados <- colorRampPalette(brewer.pal(9, "RdYlGn"))(200)
  
  # Determinar la cantidad de gráficos
  num_graficos <- length(correlaciones_lista)
  graficos_por_pagina <- 10  # Máximo 10 gráficos por página (2x5)
  paginas <- ceiling(num_graficos / graficos_por_pagina)
  
  # Crear lista para almacenar los gráficos
  lista_graficos <- list()
  
  
  # Definir el nombre del archivo para guardar la página como imagen
  archivo_salida <- paste0("Resumen correlaciones",".png")
  
  # Abrir un dispositivo gráfico para guardar como imagen
  png(archivo_salida, width = 6000, height = 3000, res = 300)  # Más ancho y achatado
  
  # Ajustar la ventana gráfica para 2x5
  par(mfrow = c(2, 4), mar = c(1, 1, 2, 1), oma = c(4,4,4,4))
  
  metodos <- names(correlaciones_lista)
  
  # Crear las gráficas para los años actuales
  for (metodo in metodos) {
    corr_matrix <- correlaciones_lista[[metodo]]
    
    if (length(etiquetas) != 0) {
      rownames(corr_matrix) <- etiquetas
      colnames(corr_matrix) <- etiquetas
    }
    
    format_num <- function(x) {
      ifelse(x == 1, "1", format(round(x, 3), nsmall = 3))
    }
    
    corrplot(
      corr_matrix, 
      col = colores_personalizados, 
      method = "color", 
      title = paste("Año", metodo),
      mar = c(1, 1, 8, 1),
      addCoef.col = "black",
      number.cex = 1.2,
      cl.ratio = 0.4,
      cl.align = "r", 
      tl.col = "black",
      number.format = format_num  # Aplicar formato personalizado
    )
    
  }
  
  # Determinar título según orientación
  if (orientacion == "io") {
    texto <- "Matrices de correlación de métodos orientado a entradas por año"
  } else if (orientacion == "oo") {
    texto <- "Matrices de correlación de métodos orientado a salidas por año"
  } else {
    texto <- "Matrices de correlación de métodos por año"
  }
  
  # Título principal centrado
  mtext(
    texto, 
    outer = TRUE, 
    cex = 1.5,  # Tamaño del texto
    font = 2,   # Estilo en negrita
    line = 1.5  # Ajusta la posición vertical
  )
  
  # Subtítulo centrado y más cerca del título
  if (subtitulo != "") {
    mtext(
      subtitulo,
      outer = TRUE, 
      cex = 1.2,  # Tamaño del texto
      font = 3,   # Cursiva
      line = 0.1  # Justo debajo del título principal
    )
  }
  
  # Cerrar el dispositivo gráfico
  dev.off()
  
  # Restablecer configuración gráfica
  par(mfrow = c(1, 1))

  

}



graficar_boxplots <- function(df, eficiencia, titulo, subtitulo) {

  color_fijo <- brewer.pal(11, "RdYlGn")[9]
  color_median <- brewer.pal(11, "RdYlGn")[11]
  color_linea <- "black"  # Color de la línea de medianas
  
  # Calcular la mediana por año
  medianas <- df %>%
    group_by(year) %>%
    summarise(mediana = median(.data[[eficiencia]], na.rm = TRUE))  # Usar `na.rm = TRUE` para evitar problemas con NA
  
  ggplot(df, aes_string(x = "year", y = eficiencia)) +
    geom_violin(fill = color_fijo, color = color_fijo, alpha = 0.6) +  # Mismo color para contorno y relleno
    #stat_summary(fun = median, geom = "point", size = 3, color = color_median) + # Agrega puntos de la mediana
    #stat_summary(fun = median, geom = "crossbar", width = 0.2, color = color_median) +
    

    geom_line(data = medianas, aes(x = factor(year), y = mediana, group = 1), 
              color = color_median, size = 0.5, linetype = "dashed") +  # Línea punteada
    geom_point(data = medianas, aes(x = factor(year), y = mediana), 
               color = color_median, size = 1) +  # Puntos en la línea
    
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Año",
      y = paste("Eficiencia", toupper(eficiencia))  # VRS o CRS
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      legend.position = "none"  # Ocultar la leyenda
    )
}
