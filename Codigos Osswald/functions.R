source("auxiliar.R")

library(randomForest)
library(Benchmarking)
library(matrixStats)
library(gridExtra)
library(openxlsx)
library(corrplot)
library(censReg)
library(Metrics)
library(readxl)
library(purrr)
library(tidyr)
library(dplyr)
library(caret)
library(deaR)



# ===================================================
# CONSOLIDACIÓN DE DATOS
# ===================================================
consolidar_datos_por_anio <- function(anio) {
  
  #browser()
  # Establecer directorio de trabajo
  #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
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

# ===================================================
#  APLICACIÓN DE ANÁLISIS ENVOLVENTE DE DATOS
#  PARA UN AÑO EN ESPECÍFICO
# ===================================================

analisis_dea_general <- function(data, orientation) {
  # --- Preparar inputs y outputs
  model <- make_deadata(data, ni=3, no="IdEstablecimiento", dmus=3, inputs=8:10, outputs=5:7)
  #browser()
  # ---Aplicar DEA con la orientación y RTS (VRS y CRS)
  resultado_dea_vrs <- model_basic(model, orientation=orientation, rts="vrs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  resultado_dea_crs <- model_basic(model, orientation=orientation, rts="crs", dmu_eval = 1:nrow(data), dmu_ref = 1:nrow(data)) 
  
  #--- Calcular eficiencias
  eficiencia_vrs <- deaR::efficiencies(resultado_dea_vrs)
  eficiencia_crs <- deaR::efficiencies(resultado_dea_crs)
  
  if (orientation == "oo"){
    eficiencia_vrs <- normalize_max_min(eficiencia_vrs)
    eficiencia_crs <- normalize_max_min(eficiencia_crs)
  }
  
  
  # Crear dataframe con eficiencias y retorno a escala
  eficiencia_df <- data.frame(
    IdEstablecimiento = data$IdEstablecimiento,
    Nombre = data$'Nombre Establecimiento',
    complejidad = data$complejidad,
    Region = data$'Region',
    vrs = round(eficiencia_vrs, 3),
    crs = round(eficiencia_crs, 3),
    escala = round(eficiencia_crs / eficiencia_vrs, 3),
    latitud = data$latitud,
    longitud = data$longitud,
    region_id = data$region_id
  )
  

  
  return(list(data = eficiencia_df, 
              dea_vrs = resultado_dea_vrs,
              dea_crs = resultado_dea_crs))
}

# ===================================================
#  ELIMINACIÓN DE DATOS SEGÚN VALOR ENTREGADO Y SEGUN 
#  SU TIPO DE DEA (VRS O CRS)
# ===================================================

sensibilidad_parametro_general <- function(data, data_original, mayor, valor, orientacion, tipo) {
  # Determinar la columna a trabajar (vrs o crs)
  #browser()
  columna <- ifelse(tipo == "vrs", "vrs", ifelse(tipo == "crs", "crs", "escala"))
  
  # Filtrar los datos en función del parámetro `mayor` y el valor dado
  if (mayor) {
    data_filtrada <- subset(data_original, data_original[[columna]] > valor)
  } else {
    data_filtrada <- subset(data_original, data_original[[columna]] < valor)
  }
  
  # Filtrar el dataset por IdEstablecimiento
  data_set <- data[data$IdEstablecimiento %in% data_filtrada$IdEstablecimiento, ]

  # Aplicar el análisis DEA
  resultados_in  <- analisis_dea_general(data_set, orientacion)
  
  return (resultados = resultados_in)
}

# ==============================================
#  CÁLCULO DE INDICE MALMQUIST
# ==============================================
malmquist <- function(datos, tipo, orientacion) {
  # Extraer inputs, outputs, ID y TIME
  input_data <- do.call(rbind, lapply(names(datos), function(year) as.matrix(datos[[year]][, 8:10])))
  output_data <- do.call(rbind, lapply(names(datos), function(year) as.matrix(datos[[year]][, 5:7])))
  ID <- unlist(lapply(datos, function(data) data$IdEstablecimiento))
  TIME <- unlist(lapply(names(datos), function(year) rep(year, nrow(datos[[year]]))))
  
  # Eliminar duplicados
  unique_indices <- !duplicated(data.frame(ID = ID, TIME = TIME))
  input_data <- input_data[unique_indices, ]
  output_data <- output_data[unique_indices, ]
  ID <- ID[unique_indices]
  TIME <- TIME[unique_indices]
  
  # Calcular el índice Malmquist
  malmquist_index <- Benchmarking::malmquist(X = input_data, Y = output_data, ID = ID, TIME = TIME, 
                                             RTS = tipo, ORIENTATION = orientacion)
  
  # Crear dataframe de resultados
  resultados_df <- data.frame(
    ID = malmquist_index$id,
    Año = malmquist_index$time,
    MalmquistIndex = malmquist_index$m,
    Effch = malmquist_index$ec,
    Techch = malmquist_index$tc
  )
  
  # Crear dataframes pivotados
  pivotar_resultados <- function(data, column) {
    data %>%
      select(ID, Año, !!sym(column)) %>%
      pivot_wider(names_from = Año, values_from = !!sym(column)) %>%
      Filter(function(x) !all(is.na(x)), .)
  }
  
  malmquist_df <- pivotar_resultados(resultados_df, "MalmquistIndex")
  effch_df <- pivotar_resultados(resultados_df, "Effch")
  techch_df <- pivotar_resultados(resultados_df, "Techch")
  
  # Crear eficiencia en formato wide
  efficiency_df <- data.frame(
    ID = malmquist_index$id,
    TIME = malmquist_index$time,
    Efficiency = malmquist_index$e11
  ) %>%
    arrange(ID, TIME) %>%
    pivot_wider(names_from = TIME, values_from = Efficiency)
  
  print(5)
  
  # Se elimina la columna que no tienen ningun valor
  malmquist_df <- Filter(function(x) !all(is.na(x)),malmquist_df)
  effch_df <- Filter(function(x) !all(is.na(x)),effch_df)
  techch_df <- Filter(function(x) !all(is.na(x)), techch_df)
  
  malmquist_df <- malmquist_index(malmquist_df)
  
  return(list(eficiencia = efficiency_df, 
              index = malmquist_df,
              tech = techch_df,
              eff = effch_df))
}

# ==============================================
#  COMBINACIÓN DE RESULTADO DE ITERACIONES
# ==============================================
combinar_resultados_iteraciones <- function(resultados_in, resultados_in_2_vrs, resultados_in_3_vrs, resultados_in_2_crs, resultados_in_3_crs,resultados_in_2_esc, resultados_in_3_esc) {
  #browser()
  # Crear una lista de dataframes, uno por cada año, con valores de VRS y CRS
  lista_resultados_combinados <- lapply(unique(names(resultados_in)), function(anio) {
    # Seleccionar los datos de las iteraciones de VRS
    df_vrs_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_1 = vrs)
    
    df_vrs_2 <- resultados_in_2_vrs[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_2 = vrs)
    
    df_vrs_3 <- resultados_in_3_vrs[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_iteracion_3 = vrs)
    
    # Unir los dataframes de VRS por IdEstablecimiento
    df_vrs_combinado <- df_vrs_1 %>%
      full_join(df_vrs_2, by = "IdEstablecimiento") %>%
      full_join(df_vrs_3, by = "IdEstablecimiento") %>%
      mutate(
        vrs_iteracion_1 = ifelse(is.na(vrs_iteracion_1), "NO APLICA", vrs_iteracion_1),
        vrs_iteracion_2 = ifelse(is.na(vrs_iteracion_2), "NO APLICA", vrs_iteracion_2),
        vrs_iteracion_3 = ifelse(is.na(vrs_iteracion_3), "NO APLICA", vrs_iteracion_3)
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_crs_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_1 = crs)
    
    df_crs_2 <- resultados_in_2_crs[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_2 = crs)
    
    df_crs_3 <- resultados_in_3_crs[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_iteracion_3 = crs)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_crs_combinado <- df_crs_1 %>%
      full_join(df_crs_2, by = "IdEstablecimiento") %>%
      full_join(df_crs_3, by = "IdEstablecimiento") %>%
      mutate(
        crs_iteracion_1 = ifelse(is.na(crs_iteracion_1), "NO APLICA", crs_iteracion_1),
        crs_iteracion_2 = ifelse(is.na(crs_iteracion_2), "NO APLICA", crs_iteracion_2),
        crs_iteracion_3 = ifelse(is.na(crs_iteracion_3), "NO APLICA", crs_iteracion_3)
      )
    
    
    df_esc_1 <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_1 = escala)
    
    df_esc_2 <- resultados_in_2_esc[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_2 = escala)
    
    df_esc_3 <- resultados_in_3_esc[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_iteracion_3 = escala)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_esc_combinado <- df_esc_1 %>%
      full_join(df_esc_2, by = "IdEstablecimiento") %>%
      full_join(df_esc_3, by = "IdEstablecimiento") %>%
      mutate(
        esc_iteracion_1 = ifelse(is.na(esc_iteracion_1), "NO APLICA", esc_iteracion_1),
        esc_iteracion_2 = ifelse(is.na(esc_iteracion_2), "NO APLICA", esc_iteracion_2),
        esc_iteracion_3 = ifelse(is.na(esc_iteracion_3), "NO APLICA", esc_iteracion_3)
      )
    
    
    
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento") %>%
      full_join(df_esc_combinado, by = "IdEstablecimiento")
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}

# ==============================================
#  GENERACIÓN DE RESULTADOS DE ITERACION
# ==============================================

resultados_iteracion <- function(datos, orientacion){
  anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
  original <-  sapply(datos, function(data) analisis_dea_general(data, orientacion), simplify = FALSE)
  names(original) <- anios
  
  if (orientacion == "io"){
      print("VRS")
      iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 0.99, orientacion, "vrs", FALSE)
      print("CRS")
      iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.98, orientacion, "crs", FALSE)
      iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 0.98, orientacion, "crs", FALSE)
      print("ESC")
      iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
      iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 0.99, orientacion, "esc", FALSE)
    
      
  }else{
    print("VRS")
    iteracion_1_vrs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    iteracion_2_vrs <- aplicar_sensibilidad(datos, lapply(iteracion_1_vrs, `[[`, "data"), 1, orientacion, "vrs", FALSE)
    print("CRS")
    iteracion_1_crs <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "crs", FALSE)
    iteracion_2_crs <- aplicar_sensibilidad(datos, lapply(iteracion_1_crs, `[[`, "data"), 1, orientacion, "crs", FALSE)
    print("ESC")
    iteracion_1_esc <- aplicar_sensibilidad(datos, lapply(original, `[[`, "data"), 1, orientacion, "esc", FALSE)
    iteracion_2_esc <- aplicar_sensibilidad(datos, lapply(iteracion_1_esc, `[[`, "data"), 1, orientacion, "esc", FALSE)
  }

  resultados_combinados <- combinar_resultados_iteraciones(original, iteracion_1_vrs, iteracion_2_vrs, iteracion_1_crs, iteracion_2_crs, iteracion_1_esc, iteracion_2_esc)
  resultados_correlacion <- calcular_correlaciones_all(resultados_combinados)
  

  lista_outliers_vrs <- list()
  vector_outliers_vrs <- c()

  lista_outliers_crs <- list()
  vector_outliers_crs <- c()

  lista_outliers_esc <- list()
  vector_outliers_esc <- c()

  # Especificar los años que quieres iterar
  for (anio in anios) {

    # Generar y almacenar los valores atípicos de VRS
    outliers_vrs <- boxplot.stats(original[[anio]][["data"]]$vrs)$out

    if (length(outliers_vrs)  > 0 ){
    ids_outliers_vrs <- original[[anio]][["data"]] %>%
      filter(vrs %in% outliers_vrs) %>%
      select(IdEstablecimiento, vrs)
      lista_outliers_vrs[[anio]] <- ids_outliers_vrs
      vector_outliers_vrs <- unique(c(vector_outliers_vrs, ids_outliers_vrs$IdEstablecimiento))
      
    }else{
      lista_outliers_vrs[[anio]] <- list()
      vector_outliers_vrs <- list()
    }
    
    # ----------------- #
    # Generar y almacenar los valores atípicos de CRS
    outliers_crs <- boxplot.stats(original[[anio]][["data"]]$crs)$out

    if (length(outliers_crs)  > 0){
      ids_outliers_crs <- original[[anio]][["data"]] %>%
        filter(crs %in% outliers_crs) %>%
        select(IdEstablecimiento, crs)
      lista_outliers_crs[[anio]] <- ids_outliers_crs
      vector_outliers_crs <- unique(c(vector_outliers_crs, ids_outliers_crs$IdEstablecimiento))
      
    }else{
      lista_outliers_crs[[anio]] <- list()
      vector_outliers_crs <- list()
    }

    # ----------------- #
    # Generar y almacenar los valores atípicos de escala
    outliers_esc <- boxplot.stats(original[[anio]][["data"]]$escala)$out
    
    if (length(outliers_esc)  > 0){
      ids_outliers_esc <- original[[anio]][["data"]] %>%
        filter(escala %in% outliers_esc) %>%
        select(IdEstablecimiento, escala)
      lista_outliers_esc[[anio]] <- ids_outliers_esc
      vector_outliers_esc <- unique(c(vector_outliers_esc, ids_outliers_esc$IdEstablecimiento))
      
    }else{
      lista_outliers_esc[[anio]] <- list()
      vector_outliers_esc <- list()
    }

  }

  list(
    original =  original,
    iteracion_1_vrs = iteracion_1_vrs,
    iteracion_2_vrs = iteracion_2_vrs,
    iteracion_1_crs = iteracion_1_crs,
    iteracion_2_crs = iteracion_2_crs,
    
    iteracion_1_esc = iteracion_1_esc,
    iteracion_2_esc = iteracion_2_esc,
    
    resultados_combinados = resultados_combinados,
    resultados_correlacion = resultados_correlacion,
    
    lista_outliers_vrs = lista_outliers_vrs,
    vector_outliers_vrs = vector_outliers_vrs,
    lista_outliers_crs = lista_outliers_crs,
    vector_outliers_crs = vector_outliers_crs,
    lista_outliers_esc = lista_outliers_esc,
    vector_outliers_esc = vector_outliers_esc
  )
}

# ==============================================
#  COMBINAR RESULTADOS IN OUT
# ==============================================
combinar_resultados_in_out <- function(resultados_in, resultados_out) {
  # Crear una lista de dataframes, uno por cada año, con valores de VRS y CRS
  lista_resultados_combinados <- lapply(unique(names(resultados_in)), function(anio) {
    # Seleccionar los datos de las iteraciones de VRS
    df_vrs_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_input = vrs)
    
    df_vrs_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, vrs) %>%
      rename(vrs_output = vrs)
    
    # Unir los dataframes de VRS por IdEstablecimiento
    df_vrs_combinado <- df_vrs_input %>%
      full_join(df_vrs_output, by = "IdEstablecimiento") %>%
      mutate(
        vrs_input = ifelse(is.na(vrs_input), NA, as.numeric(vrs_input)),
        vrs_output = ifelse(is.na(vrs_output), NA, as.numeric(vrs_output))
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_crs_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_input = crs)
    
    df_crs_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, crs) %>%
      rename(crs_output = crs)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_crs_combinado <- df_crs_input %>%
      full_join(df_crs_output, by = "IdEstablecimiento") %>%
      mutate(
        crs_input = ifelse(is.na(crs_input), NA, as.numeric(crs_input)),
        crs_output = ifelse(is.na(crs_output), NA, as.numeric(crs_output))
      )
    
    # Seleccionar los datos de las iteraciones de CRS
    df_esc_input <- resultados_in[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_input = escala)
    
    df_esc_output <- resultados_out[[anio]]$data %>%
      select(IdEstablecimiento, escala) %>%
      rename(esc_output = escala)
    
    # Unir los dataframes de CRS por IdEstablecimiento
    df_esc_combinado <- df_esc_input %>%
      full_join(df_esc_output, by = "IdEstablecimiento") %>%
      mutate(
        esc_input = ifelse(is.na(esc_input), NA, as.numeric(esc_input)),
        esc_output = ifelse(is.na(esc_output), NA, as.numeric(esc_output))
      )
    
    # Unir los resultados de VRS y CRS en un solo dataframe por IdEstablecimiento
    df_combinado <- df_vrs_combinado %>%
      full_join(df_crs_combinado, by = "IdEstablecimiento") %>%
      full_join(df_esc_combinado, by = "IdEstablecimiento")
    
    return(df_combinado)
  })
  
  # Nombrar la lista con los años para identificación
  names(lista_resultados_combinados) <- unique(names(resultados_in))
  
  return(lista_resultados_combinados)
}

# ==============================================
#  RANDOM FOREST
# ==============================================
analize_rf <- function(year, resultados_in, n_top,tipo, orientacion ){
  #year <- 2014
  print("---------------------------")
  print("---------------------------")
  print(paste0("AÑO: ", year, "- RETORNO: ", tipo))
  
  data_path <- paste0("data/", year, "/", year, "_consolidated_data.csv")
  
  #print(data_path)
  # Leer los datos consolidados
  datos_consolidados <- read.table(data_path, sep = ";", header = TRUE)
  colnames(datos_consolidados) <- gsub("\\.0$", "", colnames(datos_consolidados))
  
  df <- datos_consolidados

  
  # Convertir columnas a enteros
  df[colnames(datos_consolidados)] <- lapply(df[colnames(datos_consolidados)], as.integer)
  
  # Filtrar los resultados de VRS
  df_vrs <- resultados_in[["original"]][[as.character(year)]][["data"]][, c("IdEstablecimiento", tipo)] %>% 
    rename("idEstablecimiento" = "IdEstablecimiento")


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
  
  cat(paste0("\n", year, "-", orientacion, " ", tipo))
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

  
  print("---------------------------")
  print("---------------------------")
  
  return(list(importancia = importancia,
              modelo = modelo_rf,
              correlaciones = top_correlacion))
}


# ==============================================
#  DETERMINANTES IMPORTANCIA
# ==============================================

determinantes_importancia_single <- function(random_forest, anios_pre_pandemia, anios_pandemia) {

  # Crear dataframes vacíos
  df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  df_corr <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  
  # Recopilar datos por año
  for (anio in names(random_forest)) {
    importancia <- random_forest[[anio]]$importancia
    importancia <- importancia[order(-importancia[, "%IncMSE"]), ]  
  
    temp_IncMSE <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "%IncMSE"],
      Año = anio
    )
    df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)
    
    temp_IncNodePurity <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "IncNodePurity"],
      Año = anio
    )
    df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
    
    temp_corr <- data.frame(
      Variable = rownames(importancia),
      Valor = random_forest[[anio]]$correlaciones,
      Año = anio
    )
    df_corr <- rbind(df_corr, temp_corr)
  }
  browser()
  pivot_IncMSE <- df_IncMSE %>% pivot_wider(names_from = Año, values_from = Valor)
  pivot_IncMSE$Frecuencia_General <- rowSums(!is.na(pivot_IncMSE[, -1]))
  pivot_IncMSE$Frecuencia_Pre <- rowSums(!is.na(pivot_IncMSE[, anios_pre_pandemia]))
  pivot_IncMSE$Frecuencia_Post <- rowSums(!is.na(pivot_IncMSE[, anios_pandemia]))
  
  pivot_IncMSE$Promedio_General <- rowMeans(pivot_IncMSE[, -1], na.rm = TRUE)
  pivot_IncMSE$Promedio_Pre <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
  pivot_IncMSE$Promedio_Post <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
  
  pivot_IncMSE$Varianza_General <- apply(pivot_IncMSE[, -1], 1, var, na.rm = TRUE)
  pivot_IncMSE$Varianza_Pre <- apply(pivot_IncMSE[, anios_pre_pandemia], 1, var, na.rm = TRUE)
  pivot_IncMSE$Varianza_Post <- apply(pivot_IncMSE[, anios_pandemia], 1, var, na.rm = TRUE)
  
  pivot_IncMSE <- pivot_IncMSE %>%
    filter(Frecuencia_General >= 2)
  
  pivot_Pre <- pivot_IncMSE %>%
    filter(Varianza_Pre <= quantile(Varianza_Pre, 0.75, na.rm = TRUE) & Frecuencia_Pre >= 2) %>%
    arrange(desc(Promedio_Pre))
  
  pivot_Post <- pivot_IncMSE %>%
    filter(Varianza_Post <= quantile(Varianza_Post, 0.75, na.rm = TRUE) & Frecuencia_Post >= 2) %>%
    arrange(desc(Promedio_Post))
  
  pivot_General <- pivot_IncMSE %>%
    filter(Varianza_General <= quantile(Varianza_General, 0.75, na.rm = TRUE)) %>%
    arrange(desc(Promedio_General))
  
  
  pivot_General <- pivot_General[order(-pivot_General$Promedio_General), ]
  pivot_Pre <- pivot_Pre[order(-pivot_Pre$Promedio_Pre), ] 
  pivot_Post <- pivot_Post[order(-pivot_Post$Promedio_Post), ] 
  
  top50_IncMSE_General <- head(pivot_General$Variable, 50)
  top50_IncMSE_Pre <- head(pivot_Pre$Variable, 50)
  top50_IncMSE_Post <- head(pivot_Post$Variable, 50)
  
  
  
  
  
  
  pivot_IncNodePurity <- df_IncNodePurity %>% pivot_wider(names_from = Año, values_from = Valor)
  pivot_corr <- df_corr %>% pivot_wider(names_from = Año, values_from = Valor)
  
  generar_dataframe_intercalado <- function(variables_seleccionadas, incmse, incnodepurity, corr, anios, periodo) {
    df_final <- data.frame(Variable = variables_seleccionadas, stringsAsFactors = FALSE)
    
    for (anio in anios) {
      df_final[[paste0(anio, "_IncMSE")]] <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), anio]))
      df_final[[paste0(anio, "_IncNodePurity")]] <- as.numeric(unlist(incnodepurity[match(df_final$Variable, incnodepurity$Variable), anio]))
      df_final[[paste0(anio, "_Correlacion")]] <- as.numeric(unlist(corr[match(df_final$Variable, corr$Variable), anio]))
    }
    
    if (periodo == "pre"){
      df_final$Frecuencia <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Frecuencia_Pre"]))
      df_final$Promedio <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Promedio_Pre"]))
      df_final$Varianza <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Varianza_Pre"]))
      
    }else if (periodo == "post"){
      df_final$Frecuencia <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Frecuencia_Post"]))
      df_final$Promedio <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Promedio_Post"]))
      df_final$Varianza <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Varianza_Post"]))
      
    }else{
      df_final$Frecuencia <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Frecuencia_General"]))
      df_final$Promedio <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Promedio_General"]))
      df_final$Varianza <- as.numeric(unlist(incmse[match(df_final$Variable, incmse$Variable), "Varianza_General"]))
      
    }

    return(df_final)
  }
  
  df_pre_pandemia <- generar_dataframe_intercalado(top50_IncMSE_Pre, pivot_Pre, pivot_IncNodePurity, pivot_corr, anios_pre_pandemia, "pre")
  df_post_pandemia <- generar_dataframe_intercalado(top50_IncMSE_Post, pivot_Post, pivot_IncNodePurity, pivot_corr, anios_pandemia, "post")
  df_general <- generar_dataframe_intercalado(top50_IncMSE_General, pivot_General, pivot_IncNodePurity, pivot_corr, c(anios_pre_pandemia, anios_pandemia), "general")
  
  return(list(
    DataFrame_Pre = df_pre_pandemia,
    DataFrame_Post = df_post_pandemia,
    DataFrame_General = df_general
  ))
}


# ==============================================
#  FILTRO DE DATOS SEGUN DATOS ATIPICOS DE ITERACIONES AL CONJUNTO ORIGINAL
# ==============================================

datos_filtrados_atipicos <- function(datos, resultados) {
  # Filtrar datos
  datos_filtrados_vrs_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_vrs"]])
  datos_filtrados_crs_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_crs"]])
  datos_filtrados_vrs_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_vrs"]])
  datos_filtrados_crs_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_crs"]])
  datos_filtrados_esc_io <- filtrar_datos(datos, resultados[["io"]][["vector_outliers_esc"]])
  datos_filtrados_esc_oo <- filtrar_datos(datos, resultados[["oo"]][["vector_outliers_esc"]])
  
  return (list(
    vrs_io = datos_filtrados_vrs_io,
    crs_io = datos_filtrados_crs_io,
    vrs_oo = datos_filtrados_vrs_oo,
    crs_oo = datos_filtrados_crs_oo,
    esc_io = datos_filtrados_esc_io,
    esc_oo = datos_filtrados_esc_oo
    
  ))
}

# ==============================================
#  GUARDAR DATOS DE EFICIENCIA Y DETERMINANTES
# ==============================================
guardar_resultados <- function(dataframes, retorno, resultados_importancia,
                               malmquist,
                               carpeta,
                               archivo_salida, prefijo) {

  
  ruta_completa <- file.path(carpeta, archivo_salida)
  
  # Verificar si la carpeta existe, si no, crearla
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = TRUE)  # recursive = TRUE permite crear subcarpetas si es necesario
  }
  
  
  reemplazar_nulos <- function(df) {
    df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.x) | .x == "", "-", .x)))
    
    return(df)
  }  
  
  
  # Crear un workbook
  wb <- createWorkbook()
  
  # EFICIENCIA TÉCNICA
  browser()
  ef_tec <- guardar_dataframe_por_columna(dataframes, retorno)

  addWorksheet(wb, "ET")
  writeData(wb, "ET", ef_tec)

  
  
  # MALMQUIST
  addWorksheet(wb, "MALMQUIST")
  writeData(wb, "MALMQUIST", malmquist)


  
  #df_pre <- as.data.frame(resultados_importancia[["DataFrame_Pre"]])
  #df_post <- as.data.frame(resultados_importancia[["DataFrame_Post"]])
  #df_general <- as.data.frame(resultados_importancia[["DataFrame_General"]])
  
  # Escribir en el Excel solo si no están vacíos
  if (nrow(resultados_importancia) > 0) {
    addWorksheet(wb, "DETERM")
    writeData(wb, "DETERM", resultados_importancia)
  }

  
  # Convertir listas a DataFrames si es necesario
  #df_pre <- as.data.frame(resultados_importancia[["DataFrame_Pre"]])
  #df_post <- as.data.frame(resultados_importancia[["DataFrame_Post"]])
  #df_general <- as.data.frame(resultados_importancia[["DataFrame_General"]])
  
  # Escribir en el Excel solo si no están vacíos
  #if (nrow(df_pre) > 0) {
  #  addWorksheet(wb, "DET PRE")
  #  writeData(wb, "DET PRE", df_pre)
  #}
  
  #if (nrow(df_post) > 0) {
  #  addWorksheet(wb, "DET POST")
  #  writeData(wb, "DET POST", df_post)
  #}
  
  #if (nrow(df_general) > 0) {
  #  addWorksheet(wb, "DET GENERAL")
  #  writeData(wb, "DET GENERAL", df_general)
  #}


  
  
  # Guardar el archivo
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  cat("Archivo guardado como:", archivo_salida, "\n")
}



# PARA VERIFICAR QUE LAS EFICIENCIAS CAMBIAN
verificar_normalidad <- function(df, columnas) {
  resultados <- data.frame(Columna = character(), p_valor = numeric(), Normalidad = character())
  for (col in columnas) {
    mat <- as.matrix(df[,col])
    vec <- as.numeric(mat) 
    
    shapiro <- shapiro.test(vec)
    resultados <- rbind(resultados, data.frame(
      Columna = col,
      p_valor = shapiro$p.value,
      Normalidad = ifelse(shapiro$p.value > 0.05, "Sí", "No")
    ))
  }
  return(resultados)
}

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
    lista_corr[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, Corr = df_top50$Corr)
    lista_todos[[as.character(anio)]] <- data.frame(Variable = df_top50$Variable, IncMSE = df_top50$IncMSE, Corr = df_top50$Corr)
    
    # Filtrar las 50 variables con mayor IncMSE en ese año
    df_top50$Año <- anio
    
    # Guardar en la lista de Top 50
    lista_top50_incmse[[as.character(anio)]] <- df_top50
    
  }
  
  
  
  # Crear dataframes con años como columnas
  df_incmse <- crear_dataframe(lista_incmse)
  df_incmse_10  <- crear_dataframe(lista_incmse_10)
  
  return(list(top_50 = lista_top50_incmse,
    df_incmse = df_incmse,
    df_incmse_10 = df_incmse_10,
    df_incmse_est = calcular_estadisticas(df_incmse),
    df_incmse_est_10 = calcular_estadisticas(df_incmse_10) ))
}
