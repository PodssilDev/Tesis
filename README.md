# Análisis y comparación de la eficiencia técnica de hospitales públicos en Chile usando SFA y DEA

Este proyecto de investigación analiza la eficiencia técnica de hospitales públicos en Chile utilizando 
el Análisis de Fronteras Estocásticas (SFA) y Análisis Envolvente de Datos (DEA). 
Incluye un flujo completo de procesamiento de datos, cálculo de eficiencias, 
análisis de determinantes, pruebas estadísticas, visualización geográfica 
y generación de un informe automatizado en PDF. Además de obtener resultados con SFA, el cual es un
método poco utilizado en la literatura de la eficiencia técnica, se busca comparar los resultados con DEA
para ver cuanto difieren, si los resultados dependen del método empleado o bien, si se complementan entre si.

## Estructura principal del repositorio
```
├── src/                # Scripts principales (procesamiento, modelos, pruebas, mapas, etc.)
├── resultados/         # Resultados generados
│   ├── mapas/          # Mapas de eficiencia por año
│   ├── determinantes/  # Gráficos de variables determinantes y registro de determinantes con su incMSE
│   ├── comparacion/    # Resultados de comparación SFA vs DEA (Planillas .xlsx y graficos de barra)
│   └── informe/        # Informe PDF con resultados obtenidos y datos asociados (.RDATA)
├── data/               # Datos de entradas, obtenidos de sitios web públicos como DEIS y FONASA, además de los resultados de DEA 
├── main                # Menú interactivo para ejecutar el proyecto, calcular tiempo de ejecución y aplicar pruebas de validación
└── README.md      
```

## Alcances de la investigación

El proyecto  abarca la consolidación de datos hospitalarios de Chile entre los años 2014 y 2023, normalizando la información proveniente de distintas fuentes para construir una base homogénea y trazable. Con estos datos se estiman niveles de eficiencia técnica mediante modelos de frontera estocástica (SFA), considerando especificaciones Cobb-Douglas y se genera un indicador global que integra áreas clave como egresos, consultas e intervenciones quirúrgicas.

Además, se evalúa la distribución de las eficiencias, identificando valores atípicos y realizando análisis de sensibilidad para determinar su impacto. El estudio incorpora la identificación de los principales factores asociados a la eficiencia utilizando técnicas de aprendizaje automático (Random Forest) y compara los resultados obtenidos con los del enfoque DEA, analizando distancias y correlaciones por año y complejidad hospitalaria. Finalmente, se generan visualizaciones geográficas, análisis estadísticos complementarios y se analizan los resultados obtenidos, abarcando las complejidades hospitalarias año a año.

<img width="1127" height="587" alt="diagramamodelado" src="https://github.com/user-attachments/assets/827608f9-5f0b-485c-ade2-26a387b9baaf" />



## Requisitos técnicos utilizados

- [R 4.4.3](https://www.r-project.org)
- [RStudio 2025.05.1+513](https://posit.co/download/rstudio-desktop)
- Paquetes indicados en `src/dependencias.R`
- LaTeX instalado (para exportar el informe a PDF)
- Sistema Operativo Windows 11

## Paso a paso de ejecución

1. Clonar el repositorio:
 ```
 git clone https://github.com/PodssilDev/Tesis.git
 ```
2. Instalar dependencias ejecutando el archivo `dependencias.R` disponible en `src/`
3. Ejecutar el menú interactivo, disponible en el archivo `main.R`
4. Seguir las instrucciones de la consola para:
  * Procesar datos
  * Calcular eficiencia técnica
  * Generar mapas y gráficas
  * Cálcular métricas y aplicar pruebas sobre la eficiencia técnica
  * Obtener los determinantes de la eficiencia técnica
  * Crear un informe PDF con resultados (gráficos, tablas, entre otros)


## Ejemplo de resultados
A continuación, se muestra un gráfico de violin que abarca la distribución de la eficiencia técnica de hospitales públicos en Chile en el periodo 2014-2023, considerando las 3 complejidades hospitalarias: alta, mediana y baja.

<img width="1652" height="548" alt="image" src="https://github.com/user-attachments/assets/fa7308f8-abfe-494c-ac1c-ef399eb5a5a9" />

## Agradecimientos

Este proyecto de investigación fue realizado en el marco de una tesis de pregrado para obtener el título de Ingeniero Civil en Informática. Agradezco al Departamento de Ingeniería en Informática (DIINF),
a mi profesor guía Manuel Villalobos y a todos quienes aportaron para que este proyecto de investigación fuera realizado exitosamente.
