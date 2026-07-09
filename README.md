# repositorio_datos_inflacion

Descripción de la carpeta inflación (Dropbox — inflacion)
La carpeta principal contiene cinco subcarpetas y la presentación más actualizada, inflacion_final_m3, la cual también se encuentra en este repositorio.
El contenido de las cinco carpetas es el siguiente:
1. Precios_promedio
Contiene los precios promedio del pollo y la electricidad para el periodo 2025–2026, desagregados por área geográfica. Estos archivos son la fuente primaria de precios que se utiliza en la Actividad 3.
2. Documentos_inflacion
Contiene los dos documentos de referencia teórica y metodológica:

inflacion.pdf — Metodología principal del INPC publicada por el INEGI. Describe el marco conceptual del índice, la estructura de genéricos y subíndices, los criterios de ponderación y el método de encadenamiento con base en la Encuesta Nacional de Gasto de los Hogares (ENGASTO).
paper_inflacion.pdf — Artículo académico base sobre el cual se construyeron las definiciones de trimmed inflation, core inflation y median inflation utilizadas en las actividades. Provee el sustento conceptual para entender las diferencias entre cada medida alternativa de inflación subyacente.

3. Ponderadores
Contiene dos archivos:

ponderadores_limpio — Versión simplificada con los ponderadores nacionales de los rubros de gasto, lista para usar directamente en los cálculos.
Archivo principal de ponderadores — Contiene los ponderadores nacionales por genérico junto con el factor de encadenamiento. Incluye la desagregación por genérico y la clasificación de cada uno como subyacente o no subyacente.

4. Ciudades
Contiene 55 archivos con el histórico de índices genéricos por área geográfica para el periodo 2025–2026. No todos los archivos contienen el universo completo de genéricos, pero todos incluyen el índice del pollo y el de la electricidad. Estos archivos son la fuente a partir de la cual se construye el archivo maestro_indices_ciudades, descrito en la siguiente sección.

6. Codigos_inflacion
Contiene los cuatro scripts principales para la realización de las actividades propuestas:

inflacion_actividad1.R — Actividad 1.
inflacion_actividad2.R — Actividad 2.
actividad_inflacion3.R — Actividad 3. Trata la electricidad como cualquier genérico para observar el resultado y compararlo con la aproximación metodológica específica desarrollada en el script complementario.
electricidad_metodo3.R — Script complementario a actividad_inflacion3.R. Implementa la metodología particular para el genérico de electricidad, cuya estructura de precios y estacionalidad requiere un tratamiento distinto al resto de genéricos.

Además de los scripts principales, esta carpeta contiene las bases de datos construidas para facilitar los cálculos:

inpc_general — Datos descargados del INPC general, subyacente y no subyacente para el periodo 2023–2026. Es la base de la Actividad 1.
genericos_vinculados — Resultado del merge entre la base de ponderadores nacionales por genérico y los 292 índices genéricos para el periodo 2024–2026.
maestro_indices_ciudades.csv — Merge de los índices del pollo y la electricidad por área geográfica para el periodo 2025–2026, construido a partir de los 55 archivos de la carpeta Ciudades. Selecciona únicamente esos dos genéricos y genera un archivo consolidado. Es la base principal de la Actividad 3.
maestro_precios_promedio.csv — Agregación de los archivos de la carpeta Precios_promedio en un único archivo consolidado. Se utiliza junto con maestro_indices_ciudades.csv en la Actividad 3.
