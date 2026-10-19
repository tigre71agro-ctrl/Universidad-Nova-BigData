Proyecto académico — Curso de Big Data.
para clasificación

28 985

Exactitud del modelo de clasificación

72,71 %

AUC-ROC

0,6693

Estudiantes analizados con K-means

14 727

Clústeres identificados

2

Índice de silueta

0,53510

Procesamiento distribuido

Se configuró un clúster Apache Spark mediante Docker con un nodo Master y un Worker. Se verificó la ejecución de SparkPi, una consulta con SparklyR y un modelo de regresión logística sobre el clúster.

El modelo distribuido obtuvo métricas coincidentes con las del modelo ejecutado previamente en Spark local.

Archivos del repositorio

Los scripts R permiten desarrollar las diferentes etapas del proyecto. Para reproducirlo, deben ejecutarse en orden, utilizando las versiones corregidas y adaptando las rutas de archivos al directorio de trabajo del usuario.

Los archivos CSV de resultados y las evidencias de ejecución deben conservarse en sus carpetas correspondientes.

Consideraciones

Todos los datos empleados son sintéticos y no representan información de personas reales.

Los registros separados en cuarentena no se consideran corregidos. Asimismo, los modelos predictivos tienen limitaciones de desempeño y no están validados para tomar decisiones académicas reales.

