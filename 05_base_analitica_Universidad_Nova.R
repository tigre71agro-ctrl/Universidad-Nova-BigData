# ============================================================================
# UNIVERSIDAD NOVA | PROYECTO BIG DATA | APARTADO 3.9
# CONSTRUCCION DE BASE ANALITICA PARA SPARKLYR - SCRIPT COMPLETO
# Requiere: nueve archivos CSV del ETL 3.8 dentro de 'datos_depurados/'.
# No instala paquetes. No modifica datos_simulados, datos_con_errores
# ni datos_depurados: solamente escribe 'base_analitica/' y
# 'resultados_analitica/'.
# Fecha de corte del proyecto: 2026-10-18.
# NOTA: resultados estrictamente simulados; no inferir riesgos personales.
# ============================================================================

cat("\n*** UNIVERSIDAD NOVA - APARTADO 3.9 - BASE ANALITICA V1 ***\n")
fecha_corte <- as.Date("2026-10-18")
ruta_entrada <- "datos_depurados"
ruta_salida <- "base_analitica"
ruta_reportes <- "resultados_analitica"
set.seed(2026)

validar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call. = FALSE)
}

# PASO 1. ENCONTRAR Y PROTEGER LAS ENTRADAS -----------------------------------
cat("\nPASO 1 - Verificando directorio y archivos de entrada...\n")
archivos <- c("carreras", "estudiantes", "docentes", "administrativos",
              "asignaturas", "matriculas", "calificaciones", "pagos",
              "actividad_virtual")
rutas <- file.path(ruta_entrada, paste0(archivos, ".csv"))
validar(dir.exists(ruta_entrada), paste0(
  "No existe la carpeta 'datos_depurados' dentro de: ", getwd(),
  ". Configure el directorio de trabajo que contiene esa carpeta."))
validar(all(file.exists(rutas)), paste0(
  "Faltan los CSV depurados: ",
  paste(basename(rutas[!file.exists(rutas)]), collapse=", ")))
md5_antes <- unname(tools::md5sum(rutas))

# PASO 2. EXTRAER Y VALIDAR LA ESTRUCTURA ------------------------------------
cat("PASO 2 - Importando los CSV depurados...\n")
leer <- function(archivo) {
  read.csv(archivo, stringsAsFactors=FALSE, fileEncoding="UTF-8",
           na.strings=c("", "NA"))
}
d <- setNames(lapply(rutas, leer), archivos)
requeridos <- list(
  carreras=c("carrera_id", "carrera", "facultad"),
  estudiantes=c("estudiante_id", "carrera_id", "anio_ingreso", "estado"),
  asignaturas=c("asignatura_id", "carrera_id", "ciclo"),
  matriculas=c("matricula_id", "estudiante_id", "asignatura_id",
               "periodo", "fecha_matricula"),
  calificaciones=c("matricula_id", "tipo_evaluacion", "nota", "fecha_evaluacion"),
  actividad_virtual=c("matricula_id", "fecha_actividad",
                     "tipo_actividad", "duracion_minutos")
)
for (tb in names(requeridos)) {
  validar(all(requeridos[[tb]] %in% names(d[[tb]])),
          paste("Faltan columnas requeridas en:", tb))
}
ca <- d$carreras
es <- d$estudiantes
asig <- d$asignaturas
mat <- d$matriculas
cal <- d$calificaciones
act <- d$actividad_virtual

validar(nrow(mat)>0L && nrow(es)>0L && nrow(cal)>0L,
        "Hay tablas obligatorias sin registros.")
validar(!anyDuplicated(mat$matricula_id) &&
          !anyDuplicated(es$estudiante_id) &&
          !anyDuplicated(asig$asignatura_id) &&
          !anyDuplicated(ca$carrera_id),
        "Las claves de las tablas depuradas no son unicas.")
validar(all(cal$tipo_evaluacion %in% c("Parcial", "Final")),
        "Las calificaciones contienen tipos de evaluacion no previstos.")
validar(all(act$matricula_id %in% mat$matricula_id) &&
          all(cal$matricula_id %in% mat$matricula_id),
        "Existen actividades o notas sin matricula en la base depurada.")

# PASO 3. CONSTRUIR RELACIONES Y DIMENSIONES ---------------------------------
cat("PASO 3 - Construyendo las relaciones entre entidades...\n")
i_est <- match(mat$estudiante_id, es$estudiante_id)
i_asig <- match(mat$asignatura_id, asig$asignatura_id)
validar(!anyNA(i_est) && !anyNA(i_asig),
        "Hay matriculas sin estudiante o asignatura depurada.")
i_carrera <- match(es$carrera_id[i_est], ca$carrera_id)
validar(!anyNA(i_carrera), "Una carrera de estudiante no existe.")
validar(all(es$carrera_id[i_est] == asig$carrera_id[i_asig]),
        "Estudiantes y asignaturas asociados a carreras distintas.")

# Seudonimos reproducibles de uso analitico (no guardamos la tabla de enlace).
# No se publican nombres ni documentos. No es anonimato irreversible.
ids_est <- sort(unique(as.character(es$estudiante_id)))
seudonimos <- sprintf("U%05d", sample.int(length(ids_est)))
seudonimo_por_est <- seudonimos[match(as.character(mat$estudiante_id), ids_est)]
validar(!anyNA(seudonimo_por_est), "Error al asignar seudonimos.")

dim_carreras <- ca[,c("carrera_id", "carrera", "facultad"),drop=FALSE]
dim_asignaturas <- asig[,c("asignatura_id", "carrera_id", "ciclo"),drop=FALSE]

# PASO 4. SEPARAR EVALUACIONES PARCIALES Y FINALES ----------------------------
cat("PASO 4 - Integrando notas parciales y finales...\n")
parciales <- cal[cal$tipo_evaluacion=="Parcial",,drop=FALSE]
finales <- cal[cal$tipo_evaluacion=="Final",,drop=FALSE]
validar(!anyDuplicated(parciales$matricula_id) &&
          !anyDuplicated(finales$matricula_id),
        "Hay mas de una nota Parcial o Final para una matricula.")
i_parcial <- match(mat$matricula_id, parciales$matricula_id)
i_final <- match(mat$matricula_id, finales$matricula_id)
nota_parcial <- suppressWarnings(as.numeric(parciales$nota[i_parcial]))
nota_final <- suppressWarnings(as.numeric(finales$nota[i_final]))
fecha_parcial <- as.Date(parciales$fecha_evaluacion[i_parcial], format="%Y-%m-%d")
fecha_final <- as.Date(finales$fecha_evaluacion[i_final], format="%Y-%m-%d")
fecha_matricula <- as.Date(mat$fecha_matricula, format="%Y-%m-%d")

fact <- data.frame(
  matricula_id=as.character(mat$matricula_id),
  estudiante_pseudo=seudonimo_por_est,
  carrera_id=as.character(es$carrera_id[i_est]),
  facultad=as.character(ca$facultad[i_carrera]),
  asignatura_id=as.character(mat$asignatura_id),
  ciclo=as.integer(asig$ciclo[i_asig]),
  periodo=as.character(mat$periodo),
  fecha_matricula=fecha_matricula,
  fecha_parcial=fecha_parcial,
  fecha_final=fecha_final,
  nota_parcial=nota_parcial,
  nota_final=nota_final,
  stringsAsFactors=FALSE
)
validar(all(!is.na(fact$fecha_matricula) &
              fact$fecha_matricula<=fecha_corte),
        "Las fechas de matricula no cumplen el corte temporal.")

# PASO 5. CALCULAR ACTIVIDAD OCURRIDA DESPUES DEL PARCIAL ---------------------
# Se cuentan solo eventos entre Parcial y Final (sin incluir dia del examen
# final). Los eventos posteriores al desenlace no ingresan como predictores.
cat("PASO 5 - Calculando actividad previa al examen final...\n")
fact$actividades_entre_evaluaciones <- integer(nrow(fact))
fact$minutos_entre_evaluaciones <- numeric(nrow(fact))
fact$tareas_entre_evaluaciones <- integer(nrow(fact))
pos <- match(act$matricula_id, fact$matricula_id)
validar(!anyNA(pos), "Actividades sin matricula despues de la integracion.")
f_actividad <- as.Date(act$fecha_actividad, format="%Y-%m-%d")
minutos <- suppressWarnings(as.numeric(act$duracion_minutos))
validar(!anyNA(f_actividad) && !anyNA(minutos) &&
          all(is.finite(minutos) & minutos>0),
        "La actividad depurada tiene fechas o duraciones invalidas.")
en_ventana <- !is.na(fact$fecha_parcial[pos]) &
             !is.na(fact$fecha_final[pos]) &
             f_actividad>fact$fecha_parcial[pos] &
             f_actividad<fact$fecha_final[pos] &
             f_actividad<=fecha_corte
validar(!anyNA(en_ventana), "La seleccion temporal contiene valores NA.")

if (any(en_ventana)) {
  posiciones <- pos[en_ventana]
  fact$actividades_entre_evaluaciones <- tabulate(
    posiciones, nbins=nrow(fact))
  suma_minutos <- rowsum(matrix(minutos[en_ventana], ncol=1L),
                         group=posiciones, reorder=FALSE)
  fact$minutos_entre_evaluaciones[as.integer(rownames(suma_minutos))] <-
    as.numeric(suma_minutos[,1L])
  es_tarea <- en_ventana & act$tipo_actividad=="Entrega_tarea"
  es_tarea[is.na(es_tarea)] <- FALSE
  if (any(es_tarea)) {
    fact$tareas_entre_evaluaciones <- tabulate(
      pos[es_tarea], nbins=nrow(fact))
  }
}
validar(all(fact$tareas_entre_evaluaciones <=
              fact$actividades_entre_evaluaciones),
        "El numero de tareas supera el numero de eventos.")

# PASO 6. DEFINIR VARIABLE OBJETIVO SIN FUGA DE INFORMACION ------------------
# Resultado: haber aprobado el examen final simulado (nota >= 11/20).
# Solo se modelan matriculas con parcial Y final validos. La nota final
# NO se exporta como variable predictora del dataset_ml_clasificacion.
cat("PASO 6 - Preparando el objetivo y la cohorte modelable...\n")
fact$aprobado_final <- ifelse(is.na(fact$nota_final), NA_integer_,
                             as.integer(fact$nota_final>=11))
modelable <- !is.na(fact$nota_parcial) & !is.na(fact$nota_final) &
            is.finite(fact$nota_parcial) & is.finite(fact$nota_final) &
            fact$nota_parcial>=0 & fact$nota_parcial<=20 &
            fact$nota_final>=0 & fact$nota_final<=20 &
            !is.na(fact$fecha_parcial) & !is.na(fact$fecha_final) &
            fact$fecha_parcial>fact$fecha_matricula &
            fact$fecha_final>fact$fecha_parcial &
            fact$fecha_final<=fecha_corte &
            !is.na(fact$ciclo)
modelable[is.na(modelable)] <- FALSE
fact$registro_modelable <- as.integer(modelable)
validar(sum(modelable)>=1000L,
        "Hay menos de 1000 matriculas con ambas evaluaciones validas.")

# Mantener identificadores solo para trazabilidad y particion por estudiante.
# No incluir nota_final, fecha_final ni datos financieros en variables ML.
ml <- fact[modelable,c(
  "matricula_id", "estudiante_pseudo", "carrera_id", "ciclo", "periodo",
  "nota_parcial", "actividades_entre_evaluaciones",
  "minutos_entre_evaluaciones", "tareas_entre_evaluaciones",
  "aprobado_final"),drop=FALSE]
validar(all(!is.na(ml)) &&
          all(ml$aprobado_final %in% c(0L,1L)) &&
          length(unique(ml$aprobado_final))==2L,
        "Hay faltantes o una sola clase en el conjunto para clasificacion.")
validar(!any(c("nota_final", "fecha_final", "documento_simulado", "nombre",
              "estado_pago", "monto") %in% names(ml)),
        "Fuga de informacion: variables no permitidas en el dataset ML.")

# PASO 7. SEPARAR ENTRENAMIENTO / PRUEBA POR ESTUDIANTE -----------------------
# Un mismo estudiante no puede aparecer en entrenamiento y prueba.
cat("PASO 7 - Creando particion reproducible por estudiante...\n")
est_ml <- sort(unique(ml$estudiante_pseudo))
validar(length(est_ml)>=20L, "Muy pocos estudiantes para particionar.")
set.seed(2026)
n_train <- floor(0.80*length(est_ml))
est_train <- est_ml[sample.int(length(est_ml), n_train)]
ml$particion <- ifelse(ml$estudiante_pseudo %in% est_train,
                       "entrenamiento", "prueba")
validar(length(intersect(
  unique(ml$estudiante_pseudo[ml$particion=="entrenamiento"]),
  unique(ml$estudiante_pseudo[ml$particion=="prueba"])))==0L,
  "Un estudiante aparece en entrenamiento y prueba.")
validar(all(c("entrenamiento","prueba") %in% ml$particion),
        "No se generaron ambos conjuntos de datos.")
validar(all(c(0L,1L) %in% ml$aprobado_final[ml$particion=="entrenamiento"]) &&
          all(c(0L,1L) %in% ml$aprobado_final[ml$particion=="prueba"]),
        "Una particion carece de una clase. Revise el reparto de estudiantes.")

# PASO 8. PREPARAR UN CONJUNTO PARA AGRUPAMIENTO -----------------------------
# Se agrupan perfiles historicos de participacion y parciales, NO el
# resultado final; el agrupamiento no usa la variable objetivo.
cat("PASO 8 - Construyendo la tabla para agrupamiento...\n")
vars_grupo <- c("nota_parcial", "actividades_entre_evaluaciones",
                "minutos_entre_evaluaciones", "tareas_entre_evaluaciones")
clust <- aggregate(ml[,vars_grupo,drop=FALSE],
                   by=list(estudiante_pseudo=ml$estudiante_pseudo), FUN=mean)
n_matriculas <- table(ml$estudiante_pseudo)
clust$matriculas_evaluables <- as.integer(
  n_matriculas[match(clust$estudiante_pseudo, names(n_matriculas))])
names(clust)[names(clust)=="nota_parcial"] <- "promedio_nota_parcial"
names(clust)[names(clust)=="actividades_entre_evaluaciones"] <-
  "promedio_actividades"
names(clust)[names(clust)=="minutos_entre_evaluaciones"] <-
  "promedio_minutos"
names(clust)[names(clust)=="tareas_entre_evaluaciones"] <-
  "promedio_tareas"
validar(!anyNA(clust) && !anyDuplicated(clust$estudiante_pseudo),
        "El dataset para agrupamiento contiene valores invalidos.")

# PASO 9. VERIFICAR INTEGRIDAD Y EMITIR INFORMES -----------------------------
cat("PASO 9 - Preparando el resumen y el diccionario de variables...\n")
validar(nrow(fact)==nrow(mat) &&
          !anyDuplicated(fact$matricula_id) &&
          nrow(ml)==sum(fact$registro_modelable),
        "No coincide el numero de matriculas integrado o modelables.")
validar(nrow(ml)==sum(ml$particion=="entrenamiento") +
                     sum(ml$particion=="prueba"),
        "Error en el reparto entrenamiento/prueba.")
validar(sum(clust$matriculas_evaluables)==nrow(ml),
        "El agrupamiento no conserva el numero de matriculas modelables.")
validar(identical(md5_antes,unname(tools::md5sum(rutas))),
        "Un archivo fuente depurado se modifico durante el procesamiento.")

resumen <- data.frame(
  indicador=c("matriculas_depuradas", "notas_parciales_disponibles",
              "notas_finales_disponibles", "matriculas_modelables",
              "matriculas_no_modelables", "actividades_depuradas",
              "actividades_entre_parcial_y_final", "estudiantes_modelables",
              "registros_entrenamiento", "registros_prueba",
              "estudiantes_entrenamiento", "estudiantes_prueba",
              "aprobados_final", "no_aprobados_final",
              "filas_dataset_agrupamiento"),
  valor=c(nrow(mat), sum(!is.na(i_parcial)),sum(!is.na(i_final)),
          nrow(ml),sum(!modelable),nrow(act),sum(en_ventana),
          length(est_ml),sum(ml$particion=="entrenamiento"),
          sum(ml$particion=="prueba"),
          length(unique(ml$estudiante_pseudo[ml$particion=="entrenamiento"])),
          length(unique(ml$estudiante_pseudo[ml$particion=="prueba"])),
          sum(ml$aprobado_final==1L),sum(ml$aprobado_final==0L),nrow(clust)),
  stringsAsFactors=FALSE
)

diccionario <- data.frame(
  variable=c("matricula_id", "estudiante_pseudo", "carrera_id", "ciclo",
             "periodo", "nota_parcial", "actividades_entre_evaluaciones",
             "minutos_entre_evaluaciones", "tareas_entre_evaluaciones",
             "aprobado_final", "particion"),
  descripcion=c("Clave de matricula sintetica para trazabilidad",
                "Seudonimo para dividir por estudiante sin identidad directa",
                "Codigo de carrera (categoria)",
                "Ciclo de la asignatura matriculada",
                "Semestre de matricula (categoria)",
                "Calificacion parcial, escala 0-20",
                "Numero de eventos posteriores al parcial y previos al final",
                "Duracion total en minutos en la misma ventana temporal",
                "Entregas registradas en la misma ventana temporal",
                "Objetivo: 1 si nota final >= 11, 0 en otro caso",
                "Entrenamiento o prueba; particion por estudiante"),
  uso=c("trazabilidad", "particion_no_predictora", "predictora",
        "predictora", "predictora", "predictora", "predictora",
        "predictora", "predictora", "objetivo", "control_no_predictor"),
  stringsAsFactors=FALSE
)

integridad <- data.frame(
  archivo=basename(rutas), md5_antes=md5_antes,
  md5_despues=unname(tools::md5sum(rutas)),
  intacto=md5_antes==unname(tools::md5sum(rutas)),
  stringsAsFactors=FALSE
)
validar(all(integridad$intacto), "Fallaron las huellas de integridad MD5.")

# PASO 10. EXPORTAR Y VALIDAR LOS ARCHIVOS DE SALIDA -------------------------
cat("PASO 10 - Guardando archivos analiticos y verificando su lectura...\n")
dir.create(ruta_salida, showWarnings=FALSE, recursive=TRUE)
dir.create(ruta_reportes, showWarnings=FALSE, recursive=TRUE)
exportar <- function(x, directorio, archivo) {
  write.csv(x, file.path(directorio,archivo), row.names=FALSE,
            fileEncoding="UTF-8", na="")
}
exportar(dim_carreras,ruta_salida,"dim_carreras.csv")
exportar(dim_asignaturas,ruta_salida,"dim_asignaturas.csv")
exportar(fact,ruta_salida,"fact_rendimiento_matriculas.csv")
exportar(ml,ruta_salida,"dataset_ml_clasificacion.csv")
exportar(clust,ruta_salida,"dataset_ml_agrupamiento.csv")
exportar(resumen,ruta_reportes,"resumen_base_analitica.csv")
exportar(diccionario,ruta_reportes,"diccionario_variables.csv")
exportar(integridad,ruta_reportes,"integridad_entradas.csv")

esperados <- c(dim_carreras=nrow(dim_carreras),
               dim_asignaturas=nrow(dim_asignaturas),
               fact_rendimiento_matriculas=nrow(fact),
               dataset_ml_clasificacion=nrow(ml),
               dataset_ml_agrupamiento=nrow(clust))
leidos <- vapply(names(esperados), function(nombre) {
  nrow(leer(file.path(ruta_salida,paste0(nombre,".csv"))))
}, integer(1))
validar(all(leidos==esperados),
        "Los CSV de salida no conservan sus recuentos de filas.")
validar(identical(md5_antes,unname(tools::md5sum(rutas))),
        "Los archivos del ETL fueron modificados inesperadamente.")
cat("\n=====================================================\n")
cat("UNIVERSIDAD NOVA - RESUMEN DE BASE ANALITICA 3.9\n")
cat("=====================================================\n")
print(resumen, row.names=FALSE)
cat("\nClases de aprobacion final por particion:\n")
print(table(ml$particion,ml$aprobado_final))
cat("\nFuentes depuradas intactas:", all(integridad$intacto), "\n")
cat("Archivos analiticos:",normalizePath(ruta_salida),"\n")
cat("Informes:",normalizePath(ruta_reportes),"\n")
cat("\nPROCESO 3.9 COMPLETADO CORRECTAMENTE.\n")
cat("NOTA: datos sinteticos; no se ha entrenado ningun modelo ML aun.\n")

