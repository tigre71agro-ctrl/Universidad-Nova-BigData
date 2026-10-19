# ===========================================================================
# UNIVERSIDAD NOVA | APARTADO 3.10 | MODELO DE CLASIFICACION CON SPARKLYR
# MODELO 1: REGRESION LOGISTICA BINARIA, SPARK LOCAL EN WINDOWS
# Datos de entrada: base_analitica/dataset_ml_clasificacion.csv (apartado 3.9)
# Salidas: resultados_ml/ (NO modifica los archivos de entrada)
# Ejecutar de una sola vez con source(file.choose(), echo=FALSE)
# Este script NO demuestra todavia conexion al cluster Spark de Docker.
# ===========================================================================

cat("\n*** UNIVERSIDAD NOVA - MODELO 1 CON SPARKLYR ***\n")

# PASO 1. Dependencias ------------------------------------------------------
if (!requireNamespace("sparklyr", quietly=TRUE)) {
  stop("Instale sparklyr antes de continuar.", call.=FALSE)
}
if (!requireNamespace("dplyr", quietly=TRUE)) {
  stop("Instale dplyr antes de continuar: install.packages('dplyr')", call.=FALSE)
}
library(sparklyr)
validar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call.=FALSE)
}
proporcion <- function(n, d) if (d == 0L) NA_real_ else n / d

# PASO 2. Localizar CSV generado en el apartado 3.9 -------------------------
ruta <- "C:/Users/tigre/Downloads/base_analitica/dataset_ml_clasificacion.csv"
validar(file.exists(ruta), paste("No se encontro:", ruta))
huella_original <- unname(tools::md5sum(ruta))
datos <- read.csv(ruta, stringsAsFactors=FALSE, fileEncoding="UTF-8")
columnas <- c("matricula_id", "estudiante_pseudo", "carrera_id", "ciclo",
              "periodo", "nota_parcial", "actividades_entre_evaluaciones",
              "minutos_entre_evaluaciones", "tareas_entre_evaluaciones",
              "aprobado_final", "particion")
validar(all(columnas %in% names(datos)), paste(
  "Columnas faltantes:", paste(setdiff(columnas, names(datos)), collapse=", ")))
validar(nrow(datos) == 28985L,
        paste("Se esperaban 28985 matriculas; se encontraron", nrow(datos)))
validar(!anyNA(datos[,columnas]) && !anyDuplicated(datos$matricula_id),
        "Se detectaron valores faltantes o matriculas duplicadas.")
validar(all(datos$aprobado_final %in% c(0,1)) &&
          all(datos$particion %in% c("entrenamiento", "prueba")),
        "Los valores objetivo o la particion son incorrectos.")
validar(all(datos$nota_parcial >= 0 & datos$nota_parcial <= 20),
        "La nota parcial esta fuera de la escala de 0 a 20.")
cat("Datos originales importados:", nrow(datos), "filas.\n")

# PASO 3. Respetar las particiones por estudiante ---------------------------
entrena <- datos[datos$particion=="entrenamiento", ,drop=FALSE]
prueba  <- datos[datos$particion=="prueba", ,drop=FALSE]
validar(nrow(entrena)==23192L && nrow(prueba)==5793L,
        "El reparto no coincide con el apartado 3.9.")
validar(length(intersect(entrena$estudiante_pseudo,
                         prueba$estudiante_pseudo)) == 0L,
        "FUGA DE INFORMACION: un estudiante aparece en ambas particiones.")
validar(length(unique(entrena$aprobado_final))==2L &&
          length(unique(prueba$aprobado_final))==2L,
        "Una particion carece de una clase.")
cat("Entrenamiento:", nrow(entrena), " | Prueba:", nrow(prueba), "\n")
cat("Distribucion objetivo:\n")
print(table(datos$particion, datos$aprobado_final))

# PASO 4. Seleccionar exclusivamente variables previas al examen final -----
# NO utilizar: nota_final, fechas posteriores, IDs, estudiante_pseudo,
# particion, ni variable objetivo como predictores.
predictores <- c("nota_parcial", "ciclo",
                 "actividades_entre_evaluaciones",
                 "minutos_entre_evaluaciones", "tareas_entre_evaluaciones")
para_spark <- c("matricula_id", "aprobado_final", predictores)
for (campo in c("aprobado_final", predictores)) {
  entrena[[campo]] <- as.numeric(entrena[[campo]])
  prueba[[campo]] <- as.numeric(prueba[[campo]])
}
validar(all(vapply(entrena[,c("aprobado_final",predictores),drop=FALSE],
                     function(x) all(is.finite(x)), logical(1L))) &&
          all(vapply(prueba[,c("aprobado_final",predictores),drop=FALSE],
                     function(x) all(is.finite(x)), logical(1L))),
        "Los predictores contienen valores no numericos o no finitos.")

# PASO 5. Conectar SparklyR (local, dos nucleos) -----------------------------
ejecutar_modelo <- function() {
  cat("\nConectando Apache Spark 3.5.7 en modo local[2]...\n")
  sc <- sparklyr::spark_connect(
    master="local[2]", version="3.5.7", hadoop_version="3",
    app_name="Universidad_Nova_Clasificacion"
  )
  on.exit(try(sparklyr::spark_disconnect(sc), silent=TRUE), add=TRUE)
  validar(as.character(sparklyr::spark_version(sc))=="3.5.7",
          "No se conecto a Spark 3.5.7.")

  # PASO 6. Transferir por separado entrenamiento y prueba ------------------
  tabla_entrena <- sparklyr::sdf_copy_to(
    sc, entrena[,para_spark,drop=FALSE],
    name="nova_entrenamiento", overwrite=TRUE
  )
  tabla_prueba <- sparklyr::sdf_copy_to(
    sc, prueba[,para_spark,drop=FALSE],
    name="nova_prueba", overwrite=TRUE
  )
  validar(sparklyr::sdf_nrow(tabla_entrena)==23192L &&
            sparklyr::sdf_nrow(tabla_prueba)==5793L,
          "Las cantidades copiadas a Spark no coinciden.")
  cat("Transferencia Spark completada.\n")

  # PASO 7. Entrenar regresion logistica binaria en Spark -------------------
  cat("Entrenando regresion logistica binaria...\n")
  modelo <- sparklyr::ml_logistic_regression(
    tabla_entrena,
    aprobado_final ~ nota_parcial + ciclo +
      actividades_entre_evaluaciones + minutos_entre_evaluaciones +
      tareas_entre_evaluaciones,
    family="binomial", max_iter=100L
  )

  # PASO 8. Evaluar UNICAMENTE con datos de prueba no vistos --------------
  pred_spark <- sparklyr::ml_predict(modelo, tabla_prueba)
  columnas_pred <- colnames(pred_spark)
  validar(all(c("prediction", "rawPrediction") %in% columnas_pred),
          "La prediccion no contiene las columnas esperadas.")
  columna_etiqueta <- if ("label" %in% columnas_pred) "label" else "aprobado_final"
  auc <- sparklyr::ml_binary_classification_evaluator(
    pred_spark, label_col=columna_etiqueta,
    raw_prediction_col="rawPrediction", metric_name="areaUnderROC"
  )
  pred <- dplyr::collect(dplyr::select(
    pred_spark, dplyr::all_of(c("matricula_id", "aprobado_final", "prediction"))
  ))
  names(pred) <- c("matricula_id", "real", "predicho")
  pred$real <- as.integer(pred$real)
  pred$predicho <- as.integer(pred$predicho)
  validar(nrow(pred)==nrow(prueba) && !anyNA(pred) &&
            all(pred$predicho %in% c(0L,1L)),
          "Las predicciones estan incompletas o tienen valores invalidos.")

  tp <- sum(pred$real==1L & pred$predicho==1L)
  tn <- sum(pred$real==0L & pred$predicho==0L)
  fp <- sum(pred$real==0L & pred$predicho==1L)
  fn <- sum(pred$real==1L & pred$predicho==0L)
  sensibilidad <- proporcion(tp,tp+fn)
  especificidad <- proporcion(tn,tn+fp)
  precision <- proporcion(tp,tp+fp)
  f1 <- if (is.na(precision) || is.na(sensibilidad) ||
            precision+sensibilidad == 0) NA_real_ else
    2*precision*sensibilidad/(precision+sensibilidad)
  exactitud <- (tp+tn)/nrow(pred)
  balanceada <- mean(c(sensibilidad,especificidad))
  clase_base <- as.integer(names(which.max(table(entrena$aprobado_final))))
  exactitud_base <- mean(prueba$aprobado_final==clase_base)

  confusion <- as.data.frame(table(
    real=factor(pred$real,levels=0:1),
    predicho=factor(pred$predicho,levels=0:1)
  ))
  names(confusion)[3] <- "n"
  metricas <- data.frame(
    indicador=c("accuracy_prueba", "accuracy_baseline_mayoritaria",
                "precision_aprobado_1", "sensibilidad_aprobado_1",
                "especificidad_no_aprobado_0", "f1_aprobado_1",
                "balanced_accuracy", "auc_roc_prueba"),
    valor=c(exactitud,exactitud_base,precision,sensibilidad,
            especificidad,f1,balanceada,as.numeric(auc))
  )
  validar(all(is.finite(metricas$valor[!is.na(metricas$valor)])),
          "El modelo produjo indicadores no finitos.")

  # PASO 9. Exportar resultados, no datos originales ----------------------
  carpeta <- "C:/Users/tigre/Downloads/resultados_ml"
  dir.create(carpeta, showWarnings=FALSE, recursive=TRUE)
  write.csv(metricas, file.path(carpeta,"metricas_clasificacion.csv"),
            row.names=FALSE)
  write.csv(confusion, file.path(carpeta,"matriz_confusion.csv"),
            row.names=FALSE)
  write.csv(pred, file.path(carpeta,"predicciones_prueba.csv"),
            row.names=FALSE)
  validar(identical(huella_original,unname(tools::md5sum(ruta))),
          "El CSV fuente cambio durante el modelamiento.")

  # PASO 10. Mostrar resultados reales ------------------------------------
  cat("\n===== UNIVERSIDAD NOVA | CLASIFICACION SPARKLYR =====\n")
  cat("Motor: Spark local[2] (NO cluster Docker)\n")
  cat("Entrenamiento:",nrow(entrena),"\n")
  cat("Prueba:",nrow(prueba),"\n")
  cat("\nMATRIZ DE CONFUSION\n")
  print(confusion,row.names=FALSE)
  cat("\nMETRICAS REALES DE PRUEBA\n")
  print(metricas,row.names=FALSE, digits=4)
  cat("\nResultados guardados en:",carpeta,"\n")
  cat("ARCHIVO DE ENTRADA INTACTO: TRUE\n")
  cat("MODELO DE CLASIFICACION COMPLETADO CORRECTAMENTE.\n")
  invisible(metricas)
}

resultado_modelo <- ejecutar_modelo()
