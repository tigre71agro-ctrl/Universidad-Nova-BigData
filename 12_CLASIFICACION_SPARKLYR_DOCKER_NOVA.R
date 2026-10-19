# =====================================================================
# UNIVERSIDAD NOVA - APARTADO 3.13
# REGRESION LOGISTICA EN CLUSTER SPARK DOCKER DESDE RSTUDIO WINDOWS
# Basado en scripts 07 (clasificacion local) y 11 (conexion comprobada).
# Datos: base_analitica/dataset_ml_clasificacion.csv
# Salida nueva: resultados_ml_docker (NO sobrescribe resultados_ml).
# Ejecutar entero: source(file.choose(), echo=FALSE, encoding="UTF-8")
# =====================================================================

cat("\n*** UNIVERSIDAD NOVA - CLASIFICACION EN CLUSTER DOCKER ***\n")

# PASO 1. Dependencias y verificaciones generales ----------------------
for (paquete in c("sparklyr", "dplyr")) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    stop("Falta el paquete: ", paquete, ". No se modificaron archivos.",
         call. = FALSE)
  }
}
validar <- function(cond, texto) {
  if (!isTRUE(cond)) stop(texto, call. = FALSE)
}
proporcion <- function(a, b) if (b == 0) NA_real_ else a / b

# PASO 2. Resolver Spark HOME en Windows (correccion ya probada) --------
spark_home <- sparklyr::spark_home_dir(
  version = "3.5.7", hadoop_version = "3"
)
validar(length(spark_home) == 1L && !is.na(spark_home) &&
          nzchar(spark_home) && dir.exists(spark_home),
        "No se localiza Spark 3.5.7 en Windows.")
spark_home <- normalizePath(spark_home, winslash = "/", mustWork = TRUE)
validar(file.exists(file.path(spark_home, "bin", "spark-submit.cmd")),
        "No existe spark-submit.cmd en la carpeta Spark detectada.")
Sys.setenv(SPARK_HOME = spark_home)
cat("Spark HOME:", spark_home, "\n")

# PASO 3. Localizar el CSV y mantener huella MD5 -------------------------
descargas <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
validar(dir.exists(descargas), "No se encontro la carpeta Descargas.")
ruta_csv <- file.path(
  descargas, "base_analitica", "dataset_ml_clasificacion.csv"
)
validar(file.exists(ruta_csv), paste("Falta el CSV:", ruta_csv))
md5_antes <- unname(tools::md5sum(ruta_csv))
datos <- read.csv(ruta_csv, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8")
columnas_obligatorias <- c(
  "matricula_id", "estudiante_pseudo", "carrera_id", "ciclo",
  "periodo", "nota_parcial", "actividades_entre_evaluaciones",
  "minutos_entre_evaluaciones", "tareas_entre_evaluaciones",
  "aprobado_final", "particion"
)
validar(all(columnas_obligatorias %in% names(datos)),
        paste("Columnas faltantes:", paste(
          setdiff(columnas_obligatorias, names(datos)), collapse = ", ")))
validar(nrow(datos) == 28985L, "Numero inesperado de filas en dataset.")
validar(!anyNA(datos[, columnas_obligatorias, drop = FALSE]),
        "Hay faltantes en campos obligatorios del CSV analitico.")
validar(!anyDuplicated(datos$matricula_id),
        "Se detectaron IDs de matricula repetidos.")
validar(all(datos$aprobado_final %in% c(0, 1)),
        "La clase final debe tomar valores 0 y 1.")
validar(all(datos$particion %in% c("entrenamiento", "prueba")),
        "Existen particiones desconocidas.")

# PASO 4. Respetar la particion original por estudiante ----------------
entrenamiento <- datos[datos$particion == "entrenamiento", , drop = FALSE]
prueba <- datos[datos$particion == "prueba", , drop = FALSE]
validar(nrow(entrenamiento) == 23192L && nrow(prueba) == 5793L,
        "Los tamanos de las particiones no coinciden con el apartado 3.9.")
validar(length(intersect(
  entrenamiento$estudiante_pseudo, prueba$estudiante_pseudo
)) == 0L, "Fuga: existe un estudiante en ambas particiones.")
validar(length(unique(entrenamiento$aprobado_final)) == 2L &&
          length(unique(prueba$aprobado_final)) == 2L,
        "Falta una clase en alguna particion.")
predictores <- c(
  "nota_parcial", "ciclo", "actividades_entre_evaluaciones",
  "minutos_entre_evaluaciones", "tareas_entre_evaluaciones"
)
for (nm in c("aprobado_final", predictores)) {
  entrenamiento[[nm]] <- as.numeric(entrenamiento[[nm]])
  prueba[[nm]] <- as.numeric(prueba[[nm]])
}
validar(all(vapply(entrenamiento[,c("aprobado_final",predictores),drop=FALSE],
                    function(v) all(is.finite(v)), logical(1))) &&
          all(vapply(prueba[,c("aprobado_final",predictores),drop=FALSE],
                     function(v) all(is.finite(v)), logical(1))),
        "El conjunto contiene predictores no numericos o no finitos.")
para_spark <- c("matricula_id", "aprobado_final", predictores)
cat("CSV validado:", nrow(datos), "observaciones.\n")
cat("Entrenamiento:", nrow(entrenamiento),
    "| Prueba:", nrow(prueba), "\n")

# PASO 5. Comprobar puerto Spark Master ---------------------------------
tcp_ok <- tryCatch({
  con <- socketConnection("127.0.0.1", port=7077,
                          open="r+", timeout=5, blocking=TRUE)
  close(con)
  TRUE
}, error=function(e) {
  cat("No se puede acceder al puerto 7077:", conditionMessage(e), "\n")
  FALSE
})
validar(tcp_ok, "Docker Spark Master no accesible; compruebe localhost:8080.")

# PASO 6. Configurar driver Windows y worker Docker ----------------------
config <- sparklyr::spark_config()
config[["spark.driver.host"]] <- "host.docker.internal"
config[["spark.driver.bindAddress"]] <- "0.0.0.0"
config[["spark.driver.port"]] <- 37077L
config[["spark.blockManager.port"]] <- 37078L
config[["spark.cores.max"]] <- 2L
config[["spark.executor.cores"]] <- 1L
config[["spark.executor.memory"]] <- "1g"
config[["spark.sql.shuffle.partitions"]] <- 2L
config[["spark.ui.enabled"]] <- FALSE

# PASO 7. Conectar, entrenar y predecir en el cluster Docker ------------
modelo_en_docker <- function() {
  cat("\nConectando SparklyR a spark://127.0.0.1:7077...\n")
  sc <- sparklyr::spark_connect(
    master = "spark://127.0.0.1:7077",
    spark_home = spark_home,
    version = "3.5.7",
    app_name = "Universidad_Nova_Regresion_Logistica_Docker",
    method = "shell",
    config = config
  )
  on.exit(try(sparklyr::spark_disconnect(sc), silent = TRUE), add = TRUE)
  validar(as.character(sparklyr::spark_version(sc)) == "3.5.7",
          "La conexion no corresponde a Spark 3.5.7.")

  cat("Cargando datos de entrenamiento y prueba al cluster...\n")
  tabla_train <- sparklyr::sdf_copy_to(
    sc, entrenamiento[,para_spark,drop=FALSE],
    name = "nova_docker_entrenamiento", overwrite = TRUE
  )
  tabla_test <- sparklyr::sdf_copy_to(
    sc, prueba[,para_spark,drop=FALSE],
    name = "nova_docker_prueba", overwrite = TRUE
  )
  validar(sparklyr::sdf_nrow(tabla_train) == 23192L &&
            sparklyr::sdf_nrow(tabla_test) == 5793L,
          "La transferencia al cluster tiene filas faltantes.")

  cat("Entrenando regresion logistica en el cluster Docker...\n")
  modelo <- sparklyr::ml_logistic_regression(
    tabla_train,
    aprobado_final ~ nota_parcial + ciclo +
      actividades_entre_evaluaciones + minutos_entre_evaluaciones +
      tareas_entre_evaluaciones,
    family = "binomial", max_iter = 100L
  )
  pred_spark <- sparklyr::ml_predict(modelo, tabla_test)
  columnas_pred <- colnames(pred_spark)
  validar(all(c("prediction", "rawPrediction") %in% columnas_pred),
          "El modelo no produjo prediction y rawPrediction.")
  label_col <- if ("label" %in% columnas_pred) "label" else "aprobado_final"
  auc <- sparklyr::ml_binary_classification_evaluator(
    pred_spark, label_col = label_col,
    raw_prediction_col = "rawPrediction", metric_name = "areaUnderROC"
  )
  pred <- dplyr::collect(dplyr::select(
    pred_spark,
    dplyr::all_of(c("matricula_id", "aprobado_final", "prediction"))
  ))
  names(pred) <- c("matricula_id", "real", "predicho")
  pred$real <- as.integer(pred$real)
  pred$predicho <- as.integer(pred$predicho)
  validar(nrow(pred) == 5793L && !anyNA(pred) &&
            all(pred$real %in% 0:1) && all(pred$predicho %in% 0:1),
          "Las predicciones son invalidas o incompletas.")

  # PASO 8. Metricas de prueba y matriz de confusion --------------------
  tp <- sum(pred$real == 1L & pred$predicho == 1L)
  tn <- sum(pred$real == 0L & pred$predicho == 0L)
  fp <- sum(pred$real == 0L & pred$predicho == 1L)
  fn <- sum(pred$real == 1L & pred$predicho == 0L)
  sensibilidad <- proporcion(tp, tp + fn)
  especificidad <- proporcion(tn, tn + fp)
  precision <- proporcion(tp, tp + fp)
  f1 <- if (is.na(precision) || is.na(sensibilidad) ||
            precision + sensibilidad == 0) NA_real_ else
    2 * precision * sensibilidad / (precision + sensibilidad)
  accuracy <- (tp + tn) / nrow(pred)
  clase_base <- as.integer(names(which.max(table(entrenamiento$aprobado_final))))
  accuracy_base <- mean(prueba$aprobado_final == clase_base)
  confusion <- as.data.frame(table(
    real = factor(pred$real, levels = 0:1),
    predicho = factor(pred$predicho, levels = 0:1)
  ))
  names(confusion)[3L] <- "n"
  metricas <- data.frame(
    indicador = c("accuracy_prueba", "accuracy_baseline_mayoritaria",
                  "precision_aprobado_1", "sensibilidad_aprobado_1",
                  "especificidad_no_aprobado_0", "f1_aprobado_1",
                  "balanced_accuracy", "auc_roc_prueba"),
    valor = c(accuracy, accuracy_base, precision, sensibilidad,
              especificidad, f1, mean(c(sensibilidad, especificidad)),
              as.numeric(auc)), stringsAsFactors = FALSE
  )
  validar(all(is.finite(metricas$valor[!is.na(metricas$valor)])),
          "Hay metricas no finitas.")
  validar(identical(md5_antes, unname(tools::md5sum(ruta_csv))),
          "El archivo fuente cambio durante el entrenamiento.")

  # PASO 9. Resultados en carpeta NUEVA, sin modificar resultados_ml ---
  salida <- file.path(descargas, "resultados_ml_docker")
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  write.csv(metricas, file.path(salida, "metricas_clasificacion_docker.csv"),
            row.names = FALSE)
  write.csv(confusion, file.path(salida, "matriz_confusion_docker.csv"),
            row.names = FALSE)
  write.csv(pred, file.path(salida, "predicciones_prueba_docker.csv"),
            row.names = FALSE)
  detalles <- data.frame(
    nombre = c("motor", "servidor_master", "version_spark", "modelo",
               "entrenamiento", "prueba", "archivo_md5_intacto"),
    valor = c("SparklyR - Docker", "spark://127.0.0.1:7077", "3.5.7",
              "regresion_logistica_binaria", "23192", "5793", "TRUE")
  )
  write.csv(detalles, file.path(salida, "ejecucion_docker.csv"),
            row.names = FALSE)

  local_csv <- file.path(descargas, "resultados_ml", "metricas_clasificacion.csv")
  if (file.exists(local_csv)) {
    local <- read.csv(local_csv, stringsAsFactors = FALSE)
    if (all(c("indicador", "valor") %in% names(local))) {
      comparacion <- merge(
        local[,c("indicador","valor")], metricas,
        by = "indicador", all = TRUE,
        suffixes = c("_local", "_docker")
      )
      comparacion$diferencia_docker_menos_local <-
        comparacion$valor_docker - comparacion$valor_local
      write.csv(comparacion,
                file.path(salida, "comparacion_local_vs_docker.csv"),
                row.names = FALSE)
    }
  }

  # PASO 10. Resumen trazable -------------------------------------------
  cat("\n===== REGRESION LOGISTICA - SPARK CLUSTER DOCKER =====\n")
  cat("Entrenamiento:", nrow(entrenamiento), "| Prueba:", nrow(prueba), "\n")
  cat("\nMATRIZ DE CONFUSION\n")
  print(confusion, row.names = FALSE)
  cat("\nMETRICAS EN DATOS DE PRUEBA\n")
  print(metricas, row.names = FALSE, digits = 5)
  cat("\nDatos de entrada intactos: TRUE\n")
  cat("Resultados Docker:", normalizePath(salida, winslash = "/"), "\n")
  cat("Verifique la aplicacion en http://localhost:8080\n")
  cat("MODELO DE CLASIFICACION EN DOCKER COMPLETADO CORRECTAMENTE.\n")
  invisible(metricas)
}

resultado_modelo_docker <- modelo_en_docker()
