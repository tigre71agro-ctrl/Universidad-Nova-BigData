# ================================================================
# UNIVERSIDAD NOVA | APARTADO 3.13
# CONEXION SPARKLYR EN WINDOWS -> CLUSTER DOCKER (VERSION CORREGIDA)
# Motor Spark 3.5.7 + Hadoop 3 + SparklyR 1.9.6
# Esta prueba no modifica ningun archivo del proyecto ni contenedor.
# ================================================================

cat("\nUNIVERSIDAD NOVA - PRUEBA SPARKLYR CONTRA DOCKER\n")
cat("VERSION CORREGIDA: SPARK_HOME EXPLICITO\n")

# PASO 1. Dependencias ya instaladas
if (!requireNamespace("sparklyr", quietly = TRUE)) {
  stop("No se encontro sparklyr. Consulte la instalacion anterior.")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("No se encontro dplyr. Instale con install.packages('dplyr').")
}

# PASO 2. Resolver la ubicacion REAL de Spark en Windows.
# No se debe usar /opt/spark porque esa es la ruta dentro de Docker.
spark_home <- sparklyr::spark_home_dir(
  version = "3.5.7", hadoop_version = "3"
)
if (is.null(spark_home) || length(spark_home) != 1L ||
    !nzchar(spark_home) || !dir.exists(spark_home)) {
  stop("No se encontro Spark 3.5.7/Hadoop 3 en Windows. ",
       "Revise spark_installed_versions(); no reinstale nada aun.")
}
spark_home <- normalizePath(spark_home, winslash = "/", mustWork = TRUE)
spark_submit <- file.path(spark_home, "bin", "spark-submit.cmd")
if (!file.exists(spark_submit)) {
  stop("No se encontro spark-submit.cmd en: ",
       file.path(spark_home, "bin"),
       "\nConfirme si existe ese archivo en el Explorador de Windows.")
}
Sys.setenv(SPARK_HOME = spark_home)
cat("PASO 1. Spark instalado en:\n", spark_home, "\n", sep = "")
cat("Archivo spark-submit.cmd: ENCONTRADO\n")

# PASO 3. Verificar disponibilidad TCP del Master.
puerto_disponible <- tryCatch({
  con <- socketConnection(host = "127.0.0.1", port = 7077,
                          open = "r+", blocking = TRUE, timeout = 5)
  close(con)
  TRUE
}, error = function(e) {
  cat("Error de puerto 7077: ", conditionMessage(e), "\n", sep = "")
  FALSE
})
if (!puerto_disponible) {
  stop("Spark Master no es accesible. Compruebe Docker y localhost:8080.")
}
cat("PASO 2. Puerto 7077: DISPONIBLE (prueba TCP solamente)\n")

# PASO 4. Red bidireccional: ejecutor Docker -> driver Windows.
# Los nombres y puertos se comprobaran durante la conexion real.
cfg <- sparklyr::spark_config()
cfg[["spark.driver.host"]] <- "host.docker.internal"
cfg[["spark.driver.bindAddress"]] <- "0.0.0.0"
cfg[["spark.driver.port"]] <- 37077L
cfg[["spark.blockManager.port"]] <- 37078L
cfg[["spark.cores.max"]] <- 2L
cfg[["spark.executor.cores"]] <- 1L
cfg[["spark.executor.memory"]] <- "1g"
cfg[["spark.sql.shuffle.partitions"]] <- 2L
cfg[["spark.ui.enabled"]] <- FALSE

# PASO 5. Conectar al CLUSTER y ejecutar una accion (no usar local[2]).
sc <- NULL
conexion_ok <- FALSE
motivo <- NULL

tryCatch({
  cat("PASO 3. Iniciando SparklyR contra el Master de Docker...\n")
  sc <- sparklyr::spark_connect(
    master = "spark://127.0.0.1:7077",
    spark_home = spark_home,        # CORRECCION PRINCIPAL
    version = "3.5.7",
    app_name = "Universidad_Nova_SparklyR_Docker",
    method = "shell",
    config = cfg
  )
  cat("Spark conectado. Version: ", as.character(sparklyr::spark_version(sc)),
      "\n", sep = "")

  cat("PASO 4. Cargando 10000 registros sinteticos de prueba...\n")
  datos_prueba <- data.frame(
    valor = seq_len(10000L),
    grupo = rep(c("A", "B"), length.out = 10000L)
  )
  tbl <- sparklyr::sdf_copy_to(
    sc, datos_prueba, name = "nova_verificacion_cluster", overwrite = TRUE
  )
  resultado <- dplyr::collect(
    dplyr::summarise(tbl,
                     registros = dplyr::n(),
                     suma_valores = sum(valor))
  )
  print(resultado)

  stopifnot(nrow(resultado) == 1L,
            as.numeric(resultado$registros[1]) == 10000,
            as.numeric(resultado$suma_valores[1]) == 50005000)
  conexion_ok <- TRUE
}, error = function(e) {
  motivo <<- conditionMessage(e)
}, finally = {
  if (!is.null(sc)) {
    try(sparklyr::spark_disconnect(sc), silent = TRUE)
  }
})

# PASO 6. Resultado honesto: no declarar exito si no completo la consulta.
if (conexion_ok) {
  cat("\nPRUEBA SPARKLYR-DOCKER COMPLETADA CORRECTAMENTE.\n")
  cat("Registros: 10000 | Suma: 50005000\n")
  cat("Revise la aplicacion en http://localhost:8080\n")
} else {
  cat("\nPRUEBA SPARKLYR-DOCKER TODAVIA PENDIENTE.\n")
  cat("Motivo: ", motivo, "\n", sep = "")
  cat("NO modifique los CSV ni reinstale Spark o Docker.\n")
  cat("Envie una captura de este mensaje para diagnosticar el siguiente paso.\n")
}
cat("Estado final: ", if (conexion_ok) "EXITO" else "PENDIENTE", "\n", sep = "")
