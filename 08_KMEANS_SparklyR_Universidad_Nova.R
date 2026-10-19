# =============================================================================
# UNIVERSIDAD NOVA | PROYECTO BIG DATA | APARTADO 3.11
# AGRUPAMIENTO K-MEANS CON SPARKLYR (Spark 3.5.7, modo local[2])
# =============================================================================
# ENTRADA:  Downloads/base_analitica/dataset_ml_agrupamiento.csv
#           (producido por el script 05; 14 727 estudiantes simulados)
# SALIDAS:  Downloads/resultados_agrupamiento/
#           Archivos CSV, cuatro graficos PNG y reporte metodologico TXT.
# NO altera el CSV de entrada; verifica su huella MD5 al terminar.
# NO utiliza calificacion final, aprobacion, nombres ni datos personales.
# NOTA: Spark local[2] NO acredita ejecucion distribuida en Docker.
# EJECUTAR TODO JUNTO desde la consola de RStudio:
# source(file.choose(), echo=FALSE, encoding="UTF-8")
# Paquetes necesarios: sparklyr, dplyr (ya empleados en apartado 3.10).
# =============================================================================

cat("\n*** UNIVERSIDAD NOVA - K-MEANS CON SPARKLYR - APARTADO 3.11 ***\n")

# PASO 1. DEPENDENCIAS Y FUNCIONES DE SEGURIDAD -----------------------------
for (paquete in c("sparklyr", "dplyr")) {
  if (!requireNamespace(paquete, quietly=TRUE)) {
    stop(paste("Falta el paquete", paquete,
               "- instale con install.packages('", paquete, "')", sep=""),
         call.=FALSE)
  }
}
validar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call.=FALSE)
}
set.seed(2026)

# PASO 2. LEER LOS DATOS REALES DEL APARTADO 3.9 ----------------------------
raiz <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
ruta_entrada <- file.path(raiz, "base_analitica",
                          "dataset_ml_agrupamiento.csv")
ruta_salida <- file.path(raiz, "resultados_agrupamiento")
validar(file.exists(ruta_entrada), paste(
  "No se encontro el CSV:", ruta_entrada,
  "\nVerifique que exista base_analitica dentro de Descargas."))
md5_antes <- unname(tools::md5sum(ruta_entrada))

datos <- read.csv(ruta_entrada, stringsAsFactors=FALSE,
                  fileEncoding="UTF-8", na.strings=c("", "NA"))
columnas <- c("estudiante_pseudo", "promedio_nota_parcial",
              "promedio_actividades", "promedio_minutos",
              "promedio_tareas", "matriculas_evaluables")
validar(all(columnas %in% names(datos)), paste(
  "Faltan columnas:", paste(setdiff(columnas,names(datos)),collapse=", ")))
validar(nrow(datos)==14727L, paste(
  "Se esperaban 14727 perfiles del apartado 3.9; se encontraron",nrow(datos)))
validar(!anyNA(datos[,columnas,drop=FALSE]),
        "El CSV presenta valores faltantes en campos requeridos.")
validar(!anyDuplicated(datos$estudiante_pseudo),
        "Existen identificadores seudonimos duplicados.")
validar(all(nzchar(datos$estudiante_pseudo)),
        "Hay seudonimos vacios.")

variables <- c("promedio_nota_parcial", "promedio_actividades",
               "promedio_minutos", "promedio_tareas")
for (variable in c(variables,"matriculas_evaluables")) {
  validar(is.numeric(datos[[variable]]), paste(
    "La variable", variable, "no es numerica."))
  validar(all(is.finite(datos[[variable]])), paste(
    "La variable", variable, "tiene valores no finitos."))
}
validar(all(datos$promedio_nota_parcial >= 0 &
            datos$promedio_nota_parcial <= 20),
        "La nota parcial promedio esta fuera del rango 0 a 20.")
validar(all(datos$promedio_actividades >= 0 &
            datos$promedio_minutos >= 0 &
            datos$promedio_tareas >= 0),
        "Existen promedios de actividad negativos.")
validar(all(datos$promedio_tareas <= datos$promedio_actividades + 1e-8),
        "El promedio de tareas supera el de actividades.")
validar(all(datos$matriculas_evaluables >= 1 &
            datos$matriculas_evaluables ==
              floor(datos$matriculas_evaluables)),
        "Hay cantidades invalidas de matriculas evaluables.")
validar(!any(c("nota_final", "aprobado_final", "nombre",
               "documento_simulado") %in% variables),
        "La seleccion incluye una variable prohibida.")
cat("PASO 2 - Perfiles importados:", nrow(datos), "\n")

# PASO 3. ESTANDARIZAR SIN PERDER LA ESCALA ORIGINAL -------------------------
# K-means utiliza distancia euclidiana. Conservar escala en los informes,
# pero trabajar con puntuaciones Z evita que el tiempo en minutos domine.
matriz_original <- as.matrix(datos[,variables,drop=FALSE])
medias <- colMeans(matriz_original)
desviaciones <- apply(matriz_original, 2, sd)
activas <- variables[is.finite(desviaciones) & desviaciones > 0]
validar(length(activas)>=2L,
        "No hay al menos dos variables con varianza positiva para agrupar.")
inactivas <- setdiff(variables, activas)
if (length(inactivas)>0) cat(
  "Variables sin variacion excluidas:",paste(inactivas,collapse=", "),"\n")
matriz_z <- scale(matriz_original[,activas,drop=FALSE],
                  center=medias[activas], scale=desviaciones[activas])
validar(all(is.finite(matriz_z)), "La estandarizacion produjo datos invalidos.")
colnames(matriz_z) <- paste0("z_",activas)

datos_spark <- data.frame(
  estudiante_pseudo=datos$estudiante_pseudo,
  matriz_z, check.names=FALSE, stringsAsFactors=FALSE
)
col_z <- colnames(matriz_z)
parametros <- data.frame(variable=variables,
                         media=unname(medias),
                         desviacion_estandar=unname(desviaciones),
                         usada_kmeans=variables %in% activas,
                         stringsAsFactors=FALSE)
cat("PASO 3 - Variables estandarizadas:",paste(activas,collapse=", "),"\n")

# PASO 4. CONECTAR A SPARK Y ENSAMBLAR LAS CARACTERISTICAS -------------------
# Esta sesion es Spark LOCAL en Windows. Docker sigue independiente.
analizar_con_spark <- function() {
  cat("PASO 4 - Conectando Spark local[2] version 3.5.7...\n")
  sc <- sparklyr::spark_connect(
    master="local[2]", version="3.5.7", hadoop_version="3",
    app_name="Universidad_Nova_KMeans"
  )
  on.exit(try(sparklyr::spark_disconnect(sc),silent=TRUE),add=TRUE)
  validar(as.character(sparklyr::spark_version(sc))=="3.5.7",
          "La conexion no corresponde a Spark 3.5.7.")

  tabla <- sparklyr::sdf_copy_to(
    sc, datos_spark, name="nova_perfiles_kmeans", overwrite=TRUE
  )
  validar(sparklyr::sdf_nrow(tabla)==nrow(datos),
          "La transferencia de perfiles a Spark esta incompleta.")

  tabla_features <- sparklyr::ft_vector_assembler(
    tabla, input_cols=col_z, output_col="features"
  )
  validar("features" %in% colnames(tabla_features),
          "No se pudo formar la columna vectorial 'features'.")
  cat("PASO 4 - Datos y vector de caracteristicas creados en Spark.\n")

  # PASO 5. COMPARAR K DE 2 A 5 MEDIANTE SILUETA ---------------------------
  # La silueta de Spark se calcula con distancia euclidiana CUADRADA.
  ks <- 2:5
  modelos <- vector("list",length(ks))
  names(modelos) <- as.character(ks)
  predicciones <- vector("list",length(ks))
  names(predicciones) <- as.character(ks)
  filas <- vector("list", length(ks))
  cat("PASO 5 - Evaluando K=2,3,4,5...\n")

  for (i in seq_along(ks)) {
    k <- ks[i]
    cat("  Entrenando K =",k,"...\n")
    modelo <- sparklyr::ml_kmeans(
      tabla_features, k=as.integer(k), max_iter=50L,
      seed=2026L, features_col="features", prediction_col="prediction"
    )
    pred_tbl <- sparklyr::ml_predict(modelo, tabla_features)
    validar(all(c("estudiante_pseudo","prediction","features") %in%
                  colnames(pred_tbl)),
            paste("El resultado para K=",k,"no trae las columnas requeridas."))
    silueta <- as.numeric(sparklyr::ml_clustering_evaluator(
      pred_tbl, features_col="features", prediction_col="prediction",
      metric_name="silhouette"
    ))
    validar(length(silueta)==1L && is.finite(silueta) &&
              silueta >= -1 && silueta <= 1,
            paste("La silueta no es valida para K=",k))

    pred <- dplyr::collect(dplyr::select(
      pred_tbl, estudiante_pseudo, prediction
    ))
    validar(nrow(pred)==nrow(datos) &&
              !anyDuplicated(pred$estudiante_pseudo) &&
              !anyNA(pred), paste("Predicciones incompletas para K=",k))
    indices <- match(datos$estudiante_pseudo, pred$estudiante_pseudo)
    validar(!anyNA(indices), "Algunos seudonimos no regresaron de Spark.")
    grupo <- as.integer(pred$prediction[indices]) + 1L
    validar(all(grupo %in% seq_len(k)) &&
              length(unique(grupo))==k,
            paste("K=",k,"produjo clusters vacios o etiquetas invalidas."))

    # Suma de cuadrados interna (WSS) calculada en R a partir de la
    # ASIGNACION obtenida por Spark; no sustituye el ajuste Spark MLlib.
    wss <- 0
    for (g in seq_len(k)) {
      puntos <- matriz_z[grupo==g,,drop=FALSE]
      centro <- colMeans(puntos)
      wss <- wss + sum(sweep(puntos,2L,centro,"-")^2)
    }
    filas[[i]] <- data.frame(k=k, silueta=silueta,
                             wss_estandarizada=wss,
                             clusters_observados=length(unique(grupo)))
    modelos[[as.character(k)]] <- modelo
    predicciones[[as.character(k)]] <- grupo
    cat(sprintf("    Silueta Spark = %.4f | WSS = %.2f\n",silueta,wss))
  }

  metricas_k <- do.call(rbind,filas)
  # Ante empate de silueta, elegir el menor K para simplicidad.
  indice_optimo <- order(-metricas_k$silueta,metricas_k$k)[1L]
  k_optimo <- as.integer(metricas_k$k[indice_optimo])
  grupo_optimo <- predicciones[[as.character(k_optimo)]]
  validar(length(grupo_optimo)==nrow(datos),
          "La seleccion del agrupamiento optimo quedo incompleta.")
  cat("PASO 5 - K seleccionado:",k_optimo,
      "| Silueta:",round(metricas_k$silueta[indice_optimo],4),"\n")
  list(metricas_k=metricas_k, k_optimo=k_optimo,
       grupo=grupo_optimo, version_spark=as.character(sparklyr::spark_version(sc)))
}

resultados <- analizar_con_spark()

# PASO 6. PERFILAR LOS GRUPOS EN LA ESCALA ORIGINAL --------------------------
cat("PASO 6 - Preparando perfiles y centroides interpretables...\n")
k_optimo <- resultados$k_optimo
grupo <- resultados$grupo
perfiles <- datos[,columnas,drop=FALSE]
perfiles$cluster <- as.integer(grupo)
perfiles <- perfiles[,c("estudiante_pseudo","cluster",setdiff(columnas,
                        "estudiante_pseudo")),drop=FALSE]
validar(nrow(perfiles)==14727L && !anyNA(perfiles) &&
          all(perfiles$cluster %in% seq_len(k_optimo)),
        "Los perfiles por cluster tienen valores invalidos.")

centroides <- aggregate(perfiles[,c(variables,"matriculas_evaluables"),
                                     drop=FALSE],
                        by=list(cluster=perfiles$cluster), FUN=mean)
centroides <- centroides[order(centroides$cluster),,drop=FALSE]
tamanos <- as.data.frame(table(factor(perfiles$cluster,
                                      levels=seq_len(k_optimo))))
names(tamanos) <- c("cluster", "n_estudiantes")
tamanos$cluster <- as.integer(as.character(tamanos$cluster))
tamanos$porcentaje <- round(100*tamanos$n_estudiantes/nrow(perfiles),2)
validar(sum(tamanos$n_estudiantes)==nrow(perfiles),
        "No coincide el numero total de estudiantes por cluster.")
centroides <- merge(tamanos,centroides,by="cluster",sort=TRUE)

centroides_z <- aggregate(as.data.frame(matriz_z),
                          by=list(cluster=grupo),FUN=mean)
centroides_z <- centroides_z[order(centroides_z$cluster),,drop=FALSE]

# PASO 7. COORDENADAS PCA PARA REPRESENTACION VISUAL ------------------------
# PCA es SOLO visualizacion: K-means se ajusto con todas las variables Z.
cat("PASO 7 - Calculando coordenadas PCA para graficos...\n")
acp <- prcomp(matriz_z, center=FALSE, scale.=FALSE)
validar(ncol(acp$x)>=2L,"No se pudieron obtener dos componentes PCA.")
varianza_explicada <- 100*(acp$sdev^2)/sum(acp$sdev^2)
coordenadas <- data.frame(estudiante_pseudo=datos$estudiante_pseudo,
                          cluster=grupo,
                          CP1=unname(acp$x[,1L]),
                          CP2=unname(acp$x[,2L]))
validar(all(is.finite(coordenadas$CP1)) &&
          all(is.finite(coordenadas$CP2)),
        "Las coordenadas PCA contienen valores no finitos.")

# PASO 8. VERIFICAR QUE LA FUENTE NO CAMBIO Y EXPORTAR -----------------------
cat("PASO 8 - Verificando integridad y exportando CSV...\n")
md5_despues <- unname(tools::md5sum(ruta_entrada))
validar(identical(md5_antes,md5_despues),
        "El archivo original cambio durante el analisis.")
dir.create(ruta_salida,recursive=TRUE,showWarnings=FALSE)
escribir <- function(tabla, archivo) {
  write.csv(tabla, file.path(ruta_salida,archivo), row.names=FALSE,
            fileEncoding="UTF-8")
}
escribir(resultados$metricas_k,"comparacion_k_silueta_wss.csv")
escribir(perfiles,"perfiles_estudiantiles_clusters.csv")
escribir(centroides,"resumen_centroides_clusters.csv")
escribir(centroides_z,"centroides_estandarizados.csv")
escribir(coordenadas,"coordenadas_pca_clusters.csv")
escribir(parametros,"parametros_estandarizacion.csv")
escribir(data.frame(archivo=basename(ruta_entrada),
                    md5_antes=md5_antes,md5_despues=md5_despues,
                    intacto=identical(md5_antes,md5_despues)),
         "integridad_fuente.csv")

# PASO 9. GENERAR GRAFICOS PNG PARA EL INFORME -------------------------------
cat("PASO 9 - Guardando graficos PNG...\n")
metricas_k <- resultados$metricas_k

png(file.path(ruta_salida,"figura_1_silueta_k.png"),
    width=1600,height=1000,res=150)
plot(metricas_k$k,metricas_k$silueta,type="b",pch=19,lwd=2,
     xlab="Numero de clusters (K)",ylab="Silueta Spark (distancia euclidiana cuadrada)",
     main="Universidad Nova - Seleccion de K por silueta",xaxt="n")
axis(1,at=metricas_k$k)
abline(v=k_optimo,lty=2,col="grey50")
points(k_optimo,metricas_k$silueta[metricas_k$k==k_optimo],
       pch=19,cex=1.6)
grid()
dev.off()

png(file.path(ruta_salida,"figura_2_wss_k.png"),
    width=1600,height=1000,res=150)
plot(metricas_k$k,metricas_k$wss_estandarizada,type="b",pch=19,lwd=2,
     xlab="Numero de clusters (K)",ylab="Suma de cuadrados intragrupo (WSS)",
     main="Universidad Nova - Variacion de WSS",xaxt="n")
axis(1,at=metricas_k$k)
grid()
dev.off()

png(file.path(ruta_salida,"figura_3_tamano_clusters.png"),
    width=1600,height=1000,res=150)
barplot(tamanos$n_estudiantes,
        names.arg=paste("Cluster",tamanos$cluster),
        main="Universidad Nova - Estudiantes por cluster",
        ylab="Cantidad de estudiantes",xlab="Agrupamiento K-means",
        col="grey65",border=NA)
abline(h=0)
dev.off()

# Muestra reproducible para que 14 727 puntos no oculten el patron.
set.seed(2026)
indices_grafico <- if (nrow(coordenadas)>5000L) {
  sample.int(nrow(coordenadas),5000L)
} else seq_len(nrow(coordenadas))
paleta <- grDevices::hcl.colors(k_optimo,palette="Dark 3")
png(file.path(ruta_salida,"figura_4_clusters_pca.png"),
    width=1700,height=1100,res=150)
plot(coordenadas$CP1[indices_grafico],
     coordenadas$CP2[indices_grafico],
     col=grDevices::adjustcolor(paleta[grupo[indices_grafico]],alpha.f=0.45),
     pch=16,cex=0.55,
     xlab=sprintf("CP1 (%.1f%%)",varianza_explicada[1]),
     ylab=sprintf("CP2 (%.1f%%)",varianza_explicada[2]),
     main="Universidad Nova - Agrupamiento visualizado con PCA")
legend("topright",legend=paste("Cluster",seq_len(k_optimo)),
       col=paleta,pch=16,cex=0.85,bg="white")
grid()
dev.off()

# PASO 10. REPORTE E INSPECCION FINAL ---------------------------------------
cat("PASO 10 - Comprobando los archivos y elaborando el reporte...\n")
salidas_esperadas <- c(
  "comparacion_k_silueta_wss.csv", "perfiles_estudiantiles_clusters.csv",
  "resumen_centroides_clusters.csv", "centroides_estandarizados.csv",
  "coordenadas_pca_clusters.csv", "parametros_estandarizacion.csv",
  "integridad_fuente.csv", "figura_1_silueta_k.png",
  "figura_2_wss_k.png", "figura_3_tamano_clusters.png",
  "figura_4_clusters_pca.png"
)
validar(all(file.exists(file.path(ruta_salida,salidas_esperadas))),
        "No se pudieron comprobar todos los archivos exportados.")
validar(nrow(read.csv(file.path(ruta_salida,
          "perfiles_estudiantiles_clusters.csv")))==nrow(datos),
        "El CSV final no conserva los 14 727 perfiles.")

reporte <- c(
  "UNIVERSIDAD NOVA - REPORTE TECNICO DE AGRUPAMIENTO 3.11",
  paste("Fecha de ejecucion:",format(Sys.time(),"%Y-%m-%d %H:%M:%S")),
  paste("Motor: Apache Spark",resultados$version_spark,
        "con SparklyR en modo local[2] (NO cluster Docker)"),
  paste("Cantidad de perfiles:",nrow(perfiles)),
  paste("Variables de agrupamiento:",paste(activas,collapse=", ")),
  paste("Metodo de estandarizacion: Z = (x-media)/desviacion estandar"),
  paste("K seleccionada:",k_optimo),
  sprintf("Silueta promedio Spark: %.6f (distancia euclidiana cuadrada)",
          metricas_k$silueta[metricas_k$k==k_optimo]),
  paste("Variables finales/aprobacion usadas para agrupar: NO"),
  paste("Archivo original conservado (MD5):",identical(md5_antes,md5_despues)),
  "Advertencia: clusters construidos con datos sinteticos; son exploratorios.",
  "No implican diagnosticos individuales ni demuestran utilidad predictiva.",
  "Los identificadores de cluster son etiquetas, no una jerarquia de calidad.",
  "La silueta evaluada en entrenamiento describe cohesion/separacion interna.",
  "La ejecucion distribuida en el cluster Docker requiere prueba independiente."
)
writeLines(reporte,file.path(ruta_salida,"reporte_metodologico.txt"),
           useBytes=TRUE)
validar(file.exists(file.path(ruta_salida,"reporte_metodologico.txt")),
        "No se creo el reporte metodologico.")

cat("\n========== UNIVERSIDAD NOVA - RESULTADOS 3.11 ==========\n")
cat("PERFILES ANALIZADOS:",nrow(perfiles),"\n")
cat("MOTOR: Apache Spark",resultados$version_spark,"local[2]\n")
cat("\nCOMPARACION DE NUMEROS DE CLUSTERS:\n")
print(metricas_k,row.names=FALSE,digits=5)
cat("\nK SELECCIONADA:",k_optimo,"\n")
cat("\nTAMANO DE LOS CLUSTERS:\n")
print(tamanos,row.names=FALSE)
cat("\nCENTROIDES EN ESCALA ORIGINAL:\n")
print(centroides,row.names=FALSE,digits=4)
cat("\nARCHIVO FUENTE INTACTO:",identical(md5_antes,md5_despues),"\n")
cat("RESULTADOS GUARDADOS EN:",normalizePath(ruta_salida),"\n")
cat("MODELO K-MEANS 3.11 COMPLETADO CORRECTAMENTE.\n")
