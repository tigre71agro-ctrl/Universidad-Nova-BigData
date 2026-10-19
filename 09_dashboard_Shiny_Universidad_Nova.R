# ============================================================================
# UNIVERSIDAD NOVA | APARTADO 3.12 | DASHBOARD SHINY
# Integra resultados VERIFICADOS de los apartados 3.8, 3.9, 3.10 y 3.11.
# Solo LECTURA de los CSV: no modifica datos, no reentrena modelos.
# Motor de los modelos de origen: Apache Spark 3.5.7 local[2] con sparklyr.
# Ejecutar en la Console de RStudio: source(file.choose(), echo=FALSE)
# ============================================================================

# PASO 1. DEPENDENCIA --------------------------------------------------------
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Falta el paquete Shiny. Ejecute install.packages('shiny') y reinicie este archivo.",
       call. = FALSE)
}

# PASO 2. LOCALIZAR LA CARPETA DONDE SE GUARDARON LOS RESULTADOS -------------
# Si se utiliza otra carpeta principal, definir antes de ejecutar:
# Sys.setenv(NOVA_DIR = "D:/MiProyecto/UniversidadNova")
archivos_necesarios <- c(
  "resultados_etl/resumen_etl.csv",
  "resultados_etl/tasas_calidad_etl.csv",
  "resultados_analitica/resumen_base_analitica.csv",
  "resultados_ml/metricas_clasificacion.csv",
  "resultados_ml/matriz_confusion.csv",
  "resultados_ml/predicciones_prueba.csv",
  "base_analitica/dataset_ml_clasificacion.csv",
  "base_analitica/dim_carreras.csv",
  "resultados_agrupamiento/comparacion_k_silueta_wss.csv",
  "resultados_agrupamiento/perfiles_estudiantiles_clusters.csv",
  "resultados_agrupamiento/resumen_centroides_clusters.csv",
  "resultados_agrupamiento/coordenadas_pca_clusters.csv"
)

candidatos <- unique(c(
  Sys.getenv("NOVA_DIR", unset = ""),
  file.path(Sys.getenv("USERPROFILE", unset = ""), "Downloads"),
  file.path(Sys.getenv("HOME", unset = ""), "Downloads"),
  getwd()
))
candidatos <- candidatos[nzchar(candidatos) & dir.exists(candidatos)]
puntaje <- vapply(candidatos, function(d) {
  sum(file.exists(file.path(d, archivos_necesarios)))
}, integer(1))
if (!length(puntaje) || max(puntaje) < length(archivos_necesarios)) {
  elegido <- if (length(puntaje)) candidatos[which.max(puntaje)] else getwd()
  faltan <- archivos_necesarios[!file.exists(file.path(elegido, archivos_necesarios))]
  stop(paste0(
    "No se encontraron todos los resultados del proyecto.\n",
    "Carpeta examinada: ", elegido, "\n",
    "Archivos faltantes:\n- ", paste(faltan, collapse = "\n- "), "\n",
    "Coloque las carpetas originales en Downloads o defina NOVA_DIR."
  ), call. = FALSE)
}
raiz <- normalizePath(candidatos[which.max(puntaje)], winslash = "/")
cat("\nUNIVERSIDAD NOVA - INICIANDO DASHBOARD 3.12\n")
cat("Carpeta de resultados: ", raiz, "\n", sep = "")

# PASO 3. IMPORTAR Y COMPROBAR LAS FUENTES ----------------------------------
rutas <- file.path(raiz, archivos_necesarios)
huellas_antes <- unname(tools::md5sum(rutas))
leer <- function(ruta_relativa) {
  read.csv(file.path(raiz, ruta_relativa),
           stringsAsFactors = FALSE, fileEncoding = "UTF-8",
           na.strings = c("", "NA"), check.names = FALSE)
}
exigir <- function(condicion, texto) {
  if (!isTRUE(condicion)) stop(texto, call. = FALSE)
}
columnas <- function(tabla, requeridas, nombre) {
  faltan <- setdiff(requeridas, names(tabla))
  exigir(length(faltan) == 0L,
         paste("Columnas faltantes en", nombre, ":", paste(faltan, collapse = ", ")))
}

etl <- leer("resultados_etl/resumen_etl.csv")
tasas <- leer("resultados_etl/tasas_calidad_etl.csv")
analitica <- leer("resultados_analitica/resumen_base_analitica.csv")
metricas <- leer("resultados_ml/metricas_clasificacion.csv")
confusion <- leer("resultados_ml/matriz_confusion.csv")
predicciones <- leer("resultados_ml/predicciones_prueba.csv")
ml <- leer("base_analitica/dataset_ml_clasificacion.csv")
carreras <- leer("base_analitica/dim_carreras.csv")
comparacion_k <- leer("resultados_agrupamiento/comparacion_k_silueta_wss.csv")
perfiles <- leer("resultados_agrupamiento/perfiles_estudiantiles_clusters.csv")
centroides <- leer("resultados_agrupamiento/resumen_centroides_clusters.csv")
coordenadas <- leer("resultados_agrupamiento/coordenadas_pca_clusters.csv")

columnas(etl, c("tabla", "registros_entrada", "duplicados_descartados",
               "registros_cuarentena", "registros_salida", "celdas_corregidas"),
         "resumen_etl.csv")
columnas(tasas, c("etapa", "tabla", "indicador", "registros", "casos",
                 "tasa_por_100_registros"), "tasas_calidad_etl.csv")
columnas(analitica, c("indicador", "valor"), "resumen_base_analitica.csv")
columnas(metricas, c("indicador", "valor"), "metricas_clasificacion.csv")
columnas(confusion, c("real", "predicho", "n"), "matriz_confusion.csv")
columnas(predicciones, c("matricula_id", "real", "predicho"),
         "predicciones_prueba.csv")
columnas(ml, c("matricula_id", "carrera_id", "nota_parcial", "particion",
               "aprobado_final"), "dataset_ml_clasificacion.csv")
columnas(carreras, c("carrera_id", "carrera"), "dim_carreras.csv")
columnas(comparacion_k, c("k", "silueta", "wss_estandarizada"),
         "comparacion_k_silueta_wss.csv")
columnas(perfiles, c("estudiante_pseudo", "cluster", "promedio_nota_parcial",
                       "promedio_actividades", "promedio_minutos", "promedio_tareas",
                       "matriculas_evaluables"), "perfiles_estudiantiles_clusters.csv")
columnas(centroides, c("cluster", "n_estudiantes", "porcentaje",
                        "promedio_nota_parcial", "promedio_actividades",
                        "promedio_minutos", "promedio_tareas"),
         "resumen_centroides_clusters.csv")
columnas(coordenadas, c("estudiante_pseudo", "cluster", "CP1", "CP2"),
         "coordenadas_pca_clusters.csv")

exigir(!anyDuplicated(ml$matricula_id), "La base de ML tiene matrículas duplicadas.")
exigir(!anyDuplicated(predicciones$matricula_id), "Hay predicciones duplicadas.")
exigir(!anyDuplicated(perfiles$estudiante_pseudo),
       "Hay seudónimos repetidos en los perfiles de K-means.")
exigir(!anyDuplicated(coordenadas$estudiante_pseudo),
       "Existen coordenadas PCA duplicadas.")
exigir(sum(etl$registros_entrada) ==
         sum(etl$duplicados_descartados + etl$registros_cuarentena +
               etl$registros_salida), "No cuadra el balance ETL.")
exigir(nrow(predicciones) == sum(ml$particion == "prueba"),
       "Las predicciones no corresponden a la cantidad de registros de prueba.")
exigir(nrow(perfiles) == nrow(coordenadas),
       "Las coordenadas PCA no coinciden en cantidad con los perfiles.")

idx_ml <- match(predicciones$matricula_id, ml$matricula_id)
exigir(!anyNA(idx_ml), "Una predicción no tiene matrícula en el conjunto analítico.")
exigir(all(ml$particion[idx_ml] == "prueba"),
       "Las predicciones incluyen observaciones ajenas a prueba.")
predicciones$carrera_id <- as.character(ml$carrera_id[idx_ml])
predicciones$nota_parcial <- as.numeric(ml$nota_parcial[idx_ml])
predicciones$real <- as.integer(as.character(predicciones$real))
predicciones$predicho <- as.integer(as.character(predicciones$predicho))
exigir(all(!is.na(predicciones$real) & predicciones$real %in% 0:1) &&
         all(!is.na(predicciones$predicho) & predicciones$predicho %in% 0:1),
       "Clases reales o predichas inválidas.")
exigir(all(predicciones$real == ml$aprobado_final[idx_ml]),
       "La etiqueta real no coincide con la base de prueba.")
exigir(sum(as.numeric(confusion$n)) == nrow(predicciones),
       "La matriz de confusión no coincide con los datos de prueba.")

idx_pca <- match(perfiles$estudiante_pseudo, coordenadas$estudiante_pseudo)
exigir(!anyNA(idx_pca), "Faltan coordenadas para uno o varios estudiantes.")
perfiles$CP1 <- as.numeric(coordenadas$CP1[idx_pca])
perfiles$CP2 <- as.numeric(coordenadas$CP2[idx_pca])
perfiles$cluster <- as.integer(perfiles$cluster)
exigir(all(perfiles$cluster == as.integer(coordenadas$cluster[idx_pca])),
       "Las etiquetas de clúster no coinciden con el PCA.")
exigir(sum(centroides$n_estudiantes) == nrow(perfiles),
       "Los tamaños de clúster no coinciden con los perfiles.")
exigir(identical(huellas_antes, unname(tools::md5sum(rutas))),
       "Algún archivo fuente cambió durante la lectura.")

# PASO 4. PREPARAR RESÚMENES E INTERFAZ ------------------------------------
n_filas <- function(x) formatC(x, format = "d", big.mark = " ")
numero <- function(x, digitos = 2) formatC(x, digits = digitos,
                                          format = "f", decimal.mark = ",")
porcentaje <- function(x, digitos = 2) paste0(numero(100 * x, digitos), "%")
seguro <- function(a, b) if (b > 0) a / b else NA_real_
extraer <- function(tabla, nombre) {
  x <- tabla$valor[match(nombre, tabla$indicador)]
  if (!length(x)) return(NA_real_)
  as.numeric(x[1L])
}
colores_clusters <- c("#147D86", "#D28A3B", "#6D60A8", "#A45764", "#617B34")
col_cluster <- function(x) colores_clusters[((as.integer(x) - 1L) %%
                                               length(colores_clusters)) + 1L]

bloque_kpi <- function(titulo, identificador, nota = "") {
  shiny::column(3, shiny::div(class = "nova-kpi",
    shiny::div(class = "nova-kpi-title", titulo),
    shiny::textOutput(identificador, inline = FALSE),
    shiny::div(class = "nova-kpi-sub", nota)))
}

paleta_etapas <- c("#D99A50", "#167B86")

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("
    body {background:#F5F8FB; color:#162A3A; font-family:Segoe UI, Arial, sans-serif;}
    .nova-header {background:#153B54;color:white;border-radius:12px;padding:23px 26px;margin:16px 0;}
    .nova-header h2 {margin:0 0 7px;font-weight:700;}
    .nova-header p {margin:0;color:#D8E6EE;}
    .nova-kpi {background:white;border:1px solid #DDE6EB;border-radius:10px;
      padding:15px 18px;margin-bottom:18px;min-height:118px;}
    .nova-kpi-title{font-size:13px;color:#51697A;font-weight:600;}
    .nova-kpi .shiny-text-output {font-size:28px;font-weight:700;color:#174D63;}
    .nova-kpi-sub{font-size:11px;color:#6D7E88;margin-top:5px;}
    .nova-panel {background:white;border:1px solid #E0E8ED;border-radius:10px;
      padding:15px;margin:8px 0 18px;}
    .nova-note{color:#607482;font-size:12px;line-height:1.55;}
    .nav-tabs{margin-bottom:18px;} .tab-content{padding-bottom:22px;}
    table {font-size:12px;} .shiny-plot-output{max-width:100%;}
  "))),
  shiny::div(class="nova-header",
    shiny::h2("Universidad Nova | Panel de análisis Big Data"),
    shiny::p("Calidad de datos, rendimiento académico y perfiles K-means | Datos sintéticos")),
  shiny::tabsetPanel(id = "seccion",
    shiny::tabPanel("Resumen general",
      shiny::fluidRow(
        bloque_kpi("Registros recibidos por ETL", "kpi_entrada", "Suma de nueve tablas"),
        bloque_kpi("Registros depurados", "kpi_salida", "Después de duplicados y cuarentena"),
        bloque_kpi("Matrículas modelables", "kpi_ml", "Entrenamiento + prueba"),
        bloque_kpi("Perfiles agrupados", "kpi_perfiles", "Estudiantes seudonimizados")
      ),
      shiny::fluidRow(
        shiny::column(7, shiny::div(class="nova-panel",
          shiny::h4("Balance del proceso ETL"),
          shiny::plotOutput("grafico_balance", height="330px"))),
        shiny::column(5, shiny::div(class="nova-panel",
          shiny::h4("Distribución del resultado académico"),
          shiny::plotOutput("grafico_aprobacion", height="330px")))
      ),
      shiny::div(class="nova-panel", shiny::h4("Trazabilidad y alcance"),
        shiny::tableOutput("tabla_resumen_analitico"),
        shiny::p(class="nova-note",
          "Los registros separados en cuarentena NO se consideran corregidos. ",
          "El dashboard visualiza CSV producidos por scripts SparklyR en modo local[2]; ",
          "no constituye una demostración de ejecución distribuida en Docker."))
    ),
    shiny::tabPanel("Calidad y ETL",
      shiny::fluidRow(
        shiny::column(4,
          shiny::div(class="nova-panel",
            shiny::selectInput("tabla_etl", "Tabla", choices=c("Todas", as.character(etl$tabla))),
            shiny::selectInput("indicador_etl", "Indicador de calidad",
              choices=c("Claves duplicadas"="claves_duplicadas",
                        "Filas con faltantes"="filas_con_faltantes",
                        "Referencias inválidas"="referencias_invalidas",
                        "Reglas de negocio inválidas"="reglas_negocio_invalidas",
                        "Fechas inválidas"="fechas_invalidas")),
            shiny::radioButtons("modo_etl", "Escala", inline=TRUE,
              choices=c("Casos"="casos", "Tasa por 100 registros"="tasa_por_100_registros")),
            shiny::downloadButton("descargar_etl", "Exportar balance filtrado"),
            shiny::p(class="nova-note", "Las tasas pueden superar 100 cuando hay múltiples incidencias por registro."))
        ),
        shiny::column(8, shiny::div(class="nova-panel",
          shiny::h4("Antes y después de la depuración"),
          shiny::plotOutput("grafico_etl", height="360px")))
      ),
      shiny::div(class="nova-panel", shiny::h4("Balance de filas por tabla"),
        shiny::tableOutput("tabla_etl"))
    ),
    shiny::tabPanel("Clasificación SparklyR",
      shiny::fluidRow(
        bloque_kpi("Exactitud global (prueba)", "kpi_accuracy", "Sin filtros | Regresión logística"),
        bloque_kpi("Referencia mayoritaria", "kpi_baseline", "Sin filtros"),
        bloque_kpi("AUC-ROC de prueba", "kpi_auc", "Sin filtros"),
        bloque_kpi("Exactitud balanceada", "kpi_balanceada", "Sin filtros")
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::div(class="nova-panel",
          shiny::selectInput("carrera_clasif", "Carrera", choices=NULL),
          shiny::selectInput("real_clasif", "Resultado real",
            choices=c("Ambos"="todos", "No aprobado (0)"="0", "Aprobado (1)"="1")),
          shiny::selectInput("pred_clasif", "Resultado predicho",
            choices=c("Ambos"="todos", "No aprobado (0)"="0", "Aprobado (1)"="1")),
          shiny::downloadButton("descargar_confusion", "Exportar matriz filtrada"),
          shiny::p(class="nova-note",
            "Los filtros recalculan cifras descriptivas sobre las predicciones existentes. ",
            "No reentrenan el modelo ni recalculan el AUC."))),
        shiny::column(8, shiny::div(class="nova-panel",
          shiny::h4("Matriz de confusión (selección actual)"),
          shiny::plotOutput("grafico_confusion", height="320px"),
          shiny::textOutput("texto_clasificacion")))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::div(class="nova-panel",
          shiny::h4("Métricas originales de prueba"), shiny::tableOutput("tabla_metricas"))),
        shiny::column(6, shiny::div(class="nova-panel",
          shiny::h4("Nota parcial según aprobación real (selección)"),
          shiny::plotOutput("grafico_notas", height="330px")))
      )
    ),
    shiny::tabPanel("Agrupamiento K-means",
      shiny::fluidRow(
        bloque_kpi("Estudiantes agrupados", "kpi_estudiantes_cluster", "Universidad Nova"),
        bloque_kpi("K seleccionado", "kpi_k", "Máximo índice de silueta"),
        bloque_kpi("Silueta seleccionada", "kpi_silueta", "Spark, distancia cuadrada"),
        bloque_kpi("Clústeres disponibles", "kpi_grupos", "Sin etiquetas personales")
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::div(class="nova-panel",
          shiny::selectInput("cluster_filtro", "Clúster", choices=NULL),
          shiny::sliderInput("nota_rango", "Promedio de nota parcial",
                             min=0, max=20, value=c(0,20), step=0.5),
          shiny::selectInput("variable_cluster", "Variable para comparar centroides",
            choices=c("Nota parcial"="promedio_nota_parcial",
              "Actividades virtuales"="promedio_actividades",
              "Minutos de participación"="promedio_minutos",
              "Tareas"="promedio_tareas")),
          shiny::downloadButton("descargar_clusters", "Exportar resumen filtrado"),
          shiny::p(class="nova-note",
            "El gráfico PCA es una representación visual; K-means se entrenó con ",
            "las variables estandarizadas completas."))),
        shiny::column(8, shiny::div(class="nova-panel",
          shiny::h4("Proyección PCA de los grupos"),
          shiny::plotOutput("grafico_pca", height="380px"),
          shiny::textOutput("texto_muestra")))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::div(class="nova-panel",
          shiny::h4("Silueta por número de clústeres"),
          shiny::plotOutput("grafico_silueta", height="300px"))),
        shiny::column(6, shiny::div(class="nova-panel",
          shiny::h4("Centroides: selección actual"),
          shiny::plotOutput("grafico_centroides", height="300px")))
      ),
      shiny::div(class="nova-panel", shiny::h4("Perfil promedio por grupo (selección actual)"),
        shiny::tableOutput("tabla_clusters"))
    ),
    shiny::tabPanel("Metodología y fuentes",
      shiny::div(class="nova-panel",
        shiny::h3("Interpretación responsable"),
        shiny::tags$ul(
          shiny::tags$li("Todas las observaciones y anomalías son simuladas; no corresponden a estudiantes reales."),
          shiny::tags$li("El ETL conserva en cuarentena los registros no recuperables. Un rechazo no es una corrección."),
          shiny::tags$li("La clasificación utilizó una partición por estudiante y no incorporó la nota final como predictor."),
          shiny::tags$li("Exactitud alta para la clase mayoritaria no implica identificar correctamente a estudiantes no aprobados."),
          shiny::tags$li("Los dos clústeres describen asociaciones exploratorias, no causas ni diagnósticos individuales."),
          shiny::tags$li("Los modelos se ejecutaron con Spark local[2]. La ejecución distribuida en Docker debe comprobarse por separado."),
          shiny::tags$li("Los datos se leen una vez al abrir el dashboard y las fuentes CSV no se modifican.")
        ),
        shiny::h4("Directorio de resultados"), shiny::verbatimTextOutput("ruta_fuentes"),
        shiny::h4("Controles de integridad y consistencia"),
        shiny::verbatimTextOutput("texto_integridad"),
        shiny::h4("Archivos utilizados"),
        shiny::tableOutput("tabla_fuentes")))
  )
)

# PASO 5. LÓGICA INTERACTIVA -------------------------------------------------
server <- function(input, output, session) {
  elegir <- c("Todas" = "TODAS")
  opciones_carreras <- unique(carreras[, c("carrera_id", "carrera")])
  elegir <- c(elegir,
    stats::setNames(as.character(opciones_carreras$carrera_id),
                    as.character(opciones_carreras$carrera)))
  shiny::updateSelectInput(session, "carrera_clasif", choices = elegir,
                           selected = "TODAS")
  grupos <- sort(unique(perfiles$cluster))
  eleccion_clusters <- c("Todos" = "todos",
    stats::setNames(as.character(grupos), paste("Clúster", grupos)))
  shiny::updateSelectInput(session, "cluster_filtro", choices = eleccion_clusters,
                           selected = "todos")

  # Resumen general
  output$kpi_entrada <- shiny::renderText(n_filas(sum(etl$registros_entrada)))
  output$kpi_salida <- shiny::renderText(n_filas(sum(etl$registros_salida)))
  output$kpi_ml <- shiny::renderText(n_filas(nrow(ml)))
  output$kpi_perfiles <- shiny::renderText(n_filas(nrow(perfiles)))
  output$grafico_balance <- shiny::renderPlot({
    seleccion <- as.matrix(etl[, c("registros_entrada", "registros_salida")])
    datos <- t(seleccion)
    op <- graphics::par(mar = c(8, 5, 2, 1))
    on.exit(graphics::par(op))
    graphics::barplot(datos, beside=TRUE, col=paleta_etapas,
      names.arg=etl$tabla, las=2, cex.names=0.78,
      ylab="Registros", border=NA)
    graphics::legend("topright", legend=c("Entrada", "Salida depurada"),
      fill=paleta_etapas, bty="n", cex=0.85)
  })
  output$grafico_aprobacion <- shiny::renderPlot({
    clases <- table(factor(ml$aprobado_final, levels=0:1,
                            labels=c("No aprobado", "Aprobado")))
    op <- graphics::par(mar=c(5,4,3,1))
    on.exit(graphics::par(op))
    vals <- graphics::barplot(clases, col=c("#D3A45A", "#167B86"),
      ylim=c(0, max(clases)*1.17), ylab="Matrículas", border=NA)
    graphics::text(vals, clases, labels=paste0(n_filas(as.integer(clases)), " (",
                    porcentaje(as.numeric(clases)/sum(clases),1), ")"), pos=3)
  })
  output$tabla_resumen_analitico <- shiny::renderTable({
    datos <- analitica[analitica$indicador %in%
      c("matriculas_depuradas", "matriculas_modelables", "registros_entrenamiento",
        "registros_prueba", "estudiantes_modelables",
        "actividades_entre_parcial_y_final"), , drop=FALSE]
    names(datos) <- c("Indicador", "Cantidad")
    datos$Cantidad <- n_filas(as.integer(datos$Cantidad))
    datos
  }, striped=TRUE, spacing="s")

  # Proceso ETL: indicadores por tabla y etapa
  etl_filtrado <- shiny::reactive({
    if (is.null(input$tabla_etl) || input$tabla_etl == "Todas") return(etl)
    etl[etl$tabla == input$tabla_etl, , drop=FALSE]
  })
  tasas_filtradas <- shiny::reactive({
    x <- tasas[tasas$indicador == input$indicador_etl &
                 tasas$etapa %in% c("Antes_ETL", "Despues_ETL"), ,drop=FALSE]
    if (!is.null(input$tabla_etl) && input$tabla_etl != "Todas") {
      x <- x[x$tabla == input$tabla_etl, , drop=FALSE]
    }
    x
  })
  output$tabla_etl <- shiny::renderTable({
    x <- etl_filtrado()
    names(x) <- c("Tabla", "Entrada", "Duplicados", "Cuarentena", "Salida", "Celdas corregidas")
    x
  }, striped=TRUE, spacing="xs", digits=0)
  output$grafico_etl <- shiny::renderPlot({
    x <- tasas_filtradas()
    shiny::validate(shiny::need(nrow(x) > 0, "No hay registros para esta selección."))
    valor <- input$modo_etl
    matriz <- sapply(unique(x$tabla), function(tabla) {
      c(sum(x[[valor]][x$tabla==tabla & x$etapa=="Antes_ETL"]),
        sum(x[[valor]][x$tabla==tabla & x$etapa=="Despues_ETL"]))
    })
    matriz <- matrix(matriz, nrow=2L,
                     dimnames=list(c("Antes", "Después"), unique(x$tabla)))
    op <- graphics::par(mar=c(if(ncol(matriz)>3) 8 else 5, 5, 2, 1))
    on.exit(graphics::par(op))
    graphics::barplot(matriz, beside=TRUE, col=paleta_etapas, border=NA,
      las=if(ncol(matriz)>3) 2 else 1, cex.names=0.80,
      ylab=if(valor=="casos") "Casos" else "Incidencias por 100 registros")
    graphics::legend("topright", legend=c("Antes", "Después"),
                     fill=paleta_etapas, bty="n")
  })
  output$descargar_etl <- shiny::downloadHandler(
    filename=function() "universidad_nova_balance_etl_filtrado.csv",
    content=function(file) utils::write.csv(etl_filtrado(), file, row.names=FALSE,
                                           fileEncoding="UTF-8"))

  # Clasificación: filtrado de PREDICCIONES EXISTENTES sin reentrenamiento
  pred_filtradas <- shiny::reactive({
    x <- predicciones
    if (!is.null(input$carrera_clasif) && input$carrera_clasif != "TODAS")
      x <- x[x$carrera_id==input$carrera_clasif, , drop=FALSE]
    if (!is.null(input$real_clasif) && input$real_clasif != "todos")
      x <- x[x$real==as.integer(input$real_clasif), , drop=FALSE]
    if (!is.null(input$pred_clasif) && input$pred_clasif != "todos")
      x <- x[x$predicho==as.integer(input$pred_clasif), , drop=FALSE]
    x
  })
  matriz_confusion <- shiny::reactive({
    x <- pred_filtradas()
    m <- table(factor(x$real,levels=0:1), factor(x$predicho,levels=0:1))
    dimnames(m) <- list(Real=c("No aprobado", "Aprobado"),
                        Predicho=c("No aprobado", "Aprobado"))
    m
  })
  output$kpi_accuracy <- shiny::renderText(porcentaje(extraer(metricas,"accuracy_prueba")))
  output$kpi_baseline <- shiny::renderText(porcentaje(extraer(metricas,"accuracy_baseline_mayoritaria")))
  output$kpi_auc <- shiny::renderText(numero(extraer(metricas,"auc_roc_prueba"),4))
  output$kpi_balanceada <- shiny::renderText(porcentaje(extraer(metricas,"balanced_accuracy")))
  output$grafico_confusion <- shiny::renderPlot({
    m <- matriz_confusion()
    shiny::validate(shiny::need(sum(m)>0, "No hay predicciones con esos filtros."))
    op <- graphics::par(mar=c(5,5,2,1))
    on.exit(graphics::par(op))
    graphics::plot(NA, xlim=c(0.5,2.5), ylim=c(0.5,2.5), xaxt="n",yaxt="n",
      xlab="Predicho",ylab="Real",xaxs="i",yaxs="i", bty="n")
    graphics::axis(1,at=1:2,labels=colnames(m))
    graphics::axis(2,at=1:2,labels=rev(rownames(m)),las=1)
    for (i in 1:2) for (j in 1:2) {
      ypos <- 3-i
      color <- if (i==j) "#D6EEE8" else "#F5DED7"
      graphics::rect(j-.48,ypos-.48,j+.48,ypos+.48,col=color,border="white",lwd=3)
      graphics::text(j,ypos,labels=n_filas(as.integer(m[i,j])),cex=1.5,font=2)
    }
  })
  output$texto_clasificacion <- shiny::renderText({
    x <- pred_filtradas()
    if (!nrow(x)) return("No hay predicciones con estos filtros.")
    paste0("Observaciones: ", n_filas(nrow(x)), " | Aciertos: ",
           n_filas(sum(x$real==x$predicho)), " | Exactitud en la selección: ",
           porcentaje(mean(x$real==x$predicho)), ".")
  })
  output$tabla_metricas <- shiny::renderTable({
    x <- metricas
    x$valor <- round(as.numeric(x$valor), 4)
    names(x) <- c("Indicador", "Valor")
    x
  }, striped=TRUE, spacing="xs")
  output$grafico_notas <- shiny::renderPlot({
    x <- pred_filtradas()
    shiny::validate(shiny::need(nrow(x)>1L, "No hay observaciones suficientes."))
    op <- graphics::par(mar=c(5,4,2,1))
    on.exit(graphics::par(op))
    graphics::boxplot(nota_parcial ~ factor(real, levels=0:1,
      labels=c("No aprobado", "Aprobado")), data=x,
      col=c("#D8AF70", "#87C5C0"), xlab="Resultado real",
      ylab="Nota parcial (0–20)", outline=FALSE)
  })
  output$descargar_confusion <- shiny::downloadHandler(
    filename=function() "universidad_nova_matriz_confusion_filtrada.csv",
    content=function(file) {
      m <- as.data.frame(matriz_confusion())
      names(m) <- c("real", "predicho", "n")
      utils::write.csv(m, file, row.names=FALSE, fileEncoding="UTF-8")
    })

  # K-means: filtros para visualizar perfiles SIN recalcular grupos
  perfiles_filtrados <- shiny::reactive({
    x <- perfiles
    if (!is.null(input$cluster_filtro) && input$cluster_filtro != "todos")
      x <- x[x$cluster==as.integer(input$cluster_filtro),,drop=FALSE]
    rango <- input$nota_rango
    if (!is.null(rango))
      x <- x[x$promedio_nota_parcial>=rango[1] &
               x$promedio_nota_parcial<=rango[2],,drop=FALSE]
    x
  })
  resumen_clusters_filtrados <- shiny::reactive({
    x <- perfiles_filtrados()
    if (!nrow(x)) return(data.frame())
    variables <- c("promedio_nota_parcial", "promedio_actividades",
                   "promedio_minutos", "promedio_tareas", "matriculas_evaluables")
    promedios <- stats::aggregate(x[,variables,drop=FALSE],
                                  by=list(cluster=x$cluster),FUN=mean)
    conteos <- as.data.frame(table(x$cluster), stringsAsFactors=FALSE)
    names(conteos) <- c("cluster", "n_estudiantes")
    conteos$cluster <- as.integer(as.character(conteos$cluster))
    z <- merge(conteos,promedios,by="cluster",sort=TRUE)
    z$porcentaje_de_seleccion <- round(100*z$n_estudiantes / nrow(x),2)
    z
  })
  k_seleccionado <- as.integer(comparacion_k$k[which.max(comparacion_k$silueta)])
  output$kpi_estudiantes_cluster <- shiny::renderText(n_filas(nrow(perfiles)))
  output$kpi_k <- shiny::renderText(as.character(k_seleccionado))
  output$kpi_silueta <- shiny::renderText(numero(max(comparacion_k$silueta),5))
  output$kpi_grupos <- shiny::renderText(as.character(length(grupos)))
  output$grafico_pca <- shiny::renderPlot({
    x <- perfiles_filtrados()
    shiny::validate(shiny::need(nrow(x)>1L, "No hay suficientes perfiles para representar."))
    set.seed(2026)
    if(nrow(x)>4000L) x <- x[sample.int(nrow(x),4000L),,drop=FALSE]
    op <- graphics::par(mar=c(5,5,2,1))
    on.exit(graphics::par(op))
    graphics::plot(x$CP1,x$CP2, pch=16, cex=0.45,
      col=grDevices::adjustcolor(col_cluster(x$cluster), alpha.f=0.45),
      xlab="Componente principal 1", ylab="Componente principal 2")
    graphics::legend("topright", legend=paste("Clúster",sort(unique(x$cluster))),
      col=col_cluster(sort(unique(x$cluster))), pch=16,bty="n",cex=0.9)
  })
  output$texto_muestra <- shiny::renderText({
    x <- perfiles_filtrados()
    paste0("Perfiles que cumplen los filtros: ", n_filas(nrow(x)),
           ". Para mantener fluidez, se visualizan hasta 4 000 puntos ",
           "(muestra reproducible); los resúmenes utilizan todos los perfiles filtrados.")
  })
  output$grafico_silueta <- shiny::renderPlot({
    x <- comparacion_k[order(comparacion_k$k),,drop=FALSE]
    op <- graphics::par(mar=c(5,5,2,1))
    on.exit(graphics::par(op))
    graphics::plot(x$k, x$silueta,type="b",pch=19, lwd=2,col="#167B86",
      xlab="Número de grupos K",ylab="Índice de silueta",xaxt="n",
      ylim=c(0,max(x$silueta)*1.15))
    graphics::axis(1,at=x$k)
    graphics::points(k_seleccionado,max(x$silueta),pch=21,bg="#D99A50",cex=1.5)
  })
  output$grafico_centroides <- shiny::renderPlot({
    x <- resumen_clusters_filtrados()
    shiny::validate(shiny::need(nrow(x)>0, "No hay estudiantes para los filtros."))
    variable <- input$variable_cluster
    if (is.null(variable)) variable <- "promedio_nota_parcial"
    op <- graphics::par(mar=c(5,5,2,1))
    on.exit(graphics::par(op))
    graphics::barplot(x[[variable]],names.arg=paste("Clúster",x$cluster),
      col=col_cluster(x$cluster),border=NA,
      ylim=c(0,max(x[[variable]])*1.20+0.01),
      ylab=paste("Promedio de",gsub("_"," ",variable)))
  })
  output$tabla_clusters <- shiny::renderTable({
    x <- resumen_clusters_filtrados()
    shiny::validate(shiny::need(nrow(x)>0, "Sin perfiles para estos filtros."))
    x[,c("cluster", "n_estudiantes", "porcentaje_de_seleccion",
         "promedio_nota_parcial", "promedio_actividades", "promedio_minutos",
         "promedio_tareas", "matriculas_evaluables"),drop=FALSE]
  }, digits=3, striped=TRUE, spacing="xs")
  output$descargar_clusters <- shiny::downloadHandler(
    filename=function() "universidad_nova_clusters_resumen_filtrado.csv",
    content=function(file) utils::write.csv(resumen_clusters_filtrados(),file,
                                            row.names=FALSE,fileEncoding="UTF-8"))

  # Procedencia y trazabilidad
  output$ruta_fuentes <- shiny::renderText(raiz)
  output$texto_integridad <- shiny::renderText(paste0(
    "Archivos fuente obligatorios: ", length(rutas),
    "\nHuellas MD5 verificadas durante la carga: ",
    if (identical(huellas_antes, unname(tools::md5sum(rutas)))) "TRUE" else "FALSE",
    "\nModalidad de entrenamiento de modelos: Spark local[2] (no clúster Docker)"
  ))
  output$tabla_fuentes <- shiny::renderTable(data.frame(
    Archivo=archivos_necesarios,
    Estado=ifelse(file.exists(rutas),"Encontrado", "No disponible")),
    striped=TRUE, spacing="xs")
}

# PASO 6. INICIAR EL PANEL LOCAL EN EL NAVEGADOR -----------------------------
cat("Control de archivos: CORRECTO | Clústeres:",
    length(unique(perfiles$cluster)), "| Perfiles:",nrow(perfiles),"\n")
cat("DASHBOARD SHINY 3.12 - LISTO PARA INICIAR.\n")
cat("Para cerrar el dashboard, pulse ESC en la consola de RStudio.\n")

aplicacion <- shiny::shinyApp(ui=ui, server=server)
if (interactive()) {
  shiny::runApp(aplicacion, launch.browser=TRUE, host="127.0.0.1")
} else {
  cat("Ejecute este archivo en una sesión interactiva de RStudio.\n")
}
