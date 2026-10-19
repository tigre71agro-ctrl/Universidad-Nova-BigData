# ============================================================================
# UNIVERSIDAD NOVA - BIG DATA - APARTADO 3.8
# ETL reproducible de datos simulados. VERSION UNICA - R BASE
# ENTRADAS : datos_con_errores/*.csv + datos_simulados/*.csv (solo auditoria)
# SALIDAS  : datos_depurados/*.csv y resultados_etl/*.csv
# NO sobrescribe datos_simulados, datos_con_errores ni resultados_calidad.
# FECHA DE CORTE: 2026-10-18
# POLITICA: se recupera solo informacion derivable de fuentes relacionadas;
# el resto se pone en cuarentena con motivo; no se inventan datos personales.
# ============================================================================

cat("\n*** UNIVERSIDAD NOVA - EJECUTANDO ETL 3.8 - VERSION 1 ***\n")
fecha_corte <- as.Date("2026-10-18")
fecha_minima <- as.Date("2024-01-01")
ruta_original <- "datos_simulados"
ruta_entrada  <- "datos_con_errores"
ruta_salida   <- "datos_depurados"
ruta_reportes <- "resultados_etl"

# PASO 1. COMPROBAR DIRECTORIO, FUENTES Y ESTRUCTURA ----------------------
validar <- function(ok, mensaje) {
  if (!isTRUE(ok)) stop(mensaje, call. = FALSE)
}
tablas <- c("carreras", "estudiantes", "docentes", "administrativos",
            "asignaturas", "matriculas", "calificaciones", "pagos",
            "actividad_virtual")
claves <- c(carreras="carrera_id", estudiantes="estudiante_id",
            docentes="docente_id", administrativos="administrativo_id",
            asignaturas="asignatura_id", matriculas="matricula_id",
            calificaciones="calificacion_id", pagos="pago_id",
            actividad_virtual="actividad_id")
requeridos <- list(
  carreras = c("carrera_id", "carrera", "facultad"),
  estudiantes = c("estudiante_id", "nombre", "documento_simulado",
                  "carrera_id", "anio_ingreso", "estado"),
  docentes = c("docente_id", "nombre", "facultad", "categoria"),
  administrativos = c("administrativo_id", "nombre", "area"),
  asignaturas = c("asignatura_id", "carrera_id", "asignatura", "ciclo", "docente_id"),
  matriculas = c("matricula_id", "estudiante_id", "asignatura_id",
                 "periodo", "fecha_matricula", "estado"),
  calificaciones = c("calificacion_id", "matricula_id", "tipo_evaluacion",
                    "nota", "fecha_evaluacion"),
  pagos = c("pago_id", "matricula_id", "estudiante_id", "periodo",
            "concepto", "monto", "fecha_vencimiento", "estado_pago"),
  actividad_virtual = c("actividad_id", "matricula_id", "estudiante_id",
                        "asignatura_id", "fecha_actividad", "tipo_actividad",
                        "duracion_minutos")
)
validar(dir.exists(ruta_entrada) && dir.exists(ruta_original),
        paste0("Faltan las carpetas datos_con_errores o datos_simulados en: ",
               getwd(), ". Configure el directorio de trabajo correcto."))
archivos_alterados <- file.path(ruta_entrada, paste0(tablas, ".csv"))
archivos_originales <- file.path(ruta_original, paste0(tablas, ".csv"))
validar(all(file.exists(c(archivos_alterados, archivos_originales))),
        "Faltan CSV: comprueba ambas carpetas con list.files().")
archivos_protegidos <- c(archivos_alterados, archivos_originales)
md5_antes <- unname(tools::md5sum(archivos_protegidos))
leer <- function(ruta) read.csv(ruta, stringsAsFactors=FALSE,
                                fileEncoding="UTF-8", na.strings=c("", "NA"))
brutos <- setNames(lapply(archivos_alterados, leer), tablas)
originales <- setNames(lapply(archivos_originales, leer), tablas)
for (tabla in tablas) {
  validar(all(requeridos[[tabla]] %in% names(brutos[[tabla]])),
          paste("Faltan columnas en datos_con_errores/", tabla, ".csv", sep=""))
  validar(all(requeridos[[tabla]] %in% names(originales[[tabla]])),
          paste("Faltan columnas en datos_simulados/", tabla, ".csv", sep=""))
}
validar(nrow(brutos$estudiantes)==15120L &&
        nrow(brutos$matriculas)==30100L &&
        nrow(brutos$actividad_virtual)==100100L &&
        nrow(brutos$calificaciones)==60000L && nrow(brutos$pagos)==30000L,
        "Los recuentos no corresponden al apartado 3.7. Revise la carpeta de entrada.")
cat("PASO 1 - Fuentes localizadas; huellas digitales MD5 registradas.\n")

# PASO 2. PREPARAR LA BITACORA, CUARENTENA Y FUNCIONES -------------------
ctx <- new.env(parent=emptyenv())
ctx$eventos <- list()
ctx$cuarentenas <- setNames(vector("list", length(tablas)), tablas)
ctx$duplicados <- setNames(integer(length(tablas)), tablas)
ctx$rechazados <- setNames(integer(length(tablas)), tablas)
ctx$correcciones <- setNames(integer(length(tablas)), tablas)

faltante <- function(x) is.na(x) | trimws(as.character(x)) == ""
fecha <- function(x) suppressWarnings(as.Date(as.character(x), format="%Y-%m-%d"))
invalidar_fecha <- function(x, obligatoria=TRUE) {
  fx <- fecha(x)
  mal <- !is.na(fx) & (fx < fecha_minima | fx > fecha_corte)
  mal <- mal | (!is.na(x) & is.na(fx))
  if (obligatoria) mal <- mal | is.na(fx)
  mal[is.na(mal)] <- TRUE
  mal
}
en_texto <- function(x) {
  y <- as.character(x)
  y[is.na(x)] <- "<NA>"
  y
}
registrar <- function(tabla, x, filas, campo, viejo, nuevo, accion, regla) {
  if (length(filas)==0L) return(invisible(NULL))
  validar(length(viejo)==length(filas) && length(nuevo)==length(filas),
          "Registro de auditoria: longitudes incompatibles")
  ctx$eventos[[length(ctx$eventos)+1L]] <- data.frame(
    tabla=rep(tabla,length(filas)),
    fila_fuente=as.integer(x$fila_fuente[filas]),
    clave=en_texto(x[[claves[[tabla]]]][filas]),
    campo=rep(campo,length(filas)),
    valor_anterior=en_texto(viejo),
    valor_nuevo=en_texto(nuevo),
    accion=rep(accion,length(filas)),
    regla=rep_len(regla,length(filas)),
    stringsAsFactors=FALSE
  )
}
quitar_duplicados <- function(tabla, x) {
  idx <- which(duplicated(x[[claves[[tabla]]]]))
  if (length(idx)) {
    registrar(tabla,x,idx,claves[[tabla]],x[[claves[[tabla]]]][idx],
             rep("<descartado>",length(idx)),"DESCARTAR_DUPLICADO",
             "Conservar primera aparicion de clave primaria")
    ctx$duplicados[tabla] <- ctx$duplicados[tabla] + length(idx)
    x <- x[-idx,,drop=FALSE]
  }
  x
}
# Rechazo conservador: almacenar TODA la fila y la razon, sin destruir fuente.
cuarentenar <- function(tabla, x, motivos) {
  idx <- which(!is.na(motivos) & nzchar(motivos))
  if (length(idx)) {
    registrar(tabla,x,idx,"FILA_COMPLETA",rep("registro en fuente",length(idx)),
             rep("en cuarentena",length(idx)),"CUARENTENA",motivos[idx])
    q <- x[idx,,drop=FALSE]
    q$motivo_cuarentena <- motivos[idx]
    ctx$cuarentenas[[tabla]] <- q
    ctx$rechazados[tabla] <- ctx$rechazados[tabla] + length(idx)
    x <- x[-idx,,drop=FALSE]
  }
  x
}
adjuntar_motivo <- function(motivos, condicion, etiqueta) {
  condicion[is.na(condicion)] <- TRUE
  idx <- which(condicion)
  if (length(idx)) {
    motivos[idx] <- ifelse(nzchar(motivos[idx]),
                          paste(motivos[idx], etiqueta, sep="; "), etiqueta)
  }
  motivos
}
corregir <- function(tabla, x, campo, idx, nuevo, regla) {
  if (!length(idx)) return(x)
  validar(length(idx)==length(nuevo),"Correccion: longitudes incorrectas")
  viejo <- x[[campo]][idx]
  registrar(tabla,x,idx,campo,viejo,nuevo,"CORREGIR",regla)
  x[[campo]][idx] <- nuevo
  ctx$correcciones[tabla] <- ctx$correcciones[tabla]+length(idx)
  x
}
limpios <- setNames(lapply(brutos, function(x) {
  x$fila_fuente <- seq_len(nrow(x))
  x
}), tablas)
cat("PASO 2 - Registros de auditoria y cuarentena preparados.\n")

# PASO 3. CONSERVAR CATALOGOS/MAESTROS ESTABLES -------------------------
for (tb in c("carreras","docentes","administrativos","asignaturas")) {
  limpios[[tb]] <- quitar_duplicados(tb, limpios[[tb]])
  motivos <- rep("", nrow(limpios[[tb]]))
  oblig <- requeridos[[tb]]
  falta <- Reduce(`|`, lapply(oblig, function(f) faltante(limpios[[tb]][[f]])))
  motivos <- adjuntar_motivo(motivos,falta,"Campo obligatorio vacio")
  if (tb=="asignaturas") {
    x <- limpios[[tb]]
    motivos <- adjuntar_motivo(motivos,
      !(x$carrera_id %in% limpios$carreras$carrera_id) |
      !(x$docente_id %in% limpios$docentes$docente_id),
      "Relacion de carrera o docente inexistente")
  }
  limpios[[tb]] <- cuarentenar(tb, limpios[[tb]], motivos)
}
cat("PASO 3 - Catalogos y maestros estables verificados.\n")

# PASO 4. DEPURAR ESTUDIANTES -------------------------------------------
x <- quitar_duplicados("estudiantes", limpios$estudiantes)
# Unicamente se estandarizan mayusculas y espacios, SIN crear nuevos nombres.
nom <- as.character(x$nombre)
nom_limpio <- trimws(nom)
pos <- which(!is.na(nom_limpio) & grepl("^ESTUDIANTE SIMULADO [0-9]{5}$", nom_limpio))
nom_limpio[pos] <- sub("^ESTUDIANTE SIMULADO ",
                       "Estudiante simulado ", nom_limpio[pos])
cambiar <- which(!is.na(nom) & !is.na(nom_limpio) & nom!=nom_limpio)
x <- corregir("estudiantes",x,"nombre",cambiar,nom_limpio[cambiar],
              "Normalizacion de espacios y capitalizacion conocida")
motivos <- rep("",nrow(x))
motivos <- adjuntar_motivo(motivos,Reduce(`|`,lapply(requeridos$estudiantes,
                        function(f) faltante(x[[f]]))),"Dato obligatorio faltante")
motivos <- adjuntar_motivo(motivos,!(x$carrera_id %in% limpios$carreras$carrera_id),
                           "Carrera sin correspondencia")
motivos <- adjuntar_motivo(motivos,
          is.na(x$nombre) | !grepl("^Estudiante simulado [0-9]{5}$",x$nombre),
          "Formato de nombre no validable")
motivos <- adjuntar_motivo(motivos,
          is.na(suppressWarnings(as.integer(x$anio_ingreso))) |
          suppressWarnings(as.integer(x$anio_ingreso)) < 2019L |
          suppressWarnings(as.integer(x$anio_ingreso)) > 2026L,
          "Anio de ingreso invalido")
limpios$estudiantes <- cuarentenar("estudiantes",x,motivos)
cat("PASO 4 - Estudiantes depurados.\n")

# PASO 5. DEPURAR MATRICULAS, RECUPERAR SOLO PERIODOS DERIVABLES --------
x <- quitar_duplicados("matriculas", limpios$matriculas)
fmat <- fecha(x$fecha_matricula)
periodo_valido <- !is.na(x$periodo) &
                  grepl("^(2024|2025|2026)-[12]$", x$periodo)
mes <- suppressWarnings(as.integer(format(fmat,"%m")))
semestre <- ifelse(!is.na(mes) & mes==3L,"1",
                   ifelse(!is.na(mes) & mes==8L,"2",NA_character_))
periodo_inferido <- ifelse(!is.na(semestre),
                          paste0(format(fmat,"%Y"),"-",semestre),NA_character_)
recuperables <- which(!periodo_valido & !is.na(periodo_inferido))
x <- corregir("matriculas",x,"periodo",recuperables,
              periodo_inferido[recuperables],
              "Periodo derivado de fecha_matricula (marzo/agosto)")
idx_est <- match(x$estudiante_id, limpios$estudiantes$estudiante_id)
idx_asi <- match(x$asignatura_id, limpios$asignaturas$asignatura_id)
vinculo_valido <- !is.na(idx_est) & !is.na(idx_asi)
misma_carrera <- rep(FALSE,nrow(x))
misma_carrera[vinculo_valido] <-
  limpios$estudiantes$carrera_id[idx_est[vinculo_valido]] ==
  limpios$asignaturas$carrera_id[idx_asi[vinculo_valido]]
periodo_valido <- !is.na(x$periodo) &
                  grepl("^(2024|2025|2026)-[12]$",x$periodo)
periodo_coherente <- periodo_valido & !is.na(periodo_inferido) &
                     x$periodo==periodo_inferido
motivos <- rep("",nrow(x))
motivos <- adjuntar_motivo(motivos,Reduce(`|`,lapply(requeridos$matriculas,
                          function(f) faltante(x[[f]]))),"Campo obligatorio faltante")
motivos <- adjuntar_motivo(motivos,!vinculo_valido,
                           "Estudiante o asignatura sin referencia valida")
motivos <- adjuntar_motivo(motivos,!misma_carrera,
                           "Asignatura y estudiante de carreras diferentes")
motivos <- adjuntar_motivo(motivos,!periodo_coherente,"Periodo inconsistente con fecha")
motivos <- adjuntar_motivo(motivos,invalidar_fecha(x$fecha_matricula),
                           "Fecha de matricula fuera del rango")
motivos <- adjuntar_motivo(motivos,is.na(x$estado) | x$estado!="Registrada",
                           "Estado de matricula no valido")
limpios$matriculas <- cuarentenar("matriculas",x,motivos)
cat("PASO 5 - Matriculas depuradas y periodos verificados.\n")

# PASO 6. DEPURAR CALIFICACIONES Y PAGOS -------------------------------
x <- quitar_duplicados("calificaciones", limpios$calificaciones)
idx <- match(x$matricula_id, limpios$matriculas$matricula_id)
fex <- fecha(x$fecha_evaluacion)
fini <- fecha(limpios$matriculas$fecha_matricula[idx])
dias <- ifelse(x$tipo_evaluacion=="Parcial",30L,
               ifelse(x$tipo_evaluacion=="Final",65L,NA_integer_))
fecha_inferida <- fini + dias
fechas_malas <- invalidar_fecha(x$fecha_evaluacion)
recuperables <- which(fechas_malas & !is.na(fecha_inferida) &
                      fecha_inferida<=fecha_corte)
x <- corregir("calificaciones",x,"fecha_evaluacion",recuperables,
              as.character(fecha_inferida[recuperables]),
              "Fecha segun tipo de evaluacion y fecha de matricula")
fex <- fecha(x$fecha_evaluacion)
nota <- suppressWarnings(as.numeric(x$nota))
motivos <- rep("",nrow(x))
motivos <- adjuntar_motivo(motivos,Reduce(`|`,lapply(requeridos$calificaciones,
                          function(f) faltante(x[[f]]))),"Campo obligatorio faltante")
motivos <- adjuntar_motivo(motivos,is.na(idx),"Matricula ausente tras ETL")
motivos <- adjuntar_motivo(motivos,is.na(nota) | nota<0 | nota>20,
                           "Nota invalida o faltante")
motivos <- adjuntar_motivo(motivos,is.na(dias),"Tipo de evaluacion desconocido")
motivos <- adjuntar_motivo(motivos,invalidar_fecha(x$fecha_evaluacion) |
                            is.na(fini) | (!is.na(fex) & !is.na(fini) & fex<fini),
                           "Fecha de evaluacion no valida")
limpios$calificaciones <- cuarentenar("calificaciones",x,motivos)

x <- quitar_duplicados("pagos", limpios$pagos)
idx <- match(x$matricula_id, limpios$matriculas$matricula_id)
estado_valido <- !is.na(x$estado_pago) & x$estado_pago %in% c("Pagado","Vencido")
fp <- fecha(x$fecha_pago)
fv <- fecha(x$fecha_vencimiento)
fini <- fecha(limpios$matriculas$fecha_matricula[idx])
monto <- suppressWarnings(as.numeric(x$monto))
ref_valida <- !is.na(idx) & x$estudiante_id %in% limpios$estudiantes$estudiante_id
relacion_coherente <- rep(FALSE,nrow(x))
ok <- which(!is.na(idx))
relacion_coherente[ok] <-
  x$estudiante_id[ok]==limpios$matriculas$estudiante_id[idx[ok]] &
  x$periodo[ok]==limpios$matriculas$periodo[idx[ok]]
fecha_pagada_correcta <- ifelse(x$estado_pago=="Pagado",!is.na(fp),
                                 ifelse(x$estado_pago=="Vencido",is.na(fp),FALSE))
fecha_pagada_correcta[is.na(fecha_pagada_correcta)] <- FALSE
motivos <- rep("",nrow(x))
motivos <- adjuntar_motivo(motivos,Reduce(`|`,lapply(requeridos$pagos,
                          function(f) faltante(x[[f]]))),"Campo obligatorio faltante")
motivos <- adjuntar_motivo(motivos,!ref_valida | !relacion_coherente,
                           "Pago sin relacion valida a matricula o estudiante")
motivos <- adjuntar_motivo(motivos,is.na(monto) | !is.finite(monto) | monto<=0,
                           "Monto no positivo o invalido")
motivos <- adjuntar_motivo(motivos,!estado_valido | !fecha_pagada_correcta,
                           "Estado de pago o fecha de cancelacion incoherente")
motivos <- adjuntar_motivo(motivos,invalidar_fecha(x$fecha_vencimiento) |
                           invalidar_fecha(x$fecha_pago,FALSE) |
                           (!is.na(fini) & !is.na(fv) & fv<fini),
                           "Fecha financiera fuera del rango")
limpios$pagos <- cuarentenar("pagos",x,motivos)
cat("PASO 6 - Calificaciones y pagos depurados.\n")

# PASO 7. DEPURAR ACTIVIDAD VIRTUAL Y RECUPERAR VINCULOS UNIVOCOS -------
x <- quitar_duplicados("actividad_virtual", limpios$actividad_virtual)
idx <- match(x$matricula_id, limpios$matriculas$matricula_id)
coincide <- !is.na(idx) & x$estudiante_id %in% limpios$estudiantes$estudiante_id
fallo_id <- which(!is.na(idx) & (
  is.na(x$estudiante_id) |
  x$estudiante_id != limpios$matriculas$estudiante_id[idx]))
# Solo se reconstruye estudiante_id si la matricula es unica, valida y existe.
x <- corregir("actividad_virtual",x,"estudiante_id",fallo_id,
              limpios$matriculas$estudiante_id[idx[fallo_id]],
              "Estudiante recuperado por matricula_id unico y validado")
fact <- fecha(x$fecha_actividad)
fini <- fecha(limpios$matriculas$fecha_matricula[idx])
duracion <- suppressWarnings(as.numeric(x$duracion_minutos))
coherente <- rep(FALSE,nrow(x))
ok <- which(!is.na(idx))
coherente[ok] <-
  x$estudiante_id[ok] == limpios$matriculas$estudiante_id[idx[ok]] &
  x$asignatura_id[ok] == limpios$matriculas$asignatura_id[idx[ok]]
motivos <- rep("",nrow(x))
motivos <- adjuntar_motivo(motivos,Reduce(`|`,lapply(requeridos$actividad_virtual,
                          function(f) faltante(x[[f]]))),"Campo obligatorio faltante")
motivos <- adjuntar_motivo(motivos,is.na(idx) | !coherente,
                           "Actividad sin relacion coherente a matricula")
motivos <- adjuntar_motivo(motivos,is.na(duracion) | duracion<=0 |
                           !is.finite(duracion),"Duracion invalida")
motivos <- adjuntar_motivo(motivos,invalidar_fecha(x$fecha_actividad) |
                           (!is.na(fact) & !is.na(fini) & fact<fini),
                           "Fecha de actividad fuera de rango")
limpios$actividad_virtual <- cuarentenar("actividad_virtual",x,motivos)
cat("PASO 7 - Actividad virtual depurada.\n")

# PASO 8. MEDIR LAS MISMAS REGLAS ANTES Y DESPUES ------------------------
# IMPORTANTE: las tasas antes y despues tienen denominadores diferentes,
# porque filas no recuperables se apartan en cuarentena.
contar_ref <- function(x, ids) sum(is.na(x) | !(x %in% ids))
medir <- function(datos, etapa) {
  resultado <- list()
  for (tb in tablas) {
    x <- datos[[tb]]
    n <- nrow(x)
    pk <- claves[[tb]]
    req <- requeridos[[tb]]
    falt <- Reduce(`|`,lapply(req,function(campo) faltante(x[[campo]])))
    duplic <- sum(duplicated(x[[pk]]))
    refer <- 0L; negocio <- 0L; fechas <- 0L
    if (tb=="estudiantes") {
      refer <- contar_ref(x$carrera_id,datos$carreras$carrera_id)
      negocio <- sum(is.na(x$nombre) |
                       !grepl("^Estudiante simulado [0-9]{5}$",x$nombre))
    }
    if (tb=="asignaturas") {
      refer <- contar_ref(x$carrera_id,datos$carreras$carrera_id) +
               contar_ref(x$docente_id,datos$docentes$docente_id)
    }
    if (tb=="matriculas") {
      refer <- contar_ref(x$estudiante_id,datos$estudiantes$estudiante_id) +
               contar_ref(x$asignatura_id,datos$asignaturas$asignatura_id)
      fechas <- sum(invalidar_fecha(x$fecha_matricula))
      per <- !is.na(x$periodo) & grepl("^(2024|2025|2026)-[12]$",x$periodo)
      negocio <- sum(!per)
      ii <- match(x$estudiante_id,datos$estudiantes$estudiante_id)
      jj <- match(x$asignatura_id,datos$asignaturas$asignatura_id)
      ok <- which(!is.na(ii) & !is.na(jj))
      negocio <- negocio + sum(datos$estudiantes$carrera_id[ii[ok]] !=
                              datos$asignaturas$carrera_id[jj[ok]],na.rm=TRUE)
    }
    if (tb=="calificaciones") {
      refer <- contar_ref(x$matricula_id,datos$matriculas$matricula_id)
      nota <- suppressWarnings(as.numeric(x$nota))
      negocio <- sum(is.na(nota) | nota<0 | nota>20)
      fechas <- sum(invalidar_fecha(x$fecha_evaluacion))
    }
    if (tb=="pagos") {
      refer <- contar_ref(x$matricula_id,datos$matriculas$matricula_id) +
               contar_ref(x$estudiante_id,datos$estudiantes$estudiante_id)
      monto <- suppressWarnings(as.numeric(x$monto))
      pagado <- x$estado_pago=="Pagado"; vencido <- x$estado_pago=="Vencido"
      negocio <- sum(is.na(monto) | !is.finite(monto) | monto<=0) +
                 sum(is.na(x$estado_pago) | !(x$estado_pago %in% c("Pagado","Vencido"))) +
                 sum(pagado & is.na(fecha(x$fecha_pago)),na.rm=TRUE) +
                 sum(vencido & !is.na(fecha(x$fecha_pago)),na.rm=TRUE)
      # La fecha de pago puede faltar legitimamente si el cargo esta Vencido.
      fechas <- sum(invalidar_fecha(x$fecha_vencimiento)) +
                sum(invalidar_fecha(x$fecha_pago, obligatoria=FALSE))
    }
    if (tb=="actividad_virtual") {
      refer <- contar_ref(x$matricula_id,datos$matriculas$matricula_id) +
               contar_ref(x$estudiante_id,datos$estudiantes$estudiante_id) +
               contar_ref(x$asignatura_id,datos$asignaturas$asignatura_id)
      dur <- suppressWarnings(as.numeric(x$duracion_minutos))
      negocio <- sum(is.na(dur) | !is.finite(dur) | dur<=0)
      fechas <- sum(invalidar_fecha(x$fecha_actividad))
    }
    resultado[[tb]] <- data.frame(
      etapa=etapa, tabla=tb, registros=n,
      claves_duplicadas=duplic,
      filas_con_faltantes=sum(falt),
      referencias_invalidas=refer,
      reglas_negocio_invalidas=negocio,
      fechas_invalidas=fechas,
      stringsAsFactors=FALSE
    )
  }
  do.call(rbind,resultado)
}
# Para fuentes crudas se mide sobre la tabla, nunca sobre la columna fila_fuente.
ind_original <- medir(originales,"Original_referencia")
ind_antes <- medir(brutos,"Antes_ETL")
ind_despues <- medir(limpios,"Despues_ETL")
indicadores <- rbind(ind_original,ind_antes,ind_despues)
rownames(indicadores) <- NULL
# Tasas de incidencias por 100 registros: pueden superar 100 si una fila
# incumple varias referencias. No se interpretan como porcentaje de personas.
metricas <- c("claves_duplicadas", "filas_con_faltantes", "referencias_invalidas",
              "reglas_negocio_invalidas", "fechas_invalidas")
tasas <- do.call(rbind,lapply(metricas,function(m) {
  data.frame(etapa=indicadores$etapa, tabla=indicadores$tabla,
             indicador=m, registros=indicadores$registros,
             casos=indicadores[[m]],
             tasa_por_100_registros=round(100*indicadores[[m]] /
                                             pmax(1L,indicadores$registros),3),
             stringsAsFactors=FALSE)
}))
comparacion_tasas <- merge(
  tasas[tasas$etapa=="Antes_ETL",-1,drop=FALSE],
  tasas[tasas$etapa=="Despues_ETL",-1,drop=FALSE],
  by=c("tabla","indicador"), suffixes=c("_antes","_despues"),
  sort=FALSE)
comparacion_tasas$reduccion_casos <-
  comparacion_tasas$casos_antes-comparacion_tasas$casos_despues

# Comprobar integridad global de la salida antes de grabar.
validar(all(ind_despues$claves_duplicadas==0L),
        "ETL incompleto: siguen existiendo claves duplicadas.")
validar(all(ind_despues$filas_con_faltantes==0L),
        "ETL incompleto: hay campos obligatorios faltantes.")
validar(all(ind_despues$referencias_invalidas==0L),
        "ETL incompleto: hay referencias huerfanas.")
validar(all(ind_despues$reglas_negocio_invalidas==0L),
        "ETL incompleto: hay reglas de negocio pendientes.")
validar(all(ind_despues$fechas_invalidas==0L),
        "ETL incompleto: siguen existiendo fechas invalidas.")
cat("PASO 8 - Indicadores comparativos y controles superados.\n")

# PASO 9. GUARDAR SALIDAS CON TRAZABILIDAD SIN TOCAR FUENTES ------------
md5_revisados <- unname(tools::md5sum(archivos_protegidos))
validar(identical(md5_antes,md5_revisados),
        "Alguno de los archivos de entrada fue modificado inesperadamente.")
resumen <- data.frame(
  tabla=tablas,
  registros_entrada=vapply(brutos,nrow,integer(1)),
  duplicados_descartados=unname(ctx$duplicados[tablas]),
  registros_cuarentena=unname(ctx$rechazados[tablas]),
  registros_salida=vapply(limpios,nrow,integer(1)),
  celdas_corregidas=unname(ctx$correcciones[tablas]),
  stringsAsFactors=FALSE
)
validar(all(resumen$registros_entrada==resumen$duplicados_descartados+
             resumen$registros_cuarentena+resumen$registros_salida),
        "El balance de filas no coincide. Se cancela la exportacion.")
bitacora <- if(length(ctx$eventos)) do.call(rbind,ctx$eventos) else
  data.frame(tabla=character(),fila_fuente=integer(),clave=character(),
             campo=character(),valor_anterior=character(),valor_nuevo=character(),
             accion=character(),regla=character())
rownames(bitacora) <- NULL

dir.create(ruta_salida,showWarnings=FALSE,recursive=TRUE)
dir.create(ruta_reportes,showWarnings=FALSE,recursive=TRUE)
dir.create(file.path(ruta_reportes,"cuarentena"),showWarnings=FALSE,recursive=TRUE)
for(tb in tablas) {
  write.csv(limpios[[tb]],file.path(ruta_salida,paste0(tb,".csv")),
            row.names=FALSE,fileEncoding="UTF-8",na="")
  if(!is.null(ctx$cuarentenas[[tb]])) {
    write.csv(ctx$cuarentenas[[tb]],
              file.path(ruta_reportes,"cuarentena",paste0(tb,"_cuarentena.csv")),
              row.names=FALSE,fileEncoding="UTF-8",na="")
  }
}
write.csv(indicadores,file.path(ruta_reportes,"indicadores_antes_despues.csv"),
          row.names=FALSE,fileEncoding="UTF-8",na="")
write.csv(tasas,file.path(ruta_reportes,"tasas_calidad_etl.csv"),
          row.names=FALSE,fileEncoding="UTF-8",na="")
write.csv(comparacion_tasas,file.path(ruta_reportes,"comparacion_calidad_etl.csv"),
          row.names=FALSE,fileEncoding="UTF-8",na="")
write.csv(resumen,file.path(ruta_reportes,"resumen_etl.csv"),
          row.names=FALSE,fileEncoding="UTF-8",na="")
write.csv(bitacora,file.path(ruta_reportes,"bitacora_transformaciones.csv"),
          row.names=FALSE,fileEncoding="UTF-8",na="")
integridad <- data.frame(
  archivo=archivos_protegidos,
  md5_antes=as.character(md5_antes),
  md5_despues=as.character(tools::md5sum(archivos_protegidos)),
  intacto=md5_antes==unname(tools::md5sum(archivos_protegidos)),
  stringsAsFactors=FALSE
)
validar(all(integridad$intacto),"Se detecto un archivo fuente modificado.")
write.csv(integridad,file.path(ruta_reportes,"integridad_fuentes.csv"),
          row.names=FALSE,fileEncoding="UTF-8")
cat("PASO 9 - CSV depurados, cuarentenas e informes exportados.\n")

# PASO 10. VERIFICAR EXPORTACIONES Y MOSTRAR RESULTADOS -----------------
conteos_guardados <- vapply(tablas, function(tb) {
  nrow(leer(file.path(ruta_salida,paste0(tb,".csv"))))
}, integer(1))
validar(all(conteos_guardados==resumen$registros_salida),
        "Un CSV exportado no conserva el numero esperado de filas.")
validar(all(unname(tools::md5sum(archivos_protegidos))==md5_antes),
        "Se modificaron fuentes originales o alteradas.")
cat("\n=========================================================\n")
cat("UNIVERSIDAD NOVA - RESUMEN FINAL DEL ETL (APARTADO 3.8)\n")
cat("=========================================================\n")
print(resumen,row.names=FALSE)
cat("\nCONTROL DE CALIDAD DESPUES DEL ETL:\n")
print(ind_despues[,c("tabla","claves_duplicadas","filas_con_faltantes",
                     "referencias_invalidas","reglas_negocio_invalidas",
                     "fechas_invalidas")],row.names=FALSE)
cat("\nEventos de trazabilidad:",nrow(bitacora),"\n")
cat("Archivos de entrada originales y alterados intactos:",
    all(integridad$intacto),"\n")
cat("Archivos depurados:",normalizePath(ruta_salida),"\n")
cat("Informes y cuarentenas:",normalizePath(ruta_reportes),"\n")
cat("\nPROCESO ETL 3.8 COMPLETADO CORRECTAMENTE.\n")
cat("NOTA: los rechazos se conservan en cuarentena, no son casos solucionados.\n")

