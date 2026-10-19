# ============================================================================
# UNIVERSIDAD NOVA | PROYECTO BIG DATA - APARTADO 3.7
# SCRIPT 03 - VERSION REVISADA: INYECCION DE ERRORES + INDICADORES
# REQUIERE: archivos generados por los scripts 01 y 02 FINAL.
# DEPENDENCIAS: solo R base (sin paquetes adicionales).
# SEGURIDAD: NO EDITA NI SOBRESCRIBE archivos de datos_simulados.
# Fecha de corte del caso: 18-10-2026. Semilla: 2026.
# CORRECCION: las funciones que alteran las tablas guardan los cambios
# en la lista principal `alterados` (alcance de variables de R).
# No utilizar las versiones antiguas de este script.
# ============================================================================

# PASO 1. CONFIGURAR EL PROYECTO Y VERIFICAR SUS ARCHIVOS ------------------
cat("\n*** EJECUTANDO SCRIPT 03 REVISADO - VERSION 2 ***\n")
set.seed(2026)
fecha_corte <- as.Date("2026-10-18")
ruta_origen <- "datos_simulados"
ruta_alterados <- "datos_con_errores"
ruta_informes <- "resultados_calidad"

validar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call. = FALSE)
}

nombres_tablas <- c(
  "carreras", "estudiantes", "docentes", "administrativos",
  "asignaturas", "matriculas", "calificaciones", "pagos",
  "actividad_virtual"
)
claves <- c(
  carreras = "carrera_id", estudiantes = "estudiante_id",
  docentes = "docente_id", administrativos = "administrativo_id",
  asignaturas = "asignatura_id", matriculas = "matricula_id",
  calificaciones = "calificacion_id", pagos = "pago_id",
  actividad_virtual = "actividad_id"
)

campos_obligatorios <- list(
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
  # fecha_pago NO es obligatoria si el pago sigue pendiente.
  actividad_virtual = c("actividad_id", "matricula_id", "estudiante_id",
                        "asignatura_id", "fecha_actividad",
                        "tipo_actividad", "duracion_minutos")
)

rutas_fuente <- file.path(ruta_origen, paste0(nombres_tablas, ".csv"))
validar(dir.exists(ruta_origen), paste0(
  "No existe la carpeta 'datos_simulados'. Revisa el directorio con getwd(): ",
  getwd()
))
validar(all(file.exists(rutas_fuente)), paste0(
  "Faltan CSV de los scripts 01/02 en 'datos_simulados': ",
  paste(basename(rutas_fuente[!file.exists(rutas_fuente)]), collapse = ", ")
))
md5_antes <- unname(tools::md5sum(rutas_fuente))
cat("PASO 1 - Los nueve CSV originales estan disponibles.\n")

# PASO 2. LEER BASES MAESTRAS Y TRANSACCIONALES ---------------------------
originales <- setNames(lapply(rutas_fuente, function(archivo) {
  read.csv(archivo, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}), nombres_tablas)

for (tabla in nombres_tablas) {
  validar(all(campos_obligatorios[[tabla]] %in% names(originales[[tabla]])),
          paste("Faltan columnas obligatorias en:", tabla))
  validar(!anyDuplicated(originales[[tabla]][[claves[[tabla]]]]),
          paste("La fuente ya contiene claves duplicadas:", tabla))
}

validar(nrow(originales$estudiantes) == 15000L &&
          nrow(originales$matriculas) == 30000L &&
          nrow(originales$calificaciones) == 60000L &&
          nrow(originales$pagos) == 30000L &&
          nrow(originales$actividad_virtual) == 100000L,
        "Las cantidades no coinciden con el script 02 FINAL.")
validar(all(grepl("^(2024|2025|2026)-[12]$", originales$matriculas$periodo)),
        "La fuente contiene periodos invalidos: revisa el script 02 FINAL.")
validar(!anyNA(originales$actividad_virtual$fecha_actividad),
        "La fuente tiene fechas faltantes: revisa el script 02 FINAL.")

# Data frames se copian en memoria: nunca se guardaran sobre la fuente.
alterados <- originales
cat("PASO 2 - Bases leidas, estructura verificada y copias creadas en memoria.\n")

# PASO 3. FUNCIONES PARA INYECTAR ERRORES Y REGISTRAR SU TRAZABILIDAD -----
# Un mismo registro original no recibe dos tipos de error: facilita auditoria.
disponibles <- new.env(parent = emptyenv())
for (tabla in nombres_tablas) {
  disponibles[[tabla]] <- seq_len(nrow(originales[[tabla]]))
}
registro <- new.env(parent = emptyenv())
registro$eventos <- list()

seleccionar <- function(tabla, cantidad, elegibles = NULL) {
  pool <- disponibles[[tabla]]
  if (!is.null(elegibles)) pool <- intersect(pool, elegibles)
  validar(length(pool) >= cantidad,
          paste("No hay suficientes registros para:", tabla))
  # sample.int evita el error que causa sample(2026, 1).
  posiciones <- sample.int(length(pool), size = cantidad, replace = FALSE)
  filas <- pool[posiciones]
  disponibles[[tabla]] <- disponibles[[tabla]][
    !(disponibles[[tabla]] %in% filas)
  ]
  filas
}

registrar <- function(tabla, filas, campo, anteriores, posteriores, clase) {
  valores <- function(x) {
    y <- as.character(x)
    y[is.na(x)] <- "<NA>"
    y
  }
  registro$eventos[[length(registro$eventos) + 1L]] <- data.frame(
    tabla = tabla,
    tipo_error = clase,
    identificador = valores(alterados[[tabla]][[claves[[tabla]]]][filas]),
    fila_origen = as.integer(filas),
    campo = campo,
    valor_original = valores(anteriores),
    valor_alterado = valores(posteriores),
    stringsAsFactors = FALSE
  )
}

modificar <- function(tabla, campo, filas, valores_nuevos, clase) {
  validar(length(filas) == length(valores_nuevos),
          paste("Numero incorrecto de valores nuevos en:", tabla, campo))
  datos <- alterados[[tabla]]
  valores_viejos <- datos[[campo]][filas]
  datos[[campo]][filas] <- valores_nuevos
  # MUY IMPORTANTE: <<- persiste la tabla en la lista exterior;
  # <- solo modificaria una variable local de esta funcion.
  alterados[[tabla]] <<- datos
  registrar(tabla, filas, campo, valores_viejos,
            datos[[campo]][filas], clase)
}

duplicar_filas <- function(tabla, cantidad) {
  filas <- seleccionar(tabla, cantidad)
  datos <- alterados[[tabla]]
  # Se agregan copias exactas con la misma clave primaria.
  datos <- rbind(datos, datos[filas, , drop = FALSE])
  # Guardar tambien los duplicados en la lista exterior.
  alterados[[tabla]] <<- datos
  registrar(tabla, filas, "FILA_COMPLETA",
            rep("registro original", cantidad),
            rep("copia agregada", cantidad), "Duplicacion")
}
cat("PASO 3 - Funciones de simulacion y bitacora creadas.\n")

# PASO 4. SIMULAR ERRORES EN ESTUDIANTES Y MATRICULAS --------------------
# ESTUDIANTES: 180 nombres faltantes; 250 nombres con otro formato;
# 90 carreras inexistentes; 120 filas duplicadas.
filas <- seleccionar("estudiantes", 180L)
modificar("estudiantes", "nombre", filas,
          rep(NA_character_, length(filas)), "Dato_faltante")

filas <- seleccionar("estudiantes", 250L)
nombres_modificados <- paste0(
  "  ", toupper(alterados$estudiantes$nombre[filas]), "  "
)
modificar("estudiantes", "nombre", filas, nombres_modificados,
          "Formato_inconsistente")

filas <- seleccionar("estudiantes", 90L)
modificar("estudiantes", "carrera_id", filas,
          rep("CAR99", length(filas)), "Referencia_inexistente")

duplicar_filas("estudiantes", 120L)

# MATRICULAS: 100 periodos invalidos; 120 IDs de estudiante faltantes;
# 110 asignaturas inexistentes; 100 filas duplicadas.
filas <- seleccionar("matriculas", 100L)
modificar("matriculas", "periodo", filas,
          rep("202A-9", length(filas)), "Periodo_invalido")

filas <- seleccionar("matriculas", 120L)
modificar("matriculas", "estudiante_id", filas,
          rep(NA_character_, length(filas)), "Dato_faltante")

filas <- seleccionar("matriculas", 110L)
modificar("matriculas", "asignatura_id", filas,
          rep("ASI999", length(filas)), "Referencia_inexistente")

duplicar_filas("matriculas", 100L)
cat("PASO 4 - Errores de estudiantes y matriculas introducidos.\n")

# PASO 5. SIMULAR ERRORES EN CALIFICACIONES, PAGOS Y ACTIVIDAD VIRTUAL --
# CALIFICACIONES: 180 notas fuera de [0, 20], 80 notas faltantes,
# 100 fechas posteriores al corte.
filas <- seleccionar("calificaciones", 180L)
modificar("calificaciones", "nota", filas,
          rep(c(-3, 25), length.out = length(filas)), "Nota_fuera_de_rango")

filas <- seleccionar("calificaciones", 80L)
modificar("calificaciones", "nota", filas,
          rep(NA_real_, length(filas)), "Dato_faltante")

filas <- seleccionar("calificaciones", 100L)
modificar("calificaciones", "fecha_evaluacion", filas,
          rep("2027-01-15", length(filas)), "Fecha_fuera_de_rango")

# PAGOS: 110 montos negativos, 120 pagos marcados Pagado sin fecha,
# 90 estados desconocidos. No tocamos NA legitimos de pagos vencidos.
filas <- seleccionar("pagos", 110L)
modificar("pagos", "monto", filas,
          rep(-100, length(filas)), "Monto_invalido")

filas <- seleccionar("pagos", 120L,
                    elegibles = which(alterados$pagos$estado_pago == "Pagado"))
modificar("pagos", "fecha_pago", filas,
          rep(NA_character_, length(filas)), "Pago_sin_fecha")

filas <- seleccionar("pagos", 90L)
modificar("pagos", "estado_pago", filas,
          rep("Desconocido", length(filas)), "Estado_invalido")

# ACTIVIDAD VIRTUAL: 350 fechas faltantes, 120 fechas futuras,
# 140 duraciones cero, 160 IDs de estudiante inexistentes y 100 duplicados.
filas <- seleccionar("actividad_virtual", 350L)
modificar("actividad_virtual", "fecha_actividad", filas,
          rep(NA_character_, length(filas)), "Dato_faltante")

filas <- seleccionar("actividad_virtual", 120L)
modificar("actividad_virtual", "fecha_actividad", filas,
          rep("2027-01-15", length(filas)), "Fecha_fuera_de_rango")

filas <- seleccionar("actividad_virtual", 140L)
modificar("actividad_virtual", "duracion_minutos", filas,
          rep(0L, length(filas)), "Duracion_invalida")

filas <- seleccionar("actividad_virtual", 160L)
modificar("actividad_virtual", "estudiante_id", filas,
          rep("EST99999", length(filas)), "Referencia_inexistente")

duplicar_filas("actividad_virtual", 100L)
cat("PASO 5 - Errores en las cinco bases objetivo introducidos.\n")

# PASO 6. CREAR FUNCIONES DE MEDICION DE CALIDAD -----------------------
# Una misma fila puede incumplir dos controles. NO se suman indicadores.
contar_referencias_invalidas <- function(valores, claves_padre) {
  sum(is.na(valores) | !(valores %in% claves_padre))
}

contar_referencias_ambiguas <- function(valores, claves_padre) {
  repetidas <- unique(claves_padre[duplicated(claves_padre)])
  sum(!is.na(valores) & valores %in% repetidas)
}

fecha_parse <- function(x) {
  suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
}

fechas_fuera_rango <- function(x, requeridas = TRUE) {
  fechas <- fecha_parse(x)
  fuera <- (!is.na(fechas) &
            (fechas < as.Date("2024-01-01") | fechas > fecha_corte))
  no_parseables <- !is.na(x) & is.na(fechas)
  if (requeridas) {
    sum(is.na(x) | fuera | no_parseables)
  } else {
    sum(fuera | no_parseables)
  }
}

medir_tabla <- function(datos, tabla) {
  x <- datos[[tabla]]
  n <- nrow(x)
  pk <- claves[[tabla]]
  cols <- campos_obligatorios[[tabla]]
  faltantes <- vapply(cols, function(col) {
    y <- x[[col]]
    is.na(y) | trimws(as.character(y)) == ""
  }, logical(n))

  medidas <- c(
    claves_duplicadas = sum(duplicated(x[[pk]])),
    filas_con_faltantes_obligatorios = sum(rowSums(faltantes) > 0L),
    referencias_invalidas = 0L,
    referencias_ambiguas = 0L,
    formatos_nombre_invalidos = 0L,
    periodos_invalidos = 0L,
    valores_fuera_rango = 0L,
    fechas_invalidas = 0L,
    pagos_incoherentes = 0L,
    relaciones_inconsistentes = 0L,
    estados_invalidos = 0L
  )

  if (tabla == "estudiantes") {
    medidas["referencias_invalidas"] <- contar_referencias_invalidas(
      x$carrera_id, datos$carreras$carrera_id)
    medidas["formatos_nombre_invalidos"] <- sum(
      !is.na(x$nombre) &
        !grepl("^Estudiante simulado [0-9]{5}$", x$nombre)
    )
  }

  if (tabla == "asignaturas") {
    medidas["referencias_invalidas"] <-
      contar_referencias_invalidas(x$carrera_id, datos$carreras$carrera_id) +
      contar_referencias_invalidas(x$docente_id, datos$docentes$docente_id)
  }

  if (tabla == "matriculas") {
    medidas["referencias_invalidas"] <-
      contar_referencias_invalidas(x$estudiante_id, datos$estudiantes$estudiante_id) +
      contar_referencias_invalidas(x$asignatura_id, datos$asignaturas$asignatura_id)
    medidas["referencias_ambiguas"] <- contar_referencias_ambiguas(
      x$estudiante_id, datos$estudiantes$estudiante_id)
    medidas["periodos_invalidos"] <- sum(
      is.na(x$periodo) |
        !grepl("^(2024|2025|2026)-[12]$", x$periodo)
    )
    medidas["fechas_invalidas"] <- fechas_fuera_rango(x$fecha_matricula)
    pos_est <- match(x$estudiante_id, datos$estudiantes$estudiante_id)
    pos_asig <- match(x$asignatura_id, datos$asignaturas$asignatura_id)
    validas <- which(!is.na(pos_est) & !is.na(pos_asig))
    if (length(validas) > 0L) {
      medidas["relaciones_inconsistentes"] <- sum(
        datos$estudiantes$carrera_id[pos_est[validas]] !=
          datos$asignaturas$carrera_id[pos_asig[validas]],
        na.rm = TRUE
      )
    }
  }

  if (tabla == "calificaciones") {
    medidas["referencias_invalidas"] <- contar_referencias_invalidas(
      x$matricula_id, datos$matriculas$matricula_id)
    medidas["referencias_ambiguas"] <- contar_referencias_ambiguas(
      x$matricula_id, datos$matriculas$matricula_id)
    medidas["valores_fuera_rango"] <- sum(
      !is.na(x$nota) & (x$nota < 0 | x$nota > 20))
    medidas["fechas_invalidas"] <- fechas_fuera_rango(x$fecha_evaluacion)
  }

  if (tabla == "pagos") {
    medidas["referencias_invalidas"] <-
      contar_referencias_invalidas(x$matricula_id, datos$matriculas$matricula_id) +
      contar_referencias_invalidas(x$estudiante_id, datos$estudiantes$estudiante_id)
    medidas["referencias_ambiguas"] <-
      contar_referencias_ambiguas(x$matricula_id, datos$matriculas$matricula_id) +
      contar_referencias_ambiguas(x$estudiante_id, datos$estudiantes$estudiante_id)
    medidas["periodos_invalidos"] <- sum(
      is.na(x$periodo) | !grepl("^(2024|2025|2026)-[12]$", x$periodo)
    )
    medidas["valores_fuera_rango"] <- sum(
      !is.na(x$monto) & (x$monto <= 0 | !is.finite(x$monto)))
    medidas["estados_invalidos"] <- sum(
      is.na(x$estado_pago) |
        !(x$estado_pago %in% c("Pagado", "Vencido"))
    )
    medidas["fechas_invalidas"] <-
      fechas_fuera_rango(x$fecha_vencimiento) +
      fechas_fuera_rango(x$fecha_pago, requeridas = FALSE)
    fecha_pago_faltante <- is.na(x$fecha_pago) | trimws(x$fecha_pago) == ""
    medidas["pagos_incoherentes"] <- sum(
      x$estado_pago == "Pagado" & fecha_pago_faltante,
      na.rm = TRUE
    ) + sum(
      x$estado_pago == "Vencido" & !fecha_pago_faltante,
      na.rm = TRUE
    )
    pos <- match(x$matricula_id, datos$matriculas$matricula_id)
    validas <- which(!is.na(pos))
    if (length(validas) > 0L) {
      medidas["relaciones_inconsistentes"] <- sum(
        x$estudiante_id[validas] !=
          datos$matriculas$estudiante_id[pos[validas]] |
        x$periodo[validas] !=
          datos$matriculas$periodo[pos[validas]],
        na.rm = TRUE
      )
    }
  }

  if (tabla == "actividad_virtual") {
    medidas["referencias_invalidas"] <-
      contar_referencias_invalidas(x$matricula_id, datos$matriculas$matricula_id) +
      contar_referencias_invalidas(x$estudiante_id, datos$estudiantes$estudiante_id) +
      contar_referencias_invalidas(x$asignatura_id, datos$asignaturas$asignatura_id)
    medidas["referencias_ambiguas"] <-
      contar_referencias_ambiguas(x$matricula_id, datos$matriculas$matricula_id) +
      contar_referencias_ambiguas(x$estudiante_id, datos$estudiantes$estudiante_id)
    medidas["valores_fuera_rango"] <- sum(
      !is.na(x$duracion_minutos) & x$duracion_minutos <= 0)
    medidas["fechas_invalidas"] <- fechas_fuera_rango(x$fecha_actividad)
    pos <- match(x$matricula_id, datos$matriculas$matricula_id)
    validas <- which(!is.na(pos))
    if (length(validas) > 0L) {
      medidas["relaciones_inconsistentes"] <- sum(
        x$estudiante_id[validas] !=
          datos$matriculas$estudiante_id[pos[validas]] |
        x$asignatura_id[validas] !=
          datos$matriculas$asignatura_id[pos[validas]],
        na.rm = TRUE
      )
    }
  }

  data.frame(
    tabla = tabla,
    indicador = names(medidas),
    total_registros = rep(n, length(medidas)),
    casos = as.integer(medidas),
    porcentaje = round(100 * as.numeric(medidas) / n, 3),
    stringsAsFactors = FALSE
  )
}

medir_todo <- function(datos, escenario) {
  piezas <- lapply(nombres_tablas, function(tabla) {
    datos_tabla <- medir_tabla(datos, tabla)
    datos_tabla$escenario <- escenario
    datos_tabla
  })
  do.call(rbind, piezas)
}

indicadores_antes <- medir_todo(originales, "Original_sin_errores_intencionales")
indicadores_despues <- medir_todo(alterados, "Con_errores_simulados")
indicadores <- rbind(indicadores_antes, indicadores_despues)
indicadores <- indicadores[
  , c("escenario", "tabla", "indicador", "total_registros",
      "casos", "porcentaje")
]
rownames(indicadores) <- NULL
cat("PASO 6 - Indicadores de calidad calculados para ambos escenarios.\n")

# PASO 7. VALIDAR LOS RESULTADOS ANTES DE CREAR ARCHIVOS ------------------
incidencias <- do.call(rbind, registro$eventos)
rownames(incidencias) <- NULL

validar(nrow(alterados$estudiantes) == 15120L,
        "No se generaron los 120 duplicados de estudiantes.")
validar(nrow(alterados$matriculas) == 30100L,
        "No se generaron los 100 duplicados de matriculas.")
validar(nrow(alterados$actividad_virtual) == 100100L,
        "No se generaron los 100 duplicados de actividad virtual.")
validar(sum(duplicated(alterados$estudiantes$estudiante_id)) == 120L,
        "Error en duplicados de estudiantes.")
validar(sum(duplicated(alterados$matriculas$matricula_id)) == 100L,
        "Error en duplicados de matriculas.")
validar(sum(duplicated(alterados$actividad_virtual$actividad_id)) == 100L,
        "Error en duplicados de actividad virtual.")
validar(sum(is.na(alterados$actividad_virtual$fecha_actividad)) == 350L,
        "Las fechas faltantes simuladas no coinciden con lo esperado.")
validar(sum(is.na(alterados$calificaciones$nota)) == 80L,
        "Las notas faltantes simuladas no coinciden con lo esperado.")
validar(nrow(incidencias) == 2620L,
        "La bitacora no contiene las 2620 incidencias previstas.")
validar(sum(is.na(alterados$estudiantes$nombre)) == 180L,
        "No se generaron los 180 nombres faltantes.")
validar(sum(alterados$pagos$monto < 0, na.rm = TRUE) == 110L,
        "No se generaron los 110 montos negativos.")
cat("PASO 7 - Control de incidencias: ", nrow(incidencias), " eventos.\n", sep="")

# Importante: no borrar, mover ni sobrescribir ningun original.
md5_revision <- unname(tools::md5sum(rutas_fuente))
validar(identical(md5_antes, md5_revision),
        "Algun archivo original fue modificado durante la simulacion.")
cat("PASO 7 - Simulacion validada. Los CSV originales estan intactos.\n")

# PASO 8. GENERAR ARCHIVOS EN CARPETAS INDEPENDIENTES ------------------
dir.create(ruta_alterados, showWarnings = FALSE, recursive = TRUE)
dir.create(ruta_informes, showWarnings = FALSE, recursive = TRUE)

for (tabla in nombres_tablas) {
  write.csv(
    alterados[[tabla]],
    file.path(ruta_alterados, paste0(tabla, ".csv")),
    row.names = FALSE, fileEncoding = "UTF-8", na = ""
  )
}

write.csv(incidencias,
          file.path(ruta_informes, "incidencias_simuladas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8", na = "")
write.csv(indicadores,
          file.path(ruta_informes, "indicadores_calidad.csv"),
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

# PASO 9. INFORME COMPARATIVO Y PRUEBA DE INTEGRIDAD ----------------------
antes <- indicadores[indicadores$escenario == "Original_sin_errores_intencionales",
                     c("tabla", "indicador", "total_registros", "casos", "porcentaje")]
despues <- indicadores[indicadores$escenario == "Con_errores_simulados",
                       c("tabla", "indicador", "total_registros", "casos", "porcentaje")]
comparacion <- merge(antes, despues, by = c("tabla", "indicador"),
                     suffixes = c("_original", "_con_errores"), sort = FALSE)
comparacion$diferencia_casos <- comparacion$casos_con_errores -
  comparacion$casos_original
write.csv(comparacion,
          file.path(ruta_informes, "comparacion_calidad.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

md5_despues <- unname(tools::md5sum(rutas_fuente))
integridad <- data.frame(
  archivo_original = basename(rutas_fuente),
  md5_antes = as.character(md5_antes),
  md5_despues = as.character(md5_despues),
  archivo_intacto = (md5_antes == md5_despues),
  stringsAsFactors = FALSE
)
validar(all(integridad$archivo_intacto),
        "Atencion: un archivo original cambio en disco.")
write.csv(integridad,
          file.path(ruta_informes, "integridad_originales.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# PASO 10. RESUMEN FINAL -------------------------------------------------
cat("\n===========================================================\n")
cat("UNIVERSIDAD NOVA - RESULTADOS APARTADO 3.7\n")
cat("===========================================================\n")
resumen_tablas <- data.frame(
  tabla = nombres_tablas,
  registros_originales = vapply(originales, nrow, integer(1)),
  registros_con_errores = vapply(alterados, nrow, integer(1)),
  stringsAsFactors = FALSE
)
print(resumen_tablas, row.names = FALSE)
cat("\nNumero de eventos registrados en la bitacora:", nrow(incidencias), "\n")
cat("\nIncidencias introducidas por tipo:\n")
print(sort(table(incidencias$tipo_error), decreasing = TRUE))
cat("\nArchivos originales intactos:", all(integridad$archivo_intacto), "\n")
cat("\nDatos con errores guardados en:", normalizePath(ruta_alterados), "\n")
cat("Informes de calidad guardados en:", normalizePath(ruta_informes), "\n")
cat("\nPROCESO 3.7 COMPLETADO CORRECTAMENTE.\n")
cat("NOTA: estas son anomalías SIMULADAS, no hallazgos reales.\n")

