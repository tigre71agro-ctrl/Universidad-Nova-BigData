# =====================================================================
# UNIVERSIDAD NOVA | PROYECTO DEL CURSO BIG DATA
# APARTADO 3.6 - SCRIPT COMPLETO Y CORREGIDO
# Generacion de asignaturas, matriculas, calificaciones, pagos y
# actividad virtual. SOLO R BASE. Datos totalmente simulados.
# Fecha de corte del ejercicio: 18-10-2026.
# =====================================================================

# PASO 1. CONFIGURACION Y COMPROBACION DE ARCHIVOS DE ENTRADA ----------
set.seed(2026)
fecha_corte <- as.Date("2026-10-18")
ruta <- "datos_simulados"

validar <- function(condicion, mensaje) {
  if (!isTRUE(condicion)) stop(mensaje, call. = FALSE)
}

maestros <- c("carreras.csv", "estudiantes.csv", "docentes.csv",
             "administrativos.csv")
validar(dir.exists(ruta),
        paste0("No existe la subcarpeta 'datos_simulados'. Carpeta actual: ",
               getwd(), ". Abre el proyecto y configura su directorio."))
validar(all(file.exists(file.path(ruta, maestros))),
        "Faltan archivos CSV maestros en 'datos_simulados'. No ejecutes de nuevo el primer script si ya existen: comprueba getwd().")
cat("\nPASO 1 - Carpeta actual:", getwd(), "\n")

# PASO 2. LEER Y VALIDAR DATOS MAESTROS -------------------------------
leer_csv <- function(nombre) {
  read.csv(file.path(ruta, nombre), stringsAsFactors = FALSE,
           fileEncoding = "UTF-8")
}

carreras <- leer_csv("carreras.csv")
estudiantes <- leer_csv("estudiantes.csv")
docentes <- leer_csv("docentes.csv")
administrativos <- leer_csv("administrativos.csv")

validar(all(c("carrera_id", "facultad") %in% names(carreras)),
        "El archivo carreras.csv no tiene las columnas esperadas.")
validar(all(c("estudiante_id", "carrera_id", "anio_ingreso") %in%
              names(estudiantes)),
        "El archivo estudiantes.csv no tiene las columnas esperadas.")
validar(all(c("docente_id", "facultad") %in% names(docentes)),
        "El archivo docentes.csv no tiene las columnas esperadas.")
validar("administrativo_id" %in% names(administrativos),
        "El archivo administrativos.csv no tiene la columna esperada.")
validar(nrow(carreras) == 15L && nrow(estudiantes) == 15000L &&
          nrow(docentes) == 1200L && nrow(administrativos) == 700L,
        "Las cantidades de los archivos maestros no coinciden con la primera simulacion.")
validar(!anyNA(estudiantes$estudiante_id) &&
          !anyDuplicated(estudiantes$estudiante_id) &&
          !anyDuplicated(carreras$carrera_id) &&
          !anyDuplicated(docentes$docente_id),
        "Existen identificadores maestros faltantes o duplicados.")
validar(all(estudiantes$carrera_id %in% carreras$carrera_id),
        "Algunos estudiantes tienen carreras inexistentes.")

estudiantes$anio_ingreso <- suppressWarnings(
  as.integer(estudiantes$anio_ingreso)
)
validar(!anyNA(estudiantes$anio_ingreso) &&
          all(estudiantes$anio_ingreso >= 2019L &
              estudiantes$anio_ingreso <= 2026L),
        "Los anios de ingreso de estudiantes estan fuera del rango 2019-2026.")
validar(all(carreras$facultad %in% docentes$facultad),
        "Hay una facultad sin docentes asignados.")
cat("PASO 2 - Datos maestros leidos y verificados.\n")

# PASO 3. CREAR 60 ASIGNATURAS (4 POR CARRERA) ------------------------
n_asignaturas <- 60L
asignaturas <- data.frame(
  asignatura_id = sprintf("ASI%03d", seq_len(n_asignaturas)),
  carrera_id = rep(carreras$carrera_id, each = 4L),
  asignatura = sprintf("Asignatura simulada %03d", seq_len(n_asignaturas)),
  ciclo = rep(1:4, times = nrow(carreras)),
  docente_id = rep(NA_character_, n_asignaturas),
  stringsAsFactors = FALSE
)

facultad_por_asignatura <- carreras$facultad[
  match(asignaturas$carrera_id, carreras$carrera_id)
]
for (i in seq_len(n_asignaturas)) {
  candidatos <- docentes$docente_id[
    docentes$facultad == facultad_por_asignatura[i]
  ]
  validar(length(candidatos) >= 1L,
          "No se encontro docente para una asignatura.")
  asignaturas$docente_id[i] <- candidatos[
    sample.int(length(candidatos), 1L)
  ]
}
cat("PASO 3 - Asignaturas creadas:", nrow(asignaturas), "\n")

# PASO 4. CREAR 30000 MATRICULAS (2 POR ESTUDIANTE) ------------------
n_estudiantes <- nrow(estudiantes)
n_matriculas <- 2L * n_estudiantes
ids_asignaturas <- character(n_matriculas)
periodos_estudiante <- character(n_estudiantes)

for (i in seq_len(n_estudiantes)) {
  posiciones <- (2L * i - 1L):(2L * i)
  opciones <- asignaturas$asignatura_id[
    asignaturas$carrera_id == estudiantes$carrera_id[i]
  ]
  validar(length(opciones) >= 2L,
          "Una carrera tiene menos de dos asignaturas.")
  ids_asignaturas[posiciones] <- opciones[
    sample.int(length(opciones), size = 2L, replace = FALSE)
  ]

  # IMPORTANTE: sample.int() evita el error sample(2026, 1)
  # cuando solo queda un anio posible.
  anio_minimo <- max(2024L, estudiantes$anio_ingreso[i])
  anios_validos <- seq.int(anio_minimo, 2026L)
  anio <- anios_validos[sample.int(length(anios_validos), 1L)]
  semestre <- sample.int(2L, 1L)
  periodos_estudiante[i] <- paste0(anio, "-", semestre)
}

periodos <- rep(periodos_estudiante, each = 2L)
anios <- substr(periodos, 1L, 4L)
semestres <- substr(periodos, 6L, 6L)
fecha_base <- as.Date(paste0(
  anios,
  ifelse(semestres == "1", "-03-01", "-08-01")
))

matriculas <- data.frame(
  matricula_id = sprintf("MAT%06d", seq_len(n_matriculas)),
  estudiante_id = rep(estudiantes$estudiante_id, each = 2L),
  asignatura_id = ids_asignaturas,
  periodo = periodos,
  fecha_matricula = fecha_base +
    sample(0:7, n_matriculas, replace = TRUE),
  estado = "Registrada",
  stringsAsFactors = FALSE
)

# Deteccion temprana: los errores no pasan a otras tablas.
validar(all(grepl("^(2024|2025|2026)-[12]$", matriculas$periodo)),
        "Error: se generaron periodos academicos invalidos.")
validar(!anyNA(matriculas$fecha_matricula) &&
          all(matriculas$fecha_matricula >= as.Date("2024-01-01")) &&
          all(matriculas$fecha_matricula <= fecha_corte),
        "Error: fechas de matricula faltantes o fuera del rango.")
cat("PASO 4 - Matriculas correctas:", nrow(matriculas), "\n")

# PASO 5. CREAR 60000 CALIFICACIONES -------------------------------
# Dos evaluaciones por matricula; escala de 0 a 20.
aptitud_simulada <- rnorm(n_estudiantes, mean = 0, sd = 2)
indice_estudiante_por_matricula <- rep(seq_len(n_estudiantes), each = 2L)
indice_estudiante_por_nota <- rep(indice_estudiante_por_matricula, each = 2L)
n_calificaciones <- 2L * n_matriculas
notas <- round(pmin(20, pmax(0,
  13 + aptitud_simulada[indice_estudiante_por_nota] +
    rnorm(n_calificaciones, mean = 0, sd = 3)
)), digits = 1L)

calificaciones <- data.frame(
  calificacion_id = sprintf("CAL%06d", seq_len(n_calificaciones)),
  matricula_id = rep(matriculas$matricula_id, each = 2L),
  tipo_evaluacion = rep(c("Parcial", "Final"), times = n_matriculas),
  nota = notas,
  fecha_evaluacion = rep(matriculas$fecha_matricula, each = 2L) +
    rep(c(30L, 65L), times = n_matriculas),
  stringsAsFactors = FALSE
)
cat("PASO 5 - Calificaciones creadas:", nrow(calificaciones), "\n")

# PASO 6. CREAR 30000 PAGOS ----------------------------------------
# Un cargo simulado por matricula de asignatura (no una pension total).
estado_pago <- sample(c("Pagado", "Vencido"), n_matriculas,
                      replace = TRUE, prob = c(0.80, 0.20))
fecha_pago <- as.Date(rep(NA_character_, n_matriculas))
esta_pagado <- estado_pago == "Pagado"
fecha_pago[esta_pagado] <- matriculas$fecha_matricula[esta_pagado] +
  sample(0:35, sum(esta_pagado), replace = TRUE)

pagos <- data.frame(
  pago_id = sprintf("PAG%06d", seq_len(n_matriculas)),
  matricula_id = matriculas$matricula_id,
  estudiante_id = matriculas$estudiante_id,
  periodo = matriculas$periodo,
  concepto = "Matricula de asignatura",
  monto = sample(seq(450L, 900L, by = 50L),
                 n_matriculas, replace = TRUE),
  fecha_vencimiento = matriculas$fecha_matricula + 20L,
  fecha_pago = fecha_pago,
  estado_pago = estado_pago,
  stringsAsFactors = FALSE
)
cat("PASO 6 - Pagos creados:", nrow(pagos), "\n")

# PASO 7. CREAR 100000 ACTIVIDADES VIRTUALES ------------------------
# Fechas SIEMPRE entre matricula y 18-10-2026.
n_actividades <- 100000L
pesos <- plogis(
  aptitud_simulada[indice_estudiante_por_matricula] / 2
) + 0.15
indices_matricula <- sample.int(
  n_matriculas, size = n_actividades,
  replace = TRUE, prob = pesos
)
fechas_inicio <- matriculas$fecha_matricula[indices_matricula]
dias_disponibles <- pmin(65L, as.integer(fecha_corte - fechas_inicio))
validar(!anyNA(dias_disponibles) && all(dias_disponibles >= 0L),
        "Error: una matricula tiene fecha posterior al corte.")

# Para cada fila, genera un entero entre 0 y sus dias disponibles.
dias_actividad <- floor(
  runif(n_actividades) * (dias_disponibles + 1L)
)

actividad_virtual <- data.frame(
  actividad_id = sprintf("ACT%06d", seq_len(n_actividades)),
  matricula_id = matriculas$matricula_id[indices_matricula],
  estudiante_id = matriculas$estudiante_id[indices_matricula],
  asignatura_id = matriculas$asignatura_id[indices_matricula],
  fecha_actividad = fechas_inicio + dias_actividad,
  tipo_actividad = sample(
    c("Ingreso_aula", "Consulta_material", "Entrega_tarea", "Participacion_foro"),
    n_actividades, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)
  ),
  duracion_minutos = sample(5:120, n_actividades, replace = TRUE),
  stringsAsFactors = FALSE
)
cat("PASO 7 - Actividades virtuales creadas:",
    nrow(actividad_virtual), "\n")

# PASO 8. CONTROLES DE CALIDAD E INTEGRIDAD -------------------------
validar(nrow(asignaturas) == 60L &&
          nrow(matriculas) == 30000L &&
          nrow(calificaciones) == 60000L &&
          nrow(pagos) == 30000L &&
          nrow(actividad_virtual) == 100000L,
        "ERROR: cantidades incorrectas.")

validar(!anyDuplicated(asignaturas$asignatura_id) &&
          !anyDuplicated(matriculas$matricula_id) &&
          !anyDuplicated(calificaciones$calificacion_id) &&
          !anyDuplicated(pagos$pago_id) &&
          !anyDuplicated(actividad_virtual$actividad_id),
        "ERROR: claves primarias duplicadas.")

validar(!anyDuplicated(
  matriculas[c("estudiante_id", "asignatura_id", "periodo")]
), "ERROR: estudiante matriculado dos veces en la misma asignatura y periodo.")

validar(all(asignaturas$docente_id %in% docentes$docente_id) &&
          all(matriculas$estudiante_id %in% estudiantes$estudiante_id) &&
          all(matriculas$asignatura_id %in% asignaturas$asignatura_id) &&
          all(calificaciones$matricula_id %in% matriculas$matricula_id) &&
          all(pagos$matricula_id %in% matriculas$matricula_id) &&
          all(actividad_virtual$matricula_id %in% matriculas$matricula_id),
        "ERROR: alguna clave foranea no tiene correspondencia.")

carrera_alumno <- estudiantes$carrera_id[
  match(matriculas$estudiante_id, estudiantes$estudiante_id)
]
carrera_curso <- asignaturas$carrera_id[
  match(matriculas$asignatura_id, asignaturas$asignatura_id)
]
validar(all(carrera_alumno == carrera_curso),
        "ERROR: hay estudiantes en cursos de otra carrera.")

anio_ingreso_matriculado <- estudiantes$anio_ingreso[
  match(matriculas$estudiante_id, estudiantes$estudiante_id)
]
validar(all(as.integer(substr(matriculas$periodo, 1L, 4L)) >=
              anio_ingreso_matriculado),
        "ERROR: matriculas anteriores al anio de ingreso.")

validar(all(tabulate(
  match(calificaciones$matricula_id, matriculas$matricula_id),
  nbins = n_matriculas
) == 2L) && all(notas >= 0 & notas <= 20),
"ERROR: notas invalidas o evaluaciones incompletas.")

validar(!anyDuplicated(pagos$matricula_id) &&
          all(pagos$estudiante_id == matriculas$estudiante_id) &&
          all(pagos$periodo == matriculas$periodo) &&
          all(pagos$monto > 0) &&
          all(is.na(pagos$fecha_pago) == (pagos$estado_pago == "Vencido")),
        "ERROR: pagos o fechas de pago incoherentes.")

validar(all(actividad_virtual$estudiante_id ==
              matriculas$estudiante_id[indices_matricula]) &&
          all(actividad_virtual$asignatura_id ==
              matriculas$asignatura_id[indices_matricula]),
        "ERROR: actividad virtual asociada a estudiante o curso incorrecto.")

validar(!anyNA(matriculas$fecha_matricula) &&
          !anyNA(calificaciones$fecha_evaluacion) &&
          !anyNA(actividad_virtual$fecha_actividad) &&
          all(calificaciones$fecha_evaluacion <= fecha_corte) &&
          all(actividad_virtual$fecha_actividad >= fechas_inicio) &&
          all(actividad_virtual$fecha_actividad <= fecha_corte) &&
          all(pagos$fecha_vencimiento <= fecha_corte) &&
          all(pagos$fecha_pago[esta_pagado] <= fecha_corte),
        "ERROR: existen fechas obligatorias faltantes o fuera de rango.")

cat("PASO 8 - TODAS LAS VALIDACIONES SUPERADAS.\n")

# PASO 9. EXPORTAR SOLO DESPUES DE VALIDAR ----------------------------
salidas <- list(
  asignaturas = asignaturas,
  matriculas = matriculas,
  calificaciones = calificaciones,
  pagos = pagos,
  actividad_virtual = actividad_virtual
)
for (nombre in names(salidas)) {
  write.csv(
    salidas[[nombre]], file.path(ruta, paste0(nombre, ".csv")),
    row.names = FALSE, fileEncoding = "UTF-8", na = ""
  )
}
cat("PASO 9 - Cinco archivos CSV guardados en:",
    normalizePath(ruta), "\n")

# PASO 10. RESUMEN FINAL Y VERIFICACION DE ARCHIVOS --------------------
resumen <- data.frame(
  archivo = paste0(names(salidas), ".csv"),
  registros = vapply(salidas, nrow, integer(1)),
  stringsAsFactors = FALSE
)
conteos_guardados <- vapply(
  resumen$archivo,
  function(archivo) nrow(
    read.csv(file.path(ruta, archivo), stringsAsFactors = FALSE)
  ),
  integer(1)
)
validar(all(conteos_guardados == resumen$registros),
        "ERROR: un archivo guardado no conserva la cantidad esperada.")

cat("\n===========================================\n")
cat("RESUMEN FINAL - UNIVERSIDAD NOVA\n")
print(resumen, row.names = FALSE)
cat("Periodos:", paste(sort(unique(matriculas$periodo)),
                        collapse = ", "), "\n")
cat("Fechas NA en matriculas:",
    sum(is.na(matriculas$fecha_matricula)), "\n")
cat("Fechas NA en calificaciones:",
    sum(is.na(calificaciones$fecha_evaluacion)), "\n")
cat("Fechas NA en actividad virtual:",
    sum(is.na(actividad_virtual$fecha_actividad)), "\n")
cat("Fechas NA en pagos sin cancelar (esperado):",
    sum(is.na(pagos$fecha_pago)), "\n")
cat("PROCESO COMPLETADO CORRECTAMENTE\n")
cat("===========================================\n")

