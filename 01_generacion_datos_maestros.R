
# =======================================================
# PROYECTO BIG DATA - UNIVERSIDAD NOVA
# APARTADO 3: SIMULACION DE DATOS MAESTROS
# Autor: Estudiante de posgrado
# =======================================================

# PASO 1. Configuracion inicial
set.seed(2026)

dir.create("datos_simulados", showWarnings = FALSE)

# PASO 2. Crear facultades y carreras
facultades <- c(
  "Ingenieria y Arquitectura",
  "Ciencias Empresariales",
  "Ciencias de la Salud",
  "Educacion",
  "Humanidades"
)

nombres_carreras <- c(
  "Ingenieria de Sistemas",
  "Ingenieria Civil",
  "Arquitectura",
  "Administracion",
  "Contabilidad",
  "Economia",
  "Enfermeria",
  "Psicologia",
  "Nutricion",
  "Educacion Inicial",
  "Educacion Primaria",
  "Educacion Secundaria",
  "Comunicacion",
  "Lingüistica",
  "Historia"
)

carreras <- data.frame(
  carrera_id = sprintf("CAR%02d", 1:15),
  carrera = nombres_carreras,
  facultad = rep(facultades, each = 3)
)

# PASO 3. Simular 15000 estudiantes
n_estudiantes <- 15000

estudiantes <- data.frame(
  estudiante_id = sprintf(
    "EST%05d", 1:n_estudiantes
  ),
  nombre = sprintf(
    "Estudiante simulado %05d", 1:n_estudiantes
  ),
  documento_simulado = sprintf(
    "SIM%08d", 1:n_estudiantes
  ),
  carrera_id = sample(
    carreras$carrera_id,
    n_estudiantes,
    replace = TRUE
  ),
  anio_ingreso = sample(
    2019:2026,
    n_estudiantes,
    replace = TRUE
  ),
  estado = sample(
    c("Activo", "Inactivo"),
    n_estudiantes,
    replace = TRUE,
    prob = c(0.9, 0.1)
  )
)

# PASO 4. Simular 1200 docentes
n_docentes <- 1200

docentes <- data.frame(
  docente_id = sprintf(
    "DOC%04d", 1:n_docentes
  ),
  nombre = sprintf(
    "Docente simulado %04d", 1:n_docentes
  ),
  facultad = sample(
    facultades,
    n_docentes,
    replace = TRUE
  ),
  categoria = sample(
    c("Principal", "Asociado", "Auxiliar"),
    n_docentes,
    replace = TRUE
  )
)

# PASO 5. Simular 700 administrativos
n_administrativos <- 700

areas <- c(
  "Rectoria",
  "Finanzas",
  "Recursos Humanos",
  "Registro Academico",
  "Tecnologia",
  "Bienestar Universitario"
)

administrativos <- data.frame(
  administrativo_id = sprintf(
    "ADM%04d", 1:n_administrativos
  ),
  nombre = sprintf(
    "Administrativo simulado %04d",
    1:n_administrativos
  ),
  area = sample(
    areas,
    n_administrativos,
    replace = TRUE
  )
)

# PASO 6. Exportar archivos CSV
write.csv(
  carreras,
  "datos_simulados/carreras.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  estudiantes,
  "datos_simulados/estudiantes.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  docentes,
  "datos_simulados/docentes.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  administrativos,
  "datos_simulados/administrativos.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# PASO 7. Verificar cantidad de registros
cat("\nRESUMEN DE DATOS SIMULADOS\n")
cat("Carreras:", nrow(carreras), "\n")
cat("Estudiantes:", nrow(estudiantes), "\n")
cat("Docentes:", nrow(docentes), "\n")
cat("Administrativos:",
    nrow(administrativos), "\n")

# PASO 8. Verificar identificadores unicos
stopifnot(
  !anyDuplicated(estudiantes$estudiante_id),
  !anyDuplicated(docentes$docente_id),
  !anyDuplicated(administrativos$administrativo_id),
  all(estudiantes$carrera_id %in% carreras$carrera_id)
)

cat("\nDatos maestros generados correctamente.\n")



