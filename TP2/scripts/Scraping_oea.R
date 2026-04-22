# Celda 1 - Base programatica. 

# install.packages(c("tidyverse","rvest","httr2","xml2","here","robotstxt","udpipe","stopwords","tidytext","tm","ggplot2"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(httr2)
  library(xml2)
  library(here)
  library(robotstxt)
})

#Datos a scrapear
anio_objetivo = 2026
meses_objetivo = 1:4
crawl_delay = 3 

#Directorios: 
data_dir <- here("TP2", "data")
html_dir <- here("TP2", "data", "html")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Creando directorio: ", data_dir)
}
if (!dir.exists(html_dir)) {
  dir.create(html_dir, recursive = TRUE)
  message("Creando directorio: ", html_dir)
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


# Celda 2  - Funciones para hacer el scraping de los datos

#armar URLs de los comunicados mensuales
construir_url_mes =function(mes, anio){ 
  paste0(    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
             mes, "&nAnio=", anio
             )
  }

#Función para leer html con httr2 

leer_html = function(url, delay = 3) { #crawl delay 
  Sys.sleep(delay)
  resp = request(url) |>
    req_user_agent("MVD-TP2-scraper/1.0 (uso academico)") |>
    req_perform()
  
  if (resp_status(resp) >= 400) {
    warning("Error HTTP en: ", url)
    return(NULL)
  }
  read_html(resp_body_string(resp))
}

# Guardar HTML crudo 
guardar_html_raw <- function(url, path_archivo) {
  resp <- request(url) |>
    req_user_agent("MVD-TP2-scraper/1.0 (uso academico)") |>
    req_perform()
  
  write_file(resp_body_string(resp), path_archivo)
}

# Celda 3 - Extracción de links de comunicados en list mensuales: 

# Primero Función madre para la extracción
# En lugar de extraer el título desde cada comunicado individual, lo busco sacar directo del listado mensual, donde el texto del enlace es el título correcto porque con el path del selector del com abierto falla y extrae otra cosa.

extraer_links_mes = function(url_mes, html_dir, timestamp, delay = crawl_delay) {
  message("Leyendo listado mensual: ", url_mes)
  
  # 1) Lee el HTML dl listado mensual
  pagina = leer_html(url_mes, delay = delay)
  if (is.null(pagina)) {
    # Si falla, devolvemos tibble vacío con columnas esperadas
    return(tibble(url = character(0), titulo_listado = character(0)))
  }
  
  # 2) Guardamos el html del listado para reproducibilidad
  # Nada, por si fuera necesario auditar q página fue scrapeada y cuándo
  nombre_archivo = paste0(
    "listado_",
    str_replace_all(url_mes, "[^[:alnum:]]", "_"),
    "_", timestamp, ".html"
  )
  guardar_html_raw(url_mes, file.path(html_dir, nombre_archivo))
  
  # 3) Selector "td a" - Toma los anchors dentro de celdas de tabla
  nodos = pagina |> html_elements("td a")
  
  # Extraemos href (link) y texto visible del enlace (el título)
  links = nodos |>
    html_attr("href") |>
    discard(is.na) |>
    str_squish()
  
  titulos = nodos |>
    html_text2() |>
    str_squish()
  
  # 4) links relativos a absolutos
  # Usamos como base url_mes para conservar correctamente /es/centro_noticias/
  links = ifelse(str_detect(links, "^http"), links, url_absolute(links, url_mes))
  
  # 5) Se arma la tabla y pasa a filtrar solo comunicados de prensa
  out = tibble(
    url = links,
    titulo_listado = titulos
  ) |>
    filter(str_detect(url, "comunicado_prensa\\.asp\\?sCodigo=")) |>
    # Excluimos títulos vacíos o demasiado cortos (ruido)
    filter(titulo_listado != "", nchar(titulo_listado) > 10) |>
    distinct(url, .keep_all = TRUE)
  
  message("Links válidos detectados en el mes: ", nrow(out))
  return(out)
}
#Celda 4 - Extracción  de contenido: 

# extraer título y cuerpo de cada comunicado
# Esta función se dedica exclusivamente a:
# 1) descargar/guardar html individual
# 2) extraer y limpiar el cuerpo textuall del comunicado

extraer_contenido_comunicado <- function(url_comunicado, html_dir, timestamp, delay = 3) {
  message("Procesando comunicado de prensa: ", url_comunicado)
  
  # 1) Lee html del comunicado
  pagina <- leer_html(url_comunicado, delay = delay)
  if (is.null(pagina)) {
    return(tibble(cuerpo = NA_character_))
  }
  
  # 2) Construye identificador de archivo para guardar html
  # Si existe sCodigo en la URL se usa; si no,   ID técnico derivado de la URL
  codigo <- str_extract(url_comunicado, "sCodigo=[^&]+") |>
    str_replace("sCodigo=", "")
  
  codigo <- ifelse(
    is.na(codigo),
    str_sub(str_replace_all(url_comunicado, "[^[:alnum:]]", "_"), 1, 120),
    codigo
  )
  
  # Estandarización: 
  codigo <- str_replace_all(codigo, "[^[:alnum:]_-]", "_")
  
  # 3) Guardado html individual 
  try(
    guardar_html_raw(
      url_comunicado,
      file.path(html_dir, paste0("comunicado_", codigo, "_", timestamp, ".html"))
    ),
    silent = TRUE
  )
  
  # 4) Extraemos párrafos del cuerpo
  # Selector "p" ; contenido principal
  parrafos <- pagina |>
    html_elements("p") |>
    html_text2() |>
    str_squish()
  
  # 5) Filtrado basico de extras
  parrafos <- parrafos[parrafos != ""]
  parrafos <- parrafos[nchar(parrafos) > 40]
  
  # 6) Unifica en un solo cuerpo por comunicado
  cuerpo <- if (length(parrafos) > 0) str_c(parrafos, collapse = " ") else NA_character_
  cuerpo <- str_replace_all(cuerpo, "[\\r\\n\\t]+", " ")
  cuerpo <- str_squish(cuerpo)
  
  return(tibble(cuerpo = cuerpo))
}

# Celda 5 - Pipeline scrapeo (llamado a funciones) y guardado bruto de datos 

# Scrapea links + títulos desde listados mensuales
# Scrapea cuerpo de cada comunicado individual
# Se hace union y guarda en base raw final (id, titulo, cuerpo, url)

permitido = paths_allowed(
  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026",
  bot = "*"
)
message("Permiso robots.txt para la URL: ", permitido)

# Periodo target: enero-abril de 2026
periodo = tidyr::expand_grid(
  anio = anio_objetivo,   
  mes = meses_objetivo    
) |>
  mutate(url_mes = map2_chr(mes, anio, construir_url_mes))

# 1) Scrapeo de links + títulos desde listados mensuales
tabla_links = periodo |>
  mutate(data = map(url_mes, ~ extraer_links_mes(.x, html_dir, timestamp, delay = crawl_delay))) |>
  select(data) |>
  unnest(data) |>
  distinct(url, .keep_all = TRUE)

message("Cantidad total de links únicos: ", nrow(tabla_links))

# Filtro de   seguridad por patrón de URL esperado
tabla_links = tabla_links |>
  filter(str_detect(url, "/es/centro_noticias/comunicado_prensa\\.asp\\?sCodigo="))

message("Cantidad de links luego de filtro de ruta: ", nrow(tabla_links))

# 2) Scrapeo de cuerpos por comunicado
comunicados_raw = tabla_links |>
  mutate(data = map(url, ~ extraer_contenido_comunicado(.x, html_dir, timestamp, delay = crawl_delay))) |>
  unnest(data) |>
  # 3) Arma título final con título del listado 
  mutate(titulo = titulo_listado) |>
  # 4) Limpieza registros incompletos #
  filter(!is.na(titulo), titulo != "", !is.na(cuerpo), cuerpo != "") |>
  # Filtro de ruido (en un momento me estaba extrayendo cualquier cosa)
  filter(!str_detect(str_to_lower(cuerpo), "javascript|please|site map|temporarily unavailable|not found")) |>
  distinct(url, .keep_all = TRUE) |>
  mutate(id = row_number()) |>
  select(id, titulo, cuerpo, url)

# Registrar fecha de descarga
attr(comunicados_raw, "fecha_descarga") <- Sys.time()

# Guardado final raw
write_rds(
  comunicados_raw,
  here("TP2", "data", "oea_comunicados_raw.rds")
)

message("Guardado: TP2/data/oea_comunicados_raw.rds")
message("Total comunicados válidos: ", nrow(comunicados_raw))