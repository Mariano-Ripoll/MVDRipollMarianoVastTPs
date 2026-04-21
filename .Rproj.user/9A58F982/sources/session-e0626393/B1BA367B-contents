# PriMer modulo de procesamiento

suppressPackageStartupMessages({
  library(tidyverse)
  library(udpipe)
  library(stopwords)
  library(here)
})

input.file = here("TP2","data", "oea_comunicados_raw.rds")
output_dir = here("TP2", "output")
output_file <- here("TP2", "output", "processed_text.rds")
model_dir <- here("TP2", "data", "models")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Creando directorio: ", output_dir)
}
if (!dir.exists(model_dir)) {
  dir.create(model_dir, recursive = TRUE)
  message("Creando directorio: ", model_dir)
}


comunicados_raw <- read_rds(input_file) #lectura datos brutos

# Limpieza base del texto
comunicados_limpio <- comunicados_raw |>
  mutate(
    texto = str_c(titulo, ". ", cuerpo),
    texto = str_to_lower(texto),
    texto = str_replace_all(texto, "[\\r\\n\\t]+", " "),
    texto = str_replace_all(texto, "[[:punct:]]+", " "),
    texto = str_replace_all(texto, "[[:digit:]]+", " "),
    texto = str_replace_all(texto, "[^[:alnum:]áéíóúüñ\\s]", " "),
    texto = str_squish(texto)
  )

# Cargar modelo udpipe
m_es <- udpipe_download_model(language = "spanish", model_dir = model_dir, overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# Lematizar texto completo
noticias_lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpio$texto,
  doc_id = comunicados_limpio$id
) |>
  as.data.frame() |>
  as_tibble() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

# título
noticias_lemas <- noticias_lemas |>
  left_join(
    comunicados_limpio |> select(id, titulo),
    by = "id"
  )

# Stopwords y selecci'on de palabras relevantes
data("stopwords", package = "stopwords")
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

tokens <- as_tibble(noticias_lemas) |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(lemma = str_to_lower(lemma)) |>
  anti_join(stop_words, by = "lemma") |>
  filter(str_length(lemma) > 2, !str_detect(lemma, "^\\d+$"))

# Documento Procesado por id
processed_documents <- tokens |>
  group_by(id) |>
  summarise(texto_lematizado = str_c(lemma, collapse = " "), .groups = "drop") |>
  right_join(
    comunicados_limpio |> select(id, titulo),
    by = "id"
  ) |>
  mutate(texto_lematizado = replace_na(texto_lematizado, "")) |>
  select(id, titulo, texto_lematizado)

salida <- list(
  processed_documents = processed_documents,
  processed_tokens = tokens
)

attr(salida, "fecha_procesamiento") <- Sys.time()
write_rds(salida, output_file)

message("Guardado: ", output_file)
message("Documentos procesados: ", nrow(processed_documents))
message("Tokens finales: ", nrow(tokens))

