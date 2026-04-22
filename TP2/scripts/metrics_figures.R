# Métricas y figura final-
# Objetivo:
# 1) Leer los tokens procesados (lemmatizados y filtrados)
# 2) Construir la DTM (Document-Term Matrix) en formato matriz base
# 3) Elegir 5 términos institucionales presentes en el corpus
# 4) Calcular frecuencia total de esos 5 términos
# 5) Guardar tabla y figura final en /output

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(here)
})

# Rutas de entrada/salida
input_file <- here("TP2", "output", "processed_text.rds")
output_dir <- here("TP2", "output")

# Creamos output si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Creando directorio: ", output_dir)
}

# Leemos objeto procesado
obj <- readr::read_rds(input_file)
tokens <- obj$processed_tokens

# 1) Frecuencia de tokens por documento (id, lemma, n)
frecuencia_tokens <- tokens |>
  count(id, lemma, name = "n") |>
  arrange(id)

# 2) DTM en formato matriz (documento x término)
# Usamos xtabs para evitar dependencia de tm/cast_dtm
matriz_dtm <- xtabs(n ~ id + lemma, data = frecuencia_tokens)

# Guardamos DTM para trazabilidad
saveRDS(matriz_dtm, here("TP2", "output", "dtm_oea.rds"))
message("Guardado: TP2/output/dtm_oea.rds")

# 3) Definimos un conjunto amplio de candidatos institucionales
# Luego nos quedamos con los 5 más frecuentes que efectivamente existan en el corpus
candidatos <- c(
  "democracia", "derecho", "desarrollo", "seguridad", "mujer",
  "estado", "gobierno", "país", "pais", "regional",
  "cooperación", "cooperacion", "interamericano", "justicia", "paz", "violencia"
)

# Frecuencia total por lemma en todo el corpus
freq_global <- frecuencia_tokens |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n), .groups = "drop") |>
  arrange(desc(frecuencia_total))

# Elegimos hasta 5 términos presentes dentro de los candidatos
terminos_de_interes <- freq_global |>
  filter(lemma %in% candidatos) |>
  slice_max(order_by = frecuencia_total, n = 5, with_ties = FALSE) |>
  pull(lemma)

# Si por algún motivo hay menos de 5 en candidatos, completamos con top global
if (length(terminos_de_interes) < 5) {
  faltan <- 5 - length(terminos_de_interes)
  adicionales <- freq_global |>
    filter(!lemma %in% terminos_de_interes) |>
    slice_head(n = faltan) |>
    pull(lemma)
  terminos_de_interes <- c(terminos_de_interes, adicionales)
}

message("Términos seleccionados: ", paste(terminos_de_interes, collapse = ", "))

# 4) Frecuencia total de los 5 términos seleccionados
freq_5 <- frecuencia_tokens |>
  filter(lemma %in% terminos_de_interes) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n), .groups = "drop") |>
  complete(lemma = terminos_de_interes, fill = list(frecuencia_total = 0)) |>
  arrange(desc(frecuencia_total))

# Guardamos tabla de frecuencias final
write_rds(freq_5, here("TP2", "output", "frecuencia_5_terminos.rds"))
message("Guardado: TP2/output/frecuencia_5_terminos.rds")

# 5) Figura final (la que pide la consigna)
g <- ggplot(freq_5, aes(x = reorder(lemma, frecuencia_total), y = frecuencia_total)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    title = "Frecuencia total de 5 términos relevantes",
    subtitle = "Comunicados de prensa OEA (enero-abril 2026)",
    x = "Término",
    y = "Frecuencia total",
    caption = "Fuente: OEA - Centro de Noticias"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = g,
  width = 9,
  height = 6,
  dpi = 300
)

message("Figura guardada: TP2/output/frecuencia_terminos.png")