# Métricas y figura final-
# Objetivo:
# La idea es leer los tokens procesados (lemmatizados y filtrados) para armar una DTM en formato matriz base
# Luego calcular la frecuencia absoluta de 5 términos de interes y guardart tabla y figura final en /output

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(here)
})

# Rutas de entrada/salida
input_file <- here("TP2", "output", "processed_text.rds")
output_dir <- here("TP2", "output")

# Crear output primera vez
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
#xtabs para evitar dependencia de tm/cast_dtm (tm me corría mal)
matriz_dtm <- xtabs(n ~ id + lemma, data = frecuencia_tokens)

# Guardamos DTM tipo checkpoint
saveRDS(matriz_dtm, here("TP2", "output", "dtm_oea.rds"))
message("Guardado: TP2/output/dtm_oea.rds")

# 3) Acá defino conjunto amplio de términos candidatos institucionales
# Despues se filtra por los 5 dados más frecuentes
candidatos <- c(
  "democracia", "derecho", "desarrollo", "seguridad", "mujer",
  "estado", "gobierno", "país", "pais", "regional",
  "cooperación", "cooperacion", "interamericano", "justicia", "paz", "violencia"
)

# Frecuencia total por lemma en todo el contenido recabado
freq_global <- frecuencia_tokens |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n), .groups = "drop") |>
  arrange(desc(frecuencia_total))

terminos_de_interes <- freq_global |>
  filter(lemma %in% candidatos) |>
  slice_max(order_by = frecuencia_total, n = 5, with_ties = FALSE) |>
  pull(lemma)

# Como control; por si por algún motivo hubiera menos de 5 en candidatos, elijo que se completa con top global
if (length(terminos_de_interes) < 5) {
  faltan <- 5 - length(terminos_de_interes)
  adicionales <- freq_global |>
    filter(!lemma %in% terminos_de_interes) |>
    slice_head(n = faltan) |>
    pull(lemma)
  terminos_de_interes <- c(terminos_de_interes, adicionales)
}

message("Términos seleccionados: ", paste(terminos_de_interes, collapse = ", "))

# 4) Dar frec total de los 5 terminos seleccionados
freq_5 <- frecuencia_tokens |>
  filter(lemma %in% terminos_de_interes) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n), .groups = "drop") |>
  complete(lemma = terminos_de_interes, fill = list(frecuencia_total = 0)) |>
  arrange(desc(frecuencia_total))

# tabla de frecuencias final
write_rds(freq_5, here("TP2", "output", "frecuencia_5_terminos.rds"))
message("Guardado: TP2/output/frecuencia_5_terminos.rds")

# 5) Figura final 
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