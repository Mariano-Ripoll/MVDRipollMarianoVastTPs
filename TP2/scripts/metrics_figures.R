suppressPackageStartupMessages({
  library(tidyverse)
  library(tidytext)
  library(ggplot2)
  library(here)
})

input_file <- here("TP2", "output", "processed_text.rds")
output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


obj <- read_rds(input_file)
tokens <- obj$processed_tokens

# Frecuencia de tokens por documento
frecuencia_tokens <- tokens |>
  count(id, lemma, name = "n") |>
  arrange(id)

# DTM como matriz (documento x término)
matriz_dtm <- xtabs(n ~ id + lemma, data = frecuencia_tokens)

# Guardamos la DTM
saveRDS(matriz_dtm, here("TP2", "output", "dtm_oea.rds"))
message("Guardado: TP2/output/dtm_oea.rds")

# términos de interés institucional
terminos_de_interes <- c("democracia", "derecho", "desarrollo", "seguridad", "mujer")

# Nos aseguramos de quedarnos solo con columnas existentes en la DTM
terminos_presentes <- intersect(terminos_de_interes, colnames(matriz_dtm))

# Si faltam agregamos con 0 para que siempre haya 5 en el gráfico final
dtm_df <- as.data.frame(matriz_dtm) |>
  rename(id = id, lemma = lemma, n = Freq) |>
  filter(lemma %in% terminos_presentes) |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n), .groups = "drop") |>
  complete(lemma = terminos_de_interes, fill = list(frecuencia_total = 0))

saveRDS(dtm_df, here("TP2", "output", "frecuencia_5_terminos.rds"))

# Gráfico final
g <- ggplot(dtm_df, aes(x = lemma, y = frecuencia_total)) +
  geom_col(fill = "blue") +
  labs(
    title = "Frecuencia de términos de interés en la DTM",
    subtitle = "Comunicados OEA (enero-abril, 2023-2026)",
    x = "Término",
    y = "Frecuencia",
    caption = "Fuente: OEA - Comunicados de prensa"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = g,
  width = 9,
  height = 6,
  dpi = 300
)

message("Figura guardada: TP2/output/frecuencia_terminos.png")
