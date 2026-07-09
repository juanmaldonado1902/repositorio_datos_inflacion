# Actividad 1: medidas oficiales de inflacion en mexico
# insumo: inpc_general.csv del banco de indicadores del inegi
# contiene indice general, subyacente y no subyacente, ene 2023 a may 2026 (descarga directa)
# la inflacion anual de agosto 2024 requiere el indice de agosto 2023,
# por eso la descarga empieza un anio antes que la grafica

library(tidyverse)

# lectura y limpieza
# el archivo viene en utf-16 y por eso se declara la codificacion
# todo se lee como texto porque los renglones de encabezado y metadatos
# vienen mezclados con los datos y romperian la deteccion de tipos
inpc = read_csv("inpc_general.csv",
                locale = locale(encoding = "UTF-16LE"),
                col_types = cols(.default = col_character())) %>%
  # se toman las columnas por posicion porque los titulos originales son
  # demasiado largos, y se renombran en el mismo paso
  select(periodo = 1, general = 3, subyacente = 4, `no subyacente` = 5) %>%
  # solo sobreviven los renglones con forma de periodo (2023/01),
  # los encabezados y lo demás lo dejamos fuera
  filter(str_detect(periodo, "^\\d{4}/\\d{2}$")) %>%
  # el periodo se convierte en fecha y los indices de texto a numero
  mutate(fecha = ym(periodo), across(general:`no subyacente`, as.numeric)) %>%
  # de formato ancho a largo: una columna de medida y una de indice,
  # asi el calculo y la grafica se escriben una sola vez para las tres series
  pivot_longer(general:`no subyacente`, names_to = "medida", values_to = "indice")

# inflacion anual
# el orden cronologico y el agrupamiento son criticos: sin el group_by, el rezago del primer renglon de una serie tomaria valores de otra

inflacion = inpc %>%
  arrange(medida, fecha) %>%
  group_by(medida) %>%
  # lag(indice, 12) es el indice de doce meses atras dentro de cada serie
  mutate(inflacion = 100 * (indice / lag(indice, 12) - 1)) %>%
  ungroup() %>%
  # se recorta al periodo de la actividad, los primeros doce meses de cada
  # serie salen na porque no tienen anio anterior contra que compararse
  filter(fecha >= ymd("2024-08-01"))

# grafica de las tres medidas con el objetivo de banxico como referencia
# los colores se fijan a mano para que cada medida conserve el suyo
colores = c("general" = "#1A2F5A", "subyacente" = "#0B7A75", "no subyacente" = "#D97706")

ggplot(inflacion, aes(fecha, inflacion, color = medida)) +
  # la linea del objetivo se dibuja primero para que quede debajo de las series
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = colores) +
  labs(title = "Inflacion anual en mexico: general, subyacente y no subyacente",
       subtitle = "agosto 2024 a mayo 2026",
       x = NULL, y = "por ciento anual", color = NULL,
       caption = "fuente: elaboracion propia con indices del inpc, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom")
