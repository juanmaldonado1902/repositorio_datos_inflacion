# Inflación actividad 2
#  Este script construye las principales medidas de inflación
#  usando los índices de precios de los 292 genéricos del INPC 2024.
#
#  Medidas que se calculan:
#    1. INPC General (réplica del índice oficial)
#    2. Inflación general (anual y mensual)
#    3. Inflación Subyacente   (238 genéricos)
#    4. Inflación No Subyacente (54 genéricos)
#    5. Trimmed Mean      — media recortada (excluye extremos)
#    6. Median Inflation  — inflación mediana ponderada
#
#  Estructura del archivo de datos (genericos_vinculados.xlsx):
#    - id_generrico   : número del genérico (1–292)
#    - nombre_oficial : nombre completo del genérico
#    - ponderador     : peso en el INPC (debe sumar 100)
#    - subyacente     : "X" si el genérico es subyacente
#    - no_subyacente  : "X" si el genérico es no subyacente
#    - Ene 2023 ... May 2026 : índice de precios mensual 
#    - Factor de encadenamiento    

# paquetes
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "lubridate")) # Necesario en el caso de no haber descargado los paquetes necesarios
library(readxl)    # leer Excel
library(dplyr)     # manipulación de datos
library(tidyr)     # pivotear (wide -> long)
library(ggplot2)   # gráficas
library(scales)    # formato de ejes
library(lubridate) # manejo de fechas

# Carga los datos
# Ajusta la ruta
# Se selecciona solo la columna "ponderador" y las columnas de índice;
RUTA <- "genericos_vinculados.xlsx"

COLS_FECHAS <- c(
  "Ago 2024","Sep 2024","Oct 2024","Nov 2024","Dic 2024",
  "Ene 2025","Feb 2025","Mar 2025","Abr 2025","May 2025","Jun 2025",
  "Jul 2025","Ago 2025","Sep 2025","Oct 2025","Nov 2025","Dic 2025",
  "Ene 2026","Feb 2026","Mar 2026","Abr 2026","May 2026")

datos_raw <- read_excel(RUTA, sheet = "vinculados")  # lee la hoja de vinculados del Excel

# Limpieza
# Los índices vienen como texto en algunas celdas convertir a número.
# Los flags subyacente/no_subyacente vienen como "X" o vacío -a lógico.
# El ponderador se normaliza para que la suma sea exactamente 100, evitando
# pequeños errores de redondeo en el Excel original.
datos <- datos_raw %>%
  select(id_generrico, nombre_oficial, ponderador,            # columnas identificadoras
         subyacente, no_subyacente, all_of(COLS_FECHAS)) %>%  # flags ( son genéricos subyacentes o no por ejmplo) + índices mensuales
  mutate(
    across(all_of(COLS_FECHAS), ~ as.numeric(.x)),  # texto -> número en cada mes
    es_subyacente    = !is.na(subyacente),          # true si la celda tiene "X"
    es_no_subyacente = !is.na(no_subyacente),       # true si la celda tiene "X"
    pond_norm        = ponderador / sum(ponderador, na.rm = TRUE) * 100  # reescala a suma=100
  )

# Índice agregado (Laspeyres)
# El INPC se construye como una media aritmética ponderada de los índices de cada genérico
# Esta función recibe un subconjunto de genéricos (ej. solo subyacentes) y
# devuelve el índice agregado para cada mes disponible.
calcular_indice <- function(df, cols_fechas) {
  w <- df$pond_norm                          # vector de ponderadores normalizados
  sapply(cols_fechas, function(col) {        # repite el cálculo para cada mes
    idx  <- df[[col]]                        # índice de cada genérico en ese mes
    mask <- !is.na(idx)                      # ignorar genéricos sin dato ese mes
    if (sum(mask) == 0) return(NA)           # si no hay ningún dato, regresa NA (robusto)
    sum(w[mask] * idx[mask]) / sum(w[mask])  # media ponderada (fórmula de Laspeyres)
  })
}

# Indice general, subyacente y no subyacente
# Tres índices construidos con la misma fórmula, cambiando solo el
# subconjunto de genéricos que entra al promedio ponderado.
indice_general       <- calcular_indice(datos, COLS_FECHAS)                                # con los 292 genéricos
indice_subyacente    <- calcular_indice(datos %>% filter(es_subyacente),    COLS_FECHAS)  # solo los 238 subyacentes
indice_no_subyacente <- calcular_indice(datos %>% filter(es_no_subyacente), COLS_FECHAS)  # solo los 54 no subyacentes

# Tabla mensual de los tres índices, para comparar directamente contra el
# valor que publica el INEGI en el BIE (Precios -> INPC -> Índices ->
# Total / Subyacente / No subyacente). Útil para detectar en cuál de los
# tres índices empieza una posible divergencia.
cat("══════════════════════════════════════════════════════════════════\n") # Sirven para mostrar de forma más estetica la consola pero nada más
cat(sprintf("  %-12s  %12s  %12s  %12s\n",
            "Mes", "General", "Subyacente", "No subyac."))
cat("══════════════════════════════════════════════════════════════════\n")
for (i in seq_along(COLS_FECHAS)) {
  cat(sprintf("  %-12s  %12.4f  %12.4f  %12.4f\n",
              COLS_FECHAS[i], indice_general[i], indice_subyacente[i], indice_no_subyacente[i]))
}
cat("══════════════════════════════════════════════════════════════════\n\n")

# De indices a inflación
# Inflación anual
# Inflación mensual
inflacion_anual <- function(indice) {
  n <- length(indice)        # número total de meses en la serie
  tasa <- rep(NA, n)         # vector de salida, vacío al inicio
  for (i in 13:n) tasa[i] <- (indice[i] / indice[i - 12] - 1) * 100  # compara contra 12 meses atrás
  tasa
}

inflacion_mensual <- function(indice) {
  n <- length(indice)
  tasa <- rep(NA, n)
  for (i in 2:n) tasa[i] <- (indice[i] / indice[i - 1] - 1) * 100    # compara contra el mes anterior
  tasa
}

# Dataframe con fechas, índices y tasas de inflación
fechas_date <- as.Date(paste0("01 ", COLS_FECHAS), format = "%d %b %Y")  # convierte "Ago 2024" a fecha real

df_series <- tibble(
  fecha             = fechas_date,
  idx_general       = indice_general,
  idx_subyacente    = indice_subyacente,
  idx_no_subyacente = indice_no_subyacente
) %>%
  mutate(
    inf_anual_gral  = inflacion_anual(idx_general),       # inflación anual del índice general
    inf_anual_sub   = inflacion_anual(idx_subyacente),    # inflación anual subyacente
    inf_anual_nosub = inflacion_anual(idx_no_subyacente), # inflación anual no subyacente
    inf_mens_gral   = inflacion_mensual(idx_general)      # inflación mensual del índice general
  )

# Trimmed mean inflation (media recortada)
#
# Elimina los genéricos con las variaciones de precio más extremas (tanto
# las más inflacionarias como las más deflacionarias) antes de promediar,
# reduciendo el efecto de choques temporales puntuales.
#
# Procedimiento para cada mes t:
#   1. Calcular la inflación mensual de cada genérico
#   2. Ordenar los 292 genéricos de menor a mayor
#   3. Acumular los ponderadores en ese orden
#   4. Excluir el α% inferior y el α% superior (por ponderación acumulada)
#   5. Promediar (ponderado) el centro restante
# Se usa α = 8% .
TRIM_PCT <- 0.08   # porcentaje de recorte en cada cola (8% inferior + 8% superior)

trimmed_mean_mensual <- function(df, col_actual, col_anterior) {
  idx_t  <- df[[col_actual]]               # índice de cada genérico en el mes actual
  idx_t1 <- df[[col_anterior]]             # índice de cada genérico en el mes anterior
  pi_k   <- (idx_t / idx_t1 - 1) * 100     # inflación mensual de cada genérico
  w      <- df$pond_norm                   # ponderador de cada genérico

  mask <- !is.na(pi_k) & !is.na(w)   # quitar genéricos sin dato ese mes
  pi_k <- pi_k[mask]
  w    <- w[mask] / sum(w[mask]) * 100   # renormalizar a 100

  orden  <- order(pi_k)            # índices que ordenan de menor a mayor inflación
  pi_ord <- pi_k[orden]            # inflaciones ya ordenadas
  w_acum <- cumsum(w[orden])       # ponderación acumulada en ese orden (0 a 100)

  lim_inf <- TRIM_PCT * 100         # ej. 8
  lim_sup <- (1 - TRIM_PCT) * 100   # ej. 92

  central <- w_acum > lim_inf & w_acum <= lim_sup  # True solo para el centro de la distribución
  if (sum(central) == 0) return(NA)
  sum(pi_ord[central] * w[orden][central]) / sum(w[orden][central])  # media ponderada del centro
}

trim_mensual <- rep(NA, length(COLS_FECHAS))
for (i in 2:length(COLS_FECHAS)) {
  trim_mensual[i] <- trimmed_mean_mensual(datos, COLS_FECHAS[i], COLS_FECHAS[i - 1])  # mes contra mes anterior
}

# Anualizar componiendo 12 tasas mensuales consecutivas:

trim_anual <- rep(NA, length(COLS_FECHAS))
for (i in 13:length(COLS_FECHAS)) {
  meses_12 <- trim_mensual[(i - 11):i]   # ventana de los últimos 12 meses
  if (any(is.na(meses_12))) next         # si falta algún mes, no se puede anualizar
  trim_anual[i] <- (prod(1 + meses_12 / 100) - 1) * 100  # producto de los 12 factores mensuales
}

df_series <- df_series %>%
  mutate(trim_mens = trim_mensual, trim_anual = trim_anual)

# Median inflation (mediana ponderada)
# Es el valor de inflación tal que el 50% de la canasta (medido en
# ponderadores) tiene una inflación menor o igual, y el otro 50% mayor
# o igual. Mismo procedimiento de ordenar + acumular que la trimmed mean,
# pero en vez de promediar el centro se toma el punto donde se cruza 50%.
mediana_ponderada_mensual <- function(df, col_actual, col_anterior) {
  idx_t  <- df[[col_actual]]
  idx_t1 <- df[[col_anterior]]
  pi_k   <- (idx_t / idx_t1 - 1) * 100   # inflación mensual de cada genérico
  w      <- df$pond_norm

  mask <- !is.na(pi_k) & !is.na(w)
  pi_k <- pi_k[mask]
  w    <- w[mask] / sum(w[mask]) * 100

  orden  <- order(pi_k)          # orden ascendente de inflación
  pi_ord <- pi_k[orden]
  w_acum <- cumsum(w[orden])     # ponderación acumulada (0 a 100)

  pi_ord[which(w_acum >= 50)[1]]   # primer valor donde la acumulada llega a 50%
}

mediana_mensual <- rep(NA, length(COLS_FECHAS))
for (i in 2:length(COLS_FECHAS)) {
  mediana_mensual[i] <- mediana_ponderada_mensual(datos, COLS_FECHAS[i], COLS_FECHAS[i - 1])
}

mediana_anual <- rep(NA, length(COLS_FECHAS))
for (i in 13:length(COLS_FECHAS)) {
  meses_12 <- mediana_mensual[(i - 11):i]
  if (any(is.na(meses_12))) next
  mediana_anual[i] <- (prod(1 + meses_12 / 100) - 1) * 100   # mismo método de anualización
}

df_series <- df_series %>%
  mutate(mediana_mens = mediana_mensual, mediana_anual = mediana_anual)

# Resumen
resumen <- df_series %>% filter(!is.na(trim_anual)) %>% tail(1)
cat(sprintf(
  "%s | General: %.2f%% | Trimmed: %.2f%% | Mediana: %.2f%%\n",
  format(resumen$fecha, "%b %Y"),
  resumen$inf_anual_gral, resumen$trim_anual, resumen$mediana_anual))

#  Gráficas
tema_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 11),
    plot.caption     = element_text(color = "gray50", size = 9),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# Grafica única distintas tasas de inflación
# Descomposición clásica del INPC en sus dos grandes componentes.
g1_data <- df_series %>%
  filter(!is.na(inf_anual_gral)) %>%
  select(fecha, inf_anual_gral, inf_anual_sub, inf_anual_nosub) %>%
  pivot_longer(-fecha, names_to = "componente", values_to = "inflacion") %>%
  mutate(componente = recode(componente,
    "inf_anual_gral"  = "General",
    "inf_anual_sub"   = "Subyacente",
    "inf_anual_nosub" = "No subyacente"
  ))

g1 <- ggplot(g1_data, aes(x = fecha, y = inflacion, color = componente)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "gray60") +
  annotate("text", x = min(g1_data$fecha), y = 3.3,
           label = "Meta Banxico: 3%", hjust = 0, size = 3.2, color = "gray50") +
  scale_color_manual(values = c("General" = "#1a1a2e",
                                "Subyacente" = "#16213e",
                                "No subyacente" = "#e94560")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Inflación en México: General, Subyacente y No Subyacente",
    subtitle = "Variación anual (%) | Base: segunda quincena de julio 2018 = 100",
    x = NULL, y = "Inflación anual (%)", color = NULL,
    caption = "Fuente: INEGI — INPC 2024. Cálculo propio con 292 genéricos."
  ) +
  tema_base

print(g1)

# Gráfica 2 : Comparación de medidas alternativas
# Trimmed Mean y Median vs inflación general, para evaluar cuál
# medida resulta más estable frente a choques transitorios.
g2_data <- df_series %>%
  filter(!is.na(trim_anual)) %>%
  select(fecha, inf_anual_gral, trim_anual, mediana_anual) %>%
  pivot_longer(-fecha, names_to = "medida", values_to = "inflacion") %>%
  mutate(medida = recode(medida,
    "inf_anual_gral" = "General",
    "trim_anual"     = "Trimmed Mean (±8%)",
    "mediana_anual"  = "Median Inflation"
  ))

g2 <- ggplot(g2_data, aes(x = fecha, y = inflacion, color = medida, linetype = medida)) +
  geom_line(linewidth = 0.85) +
  geom_hline(yintercept = 3, linetype = "dotted", color = "gray70") +
  scale_color_manual(values = c(
    "General" = "black",
    "Trimmed Mean (±8%)" = "#FF5722", "Median Inflation" = "#4CAF50"
  )) +
  scale_linetype_manual(values = c(
    "General" = "solid",
    "Trimmed Mean (±8%)" = "dashed", "Median Inflation" = "dotdash"
  )) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Medidas Alternativas de Inflación en México",
    subtitle = "Variación anual (%) | Comparación de metodologías",
    x = NULL, y = "Inflación anual (%)", color = NULL, linetype = NULL,
    caption = paste0(
      "Trimmed Mean: recorta 8% de cada cola. ",
      "Median: mediana ponderada mensual anualizada.\n",
      "Fuente: INEGI — INPC 2024. Cálculo propio."
    )
  ) +
  tema_base

print(g2)

# Gráfica : Distribución ponderada de inflaciones mensuales
# Histograma de las inflaciones mensuales de los 292 genéricos en el mes
# más reciente. Cada barra pesa según el ponderador, no según el conteo
# de genéricos  así se ve exactamente lo que recorta la Trimmed Mean y
# dónde cae la Median Inflation.
mes_grafica  <- COLS_FECHAS[length(COLS_FECHAS)]      # último mes disponible
mes_anterior <- COLS_FECHAS[length(COLS_FECHAS) - 1]  # mes previo, para inflación mensual

df_hist <- datos %>%
  mutate(
    # Inflación mensual de cada genérico: (I_t / I_t-1 - 1) * 100
    pi_mensual = (.data[[mes_grafica]] / .data[[mes_anterior]] - 1) * 100,
    # Etiqueta de grupo para colorear las barras del histograma
    grupo = case_when(
      es_subyacente    ~ "Subyacente",
      es_no_subyacente ~ "No subyacente",
      TRUE             ~ "Otro"
    )
  ) %>%
  filter(!is.na(pi_mensual))   # quitar genéricos sin dato ese mes

# Recalcular mediana y límites de recorte para este mes en particular
w_norm  <- df_hist$pond_norm / sum(df_hist$pond_norm) * 100  # ponderador normalizado a 100
orden   <- order(df_hist$pi_mensual)                          # orden ascendente de inflación
pi_ord  <- df_hist$pi_mensual[orden]                          # inflaciones ya ordenadas
w_acum  <- cumsum(w_norm[orden])                              # ponderación acumulada (0-100)

lim_inf <- TRIM_PCT * 100        # límite inferior de recorte (ej. 8)
lim_sup <- (1 - TRIM_PCT) * 100  # límite superior de recorte (ej. 92)

val_mediana  <- pi_ord[which(w_acum >= 50)[1]]      # valor donde la acumulada cruza 50%
val_trim_inf <- pi_ord[which(w_acum >= lim_inf)[1]] # valor donde cruza el límite inferior
val_trim_sup <- pi_ord[which(w_acum >= lim_sup)[1]] # valor donde cruza el límite superior

# El ancho de barra y los saltos del eje x se calculan a partir del rango
# real de los datos de este mes, en vez de usar valores fijos. Esto evita
# que el histograma se vea vacío o con etiquetas amontonadas cuando el
# rango de inflaciones mensuales es muy angosto (como suele pasar mes a mes).
rango_x   <- max(df_hist$pi_mensual) - min(df_hist$pi_mensual)
ancho_bin <- rango_x / 40   # 40 barras a lo largo de todo el rango
saltos_x  <- pretty(df_hist$pi_mensual, n = 8)  # saltos

g3 <- ggplot(df_hist, aes(x = pi_mensual, weight = pond_norm, fill = grupo)) +
  # Histograma ponderado: cada barra suma el ponderador, no el conteo
  geom_histogram(binwidth = ancho_bin, color = "white", alpha = 0.85) +
  # Línea vertical en la mediana ponderada
  geom_vline(aes(xintercept = val_mediana, linetype = "Mediana"),
             color = "#4CAF50", linewidth = 1.1) +
  # Líneas verticales en los límites de recorte de la Trimmed Mean
  geom_vline(aes(xintercept = val_trim_inf, linetype = "Recorte Trimmed Mean"),
             color = "#FF5722", linewidth = 1) +
  geom_vline(aes(xintercept = val_trim_sup, linetype = "Recorte Trimmed Mean"),
             color = "#FF5722", linewidth = 1) +
  # Las leyendas de las líneas van en la leyenda (no como texto sobrepuesto)
  # para evitar que se amontonen cuando los valores están muy cerca entre sí
  scale_linetype_manual(name = NULL, values = c("Mediana" = "solid",
                                                "Recorte Trimmed Mean" = "dashed")) +
  scale_fill_manual(name = NULL, values = c("Subyacente" = "#1565C0",
                                            "No subyacente" = "#e94560")) +
  scale_x_continuous(labels = function(x) paste0(round(x, 2), "%"), breaks = saltos_x) +
  labs(
    title    = paste0("Distribución Ponderada de Inflaciones Mensuales — ", mes_grafica),
    subtitle = paste0(
      "Mediana ponderada: ", round(val_mediana, 3), "% | ",
      "Recorte Trimmed Mean (±", TRIM_PCT * 100, "%): [",
      round(val_trim_inf, 3), "%, ", round(val_trim_sup, 3), "%]"
    ),
    x = "Inflación mensual del genérico (%)",
    y = "Suma de ponderadores (peso en canasta)",
    caption = "Fuente: INEGI — INPC 2024. Cálculo propio con 292 genéricos."
  ) +
  tema_base +
  theme(legend.position = "top")

print(g3)
