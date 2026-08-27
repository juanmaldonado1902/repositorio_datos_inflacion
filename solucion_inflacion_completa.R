# actividad 2 unificada: del precio en pesos al generico y del generico a la inflacion
# insumos, en la raiz del proyecto:
#   genericos_vinculados.xlsx: indices, ponderadores y factores de los 292
#   objeto_gasto.csv: clasificacion de los 292 genericos en los ocho rubros
#   ponderadores_limpio.xlsx: ponderador y factor de encadenamiento por rubro
#   inpc_general.csv: el indice general oficial, para la comparacion
#   maestro_precios_promedio.csv: especificaciones con precio y estatus

library(tidyverse)
library(readxl)

# meses en español para convertir los encabezados de columna en fechas
meses = c(Ene = 1, Feb = 2, Mar = 3, Abr = 4, May = 5, Jun = 6,
          Jul = 7, Ago = 8, Sep = 9, Oct = 10, Nov = 11, Dic = 12)

# carga de los 292 genericos con sus ponderadores
# las columnas de meses se detectan por su forma (ago 2024) para que el
# script no dependa de la ventana exacta del archivo
crudo = read_xlsx("genericos_vinculados.xlsx", sheet = "vinculados")
cols_meses = names(crudo)[str_detect(names(crudo), "^[A-Z][a-z]{2} \\d{4}$")]

genericos = crudo %>%
  transmute(id_generico = id_generrico, nombre = nombre_oficial,
            ponderador, factor_enc = as.numeric(factor_enc),
            es_subyacente = !is.na(subyacente),
            es_no_subyacente = !is.na(no_subyacente),
            across(all_of(cols_meses), as.numeric)) %>%
  pivot_longer(all_of(cols_meses), names_to = "mes_txt", values_to = "indice") %>%
  mutate(fecha = make_date(as.numeric(str_sub(mes_txt, -4)),
                           meses[str_sub(mes_txt, 1, 3)])) %>%
  select(-mes_txt) %>%
  filter(fecha >= ymd("2024-08-01"))

# laspeyres: media ponderada de los indices de un subconjunto de genericos
# cada indice se lleva primero a base segunda quincena de julio 2024 con su
# factor de encadenamiento, porque los ponderadores enigh 2022 operan sobre
# indices que valen 100 en el empalme; sin ese paso el peso efectivo de cada
# generico queda multiplicado por su inflacion acumulada desde 2018
# los pesos se renormalizan sobre los genericos con dato en cada mes
laspeyres = function(df) {
  df %>%
    filter(!is.na(indice), !is.na(factor_enc)) %>%
    group_by(fecha) %>%
    summarise(indice = sum(ponderador * indice / factor_enc) / sum(ponderador),
              .groups = "drop")
}

# inflacion anual: variacion contra el mismo mes del año anterior
inflacion_anual = function(df) {
  df %>%
    arrange(fecha) %>%
    mutate(inflacion = 100 * (indice / lag(indice, 12) - 1))
}

# pregunta 1: el factor de encadenamiento
# Ejericio para ver si la omision produce error
# aleatorio o desviacion sistematica. Esto se ve mejor con las dos versiones
# encimadas: la correcta divide entre el factor y la ingenua no
general_sin_factor = genericos %>%
  filter(!is.na(indice), !is.na(factor_enc)) %>%
  group_by(fecha) %>%
  summarise(indice = sum(ponderador * indice) / sum(ponderador),
            .groups = "drop") %>%
  inflacion_anual()

general_con_factor = genericos %>% laspeyres() %>% inflacion_anual()

# la brecha no oscila alrededor de cero: sesgo es sistematico y no ruido
efecto_factor = general_con_factor %>%
  select(fecha, con_factor = inflacion) %>%
  left_join(select(general_sin_factor, fecha, sin_factor = inflacion),
            by = "fecha") %>%
  filter(!is.na(con_factor)) %>%
  mutate(diferencia = sin_factor - con_factor)

print(efecto_factor)

# Los ocho objetos del gasto
# Cada indice publicado se normaliza con su factor, se  agrega con laspeyres dentro del rubro y se reescala con el factor del
# rubro que es el procedimiento oficial del inpc 2024
objeto = read_csv("objeto_gasto.csv", col_types = cols(.default = col_character()))

factores_rubro = read_xlsx("ponderadores_limpio.xlsx") %>%
  transmute(rubro = sectores, factor_rubro = as.numeric(factor))

# el nombre del vinculado trae la clave del generico como prefijo y puede
# diferir del mapa en puntuacion y acentos, por eso la llave los quita antes de comparar
normalizar_nombre = function(x) {
  x %>% str_remove("^\\d+\\s+") %>%
    str_to_lower() %>%
    stringi::stri_trans_general("latin-ascii") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

con_rubro = genericos %>%
  mutate(llave = normalizar_nombre(nombre)) %>%
  inner_join(objeto %>%
               transmute(llave = normalizar_nombre(nombre_oficial), rubro),
             by = "llave")

# si el empate por nombre pierde genericos el peso deja de sumar cien y el
# agregado sale mal sin avisar, por eso las dos guardas detienen el script (Esto lo pongo por seguridad)
stopifnot(n_distinct(con_rubro$id_generico) == n_distinct(genericos$id_generico))

peso_total = con_rubro %>% distinct(id_generico, ponderador) %>% pull(ponderador) %>% sum()
stopifnot(abs(peso_total - 100) < 0.01)

# tabla de los ocho objetos del gasto
indices_rubro = con_rubro %>%
  filter(!is.na(indice)) %>%
  group_by(rubro, fecha) %>%
  summarise(indice_norm = sum(ponderador * indice / factor_enc) / sum(ponderador),
            .groups = "drop") %>%
  left_join(factores_rubro, by = "rubro") %>%
  mutate(indice = factor_rubro * indice_norm) %>%
  select(rubro, fecha, indice)

tabla_rubros = indices_rubro %>%
  mutate(periodo = format(fecha, "%Y-%m"), indice = round(indice, 3)) %>%
  select(periodo, rubro, indice) %>%
  pivot_wider(names_from = rubro, values_from = indice)

print(tabla_rubros)
write_csv(tabla_rubros, "tabla_objetos_gasto.csv")

# las ocho series en una misma grafica
grafica_objetos = ggplot(indices_rubro, aes(fecha, indice, color = rubro)) +
  geom_line(linewidth = 0.8) +
  labs(title = "inpc por objeto del gasto, construido desde los genericos",
       subtitle = "indices base segunda quincena de julio 2018 = 100",
       x = NULL, y = "indice", color = NULL,
       caption = "fuente: elaboracion propia con indices del inpc, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3))

print(grafica_objetos)
ggsave("inpc_objetos.png", grafica_objetos, width = 9, height = 5.5, dpi = 300)

# el crecimiento acumulado de cada rubro en la ventana ordena la lectura
# de la grafica y separa a los servicios de los bienes comerciables
crecimiento_rubros = indices_rubro %>%
  group_by(rubro) %>%
  summarise(inicio = indice[fecha == min(fecha)],
            fin = indice[fecha == max(fecha)],
            crecimiento = 100 * (fin / inicio - 1),
            .groups = "drop") %>%
  arrange(desc(crecimiento))

print(crecimiento_rubros)

# pregunta 3: la inflacion general regenerada
# el laspeyres re-basado vive en base 2q julio 2024 = 100, y para comparar
# niveles contra el oficial se reescala con el factor de encadenamiento del
# indice general; se intenta leer del archivo de ponderadores y si no trae
# la fila del general se usa el valor oficial del inegi
factor_general = read_xlsx("ponderadores_limpio.xlsx") %>%
  filter(str_detect(str_to_lower(sectores), "general")) %>%
  pull(factor) %>% as.numeric()
if (length(factor_general) == 0) factor_general = 1.3610

# la general desde los 292 genericos
general_construida = genericos %>% laspeyres() %>%
  mutate(indice = factor_general * indice) %>%
  inflacion_anual()

print(general_construida)

# comparacion contra el indice oficial
oficial = read_csv("inpc_general.csv",
                   locale = locale(encoding = "UTF-16LE"),
                   col_types = cols(.default = col_character())) %>%
  select(periodo = 1, indice_oficial = 3) %>%
  filter(str_detect(periodo, "^\\d{4}/\\d{2}")) %>%
  transmute(fecha = ym(str_sub(periodo, 1, 7)),
            indice_oficial = as.numeric(indice_oficial))

comparacion = general_construida %>%
  inner_join(oficial, by = "fecha") %>%
  mutate(brecha = indice - indice_oficial)

# la tendencia sistematica de la brecha se diagnostica con una regresion
# contra el tiempo: una pendiente distinta de cero indica que la diferencia
# crece o se corrige de forma acumulativa y no aleatoria (ilustrativa no se les pide)
tendencia = lm(brecha ~ as.numeric(fecha), data = comparacion)
print(summary(tendencia)$coefficients)

grafica_brecha = ggplot(comparacion, aes(fecha, brecha)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(color = "#1A2F5A", linewidth = 1) +
  labs(title = "brecha entre el inpc construido y el oficial",
       subtitle = "puntos de indice, construido menos oficial",
       x = NULL, y = "puntos de indice",
       caption = "fuente: elaboracion propia con indices del inpc, inegi") +
  theme_minimal()

print(grafica_brecha)
ggsave("brecha_construido_oficial.png", grafica_brecha, width = 9, height = 5.5, dpi = 300)

# subyacente, no subyacente, mediana ponderada y las dos recortadas
subyacente = genericos %>% filter(es_subyacente) %>% laspeyres() %>% inflacion_anual()
no_subyacente = genericos %>% filter(es_no_subyacente) %>% laspeyres() %>% inflacion_anual()

# inflacion mensual de cada generico, insumo de las medidas de distribucion
# aqui no hace falta el factor de encadenamiento porque el cociente contra
# el mes previo se toma dentro de cada generico y la constante se cancela
mensuales = genericos %>%
  arrange(id_generico, fecha) %>%
  group_by(id_generico) %>%
  mutate(inf_mensual = 100 * (indice / lag(indice) - 1)) %>%
  ungroup() %>%
  filter(!is.na(inf_mensual), !is.na(ponderador))

# media recortada mensual: se ordenan las inflaciones, se acumulan los
# pesos y se elimina la fraccion alfa de cada cola antes de promediar
recortada_mensual = function(df, alfa) {
  df %>%
    group_by(fecha) %>%
    arrange(inf_mensual, .by_group = TRUE) %>%
    mutate(acumulado = cumsum(ponderador) / sum(ponderador)) %>%
    filter(acumulado > alfa, acumulado <= 1 - alfa) %>%
    summarise(mensual = sum(inf_mensual * ponderador) / sum(ponderador),
              .groups = "drop")
}

# mediana ponderada mensual: el valor donde el peso acumulado cruza el 50%
mediana_mensual = mensuales %>%
  group_by(fecha) %>%
  arrange(inf_mensual, .by_group = TRUE) %>%
  mutate(acumulado = cumsum(ponderador) / sum(ponderador)) %>%
  filter(acumulado >= 0.5) %>%
  slice(1) %>%
  ungroup() %>%
  select(fecha, mensual = inf_mensual)

# anualizacion componiendo doce tasas mensuales consecutivas
# la suma movil de los logaritmos se hace con stats::filter, que es la
# forma vectorizada de un acumulado de ventana fija sin paquetes extra
# los logaritmos convierten en sumas las multiplicaciones necesarias
anualizar = function(df) {
  df %>%
    arrange(fecha) %>%
    mutate(inflacion = 100 * (exp(as.numeric(
      stats::filter(log1p(mensual / 100), rep(1, 12), sides = 1))) - 1))
}

recortada_8 = recortada_mensual(mensuales, 0.08) %>% anualizar()
recortada_55 = recortada_mensual(mensuales, 0.055) %>% anualizar()
mediana = mediana_mensual %>% anualizar()

# union de las seis medidas
seis_medidas = bind_rows(
  general_construida %>% transmute(fecha, medida = "general", inflacion),
  subyacente %>% transmute(fecha, medida = "subyacente", inflacion),
  no_subyacente %>% transmute(fecha, medida = "no subyacente", inflacion),
  recortada_8 %>% transmute(fecha, medida = "recortada 8%", inflacion),
  recortada_55 %>% transmute(fecha, medida = "recortada 5.5%", inflacion),
  mediana %>% transmute(fecha, medida = "mediana", inflacion)
) %>%
  filter(!is.na(inflacion))

# los valores del periodo 2026
tabla_2026 = seis_medidas %>%
  filter(year(fecha) == 2026) %>%
  mutate(periodo = format(fecha, "%Y-%m"), inflacion = round(inflacion, 2)) %>%
  select(periodo, medida, inflacion) %>%
  pivot_wider(names_from = medida, values_from = inflacion)

print(tabla_2026)
write_csv(tabla_2026, "tabla_2026.csv")

# las seis series en una sola grafica
colores = c("general" = "#1A2F5A", "subyacente" = "#0B7A75",
            "no subyacente" = "#D97706", "recortada 8%" = "#B91C1C",
            "recortada 5.5%" = "#E11D48", "mediana" = "#64748B")

grafica_seis = ggplot(seis_medidas, aes(fecha, inflacion, color = medida)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = colores) +
  labs(title = "seis medidas de inflacion anual en mexico",
       subtitle = "construidas desde los 292 genericos de la canasta 2024, agosto 2025 a mayo 2026",
       x = NULL, y = "por ciento anual", color = NULL,
       caption = "fuente: elaboracion propia con indices del inpc, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafica_seis)
ggsave("seis_medidas.png", grafica_seis, width = 9, height = 5.5, dpi = 300)

# los picos de cada medida
# el mes mas alto y el mas bajo de cada serie ubican los episodios que
# despues se contrastan contra los informes trimestrales de banxico
picos = seis_medidas %>%
  group_by(medida) %>%
  filter(inflacion == max(inflacion) | inflacion == min(inflacion)) %>%
  mutate(tipo = if_else(inflacion == max(inflacion), "maximo", "minimo")) %>%
  ungroup() %>%
  arrange(medida, fecha)

print(picos)

brechas_medidas = seis_medidas %>%
  pivot_wider(names_from = medida, values_from = inflacion) %>%
  mutate(general_menos_mediana = general - mediana,
         general_menos_recortada_8 = general - `recortada 8%`,
         subyacente_menos_no_subyacente = subyacente - `no subyacente`) %>%
  select(fecha, general_menos_mediana, general_menos_recortada_8,
         subyacente_menos_no_subyacente)

print(brechas_medidas)

# los genericos mas volatiles y mas estables
# la volatilidad de cada generico es la desviacion estandar de sus
# inflaciones mensuales en la ventana disponible
volatilidad = mensuales %>%
  group_by(id_generico, nombre) %>%
  summarise(sd_mensual = sd(inf_mensual), .groups = "drop") %>%
  arrange(desc(sd_mensual))

# los seis extremos
extremos = bind_rows(
  volatilidad %>% slice_head(n = 3) %>% mutate(grupo = "mas volatiles"),
  volatilidad %>% slice_tail(n = 3) %>% mutate(grupo = "mas estables")
)

print(extremos)

#  metodo grafico propuesto, las series mensuales de los seis genericos
# extremos en paneles separados; la escala libre deja ver que los volatiles
# se miden en decenas de puntos y los estables en decimas
grafica_extremos = mensuales %>%
  inner_join(select(extremos, id_generico, grupo), by = "id_generico") %>%
  ggplot(aes(fecha, inf_mensual, color = nombre)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
  labs(title = "genericos extremos del inpc: volatilidad de la inflacion mensual",
       subtitle = "los tres mas volatiles y los tres mas estables de la canasta",
       x = NULL, y = "inflacion mensual, por ciento", color = NULL,
       caption = "fuente: elaboracion propia con indices del inpc, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2))

print(grafica_extremos)
ggsave("genericos_extremos.png", grafica_extremos, width = 9, height = 6.5, dpi = 300)

# lectura de precios
# los vacios de estatus regresan como na , se restauran
# para que la condicion de desencadenado no propague na
# el kwh se extrae de la especificacion y solo existe para la electricidad
precios = read_csv("maestro_precios_promedio.csv",
                   col_types = cols(.default = col_character())) %>%
  mutate(fecha = ymd(fecha), precio = as.numeric(precio),
         estatus = replace_na(estatus, "")) %>%
  filter(clave_generico %in% c("022", "144")) %>%
  mutate(kwh = as.numeric(str_extract(especificacion, "\\d+")))

# la columna vertebral de esta parte son los relativos de corto plazo por
# especificacion: el precio de cada mes entre el del mes inmediato anterior
# la llave es ciudad + generico + consecutivo, el relativo solo se forma con
# meses consecutivos y se descarta cuando la especificacion fue sustituida
# ese mes, porque ahi el cociente compararia peras con manzanas
relativos = precios %>%
  arrange(clave_ciudad, clave_generico, consecutivo, fecha) %>%
  group_by(clave_ciudad, clave_generico, consecutivo) %>%
  mutate(
    meses_transcurridos = interval(lag(fecha), fecha) %/% months(1),
    relativo = if_else(meses_transcurridos == 1, precio / lag(precio), NA_real_),
    relativo = if_else(estatus == "DESENCADENADO", NA_real_, relativo)
  ) %>%
  ungroup()

# encadenar y rebasar: recibe factores agregados por grupo y fecha y
# devuelve el indice con base enero 2025 = 100; el primer mes de cada grupo
# arranca la cadena en uno porque no tiene relativo
encadenar = function(df, ...) {
  df %>%
    arrange(..., fecha) %>%
    group_by(...) %>%
    mutate(
      factor = if_else(fecha == min(fecha), 1, factor),
      encadenado = cumprod(factor),
      indice = 100 * encadenado / encadenado[fecha == ymd("2025-01-01")]
    ) %>%
    ungroup() %>%
    select(-factor, -encadenado)
}

#  precios promedio y estatus desencadenado
# Se observa cuantas observaciones trae la
# marca y como se reparten, porque son las que pierden su relativo
desencadenados = precios %>%
  filter(estatus == "DESENCADENADO") %>%
  count(generico, año = year(fecha), name = "observaciones")

print(desencadenados)

# el generico regular por area geografica
# el metodo elemental del inegi es jevons, la media geometrica sin ponderar
# de los relativos dentro de cada area, calculada como la exponencial de la
# media de los logaritmos; los meses sin relativo valido regresan nan y se
# sustituyen por uno, el eslabon neutro de la cadena
factores_jevons = relativos %>%
  group_by(clave_ciudad, ciudad, clave_generico, generico, fecha) %>%
  summarise(factor = exp(mean(log(relativo), na.rm = TRUE)), .groups = "drop") %>%
  mutate(factor = if_else(is.nan(factor), 1, factor))

series_areas = factores_jevons %>%
  encadenar(clave_ciudad, ciudad, clave_generico, generico)

write_csv(series_areas, "series_areas_jevons.csv")

# cada quien resalta su ciudad de nacimiento cambiando la clave
clave_resaltada = "01"

graficar_areas = function(clave, titulo) {
  datos = filter(series_areas, clave_generico == clave)
  ggplot() +
    geom_line(data = datos, aes(fecha, indice, group = ciudad),
              color = "grey75", linewidth = 0.4) +
    geom_line(data = filter(datos, clave_ciudad == clave_resaltada),
              aes(fecha, indice), color = "#1A2F5A", linewidth = 1.2) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
    labs(title = titulo,
         subtitle = "jevons encadenado por area, enero 2025 = 100, ciudad resaltada en azul",
         x = NULL, y = "indice",
         caption = "fuente: elaboracion propia con precios promedio del inpc, inegi") +
    theme_minimal()
}

g_pollo = graficar_areas("022", "pollo por area geografica, metodo regular")

print(g_pollo)
ggsave("pollo_areas.png", g_pollo, width = 9, height = 5.5, dpi = 300)


# la electricidad con la estructura del anexo n, opcional
# cuadros del documento metodologico
# cuadro 31: limite de alto consumo por tarifa, en kwh mensuales
limites_dac = c("1" = 250, "1A" = 300, "1B" = 400, "1C" = 850,
                "1D" = 1000, "1E" = 2000, "1F" = 2500)

# cuadro 33: participacion domestica y dac por region del inpc
# nota: el documento oficial imprime 29.17 para el dac de frontera norte,
# errata que suma 103; el valor consistente es 26.17 y es el que se usa
particion = tribble(
  ~region,           ~w_domestica, ~w_dac,
  "frontera norte",   73.83, 26.17,
  "noroeste",         84.90, 15.10,
  "noreste",          62.52, 37.48,
  "centro norte",     66.99, 33.01,
  "centro sur",       72.82, 27.18,
  "sur",              75.38, 24.62,
  "amcm",             62.44, 37.56)

# cuadro 34: ponderadores regionales del generico electricidad
ponderador_regional = tribble(
  ~region,           ~w_region,
  "frontera norte",   12.51,
  "noroeste",          9.90,
  "noreste",          18.15,
  "centro norte",     18.08,
  "centro sur",       15.34,
  "sur",              12.50,
  "amcm",             13.52)

# regionalizacion del inpc: las 55 areas en las 7 regiones, tomada del
# documento metodologico, la llave es la clave numerica de ciudad
regiones = tribble(
  ~clave_ciudad, ~region,
  "06", "frontera norte", "24", "frontera norte", "27", "frontera norte",
  "07", "frontera norte", "46", "frontera norte", "25", "frontera norte",
  "37", "noroeste", "09", "noroeste", "20", "noroeste",
  "32", "noroeste", "50", "noroeste",
  "19", "noreste", "35", "noreste", "21", "noreste", "53", "noreste",
  "15", "noreste", "36", "noreste", "05", "noreste", "18", "noreste",
  "30", "noreste", "55", "noreste",
  "23", "centro norte",
  "26", "centro norte", "34", "centro norte", "10", "centro norte",
  "04", "centro norte", "44", "centro norte", "29", "centro norte",
  "03", "centro norte", "39", "centro norte", "12", "centro norte",
  "47", "centro sur", "14", "centro sur", "08", "centro sur",
  "31", "centro sur", "52", "centro sur", "33", "centro sur",
  "40", "centro sur", "51", "centro sur", "11", "centro sur",
  "41", "centro sur", "22", "centro sur", "42", "centro sur",
  "16", "centro sur",
  "43", "sur", "13", "sur", "54", "sur", "38", "sur", "45", "sur",
  "49", "sur", "02", "sur", "48", "sur", "28", "sur", "17", "sur",
  "01", "amcm")

# El limite dac por el salto del costo unitario, tarifa por cuadro 31,
# anclas y tabla de la ruptura de la ciudad elegida
# el precio de cada bloque es el recibo mensual completo de consumir esa
# cantidad, de modo que el costo unitario (recibo entre kwh) sube de forma
# suave dentro de la zona subsidiada y se dispara al cruzar el limite de alto
# consumo, cuando el hogar pierde el subsidio y paga costo pleno
# el ultimo bloque antes del mayor salto unitario es el limite de la ciudad
limites = precios %>%
  filter(clave_generico == "144", fecha == max(fecha)) %>%
  arrange(clave_ciudad, kwh) %>%
  group_by(clave_ciudad, ciudad) %>%
  mutate(unitario = precio / kwh,
         salto = unitario / lag(unitario),
         kwh_previo = lag(kwh)) %>%
  filter(!is.na(salto)) %>%
  slice_max(salto, n = 1) %>%
  ungroup() %>%
  transmute(clave_ciudad, ciudad, limite_dac = kwh_previo, salto)

# el limite identifica la tarifa por el cuadro 31, invirtiendo el diccionario
firma = limites %>%
  mutate(tarifa = names(limites_dac)[match(limite_dac, limites_dac)])

# las siete tarifas aparecen porque el salto es visible en toda la malla, a
# diferencia de la firma estacional, que solo distingue las tarifas cuyo
# bloque de referencia cae en la zona subsidiada
print(count(firma, tarifa, limite_dac))

# verificacion con ciudades ancla de tarifa conocida
anclas = firma %>%
  filter(str_detect(str_to_lower(ciudad), "mexicali|cd. de m")) %>%
  select(ciudad, limite_dac, tarifa)

print(anclas)

# tabla de la ruptura domestico/dac: los niveles alrededor del limite de la
# ciudad elegida, con recibo y costo unitario, donde se ve el brinco al dac, cada quien cambia la clave por la de su ciudad
clave_ruptura = "01"
limite_ruptura = limites$limite_dac[limites$clave_ciudad == clave_ruptura]

tabla_ruptura = precios %>%
  filter(clave_generico == "144", fecha == max(fecha),
         clave_ciudad == clave_ruptura) %>%
  arrange(kwh) %>%
  mutate(unitario = precio / kwh,
         posicion = row_number(),
         pos_limite = posicion[kwh == limite_ruptura]) %>%
  filter(abs(posicion - pos_limite) <= 3) %>%
  transmute(nivel_kwh = kwh, recibo = round(precio, 2),
            unitario = round(unitario, 3),
            zona = if_else(kwh <= limite_ruptura, "domestica", "dac"))

print(tabla_ruptura)
write_csv(tabla_ruptura, "tabla_ruptura_dac.csv")

# El limite parte los niveles en zona domestica y dac; media aritmetica
# de los relativos dentro de cada zona y cada zona encadenada en su subindice
llave_dac = firma %>% select(clave_ciudad, limite_dac)

relativos_144 = relativos %>% filter(clave_generico == "144")

relativos_zonas = left_join(relativos_144, llave_dac, by = "clave_ciudad") %>%
  mutate(zona = if_else(kwh <= limite_dac, "domestica", "dac"))

# la agregacion es aritmetica y no geometrica porque asi define el anexo n a
# la electricidad; los pesos son uniformes entre niveles a falta del detalle
factores_zona = relativos_zonas %>%
  group_by(clave_ciudad, ciudad, zona, fecha) %>%
  summarise(factor = mean(relativo, na.rm = TRUE), .groups = "drop") %>%
  mutate(factor = if_else(is.nan(factor), 1, factor))

# cada zona se encadena primero en su propio subindice, como marca el anexo n
# (55 subindices domesticos y 55 dac), y las zonas se combinan despues ya como indices.
series_zona = factores_zona %>%
  encadenar(clave_ciudad, ciudad, zona)

# Combinacion de los subindices de zona con la particion de la region
# (cuadro 33), ya en niveles con base enero 2025 = 100
series_m3 = series_zona %>%
  left_join(regiones, by = "clave_ciudad") %>%
  left_join(particion, by = "region") %>%
  mutate(w_zona = if_else(zona == "domestica", w_domestica, w_dac)) %>%
  group_by(clave_ciudad, ciudad, region, fecha) %>%
  summarise(indice = sum(indice * w_zona) / sum(w_zona), .groups = "drop")

write_csv(series_m3, "electricidad_metodo_anexo_n.csv")

# agregacion de las ciudades de cada region con promedio simple
series_region = series_m3 %>%
  group_by(region, fecha) %>%
  summarise(indice_region = mean(indice), .groups = "drop")

# promedio simple por region y grafica de las siete regiones
grafica_regiones = ggplot(series_region,
                          aes(fecha, indice_region, color = region)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.9) +
  labs(title = "electricidad por region del inpc, estructura del anexo n",
       subtitle = "media aritmetica ponderada por zonas tarifarias, enero 2025 = 100",
       x = NULL, y = "indice", color = NULL,
       caption = "fuente: elaboracion propia con precios promedio del inpc y anexo n, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafica_regiones)
ggsave("electricidad_regiones.png", grafica_regiones, width = 9, height = 5.5, dpi = 300)

# el nacional con los ponderadores regionales del cuadro 34
nacional = series_region %>%
  left_join(ponderador_regional, by = "region") %>%
  group_by(fecha) %>%
  summarise(indice_nacional = sum(indice_region * w_region) / sum(w_region),
            .groups = "drop")

write_csv(nacional, "electricidad_nacional.csv")

# la grafica final de la actividad: las regiones con el nacional encimado
grafica_final = ggplot() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  geom_line(data = series_region,
            aes(fecha, indice_region, color = region), linewidth = 0.7) +
  geom_line(data = nacional, aes(fecha, indice_nacional),
            color = "black", linewidth = 1.3) +
  labs(title = "electricidad: regiones del inpc y nacional",
       subtitle = "ponderadores regionales del cuadro 34, enero 2025 = 100, nacional en negro",
       x = NULL, y = "indice", color = NULL,
       caption = "fuente: elaboracion propia con precios promedio del inpc y anexo n, inegi") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafica_final)
ggsave("electricidad_regiones_nacional.png", grafica_final, width = 9, height = 5.5, dpi = 300)
