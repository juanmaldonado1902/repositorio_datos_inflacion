# metodo 3: indice de electricidad por area geografica replicando la
# estructura tarifaria del anexo n del documento metodologico del inpc
# la idea: el metodo 1 promediaba los 38 bloques de consumo con jevons
# sin ponderar, aqui se replica el modelo oficial en cuatro pasos:
#   1. se detecta el verano tarifario de cada ciudad desde sus propios
#      precios (los seis meses consecutivos mas baratos)
#   2. se clasifica la tarifa de cada ciudad (1, 1a a 1f) por la
#      profundidad de su vaiven estacional, que es la huella observable
#      del subsidio de verano
#   3. la tarifa determina el limite dac (cuadro 31) y con el se corta
#      la malla de bloques de cada ciudad en domestico y dac
#   4. los relativos se agregan con media aritmetica ponderada, como
#      manda el anexo: bloques dentro de cada zona, zonas con la
#      particion domestico/dac de la region (cuadro 33), y las regiones
#      al nacional con los ponderadores del cuadro 34
# insumo: maestro_precios_promedio.csv
# salida: series por area y nacional del metodo 3, con base enero 2025 = 100

library(tidyverse)

# lectura de precios de electricidad
# los vacios de estatus regresan como na tras el viaje por csv
precios = read_csv("maestro_precios_promedio.csv",
                   col_types = cols(.default = col_character())) %>%
  mutate(fecha = ymd(fecha), precio = as.numeric(precio),
         estatus = replace_na(estatus, "")) %>%
  filter(clave_generico == "144") %>%
  mutate(kwh = as.numeric(str_extract(especificacion, "\\d+")))

# cuadros del anexo n del documento metodologico
# cuadro 31: limite de alto consumo por tarifa, en kwh mensuales
limites_dac = c("1" = 250, "1A" = 300, "1B" = 400, "1C" = 850,
                "1D" = 1000, "1E" = 2000, "1F" = 2500)

# cuadro 33: participacion domestica y dac por region del inpc
particion = tribble(
  ~region,           ~w_domestica, ~w_dac,
  "frontera norte",   73.83, 26.17,
  "noroeste",         84.90, 15.10,
  "noreste",          62.52, 37.48,
  "centro norte",     66.99, 33.01,
  "centro sur",       72.82, 27.18,
  "sur",              75.38, 24.62,
  "amcm",             62.44, 37.56
)

# cuadro 34: ponderadores regionales del generico electricidad
ponderador_regional = tribble(
  ~region,           ~w_region,
  "frontera norte",   12.51,
  "noroeste",          9.90,
  "noreste",          18.15,
  "centro norte",     18.08,
  "centro sur",       15.34,
  "sur",              12.50,
  "amcm",             13.52
)

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
  "30", "centro norte", "55", "centro norte", "23", "centro norte",
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
  "01", "amcm"
)

# paso 1 y 2: verano tarifario y clasificacion de tarifa por ciudad
# el bloque de referencia es el de 500 kwh, que cae en la zona subsidiada
# de todas las tarifas calidas; los seis meses mas baratos de cada ciudad
# son su verano tarifario y la razon verano entre invierno mide la
# profundidad del subsidio, que es la firma de la tarifa
firma = precios %>%
  filter(kwh == 500) %>%
  group_by(clave_ciudad, ciudad) %>%
  summarise(
    meses_verano = list(fecha[order(precio)][1:6] %>% month() %>% unique()),
    razon = mean(sort(precio)[1:6]) / mean(sort(precio, decreasing = TRUE)[1:6]),
    .groups = "drop"
  )

# los cortes de la clasificacion se leen de la grafica de escalones,
# se proponen valores iniciales y se ajustan si algun escalon los cruza
grafica_escalones = firma %>%
  arrange(razon) %>%
  mutate(orden = row_number()) %>%
  ggplot(aes(orden, razon)) +
  geom_point(color = "#1A2F5A") +
  labs(title = "firma estacional de las 55 areas",
       subtitle = "razon verano entre invierno del bloque de 500 kwh, los escalones son las tarifas",
       x = "areas ordenadas", y = "razon") +
  theme_minimal()
print(grafica_escalones)

firma = firma %>%
  mutate(tarifa = case_when(
    razon >= 0.97 ~ "1",
    razon >= 0.90 ~ "1A",
    razon >= 0.83 ~ "1B",
    razon >= 0.76 ~ "1C",
    razon >= 0.69 ~ "1D",
    razon >= 0.60 ~ "1E",
    TRUE          ~ "1F"
  ),
  limite_dac = limites_dac[tarifa])
print(count(firma, tarifa))

# paso 3: corte de la malla de bloques en domestico y dac por ciudad
# el limite de la tarifa de cada ciudad decide de que lado cae cada bloque
precios_zonas = precios %>%
  left_join(select(firma, clave_ciudad, tarifa, limite_dac), by = "clave_ciudad") %>%
  mutate(zona = if_else(kwh <= limite_dac, "domestica", "dac"))

# paso 4a: relativos por bloque, misma regla de siempre
relativos = precios_zonas %>%
  arrange(clave_ciudad, consecutivo, fecha) %>%
  group_by(clave_ciudad, consecutivo) %>%
  mutate(
    meses_transcurridos = interval(lag(fecha), fecha) %/% months(1),
    relativo = if_else(meses_transcurridos == 1, precio / lag(precio), NA_real_),
    relativo = if_else(coalesce(estatus, "") == "DESENCADENADO", NA_real_, relativo)
  ) %>%
  ungroup()

# paso 4b: media aritmetica de los relativos dentro de cada zona,
# como manda el anexo, con pesos uniformes entre bloques a falta del
# detalle del cuadro 32, supuesto declarado de esta replica
factores_zona = relativos %>%
  group_by(clave_ciudad, ciudad, zona, fecha) %>%
  summarise(factor_zona = mean(relativo, na.rm = TRUE), .groups = "drop") %>%
  mutate(factor_zona = if_else(is.nan(factor_zona), 1, factor_zona))

# paso 4c: combinacion de las zonas con la particion de la region
factores_ciudad = factores_zona %>%
  left_join(regiones, by = "clave_ciudad") %>%
  left_join(particion, by = "region") %>%
  mutate(w_zona = if_else(zona == "domestica", w_domestica, w_dac)) %>%
  group_by(clave_ciudad, ciudad, region, fecha) %>%
  summarise(factor_m3 = sum(factor_zona * w_zona) / sum(w_zona), .groups = "drop")

# encadenamiento y base enero 2025 = 100, igual que el metodo 1
series_m3 = factores_ciudad %>%
  arrange(clave_ciudad, fecha) %>%
  group_by(clave_ciudad, ciudad, region) %>%
  mutate(
    factor_m3 = if_else(fecha == min(fecha), 1, factor_m3),
    encadenado = cumprod(factor_m3),
    indice_m3 = 100 * encadenado / encadenado[fecha == ymd("2025-01-01")]
  ) %>%
  ungroup()

write_csv(series_m3, "~/Desktop/electricidad_metodo3.csv")

# agregado nacional del generico con los ponderadores del cuadro 34
# primero las ciudades de cada region con promedio simple, despues las
# regiones con su ponderador oficial
nacional_m3 = series_m3 %>%
  group_by(region, fecha) %>%
  summarise(indice_region = mean(indice_m3), .groups = "drop") %>%
  left_join(ponderador_regional, by = "region") %>%
  group_by(fecha) %>%
  summarise(indice_nacional = sum(indice_region * w_region) / sum(w_region),
            .groups = "drop")

write_csv(nacional_m3, "~/Desktop/electricidad_nacional_metodo3.csv")

# graficas: las areas con la amcm resaltada y el nacional encimado
grafica_m3 = ggplot() +
  geom_line(data = series_m3, aes(fecha, indice_m3, group = ciudad),
            color = "grey75", linewidth = 0.4) +
  geom_line(data = filter(series_m3, clave_ciudad == "01"),
            aes(fecha, indice_m3), color = "#1A2F5A", linewidth = 1.2) +
  geom_line(data = nacional_m3, aes(fecha, indice_nacional),
            color = "#D97706", linewidth = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  labs(title = "electricidad por area geografica, metodo 3",
       subtitle = "estructura tarifaria del anexo n, enero 2025 = 100, amcm en azul y nacional en ambar",
       x = NULL, y = "indice",
       caption = "fuente: elaboracion propia con precios promedio del inpc y anexo n, inegi") +
  theme_minimal()
print(grafica_m3)
ggsave("~/Desktop/electricidad_metodo3.png", grafica_m3, width = 9, height = 5.5, dpi = 300)
