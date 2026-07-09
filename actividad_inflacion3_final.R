# series historicas del pollo y la electricidad por area geografica
# construye el indice elemental de cada una de las 55 areas con jevons
# encadenado y lo presenta con dos metodos anclados en enero 2025:
# metodo 1, rebase: enero 2025 = 100, escala propia.
# metodo 2, empalme: la serie hereda el nivel oficial de enero 2025
# y queda expresada en la base 2q julio 2018 = 100
# los dos metodos comparten la misma dinamica, solo cambia el nivel
# insumos:
# maestro_precios_promedio.csv: especificaciones con precio y estatus
# maestro_indices_ciudades.csv: indices oficiales, fuente de las anclas

library(tidyverse)

# lectura de precios
# los vacios de estatus regresan como na tras el viaje por csv,
# se restauran para que la condicion de desencadenado no propague na
precios = read_csv("maestro_precios_promedio.csv",
                   col_types = cols(.default = col_character())) %>%
  mutate(fecha = ymd(fecha), precio = as.numeric(precio),
         estatus = replace_na(estatus, "")) %>%
  filter(clave_generico %in% c("022", "144"))

# relativos de corto plazo por especificacion
# la llave es ciudad + generico + consecutivo, el relativo solo se forma
# con meses consecutivos y se descarta cuando la especificacion fue
# sustituida ese mes (estatus desencadenado)
relativos = precios %>%
  arrange(clave_ciudad, clave_generico, consecutivo, fecha) %>%
  group_by(clave_ciudad, clave_generico, consecutivo) %>%
  mutate(
    meses_transcurridos = interval(lag(fecha), fecha) %/% months(1),
    relativo = if_else(meses_transcurridos == 1, precio / lag(precio), NA_real_),
    relativo = if_else(coalesce(estatus, "") == "DESENCADENADO", NA_real_, relativo)
  ) %>%
  ungroup()

# factor jevons por area, generico y mes
factores = relativos %>%
  group_by(clave_ciudad, ciudad, clave_generico, generico, fecha) %>%
  summarise(factor_jevons = exp(mean(log(relativo), na.rm = TRUE)), .groups = "drop") %>%
  mutate(factor_jevons = if_else(is.nan(factor_jevons), 1, factor_jevons))

# encadenamiento sobre toda la ventana descargada
# la cadena guarda los cambios, el nivel se lo dan los metodos de abajo
cadenas = factores %>%
  arrange(clave_ciudad, clave_generico, fecha) %>%
  group_by(clave_ciudad, ciudad, clave_generico, generico) %>%
  mutate(
    factor_jevons = if_else(fecha == min(fecha), 1, factor_jevons),
    encadenado = cumprod(factor_jevons),
    encadenado_ancla = encadenado[fecha == ymd("2025-01-01")]
  ) %>%
  ungroup()

# normalizacion de nombres de ciudad para empatar con el "maestro" oficial
# las dos fuentes difieren en puntuacion, espacios tras coma, prefijos y
# abreviaturas de estado, la amcm se unifica a mano
normalizar = function(x) {
  x %>% str_to_lower() %>%
    stringi::stri_trans_general("latin-ascii") %>%
    str_replace("area met.*mexico", "amcm") %>%
    str_remove_all("\\.") %>%
    str_replace_all(",", ", ") %>%
    str_remove("^(cd|h) ") %>%
    str_replace("edo de mex$", "mex") %>%
    str_replace("q ?r$|q roo$", "qroo") %>%
    str_squish()
}

# anclas oficiales: el nivel base 2018 de cada ciudad y generico en enero 2025
anclas = read_csv("maestro_indices_ciudades.csv",
                  col_types = cols(.default = col_character())) %>%
  mutate(fecha = ymd(fecha), indice = as.numeric(indice)) %>%
  filter(fecha == ymd("2025-01-01")) %>%
  transmute(ciudad_norm = normalizar(ciudad), generico, ancla = indice)

# los dos metodos sobre la misma cadena
# metodo 1: rebase interno, enero 2025 = 100
# metodo 2: empalme, la fraccion vale 1 en enero 2025 y el ancla oficial
# escala el resto de la serie a la base 2q julio 2018
series = cadenas %>%
  mutate(ciudad_norm = normalizar(ciudad),
         generico = str_to_lower(generico)) %>%
  inner_join(anclas, by = c("ciudad_norm", "generico")) %>%
  mutate(
    indice_m1 = 100 * encadenado / encadenado_ancla,
    indice_m2 = ancla * encadenado / encadenado_ancla
  )

write_csv(series, "~/Desktop/series_areas_dos_metodos.csv")

# graficas
# una por generico y metodo, las 55 areas en gris con la amcm (area metropolitana de la ciudad de méxico) resaltada
graficar = function(clave, columna, titulo, subtitulo, ref = NULL) {
  datos = series %>%
    filter(clave_generico == clave) %>%
    rename(valor = all_of(columna))
  g = ggplot() +
    geom_line(data = datos, aes(fecha, valor, group = ciudad),
              color = "grey75", linewidth = 0.4) +
    geom_line(data = filter(datos, clave_ciudad == "01"),
              aes(fecha, valor), color = "#1A2F5A", linewidth = 1.2) +
    labs(title = titulo, subtitle = subtitulo,
         x = NULL, y = "indice",
         caption = "fuente: elaboracion propia con precios promedio del inpc, inegi") +
    theme_minimal()
  if (!is.null(ref)) g = g + geom_hline(yintercept = ref, linetype = "dashed", color = "grey50")
  g
}

g_pollo_m1 = graficar("022", "indice_m1",
  "pollo por area geografica, metodo 1",
  "rebase interno, enero 2025 = 100, amcm resaltada", ref = 100)
g_elec_m1 = graficar("144", "indice_m1",
  "electricidad por area geografica, metodo 1",
  "rebase interno, enero 2025 = 100, amcm resaltada", ref = 100)
g_pollo_m2 = graficar("022", "indice_m2",
  "pollo por area geografica, metodo 2",
  "empalme al nivel oficial de enero 2025, base 2q julio 2018 = 100, amcm resaltada")
g_elec_m2 = graficar("144", "indice_m2",
  "electricidad por area geografica, metodo 2",
  "empalme al nivel oficial de enero 2025, base 2q julio 2018 = 100, amcm resaltada")

print(g_pollo_m1)
print(g_elec_m1)
print(g_pollo_m2)
print(g_elec_m2)

# grafica combinada por generico: los dos metodos en paneles apilados
# comparten la dinamica y difieren solo en la escala del nivel, por eso
# cada panel lleva su propio eje vertical
graficar_combinada = function(clave, titulo) {
  datos = series %>%
    filter(clave_generico == clave) %>%
    pivot_longer(c(indice_m1, indice_m2), names_to = "metodo", values_to = "valor") %>%
    mutate(metodo = recode(metodo,
      indice_m1 = "metodo 1: rebase, enero 2025 = 100",
      indice_m2 = "metodo 2: empalme, base 2q julio 2018 = 100"))
  ggplot() +
    geom_line(data = datos, aes(fecha, valor, group = ciudad),
              color = "grey75", linewidth = 0.4) +
    geom_line(data = filter(datos, clave_ciudad == "01"),
              aes(fecha, valor), color = "#1A2F5A", linewidth = 1.2) +
    facet_wrap(~ metodo, ncol = 1, scales = "free_y") +
    labs(title = titulo,
         subtitle = "misma dinamica con dos anclajes de nivel, amcm resaltada",
         x = NULL, y = "indice",
         caption = "fuente: elaboracion propia con precios promedio del inpc, inegi") +
    theme_minimal()
}

g_pollo_comb = graficar_combinada("022", "pollo por area geografica, metodos 1 y 2")
g_elec_comb = graficar_combinada("144", "electricidad por area geografica, metodos 1 y 2")

print(g_pollo_comb)
print(g_elec_comb)
