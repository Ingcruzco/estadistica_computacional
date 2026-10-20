#------------------------------------------------------------------------------#
# 04 - INTEGRACIÓN DE VIAJES Y CLIMA
#------------------------------------------------------------------------------#

library(dplyr)
library(data.table)

# 1. Preparar índice de viajes

viajes_index <- cab_rides %>%
  transmute(
    id,
    source,
    trip_ts = time_stamp / 1000
  ) %>%
  as.data.table()

# 2. Preparar mediciones meteorológicas

clima_index <- weather_clean %>%
  transmute(
    location,
    weather_ts = time_stamp,
    weather_observed_ts = time_stamp,
    temp,
    clouds,
    pressure,
    humidity,
    wind,
    rain
  ) %>%
  as.data.table()

# 3. Unión con última medición anterior

clima_asignado <- clima_index[
  viajes_index,
  on = .(
    location = source,
    weather_ts = trip_ts
  ),
  roll = TRUE,
  .(
    id = i.id,
    weather_ts = weather_observed_ts,
    temp = temp,
    clouds = clouds,
    pressure = pressure,
    humidity = humidity,
    wind = wind,
    rain = rain
  )
] %>%
  as_tibble() %>%
  left_join(
    cab_rides %>%
      select(id, time_stamp),
    by = "id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    minutos_diferencia =
      (time_stamp / 1000 - weather_ts) / 60,
    
    clima_valido =
      !is.na(minutos_diferencia) &
      minutos_diferencia >= 0 &
      minutos_diferencia <= 60,
    
    across(
      c(
        weather_ts, temp, clouds,
        pressure, humidity, wind, rain
      ),
      ~ if_else(clima_valido, .x, NA_real_)
    ),
    
    minutos_diferencia = if_else(
      clima_valido,
      minutos_diferencia,
      NA_real_
    )
  ) %>%
  select(-time_stamp)

# 4. Incorporar clima al dataset de viajes

viajes_final <- cab_rides %>%
  left_join(
    clima_asignado,
    by = "id",
    relationship = "one-to-one"
  )



# VALIDACIÓN DE LA INTEGRACIÓN


# 1. Validación de integridad

stopifnot(
  nrow(viajes_final) == nrow(cab_rides),
  n_distinct(viajes_final$id) == nrow(cab_rides),
  !anyNA(viajes_final$id)
)

# 2. Cobertura meteorológica

resumen_integracion <- viajes_final %>%
  summarise(
    total_viajes = n(),
    con_clima = sum(clima_valido),
    sin_clima = sum(!clima_valido),
    cobertura_pct = round(
      mean(clima_valido) * 100, 2
    ),
    diferencia_media_min = mean(
      minutos_diferencia,
      na.rm = TRUE
    ),
    diferencia_max_min = max(
      minutos_diferencia,
      na.rm = TRUE
    )
  )

print(resumen_integracion)

# 3. Cobertura por plataforma

viajes_final %>%
  group_by(cab_type) %>%
  summarise(
    viajes = n(),
    cobertura_pct = round(
      mean(clima_valido) * 100, 2
    ),
    .groups = "drop"
  )



# ==========================================
# 5. VALIDACIÓN FINAL
# ==========================================

validacion_final <- viajes_final %>%
  summarise(
    total_registros = n(),
    
    ids_unicos = n_distinct(id),
    
    precios_faltantes = sum(is.na(price)),
    
    viajes_con_clima = sum(clima_valido),
    
    viajes_sin_clima = sum(!clima_valido),
    
    clima_posterior = sum(
      minutos_diferencia < 0,
      na.rm = TRUE
    ),
    
    clima_fuera_rango = sum(
      minutos_diferencia > 60,
      na.rm = TRUE
    )
  )

print(validacion_final)

# Comprobar integridad de los datos

stopifnot(
  nrow(viajes_final) == 693071,
  n_distinct(viajes_final$id) == 693071,
  sum(is.na(viajes_final$price)) == 55095,
  all(!is.na(viajes_final$clima_valido)),
  all(
    viajes_final$minutos_diferencia[
      viajes_final$clima_valido
    ] >= 0
  ),
  all(
    viajes_final$minutos_diferencia[
      viajes_final$clima_valido
    ] <= 60
  )
)
