#------------------------------------------------------------------------------#
# 02 - TRANSFORMACIÓN DE DATOS
#------------------------------------------------------------------------------#

# En ambos de nuestros data sets la variable de tiempo, time_stamp, no se 
# encuentra en las mismas unidades de tienmpo. Para efectos de visualización,
# análisis y experiencia de usuario procederemos a transformar y estandarizar 
# las unidades.


# Ademas, creamos variables que permitirán clasificar variables dentro del data set
# ayudandonos a poder filtrar zonas horarias, dias de la semana entre otras.

library(dplyr)
install.packages("lubridate")
library(lubridate)

# 1. Transformación de viajes

cab_rides <- cab_rides %>%
  mutate( datetime = as_datetime(
    time_stamp / 1000,
    tz = "America/New_York"
  ),
  fecha = as.Date(
    datetime,
    tz = "America/New_York"
  ),
  hora = hour(datetime),
  dia_semana = lubridate::wday(
    datetime,
    label = TRUE,
    abbr = FALSE,
    week_start = 1
  ),
  hora_bloque = case_when(
    hora < 6  ~ "Madrugada",
    hora < 12 ~ "Mañana",
    hora < 18 ~ "Tarde",
    TRUE      ~ "Noche"
  ),
  timestamp_hour = floor(
    time_stamp / 1000 / 3600
  ) * 3600,
  cab_type = factor(
    cab_type,
    levels = c("Uber", "Lyft")
  ),
  hora_bloque = factor(
    hora_bloque,
    levels = c(
      "Madrugada",
      "Mañana",
      "Tarde",
      "Noche"
    )
  ),
  
  # Identificación de tarifa dinámica
  tarifa_dinamica = case_when(
    surge_multiplier > 1 ~ "Sí",
    surge_multiplier == 1 ~ "No",
    TRUE ~ NA_character_
  )
  )


# VALIDACIÓN DE TRANSFORMACIONES


# Validar periodo de observación

cab_rides %>%
  summarise(
    fecha_inicio = min(fecha),
    fecha_final = max(fecha),
    dias_distintos = n_distinct(fecha)
  )

# Validar la creación de franjas horarias

cab_rides %>%
  count(hora_bloque) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100, 2
    )
  ) %>%
  arrange(hora_bloque)

# Validar clasificación de tarifa dinámica

cab_rides %>%
  count(
    cab_type,
    tarifa_dinamica
  ) %>%
  group_by(cab_type) %>%
  mutate(
    porcentaje = round(
      n / sum(n) * 100, 2
    )
  ) %>%
  ungroup()

