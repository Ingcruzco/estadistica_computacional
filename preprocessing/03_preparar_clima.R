#------------------------------------------------------------------------------#
# 03 - PREPARACIÓN DATOS CLIMA
#------------------------------------------------------------------------------#

library(dplyr)
library(lubridate)

# 1. Transformación de variables

weather_clean <- weather %>%
  mutate(
    
    # Convertir timestamp de segundos a fecha
    datetime = as_datetime(
      time_stamp,
      tz = "America/New_York"
    ),
    
    # Extraer fecha y hora
    fecha = as.Date(
      datetime,
      tz = "America/New_York"
    ),
    
    hora = hour(datetime),
    
    # Llave temporal a nivel de hora
    timestamp_hour = floor(
      time_stamp / 3600
    ) * 3600,
    
    # Mantener inicialmente los NA de lluvia
    lluvia_registrada = !is.na(rain)
    
  ) %>%
  arrange(location, time_stamp)


# Resumen de duplicados
duplicados_clima %>%
  summarise(
    combinaciones_duplicadas = n(),
    registros_excedentes = sum(n_registros - 1)
  )

# Verificar correspondencia geográfica
setdiff(
  unique(cab_rides$source),
  unique(weather_clean$location)
)

resumen_clima
duplicados_clima %>% slice_head(n = 10)

setdiff(
  unique(cab_rides$source),
  unique(weather_clean$location)
)

