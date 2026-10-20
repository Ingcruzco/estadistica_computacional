#------------------------------------------------------------------------------#
# 01 - IMPORTACIÓN DATOS
#------------------------------------------------------------------------------#

library(readr)
library(dplyr)
library(tidyr)

# 2. IMPORTACIÓN DE LOS DATASETS

cab_rides <- read_csv( "data/raw/cab_rides.csv",show_col_types = FALSE)
weather <- read_csv("data/raw/weather.csv",show_col_types = FALSE)

# 3. REVISION DE LOS DATA SETS
dim(cab_rides)
dim(weather)
glimpse(cab_rides)
glimpse(weather)


# ==========================================
# 4. AUDITORÍA DE CALIDAD DE DATOS
# ==========================================

# Creamos funcion para poder auditar los datos y resumir si existen faltantes.

auditar_datos <- function(datos, nombre) {
  
  # 1. Información de las variables
  estructura <- datos %>%
    summarise(
      across(
        everything(),
        ~ class(.x)[1]
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "tipo"
    )
  
  # 2. Cantidad de valores faltantes
  faltantes <- datos %>%
    summarise(
      across(
        everything(),
        ~ sum(is.na(.x))
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "faltantes"
    )
  
  # 3. Cantidad de valores únicos
  unicos <- datos %>%
    summarise(
      across(
        everything(),
        ~ n_distinct(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "valores_unicos"
    )
  
  # 4. Consolidación de la auditoría
  auditoria <- estructura %>%
    left_join(faltantes, by = "variable") %>%
    left_join(unicos, by = "variable") %>%
    mutate(
      dataset = nombre,
      registros = nrow(datos),
      porcentaje_na = round(
        faltantes / registros * 100, 2
      )
    ) %>%
    select(
      dataset,
      variable,
      tipo,
      registros,
      faltantes,
      porcentaje_na,
      valores_unicos
    ) %>%
    arrange(desc(porcentaje_na))
  
  return(auditoria)
}

# Ejecutar auditorías

auditoria_viajes <- cab_rides %>%
  auditar_datos("Viajes")

auditoria_clima <- weather %>%
  auditar_datos("Clima")

# Visualizar resultados

View(auditoria_viajes)
View(auditoria_clima)

