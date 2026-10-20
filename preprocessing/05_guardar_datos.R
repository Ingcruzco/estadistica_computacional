#------------------------------------------------------------------------------#
# 05 - ALMACENAMIENTO DE DATOS
#------------------------------------------------------------------------------#

library(dplyr)

# 1. Crear dataset definitivo
# Conservamos precios faltantes y lluvia NA

dataset_shiny <- viajes_final %>%
  arrange(id)

# 2. Guardar datos procesados

saveRDS(
  object = dataset_shiny,
  file = "data/processed/ride_analytics.rds",
  compress = "xz"
)

# 3. Verificar que existe el archivo

file.exists(
  "data/processed/ride_analytics.rds"
)

# 4. Comprobar lectura

datos_verificacion <- readRDS(
  "data/processed/ride_analytics.rds"
)

stopifnot(
  nrow(datos_verificacion) ==
    nrow(dataset_shiny),
  
  identical(
    datos_verificacion$id,
    dataset_shiny$id
  )
)

# 5. Examinar dimensiones y tamaño

dim(datos_verificacion)

file.info(
  "data/processed/ride_analytics.rds"
)$size / 1024^2

