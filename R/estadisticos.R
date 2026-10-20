# FUNCIONES ESTADÍSTICAS

library(dplyr)
library(tidyr)

resumen_boxplot <- function(datos) {
  
  datos %>%
    filter(!is.na(price)) %>%
    group_by(cab_type) %>%
    summarise(
      n = n(),
      
      q1 = unname(
        quantile(price, 0.25)
      ),
      
      mediana = median(price),
      
      q3 = unname(
        quantile(price, 0.75)
      ),
      
      iqr = IQR(price),
      
      bigote_inferior = min(
        price[price >=
                quantile(price, 0.25) -
                1.5 * IQR(price)]
      ),
      
      bigote_superior = max(
        price[price <=
                quantile(price, 0.75) +
                1.5 * IQR(price)]
      ),
      
      .groups = "drop"
    )
}
