# Regresion lineal multiple del 04-aplicacion.Rmd:
# log(price) ~ distance + name + surge_multiplier + hora_bloque + lluvia + temp_z + wind_z + humidity_z
mod_modelacion_ui <- function(id) {
 ns<-shiny::NS(id)
 shiny::tagList(shiny::h2('Modelación predictiva'),
  shiny::p('Modelo de regresión log-lineal del Bookdown, sin métodos predictivos nuevos.'),
  bslib::layout_columns(col_widths=c(4,4,4),
   bslib::value_box('R² del ajuste',shiny::textOutput(ns('r2')),theme='#174A7E'),
   bslib::value_box('R² ajustado',shiny::textOutput(ns('r2adj')),theme='#2878B8'),
   bslib::value_box('Observaciones usadas',shiny::textOutput(ns('n')),theme='#0B1F3A')),
  bslib::card(bslib::card_header('Efectos porcentuales e intervalos de confianza (95 %)'),
   plotly::plotlyOutput(ns('coefs'),height='520px'),DT::DTOutput(ns('tabla'))),
  bslib::card(bslib::card_header('Especificación y alcance'),
   shiny::p('log(precio) = distancia + servicio + surge + bloque horario + lluvia + temperatura z + viento z + humedad z.'),
   shiny::p('Los efectos corresponden a asociaciones condicionadas. El modelo fue ajustado sobre los registros disponibles, no incluye partición de prueba externa ni garantiza rendimiento prospectivo. No se presenta un simulador de validación no desarrollado en el Bookdown.')))
 )
}
mod_modelacion_server <- function(id,resultados) {
 shiny::moduleServer(id,function(input,output,session){
  output$r2 <- shiny::renderText(sprintf('%.3f',resultados$metricas$r.squared))
  output$r2adj <- shiny::renderText(sprintf('%.3f',resultados$metricas$adj.r.squared))
  output$n <- shiny::renderText(format(resultados$n_modelo,big.mark='.',scientific=FALSE))
  output$tabla <- DT::renderDT({
   z<-resultados$coefs %>% dplyr::select(term,efecto,ic_inf,ic_sup,p.value)
   DT::datatable(z,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)) %>%
    DT::formatRound(c('efecto','ic_inf','ic_sup'),2) %>% DT::formatSignif('p.value',3)
  })
  output$coefs <- plotly::renderPlotly({
   z<-resultados$coefs %>% dplyr::arrange(efecto)
   z$term<-factor(z$term,levels=z$term)
   p<-plotly::plot_ly(z,x=~efecto,y=~term,type='scatter',mode='markers',marker=list(color='#174A7E',size=9),
      error_x=list(type='data',symmetric=FALSE,array=z$ic_sup-z$efecto,arrayminus=z$efecto-z$ic_inf))
   ride_plot(p,'Cambio porcentual del precio (%)','Factor')
  })
 })
}
