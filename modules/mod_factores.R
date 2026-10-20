# Analisis exploratorio bivariado de 04-aplicacion.Rmd
mod_factores_ui <- function(id) {
 ns<-shiny::NS(id)
 shiny::tagList(shiny::h2('Factores del precio'),shiny::p('Tarifa dinámica, bloques horarios, lluvia, distancia y precio por milla.'),
   bslib::layout_columns(col_widths=c(6,6),
    bslib::card(bslib::card_header('Tarifa normal y dinámica por plataforma'),plotly::plotlyOutput(ns('surge'),height='390px')),
    bslib::card(bslib::card_header('Precio mediano por hora'),plotly::plotlyOutput(ns('horas'),height='390px'))),
   bslib::layout_columns(col_widths=c(6,6),
    bslib::card(bslib::card_header('Precio y condiciones de lluvia'),plotly::plotlyOutput(ns('lluvia'),height='390px')),
    bslib::card(bslib::card_header('Distancia y precio: ajuste lineal por empresa'),plotly::plotlyOutput(ns('distancia'),height='390px'))),
   bslib::card(bslib::card_header('Precio por unidad de distancia'),DT::DTOutput(ns('por_milla')),
    shiny::p('Advertencia: las distancias próximas a cero generan cocientes extremos; se excluyen distancias iguales a cero.')),
   bslib::card(shiny::p('Nota metodológica: el Bookdown imputa lluvia no registrada a cero y completa mediciones por hora. Este módulo conserva los NA del RDS actual; las diferencias de resultados climáticos no deben atribuirse al precio.')))
 )
}
mod_factores_server <- function(id,datos) {
 shiny::moduleServer(id,function(input,output,session){
   d <- datos %>% dplyr::filter(!is.na(price),price>0)
   output$surge <- plotly::renderPlotly({
     z<-d %>% dplyr::group_by(cab_type,surge) %>% dplyr::summarise(media=mean(price),mediana=median(price),n=dplyr::n(),.groups='drop')
     p<-plotly::plot_ly(z,x=~surge,y=~mediana,color=~cab_type,colors=unname(ride_blue),type='scatter',mode='markers',marker=list(size=12),text=~paste('n=',n,' Media=',round(media,2)))
     ride_plot(p,'Régimen de tarifa','Precio mediano (USD)')
   })
   output$horas <- plotly::renderPlotly({
     z<-d %>% dplyr::group_by(cab_type,hora) %>% dplyr::summarise(mediana=median(price),n=dplyr::n(),.groups='drop')
     p<-plotly::plot_ly(z,x=~hora,y=~mediana,color=~cab_type,colors=unname(ride_blue),type='scatter',mode='lines+markers')
     ride_plot(p,'Hora local de Boston','Precio mediano (USD)')
   })
   output$lluvia <- plotly::renderPlotly({
     z<-d %>% dplyr::filter(!is.na(lluvia)) %>% dplyr::group_by(cab_type,lluvia) %>% dplyr::summarise(mediana=median(price),n=dplyr::n(),.groups='drop')
     p<-plotly::plot_ly(z,x=~lluvia,y=~mediana,color=~cab_type,colors=unname(ride_blue),type='scatter',mode='markers',marker=list(size=13))
     ride_plot(p,'Condición de lluvia observada','Precio mediano (USD)')
   })
   output$distancia <- plotly::renderPlotly({
     # Igual que Bookdown: lm(price ~ distance), líneas estimadas con todos los registros.
     z <- d %>% dplyr::group_by(cab_type) %>% dplyr::group_modify(~{
       fit<-stats::lm(price~distance,data=.x)
       grid<-data.frame(distance=seq(min(.x$distance),max(.x$distance),length.out=80))
       grid$precio<-stats::predict(fit,newdata=grid)
       grid
     }) %>% dplyr::ungroup()
     p<-plotly::plot_ly(z,x=~distance,y=~precio,color=~cab_type,colors=unname(ride_blue),type='scatter',mode='lines')
     ride_plot(p,'Distancia (millas)','Precio estimado (USD)')
   })
   output$por_milla <- DT::renderDT({
     z<-d %>% dplyr::filter(is.finite(precio_por_distancia)) %>% dplyr::group_by(cab_type) %>%
       dplyr::summarise(n=dplyr::n(),mediana=median(precio_por_distancia),RIQ=IQR(precio_por_distancia),.groups='drop')
     DT::datatable(z,rownames=FALSE,options=list(dom='t')) %>% DT::formatRound(c('mediana','RIQ'),2)
   })
 })
}
