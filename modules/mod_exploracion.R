# Analisis descriptivo del 04-aplicacion.Rmd: variables numericas, plataformas,
# servicios, origen/destino y distribucion de precios.
mod_exploracion_ui <- function(id) {
 ns <- shiny::NS(id)
 shiny::tagList(
   shiny::h2('Análisis exploratorio'),
   shiny::p('Distribución de variables numéricas, precios por empresa, servicio y origen/destino · Bookdown §Análisis descriptivo'),
   bslib::card(bslib::card_header('Plataforma'),shiny::selectInput(ns('empresa'),'Empresa',c('Ambas','Uber','Lyft'))),
   bslib::layout_columns(col_widths=c(6,6),
     bslib::card(bslib::card_header('Resumen descriptivo'), DT::DTOutput(ns('descriptivo'))),
     bslib::card(bslib::card_header('Precios por tipo de servicio (mediana y RIQ)'),plotly::plotlyOutput(ns('servicios'),height='480px'))),
   bslib::layout_columns(col_widths=c(6,6),
     bslib::card(bslib::card_header('Distribución del precio'),plotly::plotlyOutput(ns('hist'),height='380px')),
     bslib::card(bslib::card_header('Origen y destino: frecuencias'),plotly::plotlyOutput(ns('zonas'),height='380px')))
 )
}
mod_exploracion_server <- function(id, datos) {
 shiny::moduleServer(id,function(input,output,session){
   d <- shiny::reactive({
     z <- datos
     if(!identical(input$empresa,'Ambas')) z <- dplyr::filter(z,cab_type==input$empresa)
     dplyr::filter(z,!is.na(price))
   })
   output$descriptivo <- DT::renderDT({
     variables <- c('distance','price','surge_multiplier','temp','clouds','pressure','rain','humidity','wind')
     z <- d()
     res <- lapply(intersect(variables,names(z)),function(v){
       x <- z[[v]]; x<-x[is.finite(x)]
       data.frame(variable=v,n=length(x),media=mean(x),desviacion=sd(x),mediana=median(x),RIQ=IQR(x))
     })
     DT::datatable(dplyr::bind_rows(res),rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)) %>% DT::formatRound(c('media','desviacion','mediana','RIQ'),2)
   })
   output$servicios <- plotly::renderPlotly({
     z <- d() %>% dplyr::group_by(cab_type,name) %>% dplyr::summarise(
       mediana=median(price),q1=quantile(price,.25),q3=quantile(price,.75),n=dplyr::n(),.groups='drop') %>%
       dplyr::mutate(servicio=paste(cab_type,name,sep=' · ')) %>% dplyr::arrange(mediana)
     p <- ggplot2::ggplot(z,ggplot2::aes(y=stats::reorder(servicio,mediana),x=mediana,color=cab_type))+
       ggplot2::geom_segment(ggplot2::aes(x=q1,xend=q3,yend=stats::reorder(servicio,mediana)),linewidth=2)+
       ggplot2::geom_point(size=2.4)+ggplot2::scale_color_manual(values=ride_blue)+
       ggplot2::labs(x='Precio (USD)',y=NULL,color='Empresa')+ggplot2::theme_minimal()
     ride_plot(plotly::ggplotly(p),xlab='Mediana y rango intercuartílico (USD)')
   })
   output$hist <- plotly::renderPlotly({
     z <- d() %>% dplyr::mutate(bin=floor(price/2)*2) %>% dplyr::count(cab_type,bin,name='viajes')
     p <- plotly::plot_ly(z,x=~bin,y=~viajes,color=~cab_type,colors=unname(ride_blue),type='bar')
     ride_plot(p,'Precio: intervalos de 2 USD','Número de observaciones')
   })
   output$zonas <- plotly::renderPlotly({
     z <- d() %>% dplyr::count(source,sort=TRUE) %>% dplyr::slice_head(n=12)
     p <- plotly::plot_ly(z,x=~n,y=~reorder(source,n),type='bar',orientation='h',marker=list(color='#174A7E'))
     ride_plot(p,'Observaciones','Origen')
   })
 })
}
