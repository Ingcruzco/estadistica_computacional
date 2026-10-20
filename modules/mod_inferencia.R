# Bookdown 04-aplicacion.Rmd: IC, pruebas globales, categorias equivalentes,
# tarifa dinamica, lluvia, ANOVA, comparaciones por pares y correlaciones.
mod_inferencia_ui <- function(id) {
 ns<-shiny::NS(id)
 shiny::tagList(shiny::h2('Inferencia estadística'),
  shiny::p('Métodos y contrastes heredados de 04-aplicacion.Rmd; resultados precalculados para no recalcular pruebas en cada sesión.'),
  bslib::card(bslib::card_header('Intervalos de confianza del precio medio (95 %)'),DT::DTOutput(ns('ic'))),
  bslib::layout_columns(col_widths=c(6,6),
   bslib::card(bslib::card_header('Uber vs Lyft: t Student/Welch o Wilcoxon'),DT::DTOutput(ns('global'))),
   bslib::card(bslib::card_header('Tarifa dinámica: Lyft'),DT::DTOutput(ns('surge')))),
  bslib::card(bslib::card_header('Categorías equivalentes: diferencias Uber − Lyft, IC 95 %'),
   plotly::plotlyOutput(ns('categorias'),height='380px'),DT::DTOutput(ns('categoria_tabla'))),
  bslib::layout_columns(col_widths=c(6,6),
   bslib::card(bslib::card_header('Lluvia y precio por empresa · Holm'),DT::DTOutput(ns('lluvia'))),
   bslib::card(bslib::card_header('ANOVA de servicios y Levene'),DT::DTOutput(ns('anova')))),
  bslib::card(bslib::card_header('Comparaciones por pares'),shiny::textOutput(ns('metodo_pares')),
   DT::DTOutput(ns('pares'))),
  bslib::card(bslib::card_header('Correlaciones de Pearson'),plotly::plotlyOutput(ns('cor'),height='510px')),
  bslib::card(shiny::p('Advertencia metodológica: las inferencias suponen independencia de registros; este dataset contiene cotizaciones históricas potencialmente relacionadas. Los valores p requieren interpretación prudente. Lluvia y regresión difieren del ETL del Bookdown.'))
 )
}
mod_inferencia_server <- function(id,resultados) {
 shiny::moduleServer(id,function(input,output,session){
  tabla <- function(x,digits=3) {
   y<-as.data.frame(x); y[]<-lapply(y,function(z) if(is.numeric(z)) round(z,digits) else z)
   DT::datatable(y,rownames=FALSE,options=list(pageLength=8,scrollX=TRUE))
  }
  output$ic <- DT::renderDT(tabla(resultados$intervalos))
  output$global <- DT::renderDT(tabla(resultados$comparacion_global,4))
  output$surge <- DT::renderDT(tabla(resultados$surge,4))
  output$categoria_tabla <- DT::renderDT(tabla(resultados$categorias,4))
  output$lluvia <- DT::renderDT(tabla(resultados$lluvia,4))
  output$anova <- DT::renderDT(tabla(resultados$anova,4))
  output$pares <- DT::renderDT(tabla(resultados$pares,4))
  output$metodo_pares <- shiny::renderText(resultados$metodo_pares)
  output$categorias <- plotly::renderPlotly({
   z<-resultados$categorias
   p<-plotly::plot_ly(z,x=~diferencia,y=~categoria,type='scatter',mode='markers',
    marker=list(color='#174A7E',size=10),error_x=list(type='data',symmetric=FALSE,
    array=z$ic_sup-z$diferencia,arrayminus=z$diferencia-z$ic_inf))
   ride_plot(p,'Diferencia Uber − Lyft (USD)','Categoría')
  })
  output$cor <- plotly::renderPlotly({
   z<-resultados$correlaciones
   p<-plotly::plot_ly(x=colnames(z),y=rownames(z),z=z,type='heatmap',zmin=-1,zmax=1,
    colorscale=list(c(0,'#78B9E7'),c(.5,'#FFFFFF'),c(1,'#174A7E')))
   ride_plot(p,NULL,NULL)
  })
 })
}
