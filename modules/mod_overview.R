# INTERFAZ DASHBOARD

# ==========================================
# RIDE ANALYTICS
# MODULO 01 - OVERVIEW
# ==========================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# ==========================================
# 1. PALETA DE COLORES
# ==========================================

colores <- c(
  "Uber" = "#174A7E",
  "Lyft" = "#78B9E7"
)

color_navy <- "#0B1F3A"
color_orange <- "#F28C45"


# ==========================================
# 2. INTERFAZ DEL MODULO
# ==========================================

mod_overview_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    h2("Overview"),
    
    p(
      "Análisis general de las tarifas de Uber y Lyft."
    ),
    
    # FILTROS
    
    card(
      card_header("Filtros"),
      
      selectInput(
        inputId = ns("plataformas"),
        label = "Plataforma",
        choices = c("Uber", "Lyft"),
        selected = c("Uber", "Lyft"),
        multiple = TRUE
      )
    ),
    
    # INDICADORES
    
    layout_columns(
      col_widths = c(4, 4, 4),
      
      uiOutput(ns("kpi_viajes")),
      
      uiOutput(ns("kpi_precios")),
      
      uiOutput(ns("kpi_promedio"))
    ),
    
    # VISUALIZACIONES
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header(
          "Distribución de precios por plataforma"
        ),
        
        plotlyOutput(
          ns("grafico_promedio"),
          height = "400px"
        )
      ),
      
      card(
        card_header(
          "Resumen de dispersión de precios"
        ),
        
        plotlyOutput(
          ns("grafico_distribucion"),
          height = "400px"
        )
      )
    )
    
  )
}


# ==========================================
# 3. SERVIDOR DEL MODULO
# ==========================================

mod_overview_server <- function(id, datos) {
  
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------
    # 1. FILTRAR DATOS
    # --------------------------------------
    
    datos_filtrados <- reactive({
      
      req(length(input$plataformas) > 0)
      
      datos %>%
        filter(
          cab_type %in% input$plataformas
        )
      
    })
    
    
    # --------------------------------------
    # 2. DATOS VALIDOS DE PRECIOS
    # --------------------------------------
    
    datos_precios <- reactive({
      
      datos_filtrados() %>%
        filter(!is.na(price))
      
    })
    
    
    # --------------------------------------
    # 3. MUESTRA PARA VIOLIN PLOT
    # --------------------------------------
    
    datos_violin <- reactive({
      
      set.seed(42)
      
      datos_precios() %>%
        group_by(cab_type) %>%
        group_modify(
          ~ slice_sample(
            .x,
            n = min(nrow(.x), 6000)
          )
        ) %>%
        ungroup()
      
    })
    
    
    # --------------------------------------
    # 4. INDICADORES
    # --------------------------------------
    
    indicadores <- reactive({
      
      datos_filtrados() %>%
        summarise(
          total_viajes = n(),
          
          viajes_con_precio = sum(
            !is.na(price)
          ),
          
          precio_promedio = mean(
            price,
            na.rm = TRUE
          )
        )
      
    })
    
    
    # --------------------------------------
    # 5. RESUMEN ESTADISTICO
    # --------------------------------------
    
    resumen_precios <- reactive({
      
      datos_precios() %>%
        group_by(cab_type) %>%
        summarise(
          promedio = mean(price),
          mediana = median(price),
          viajes = n(),
          .groups = "drop"
        )
      
    })
    
    
    # --------------------------------------
    # 6. FORMATO DE NUMEROS
    # --------------------------------------
    
    formato_numero <- function(x, decimales = 0) {
      
      format(
        round(x, decimales),
        big.mark = ".",
        decimal.mark = ",",
        nsmall = decimales,
        trim = TRUE
      )
      
    }
    
    
    # ======================================
    # 7. TARJETAS KPI
    # ======================================
    
    output$kpi_viajes <- renderUI({
      
      value_box(
        title = "Viajes registrados",
        
        value = formato_numero(
          indicadores()$total_viajes
        ),
        
        theme = "#174A7E"
      )
      
    })
    
    
    output$kpi_precios <- renderUI({
      
      value_box(
        title = "Viajes con precio",
        
        value = formato_numero(
          indicadores()$viajes_con_precio
        ),
        
        theme = "#2878B8"
      )
      
    })
    
    
    output$kpi_promedio <- renderUI({
      
      value_box(
        title = "Precio promedio (USD)",
        
        value = paste0(
          "$",
          formato_numero(
            indicadores()$precio_promedio,
            decimales = 2
          )
        ),
        
        theme = "#0B1F3A"
      )
      
    })
    
    
    # ======================================
    # 8. VIOLIN PLOT
    # ======================================
    
    output$grafico_promedio <- renderPlotly({
      
      req(nrow(datos_violin()) > 0)
      
      grafico <- ggplot(
        datos_violin(),
        aes(
          x = cab_type,
          y = price,
          fill = cab_type
        )
      ) +
        
        geom_violin(
          alpha = 0.8,
          trim = FALSE,
          scale = "width",
          color = "#174A7E",
          linewidth = 0.4
        ) +
        
        # PROMEDIO EXACTO
        
        geom_point(
          data = resumen_precios(),
          aes(
            x = cab_type,
            y = promedio
          ),
          inherit.aes = FALSE,
          color = color_orange,
          size = 3.5
        ) +
        
        scale_fill_manual(
          values = colores,
          drop = FALSE
        ) +
        
        labs(
          x = NULL,
          y = "Precio del viaje (USD)"
        ) +
        
        theme_minimal(
          base_size = 12
        ) +
        
        theme(
          legend.position = "none",
          
          panel.grid.major.x =
            element_blank(),
          
          panel.grid.minor =
            element_blank(),
          
          axis.text =
            element_text(
              color = color_navy
            ),
          
          axis.title =
            element_text(
              color = color_navy
            )
        )
      
      ggplotly(
        grafico,
        tooltip = c("x", "y")
      ) %>%
        layout(
          margin = list(
            l = 65,
            r = 20,
            b = 50,
            t = 20
          )
        )
      
    })
    
    
    # ======================================
    # 9. BOXPLOT OPTIMIZADO
    # ======================================
    
    output$grafico_distribucion <- renderPlotly({
      
      resumen <- resumen_boxplot(
        datos_filtrados()
      ) %>%
        mutate(
          cab_type = as.character(cab_type),
          color = unname(colores[cab_type])
        )
      
      req(nrow(resumen) > 0)
      
      grafico <- plot_ly()
      
      for (i in seq_len(nrow(resumen))) {
        
        fila <- resumen[i, ]
        
        grafico <- grafico %>%
          add_trace(
            type = "box",
            
            name = fila$cab_type,
            
            x = rep(
              fila$cab_type,
              5
            ),
            
            q1 = fila$q1,
            median = fila$mediana,
            q3 = fila$q3,
            
            lowerfence =
              fila$bigote_inferior,
            
            upperfence =
              fila$bigote_superior,
            
            boxpoints = FALSE,
            
            fillcolor = fila$color,
            
            line = list(
              color = color_navy
            ),
            
            opacity = 0.75,
            
            showlegend = FALSE
          )
        
      }
      
      grafico %>%
        layout(
          xaxis = list(
            title = ""
          ),
          
          yaxis = list(
            title = "Precio del viaje (USD)",
            zeroline = FALSE
          ),
          
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          
          margin = list(
            l = 65,
            r = 20,
            b = 50,
            t = 20
          )
        )
      
    })
    
  })
  
}
