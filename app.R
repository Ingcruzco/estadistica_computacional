#------------------------------------------------------------------------------#
# DASHBOARD ANALISIS DE PRECIOS EN VIAJES DE UBER VS LIFT
#------------------------------------------------------------------------------#

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# ==========================================
# 1. FUNCIONES Y MODULOS
# ==========================================

source("R/estadisticos.R")
source("modules/mod_overview.R")


# ==========================================
# 2. DATOS PROCESADOS
# ==========================================

datos <- readRDS(
  "data/processed/ride_analytics.rds"
)

# ==========================================
# 3. CONFIGURACION VISUAL
# ==========================================

tema <- bs_theme(
  version = 5,
  bg = "#F5F8FC",
  fg = "#0B1F3A",
  primary = "#174A7E",
  secondary = "#78B9E7",
  success = "#2878B8"
)

# ==========================================
# 4. OPCIONES DE NAVEGACION
# ==========================================

menu <- data.frame(
  id = c(
    "overview",
    "exploracion",
    "factores",
    "inferencia",
    "modelacion",
    "conclusiones"
  ),
  
  titulo = c(
    "Overview",
    "Análisis exploratorio",
    "Factores del precio",
    "Inferencia estadística",
    "Modelación predictiva",
    "Conclusiones"
  ),
  
  icono = c(
    "table-cells-large",
    "chart-area",
    "cloud-sun",
    "calculator",
    "chart-line",
    "lightbulb"
  ),
  
  stringsAsFactors = FALSE
)

# ==========================================
# 5. COMPONENTE TEMPORAL
# ==========================================

pantalla_pendiente <- function(titulo) {
  
  card(
    class = "ride-placeholder",
    
    card_body(
      h2(titulo),
      
      p(
        "Este módulo se desarrollará",
        "en las siguientes etapas."
      )
    )
  )
}

# ==========================================
# 6. INTERFAZ PRINCIPAL
# ==========================================

ui <- page_sidebar(
  
  title = NULL,
  
  theme = tema,
  
  fillable = FALSE,
  
  # SIDEBAR
  
  sidebar = sidebar(
    width = 270,
    open = "always",
    bg = "#0B1F3A",
    fg = "#FFFFFF",
    
    div(
      class = "ride-brand",
      
      div(
        class = "ride-brand-icon",
        icon("car")
      ),
      
      div(
        h3("Ride Analytics"),
        span("PRICE INTELLIGENCE")
      )
    ),
    
    div(
      class = "ride-menu-label",
      "NAVEGACIÓN"
    ),
    
    uiOutput("menu_navegacion"),
    
    div(
      class = "ride-sidebar-footer",
      "Uber vs Lyft",
      tags$small("Analytics Dashboard")
    )
  ),
  
  # CSS
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/styles.css"
    )
  ),
  
  # ENCABEZADO
  
  div(
    class = "main-header",
    
    div(
      class = "ride-header-kicker",
      "MOBILITY DATA ANALYTICS"
    ),
    
    h2("Ride Analytics"),
    
    p(
      "Análisis estadístico de precios",
      "de viajes de Uber y Lyft"
    )
  ),
  
  # CONTENIDO DE NAVEGACION
  
  navset_hidden(
    id = "secciones",
    selected = "overview",
    
    nav_panel(
      title = "Overview",
      value = "overview",
      mod_overview_ui("overview")
    ),
    
    nav_panel(
      title = "Exploración",
      value = "exploracion",
      pantalla_pendiente(
        "Análisis exploratorio"
      )
    ),
    
    nav_panel(
      title = "Factores",
      value = "factores",
      pantalla_pendiente(
        "Factores del precio"
      )
    ),
    
    nav_panel(
      title = "Inferencia",
      value = "inferencia",
      pantalla_pendiente(
        "Inferencia estadística"
      )
    ),
    
    nav_panel(
      title = "Modelación",
      value = "modelacion",
      pantalla_pendiente(
        "Modelación predictiva"
      )
    ),
    
    nav_panel(
      title = "Conclusiones",
      value = "conclusiones",
      pantalla_pendiente(
        "Insights y conclusiones"
      )
    )
  )
)

# ==========================================
# 7. SERVIDOR
# ==========================================

server <- function(input, output, session) {
  
  # Seccion activa
  
  seccion_activa <- reactiveVal("overview")
  
  # Crear menu lateral
  
  output$menu_navegacion <- renderUI({
    
    tagList(
      lapply(seq_len(nrow(menu)), function(i) {
        
        item <- menu[i, ]
        
        actionLink(
          inputId = paste0("nav_", item$id),
          
          label = tagList(
            icon(item$icono),
            span(item$titulo)
          ),
          
          class = paste(
            "ride-menu-link",
            if (seccion_activa() == item$id) {
              "active"
            } else {
              ""
            }
          )
        )
        
      })
    )
  })
  
  # Navegacion reactiva
  
  cambiar_seccion <- function(id) {
    
    # Cambiar pestaña activa
    bslib::nav_select(
      id = "secciones",
      selected = id,
      session = session
    )
    
    # Actualizar estado del sidebar
    seccion_activa(id)
    
  }
  
  observeEvent(
    input$nav_overview,
    cambiar_seccion("overview")
  )
  
  observeEvent(
    input$nav_exploracion,
    cambiar_seccion("exploracion")
  )
  
  observeEvent(
    input$nav_factores,
    cambiar_seccion("factores")
  )
  
  observeEvent(
    input$nav_inferencia,
    cambiar_seccion("inferencia")
  )
  
  observeEvent(
    input$nav_modelacion,
    cambiar_seccion("modelacion")
  )
  
  observeEvent(
    input$nav_conclusiones,
    cambiar_seccion("conclusiones")
  )
  
  # Iniciar Overview
  
  mod_overview_server(
    id = "overview",
    datos = datos
  )
  
}

# ==========================================
# 8. EJECUTAR SHINY
# ==========================================

shinyApp(
  ui = ui,
  server = server
)
