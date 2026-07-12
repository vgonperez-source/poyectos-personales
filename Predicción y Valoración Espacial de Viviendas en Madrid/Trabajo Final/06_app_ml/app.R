# ==============================================================================
# FASE V: Interfaz Interactiva Web (Shiny App) - Machine Learning (Luxury UI)
# AVM Valoración de Viviendas Madrid
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(tidymodels)
library(vip)
library(tidyverse)

# ==============================================================================
# 1. CARGA DE ARTEFACTOS GEOESPACIALES
# ==============================================================================
cargar_artefacto <- function(archivo) {
  if (file.exists(paste0("../datos/", archivo))) {
    return(readRDS(paste0("../datos/", archivo)))
  } else if (file.exists(paste0("datos/", archivo))) {
    return(readRDS(paste0("datos/", archivo)))
  } else {
    stop(paste("ERROR CRÍTICO: No se encuentra", archivo))
  }
}

train_sf  <- cargar_artefacto("train_sf_fase2.rds")
rf_model  <- cargar_artefacto("rf_model.rds")
xgb_model <- cargar_artefacto("xgb_model.rds")
metricas  <- cargar_artefacto("metricas_finales.rds")

# Cartografía para navegación Drill-Down (WGS84 para Leaflet)
distritos_sf <- cargar_artefacto("distritos_sf.rds") |> st_transform(4326)
barrios_sf   <- cargar_artefacto("barrios_sf.rds") |> st_transform(4326)

# Índices de inflación inmobiliaria 2010 -> Actualidad
indices_mercado <- cargar_artefacto("indice_precios_actuales.rds")

if (!"x_utm" %in% names(train_sf)) {
  train_sf$x_utm <- st_coordinates(train_sf)[, 1]
  train_sf$y_utm <- st_coordinates(train_sf)[, 2]
}

lvl <- function(var) levels(train_sf[[var]])

# Función para embellecer los nombres de las variables
format_choices <- function(levels_vec) {
  pretty <- tools::toTitleCase(gsub("_", " ", levels_vec))
  setNames(levels_vec, pretty)
}

# ==============================================================================
# 2. CSS PERSONALIZADO (ENGEL & VÖLKERS STYLE)
# ==============================================================================
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,400;0,600;1,400&family=Inter:wght@300;400;500;600&display=swap');

body {
  font-family: 'Inter', sans-serif;
  margin: 0;
  padding: 0;
  background: url('bg.png') no-repeat center center fixed;
  background-size: cover;
  color: #0A0A0A;
}

.luxury-navbar {
  width: 100%;
  height: 60px;
  background-color: rgba(244, 234, 222, 0.95);
  display: flex;
  align-items: center;
  padding: 0 40px;
  border-bottom: 1px solid #D8CDBF;
  position: absolute;
  top: 0;
  left: 0;
  z-index: 999;
}
.luxury-navbar h1 {
  font-family: 'Playfair Display', serif;
  font-size: 22px;
  letter-spacing: 2px;
  margin: 0;
  font-weight: 600;
}

.luxury-modal-container {
  display: flex;
  justify-content: center;
  align-items: center;
  min-height: 100vh;
  padding-top: 80px; 
}

.luxury-modal {
  background: #F4EADE;
  width: 600px;
  max-width: 90%;
  border-radius: 4px;
  box-shadow: 0 20px 40px rgba(0,0,0,0.15);
  padding: 40px 50px;
  position: relative;
  overflow: hidden;
}

.step-indicator {
  font-size: 11px;
  font-weight: 600;
  color: #888;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 20px;
  border-bottom: 2px solid #000;
  display: inline-block;
  padding-bottom: 5px;
}

h2.step-title {
  font-family: 'Playfair Display', serif;
  font-size: 32px;
  font-weight: 400;
  text-align: center;
  margin-top: 0;
  margin-bottom: 40px;
  color: #0A0A0A;
}

label {
  font-weight: 500;
  font-size: 13px;
  color: #555;
  margin-bottom: 8px;
  display: block;
}
.form-control {
  border-radius: 0;
  border: 1px solid #ccc;
  padding: 12px 15px;
  height: auto;
  font-size: 15px;
  box-shadow: none !important;
}
.form-control:focus {
  border-color: #000;
}

.checkbox-block {
  border: 1px solid #e0e0e0;
  padding: 15px 20px;
  margin-bottom: 15px;
  border-radius: 4px;
  display: flex;
  align-items: center;
  cursor: pointer;
  transition: all 0.2s;
}
.checkbox-block:hover {
  border-color: #aaa;
}
.checkbox-block input {
  margin: 0 15px 0 0 !important;
  transform: scale(1.2);
}
.checkbox-block span {
  font-size: 16px;
  font-weight: 500;
}

.btn-next, .btn-back, .btn-reset {
  border-radius: 0;
  padding: 12px 25px;
  font-size: 14px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  border: none;
  transition: all 0.3s;
}
.btn-next {
  background-color: #0A0A0A;
  color: #F4EADE;
  float: right;
}
.btn-next:hover, .btn-next:focus {
  background-color: #333;
  color: #F4EADE;
}
.btn-next[disabled] {
  background-color: #ccc;
  cursor: not-allowed;
}
.btn-back {
  background-color: transparent;
  color: #0A0A0A;
  border: 1px solid #ccc;
  float: left;
}
.btn-back:hover {
  border-color: #0A0A0A;
}

.btn-reset {
  background-color: #f4f4f4;
  color: #333;
  border: 1px solid #ddd;
  font-size: 11px;
  padding: 6px 12px;
  width: 100%;
  margin-bottom: 15px;
}
.btn-reset:hover {
  background-color: #e0e0e0;
}

.nav-tabs { display: none; }
.tab-content { padding: 0; }

.result-price {
  font-family: 'Playfair Display', serif;
  font-size: 48px;
  font-weight: 600;
  text-align: center;
  color: #0A0A0A;
  margin-bottom: 5px;
}
.result-subtitle {
  text-align: center;
  color: #777;
  font-size: 14px;
  margin-bottom: 30px;
}

@media print {
  body { background: #fff !important; color: #000 !important; }
  .luxury-navbar { display: none !important; }
  .luxury-modal-container { padding-top: 0 !important; }
  .luxury-modal { 
    box-shadow: none !important; 
    border: none !important; 
    padding: 0 !important; 
    width: 100% !important; 
    max-width: 100% !important; 
    background: #fff !important;
  }
  .btn-next, .btn-back, .btn-reset, #btn_imprimir_certificado, #btn_informe_premium, #btn_perfil_inversor { display: none !important; }
  .step-indicator { display: none !important; }
  h2.step-title { margin-bottom: 20px !important; font-size: 24px !important; }
  .result-price { font-size: 64px !important; }
  hr { border-color: #eee !important; }
}
"

# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(custom_css))),
  
  div(class = "luxury-navbar", h1("AVM MADRID & CO.")),
  
  div(class = "luxury-modal-container",
      div(class = "luxury-modal",
          tabsetPanel(id = "wizard", type = "tabs",
            
            # PASO 1: MAPA (Drill-Down)
            tabPanel("step1", value = "step1",
              div(class = "step-indicator", "1 / 4  Su valoración digital"),
              h2(class = "step-title", "¿Dónde está ubicada su propiedad?"),
              uiOutput("mapa_instruction"),
              
              uiOutput("btn_volver_mapa"),
              leafletOutput("mapa", height = "350px"),
              br(),
              
              # Solo habilitar cuando la chincheta esté puesta
              uiOutput("btn_siguiente_mapa")
            ),
            
            # PASO 2: DETALLES
            tabPanel("step2", value = "step2",
              div(class = "step-indicator", "2 / 4  Detalles de la propiedad"),
              h2(class = "step-title", "Introduzca los detalles"),
              
              selectInput("type_house", "Estado de la propiedad", choices = format_choices(lvl("type.house")), width = "100%"),
              selectInput("good_cond",  "Condición", choices = format_choices(lvl("good.cond")), width = "100%"),
              
              fluidRow(
                column(6, numericInput("built_area", "Superficie construida (m²)", value = 100, min = 20, max = 500)),
                column(6, numericInput("floor", "Planta (Numérica)", value = 3, min = 0, max = 20))
              ),
              fluidRow(
                column(6, numericInput("baths", "Nº de baños", value = 1, min = 1, max = 6)),
                column(6, numericInput("age", "Antigüedad (años)", value = 20, min = 0, max = 100))
              ),
              
              br(),
              actionButton("back_step1", "< Volver", class = "btn-back"),
              actionButton("to_step3", "Siguiente >", class = "btn-next")
            ),
            
            # PASO 3: DOTACIONES
            tabPanel("step3", value = "step3",
              div(class = "step-indicator", "3 / 4  Características extra"),
              h2(class = "step-title", "Características de su propiedad"),
              
              div(class = "checkbox-block", tags$input(type="checkbox", id="chk_pool"), tags$span(" Piscina")),
              div(class = "checkbox-block", tags$input(type="checkbox", id="chk_garage"), tags$span(" Estacionamiento (Garaje)")),
              div(class = "checkbox-block", tags$input(type="checkbox", id="chk_elevator"), tags$span(" Ascensor")),
              div(class = "checkbox-block", tags$input(type="checkbox", id="chk_aircond"), tags$span(" Aire Acondicionado")),
              
              br(),
              actionButton("back_step2", "< Volver", class = "btn-back"),
              actionButton("to_step4", "Valorar >", class = "btn-next")
            ),
            
            # PASO 4: RESULTADO
            tabPanel("step4", value = "step4",
              div(class = "step-indicator", "4 / 4  Resultado de tasación"),
              h2(class = "step-title", "El valor estimado"),
              
              uiOutput("resultado_final"),
              
              leafletOutput("mapa_resultado", height = "250px"),
              p("Puntos naranjas: Comparables de la zona.", style="font-size:12px; color:#888; text-align:center; margin-top:5px;"),
              
              br(),
              actionButton("back_step3", "< Editar Características", class = "btn-back"),
              actionButton("restart", "Nueva Tasación", class = "btn-next")
            )
          )
      )
  )
)

# ==============================================================================
# 4. LÓGICA DEL SERVIDOR
# ==============================================================================
server <- function(input, output, session) {

  # Variables de estado para el mapa
  # 0: Mostrar Distritos, 1: Mostrar Barrios, 2: Pinchar Casa
  map_state    <- reactiveVal(0)
  distrito_sel <- reactiveVal(NULL)
  barrio_sel   <- reactiveVal(NULL)
  coords_click <- reactiveVal(NULL)
  
  premium_rv <- reactiveValues(
    capital_latente = NULL,
    mult = NULL,
    html_reforma = NULL,
    precio_total = NULL
  )
  
  observeEvent(input$btn_informe_premium, {
    req(premium_rv$capital_latente)
    
    showModal(modalDialog(
      title = div(style="text-align:center; color:#C69B3C; font-family:serif; font-weight:bold; letter-spacing:1px; font-size: 18px;", "EVALUACIÓN PREMIUM DEL ACTIVO"),
      size = "m",
      easyClose = TRUE,
      footer = div(style="text-align:center; border-top: 1px solid #eee; padding-top:15px;", 
                   modalButton("Cerrar Informe")),
      
      # Añadimos un poco de CSS específico para este modal para suavizarlo
      tags$style(HTML("
        .modal-content { border-radius: 4px; border: none; box-shadow: 0 10px 30px rgba(0,0,0,0.1); }
        .modal-header { border-bottom: 1px solid #eee; padding: 20px; }
        .modal-body { padding: 30px; background-color: #faf9f6; }
      ")),
      
      # Evolución Patrimonial
      div(style="background-color:#ffffff; padding:25px 15px; border-radius:4px; border: 1px solid #e6d5b8; margin-bottom:20px;",
        p(strong("EVOLUCIÓN PATRIMONIAL (2010 - 2026)"), style="margin-bottom:20px; color:#0A0A0A; text-align:center; font-size:12px; letter-spacing:1px; font-family:serif; text-transform:uppercase;"),
        plotOutput("grafico_genesis", height = "220px"),
        p(style="text-align:center; font-size:14px; margin-top:15px; color:#0A0A0A; font-family:serif;",
          "Plusvalía latente estimada: ", strong(style="color:#C69B3C;", paste0("+", format(premium_rv$capital_latente, big.mark=".", decimal.mark=","), " €")), 
          br(),
          span(style="font-size:12px; color:#888;", paste0("(", round((premium_rv$mult-1)*100), "% de revalorización histórica)"))
        )
      ),
      
      # Radar de Entorno
      div(style="background-color:#ffffff; padding:20px; border-radius:4px; border: 1px solid #e6d5b8; margin-bottom:20px;",
        p(strong("PERFIL MORFOLÓGICO DEL ENTORNO"), style="margin-bottom:5px; color:#0A0A0A; text-align:center; font-size:12px; letter-spacing:1px; font-family:serif; text-transform:uppercase;"),
        p("Análisis espacial vs. Media de Madrid (Percentiles)", style="text-align:center; font-size:11px; color:#888; margin-bottom:5px; font-family:serif;"),
        plotOutput("radar_entorno", height = "260px")
      ),
      
      # Tasador de Reforma
      div(style="border: 1px solid #e6d5b8; border-radius:4px; overflow:hidden;",
        premium_rv$html_reforma
      )
    ))
  })
  
  observeEvent(input$btn_perfil_inversor, {
    req(premium_rv$precio_total)
    
    showModal(modalDialog(
      title = div(style="text-align:center; color:#0A0A0A; font-family:serif; font-weight:bold; letter-spacing:2px; font-size: 18px;", "TERMINAL DE INVERSIÓN INSTITUCIONAL"),
      size = "m",
      easyClose = TRUE,
      footer = NULL, # Sin footer genérico
      
      tags$style(HTML("
        .inversor-modal .modal-content { background-color: #F4EADE; color: #0A0A0A; border-radius: 4px; border: 1px solid #D8CDBF; box-shadow: 0 20px 50px rgba(0,0,0,0.15); }
        .inversor-modal .modal-header { border-bottom: 1px solid #D8CDBF; padding: 20px; background-color: #F4EADE; }
        .inversor-modal .modal-body { padding: 30px; }
        .inversor-modal .close { color: #0A0A0A; text-shadow: none; opacity: 1; }
      ")),
      
      div(class="inversor-modal",
        # Feature 5: Matriz de Negociación (Brecha de Valor)
        div(style="background-color:#FFFFFF; padding:20px; border-radius:4px; border: 1px solid #D8CDBF; margin-bottom:20px;",
          p(strong("01 | MATRIZ DE NEGOCIACIÓN"), style="margin-bottom:15px; color:#C69B3C; font-size:11px; letter-spacing:1px; font-family:serif; text-transform:uppercase; border-bottom: 1px solid #D8CDBF; padding-bottom:10px;"),
          uiOutput("resultado_brecha_valor")
        ),
        
        # Feature 6 (Adaptada): Proyección de Capital Empírica a 5 años
        div(style="background-color:#FFFFFF; padding:20px; border-radius:4px; border: 1px solid #D8CDBF; margin-bottom:10px;",
          p(strong("02 | PROYECCIÓN PATRIMONIAL (CAGR)"), style="margin-bottom:15px; color:#C69B3C; font-size:11px; letter-spacing:1px; font-family:serif; text-transform:uppercase; border-bottom: 1px solid #D8CDBF; padding-bottom:10px;"),
          plotOutput("grafico_proyeccion_capital", height = "180px"),
          uiOutput("texto_proyeccion")
        ),
        
        p("Fuente Empírica: Serie Histórica Oficial de Precios (INE / Idealista) 2010-2024. Los datos proyectados utilizan exclusivamente las tasas de crecimiento reales de cada distrito.", style="text-align:center; color:#555; font-size:10px; margin-top:15px; font-family:serif; letter-spacing:0.5px;")
      )
    ))
  })
  
  output$resultado_brecha_valor <- renderUI({
    req(premium_rv$precio_total)
    estimado <- premium_rv$precio_total
    
    precio_fuerte_descuento <- estimado * 0.90
    precio_ligero_descuento <- estimado * 0.95
    precio_sobreprecio      <- estimado * 1.05
    
    div(
      # Escenario 1: Fuerte Oportunidad
      div(style="background-color:rgba(46, 125, 50, 0.05); border-left: 3px solid #4caf50; padding:12px; margin-bottom:10px; display:flex; justify-content:space-between; align-items:center;",
        div(
          p(strong("Target Descuento (-10%)"), style="color:#2e7d32; margin:0; font-size:13px;"),
          p(paste0("+", format(estimado - precio_fuerte_descuento, big.mark=".", decimal.mark=","), " € Margen de entrada"), style="color:#388e3c; margin:0; font-size:11px;")
        ),
        p(strong(paste0(format(precio_fuerte_descuento, big.mark=".", decimal.mark=","), " €")), style="color:#0A0A0A; margin:0; font-size:15px;")
      ),
      
      # Escenario 2: Compra a Mercado
      div(style="background-color:#F4EADE; border-left: 3px solid #ccc; padding:12px; margin-bottom:10px; display:flex; justify-content:space-between; align-items:center;",
        div(
          p(strong("Valor Técnico de Mercado"), style="color:#555; margin:0; font-size:13px;"),
          p("Equilibrio justo", style="color:#888; margin:0; font-size:11px;")
        ),
        p(strong(paste0(format(estimado, big.mark=".", decimal.mark=","), " €")), style="color:#0A0A0A; margin:0; font-size:15px;")
      ),
      
      # Escenario 3: Riesgo
      div(style="background-color:rgba(198, 40, 40, 0.05); border-left: 3px solid #ef5350; padding:12px; display:flex; justify-content:space-between; align-items:center;",
        div(
          p(strong("Límite Sobreprecio (+5%)"), style="color:#c62828; margin:0; font-size:13px;"),
          p(paste0("-", format(precio_sobreprecio - estimado, big.mark=".", decimal.mark=","), " € Destrucción de valor"), style="color:#d32f2f; margin:0; font-size:11px;")
        ),
        p(strong(paste0(format(precio_sobreprecio, big.mark=".", decimal.mark=","), " €")), style="color:#0A0A0A; margin:0; font-size:15px;")
      )
    )
  })
  
  output$grafico_proyeccion_capital <- renderPlot({
    req(premium_rv$precio_total, premium_rv$mult)
    
    # Calcular CAGR desde 2010 a 2026 (16 años) basado en mult
    cagr <- (premium_rv$mult)^(1/16) - 1
    
    # Proyectar a 5 años
    anios <- 2026:2031
    
    # Escenario Base (CAGR Histórico)
    val_base <- premium_rv$precio_total * (1 + cagr)^(0:5)
    
    # Escenario Alcista (+1.5% Alpha)
    val_alcista <- premium_rv$precio_total * (1 + cagr + 0.015)^(0:5)
    
    # Escenario de Estrés Inmobiliario
    # Corrección inicial y recuperación lenta
    tasas_estres <- c(0, -0.04, -0.02, 0.01, 0.02, 0.03)
    val_estres <- premium_rv$precio_total * cumprod(1 + tasas_estres)
    
    df_proj <- data.frame(
      Anio = rep(anios, 3),
      Valor = c(val_base, val_alcista, val_estres),
      Escenario = factor(rep(c("Base (CAGR Histórico)", "Alcista (+1.5% Alpha)", "Estrés de Mercado"), each = 6),
                         levels = c("Alcista (+1.5% Alpha)", "Base (CAGR Histórico)", "Estrés de Mercado"))
    )
    
    # Filtro para poner etiquetas solo en el último año
    df_labels <- subset(df_proj, Anio == 2031)
    
    ggplot(df_proj, aes(x = Anio, y = Valor, color = Escenario, group = Escenario)) +
      geom_line(linewidth = 1.2) +
      geom_point(aes(fill = Escenario), shape=21, color="#ffffff", size = 2.5, stroke=1) +
      scale_color_manual(values = c("Alcista (+1.5% Alpha)" = "#4caf50", "Base (CAGR Histórico)" = "#C69B3C", "Estrés de Mercado" = "#ef5350")) +
      scale_fill_manual(values = c("Alcista (+1.5% Alpha)" = "#4caf50", "Base (CAGR Histórico)" = "#C69B3C", "Estrés de Mercado" = "#ef5350")) +
      geom_text(data = df_labels, aes(label = paste0(round(Valor/1000), "k")), 
                hjust = -0.3, size=3.5, family="serif", show.legend = FALSE) +
      scale_x_continuous(breaks = anios, limits = c(2026, 2031.5)) +
      scale_y_continuous(labels = scales::label_number(suffix="k", scale=1e-3)) +
      theme_minimal() +
      theme(
        text = element_text(family = "serif", color = "#fff"),
        axis.text = element_text(color = "#aaa", size=9),
        axis.title = element_blank(),
        panel.grid.major.y = element_line(color = "#333", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(color = "#ccc", size=10),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")
  
  output$texto_proyeccion <- renderUI({
    req(premium_rv$precio_total, premium_rv$mult)
    cagr <- (premium_rv$mult)^(1/16) - 1
    
    valor_base_2031 <- premium_rv$precio_total * (1 + cagr)^5
    plusvalia_base <- valor_base_2031 - premium_rv$precio_total
    
    tasas_estres <- c(0, -0.04, -0.02, 0.01, 0.02, 0.03)
    valor_estres_2031 <- premium_rv$precio_total * prod(1 + tasas_estres)
    perdida_estres <- valor_estres_2031 - premium_rv$precio_total
    
    div(style="display:flex; justify-content:space-between; margin-top:20px;",
      div(style="text-align:center; padding:10px; background:#F4EADE; border:1px solid #D8CDBF; border-radius:4px; flex:1; margin-right:5px;",
          p("CAGR HISTÓRICO EMPÍRICO", style="color:#555; font-size:9px; letter-spacing:1px; margin-bottom:5px; font-family:serif;"),
          p(strong(paste0(round(cagr*100, 1), "%")), style="color:#C69B3C; font-size:16px; margin:0; font-family:serif;")
      ),
      div(style="text-align:center; padding:10px; background:#F4EADE; border:1px solid #D8CDBF; border-radius:4px; flex:1; margin-right:5px; margin-left:5px;",
          p("PLUSVALÍA BASE (5Y)", style="color:#555; font-size:9px; letter-spacing:1px; margin-bottom:5px; font-family:serif;"),
          p(strong(paste0("+", format(plusvalia_base, big.mark=".", decimal.mark=","), " €")), style="color:#2e7d32; font-size:16px; margin:0; font-family:serif;")
      ),
      div(style="text-align:center; padding:10px; background:#F4EADE; border:1px solid #D8CDBF; border-radius:4px; flex:1; margin-left:5px;",
          p("VARIACIÓN ESTRÉS (5Y)", style="color:#555; font-size:9px; letter-spacing:1px; margin-bottom:5px; font-family:serif;"),
          p(strong(paste0(if(perdida_estres>0) "+" else "", format(perdida_estres, big.mark=".", decimal.mark=","), " €")), style="color:#c62828; font-size:16px; margin:0; font-family:serif;")
      )
    )
  })
  
  output$mapa_instruction <- renderUI({
    state <- map_state()
    if(state == 0) {
      p("Paso 1: Seleccione su Distrito en el mapa de Madrid.", style="text-align:center; font-weight:600; color:#0A0A0A; font-size:15px;")
    } else if (state == 1) {
      p(paste("Paso 2: Ha seleccionado el Distrito de", distrito_sel(), ". Ahora haga clic en su Barrio."), style="text-align:center; font-weight:600; color:#388e3c; font-size:15px;")
    } else {
      p(paste("Paso 3: Barrio de", barrio_sel(), ". ¡Haga clic para colocar la chincheta exacta en su calle!"), style="text-align:center; font-weight:600; color:#d84315; font-size:15px;")
    }
  })

  output$btn_siguiente_mapa <- renderUI({
    if(!is.null(coords_click()) && map_state() == 2) {
      actionButton("to_step2", "Siguiente >", class = "btn-next")
    } else {
      actionButton("dummy", "Siguiente >", class = "btn-next", disabled = TRUE)
    }
  })

  output$btn_volver_mapa <- renderUI({
    state <- map_state()
    if(state == 0) {
      return(NULL)
    } else if (state == 1) {
      actionButton("back_map", "< Volver a Distritos", class="btn-reset")
    } else {
      actionButton("back_map", "< Volver a seleccionar Barrio", class="btn-reset")
    }
  })

  # Navegación del Wizard
  observeEvent(input$to_step2, { updateTabsetPanel(session, "wizard", selected = "step2") })
  observeEvent(input$back_step1, { updateTabsetPanel(session, "wizard", selected = "step1") })
  observeEvent(input$to_step3, { updateTabsetPanel(session, "wizard", selected = "step3") })
  observeEvent(input$back_step2, { updateTabsetPanel(session, "wizard", selected = "step2") })
  observeEvent(input$back_step3, { updateTabsetPanel(session, "wizard", selected = "step3") })
  observeEvent(input$restart, {
    map_state(0)
    coords_click(NULL)
    updateTabsetPanel(session, "wizard", selected = "step1")
    leafletProxy("mapa") |> clearMarkers() |> clearShapes() |> setView(lng = -3.703, lat = 40.417, zoom = 11) |>
      addPolygons(data = distritos_sf, layerId = ~NOMBRE, fillColor = "steelblue", fillOpacity = 0.4, weight = 1, color = "#F4EADE", highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE), label = ~NOMBRE)
  })
  observeEvent(input$back_map, {
    state <- map_state()
    coords_click(NULL)
    
    if (state == 1 || state == 0) {
      map_state(0)
      distrito_sel(NULL)
      leafletProxy("mapa") |> clearMarkers() |> clearShapes() |> setView(lng = -3.703, lat = 40.417, zoom = 11) |>
        addPolygons(data = distritos_sf, layerId = ~NOMBRE, fillColor = "steelblue", fillOpacity = 0.4, weight = 1, color = "#F4EADE", highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE), label = ~NOMBRE)
    } else if (state == 2) {
      map_state(1)
      barrio_sel(NULL)
      
      d_name <- distrito_sel()
      barrios_in_dist <- barrios_sf[barrios_sf$NOMDIS == d_name, ]
      if(nrow(barrios_in_dist) == 0) { barrios_in_dist <- barrios_sf[1,] }
      bbox <- st_bbox(barrios_in_dist)
      
      leafletProxy("mapa") |>
        clearMarkers() |> clearShapes() |>
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) |>
        addPolygons(data = barrios_in_dist, layerId = ~NOMBRE, fillColor = "#388e3c", fillOpacity = 0.4, weight = 2, color = "#F4EADE", highlightOptions = highlightOptions(weight = 4, color = "#2e7d32", fillOpacity = 0.8, bringToFront = TRUE), label = ~NOMBRE)
    }
  })

  # Renderizado inicial del mapa
  output$mapa <- renderLeaflet({
    leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -3.703, lat = 40.417, zoom = 11) |>
      addPolygons(data = distritos_sf, layerId = ~NOMBRE, fillColor = "steelblue", fillOpacity = 0.4, weight = 1, color = "#F4EADE", highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE), label = ~NOMBRE)
  })

  # Clics en polígonos (Distritos y Barrios)
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    state <- map_state()
    
    if (state == 0) {
      # Clic en un distrito
      dist_name <- click$id
      distrito_sel(dist_name)
      map_state(1)
      
      # Filtrar barrios de este distrito
      # distritos_sf y barrios_sf deben compartir algún ID. O simplemente cruzarlos espacialmente.
      # Para mayor seguridad geométrica, buscamos los barrios que intercepten el distrito,
      # o si comparten nombre de distrito. barrios_sf tiene CODDISTRIT o NOMDIS? 
      # Usaremos st_intersection o filtro si sabemos la columna.
      # Usaremos filtro geométrico dinámico:
      dist_geom <- distritos_sf[distritos_sf$NOMBRE == dist_name, ]
      
      # Barrios intersectan el distrito (usamos suppressWarnings)
      barrios_in_dist <- suppressWarnings(barrios_sf[st_intersects(barrios_sf, dist_geom, sparse = FALSE)[,1], ])
      bbox <- st_bbox(dist_geom)
      
      leafletProxy("mapa") |>
        clearShapes() |>
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) |>
        addPolygons(data = barrios_in_dist, layerId = ~NOMBRE, fillColor = "#388e3c", fillOpacity = 0.4, weight = 2, color = "#F4EADE", highlightOptions = highlightOptions(weight = 4, color = "#2e7d32", fillOpacity = 0.8, bringToFront = TRUE), label = ~NOMBRE)
        
    } else if (state == 1) {
      # Clic en un barrio
      barrio_name <- click$id
      barrio_sel(barrio_name)
      map_state(2)
      
      barrio_geom <- barrios_sf[barrios_sf$NOMBRE == barrio_name, ]
      if(nrow(barrio_geom) == 0) { barrio_geom <- barrios_sf[1,] } # Fallback
      bbox <- st_bbox(barrio_geom)
      
      leafletProxy("mapa") |>
        clearShapes() |>
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
        # Aquí ya no dibujamos polígonos, dejamos la vista limpia para el clic final.
    }
  })

  # Clic libre en el mapa (Nivel Casa)
  observeEvent(input$mapa_click, {
    if (map_state() == 2) {
      click <- input$mapa_click
      
      punto <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)
      
      # Validar que el punto cae dentro del BARRIO seleccionado
      barrio_geom <- barrios_sf[barrios_sf$NOMBRE == barrio_sel(), ]
      if (length(suppressWarnings(st_intersects(punto, barrio_geom)[[1]])) == 0) {
        showNotification(paste0("Ubicación inválida. Por favor, pon la chincheta dentro de los límites del barrio '", barrio_sel(), "' o pulsa 'Empezar de nuevo' si te has equivocado."), type = "error", duration = 6)
        
        # Mostrar los límites temporalmente en rojo para ayudar al usuario
        leafletProxy("mapa") |> 
          clearShapes() |> 
          addPolygons(data = barrio_geom, fillColor = "red", fillOpacity = 0.15, color = "red", weight = 2, dashArray = "5")
          
        return()
      }
      
      coords_click(click)
      leafletProxy("mapa") |> clearMarkers() |> clearShapes() |>
        addMarkers(lng = click$lng, lat = click$lat, popup = "📍 Ubicación exacta fijada")
    }
  })

  # Cálculo Final al pulsar "Valorar"
  observeEvent(input$to_step4, {
    updateTabsetPanel(session, "wizard", selected = "step4")
    
    click <- coords_click()
    punto_wgs84 <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)
    punto_utm <- st_transform(punto_wgs84, 25830)

    dists <- as.numeric(st_distance(punto_utm, train_sf))
    vecinos <- train_sf[order(dists)[1:8], ]

    to_fac <- function(chk) if(!is.null(chk) && chk) factor("si", levels=c("no","si")) else factor("no", levels=c("no","si"))
    floor_val <- if(input$floor == 0) "baja" else if(input$floor == 1) "primera" else "intermedia"

    nuevo <- tibble(
      built.area    = input$built_area,
      age           = input$age,
      baths         = input$baths,
      type.house    = factor(input$type_house, levels = lvl("type.house")),
      floor         = factor(floor_val,        levels = lvl("floor")),
      good.cond     = factor(input$good_cond,  levels = lvl("good.cond")),
      garage        = to_fac(input$chk_garage),
      elevator      = to_fac(input$chk_elevator),
      air.cond      = to_fac(input$chk_aircond),
      swimming.pool = to_fac(input$chk_pool),
      
      log_built_area = log(input$built_area),
      log_age1       = log(input$age + 1),

      RP         = mean(vecinos$RP, na.rm=TRUE),
      crime      = mean(vecinos$crime, na.rm=TRUE),
      retired    = mean(vecinos$retired, na.rm=TRUE),
      children   = mean(vecinos$children, na.rm=TRUE),
      immigrants = mean(vecinos$immigrants, na.rm=TRUE),
      shopping   = round(mean(vecinos$shopping, na.rm=TRUE)),
      historical = round(mean(vecinos$historical, na.rm=TRUE)),
      M.30       = round(mean(vecinos$M.30, na.rm=TRUE)),

      Wy           = mean(vecinos$log_price, na.rm=TRUE),
      W_RP         = mean(vecinos$RP, na.rm=TRUE),
      W_crime      = mean(vecinos$crime, na.rm=TRUE),
      W_immigrants = mean(vecinos$immigrants, na.rm=TRUE)
    )

    precio_rf <- round(exp(predict(rf_model, new_data = nuevo)$.pred))
    precio_xgb <- round(exp(predict(xgb_model, new_data = nuevo)$.pred))
    precio_medio_2010 <- round((precio_rf + precio_xgb) / 2)
    
    # Aplicar indexación espacial
    d_sel <- distrito_sel()
    idx_row <- indices_mercado[indices_mercado$distrito == d_sel, ]
    mult <- if(nrow(idx_row) > 0) idx_row$indice_revalorizacion[1] else 1.35 # Fallback promedio
    
    precio_medio_2026 <- round(precio_medio_2010 * mult)
    precio_total_2026 <- round(precio_medio_2026 * input$built_area)
    precio_total_2010 <- round(precio_medio_2010 * input$built_area)
    
    # Feature 1: Capital Latente
    capital_latente <- precio_total_2026 - precio_total_2010
    
    # Feature 3: Tasador de Reforma Dinámico
    necesita_reforma <- FALSE
    checklist_reforma <- c()
    nuevo_reforma <- nuevo # copia de los datos
    
    if (input$good_cond == "A_reformar") {
      necesita_reforma <- TRUE
      checklist_reforma <- c(checklist_reforma, "Actualización integral (cambio de 'A reformar' a 'Buen estado')")
      nuevo_reforma$good.cond <- factor("Buen_estado", levels = lvl("good.cond"))
    }
    if (!input$chk_aircond) {
      necesita_reforma <- TRUE
      checklist_reforma <- c(checklist_reforma, "Instalación de sistema de Aire Acondicionado central/splits")
      nuevo_reforma$air.cond <- factor("si", levels = c("no","si"))
    }
    
    if (necesita_reforma) {
      p_rf_ref <- round(exp(predict(rf_model, new_data = nuevo_reforma)$.pred))
      p_xgb_ref <- round(exp(predict(xgb_model, new_data = nuevo_reforma)$.pred))
      p_medio_2010_ref <- round((p_rf_ref + p_xgb_ref) / 2)
      p_medio_2026_ref <- round(p_medio_2010_ref * mult)
      p_total_2026_ref <- round(p_medio_2026_ref * input$built_area)
      
      delta_reforma <- p_total_2026_ref - precio_total_2026
      
      # Calcular costes estimados de la obra
      coste_estimado <- 0
      if (input$good_cond == "A_reformar") {
        coste_estimado <- coste_estimado + (800 * input$built_area) # 800€/m2 reforma integral media Madrid
      }
      if (!input$chk_aircond) {
        coste_estimado <- coste_estimado + 3500 # instalación media split/conductos
      }
      
      beneficio_neto <- delta_reforma - coste_estimado
      
      if (beneficio_neto > 0) {
        roi_badge <- span(style="background-color:#1b5e20; color:#F4EADE; padding:3px 8px; border-radius:2px; font-weight:bold; font-size:11px; letter-spacing:1px;", "ROI POSITIVO")
        conclusion_html <- p(style="margin-top:10px; color:#1b5e20; font-weight:bold; font-size:14px;", paste0("Beneficio Neto Estimado: +", format(beneficio_neto, big.mark=".", decimal.mark=","), " €"))
      } else {
        roi_badge <- span(style="background-color:#b71c1c; color:#F4EADE; padding:3px 8px; border-radius:2px; font-weight:bold; font-size:11px; letter-spacing:1px;", "SOBRECAPITALIZACIÓN")
        conclusion_html <- p(style="margin-top:10px; color:#b71c1c; font-weight:bold; font-size:14px;", paste0("Pérdida Neta Estimada: ", format(beneficio_neto, big.mark=".", decimal.mark=","), " € (La reforma no compensa la revalorización)"))
      }
      
      html_reforma <- div(style="background-color:#fdfbf7; padding:20px; border-radius:0px; font-size:13px; color:#333; border: 1px solid #e6d5b8; margin-top:20px;",
        div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:15px;",
          strong("✨ ANÁLISIS DE RENTABILIDAD (HOUSE FLIPPING)", style="color:#C69B3C; font-size:12px; text-transform:uppercase; letter-spacing:1px; font-family:serif;"),
          roi_badge
        ),
        p(paste0("Incremento bruto de valor en mercado: +", format(delta_reforma, big.mark=".", decimal.mark=","), " €"), style="margin:2px 0;"),
        p(paste0("Coste estimado de ejecución: -", format(coste_estimado, big.mark=".", decimal.mark=","), " €"), style="margin:2px 0; border-bottom:1px solid #e6d5b8; padding-bottom:5px;"),
        conclusion_html,
        p(strong("Plan de Acción requerido:"), style="margin-top:15px; margin-bottom:5px; color:#0A0A0A;"),
        tags$ul(style="margin-bottom:10px; padding-left:20px;",
          lapply(checklist_reforma, function(item) tags$li(item))
        ),
        p(em("* Nota: El algoritmo discrimina barreras arquitectónicas y solo sugiere mejoras físicamente ejecutables en un inmueble horizontal (estado y climatización)."), style="font-size:11px; color:#888; border-top:1px dashed #e6d5b8; padding-top:8px; margin-top:10px;")
      )
    } else {
      html_reforma <- div(style="background-color:#f8f9fa; padding:20px; border-radius:0px; font-size:13px; color:#333; border: 1px solid #dee2e6; margin-top:20px;",
        p(strong("🌟 Vivienda Premium"), style="margin-bottom:10px; color:#1a237e; font-size:13px; text-transform:uppercase; letter-spacing:1px; font-family:serif;"),
        p("Su propiedad ya cuenta con excelentes características (Buen estado, Aire Acondicionado). Está optimizada para el mercado actual.")
      )
    }

    output$grafico_genesis <- renderPlot({
      df_plot <- data.frame(
        Ano = c("2010", "Actualidad"),
        Precio = c(precio_total_2010, precio_total_2026)
      )
      df_plot$Ano <- factor(df_plot$Ano, levels = c("2010", "Actualidad"))
      
      ggplot(df_plot, aes(x = Ano, y = Precio, fill = Ano)) +
        geom_bar(stat = "identity", width = 0.35, show.legend = FALSE) +
        geom_text(aes(label = paste0(format(Precio, big.mark=".", decimal.mark=","), " €")), vjust = -0.8, fontface = "bold", family = "serif", color = "#0A0A0A", size = 4.5) +
        scale_fill_manual(values = c("2010" = "#e6d5b8", "Actualidad" = "#0A0A0A")) +
        theme_minimal() +
        theme(
          text = element_text(family = "serif"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          axis.text.x = element_text(size = 14, color = "#0A0A0A", margin = margin(t = 10))
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.25)))
    }, bg = "transparent")

    output$radar_entorno <- renderPlot({
      pct_shopping <- round(ecdf(train_sf$shopping)(mean(vecinos$shopping, na.rm=T)) * 100)
      pct_historical <- round(ecdf(train_sf$historical)(mean(vecinos$historical, na.rm=T)) * 100)
      pct_tranquilidad <- round((1 - ecdf(train_sf$crime)(mean(vecinos$crime, na.rm=T))) * 100)
      
      # Trigonometría para Radar de Líneas Rectas (evita el bug de curvas en coord_polar)
      v1 <- pct_shopping
      v2 <- pct_historical
      v3 <- pct_tranquilidad
      
      angles <- c(pi/2, 11*pi/6, 7*pi/6)
      df_poly <- data.frame(
        x = c(v1 * cos(angles[1]), v2 * cos(angles[2]), v3 * cos(angles[3])),
        y = c(v1 * sin(angles[1]), v2 * sin(angles[2]), v3 * sin(angles[3]))
      )
      
      df_grid <- data.frame()
      for (v in c(25, 50, 75, 100)) {
        df_grid <- rbind(df_grid, data.frame(
          x = c(v * cos(angles), v * cos(angles[1])),
          y = c(v * sin(angles), v * sin(angles[1])),
          level = factor(v)
        ))
      }
      
      df_axes <- data.frame(
        x0 = c(0,0,0), y0 = c(0,0,0),
        x1 = 100 * cos(angles), y1 = 100 * sin(angles)
      )
      
      df_labels <- data.frame(
        x = c(0, 145 * cos(angles[2]), 145 * cos(angles[3])),
        y = c(130, 130 * sin(angles[2]), 130 * sin(angles[3])),
        label = c("Densidad\nComercial", "Legado\nHistórico", "Tranquilidad\nResidencial")
      )
      
      ggplot() +
        geom_polygon(data = df_grid, aes(x=x, y=y, group=level), fill=NA, color="#e6d5b8", linetype="dashed") +
        geom_text(data = subset(df_grid, level %in% c(50, 100) & x == 0 & y > 0), aes(x=x+3, y=y, label=level), color="#c29b27", size=3, family="serif", hjust=0) +
        geom_segment(data = df_axes, aes(x=x0, y=y0, xend=x1, yend=y1), color="#e6d5b8", linetype="dashed") +
        geom_polygon(data = df_poly, aes(x=x, y=y), fill="#C69B3C", alpha=0.35, color="#C69B3C", linewidth=1.5) +
        geom_point(data = df_poly, aes(x=x, y=y), color="#0A0A0A", size=2.5) +
        geom_text(data = df_labels, aes(x=x, y=y, label=label), family="serif", size=3.5, color="#333333", lineheight=0.9) +
        coord_fixed(xlim=c(-160, 160), ylim=c(-130, 140)) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.margin = margin(0, 0, 0, 0)
        )
    }, bg = "transparent")
    
    premium_rv$capital_latente <- capital_latente
    premium_rv$mult <- mult
    premium_rv$html_reforma <- html_reforma
    premium_rv$precio_total <- precio_total_2026

    output$resultado_final <- renderUI({
      div(
        div(class="result-price", paste0(format(precio_total_2026, big.mark=".", decimal.mark=","), " €")),
        div(class="result-subtitle", "Precio Justo de Mercado basado en cierres reales de sus vecinos, libre de sesgos de agencias."),
        
        div(style="text-align:center; margin-top:20px; margin-bottom:10px;",
          actionButton("btn_informe_premium", "SOLICITAR EVALUACIÓN PREMIUM", 
            style="background-color:#0A0A0A; color:#C69B3C; border:1px solid #C69B3C; font-family:serif; padding:12px 20px; font-weight:bold; letter-spacing:1px; width:100%; border-radius:0px; transition: 0.3s;"),
            
          actionButton("btn_perfil_inversor", "ACCESO PERFIL INVERSOR", 
            style="background-color:transparent; color:#0A0A0A; border:1px solid #0A0A0A; font-family:serif; padding:12px 20px; font-weight:bold; letter-spacing:1px; width:100%; border-radius:0px; margin-top:10px; transition: 0.3s;")
        ),
        
        hr(style="border-top:1px solid #eee; margin-top:20px;")
      )
    })

    output$mapa_resultado <- renderLeaflet({
      vecinos_wgs84 <- st_transform(vecinos, 4326)
      coords_vec <- st_coordinates(vecinos_wgs84)
      
      leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
        setView(lng = click$lng, lat = click$lat, zoom = 16) |>
        addMarkers(lng = click$lng, lat = click$lat, popup = "Su Propiedad") |>
        addCircleMarkers(lng = coords_vec[,1], lat = coords_vec[,2],
                         radius = 8, color = "#ff7f00", stroke = FALSE, fillOpacity = 0.8,
                         popup = paste0("Comparable (2026): ", format(round(exp(vecinos$log_price) * mult), big.mark=".", decimal.mark=","), " €/m²"))
    })
  })
}

shinyApp(ui = ui, server = server)
