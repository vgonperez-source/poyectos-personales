# ==============================================================================
# FASE V: Interfaz Interactiva Web (Shiny App) - Kriging Universal
# AVM Valoración de Viviendas Madrid
#
# Objetivo: Desplegar el modelo geoestadístico en un entorno de usuario final,
# permitiendo clics interactivos para extraer valoraciones e intervalos de confianza.
# ==============================================================================

library(shiny)
library(leaflet)
library(sf)
library(gstat)
library(terra)
library(raster)
library(tidyverse)

# ==============================================================================
# 1. CARGA DE ARTEFACTOS GEOESPACIALES (LÓGICA ROBUSTA DE RUTAS)
# ==============================================================================

# Función a prueba de balas para el directorio de trabajo de RStudio
cargar_artefacto <- function(archivo) {
  if (file.exists(paste0("../datos/", archivo))) {
    return(readRDS(paste0("../datos/", archivo)))
  } else if (file.exists(paste0("datos/", archivo))) {
    return(readRDS(paste0("datos/", archivo)))
  } else {
    stop(paste("ERROR CRÍTICO: No se encuentra", archivo, "ni en 'datos/' ni en '../datos/'"))
  }
}

train_sf         <- cargar_artefacto("train_sf_fase2.rds")
variograma_ajust <- cargar_artefacto("variograma_ajustado.rds")
formula_krig     <- cargar_artefacto("formula_kriging.rds")
raster_pred      <- cargar_artefacto("kriging_raster_pred.rds")
raster_var       <- cargar_artefacto("kriging_raster_var.rds")
hull_concavo     <- cargar_artefacto("hull_concavo.rds")
metricas         <- cargar_artefacto("metricas_kriging.rds")

if (!"x_utm" %in% names(train_sf)) {
  train_sf$x_utm <- st_coordinates(train_sf)[, 1]
  train_sf$y_utm <- st_coordinates(train_sf)[, 2]
}

# ==============================================================================
# 2. TRANSFORMACIONES PARA LEAFLET (WGS84) Y PALETAS DE COLOR
# ==============================================================================

# Leaflet requiere capas raster tradicionales (RasterLayer)
raster_pred_wgs84 <- raster::raster(terra::project(raster_pred, "EPSG:4326"))
raster_var_wgs84  <- raster::raster(terra::project(raster_var,  "EPSG:4326"))

hull_wgs84 <- st_transform(hull_concavo, crs = 4326)

# BUG FIX: values() de la librería 'raster' no usa na.rm = TRUE.
pal_pred <- colorNumeric("plasma",
                         values(raster_pred_wgs84),
                         na.color = "transparent")
pal_var  <- colorNumeric("YlOrRd",
                         values(raster_var_wgs84),
                         na.color = "transparent")


# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
# ==============================================================================

ui <- fluidPage(

  titlePanel("AVM Madrid — Predicción por Kriging Universal"),

  sidebarLayout(

    sidebarPanel(
      width = 3,

      h4("Selecciona una ubicación"),
      p("Haz clic en el mapa para seleccionar el punto de valoración."),

      hr(),

      h4("Resultado"),
      uiOutput("resultado_box"),

      hr(),

      h4("Capas del mapa"),
      checkboxInput("show_pred",  "Mapa de predicción",     value = TRUE),
      checkboxInput("show_var",   "Mapa de varianza",       value = FALSE),
      checkboxInput("show_train", "Datos de entrenamiento", value = FALSE),

      hr(),

      h4("Métricas del modelo"),
      tableOutput("tabla_metricas"),

      hr(),
      p(em("La predicción se extrae del raster precalculado.
            Para puntos fuera del área interpolada, se ejecuta
            Kriging puntual (puede tardar unos segundos).")),
      p(em("Área válida: interior del polígono azul."))
    ),

    mainPanel(
      width = 9,
      leafletOutput("mapa", height = "650px"),
      br(),
      fluidRow(
        column(6, plotOutput("grafico_pred", height = "250px")),
        column(6, tableOutput("tabla_resultado"))
      )
    )
  )
)


# ==============================================================================
# 4. LÓGICA DEL SERVIDOR (BACKEND)
# ==============================================================================

server <- function(input, output, session) {

  punto_seleccionado <- reactiveVal(NULL)

  output$mapa <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -3.703, lat = 40.417, zoom = 12) |>
      addPolygons(data = hull_wgs84,
                  color = "steelblue", weight = 2,
                  fillOpacity = 0.05,
                  group = "Área de interpolación") |>
      addLayersControl(
        overlayGroups = c("Predicción (log EUR/m2)", "Varianza",
                          "Datos train", "Área de interpolación"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observe({
    proxy <- leafletProxy("mapa")

    if (input$show_pred) {
      proxy |>
        addRasterImage(raster_pred_wgs84, colors = pal_pred,
                       opacity = 0.7, group = "Predicción (log EUR/m2)") |>
        addLegend(position = "bottomright", pal = pal_pred,
                  values = values(raster_pred_wgs84),
                  title = "log(EUR/m2)", layerId = "legend_pred")
    } else {
      proxy |>
        clearGroup("Predicción (log EUR/m2)") |>
        removeControl("legend_pred")
    }

    if (input$show_var) {
      proxy |>
        addRasterImage(raster_var_wgs84, colors = pal_var,
                       opacity = 0.65, group = "Varianza") |>
        addLegend(position = "bottomleft", pal = pal_var,
                  values = values(raster_var_wgs84),
                  title = "Varianza", layerId = "legend_var")
    } else {
      proxy |>
        clearGroup("Varianza") |>
        removeControl("legend_var")
    }

    if (input$show_train) {
      train_wgs84  <- st_transform(train_sf, 4326)
      coords_train <- st_coordinates(train_wgs84)
      proxy |>
        addCircleMarkers(
          lng = coords_train[, 1], lat = coords_train[, 2],
          radius = 2, color = "grey30", fillOpacity = 0.4,
          stroke = FALSE, group = "Datos train"
        )
    } else {
      proxy |> clearGroup("Datos train")
    }
  })

  observeEvent(input$mapa_click, {
    click <- input$mapa_click
    punto_seleccionado(click)

    leafletProxy("mapa") |>
      clearMarkers() |>
      addMarkers(lng = click$lng, lat = click$lat,
                 popup = "Punto seleccionado")
  })

  resultado <- reactive({
    req(punto_seleccionado())
    click <- punto_seleccionado()

    punto_wgs84 <- st_as_sf(
      data.frame(lon = click$lng, lat = click$lat),
      coords = c("lon", "lat"), crs = 4326
    )
    punto_utm <- st_transform(punto_wgs84, 25830)

    dentro <- st_intersects(punto_utm, hull_concavo, sparse = FALSE)[1, 1]

    if (!dentro) {
      return(list(
        valido  = FALSE,
        mensaje = "Punto fuera del área de interpolación. Selecciona un punto dentro del polígono azul."
      ))
    }

    precio_log <- tryCatch({
      pt_vect <- vect(punto_utm)
      terra::extract(raster_pred, pt_vect)[1, 2]
    }, error = function(e) NA)

    varianza <- tryCatch({
      pt_vect <- vect(punto_utm)
      terra::extract(raster_var, pt_vect)[1, 2]
    }, error = function(e) NA)

    # kriging puntual como fallback en bordes del hull
    if (is.na(precio_log)) {
      punto_utm$x_utm <- st_coordinates(punto_utm)[1]
      punto_utm$y_utm <- st_coordinates(punto_utm)[2]

      pred_puntual <- tryCatch(
        krige(formula_krig, locations = train_sf,
              newdata = punto_utm, model = variograma_ajust),
        error = function(e) NULL
      )
      if (!is.null(pred_puntual)) {
        precio_log <- pred_puntual$var1.pred
        varianza   <- pred_puntual$var1.var
      }
    }

    if (is.na(precio_log)) {
      return(list(valido = FALSE, mensaje = "No se pudo calcular la predicción."))
    }

    precio_m2 <- exp(precio_log)
    ic_lower  <- exp(precio_log - 1.96 * sqrt(varianza))
    ic_upper  <- exp(precio_log + 1.96 * sqrt(varianza))

    list(
      valido     = TRUE,
      precio_log = round(precio_log, 4),
      varianza   = round(varianza, 6),
      precio_m2  = round(precio_m2),
      ic_lower   = round(ic_lower),
      ic_upper   = round(ic_upper),
      lon        = click$lng,
      lat        = click$lat
    )
  })

  output$resultado_box <- renderUI({
    if (is.null(punto_seleccionado())) {
      return(p("Haz clic en el mapa para obtener una valoración.", style = "color:grey;"))
    }
    r <- resultado()
    if (!r$valido) {
      return(div(class = "alert alert-warning", r$mensaje))
    }
    div(
      style = "background:#f0f7ff; padding:12px; border-radius:6px; border-left:4px solid steelblue;",
      h3(paste0(format(r$precio_m2, big.mark = "."), " €/m²"),
         style = "margin:0; color:steelblue;"),
      p(paste0("IC 95%: [", format(r$ic_lower, big.mark = "."),
               " — ", format(r$ic_upper, big.mark = "."), "] €/m²"),
        style = "margin:4px 0; font-size:0.9em;"),
      p(paste0("log(precio): ", r$precio_log, " | Varianza: ", r$varianza),
        style = "margin:0; font-size:0.8em; color:grey;")
    )
  })

  output$tabla_resultado <- renderTable({
    req(resultado())
    r <- resultado()
    if (!r$valido) return(NULL)
    data.frame(
      Métrica = c("Precio estimado (€/m²)", "IC 95% inferior", "IC 95% superior",
                  "log(precio)", "Varianza", "Longitud", "Latitud"),
      Valor   = c(format(r$precio_m2, big.mark = "."),
                  format(r$ic_lower, big.mark = "."),
                  format(r$ic_upper, big.mark = "."),
                  r$precio_log, r$varianza,
                  round(r$lon, 5), round(r$lat, 5))
    )
  }, striped = TRUE, hover = TRUE)

  output$grafico_pred <- renderPlot({
    req(resultado())
    r <- resultado()
    if (!r$valido) return(NULL)

    precio_range     <- seq(r$ic_lower * 0.8, r$ic_upper * 1.2, length.out = 200)
    precio_log_range <- log(precio_range)
    dens             <- dnorm(precio_log_range, mean = r$precio_log, sd = sqrt(r$varianza))

    plot(precio_range, dens,
         type = "l", lwd = 2, col = "steelblue",
         xlab = "Precio (€/m²)", ylab = "Densidad",
         main = "Distribución de la Predicción Kriging")
    abline(v = r$precio_m2, col = "red", lwd = 2)
    abline(v = c(r$ic_lower, r$ic_upper), col = "orange", lwd = 1.5, lty = 2)

    idx_ic <- precio_range >= r$ic_lower & precio_range <= r$ic_upper
    polygon(c(precio_range[idx_ic], rev(precio_range[idx_ic])),
            c(dens[idx_ic], rep(0, sum(idx_ic))),
            col = adjustcolor("steelblue", alpha.f = 0.2), border = NA)

    legend("topright",
           legend = c(paste0("Predicción: ", format(r$precio_m2, big.mark = "."), " €/m²"),
                      "IC 95%"),
           col = c("red", "orange"), lty = c(1, 2), lwd = 2, cex = 0.8)
  })

  output$tabla_metricas <- renderTable({
    data.frame(
      Métrica = c("RMSE (log)", "MAE (log)", "R² (log)", "RMSE (€/m²)", "MAPE (%)"),
      Valor   = round(c(metricas$rmse_log, metricas$mae_log, metricas$r2_log,
                        metricas$rmse_eur, metricas$mape), 4)
    )
  }, striped = TRUE, digits = 4)
}

shinyApp(ui = ui, server = server)
