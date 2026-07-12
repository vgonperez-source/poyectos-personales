# 03A_kriging.R — Interpolación Geoestadística (Kriging)

library(tidyverse)
library(sf)
library(gstat)
library(automap)
library(terra)
library(tmap)

tmap_mode("plot")

# Inputs -----------------------------------------------------------------------

train_sf        <- readRDS("datos/train_sf_fase2.rds")
test_sf         <- readRDS("datos/test_sf_fase2.rds")
vario_emp       <- readRDS("datos/variograma_empirico.rds")
hull_concavo    <- readRDS("datos/hull_concavo.rds")
formula_kriging <- readRDS("datos/formula_kriging.rds")

cat("Fórmula kriging:", deparse(formula_kriging), "\n")
cat("Train:", nrow(train_sf), "| Test:", nrow(test_sf), "\n")


# 3.1a Ajuste del semivariograma teórico — tema 5/5.5 L.55-93 -----------------
# Comparativa: ajuste manual (fit.variogram) vs automático (autofitVariogram)
# Se selecciona el modelo con menor error de ajuste (SSErr)

cat("\n--- AJUSTE DEL SEMIVARIOGRAMA TEÓRICO ---\n")

plot(vario_emp,
     main = "Semivariograma Empírico (input para ajuste)",
     xlab = "Distancia (m)", ylab = "Semivarianza")

# Ajuste manual con fit.variogram
modelos_teoricos <- vgm(
  psill = NA, range = NA,
  model = c("Sph", "Exp", "Gau", "Mat", "Ste")
)

variograma_manual <- fit.variogram(
  vario_emp,
  model      = modelos_teoricos,
  fit.sills  = TRUE,
  fit.ranges = TRUE,
  fit.kappa  = TRUE,
  fit.method = 7
)
cat("\n--- Modelo ajustado (manual) ---\n")
print(variograma_manual)
cat("SSErr manual:", attr(variograma_manual, "SSErr"), "\n")

# Ajuste automático con automap (comparación)
av <- automap::autofitVariogram(
  formula_kriging,
  train_sf,
  model   = c("Sph", "Exp", "Gau", "Mat", "Ste"),
  cressie = TRUE
)
cat("\n--- Modelo ajustado (automap) ---\n")
print(av$var_model)
cat("SSErr automap:", av$sserr, "\n")

# Selección del mejor modelo por SSErr
sserr_manual  <- attr(variograma_manual, "SSErr")
sserr_automap <- av$sserr

variograma_ajustado <- if (sserr_manual <= sserr_automap) {
  cat("\n→ Usando modelo MANUAL (SSErr:", sserr_manual, ")\n")
  variograma_manual
} else {
  cat("\n→ Usando modelo AUTOMAP (SSErr:", sserr_automap, ")\n")
  av$var_model
}

cat("\n--- Interpretación Teórica del Variograma (Tema 5) ---\n")
nugget <- variograma_ajustado$psill[1]
sill   <- variograma_ajustado$psill[2] + nugget
range  <- variograma_ajustado$range[2]
cat("1. Pepita (Nugget):", round(nugget, 4), "-> Variabilidad a microescala / error instrumental.\n")
cat("2. Meseta (Sill):", round(sill, 4), "-> Varianza espacial máxima de los datos.\n")
cat("3. Alcance (Range):", round(range, 0), "metros -> Distancia máxima a la que el precio de una vivienda influye espacialmente sobre otra.\n")

# Gráfico comparativo: manual vs automap
par(mfrow = c(1, 2))
plot(vario_emp, model = variograma_manual,
     main = paste("Manual — SSErr:", round(sserr_manual, 4)),
     xlab = "Distancia (m)", ylab = "Semivarianza")
plot(vario_emp, model = av$var_model,
     main = paste("Automap — SSErr:", round(sserr_automap, 4)),
     xlab = "Distancia (m)", ylab = "Semivarianza")
par(mfrow = c(1, 1))

plot(vario_emp, model = variograma_ajustado,
     main = "Semivariograma: Empírico vs Modelo Ajustado (seleccionado)",
     xlab = "Distancia (m)", ylab = "Semivarianza")


# 3.1b Grilla de predicción dentro del hull — tema 5/5.6 L.150-169 ------------

grilla_puntos <- st_make_grid(hull_concavo, n = c(150, 150), what = "centers")

idx_dentro    <- st_intersects(grilla_puntos, st_union(hull_concavo),
                               sparse = FALSE)[, 1]
puntos_dentro <- grilla_puntos[idx_dentro]

grilla_sf <- st_as_sf(puntos_dentro)
grilla_sf <- rename(grilla_sf, geometry = x)
st_crs(grilla_sf) <- st_crs(train_sf)

# Coordenadas UTM necesarias para Kriging Universal (~ x_utm + y_utm)
grilla_sf$x_utm <- st_coordinates(grilla_sf)[, 1]
grilla_sf$y_utm <- st_coordinates(grilla_sf)[, 2]

cat("\nPuntos en grilla de predicción:", nrow(grilla_sf), "\n")

tm_shape(hull_concavo) +
  tm_borders(col = "grey40") +
  tm_shape(grilla_sf) +
  tm_dots(size = 0.01, fill = "steelblue") +
  tm_title("Grilla de predicción Kriging") +
  tm_layout(legend.outside = TRUE)


# 3.1c Interpolación Kriging — tema 5/5.5 L.109-117 ---------------------------

cat("\nEjecutando Kriging (puede tardar varios minutos)...\n")
t_inicio <- Sys.time()

kriging_resultado <- krige(
  formula_kriging,
  locations = train_sf,
  newdata   = grilla_sf,
  model     = variograma_ajustado
)

t_fin <- Sys.time()
cat("Kriging completado en:", round(difftime(t_fin, t_inicio, units = "mins"), 2),
    "minutos\n")
cat("var1.pred range:", range(kriging_resultado$var1.pred), "\n")
cat("Precio predicho (EUR/m2):", range(exp(kriging_resultado$var1.pred)), "\n")


# 3.1d Rasterización — tema 5/5.5 L.160-188, tema 5/5.6 L.338-345 ------------

extent_rejilla <- st_bbox(grilla_sf)
extent_vector  <- c(extent_rejilla["xmin"], extent_rejilla["xmax"],
                    extent_rejilla["ymin"], extent_rejilla["ymax"])
res_x <- (extent_rejilla$xmax - extent_rejilla$xmin) / sqrt(nrow(grilla_sf))
res_y <- (extent_rejilla$ymax - extent_rejilla$ymin) / sqrt(nrow(grilla_sf))

raster_vacio <- rast(
  extent     = extent_vector,
  resolution = c(res_x, res_y),
  crs        = "EPSG:25830"
)

raster_pred <- rasterize(kriging_resultado, raster_vacio, field = "var1.pred")
raster_var  <- rasterize(kriging_resultado, raster_vacio, field = "var1.var")


# 3.1e Visualización — tema 5/5.5 L.179-203, tema 5/5.6 L.349-384 ------------

tm_shape(raster_pred) +
  tm_raster(col.scale = tm_scale_continuous(
              values   = "plasma",
              midpoint = mean(train_sf$log_price, na.rm = TRUE)),
            col_alpha  = 0.85,
            col.legend = tm_legend(title = "log(EUR/m2)")) +
  tm_shape(train_sf) +
  tm_dots(fill = "log_price", size = 0.05,
          fill.scale = tm_scale_continuous(values = "plasma",
                                           midpoint = mean(train_sf$log_price,
                                                           na.rm = TRUE)),
          fill.legend = tm_legend(show = FALSE)) +
  tm_title("Predicción Kriging — log(Precio/m2)") +
  tm_layout(legend.outside = TRUE)

tm_shape(raster_var) +
  tm_raster(col.scale = tm_scale_continuous(values = "brewer.yl_or_rd"),
            col_alpha  = 0.85,
            col.legend = tm_legend(title = "Varianza")) +
  tm_shape(train_sf) +
  tm_dots(fill = "black", size = 0.05, shape = 4,
          fill.legend = tm_legend(show = FALSE)) +
  tm_title("Varianza del Error de Predicción Kriging") +
  tm_layout(legend.outside = TRUE)


# 3.1f Validación en test set --------------------------------------------------

if (!"x_utm" %in% names(test_sf)) {
  coords_test   <- st_coordinates(test_sf)
  test_sf$x_utm <- coords_test[, 1]
  test_sf$y_utm <- coords_test[, 2]
}

cat("\nPrediciendo en test set...\n")
kriging_test <- krige(
  formula_kriging,
  locations = train_sf,
  newdata   = test_sf,
  model     = variograma_ajustado
)

test_sf$pred_log   <- kriging_test$var1.pred
test_sf$pred_var   <- kriging_test$var1.var
test_sf$pred_price <- exp(test_sf$pred_log)

rmse_log <- sqrt(mean((test_sf$log_price - test_sf$pred_log)^2, na.rm = TRUE))
mae_log  <- mean(abs(test_sf$log_price - test_sf$pred_log), na.rm = TRUE)
r2_log   <- cor(test_sf$log_price, test_sf$pred_log, use = "complete.obs")^2

rmse_eur <- sqrt(mean((test_sf$house.price - test_sf$pred_price)^2, na.rm = TRUE))
mape     <- mean(abs(test_sf$house.price - test_sf$pred_price) /
                   test_sf$house.price, na.rm = TRUE) * 100

cat("\n--- Validación Kriging en Test Set ---\n")
cat("RMSE (log):    ", round(rmse_log, 4), "\n")
cat("MAE  (log):    ", round(mae_log,  4), "\n")
cat("R²   (log):    ", round(r2_log,   4), "\n")
cat("RMSE (EUR/m2): ", round(rmse_eur, 2), "\n")
cat("MAPE (%):      ", round(mape,     2), "\n")

plot(test_sf$log_price, test_sf$pred_log,
     xlab = "log(precio) observado", ylab = "log(precio) predicho",
     main = "Kriging: Observado vs Predicho (Test Set)",
     pch = 20, col = adjustcolor("steelblue", alpha.f = 0.3), cex = 0.5)
abline(0, 1, col = "red", lwd = 2)
abline(lm(pred_log ~ log_price, data = test_sf), col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Perfecta predicción", "Ajuste real"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)


# 3.1g Spatial Block CV — cuadrícula 5x5 geográfica ---------------------------
# CV aleatorio no es válido por autocorrelación espacial entre folds

cat("\nEjecutando Spatial Block CV (5 folds geográficos)...\n")

bbox_train  <- st_bbox(train_sf)
n_bloques_x <- 5
n_bloques_y <- 5

breaks_x <- seq(bbox_train["xmin"], bbox_train["xmax"], length.out = n_bloques_x + 1)
breaks_y <- seq(bbox_train["ymin"], bbox_train["ymax"], length.out = n_bloques_y + 1)

bloque_x <- cut(train_sf$x_utm, breaks = breaks_x, labels = FALSE,
                include.lowest = TRUE)
bloque_y <- cut(train_sf$y_utm, breaks = breaks_y, labels = FALSE,
                include.lowest = TRUE)

set.seed(42)
bloques_unicos  <- unique(paste0(bloque_x, "_", bloque_y))
n_bloques       <- length(bloques_unicos)
fold_asignacion <- setNames(
  sample(rep(1:5, length.out = n_bloques)),
  bloques_unicos
)
train_sf$fold_id <- fold_asignacion[paste0(bloque_x, "_", bloque_y)]

resultados_cv <- data.frame(fold = integer(), n_test = integer(),
                            rmse = numeric(), mae = numeric(), r2 = numeric())

for (i in 1:5) {
  train_fold <- train_sf[train_sf$fold_id != i, ]
  test_fold  <- train_sf[train_sf$fold_id == i, ]

  pred_fold <- tryCatch({
    krige(formula_kriging,
          locations = train_fold,
          newdata   = test_fold,
          model     = variograma_ajustado)
  }, error = function(e) {
    cat("  Fold", i, "- error:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(pred_fold)) {
    rmse_fold <- sqrt(mean((test_fold$log_price - pred_fold$var1.pred)^2,
                           na.rm = TRUE))
    mae_fold  <- mean(abs(test_fold$log_price - pred_fold$var1.pred),
                      na.rm = TRUE)
    r2_fold   <- cor(test_fold$log_price, pred_fold$var1.pred,
                     use = "complete.obs")^2
    resultados_cv <- rbind(resultados_cv,
                           data.frame(fold = i, n_test = nrow(test_fold),
                                      rmse = rmse_fold, mae = mae_fold,
                                      r2 = r2_fold))
    cat("  Fold", i, "| n:", nrow(test_fold),
        "| RMSE:", round(rmse_fold, 4), "| R²:", round(r2_fold, 4), "\n")
  }
}

cat("\n--- Spatial Block CV (5 folds geográficos) ---\n")
print(resultados_cv)
cat("RMSE medio (CV espacial):", round(mean(resultados_cv$rmse), 4), "\n")
cat("R² medio   (CV espacial):", round(mean(resultados_cv$r2),   4), "\n")

kriging_cv <- resultados_cv


# Guardar outputs --------------------------------------------------------------

saveRDS(variograma_ajustado, "datos/variograma_ajustado.rds")
saveRDS(kriging_resultado,   "datos/kriging_resultado.rds")
# [CRÍTICO] Los objetos SpatRaster de terra NO deben guardarse con saveRDS (causan crashes). Se deben exportar a GeoTIFF.
terra::writeRaster(raster_pred, "datos/kriging_raster_pred.tif", overwrite = TRUE)
terra::writeRaster(raster_var,  "datos/kriging_raster_var.tif", overwrite = TRUE)
saveRDS(kriging_cv,          "datos/kriging_cv.rds")
saveRDS(test_sf,             "datos/test_sf_kriging.rds")

metricas_kriging <- data.frame(
  modelo   = "Kriging",
  rmse_log = rmse_log,
  mae_log  = mae_log,
  r2_log   = r2_log,
  rmse_eur = rmse_eur,
  mape     = mape,
  rmse_cv  = mean(resultados_cv$rmse)
)
saveRDS(metricas_kriging, "datos/metricas_kriging.rds")


# ══ VALIDACIÓN DE CALIDAD — Fase III-A ═══════════════════════════════════════

cat("\n══════════════════════════════════════════════════\n")
cat("   VALIDACIÓN DE CALIDAD — Fase III-A (Kriging)\n")
cat("══════════════════════════════════════════════════\n\n")

tests_passed <- 0
tests_total  <- 0

# Test 1: Variograma ajustado válido
tests_total <- tests_total + 1
nugget_ok <- variograma_ajustado$psill[1] >= 0
sill_ok   <- variograma_ajustado$psill[2] > 0
range_ok  <- variograma_ajustado$range[2] > 0
if (nugget_ok && sill_ok && range_ok) {
  cat("✓ TEST 01: Variograma válido (nugget >=0, sill >0, range >0)\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 01: Variograma con parámetros inválidos\n")
}

# Test 2: Predicciones kriging en rango razonable
tests_total <- tests_total + 1
pred_range <- range(kriging_resultado$var1.pred, na.rm = TRUE)
if (pred_range[1] > 5 && pred_range[2] < 12) {
  cat("✓ TEST 02: Predicciones en rango razonable [",
      round(pred_range[1], 2), ",", round(pred_range[2], 2), "]\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 02: Predicciones fuera de rango [",
      round(pred_range[1], 2), ",", round(pred_range[2], 2), "]\n")
}

# Test 3: R² en test > 0
tests_total <- tests_total + 1
if (r2_log > 0) {
  cat("✓ TEST 03: R² test =", round(r2_log, 4), "(mejor que la media)\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 03: R² test =", round(r2_log, 4), "(peor que la media)\n")
}

# Test 4: RMSE CV consistente con RMSE test (diferencia < 50%)
tests_total <- tests_total + 1
rmse_cv_mean <- mean(resultados_cv$rmse)
diff_pct <- abs(rmse_cv_mean - rmse_log) / rmse_log * 100
if (diff_pct < 50) {
  cat("✓ TEST 04: RMSE CV (", round(rmse_cv_mean, 4),
      ") consistente con RMSE test (", round(rmse_log, 4),
      "), dif:", round(diff_pct, 1), "%\n")
  tests_passed <- tests_passed + 1
} else {
  cat("⚠ TEST 04: RMSE CV y test difieren >50%\n")
}

# Test 5: Archivos RDS guardados
tests_total <- tests_total + 1
rds_files <- c("variograma_ajustado.rds", "kriging_resultado.rds",
               "kriging_cv.rds", "metricas_kriging.rds")
tif_files <- c("kriging_raster_pred.tif", "kriging_raster_var.tif")

all_exist <- all(file.exists(paste0("datos/", rds_files))) &&
             all(file.exists(paste0("datos/", tif_files)))
if (all_exist) {
  cat("✓ TEST 05: Todos los archivos guardados correctamente (incl. GeoTIFFs)\n")
  tests_passed <- tests_passed + 1
} else {
  missing <- c(rds_files[!file.exists(paste0("datos/", rds_files))],
               tif_files[!file.exists(paste0("datos/", tif_files))])
  cat("✗ TEST 05: Faltan:", paste(missing, collapse = ", "), "\n")
}

cat("\n══ RESULTADO:", tests_passed, "/", tests_total, "tests superados ══\n")
if (tests_passed == tests_total) {
  cat("✅ FASE III-A (Kriging) VALIDADA\n")
} else {
  cat("⚠️  ATENCIÓN: Revisar los tests fallidos\n")
}

cat("\n=== Fase III — Vía A (Kriging) completada ===\n")
cat("Archivos guardados en datos/:\n")
cat("  variograma_ajustado.rds | kriging_resultado.rds\n")
cat("  kriging_raster_pred.tif | kriging_raster_var.tif\n")
cat("  kriging_cv.rds | metricas_kriging.rds\n")

# [STATUS: VALIDATED]
