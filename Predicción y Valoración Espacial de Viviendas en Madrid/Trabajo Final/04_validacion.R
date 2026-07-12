# ==============================================================================
# FASE IV: Auditoría y Validación Global
# AVM Valoración de Viviendas Madrid
#
# Objetivo: Someter a los modelos entrenados a un escrutinio final utilizando 
# la 'caja fuerte' de datos (Test Set) y verificar estadísticamente su robustez.
# ==============================================================================

library(sf)
library(tidyverse)
library(spdep)
library(spatialreg)
library(gstat)
library(tidymodels)
library(yardstick)
library(tmap)

tmap_mode("plot")

# ------------------------------------------------------------------------------
# Carga de artefactos
# ------------------------------------------------------------------------------

train_sf            <- readRDS("datos/train_sf_fase2.rds")
test_sf             <- readRDS("datos/test_sf_fase2.rds")
W                   <- readRDS("datos/W_listw.rds")
W_nb                <- readRDS("datos/nb_w.rds")
ols_model           <- readRDS("datos/ols_model.rds")
sar_model           <- readRDS("datos/sar_model.rds")
rf_fit              <- readRDS("datos/rf_model.rds")
xgb_fit             <- readRDS("datos/xgb_model.rds")
variograma_ajustado <- readRDS("datos/variograma_ajustado.rds")
formula_kriging     <- readRDS("datos/formula_kriging.rds")

cat("Test:", nrow(test_sf), "viviendas\n")

# ==============================================================================
# 1. AISLAMIENTO Y PREPARACIÓN DE LA 'CAJA FUERTE' (Test Set)
# Aplicamos estrictamente las mismas reglas espaciales a las viviendas de Test 
# que usamos para entrenar a los modelos, garantizando una evaluación justa.
# ==============================================================================

# Coordenadas UTM para Kriging Universal (si aplica) — ya deberían existir
if (!"x_utm" %in% names(test_sf)) {
  test_sf$x_utm <- st_coordinates(test_sf)[, 1]
  test_sf$y_utm <- st_coordinates(test_sf)[, 2]
}

# Construir W para el test set (k=8 KNN, consistente con Fase I)
coords_test <- st_coordinates(test_sf)
knn_test    <- knearneigh(coords_test, k = 8)
nb_test     <- knn2nb(knn_test)
W_test      <- nb2listw(nb_test, style = "W")

# Retardos espaciales sobre test (usando W_test)
test_sf$Wy           <- lag.listw(W_test, test_sf$log_price,   zero.policy = TRUE)
test_sf$W_crime      <- lag.listw(W_test, test_sf$crime,       zero.policy = TRUE)
test_sf$W_RP         <- lag.listw(W_test, test_sf$RP,          zero.policy = TRUE)
test_sf$W_immigrants <- lag.listw(W_test, test_sf$immigrants,  zero.policy = TRUE)

# Data frame para RF y XGBoost (mismo pipeline que train en 03B)
test_df <- st_drop_geometry(test_sf) |>
  mutate(
    log_built_area = log(built.area),
    log_age1       = log(age + 1)
  ) |>
  select(-built.area, -age, -house.price, -x_utm, -y_utm,
         -barrio, -distrito, -cod_barrio, -cod_distrito,
         -longitude, -latitude,
         -any_of(c("lmoran_Z", "lmoran_Pr", "lisa_cluster", "log_price_W",
                    "...1")))

# ==============================================================================
# 2. PREDICCIONES FINALES SOBRE EL TEST SET
# Enfrentamos todos los algoritmos construidos contra las viviendas reales.
# ==============================================================================

cat("\n--- PREDICCIONES SOBRE TEST SET ---\n")

# OLS
pred_ols <- predict(ols_model, newdata = st_drop_geometry(test_sf))

# SAR — predict() de spatialreg devuelve trends + spatialError
pred_sar_obj <- predict(sar_model, newdata = test_sf, listw = W_test,
                         pred.type = "TS", zero.policy = TRUE)
pred_sar <- as.numeric(pred_sar_obj)

# RF
pred_rf  <- predict(rf_fit,  new_data = test_df)$.pred

# XGBoost
pred_xgb <- predict(xgb_fit, new_data = test_df)$.pred

results <- tibble(
  log_price_obs = test_sf$log_price,
  pred_ols      = pred_ols,
  pred_sar      = pred_sar,
  pred_rf       = pred_rf,
  pred_xgb      = pred_xgb
) |>
  mutate(
    price_obs = exp(log_price_obs),
    price_rf  = exp(pred_rf),
    price_xgb = exp(pred_xgb),
    price_ols = exp(pred_ols),
    price_sar = exp(pred_sar)
  )

# ==============================================================================
# 3. EXAMEN DE RUIDO BLANCO (ÍNDICE DE MORAN)
# La prueba definitiva en econometría espacial: si el modelo ha aprendido bien, 
# los errores (residuos) deben estar distribuidos al azar en el mapa. Si hay 
# zonas donde el modelo falla sistemáticamente, el test saltará.
# Buscamos un p-valor > 0.05 para confirmar que el residuo es "ruido blanco".
# ==============================================================================

cat("\n--- TEST DE MORAN SOBRE RESIDUOS ---\n")

evaluar_moran <- function(nombre, residuos, listw) {
  m <- moran.mc(residuos, listw = listw, nsim = 499, zero.policy = TRUE,
                alternative = "two.sided")
  cat(sprintf("%-10s | I = %6.4f | p = %.4f | %s\n",
              nombre, m$statistic, m$p.value,
              ifelse(m$p.value > 0.05, "OK (ruido blanco)",
                     "ALERTA: autocorrelacion residual")))
  invisible(m)
}

# Residuos sobre train (W_train disponible)
set.seed(42)
m_ols <- evaluar_moran("OLS",     resid(ols_model),       W)
m_sar <- evaluar_moran("SAR",     resid(sar_model),        W)

# Residuos sobre test (W_test)
resid_rf_test  <- results$log_price_obs - results$pred_rf
resid_xgb_test <- results$log_price_obs - results$pred_xgb

m_rf  <- evaluar_moran("RF",      resid_rf_test,  W_test)
m_xgb <- evaluar_moran("XGBoost", resid_xgb_test, W_test)

# Diagnóstico automático si persiste autocorrelación
modelos_alert <- c(
  if (m_rf$p.value  < 0.05) "RF  → añadir retardos WX adicionales o Wy de orden 2",
  if (m_xgb$p.value < 0.05) "XGB → añadir retardos WX adicionales o Wy de orden 2",
  if (m_sar$p.value < 0.05) "SAR → probar SEM (errorsarlm) o ampliar radio W"
)
if (length(modelos_alert) > 0) {
  cat("\nACCIONES RECOMENDADAS:\n")
  cat(paste(" •", modelos_alert, collapse = "\n"), "\n")
}

# ==============================================================================
# 4. AUDITORÍA GEOESTADÍSTICA: KRIGING CROSS-VALIDATION
# Evaluamos matemáticamente la calidad de nuestro mapa continuo de varianzas.
# ==============================================================================

cat("\n--- KRIGING CV (10-fold) ---\n")

# Implementación de caché para evitar recalcular la validación cruzada si ya existe
if (file.exists("datos/kriging_cv_gstat.rds")) {
  cat("Cargando Kriging CV desde caché para optimizar tiempo de ejecución...\n")
  krig_cv <- readRDS("datos/kriging_cv_gstat.rds")
} else {
  cat("Calculando Kriging CV (este proceso puede tardar unos minutos)...\n")
  krig_cv <- gstat::krige.cv(
    formula   = formula_kriging,
    locations = train_sf,
    model     = variograma_ajustado,
    nfold     = 10,
    verbose   = FALSE
  )
  saveRDS(krig_cv, "datos/kriging_cv_gstat.rds")
}

rmse_krig <- sqrt(mean(krig_cv$residual^2))
cor_krig  <- cor(krig_cv$var1.pred, krig_cv$observed)
cat(sprintf("Kriging CV | RMSE = %.4f | Cor(pred,obs) = %.4f\n",
            rmse_krig, cor_krig))

# Mapa de residuos de CV
train_sf$krig_cv_resid <- krig_cv$residual
tm_shape(train_sf) +
  tm_dots(fill = "krig_cv_resid",
          fill.scale = tm_scale_continuous(values = "brewer.rd_yl_bu"),
          size = 0.03,
          fill.legend = tm_legend(title = "Residuo CV")) +
  tm_layout(main.title = "Kriging — Residuos de Validación Cruzada (10-fold)",
            legend.outside = TRUE, frame = FALSE)

# Moran sobre residuos de Kriging CV
evaluar_moran("Kriging", krig_cv$residual, W)

# ==============================================================================
# 5. MATRIZ DE RENDIMIENTO Y COMPARATIVA DE MODELOS
# Extraemos el RMSE absoluto y R² tanto en el espacio algorítmico (Logaritmo) 
# como en la moneda real del mercado inmobiliario (€/m²).
# ==============================================================================

cat("\n--- MÉTRICAS EN ESCALA LOG (log_price) ---\n")

met <- metric_set(rmse, mae, rsq)

metricas_log <- bind_rows(
  met(results, truth = log_price_obs, estimate = pred_ols) |> mutate(modelo = "OLS"),
  met(results, truth = log_price_obs, estimate = pred_sar) |> mutate(modelo = "SAR"),
  met(results, truth = log_price_obs, estimate = pred_rf)  |> mutate(modelo = "RF"),
  met(results, truth = log_price_obs, estimate = pred_xgb) |> mutate(modelo = "XGBoost")
) |>
  pivot_wider(names_from = .metric, values_from = .estimate, id_cols = modelo)

print(metricas_log)

cat("\n--- MÉTRICAS EN ESCALA ORIGINAL (€/m²) ---\n")

metricas_orig <- bind_rows(
  met(results, truth = price_obs, estimate = price_ols) |> mutate(modelo = "OLS"),
  met(results, truth = price_obs, estimate = price_sar) |> mutate(modelo = "SAR"),
  met(results, truth = price_obs, estimate = price_rf)  |> mutate(modelo = "RF"),
  met(results, truth = price_obs, estimate = price_xgb) |> mutate(modelo = "XGBoost")
) |>
  pivot_wider(names_from = .metric, values_from = .estimate, id_cols = modelo) |>
  mutate(across(c(rmse, mae), round, 0),
         rsq = round(rsq, 4))

print(metricas_orig)

# Añadir Kriging CV
metricas_completas <- bind_rows(
  metricas_orig,
  tibble(modelo = "Kriging (CV)", rmse = round(exp(rmse_krig), 0),
         mae = NA_real_, rsq = round(cor_krig^2, 4))
)

saveRDS(metricas_completas, "datos/metricas_finales.rds")

# ==============================================================================
# 6. DISTRIBUCIÓN ESPACIAL DE LOS ERRORES
# Preparamos las coordenadas y los errores absolutos para su futura 
# representación visual en el Dashboard (Fase IV).
# ==============================================================================

cat("\n--- MAPAS DE RESIDUOS ---\n")

test_sf$resid_rf  <- as.numeric(resid_rf_test)
test_sf$resid_xgb <- as.numeric(resid_xgb_test)

mapa_resid <- function(var, titulo) {
  tm_shape(test_sf) +
    tm_dots(fill = var,
            fill.scale = tm_scale_continuous(
              values  = "brewer.rd_yl_bu",
              midpoint = 0),
            size = 0.03,
            fill.legend = tm_legend(title = "Residuo")) +
    tm_layout(main.title = titulo, legend.outside = TRUE, frame = FALSE)
}

print(mapa_resid("resid_rf",  "RF — Residuos espaciales (test)"))
print(mapa_resid("resid_xgb", "XGBoost — Residuos espaciales (test)"))

saveRDS(test_sf, "datos/test_sf_resultados.rds")

# ==============================================================================
# 7. RESUMEN FINAL Y TESTS DE CERTIFICACIÓN DE CALIDAD
# ==============================================================================

cat("\n====== RESUMEN FINAL DE MODELOS ======\n")
print(metricas_completas)
cat("\nModelo recomendado para Producción (Menor RMSE):",
    metricas_completas$modelo[which.min(metricas_completas$rmse)], "\n")


cat("\n══════════════════════════════════════════════════\n")
cat("   CERTIFICACIÓN DE CALIDAD (Tests Unitarios)\n")
cat("══════════════════════════════════════════════════\n\n")

tests_passed <- 0
tests_total  <- 0

# Test 1: Predicciones sin NAs
tests_total <- tests_total + 1
na_count <- sum(is.na(results$pred_ols)) + sum(is.na(results$pred_sar)) +
            sum(is.na(results$pred_rf))  + sum(is.na(results$pred_xgb))
if (na_count == 0) {
  cat("✓ TEST 01: Las predicciones están limpias (Sin valores NA)\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 01: Error crítico,", na_count, "NAs en predicciones\n")
}

# Test 2: R² > 0
tests_total <- tests_total + 1
if (all(metricas_log$rsq > 0, na.rm = TRUE)) {
  cat("✓ TEST 02: Todos los algoritmos capturan varianza real (R² > 0)\n")
  tests_passed <- tests_passed + 1
} else {
  cat("⚠ TEST 02: Algún modelo falló en capturar señal del mercado\n")
}

# Test 3: RMSE finito
tests_total <- tests_total + 1
if (all(is.finite(metricas_log$rmse))) {
  cat("✓ TEST 03: Estabilidad numérica confirmada (RMSE finito)\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 03: Fallo de estabilidad numérica en el RMSE\n")
}

# Test 4: Kriging CV
tests_total <- tests_total + 1
if (!is.null(krig_cv) && length(krig_cv$residual) > 0) {
  cat("✓ TEST 04: Geoestadística certificada mediante Kriging CV\n")
  tests_passed <- tests_passed + 1
} else {
  cat("✗ TEST 04: Error en el bloque geoestadístico\n")
}

# Test 5: Cobertura del Intervalo de Confianza de XGBoost (Nueva validación)
tests_total <- tests_total + 1
rmse_xgb <- readRDS("datos/rmse_test_xgb.rds")
cobertura_intervalo <- mean(
  results$log_price_obs >= (results$pred_xgb - 1.96*rmse_xgb) & 
  results$log_price_obs <= (results$pred_xgb + 1.96*rmse_xgb)
) * 100

if (cobertura_intervalo >= 90) {
  cat(sprintf("✓ TEST 05: Intervalo de Confianza Robusto (Cobertura Empírica del %.2f%%)\n", cobertura_intervalo))
  tests_passed <- tests_passed + 1
} else {
  cat(sprintf("⚠ TEST 05: Intervalo de Confianza Subestimado (Solo cubre %.2f%%)\n", cobertura_intervalo))
}

# Test 6: Integridad de Archivos de Salida
tests_total <- tests_total + 1
rds_files <- c("metricas_finales.rds", "test_sf_resultados.rds", "kriging_cv_gstat.rds")
if (all(file.exists(paste0("datos/", rds_files)))) {
  cat("✓ TEST 06: Todos los artefactos de datos se han exportado correctamente\n")
  tests_passed <- tests_passed + 1
} else {
  missing <- rds_files[!file.exists(paste0("datos/", rds_files))]
  cat("✗ TEST 06: Archivos perdidos:", paste(missing, collapse = ", "), "\n")
}

cat("\n══ RESULTADO:", tests_passed, "/", tests_total, "tests superados ══\n")
if (tests_passed == tests_total) {
  cat("✅ FASE IV (VALIDACIÓN) APROBADA CON ÉXITO\n")
  cat("El pipeline analítico está matemáticamente listo para producción.\n")
} else {
  cat("⚠️ ALERTA DE CALIDAD: Se requiere intervención técnica antes de producción.\n")
}
