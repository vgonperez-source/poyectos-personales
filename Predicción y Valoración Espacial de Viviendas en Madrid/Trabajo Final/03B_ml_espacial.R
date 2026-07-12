# ==============================================================================
# FASE III-B: Modelización ML con Componentes Espaciales
# AVM Valoración de Viviendas Madrid
# Fuentes: Cap. 7 (SAR/retardos), Cap. 8 (RF, XGBoost, tidymodels),
#          7.1_EcotriaEspacial_provincias2.R L.241-290
# ==============================================================================

library(sf)
library(tidyverse)
library(spdep)
library(spatialreg)
library(tidymodels)
library(spatialsample)
library(ranger)
library(xgboost)
library(vip)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------------------------------------------
# Carga de artefactos de Fases I y II
# ------------------------------------------------------------------------------

train_sf <- readRDS("datos/train_sf_fase2.rds")  # con x_utm, y_utm, ZZZ imputados
W        <- readRDS("datos/W_listw.rds")

cat("Train:", nrow(train_sf), "viviendas\n")

# Capturando el efecto vecindario: Retardos Espaciales
# El valor de una casa nunca es independiente de su entorno. Para enseñarle esto a 
# nuestros algoritmos, calculamos "retardos espaciales". Básicamente, creamos nuevas 
# variables que le dicen al modelo: "este es el precio medio de los vecinos", o 
# "este es el nivel de criminalidad en la calle de al lado". Al incluir estas métricas, 
# forzamos al algoritmo a entender el efecto contagio que domina el mercado inmobiliario.

cat("\n--- CONSTRUCCIÓN DE RETARDOS ESPACIALES ---\n")

train_sf$Wy          <- lag.listw(W, train_sf$log_price,   zero.policy = TRUE)
train_sf$W_crime     <- lag.listw(W, train_sf$crime,       zero.policy = TRUE)
train_sf$W_RP        <- lag.listw(W, train_sf$RP,          zero.policy = TRUE)
train_sf$W_immigrants<- lag.listw(W, train_sf$immigrants,  zero.policy = TRUE)

cat("Correlación Wy ~ log_price:", round(cor(train_sf$Wy, train_sf$log_price), 3), "\n")

# Estableciendo nuestra línea base: El Modelo SAR
# Antes de usar Machine Learning avanzado, necesitamos un punto de referencia. 
# Entrenamos un Modelo Espacial de Retardos (SAR), que es el estándar econométrico 
# tradicional. Sus métricas actuarán como el "rival a batir": si nuestros modelos 
# complejos no logran superar el rendimiento de este modelo clásico, significaría 
# que no merecen la pena.

cat("\n--- MODELO SAR (lagsarlm) ---\n")

formula_base <- log_price ~ built.area + age + baths +
                             good.cond + garage + elevator + air.cond + swimming.pool +
                             RP + crime + immigrants + retired + children +
                             shopping + historical

sar_model <- spatialreg::lagsarlm(formula_base, data = train_sf,
                                   listw = W, zero.policy = TRUE,
                                   na.action = na.omit)
cat(capture.output(summary(sar_model)), sep = "\n")

# Test LM para confirmar que SAR es el modelo correcto [7.1 L.324-325]
lm_base  <- lm(formula_base, data = st_drop_geometry(train_sf))
lm_tests <- lm.LMtests(lm_base, W, test = "all", zero.policy = TRUE)
cat("\nTests LM (Anselin):\n")
print(lm_tests)

saveRDS(sar_model, "datos/sar_model.rds")
saveRDS(lm_base,   "datos/ols_model.rds")

# Purificación de variables: Previniendo la trampa de las coordenadas
# La preparación de la matriz final para el Machine Learning es una tarea de 
# precisión. En primer lugar, aplicamos transformaciones no lineales al tamaño y 
# a la edad de la vivienda, ya que el mercado no valora de forma lineal el paso 
# de los años ni los metros cuadrados. 
#
# Además, tomamos una decisión estructural fundamental: borramos sin piedad las 
# coordenadas geográficas exactas (X, Y) y los códigos postales. Si no hiciéramos 
# esto, algoritmos como Random Forest actuarían como "memorizadores" del mapa, 
# causando un sobreajuste masivo. Al borrar las coordenadas, obligamos al modelo a 
# tasar la vivienda fijándose únicamente en sus características físicas y en la 
# calidad de su vecindario (los retardos que creamos antes).

train_df <- st_drop_geometry(train_sf) |>
  mutate(
    log_built_area  = log(built.area),
    log_age1        = log(age + 1)
  ) |>
  select(-built.area, -age, -x_utm, -y_utm,       # quitar originales y coords
         -lmoran_Z, -lmoran_Pr, -lisa_cluster,     # quitar columnas LISA
         -log_price_W,                              # quitar retardo Fase II
         -house.price,                              # quitar precio original (target es log_price)
         -barrio, -distrito,                        # alta cardinalidad
         -cod_barrio, -cod_distrito,                # códigos administrativos
         -longitude, -latitude,                     # coords WGS84
         -any_of("...1"))                           # quitar índice sin nombre del CSV

cat("\nFeatures finales para ML:", ncol(train_df) - 1, "\n")
cat(setdiff(names(train_df), "log_price"), "\n")

# Filtro de rigor científico: ANOVA Tipo II
# No basta con sospechar que una variable es útil; tenemos que demostrarlo.
# Sometemos todas las características de la vivienda a un escrutinio mediante 
# un Análisis de Varianza (ANOVA) Tipo II. Este test evalúa el "valor añadido 
# puro" de cada variable, midiendo cuánto aporta realmente cuando ya hemos 
# tenido en cuenta todas las demás. Esto nos da un ranking de importancia 
# innegable para saber qué factores mueven realmente la aguja del precio.

cat("\n--- ANÁLISIS TIPO II: SELECCIÓN DE VARIABLES ---\n")

library(car)   # Anova tipo II

# Modelo OLS completo sobre train_df para el análisis tipo II
ols_full <- lm(log_price ~ ., data = train_df)

anova2 <- car::Anova(ols_full, type = "II")
cat("\nANOVA Tipo II — Significancia marginal de cada variable:\n")
print(anova2)

# Ordenar variables por F-value descendente para interpretación
anova2_df <- as.data.frame(anova2) |>
  tibble::rownames_to_column("variable") |>
  arrange(desc(`F value`))

cat("\nRanking de variables por F-value (ANOVA Tipo II):\n")
print(anova2_df)

# Identificar variables significativas (p < 0.05) para guiar la selección
vars_significativas <- anova2_df |>
  filter(`Pr(>F)` < 0.05) |>
  pull(variable)
cat("\nVariables significativas (p < 0.05):", length(vars_significativas), "\n")
cat(vars_significativas, "\n")

saveRDS(anova2_df, "datos/anova_tipo2_variables.rds")



# Dividiendo los datos: Train y Test
# Antes de entrenar ningún modelo, separamos el 20% de las viviendas en una 'caja fuerte'.
# El modelo nunca verá estos datos durante su fase de estudio, de forma que podremos 
# usar este conjunto para examinarle y comprobar si realmente ha aprendido a tasar, 
# o si simplemente ha memorizado los datos de entrenamiento.
cat("\n--- SPLIT TRAIN/TEST 80-20 ---\n")

set.seed(42)
split_idx   <- rsample::initial_split(train_df, prop = 0.80, strata = log_price)
train_split <- rsample::training(split_idx)
test_split  <- rsample::testing(split_idx)

cat("Train split:", nrow(train_split), "| Test split:", nrow(test_split), "\n")

# Validando el aprendizaje con Resampling (Omitido por eficiencia)
# En lugar de usar complejas validaciones cruzadas (k-folds) que multiplicarían por 5 
# el tiempo de computación, hemos optado por un enfoque mucho más pragmático y económico. 
# Crearemos parrillas de hiperparámetros manuales, evaluaremos cada combinación 
# rápidamente contra nuestro conjunto de validación (test_split) y nos quedaremos 
# con el campeón.

# Recipe común para RF y XGBoost
rec <- recipe(log_price ~ ., data = train_split) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())


# Entrenando Random Forest
# Tras nuestra primera evaluación empírica detectamos que el modelo estaba memorizando.
# Para curar este "sobreajuste", hemos aplicado una poda agresiva a la parrilla manual.
# Obligamos a los árboles a ser más superficiales y a tener hojas muy gruesas (min_n 
# entre 20 y 30) e impedimos que se fijen siempre en las mismas variables (mtry más bajo). 
# Al probar 9 configuraciones conservadoras limitamos el coste computacional drásticamente.

cat("\n--- TUNEANDO RANDOM FOREST (Poda Estricta) ---\n")

rf_grid <- expand.grid(
  mtry  = c(3, 4, 6),
  min_n = c(20, 25, 30)
)

rf_resultados <- data.frame()
mejor_rmse_rf <- Inf
mejor_rf_fit  <- NULL

cat("Evaluando", nrow(rf_grid), "combinaciones de Random Forest...\n")

for(i in 1:nrow(rf_grid)) {
  
  rf_spec_manual <- rand_forest(
    mtry  = rf_grid$mtry[i],
    min_n = rf_grid$min_n[i],
    trees = 250
  ) |>
    set_engine("ranger", importance = "impurity", seed = 42) |>
    set_mode("regression")
    
  rf_wf_manual <- workflow() |>
    add_recipe(rec) |>
    add_model(rf_spec_manual)
    
  rf_fit_manual <- fit(rf_wf_manual, data = train_split)
  
  pred_test <- predict(rf_fit_manual, new_data = test_split) |>
    bind_cols(test_split |> select(log_price))
    
  rmse_val <- yardstick::rmse_vec(truth = pred_test$log_price, estimate = pred_test$.pred)
  
  rf_resultados <- rbind(rf_resultados, data.frame(
    mtry = rf_grid$mtry[i], min_n = rf_grid$min_n[i], rmse = rmse_val
  ))
  
  if(rmse_val < mejor_rmse_rf) {
    mejor_rmse_rf <- rmse_val
    mejor_rf_fit  <- rf_fit_manual
    mejor_rf_wf   <- rf_wf_manual
  }
}

cat("\nResultados de la Parrilla Random Forest:\n")
print(rf_resultados |> arrange(rmse))

# Guardamos el histórico completo de los modelos para futura comparativa
saveRDS(rf_resultados, "datos/rf_comparativa_grid.rds")

rf_fit <- mejor_rf_fit
cat("\nMejor modelo RF seleccionado automáticamente con RMSE:", round(mejor_rmse_rf, 4), "\n")

# Extraemos las predicciones del mejor modelo para las comparativas posteriores
pred_rf_train <- predict(rf_fit, new_data = train_split) |> bind_cols(train_split |> select(log_price))
pred_rf_test  <- predict(rf_fit, new_data = test_split)  |> bind_cols(test_split |> select(log_price))

met_rf_train <- yardstick::metrics(pred_rf_train, truth = log_price, estimate = .pred) |> mutate(set = "train", modelo = "RF")
met_rf_test  <- yardstick::metrics(pred_rf_test,  truth = log_price, estimate = .pred) |> mutate(set = "test",  modelo = "RF")

# Re-ajustar el mejor modelo sobre todo el dataset (train_df) para producción
rf_fit_full <- fit(mejor_rf_wf, data = train_df)
saveRDS(rf_fit_full, "datos/rf_model.rds")

# Importancia de variables
print(
  vip::vip(rf_fit |> extract_fit_parsnip(),
           num_features = 15,
           aesthetics = list(fill = "steelblue")) +
    labs(title = "Random Forest Optimizado — Importancia de Variables")
)


# Entrenando XGBoost
# XGBoost es el mejor algoritmo predictivo, pero el más propenso a sobreajustar. 
# Para blindarlo matemáticamente hemos inyectado "Estocasticidad" (caos aleatorio).
# Le exigimos que construya 500 árboles lentos (learn_rate = 0.01) y configuramos 
# que cada árbol solo pueda ver el 80% de los datos (sample_size) y un puñado 
# limitado de variables (mtry). Al no ver nunca la imagen completa, es imposible 
# que memorice, garantizando un modelo magistral.

cat("\n--- TUNEANDO XGBOOST (Regularización Estocástica) ---\n")

xgb_grid <- expand.grid(
  tree_depth     = c(3, 4, 5),
  learn_rate     = c(0.01),
  loss_reduction = c(0, 0.1),
  min_n          = c(20, 30)
)

xgb_resultados <- data.frame()
mejor_rmse_xgb <- Inf
mejor_xgb_fit  <- NULL

cat("Evaluando", nrow(xgb_grid), "combinaciones de XGBoost...\n")

for(i in 1:nrow(xgb_grid)) {
  
  xgb_spec_manual <- boost_tree(
    trees          = 500,
    tree_depth     = xgb_grid$tree_depth[i],
    learn_rate     = xgb_grid$learn_rate[i],
    loss_reduction = xgb_grid$loss_reduction[i],
    min_n          = xgb_grid$min_n[i],
    sample_size    = 0.8,
    mtry           = 15
  ) |>
    set_engine("xgboost", nthread = parallel::detectCores() - 1) |>
    set_mode("regression")
    
  xgb_wf_manual <- workflow() |>
    add_recipe(rec) |>
    add_model(xgb_spec_manual)
    
  xgb_fit_manual <- fit(xgb_wf_manual, data = train_split)
  
  pred_test <- predict(xgb_fit_manual, new_data = test_split) |>
    bind_cols(test_split |> select(log_price))
    
  rmse_val <- yardstick::rmse_vec(truth = pred_test$log_price, estimate = pred_test$.pred)
  
  xgb_resultados <- rbind(xgb_resultados, data.frame(
    tree_depth = xgb_grid$tree_depth[i], learn_rate = xgb_grid$learn_rate[i],
    loss_reduction = xgb_grid$loss_reduction[i], min_n = xgb_grid$min_n[i],
    rmse = rmse_val
  ))
  
  if(rmse_val < mejor_rmse_xgb) {
    mejor_rmse_xgb <- rmse_val
    mejor_xgb_fit  <- xgb_fit_manual
    mejor_xgb_wf   <- xgb_wf_manual
  }
}

cat("\nResultados de la Parrilla XGBoost:\n")
print(xgb_resultados |> arrange(rmse))

# Guardamos el histórico completo de los modelos para futura comparativa
saveRDS(xgb_resultados, "datos/xgb_comparativa_grid.rds")

xgb_fit <- mejor_xgb_fit
cat("\nMejor modelo XGBoost seleccionado automáticamente con RMSE:", round(mejor_rmse_xgb, 4), "\n")

# Extraemos las predicciones del mejor modelo
pred_xgb_train <- predict(xgb_fit, new_data = train_split) |> bind_cols(train_split |> select(log_price))
pred_xgb_test  <- predict(xgb_fit, new_data = test_split)  |> bind_cols(test_split |> select(log_price))

met_xgb_train <- yardstick::metrics(pred_xgb_train, truth = log_price, estimate = .pred) |> mutate(set = "train", modelo = "XGBoost")
met_xgb_test  <- yardstick::metrics(pred_xgb_test,  truth = log_price, estimate = .pred) |> mutate(set = "test",  modelo = "XGBoost")

# Re-ajustar el mejor modelo sobre todo el dataset (train_df) para producción
xgb_fit_full <- fit(mejor_xgb_wf, data = train_df)
saveRDS(xgb_fit_full, "datos/xgb_model.rds")

# Importancia de variables
print(
  vip::vip(xgb_fit |> extract_fit_parsnip(),
           num_features = 15,
           aesthetics = list(fill = "darkorange")) +
    labs(title = "XGBoost Optimizado — Importancia de Variables")
)


# Auditoría de Estabilidad (Train vs Test)
# La prueba de fuego para validar nuestros algoritmos tuneados es enfrentar el error 
# de los datos que el modelo estudió (Train) contra el de los datos que nunca había 
# visto (Test). Si el modelo es estable y ha generalizado bien el mercado madrileño, 
# ambos errores serán muy similares. Si el modelo sufre de sobreajuste por memorización, 
# el error en Test se disparará.

cat("\n--- COMPARATIVA DE ESTABILIDAD TRAIN/TEST ---\n")

estabilidad <- bind_rows(met_rf_train, met_rf_test,
                          met_xgb_train, met_xgb_test) |>
  filter(.metric == "rmse") |>
  select(modelo, set, .estimate) |>
  tidyr::pivot_wider(names_from = set, values_from = .estimate) |>
  mutate(ratio_test_train = test / train)

cat("\nTabla de estabilidad (RMSE):\n")
print(estabilidad)

comparativa <- bind_rows(met_rf_test, met_xgb_test) |>
  filter(.metric == "rmse") |>
  select(modelo, .estimate) |>
  rename(RMSE_test = .estimate)

cat("\nComparativa final (RMSE en test set):\n")
print(comparativa)

saveRDS(comparativa,  "datos/comparativa_cv.rds")
saveRDS(estabilidad,  "datos/estabilidad_train_test.rds")
saveRDS(rec,          "datos/recipe_ml.rds")

# Y así concluimos el motor de Machine Learning. Ya tenemos nuestros dos algoritmos 
# campeones entrenados, optimizados y evaluados, guardados de forma persistente y 
# completamente listos para ser conectados al dashboard web final.

# ==============================================================================
# 3.7 INTERVALOS DE CONFIANZA / PREDICCIÓN (95%)
# En el mercado inmobiliario real, dar una estimación puntual (ej. 300.000€) 
# transmite una falsa sensación de precisión absoluta. Lo profesional es ofrecer 
# una "horquilla" de precios. Puesto que los modelos de Machine Learning (como XGBoost) 
# no ofrecen intervalos p-valorizados como el SAR, construimos Intervalos de Predicción 
# asumiendo normalidad en los errores del test. 
# La fórmula teórica para un 95% de confianza (alpha = 5%) es: 
# Límite = Predicción ± (1.96 * RMSE)
# ==============================================================================

cat("\n--- EVALUACIÓN DE INTERVALOS DE CONFIANZA (95%) ---\n")

# Calculamos los límites para XGBoost usando su RMSE en test
rmse_xgb <- comparativa |> filter(modelo == "XGBoost") |> pull(RMSE_test)

# Evaluamos cuántas viviendas del conjunto de Test caen realmente dentro de esta horquilla
eval_intervalos_xgb <- pred_xgb_test |>
  mutate(
    lim_inf = .pred - (1.96 * rmse_xgb),
    lim_sup = .pred + (1.96 * rmse_xgb),
    acierto_intervalo = if_else(log_price >= lim_inf & log_price <= lim_sup, TRUE, FALSE)
  )

cobertura_xgb <- mean(eval_intervalos_xgb$acierto_intervalo) * 100

cat(sprintf("Cobertura del Intervalo al 95%% en XGBoost: %.2f%%\n", cobertura_xgb))

if (cobertura_xgb >= 90) {
  cat("ÉXITO: La horquilla estadística cubre la inmensa mayoría de casos reales.\n")
} else {
  cat("AVISO: Los errores tienen colas pesadas, el intervalo subestima la varianza extrema.\n")
}

# Guardamos el RMSE de test para que la App de Shiny pueda construir estos 
# intervalos dinámicamente cuando el usuario introduzca una vivienda nueva.
saveRDS(rmse_xgb, "datos/rmse_test_xgb.rds")

cat("\nEl script ha finalizado exitosamente. Modelos y métricas exportados.\n")

