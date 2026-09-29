## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)
has_deps = all(vapply(
  c("mlr3", "mlr3learners", "ranger", "ISLR2"),
  requireNamespace, logical(1), quietly = TRUE
))

## ----load---------------------------------------------------------------------
library(xplaineff)

## ----synthetic-data-----------------------------------------------------------
set.seed(1)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = runif(n, -1, 1)
y = 0.2 * x1 + ifelse(x3 > 0.3, 3, -3) * x2 + rnorm(n, 0, 0.3)
syn_data = data.frame(x1, x2, x3, y)

## ----synthetic-model----------------------------------------------------------
syn_model = lm(y ~ x1 + x2 * I(x3 > 0.3), data = syn_data)

## ----synthetic-fit------------------------------------------------------------
syn_tree = GadgetTree$new(
  strategy = PdStrategy$new(),
  n_split = 2,
  min_node_size = 50,
  n_quantiles = 40
)
syn_tree$fit(
  data = syn_data,
  target_feature_name = "y",
  model = syn_model,
  feature_set = "x2",
  split_feature = c("x1", "x3"),
  n_grid = 20L
)

## ----synthetic-split-info-----------------------------------------------------
syn_split = syn_tree$extract_split_info()
print(
  syn_split[, c("id", "depth", "n_obs", "node_type", "split_feature", "split_value", "int_imp", "is_final")],
  row.names = FALSE, digits = 2
)

## ----synthetic-plot, fig.height = 3.5-----------------------------------------
syn_plots = syn_tree$plot(
  data = syn_data,
  target_feature_name = "y",
  show_plot = FALSE
)
syn_plots$Depth_1$Node_1
syn_plots$Depth_2$Node_2
syn_plots$Depth_2$Node_3

## ----bike-check, echo = FALSE, results = "asis"-------------------------------
if (!has_deps) {
  cat("*The packages mlr3, mlr3learners, ranger, and ISLR2 are not all installed, so the remaining code is",
    "shown but not evaluated.*\n")
}

## ----bike-data, eval = has_deps-----------------------------------------------
library(mlr3)
library(mlr3learners)

data("Bikeshare", package = "ISLR2")
set.seed(123)
bike = Bikeshare[sample(seq_len(nrow(Bikeshare)), 1000), ]
factor_features = c("season", "mnth", "weekday", "workingday", "holiday", "weathersit")
bike[factor_features] = lapply(bike[factor_features], as.factor)
bike_data = bike[, c("hr", "temp", "workingday", "season", "mnth", "day", "holiday", "weekday",
  "weathersit", "atemp", "hum", "windspeed", "bikers")]
names(bike_data)[names(bike_data) == "bikers"] = "target"

task = TaskRegr$new(id = "bike", backend = bike_data, target = "target")
learner = lrn("regr.ranger")
learner$train(task)

## ----bike-pd-fit, eval = has_deps---------------------------------------------
effect_features = c("hr", "temp", "workingday", "season")
split_features = c("temp", "workingday", "season")

bike_pd = GadgetTree$new(
  strategy = PdStrategy$new(),
  n_split = 2,
  min_node_size = 50
)
bike_pd$fit(
  data = bike_data,
  target_feature_name = "target",
  model = learner,
  feature_set = effect_features,
  split_feature = split_features,
  n_grid = 20L
)

## ----bike-pd-tree, eval = has_deps, fig.height = 5----------------------------
bike_pd$plot_tree_structure()

## ----bike-pd-split-info, eval = has_deps--------------------------------------
bike_pd_split = bike_pd$extract_split_info()
print(
  bike_pd_split[, c("id", "depth", "n_obs", "node_type", "split_feature", "split_value", "int_imp", "is_final")],
  row.names = FALSE, digits = 2
)

## ----bike-pd-plot, eval = has_deps, fig.height = 3.5--------------------------
bike_pd_plots = bike_pd$plot(
  data = bike_data,
  target_feature_name = "target",
  features = "hr",
  show_plot = FALSE
)
bike_pd_plots$Depth_1$Node_1
bike_pd_plots$Depth_2$Node_2
bike_pd_plots$Depth_2$Node_3

## ----bike-ale-fit, eval = has_deps--------------------------------------------
bike_ale = GadgetTree$new(
  strategy = AleStrategy$new(),
  n_split = 2,
  impr_par = 0.01,
  min_node_size = 50
)
bike_ale$fit(
  data = bike_data,
  target_feature_name = "target",
  model = learner,
  feature_set = effect_features,
  split_feature = split_features,
  n_intervals = 10
)
bike_ale_split = bike_ale$extract_split_info()
print(
  bike_ale_split[, c("id", "depth", "n_obs", "node_type", "split_feature", "split_value", "int_imp", "is_final")],
  row.names = FALSE, digits = 2
)

## ----bike-ale-imp, eval = has_deps--------------------------------------------
print(
  bike_ale_split[!bike_ale_split$is_final,
    c("id", "split_feature", setdiff(grep("^int_imp_", names(bike_ale_split), value = TRUE), "int_imp_parent"))],
  row.names = FALSE, digits = 2
)

## ----bike-ale-plot, eval = has_deps, fig.height = 3.5-------------------------
bike_ale_plots = bike_ale$plot(
  data = bike_data,
  target_feature_name = "target",
  features = "hr",
  show_plot = FALSE
)
bike_ale_plots$Depth_1$Node_1
bike_ale_plots$Depth_2$Node_2
bike_ale_plots$Depth_2$Node_3

## ----plot-options, eval = has_deps, fig.height = 3.5--------------------------
one_plot = bike_pd$plot(
  data = bike_data,
  target_feature_name = "target",
  features = "temp",
  depth = 2,
  node_id = 3,
  show_point = TRUE,
  mean_center = FALSE,
  show_plot = FALSE
)
one_plot$Depth_2$Node_3

## ----iml, eval = FALSE--------------------------------------------------------
# library(iml)
# predictor = Predictor$new(
#   model = learner,
#   data = bike_data[, setdiff(names(bike_data), "target")],
#   y = bike_data$target
# )
# effects = FeatureEffects$new(predictor, features = effect_features, method = "ice", grid.size = 20)
# 
# bike_pd_iml = GadgetTree$new(strategy = PdStrategy$new(), n_split = 2, min_node_size = 50)
# bike_pd_iml$fit(
#   data = bike_data,
#   target_feature_name = "target",
#   effect = effects,
#   split_feature = split_features
# )
# bike_pd_iml$plot(effect = effects, data = bike_data, target_feature_name = "target", features = "hr")

