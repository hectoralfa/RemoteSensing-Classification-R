
# =========================
# Librerías necesarias
# =========================
library(dplyr)
library(purrr)
library(tidyr)
library(caret)
library(furrr)
library(pROC)
library(glmnet)
library(e1071)
library(class)
library(ranger)

set.seed(1234)

# =========================
# Clip (raster/shape)
# Nota: requiere raster/terra y sf/sp
# =========================
clip <- function(raster_obj, shape_obj) {
  a1_crop <- raster::crop(raster_obj, shape_obj)
  mask_r  <- raster::rasterize(shape_obj, a1_crop)
  a1_crop * mask_r
}

# =========================
# Repeated Hold-Out (sin balanceo)
# df: data.frame
# Y: nombre de variable respuesta (string)
# =========================
Repeat_HO <- function(df, Y, B, p1 = 0.8) {
  splits <- vector("list", B)
  
  for (i in seq_len(B)) {
    idx <- caret::createDataPartition(df[[Y]], p = p1, list = FALSE)
    Train <- df[idx, , drop = FALSE]
    Test  <- df[-idx, , drop = FALSE]
    splits[[i]] <- list(Train = Train, Test = Test)
  }
  
  # regresa lista con Train y Test como listas paralelas
  splits %>% transpose()
}

# =========================
# Repeated Hold-Out (balanceando SOLO Train con downSample)
# class_col: nombre de variable clase (string)
# =========================
Repeat_HO_Balanceo <- function(df, Y, B, p1 = 0.8) {
  splits <- vector("list", B)
  
  for (i in seq_len(B)) {
    idx <- caret::createDataPartition(df[[Y]], p = p1, list = FALSE)
    
    Train_raw <- df[idx, , drop = FALSE]
    Test      <- df[-idx, , drop = FALSE]
    
      x = Train_raw %>% select(-all_of(Y)),
      y = Train_raw[[Y]],
      yname = Y
    )
    
    splits[[i]] <- list(Train = Train_ds, Test = Test)
  }
  
  splits %>% transpose()
}

# =========================
# Métricas desde matrices de confusión (2x2)
# =========================
ErroresClasificacion <- function(MC.Train, MC.Test) {
  # asumiendo orden [clase1, clase2] en filas/cols
  Y1Train <- MC.Train[1, 1] / sum(MC.Train[1, ])
  Y2Train <- MC.Train[2, 2] / sum(MC.Train[2, ])
  GlobalTrain <- sum(diag(MC.Train)) / sum(MC.Train)
  
  Y1Test <- MC.Test[1, 1] / sum(MC.Test[1, ])
  Y2Test <- MC.Test[2, 2] / sum(MC.Test[2, ])
  GlobalTest <- sum(diag(MC.Test)) / sum(MC.Test)
  
  data.frame(Y1Train, Y2Train, GlobalTrain, Y1Test, Y2Test, GlobalTest)
}

# =========================
# Wrapper paralelo para correr métodos en cada split
# Metodo: string con el nombre de la función (ej: "Logit")
# workers: núm de procesos
# datos_splits: salida de Repeat_HO() o Repeat_HO_Balanceo()
# =========================
Result_Par <- function(datos_splits, Metodo, workers = 5) {
  opciones <- furrr::furrr_options(seed = TRUE)
  future::plan(strategy = multisession, workers = workers)
  
  res <- furrr::future_map2(
    .x = datos_splits$Train,
    .y = datos_splits$Test,
    .f = Metodo,
    .options = opciones,
    .progress = TRUE
  )
  
  metrics_ind <- purrr::map(res, ~ ErroresClasificacion(.x$MC.Train, .x$MC.Test)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x * 100, 3))) %>%
    tibble::add_column(Metodo = deparse(substitute(Metodo)), .before = 1)
  
  metrics_glob <- metrics_ind %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), mean), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) %>%
    tibble::add_column(Metodo = deparse(substitute(Metodo)), .before = 1)
  
  hypers <- purrr::map(res, "hipe")
  
  list(Individual = metrics_ind, Global = metrics_glob, hiper = hypers)
}


# ==========================================================
#           FUNCIONES DE MODELOS (consistentes)
# ==========================================================

# -------------------------
# 1) Logit con punto óptimo (Youden)
# -------------------------
Logit <- function(Train, Test, Y = "Cl1984") {
  form <- as.formula(paste0(Y, " ~ ."))
  
  logit <- glm(formula = form, family = binomial("logit"), data = Train)
  
  roc_obj <- pROC::roc(logit$y, fitted(logit), quiet = TRUE)
  pr <- pROC::coords(roc_obj, "best", best.method = "youden")$threshold
  
  prob_train <- predict(logit, newdata = Train, type = "response")
  prob_test  <- predict(logit, newdata = Test, type = "response")
  
  lev <- levels(Train[[Y]])
  PredTrain <- ifelse(prob_train > pr, lev[2], lev[1])
  PredTest  <- ifelse(prob_test  > pr, lev[2], lev[1])
  
  MC.Train <- table(Observados = Train[[Y]], Predic = PredTrain)
  MC.Test  <- table(Observados = Test[[Y]],  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = list(pto = pr, coe = as.list(coef(logit)))
  )
}

# -------------------------
# 2) Probit con punto óptimo (Youden)
# -------------------------
Probit <- function(Train, Test, Y = "Cl1984") {
  form <- as.formula(paste0(Y, " ~ ."))
  
  probitt <- glm(formula = form, family = binomial("probit"), data = Train)
  
  roc_obj <- pROC::roc(probitt$y, fitted(probitt), quiet = TRUE)
  pr <- pROC::coords(roc_obj, "best", best.method = "youden")$threshold
  
  prob_train <- predict(probitt, newdata = Train, type = "response")
  prob_test  <- predict(probitt, newdata = Test, type = "response")
  
  lev <- levels(Train[[Y]])
  PredTrain <- ifelse(prob_train > pr, lev[2], lev[1])
  PredTest  <- ifelse(prob_test  > pr, lev[2], lev[1])
  
  MC.Train <- table(Observados = Train[[Y]], Predic = PredTrain)
  MC.Test  <- table(Observados = Test[[Y]],  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = list(pto = pr, coe = as.list(coef(probitt)))
  )
}

# -------------------------
# 3) Logit + LASSO (glmnet) con punto óptimo (Youden) sobre Train
# -------------------------
Logit_Lasso <- function(Train, Test, Y = "Cl1984") {
  form <- as.formula(paste0(Y, " ~ ."))
  
  XTrain <- model.matrix(form, data = Train)[, -1, drop = FALSE]
  YTrain <- Train[[Y]]
  XTest  <- model.matrix(form, data = Test)[, -1, drop = FALSE]
  
  lasso_tun <- cv.glmnet(
    x = XTrain, y = YTrain,
    family = "binomial",
    type.measure = "class",
    nfolds = 10,
    nlambda = 300
  )
  
  pred_prob_train <- as.numeric(predict(lasso_tun, newx = XTrain, s = "lambda.min", type = "response"))
  roc_obj <- pROC::roc(YTrain, pred_prob_train, quiet = TRUE)
  pr <- pROC::coords(roc_obj, "best", best.method = "youden")$threshold
  
  pred_prob_test <- as.numeric(predict(lasso_tun, newx = XTest, s = "lambda.min", type = "response"))
  
  lev <- levels(YTrain)
  PredTrain <- ifelse(pred_prob_train > pr, lev[2], lev[1])
  PredTest  <- ifelse(pred_prob_test  > pr, lev[2], lev[1])
  
  MC.Train <- table(Observados = Train[[Y]], Predic = PredTrain)
  MC.Test  <- table(Observados = Test[[Y]],  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = list(pto = pr, lambda = lasso_tun$lambda.min)
  )
}

# -------------------------
# 4) kNN
# -------------------------
Knn <- function(Train, Test, Y = "Cl1984", k_grid = 2:25, folds = 10) {
  # preProcess SOLO con Train
  pp <- caret::preProcess(Train, method = c("center", "scale"))
  Train_sc <- predict(pp, Train)
  Test_sc  <- predict(pp, Test)
  
  x_train <- Train_sc %>% select(-all_of(Y))
  y_train <- Train_sc[[Y]]
  x_test  <- Test_sc  %>% select(-all_of(Y))
  y_test  <- Test_sc[[Y]]
  
  # CV para escoger k (accuracy)
  idx_folds <- caret::createFolds(y_train, k = folds, returnTrain = FALSE)
  
  acc_k <- sapply(k_grid, function(k) {
    accs <- sapply(idx_folds, function(id_val) {
      x_tr <- x_train[-id_val, , drop = FALSE]
      y_tr <- y_train[-id_val]
      x_va <- x_train[id_val, , drop = FALSE]
      y_va <- y_train[id_val]
      
      pred <- class::knn(train = x_tr, test = x_va, cl = y_tr, k = k)
      mean(pred == y_va)
    })
    mean(accs)
  })
  
  k_best <- k_grid[which.max(acc_k)]
  
  PredTrain <- class::knn(train = x_train, test = x_train, cl = y_train, k = k_best)
  PredTest  <- class::knn(train = x_train, test = x_test,  cl = y_train, k = k_best)
  
  MC.Train <- table(Observados = y_train, Predic = PredTrain)
  MC.Test  <- table(Observados = y_test,  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = list(kbest = k_best)
  )
}

# -------------------------
# 5) Random Forest
# -------------------------
RandomForest <- function(Train, Test, Y = "Cl1984") {
  grid <- expand.grid(
    mtry = seq(3, 7, 1),
    min.node.size = c(2, 10, 15),
    num.trees = c(100, 500)
  )
  grid$cv_err <- NA_real_
  
  folds <- caret::createFolds(Train[[Y]], k = 10, returnTrain = FALSE)
  
  for (i in seq_len(nrow(grid))) {
    errs <- numeric(length(folds))
    
    for (j in seq_along(folds)) {
      id_val <- folds[[j]]
      TrainCV <- Train[-id_val, , drop = FALSE]
      ValidCV <- Train[id_val,  , drop = FALSE]
      
      rf <- ranger::ranger(
        formula = as.formula(paste0(Y, " ~ .")),
        data = TrainCV,
        num.trees = grid$num.trees[i],
        mtry = grid$mtry[i],
        min.node.size = grid$min.node.size[i],
        importance = "impurity",
        probability = FALSE
      )
      
      pred <- predict(rf, data = ValidCV)$predictions
      errs[j] <- mean(pred != ValidCV[[Y]])
    }
    
    grid$cv_err[i] <- mean(errs)
  }
  
  best <- which.min(grid$cv_err)
  
  rf_fit <- ranger::ranger(
    formula = as.formula(paste0(Y, " ~ .")),
    data = Train,
    num.trees = grid$num.trees[best],
    mtry = grid$mtry[best],
    min.node.size = grid$min.node.size[best],
    importance = "impurity",
    probability = FALSE
  )
  
  PredTrain <- predict(rf_fit, data = Train)$predictions
  PredTest  <- predict(rf_fit, data = Test)$predictions
  
  MC.Train <- table(Observados = Train[[Y]], Predic = PredTrain)
  MC.Test  <- table(Observados = Test[[Y]],  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = as.list(grid[best, c("num.trees", "mtry", "min.node.size")])
  )
}

# -------------------------
# 6) SVM radial
# -------------------------
SVM <- function(Train, Test, Y = "Cl1984") {
  # preProcess SOLO con Train
  pp <- caret::preProcess(Train, method = c("center", "scale"))
  Train_sc <- predict(pp, Train)
  Test_sc  <- predict(pp, Test)
  
  tun <- e1071::tune(
    "svm",
    as.formula(paste0(Y, " ~ .")),
    data = Train_sc,
    kernel = "radial",
    ranges = list(
      cost  = 10^(-1:2),
      gamma = c(.01, .03, 0.0667, .5, .8)
    ),
    cross = 10
  )
  
  best_cost  <- tun$best.parameters$cost
  best_gamma <- tun$best.parameters$gamma
  
  svm_fit <- e1071::svm(
    as.formula(paste0(Y, " ~ .")),
    data = Train_sc,
    kernel = "radial",
    cost = best_cost,
    gamma = best_gamma
  )
  
  PredTrain <- predict(svm_fit, newdata = Train_sc)
  PredTest  <- predict(svm_fit, newdata = Test_sc)
  
  MC.Train <- table(Observados = Train_sc[[Y]], Predic = PredTrain)
  MC.Test  <- table(Observados = Test_sc[[Y]],  Predic = PredTest)
  
  list(
    MC.Train = MC.Train,
    MC.Test = MC.Test,
    hipe = list(cost = best_cost, gamma = best_gamma)
  )
}


# funciones EDA

plot_pixel_distributions <- function(x,
                                     bands = NULL,
                                     size = 200000,
                                     bins = 50,
                                     ncol = 1,
                                     scales = "free_x",
                                     title = "Distribución de pixeles por banda",
                                     seed = 1) {
  stopifnot(inherits(x, "SpatRaster"))
  
  # Paquetes necesarios
  requireNamespace("terra")
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  requireNamespace("ggplot2")
  
  set.seed(seed)
  
  df <- terra::spatSample(x, size = size, na.rm = TRUE, as.df = TRUE)
  
  if (is.null(bands)) {
    bands_use <- names(df)
  } else {
    bands_use <- intersect(bands, names(df))
  }
  
  if (length(bands_use) == 0) {
    stop("Ninguna banda encontrada. Revisa names(x) y el vector 'bands'.")
  }
  
  long <- df %>% 
    dplyr::select(dplyr::all_of(bands_use)) %>% 
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "banda",
                        values_to = "valor")
  
  ggplot2::ggplot(long, ggplot2::aes(x = valor)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = bins) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ banda, ncol = ncol, scales = scales) +
    ggplot2::labs(x = NULL, y = "Densidad", title = title)
}

##############


boxplot_por_patron <- function(df, patron, titulo, xlab,
                               abbrev = FALSE, ignore_case = TRUE) {
  
  df2 <- subset(df, grepl(patron, variable, ignore.case = ignore_case))
  
  p <- ggplot(df2, aes(x = variable, y = value, fill = Cl1984)) +
    geom_boxplot() +
    labs(title = titulo) +
    scale_x_discrete(name = xlab) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (abbrev) {
    p <- p + scale_x_discrete(labels = abbreviate, name = xlab)
  }
  
  p
}


# Calcula GLCM de una banda y guarda todo 
glcm_band_to_files <- function(r_stack, band_i, out_dir, prefijo,
                               window = c(3,3),
                               statistics = stats_glcm,
                               shift = shift_glcm,
                               datatype = "FLT4S",
                               overwrite = TRUE,
                               nombres = NULL) {
  
  r_band <- raster::raster(r_stack, layer = band_i)
  
  # calcular glcm
  g <- glcm::glcm(
    r_band,
    window = window,
    statistics = statistics,
    shift = shift
  )
  
  if (!is.null(nombres)) {
    nombres <- c(nombres, paste0(names(g), "_", band_i))
  }
  
  # Escribir cada capa del stack 'g' (una por estadístico)
  # names(g) típicamente: "glcm_mean", "glcm_variance", etc.
  for (nm in names(g)) {
    stat_name <- sub("^glcm_", "", nm)  # mean, variance, ...
    fname <- file.path(out_dir, sprintf("%s_%s_b%d.img", stat_name, prefijo, band_i))
    
    raster::writeRaster(
      g[[nm]],
      filename = fname,
      datatype = datatype,
      overwrite = overwrite
    )
  }
  
  return(list(glcm = g, nombres = nombres))
}
