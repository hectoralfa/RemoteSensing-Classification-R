rm(list = ls(all.names = TRUE))
gc()


# =========================
# Librerías
# =========================

library(dplyr)
library(tidyverse)
library(MASS)
library(glmnet)
library(e1071)
library(rpart.plot)
library(rpart)

set.seed(12323)

# =========================
# Datos: Diabetes in Pima Indian Women
# =========================
data("Pima.te") 

# =========================
# 1) Selección por pasos (forward) en regresión logística
# =========================
ajuste_nulo <- glm(
  type ~ 1,
  family = binomial(link = "logit"),
  data = Pima.te
)

ajuste_saturado <- glm(
  type ~ .,
  family = binomial(link = "logit"),
  data = Pima.te
)

logit_forward <- step(
  object = ajuste_nulo,
  scope  = list(lower = ajuste_nulo, upper = ajuste_saturado),
  trace  = TRUE,
  direction = "forward"
)

summary(logit_forward)

# =========================
# 2) LASSO 
# =========================
XVar <- model.matrix(type ~ ., data = Pima.te)[, -1]  # quita intercepto
YRes <- Pima.te$type

lasso_tun <- cv.glmnet(
  x = XVar,
  y = YRes,
  nfolds = 10,
  type.measure = "class",
  family = "binomial",
  nlambda = 300
)

plot(lasso_tun)
lasso_tun$lambda.min
lasso_tun$lambda.1se
coef(lasso_tun, s = "lambda.min")

# =========================
# 3) Ejemplo SVM (kernel lineal)
# =========================
n <- 700
df <- data.frame(
  x1 = runif(n),
  x2 = runif(n)
)

df$y <- factor(
  ifelse(df$x2 - 1.4 * df$x1 < 0, -1, 1),
  levels = c(-1, 1)
)

create_svm_plot <- function(trainset, cost_value) {
  svm_model <- svm(
    y ~ .,
    data   = trainset,
    kernel = "linear",
    cost   = cost_value,
    scale  = FALSE
  )
  
  # w = t(alpha_i * y_i) %*% SV
  w <- t(svm_model$coefs) %*% svm_model$SV
  
  slope_1     <- -w[1] / w[2]
  intercept_1 <-  svm_model$rho / w[2]
  
  scatter_plot <- ggplot(trainset, aes(x = x1, y = x2, color = y)) +
    geom_point() +
    scale_color_manual(values = c("red", "blue"))
  
  plot_decision <- scatter_plot +
    geom_abline(slope = slope_1, intercept = intercept_1)
  
  plot_margins <- plot_decision +
    geom_abline(slope = slope_1, intercept = intercept_1 - 1 / w[2], linetype = "dashed") +
    geom_abline(slope = slope_1, intercept = intercept_1 + 1 / w[2], linetype = "dashed")
  
  plot_margins + labs(title = paste("SVM con cost =", cost_value))
}

# Train/test split
tamano_m <- floor(0.7 * nrow(df))
idx_train <- sample(seq_len(nrow(df)), size = tamano_m)
trainset <- df[idx_train, ]
testset  <- df[-idx_train, ]

# Probar distintos cost y graficar
cost_values <- c(1, 40, 45, 50, 100, 1000)

for (cost_value in cost_values) {
  print(create_svm_plot(trainset, cost_value))
}

# Tuneo
svm_tuned <- tune(
  svm,
  y ~ .,
  data = trainset,
  kernel = "linear",
  ranges = list(cost = cost_values),
  scale = FALSE
)

print(svm_tuned)
best_cost <- svm_tuned$best.parameters$cost
best_cost

# =========================
# 4) Árboles de decisión y "bosque" de 3 árboles
# =========================
idx_entrenamiento <- sample(seq_len(nrow(Pima.te)), size = floor(0.7 * nrow(Pima.te)))
conjunto_entrenamiento <- Pima.te[idx_entrenamiento, ]
conjunto_prueba        <- Pima.te[-idx_entrenamiento, ]

arbol1 <- rpart(
  type ~ .,
  data = conjunto_entrenamiento,
  method = "class",
  control = rpart.control(maxdepth = 3)
)

arbol2 <- rpart(
  type ~ skin + ped + age + bmi,
  data = conjunto_entrenamiento,
  method = "class",
  control = rpart.control(maxdepth = 3)
)

arbol3 <- rpart(
  type ~ glu + ped + bmi + npreg,
  data = conjunto_entrenamiento,
  method = "class",
  control = rpart.control(maxdepth = 3)
)

par(mfrow = c(1, 3))

rpart.plot(arbol1, extra = 1, under = TRUE, box.palette = "Blues",  tweak = 1.2)
rpart.plot(arbol2, extra = 1, under = TRUE, box.palette = "Greens", tweak = 1.2)
rpart.plot(arbol3, extra = 1, under = TRUE, box.palette = "Reds",   tweak = 1.2)
