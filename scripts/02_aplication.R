rm(list = ls(all.names = TRUE))

source("00_funciones.R")

# Lectura de información
ta_df=read.csv("../output/df_preproc.csv") %>% na.omit()

## Agrupar las etiquetas a un problema de clasificación binaria

ta_df$Cl1984 <-ifelse(ta_df$Cl1984=="Agr", "Agr", "NoAgr")

# transformar la variable Y como una variable categorica
ta_df$Cl1984<-as.factor(ta_df$Cl1984)

summary(ta_df$Cl1984)

datos_analisis <- tidyr::gather(ta_df, variable, value, -Cl1984) 


boxplot_por_patron(
  datos_analisis,
  patron = "^CostzPr",
  titulo = "Corrección radiométrica",
  xlab   = "Bandas",
  abbrev = TRUE
)

# Boxplot
# Indices Espectrales
boxplot_por_patron(
  datos_analisis,
  patron = "^(NDVI|SAVI|MSAVI)_",
  titulo = "Índices espectrales",
  xlab   = "Covariables de índices espectrales",
  abbrev = TRUE
)
# Indices de TEXTURA
boxplot_por_patron(datos_analisis, "^homogeneity_", "Índices de textura: homogeneidad", "Covariables de homogeneidad")
boxplot_por_patron(datos_analisis, "^mean_",        "Índices de textura: media",        "Covariables de media")
boxplot_por_patron(datos_analisis, "^variance_",    "Índices de textura: varianza",     "Covariables de varianza")
# Correlaciones

# Calcular la matriz de correlación para cada grupo
cor_matrix_group1 <- cor(ta_df[ta_df$Cl1984 == "Agr", 2:22])
cor_matrix_group2 <- cor(ta_df[ta_df$Cl1984 == "NoAgr", 2:22])


# Convertir matrices a marcos de datos para ggplot2
cor_df_group1 <- as.data.frame(as.table(cor_matrix_group1))
cor_df_group2 <- as.data.frame(as.table(cor_matrix_group2))

par(mfrow=c(1,2))
# Crear gráficos de calor 

ggplot(cor_df_group1, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = "Correlación", midpoint = 0) +
  theme_minimal() +
  labs(title = "Gráfico de correlación: Clase - Agr") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggplot(cor_df_group2, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = "Correlación", midpoint = 0) +
  theme_minimal() +
  labs(title = "Gráfico de correlación: Clase - NoAgr") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))



# Aplicaciones de los modelos supervisados

# Poder predictivo (Repeated Hold Out)
datos_analisis <- Repeat_HO(df = ta_df, Y = "Cl1984", B = 100, p1 = 0.80)

start_time <- proc.time()
logitEP <- Result_Par(datos_analisis,Metodo = Logit, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
probitEP <- Result_Par(datos_analisis,Metodo = Probit, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
LassoEp <- Result_Par(datos_analisis,Metodo = Logit_Lasso, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
logit2EP <- Result_Par(datos_analisis,Metodo = Logit2_Step, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
probit2EP <- Result_Par(datos_analisis,Metodo = Probit2_Step, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
lasso2EP <- Result_Par(datos_analisis,Metodo = Logit2_LassoN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
RF <- Result_Par(datos_analisis,Metodo = RandomForest, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
Knnfit <- Result_Par(datos_analisis,Metodo = Knn, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
SvM <- Result_Par(datos_analisis,Metodo = SVM, workers = 8)
t <- proc.time() - start_time
beep(2)

# ---- BLOQUE 2: datos balanceados ----
datos_analisis <- Repeat_HOBalanceo(df = ta_df, Y = "Cl1984", B = 100, p1 = 0.8)

start_time <- proc.time()
logitEP <- Result_Par(datos_analisis, Metodo = LogitN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
probitEP <- Result_Par(datos_analisis, Metodo = ProbitN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
LassoEp <- Result_Par(datos_analisis, Metodo = Logit_LassoN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
logit2EP <- Result_Par(datos_analisis,Metodo = Logit2_StepN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
probit2EP <- Result_Par(datos_analisis, Metodo = Probit2_StepN, workers = 8)
t <- proc.time() - start_time
beep(2)

start_time <- proc.time()
lasso2EP <- Result_Par(datos_analisis, Metodo = Logit2_LassoN, workers = 8)
t <- proc.time() - start_time
beep(2)
