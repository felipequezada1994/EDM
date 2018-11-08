library(readxl)
library(FSelector)
library(rpart)
##############################################################
###                  FACULTAD DE CIENCIAS                  ###
##############################################################
datos <- read_excel("C:/Users/FELIPE/Desktop/DATOS_ALUMNOS/Fac_Ciencias/CIENCIAS_T0.xlsx")
datos1 <- read_excel("C:/Users/FELIPE/Desktop/DATOS_ALUMNOS/Fac_Ciencias/CIENCIAS_T1.xlsx")
attach(datos)
attach(datos1)

# Elimina variables 
datos <- datos[,-9]
datos <- datos[,-5]
datos <- datos[,-1]
colnames(datos)[20]<-"respuesta"
datos$respuesta <- factor(datos$respuesta)

datos1 <- datos1[,-9]
datos1 <- datos1[,-5]
datos1 <- datos1[,-1]
datos1 <- datos1[,-20]
colnames(datos1)[20]<-"respuesta"
datos1$respuesta <- factor(datos1$respuesta)
datos1[is.na(datos1$prom_1), "prom_1"] <- 0


##############################################################
###                   MUESTRA DEL 80%                      ###
##############################################################
set.seed(2018)
total_individuos <- nrow(datos)
total_individuos
entrenamiento <- round(total_individuos*0.8)
entrenamiento
prueba <- sample(1:total_individuos, size=entrenamiento)
datos.entrenamiento <- datos[prueba,]

total_individuos1 <- nrow(datos1)
total_individuos1
entrenamiento1 <- round(total_individuos1*0.8)
entrenamiento1
prueba1 <- sample(1:total_individuos1, size=entrenamiento1)
datos1.entrenamiento <- datos1[prueba1,]

#########################################################
###                 REGRESIÓN LOGÍSTICA               ###
#########################################################
#############################
### selección de variables###
#############################
modelo = glm(respuesta~.,data=datos.entrenamiento, family = "binomial")
summary(modelo)
step(modelo, direction = "backward")
step(modelo, direction = "forward")
step(modelo, direction = "both")

modelo1 = glm(respuesta~.,data =datos1.entrenamiento, family = "binomial")
summary(modelo1)
step(modelo1, direction = "backward")
step(modelo1, direction = "forward")
step(modelo1, direction = "both")

##################
### Validación ###
##################
set.seed(1994)
Folds         <- 10    

#t=0
datos$kfold   <- sample(1:Folds, nrow(datos), replace = T)
Iter_RL   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, 
                     Recall = NULL, specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos, kfold  == i)
  Entrenamiento <- subset(datos, !kfold == i) 
  modelo = glm(respuesta~psu_matematica+psu_nem+familia_salud+
               familia_proseguir_estudios,data =Entrenamiento, family = "binomial")
  glm.prob <- predict (modelo , Test, type ="response")
  glm.pred <- rep (0 ,nrow(Test)) 
  glm.pred [ glm.prob >.5] <- 1
  MC            <- table(glm.pred,Test$respuesta)           
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter_RL          <- rbind(Iter_RL, data.frame(Iter_RL = i,Accurracy = accurracy, 
                   Precision = precision, Recall = recall, Specificidad = specificidad,
                   Score = score))
}
Iter_RL
mean(Iter_RL$Accurracy)
mean(Iter_RL$Precision)
mean(Iter_RL$Recall)
mean(Iter_RL$Specificidad)
mean(Iter_RL$Score)

### ODD
summary(modelo)
odd=exp(coef(modelo))
odd
lrtest(modelo)
logLik(modelo)
#anova(modelo,test="Chisq")
#confint(modelo)


#t=1
datos1$kfold   <- sample(1:Folds, nrow(datos1), replace = T)

Iter1_RL   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, Recall = NULL,
                     specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos1, kfold  == i)
  Entrenamiento <- subset(datos1, !kfold == i) 
  modelo1 = glm(respuesta~ingreso_carrera + psu_matematica + psu_lenguaje+
                  APRO_1+ familia_salud2,data =Entrenamiento, family = "binomial")
  glm.prob <- predict (modelo1 , Test, type ="response")
  glm.pred <- rep (0 ,nrow(Test)) 
  glm.pred [ glm.prob >.5] <- 1
  MC            <- table(glm.pred,Test$respuesta)           
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter1_RL          <- rbind(Iter1_RL, data.frame(Iter1_RL = i,Accurracy = accurracy, 
                                          Precision = precision, Recall = recall, Specificidad = specificidad,
                                          Score = score))
}
Iter1_RL
mean(Iter1_RL$Accurracy)
mean(Iter1_RL$Precision)
mean(Iter1_RL$Recall)
mean(Iter1_RL$Specificidad)
mean(Iter1_RL$Score)

### ODD
summary(modelo1)
odd<- exp(coef(modelo1))
odd
lrtest(modelo1)
logLik(modelo1)





#########################################################
###           MAQUINA DE VECTORES DE SOPORTE          ###
#########################################################
########################################
### SELECCIÓN DE VARIABLES (ENTROÍA) ###
########################################
pesos <- information.gain(respuesta~., datos.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
a <- as.simple.formula(subset, "respuesta")
print(a)
#para normalizar las variables
#norm=   pesos$attr_importance
#norm
#max <- max(norm)
#max
#min <- min(norm)
#min
#datos_norm <- as.data.frame(scale(norm, center = min, scale = max- min))
#datos_norm <- (datos_norm*100)
#datos_norm

pesos <- gain.ratio(respuesta~., datos.entrenamiento, unit = "log2")
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
b<- as.simple.formula(subset, "respuesta")
print(b)
pesos <- rank.correlation(respuesta~., datos.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
c <- as.simple.formula(subset, "respuesta")
print(c)

pesos <- chi.squared(respuesta~., datos.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
d <- as.simple.formula(subset, "respuesta")
print(d)

# BUSQUEDA SUBCONJUNTO
set.seed(1994)
evaluator <- function(subconjunto) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(datos.entrenamiento))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- datos.entrenamiento[test.idx, , drop=FALSE]
    train <- datos.entrenamiento[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subconjunto, "respuesta"), train)
    error.rate = sum(test$respuesta != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subconjunto)
  print(mean(results))
  return(mean(results))
}

posicion <-names(datos.entrenamiento)[-20] #Ubicación variable respuesta

subconjunto <- forward.search(posicion, evaluator)
e <- as.simple.formula(subconjunto, "respuesta")
print(e)
subconjunto <- backward.search(posicion, evaluator)
f <- as.simple.formula(subconjunto, "respuesta")
print(f)
subconjunto <- hill.climbing.search(posicion, evaluator)
g <- as.simple.formula(subconjunto, "respuesta")
print(g)
subconjunto <- exhaustive.search(posicion, evaluator)
i <- as.rsimple.formula(subconjunto, "respuesta")
print(i)
############################################
### SELECCIÓN DE VARIABLES t=1 (ENTROÍA) ###
############################################
pesos <- information.gain(respuesta~., datos1.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
a <- as.simple.formula(subset, "respuesta")
print(a)
pesos <- gain.ratio(respuesta~., datos1.entrenamiento, unit = "log2")
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
b<- as.simple.formula(subset, "respuesta")
print(b)
pesos <- rank.correlation(respuesta~., datos1.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
c <- as.simple.formula(subset, "respuesta")
print(c)
pesos <- chi.squared(respuesta~., datos1.entrenamiento)
pesos=pesos[order(pesos$attr_importance,decreasing=TRUE),,drop=F]
print(pesos)
subset <- cutoff.biggest.diff(pesos)
d <- as.simple.formula(subset, "respuesta")
print(d)

# BUSQUEDA SUBCONJUNTO T=1
set.seed(1994)
evaluator <- function(subconjunto) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(datos1.entrenamiento))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- datos1.entrenamiento[test.idx, , drop=FALSE]
    train <- datos1.entrenamiento[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subconjunto, "respuesta"), train)
    error.rate = sum(test$respuesta != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subconjunto)
  print(mean(results))
  return(mean(results))
}

posicion <-names(datos1.entrenamiento)[-23] #Ubicación variable respuesta

subconjunto <- forward.search(posicion, evaluator)
e <- as.simple.formula(subconjunto, "respuesta")
print(e)
subconjunto <- backward.search(posicion, evaluator)
f <- as.simple.formula(subconjunto, "respuesta")
print(f)
subconjunto <- hill.climbing.search(posicion, evaluator)
g <- as.simple.formula(subconjunto, "respuesta")
print(g)
subconjunto <- exhaustive.search(posicion, evaluator)
i <- as.simple.formula(subconjunto, "respuesta")
print(i)

#########################################
### Selección de los parámetros (t=0) ###
#########################################
library(e1071)
#lineal
kernel_lineal <- tune.svm(respuesta ~ psu_matematica + psu_ciencia+ psu_lenguaje+
                          psu_nem, data = datos, 
                          gamma = 10^(-3:-1), cost = 10^(1:2),kernel="linear")
lineal <-  kernel_lineal$best.performance

#radial
kernel_radial <- tune.svm(respuesta ~ psu_matematica + psu_ciencia+ psu_lenguaje+
                            psu_nem, data = datos, 
                  gamma = 10^(-3:-1), cost = 10^(1:2),kernel="radial")
radial <-  kernel_radial$best.performance

#sigmoidal
kernel_sigmoidal <- tune.svm(respuesta ~ psu_matematica + psu_ciencia+ psu_lenguaje+
                               psu_nem, data = datos, 
                             gamma = 10^(-3:-1), cost = 10^(1:2),kernel="sigmoid")
sigmoidal <-  kernel_sigmoidal$best.performance

#polynomial
kernel_polynomial <- tune.svm(respuesta ~ psu_matematica + psu_ciencia+ psu_lenguaje+
                                psu_nem, data = datos, 
                          gamma = 10^(-3:-1), cost = 10^(1:2),kernel="polynomial")
polynomial <-  kernel_polynomial$best.performance

mejor.kernel <- c(lineal,radial,sigmoidal,polynomial)
mejor.kernel

kernel_sigmoidal$best.parameters
#########################################
### Selección de los parámetros (t=1) ###
#########################################
#lineal
kernel_lineal.1 <- tune.svm(respuesta ~ APRO_1+ REP_1+ prom_1+ psu_matematica+
                              psu_ciencia+psu_nem, data = datos1.entrenamiento, 
                          gamma = 10^(-3:-1), cost = 10^(1:2),kernel="linear")
lineal.1 <-  kernel_lineal.1$best.performance

#radial
kernel_radial.1 <- tune.svm(respuesta ~ APRO_1+ REP_1+ prom_1+ psu_matematica+
                              psu_ciencia+psu_nem, data = datos1.entrenamiento, 
                          gamma = 10^(-3:-1), cost = 10^(1:2),kernel="radial")
radial.1 <-  kernel_radial.1$best.performance

#polynomial
kernel_polynomial.1 <- tune.svm(respuesta ~ APRO_1+ REP_1+ prom_1+ psu_matematica+
                                  psu_ciencia+psu_nem, data = datos1.entrenamiento, 
                              gamma = 10^(-3:-1), cost = 10^(1:2),kernel="polynomial")
polynomial.1 <-  kernel_polynomial.1$best.performance

#sigmoidal
kernel_sigmoidal.1 <- tune.svm(respuesta ~ APRO_1+ REP_1+ prom_1+ psu_matematica+
                                 psu_ciencia+psu_nem, data = datos1.entrenamiento, 
                             gamma = 10^(-3:-1), cost = 10^(1:2),kernel="sigmoid")
sigmoidal.1 <-  kernel_sigmoidal.1$best.performance

mejor.kernel.1 <- c(lineal.1,radial.1,sigmoidal.1,polynomial.1)
mejor.kernel.1

kernel_radial.1$best.parameters
##################
### Validación ###
##################
set.seed(1994)
Folds         <- 10            
#t=0
datos$kfold   <- sample(1:Folds, nrow(datos), replace = T)
Iter_SVM   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, 
                     Recall = NULL, specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos, kfold  == i)
  Entrenamiento <- subset(datos, !kfold == i) 
  Modelo <- svm(respuesta ~ psu_matematica + psu_ciencia+ psu_lenguaje+psu_nem, 
                data = Entrenamiento, kernel="sigmoid", gamma=0.01, cost=10)
  Prediccion <- predict(Modelo, new= Test, type="class")
  MC            <- table(Prediccion, Test$respuesta)           
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter_SVM          <- rbind(Iter_SVM, data.frame(Iter_SVM = i,Accurracy = accurracy, 
                    Precision = precision, Recall = recall, Specificidad = specificidad,
                    Score = score))
}
Iter_SVM
mean(Iter_SVM$Accurracy)
mean(Iter_SVM$Precision)
mean(Iter_SVM$Recall)
mean(Iter_SVM$Specificidad)
mean(Iter_SVM$Score)

### t=1 ###
set.seed(1994)
Folds         <- 10  
datos1$kfold   <- sample(1:Folds, nrow(datos1), replace = T)
Iter1_SVM   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, 
                         Recall = NULL, specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos1, kfold  == i)
  Entrenamiento <- subset(datos1, !kfold == i) 
  Modelo <- svm(respuesta ~ APRO_1+ REP_1+ prom_1+ psu_matematica+
                  psu_ciencia+psu_nem, data = Entrenamiento, kernel="radial", gamma=0.1, cost=10)
  Prediccion <- predict(Modelo, new= Test, type="class")
  MC            <- table(Prediccion, Test$respuesta)           
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter1_SVM          <- rbind(Iter1_SVM, data.frame(Iter1_SVM = i,Accurracy = accurracy, 
                                                  Precision = precision, Recall = recall, Specificidad = specificidad,
                                                  Score = score))
}
Iter1_SVM
mean(Iter1_SVM$Accurracy)
mean(Iter1_SVM$Precision)
mean(Iter1_SVM$Recall)
mean(Iter1_SVM$Specificidad)
mean(Iter1_SVM$Score)


#########################################################
###               REDES NEURONALES                    ###
#########################################################
library(readxl)
library(nnet)
library(caret)
library(NeuralNetTools)
datos <- read_excel("C:/Users/FELIPE/Desktop/DATOS_ALUMNOS/Fac_Ciencias/CIENCIAS_T0.xlsx")
datos1 <- read_excel("C:/Users/FELIPE/Desktop/DATOS_ALUMNOS/Fac_Ciencias/CIENCIAS_T1.xlsx")
attach(datos)
attach(datos1)

# Elimina variables 
datos <- datos[,-9]
datos <- datos[,-5]
datos <- datos[,-1]
colnames(datos)[20]<-"respuesta"

datos1 <- datos1[,-9]
datos1 <- datos1[,-5]
datos1 <- datos1[,-1]
datos1 <- datos1[,-20]
colnames(datos1)[20]<-"respuesta"
datos1[is.na(datos1$prom_1), "prom_1"] <- 0

# Datos normalizados
apply(datos,2,range)
max <- apply(datos, 2, max) 
min <- apply(datos, 2, min)
apply(datos1,2,range)
max1 <- apply(datos1, 2, max) 
min1 <- apply(datos1, 2, min)

datos_norm <- as.data.frame(scale(datos, center = min, scale = max- min))
datos_norm$respuesta <- factor(datos_norm$respuesta)
datos_norm1 <- as.data.frame(scale(datos1, center = min1, scale = max1- min1))
datos_norm1$respuesta <- factor(datos_norm1$respuesta)


set.seed(2018)
total_individuos <- nrow(datos_norm)
total_individuos
entrenamiento <- round(total_individuos*0.8)
entrenamiento
prueba <- sample(1:total_individuos, size=entrenamiento)
datos.entrenamiento <- datos[prueba,]

total_individuos1 <- nrow(datos_norm1)
total_individuos1
entrenamiento1 <- round(total_individuos1*0.8)
entrenamiento1
prueba1 <- sample(1:total_individuos1, size=entrenamiento1)
datos1.entrenamiento <- datos1[prueba1,]
###################################
### selección de parámetros t=0 ###
###################################
#############################
### selección de variables###
#############################
modelo = glm(respuesta~.,data=datos.entrenamiento, family = "binomial")
summary(modelo)
step(modelo, direction = "backward")
step(modelo, direction = "forward")
step(modelo, direction = "both")

modelo1 = glm(respuesta~.,data =datos1.entrenamiento, family = "binomial")
summary(modelo1)
step(modelo1, direction = "backward")
step(modelo1, direction = "forward")
step(modelo1, direction = "both")
################################################
parametros <- train(respuesta~psu_matematica+psu_nem+familia_salud +
                      familia_proseguir_estudios, data=datos.entrenamiento, method="nnet", trace=F, 
                    tuneGrid=expand.grid(.size=c(3,4,5),.decay=c(0,0.1,0.2,0.3,0.4,0.5)))
plot(parametros)
summary(parametros)
parametros$bestTune
size <- parametros$bestTune$size      #N° capas ocultas 
decay <- parametros$bestTune$decay    #Para evitar el sobre-ajuste

# selección de parámetros t=1
parametros1 <- train(respuesta~ingreso_carrera+psu_lenguaje+psu_matematica+
                       familia_salud+APRO_1, data=datos1.entrenamiento, method="nnet", trace=F, 
                    tuneGrid=expand.grid(.size=c(3,4,5,6),.decay=c(0.3,0.5,0.6,0.7)))
plot(parametros1)
summary(parametros1)
parametros1$bestTune
size1 <- parametros1$bestTune$size      #N° capas ocultas 
decay1 <- parametros1$bestTune$decay


##################
### Validación ###
##################
set.seed(1994)
Folds         <- 10           
#t=0
datos_norm$kfold   <- sample(1:Folds, nrow(datos_norm), replace = T)
Iter_RN   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, 
                     Recall = NULL, specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos_norm, kfold  == i)
  Entrenamiento <- subset(datos_norm, !kfold == i) 
  Modelo <- nnet(respuesta ~ psu_matematica+psu_nem+familia_salud +
                   familia_proseguir_estudios, data= Entrenamiento, 
                 size=size, decay=decay, trace=F)
  Prediccion <- predict(Modelo, new= Test, type="class")
  MC            <- table(Prediccion, Test$respuesta)     
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter_RN          <- rbind(Iter_RN, data.frame(Iter_RN = i,Accurracy = accurracy, 
                                          Precision = precision, Recall = recall, Specificidad = specificidad,
                                          Score = score))
}
Iter_RN
mean(Iter_RN$Accurracy)
mean(Iter_RN$Precision)
mean(Iter_RN$Recall)
mean(Iter_RN$Specificidad)
mean(Iter_RN$Score)

#t=1
datos_norm1$kfold   <- sample(1:Folds, nrow(datos_norm1), replace = T)
Iter1_RN   <- data.frame(iteracion = NULL, Accurracy = NULL, Precision = NULL, 
                     Recall = NULL, specificidad = NULL, Score = NULL)
for (i in 1:Folds)
{
  Test          <- subset(datos_norm1, kfold  == i)
  Entrenamiento <- subset(datos_norm1, !kfold == i) 
  Modelo        <- nnet(respuesta ~ ingreso_carrera+psu_lenguaje+psu_matematica+
                        familia_salud+APRO_1, data= Entrenamiento, 
                        size=size1, decay=decay1, trace=F)
  Prediccion    <- predict(Modelo, new= Test, type="class")
  MC            <- table(Prediccion, Test$respuesta)     
  accurracy     <- sum(diag(MC)) / sum(MC)
  precision     <- MC[1,1] / (MC[1,1] + MC[1,2])
  recall        <- MC[1,1] / (MC[1,1] + MC[2,1])
  specificidad  <- MC[2,2] / (MC[2,2] + MC[1,2])
  score         <- (2*precision*recall)/(recall+precision)
  Iter1_RN          <- rbind(Iter1_RN, data.frame(Iter1_RN = i,Accurracy = accurracy, 
                                          Precision = precision, Recall = recall, Specificidad = specificidad,
                                          Score = score))
}

Iter1_RN
str(Iter1_RN)
mean(Iter1_RN$Accurracy)
mean(Iter1_RN$Precision)
mean(Iter1_RN$Recall)
mean(Iter1_RN$Specificidad)
mean(Iter1_RN$Score)

#########################################
###      GRÁFICOS DE RED NEURONAL     ###
#########################################
library(png)
png("red_0.jpg", width=1000)
plotnet(Modelo, skip = F,x_names = c("Matemáticas","NEM","Cobertura salud","Pros. Estudios"))
dev.off()

png("red_1.jpg", width=1000)
plotnet(Modelo, skip = TRUE,x_names = c("Carrera", "Lenguaje","Matemáticas","Cobertura salud","Ramos Apro."))
dev.off()

#########################################
###         GRÁFICOS DE LINEA         ###
#########################################
Iter_RL
Iter1_RL
Iter_SVM
Iter1_SVM
Iter_RN
Iter1_RN


library(png)
metodo <- Iter_SVM
png("SVM_0.jpg", width=840)
plot_colores <- c("springgreen","purple","brown1","snow4","orange")
plot(metodo$Precision, axes=T , type = "o", lwd= 2,lty=1, pch=21, cex=.8,
     xlab ="N° de iteración", ylab="Eficiencia en la predicción",
     xlim = c(1,10), ylim= c(0,1), col = plot_colores[1])
lines(metodo$Specificidad, lty=1, cex=.8, pch=22, type = "o", lwd= 2, col = plot_colores[2])
lines(metodo$Accurracy, lty=1, cex=.8, pch=23, type = "o", lwd= 2, col = plot_colores[3])
lines(metodo$Recall, lty=1, cex=.8, pch=24, type = "o", lwd= 2, col = plot_colores[4])
lines(metodo$Score, lty=1, cex=.8, pch=25, type = "o", lwd= 2, col = plot_colores[5])
legend("bottom", horiz = F, lwd=2, pch= 21:25, legend = c(
  paste("Precisión=", format(mean(metodo$Precision)*100,digits=4), "%"),
  paste("Especificidad=", format(mean(metodo$Specificidad)*100,digits=4), "%"),
  paste("Exactitud=", format(mean(metodo$Accurracy)*100,digits=4), "%"),
  paste("Sensibilidad=", format(mean(metodo$Recall)*100,digits=4), "%"),
  paste("F-1=", format(mean(metodo$Score)*100,digits=4), "%")
),cex = 1, col = plot_colores, lty = c(1,1,1,1,1))
box()
dev.off()
#########################################
###         GRÁFICOS DE BARRA         ###
#########################################
t0<-  Iter_RN  # matriz de resultados validación cruzada por método (t=0)
t1<-  Iter1_RN #matriz de resultados validación cruzada por método (t=1)
vector_medias <-matrix(c(mean(t0$Accurracy),mean(t1$Accurracy),
                         mean(t0$Precision),mean(t1$Precision),
                         mean(t0$Recall),mean(t1$Recall),
                         mean(t0$Specificidad),mean(t1$Specificidad),
                         mean(t0$Score),mean(t1$Score)),
                       nrow=2,byrow=F)
colnames(vector_medias)<-c("Exactitud","Precisión","Sensibilidad","Especificidad","F-1")

barplot_colores <- c("springgreen","orange")
png("bar_RN.jpg", width=840)
barplot(as.matrix(vector_medias), ylab= "Eficiencia en la predicción",
        ylim=c(0,1),beside=TRUE, col=barplot_colores)
legend("topleft", c("Clasificación pre-ingreso","Clasificación primer año"), 
       cex=1.2, bty="n", fill=barplot_colores);
dev.off()
