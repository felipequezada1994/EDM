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

library(png)
png("red_0.jpg", width=1000)
plotnet(Modelo, skip = F,x_names = c("Matemáticas","NEM","Cobertura salud","Pros. Estudios"))
dev.off()

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


png("red_1.jpg", width=1000)
plotnet(Modelo, skip = TRUE,x_names = c("Carrera", "Lenguaje","Matemáticas","Cobertura salud","Ramos Apro."))
dev.off()






#########################################################
###                       GRÁFICO                     ###
#########################################################
Iter_RL
Iter1_RL
Iter_SVM
Iter1_SVM
Iter_RN
Iter1_RN
write.csv(Iter_RL, file="Iter_RL.csv")
write.csv(Iter1_RL, file="Iter1_RL.csv")
write.csv(Iter_SVM, file="Iter_SVM.csv")
write.csv(Iter1_SVM, file="Iter1_SVM.csv")
write.csv(Iter_RN, file="Iter_RN.csv")
write.csv(Iter1_RN, file="Iter1_RN.csv")

##############################
### Gráficos de eficiencia ###
##############################




###########################
### Gráficos de Barplot ###
###########################

vector1 <- c(Iter_RL$Precision,Iter_SVM$Precision,Iter_RN$Precision)
vector2 <- c(Iter_RL$Recall,Iter_SVM$Recall,Iter_RN$Recall)
vector3 <- c(Iter_RL$Score,Iter_SVM$Score,Iter_RN$Score)
vector4 <- as.factor(c(rep('RL',10),rep('SVM',10),rep('RN',10)))
valores <- data.frame(vector1,vector2,vector3,vector4)
colnames(valores)= c("Precision","Recall","Score","Algoritmo")

vector1.1 <- c(Iter1_RL$Precision,Iter1_SVM$Precision,Iter1_RN$Precision)
vector2.1 <- c(Iter1_RL$Recall,Iter1_SVM$Recall,Iter1_RN$Recall)
vector3.1 <- c(Iter1_RL$Score,Iter1_SVM$Score,Iter1_RN$Score)
vector4.1 <- as.factor(c(rep('RL',10),rep('SVM',10),rep('RN',10)))
valores.1 <- data.frame(vector1.1,vector2.1,vector3.1,vector4.1)
colnames(valores.1)= c("Precision","Recall","Score","Algoritmo")

write.csv(valores, file="valores.csv")
write.csv(valores.1, file="valores1.csv")


Precision <- aggregate(Precision ~ Algoritmo,valores, mean)
Recall    <- aggregate(Recall ~ Algoritmo, valores, mean)
Score     <- aggregate(Score ~ Algoritmo, valores, mean)

valMedios <- matrix(c(Precision$Precision, Recall$Recall, Score$Score),
                       nrow=3, ncol=3)

rownames(valMedios) <- Precision$Algoritmo

barplot(valMedios, beside = T, horiz =T, col = heat.colors(3),
           legend=F,xlim = c(0,1),ylim = c(0,15),
        names.arg = c('Precision','Recall','F-1'),args.legend = list(x="center"))

legend("topright", c("RL", "RN", "SVM"), horiz=T,inset = .02,cex=0.4, bty="n", fill=heat.colors(3))


#########################################################
###                 MUESTRA BALANCEADA                ###
#########################################################
#prop.table(table(datos$respuesta))
#table(datos.entrenamiento$respuesta)
#datos.balanceado <- ovun.sample(respuesta~., data=datos.entrenamiento,
#                                method="both", p=0.5, N=675,  
#                                seed=1)$data
#table(datos.balanceado$respuesta)

########################################################################
### PROBABILIDAD POSTERIOR DE CLASIFICACIÓN CON LA DISTRIBUCIÓN BETA ###
########################################################################
Modelo <- svm(respuesta ~ psu_matematica+psu_ciencia+psu_lenguaje+ingreso_carrera, data = datos.entrenamiento, method= "C-classification",
              kernel="radial", gamma=0.01, cost=10)
pred    <- predict(Modelo, datos.prueba, type = "class")  
max(pred)
min(pred)
#Normalización de vector de respuesta
real <- datos.prueba[,20]
real
names (real)[1] = "Real"
names (pred)[1] = "Predicción"
comparacion <-cbind(pred,real)
comparacion
head(comparacion)

normalizado = as.data.frame(lapply(comparacion, 
                                   function(columna){ 
                                     if(is.numeric(columna)) 
                                       (columna-min(columna))/(max(columna)-min(columna)) 
                                     else columna}))

normalizado
normalizado <- normalizado$pred
names (normalizado)[1] = "normalizado"

comparacion <-cbind(comparacion,normalizado)
head(comparacion)
class(comparacion)
### Probabilidad posterior de clasificación
datosseg <- split(comparacion, comparacion$Real)
datosseg

beta_exito <- datosseg$"1"
beta_fracaso <- datosseg$"0"
head (beta_exito)
min(beta_exito$normalizado)
max(beta_exito$normalizado)
min(beta_fracaso$normalizado)
max(beta_fracaso$normalizado)

media_exito <- mean(beta_exito$normalizado)
var_exito <- var(beta_exito$normalizado)
media_fracaso <- mean(beta_fracaso$normalizado)
var_fracaso <- var(beta_fracaso$normalizado)

hist(beta_exito$normalizado)
hist(beta_fracaso$normalizado)

# Función de distribución acumulada
media <- media_exito
media
varianza <- var_exito
varianza
# Parámetro alpha
alpha <- media * (((media*(1-media))/varianza)-1)
alpha
# Parámetro beta
beta <- (1-media) * (((media*(1-media))/varianza)-1)
beta
qbeta(0.1, alpha, beta, ncp = 0, lower.tail = T, log.p = F)
curve(dbeta(x,alpha,beta))


# Prueba KS para hipótesis de distribución beta
library(MASS)
par(mfrow=c(1,2))
hist(beta_exito[,3], xlab="Predicción", ylab="Frecuencia", las=1, main="Predicciones de éxito")
plot(density(beta_exito[,3]), xlab="Predicción", ylab="Densidad", las=1, main="")

prueba <- beta_exito[,3]
class(prueba)
mode(prueba)
prueba <- (beta_exito[2:20,3])
var(prueba)
prueba <- as.numeric(prueba)
class(beta_exito$col3)
class(beta_exito$col3)
max(beta_exito[2:20,3])
prueba2 <- as.numeric(prueba)
Peso <- c(750.0, 749.3, 752.5, 748.9, 749.9, 748.6, 750.2, 748.4, 747.8, 749.3, 749.6, 749.0, 747.7, 748.3, 750.5, 750.6, 750.0, 750.4, 752.0, 750.2, 751.4, 750.9, 752.4, 751.7, 750.6)

ajuste <- fitdistr(prueba,"beta", start = list (shape1 = 0.7, shape2 = 0.015))
ajuste
xKs<- ks.test(beta_exito[2:20,3], "pbeta", mean =ajuste$estimate[1], sd= ajuste$estimate[2])
Ks
