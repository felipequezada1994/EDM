Iter_RL<-read.csv("C:/Users/FELIPE/Documents/Iter_RL.csv", header=T)
Iter1_RL<-read.csv("C:/Users/FELIPE/Documents/Iter1_RL.csv", header=T)
Iter_SVM <-read.csv("C:/Users/FELIPE/Documents/Iter_SVM.csv", header=T)
Iter1_SVM <-read.csv("C:/Users/FELIPE/Documents/Iter1_SVM.csv", header=T)
Iter_RN <-read.csv("C:/Users/FELIPE/Documents/Iter_RN.csv", header=T)
Iter1_RN <-read.csv("C:/Users/FELIPE/Documents/Iter1_RN.csv", header=T)# Time series for each county, with transparency

#########################################
###         GRÁFICOS DE LINEA         ###
#########################################
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

#####################################
###   Gráfico con transparencia   ###
#####################################
head(Iter1_RL)
Iter1_RL<- Iter1_RL[,-1]
Iter1_RL<- Iter1_RL[,-1]
Iter<- t(Iter1_RL)
data.frame(Iter)

png("transparencia.jpg", width=840)
plot(0, 0, xlim = c(1,10), ylim= c(0,1), type = "n", las=1,
     bty = "n", xlab ="Iteración", ylab="Eficiencia de prediccion")

for (i in 1:length(Iter[,1])) {
  lines(1:10, Iter[i,1:10], col="#00000010")
}
dev.off()

