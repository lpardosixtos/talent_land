rm(list = ls())

library(ggplot2)
library(ggrepel)

#Lectura de datos

dataSel <- read.csv('datos_seleccionados_2017.csv')
dataSel[,-1] <- scale(dataSel[,-1])


#Proyección en componentes principales para visualización
PCA <- princomp(dataSel[,-1], cor= T)

dPCA <- data.frame('Estado' = dataSel[,1], 'Componente_1' = PCA$scores[,1], 'Componente_2' = PCA$scores[,2])

#Método k-medias para agrupamiento
KMEANs <- kmeans(dataSel[,-1], centers = 5)
dataK <- data.frame(dPCA, cluster = KMEANs$cluster)

#Visualización de los clusters
ggplot(dataK)+ geom_point(aes(Componente_1, Componente_2, color = factor(cluster)))+guides(color = FALSE)+
  geom_label_repel(aes(Componente_1, Componente_2, label = Estado),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')
write.csv(dataK, 'KMcluster.csv')