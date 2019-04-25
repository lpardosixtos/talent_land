library(psych)

#Regresi�n para el ITAEE
datos1=read.csv("Datos.csv",header = TRUE)
datos1=datos1[,-1]
#Histograma de cada variable
multi.hist(x = datos1, dcol = c("black", "red"),bcol=4, dlty = c("dotted", "solid"),
           main = c("ITAEE","PIB","Sucursales","Cajeros","Densidad_poblaci�n","I.E.D","Prod_mayor","Prod_menor","Prod_manof","Salario"))
#Visualizaci�n de variables a pares
pairs(datos1,col=4,pch=20)
#Predictor del ITAEE respecto a las variables explicativas
reg1=lm(datos1[,1]~datos1[,3]+datos1[,4]+datos1[,5]+datos1[,6]+datos1[,7]+datos1[,8]+datos1[,9]+datos1[,10])
summary(reg1)
#Regresi�n para el PIB
datos2=read.csv("Datos_anuales.csv",header = TRUE)
datos2=datos2[,-1]
reg2=lm(datos2[,1]~datos2[,2]+datos2[,3]+datos2[,4]+datos2[,5]+datos2[,6]+datos2[,7]+datos2[,8]+datos2[,9])
summary(reg2)

#Regresi�n con la union de las variables significativas
reg3=lm(datos1[,1]~datos1[,3]+datos1[,5]+datos1[,6]+datos1[,7]+datos1[,8]+datos1[,10])
summary(reg3)

reg4=lm(datos2[,1]~datos2[,2]+datos2[,4]+datos2[,5]+datos2[,6]+datos2[,7]+datos2[,9])
summary(reg4)

#Regresi�n con la intersecci�n de las variables explicativas
reg5=lm(datos1[,1]~datos1[,3]+datos1[,5]+datos1[,6])
summary(reg5)

reg6=lm(datos2[,1]~datos2[,2]+datos2[,4]+datos2[,5])
summary(reg6)
