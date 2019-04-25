### An�lisis Factorial para PIB
data_anu=read.csv("Datos_anuales.csv")
data_anu=data_anu[,-1]
data_anu=data_anu[,-1]
head(data_anu)

datos_rep_pib=cbind(data_anu[,1],data_anu[,3:6])

head(datos_rep_pib)

factanal(datos_rep_pib, factors=2, method="mle")

### An�lisis Factorial para IDAEE
data_trim=read.csv("Datos.csv")
data_trim=data_trim[,-1]
data_trim=data_trim[,-1]
data_trim=data_trim[,-1]
head(data_trim)

datos_rep_itaee=cbind(data_trim[,1],data_trim[,3:4],data_trim[,8])

head(datos_rep_itaee)

factanal(datos_rep_itaee, factors=1, method="mle")

### An�lisis factorial en general

data=read.csv("Datos_anuales.csv")
data=data[,-1]
data=data[,-1]
head(data)

datos=cbind(data[,1],data[,3:6],data[,8])
head(datos)

factanal(datos, factors=3, method="mle")
