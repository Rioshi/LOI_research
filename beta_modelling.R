##############################
###### LIBRERIAS #############
##############################
library(betareg)
library(nortest)
library(lmtest)
##############################
###### Lectura DAtos##########
##############################

df <- read.delim("clipboard",header = TRUE)
df$TEMP <- as.factor(df$TEMP)
NOVALOR <- which(is.na(df$MO_LOI))
df[NOVALOR,]  

df$MO_WyB <- df$MO_WyB/100
df$MO_LOI <- df$MO_LOI/100

df300 <- subset(df,TEMP=="300")
df400 <- subset(df,TEMP=="400")
df500 <- subset(df,TEMP=="500")
df600 <- subset(df,TEMP=="600")
df700 <- subset(df,TEMP=="700")

###############################################
###### ANALISIS EXPLORATORIO DE DATOS##########
###############################################
ggplot(df, aes(x=MO_WyB)) + 
  geom_histogram(color="darkblue", fill="lightblue") + 
  xlab("Materia Orgánica") + ylab("Frecuencia")

ggplot(df, aes(x=TEMP, y=MO_LOI, color=TEMP)) +
  geom_boxplot() + xlab("") + ylab("Rendimiento (Tm/ha)")

df1 <- df
df <- subset(df,MO_WyB <= 0.20)
########################################
###### Modelos Sin Carbonatos ##########
########################################

#Temperatura 300
md1 <- betareg(MO_WyB~MO_LOI,data=df300,link = "logit")
summary(md1)
plot(md1)


#Test de Wald para los coeficientes || Ho: Coeficiente = 0
coeftest(md1)

#Revision de supuestos y valores anormales
plot(md1, which = 2) #Distancia de cook
plot(md1, which = 5, type = "deviance", sub.caption = "")
plot(md1, which = 1, type = "deviance", sub.caption = "")
DV <- which(abs(residuals(md1,type = "deviance")) >= 2) 
DC <- which(cooks.distance(md1)> 0.05)
DC ; DV

#Rearmado del modelo
df300_1 <- df300[-DC,]
md11 <- betareg(MO_WyB~MO_LOI,data=df300_1,link = "logit")
summary(md11)
plot(md11, which= 2)
plot(md11, which = 5, type = "deviance", sub.caption = "")

DV <- which(abs(residuals(md11,type = "deviance")) >= 2) 
DC <- which(cooks.distance(md11)> 0.05)
DC ; DV

#Rearmado del modelo
md12 <- betareg(MO_WyB~MO_LOI,data=df300[-DC,],link = "logit")
summary(md12)
plot(md11, which= 2)
plot(md11, which = 5, type = "deviance", sub.caption = "")



#Temperatura 400
md2 <- betareg(MO_WyB~MO_LOI+TEMP,data=df400,link = "logit")
summary(md2)
plot(md2)


#Test de Wald para los coeficientes || Ho: Coeficiente = 0
coeftest(md2)

#Revision de supuestos y valores anormales
plot(md2, which = 1:4, type = "pearson")
plot(md2, which = 5, type = "deviance", sub.caption = "")
plot(md2, which = 1, type = "deviance", sub.caption = "")


