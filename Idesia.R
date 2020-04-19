######################################################
#### LECTURA DE DATOS Y LIBRERIAS ####################
######################################################

library(dplyr)
library(caret)
library(ggplot2)
library(alr3)
library(betareg)
#Lectura de datos#
yul <- readRDS(file="D:/Documents/GitHub/LOI_research/loi.rds")
yul$MO_WyB <- yul$MO_WyB/100
yul$MO_LOI <- yul$MO_LOI/100

#datos de Diferencia de valores de Wilcoxon
yul.dife <- readRDS("D:/Documents/GitHub/LOI_research/dif.rds")

####################################################
#### RESULTADO DE LA PRUEBA DE WILCOXON ############
####################################################
ggplot(yul.dife,aes(x=T.,y=DIF,colour=CC)) +
  geom_point() + geom_line() + theme_bw()
  xlab("Temperatura (°C)") + ylab("Diferencia")


######################################
#### MODELAMIENTO #####
######################################
yul2 <- subset(yul,TEMP == "300" | TEMP == "400" | TEMP == "500")
yul2 <- subset(yul2,MO_WyB < 0.15)  

###
### MODELO COMPLETO
###
md1 <- betareg(MO_WyB~MO_LOI+TEMP+Ca,data=yul2,link = "logit")
summary(md1)
resid.md1 <- residuals(md1,type="pearson")
eje.x <- as.numeric(names(resid.md1))
df <- data.frame(x=eje.x,y=resid.md1)

#Evaluar residuos totales
ggplot(data = df, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()

ggplot(data = yul2, aes(MO_LOI*100, residuals(md1,"pearson"))) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + xlab("Materia Orgánica (%)") + ylab("Residuales") + 
  ggtitle("Predictor Lineal")

ggplot(data = yul2, aes(Ca, residuals(md1,"pearson"))) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + xlab("CaCO3 (%)") + ylab("Residuales") + 
  ggtitle("Predictor Lineal")

### MODELO LINEALIZANDO LA RELACION
md2 <- betareg(MO_WyB~log(MO_LOI)+TEMP+Ca,data=yul2,link = "logit")
summary(md2)
resid.md2 <- residuals(md2,type="pearson")
eje.x <- as.numeric(names(resid.md2))
df <- data.frame(x=eje.x,y=resid.md2)

ggplot(data = df, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()

ggplot(data = yul2, aes(MO_LOI, residuals(md2,"pearson"))) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + xlab("Materia Orgánica (%)") + ylab("Residuales") + 
  ggtitle("Predictor Lineal")

ggplot(data = yul2, aes(Ca, residuals(md2,"pearson"))) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw() + xlab("CaCO3 (%)") + ylab("Residuales") + 
  ggtitle("Predictor Lineal")


#Evaluacion de Valores Influyentes
indice <- which(abs(resid.md2) > 2)

#Revisar los valores Influyentes
yul2[indice,]

###
### REMODALAMIENTO SIN INFLUYENTES
###
md3 <- update(md2, subset = -indice)
summary(md3)

#Comparar ambos modelos
AIC(md1,md2,md3) #Nos quedamos con el modelo 2

#Obtener las métricas por cross validation

set.seed(666)
breg.cv(data = yul2,fm.es = MO_WyB~MO_LOI+TEMP+Ca,respuesta = 4,k = 10,t = 1000)
set.seed(555)
breg.cv(data = yul2,fm.es = MO_WyB~log(MO_LOI)+TEMP+Ca,respuesta = 4,k = 10,t = 1000)










##############################################
#K-fold Cross validation for beta regression
##############################################
#data = data.frame con los datos
#fm.es = Estructura del modelo
#respuesta = indice de la variable respuesta en data
#k y t = fold y repeticiones
breg.cv <- function(data,fm.es,respuesta,k,t,...){
  require("caret")
  n <- nrow(data)
  particion <- createMultiFolds(y=data[,respuesta],k = k,times = t)
  modelos <- list()
  MSE_te <- rep(0,times=length(particion))
  pseR2 <- rep(0,times=length(particion))
  phi <- rep(0,times=length(particion))
  nk <- rep(0,times=length(particion))
  for (j in 1:length(particion)) {
    nk[j] <- n - length(particion[[j]])
  }
  for (i in 1:length(particion)) {
    modelos[[i]] <- betareg(formula = fm.es,data = data[particion[[i]],])
    MSE_te[i] <- mean((data[-particion[[i]],respuesta] - predict(modelos[[i]],data[-particion[[i]],]))^2)
    pseR2[i] <- modelos[[i]]$pseudo.r.squared
    phi[i] <- modelos[[i]]$coefficients$precision
  }
  MSE_te <- matrix(MSE_te,nrow = t,ncol = k)
  pseR2 <- matrix(pseR2,nrow = t,ncol = k)
  phi <- matrix(phi,nrow = t,ncol = k)
  nk <- matrix(nk,nrow=t,ncol=k)
  MSE_CV <- rowSums(MSE_te*nk)/n
  pseR2_CV <- rowSums(pseR2*nk)/n
  phi_CV <- rowSums(phi*nk)/n
  return(list("MSE"=mean(MSE_CV),"MSE_sd"=sd(MSE_CV),"pR2"=mean(pseR2_CV),"pR2_sd"=sd(pseR2_CV),
              "phi"=mean(phi_CV),"phi_sd"=sd(phi_CV)))
}
