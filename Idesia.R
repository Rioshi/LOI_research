library(dplyr)
library(caret)
library(ggplot2)
library(alr3)
library(betareg)
#Lectura de datos#
yul <- readRDS(file="D:/Documents/GitHub/LOI_research/loi.rds")
yul$MO_WyB <- yul$MO_WyB/100
yul$MO_LOI <- yul$MO_LOI/100


######################################
####Modelamiento de datos totales#####
######################################

###
### MODELO COMPLETO
###
md1 <- betareg(MO_WyB~MO_LOI+TEMP+Ca,data=yul,link = "logit")
resid.md1 <- residuals(md1,type="pearson")
eje.x <- as.numeric(names(resid.md1))
df <- data.frame(x=eje.x,y=resid.md1)

ggplot(data = df, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()

plot(x=yul$MO_LOI,y=yul$MO_WyB)

#Evaluacion de Valores Influyentes
indice <- which(abs(resid.md1) > 2)

#Revisar los valores Influyentes
yul[indice,]

###
### REMODALAMIENTO SIN INFLUYENTES
###
md2 <- update(md1, subset = -indice)
summary(md2)

#Comparar ambos modelos
AIC(md1,md2)

#Evaluacion grafica
resid.md2 <- residuals(md2,type="pearson")
eje.x <- as.numeric(names(resid.md2))
df2 <- data.frame(x=eje.x,y=resid.md2)

ggplot(data = df2, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()

### EL SEGUNDO MODELO TIENE MAYOR pseudo R2


######################################
####MODELAMIENTO DE DATOS HASTA 15%###
######################################
yul2 <- subset(yul,MO_WyB<0.15)

md3 <- betareg(MO_WyB~MO_LOI+TEMP+Ca,data=yul,link = "logit")
summary(md3)
resid.md3 <- residuals(md3,type="pearson")
eje.x <- as.numeric(names(resid.md3))
df <- data.frame(x=eje.x,y=resid.md3)

ggplot(data = df, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()













#Evaluacion de Influyentes
indice2 <- which(abs(resid.md2) > 2)
yul[indice2,]

#Remodelar 2da
indice3 <- c(as.numeric(indice),as.numeric(indice2))
md3 <- update(md1, subset = -indice3)
summary(md3)

#Evaluacion grafica
resid.md3 <- residuals(md3,type="pearson")
eje.x <- as.numeric(names(resid.md3))
df3 <- data.frame(x=eje.x,y=resid.md3)

ggplot(data = df3, aes(x,y)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  geom_hline(yintercept=c(-2,2), linetype="dashed", color = "red") +
  theme_bw()

AIC(md1,md2,md3)

set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100) #5% de las observaciones
md.1 <- train(MO_WyB~MO_LOI,data=yul, method = "lm",
              trControl = train.control)
summary(md.1)
print(md.1)

plot(md.1$finalModel)
shapiro.test(residuals(md.1$finalModel))
car::ncvTest(md.1$finalModel)

ggplot(yul,aes(x=MO_LOI,y=MO_WyB)) +
  geom_point(shape=1)+
  geom_smooth(method=lm,se = TRUE, aes(group=1),color="black",size=0.5) +
  xlab("Loss on Ignition (%)") + ylab("Walkley & Black (%)") +
  theme_minimal() +
  theme(axis.text=element_text(size=11),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank())

boxCox(md.1, lambda = seq(0, 1, by = 0.1))
car::influenceIndexPlot(md.1$finalModel)


######################################
####Modelamiento de datos TEMP = 300#####
######################################
md.2 <- lm(MO_WyB~MO_LOI,data=yul, subset=(TEMP=="300"))
summary(md.2)
shapiro.test(residuals(md.2))
car::ncvTest(md.2)
car::influenceIndexPlot(md.2)


set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100) #10% de las observaciones
md.2 <- train(MO_WyB~MO_LOI,data=yul, subset=(TEMP=="300"),method = "lm",
              trControl = train.control)
summary(md.2)
print(md.2)

plot(md.2$finalModel)
shapiro.test(residuals(md.2$finalModel))
car::ncvTest(md.2$finalModel)

ggplot(yul,aes(x=MO_LOI,y=MO_WyB)) +
  geom_point(shape=1)+
  geom_smooth(method=lm,se = TRUE, aes(group=1),color="black",size=0.5) +
  xlab("Loss on Ignition (%)") + ylab("Walkley & Black (%)") +
  theme_minimal() +
  theme(axis.text=element_text(size=11),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank())





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
