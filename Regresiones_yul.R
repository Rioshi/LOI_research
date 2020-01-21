library(caret)
library(ggplot2)
#Lectura de datos#
yul <- read.csv(file.choose(),header = TRUE,sep=";")
str(yul)
yul$TEMP <- as.factor(yul$TEMP)
yul <- subset(df,TEMP=="700"& MO_WyB < 8)
#Modelo Lineal con Cross Validation
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 1000) #5% de las observaciones
md.1 <- train(MO_WyB~MO_LOI+Ca,data=yul, method = "lm",
               trControl = train.control)
summary(md.1)
print(md.1)

#Grafico del modelo lineal sin carbonatos
jpeg("/home/carlos/Documentos/reg_total.jpeg", width = 20, height = 15, units = 'cm', res = 400)
ggplot(yul,aes(x=MO_LOI,y=MO_WyB,colour=CC,shape=CC)) +
  geom_point(size=3)+
  geom_smooth(method=lm,se = TRUE, aes(group=1)) +
  xlab("M.O. LOI (%)") + ylab("M.O. WyB (%)") +
  theme_minimal() +
  theme(axis.text=element_text(size=11),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank())
dev.off()


#Grafico del modelo lineal con carbonatos
pred <- predict(md.1$finalModel, yul, se.fit = TRUE, interval = "confidence")
pred <- data.frame(MO_WyB = pred$fit[,1], MO_LOI = yul$MO_LOI, lwr = pred$fit[,2], upr = pred$fit[,3],CC=yul$CC)

jpeg("/home/carlos/Documentos/reg_total_carb.jpeg", width = 20, height = 15, units = 'cm', res = 400)
ggplot(pred,aes(x=MO_LOI,y=MO_WyB,colour=CC,shape=CC)) +
  geom_point(size=1)+
  geom_smooth(method=lm,se = TRUE, aes(group=1)) +
  xlab("M.O. LOI (%)") + ylab("M.O. WyB (%)") +
  theme_minimal() +
  theme(axis.text=element_text(size=11),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.title=element_blank())
dev.off()






#Modelo Lineal con carbonatos a 300°C
yul.300 <- subset(yul,TEMP=="300")
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100) #5% de las observaciones
md.2 <- train(MO_WyB~MO_LOI+Ca,data=yul, method = "lm",
              trControl = train.control)
summary(md.2)
print(md.2)
