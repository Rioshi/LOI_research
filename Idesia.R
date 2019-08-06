library(caret)
library(ggplot2)
library(alr3)
#Lectura de datos#
yul <- read.delim("clipboard",header = TRUE)
str(yul)
yul$TEMP <- as.factor(yul$TEMP)
yul$Repete <- as.factor(yul$Repete)
yul <- subset(yul,MO_WyB<15 & MO_LOI<25)


######################################
####Modelamiento de datos totales#####
######################################

set.seed(123) 
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 100) #5% de las observaciones
md.1 <- train(MO_WyB~log(MO_LOI),data=yul, method = "lm",
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



######################################
####Modelamiento de datos TEMP = 300#####
######################################
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