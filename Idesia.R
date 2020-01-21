library(googlesheets)
library(dplyr)
library(caret)
library(ggplot2)
library(alr3)
#Lectura de datos#
gs_auth()
my_sheets <- gs_ls()
gap <- gs_title("Experimentos")
yul <- gap %>%
  gs_read(ws = "Yuleysi")
yul <- as.data.frame(yul)
rm(my_sheets,gap)

yul$TEMP <- as.factor(yul$TEMP)
yul$Repete <- as.factor(yul$Repete)
yul <- subset(yul,MO_WyB < 8)


######################################
####Modelamiento de datos totales#####
######################################
md.1 <- lm(MO_WyB~MO_LOI,data=yul)
boxCox(md.1, lambda = seq(0, 1, by = 0.1))


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