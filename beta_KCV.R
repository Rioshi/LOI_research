library(caret)
library(betareg)

# data : data.frame containing data
# form : Model description
# K : Fold number
# resp: the column name of response variable
# R : Repetition number
vc <- 1:100

dd <- createFolds(y = df$MO_LOI,k = 10,list = FALSE)
which(dd == 3)

betaCV <- function(data,form,resp,K,R){
  EVC <- matrix(0,R,K)
  for (j in 1:R) {
  folds <- createFolds(y = data[,resp],k = K,list = FALSE)
  for (i in 1:K) {
    indice <- which(folds == i)
    md <- betareg(formula(form),data = data[-indice,],link = "logit")
    pp <- predict(object = md,newdata=data[indice,])
    EVC[j,i] <- mean((data[indice,resp] - pp)^2)
  }
  }
  return(EVC)
}

betaCV(data=df,form="MO_WyB~MO_LOI",resp="MO_WyB",K=10,R=2)


