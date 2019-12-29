betaCV <- function(data,y,R,K){
  data=as.matrix(data)
  n=dim(data)[1]
  p=dim(data)[2]
  EVC=rep(0, R)
  for (i in 1:R)
  {
    resid=matrix(0,1,K)
    indices=sample(1:n,n,replace=F)
    azar=data[indices,]
    subm=floor(n/K)
    for (j in 1:K)
    {
      unid=((j-1)*subm+1):(j*subm)
      if (j == K)
      {
        unid=((j-1)*subm+1):n
      }
      datap=azar[unid,]
      datae=azar[-unid,]
      ye=datae[,y]
      xe=datae[,-y]
      modelo=betareg(ye~xe)
      unos=rep(1,dim(datap)[1])
      data1=cbind(unos,datap[,-y])
      predict=predict(modelo,newdata=datap[,-y])
      resid[j]=sum((predict-datap[,y])^2)
    }
    EVC[i]=sum(resid)/n
  }
  EVCP=mean(EVC)   
  return (list(EVC=EVC, EVCP=EVCP))
}
betaCV(data=subset(df,TEMP=="300")[,c("MO_WyB","MO_LOI")],y = 1,R = 10,K = 10)
