setwd("C:/Users/USER/Documents/BASM")


library(lsa)
data0 <- read.csv("piccollage_accounts_bundles.csv")


generateTop5 <-function( mydata ){
  data1<- mydata[1:nrow(mydata),2:ncol(mydata)]
  ndimensions <- ncol(mydata)-1
  ncases <- nrow(mydata)
   data2 <- matrix( as.numeric(unlist(data1)) , ncol=ndimensions)
  data3<- cosine(data2)
 
  
  coln<- colnames(data1)
  colnames(data3)<- coln
  row.names(data3)<- coln
  
  recomendations <- c()
  for (i in 1: (ndimensions)) {
    aaaaa<- row.names(data.frame(sort(data3[i,], decreasing = TRUE)))[2:6]
    recomendations <- c( recomendations , aaaaa )

  }
  recomendations <- matrix( recomendations , ncol=5 , byrow=TRUE )
  rownames(recomendations) <- coln
  return( recomendations )
  for (yy in 1: (ndimensions)) {
    b <- row.names(data.frame(sort(data3[yy,], decreasing = TRUE)))[2:6]
    assign(toString(coln[yy]), b )
  }
}




data6<- data.frame(read.csv("piccollage_accounts_bundles.csv"))
coln<- colnames(data6)
users<- data6$account_id

y <- c()
for (i in 1:(ncol(data0)-1)) {
    m<-mean(data6[,1+i]) 
    x <- c()
    for (j in 1:nrow(data6)){
      a <- data6[j,1+i] - m
      x <- c(x, a)
      }
   assign( paste("b",i, sep = ""), x)
   y<- cbind(y, get( paste("b",i, sep = "") ))
}

colnames(y)<- coln 
y<- cbind( users, y)



generateTop5(y)



data6<- data.frame(read.csv("piccollage_accounts_bundles.csv"))
coln<- colnames(data6)[-1]
z <- c()
for (i in 1:nrow(data0)) {
  vec<- as.numeric(data6[i,2:ncol(data6)])
  m<-mean( vec ) 
  x <- c()
  for (j in 1:(ncol(data6)-1)) {
    a <- data6[i,1+j] - m
    x <- c(x, a)
  }
  assign( paste("c",i, sep = ""), x)
  z<- rbind(z, get( paste("c",i, sep = "") ))
}

colnames(z)<- coln 
users<- data0$account_id
z<- cbind( users, z)
write.csv(z, file= "z.csv", row.names = FALSE)
z <- c()

z<- data.frame(read.csv("z2.csv"))
generateTop5(z)


    
