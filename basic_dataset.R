
#source("functions.R")


getMainDataSet<-function(){
  
  dt<-getRowData()
  
  
  basicPsi <- getFirstPsi()   # First Psi is created by us 
  
  chanceMatrix <- obtainChanceMatrix( basicPsi )
  
  dt<-getFirstCompleteDT(dt,chanceMatrix)
  
  dt
}



getRowData <-function(){
  
  rowDt <- matrix(NA,80,19)
  
  states <- 1:3
  firstcoll <- sample(states, 80, replace = T)

  
  cbind(firstcoll,rowDt)
  
}

getFirstPsi<- function(){
  
  dt <- c(.3, .15,.55, .2,.4,.4,.55,.25,.2)
  matrix(dt, 3, 3, byrow=T)    # First Psi is created by us 
  
}

forcastState<-function(chanceMatrix,randomN,dt,row,column){
  
  if(randomN < chanceMatrix[1])
    dt[row,column] <- 1
  
  else if( randomN > chanceMatrix[1] & randomN < chanceMatrix[2] )
    dt[row,column] <- 2
  
  else
    dt[row,column] <- 3  
  
  dt
}



getFirstCompleteDT<-function(mat,chanceMatrix){
  for (i in 1:nrow(mat)) # row
    for(j in 2:ncol(mat)){    # column
      
      previousState <-mat[i,j-1]   
      randomN <- runif(1)     # Generate random number    
      
      mat <- forcastState(chanceMatrix[previousState,],randomN,mat,i,j)
    }
  
  mat
}

getMissingIndex <- function(mat){
  
#  lowerBound<-nrow(mat)+1
#  upperBound <- length(mat)-nrow(mat) 
  
#  vec<-  sequence(lowerBound, upperBound)   # SHOULD BE CHANGED
  size <- .2*ncol(mat)*nrow(mat)
  sort(sample (1:length(mat),size, replace = F))
 # sort(sample (81:1520,size, replace = F))
  
}

