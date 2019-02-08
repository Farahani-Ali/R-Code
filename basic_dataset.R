
getMainDataSet<-function(){
  
  dt<-getRowData()
  
  
  basicPsi <- getFirstPsi()   # First Psi is created by us 
  
  chanceMatrix <- obtainChanceMatrix( basicPsi )
  
  dt<-getFirstCompleteDT(dt,chanceMatrix)
  
  dt
}



getRowData <-function(){
  
  rowDt <- matrix(NA,80,20)
  
  rowDt[,1] <- sample(1:3, 80, replace = T) # Set the first column

  rowDt

  
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
  
  size <- .2*ncol(mat)*nrow(mat)
  sort(sample (1:length(mat),size, replace = F))

}

