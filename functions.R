# selectRandomIndices <- function(matrix){
#   size<-round(length(matrix)*0.2)
#   indices<-sort( sample(1:length(matrix),size ))
#   indices
# }
# 


omitEnteries<- function(matrix,indexes){
  
  matrix[indexes]<-NA
  matrix
  
}


stateChangeCounter <- function(matrix, firstState,secondState){
  
  sum<-0               
  for( row in 1:nrow(mat))
    for( col in 1:(ncol(mat)-1))
      if(!is.na(mat[row,col+1]) & !is.na(mat[row,col]) )
        if(mat[row,col+1]==secondState & 
           mat[row,col]==firstState   )
          sum<-sum+1
        
        #      print(paste0(firstState," to ",secondState, " : ", sum))    
        sum    
}

createStateChangeMatrix<-function(matrix){
  
  count1to1<-stateChangeCounter(matrix,1,1)
  count1to2<-stateChangeCounter(matrix,1,2)
  count1to3<-stateChangeCounter(matrix,1,3)  
  
  count2to1<-stateChangeCounter(matrix,2,1)
  count2to2<-stateChangeCounter(matrix,2,2)
  count2to3<-stateChangeCounter(matrix,2,3)  
  
  count3to1<-stateChangeCounter(matrix,3,1)
  count3to2<-stateChangeCounter(matrix,3,2)
  count3to3<-stateChangeCounter(matrix,3,3)
  
  rbind(c(count1to1,count1to2,count1to3),
        c(count2to1,count2to2,count2to3),
        c(count3to1,count3to2,count3to3))
}


createTransitionMatrix <- function(matrix){  # obtain psi matrix
  
  matrix <- createStateChangeMatrix(matrix)

    
  row1 <- rdirichlet(1, c(matrix[1,1]+1,matrix[1,2]+1,matrix[1,3]+1))
  row2 <- rdirichlet(1, c(matrix[2,1]+1,matrix[2,2]+1,matrix[2,3]+1))
  row3 <- rdirichlet(1, c(matrix[3,1]+1,matrix[3,2]++1,matrix[3,3]+1))
  
  round(rbind(row1,row2,row3),5)                   
  
}


obtainChanceMatrix <- function(transitionMatrix){
  
  chanceMatrix<-NULL
  for( row in 1:nrow(transitionMatrix))
    chanceMatrix <- rbind(chanceMatrix, cumsum(transitionMatrix[row, ]))
  
  chanceMatrix
  
}
