
predictStateIteration1(chanceMatrix,3  )

predictStateIteration1 <- function(chanceMatrix,previousState){  # For the first iteration
  
  randomNumber<-runif(1) 
  state<-NULL

  print(paste0("Random Number: ", randomNumber)  )
  
  
    
  if(previousState == 0 )
    state<-sample(1:3,1)
  
  else if( randomNumber < chanceMatrix[previousState,1] )
    state <- 1
  
  else if( chanceMatrix[previousState,1] < randomNumber  & randomNumber < chanceMatrix[previousState,2] )
    state <- 2
  
  else
    state <- 3
  
  print(paste0("Predicted State",state))
  
  state
}



fillNA_First_Iteration<-function(matrix,chanceMatrix,indexSet){
  
  print(indexSet)
  
  for( elm in indexSet ){
      print("Heyyyyyyyyyyyyyyyyy") 
    if(elm<nrow(matrix))  
      matrix[elm] <- predictStateIteration1(chanceMatrix, 0)   # There is no previous state
    else 
      matrix[elm] <- predictStateIteration1(chanceMatrix, matrix[elm-5])  # The previous state exist in -5 indices before
    
  }
  #  matrix
    
}
