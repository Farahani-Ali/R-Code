
predict_State_Iteration1 <-
  function(chanceMatrix, previousState) {     # For the first iteration
    
    randomNumber <- runif(1)
    state <- NULL
    


    if (previousState == 0)
      state <- sample(1:3, 1)
    
    else if (randomNumber < chanceMatrix[previousState, 1])
      state <- 1
    
    else if (chanceMatrix[previousState, 1] < randomNumber  &
             randomNumber < chanceMatrix[previousState, 2])
      state <- 2
    
    else
      state <- 3
    
    state
  }



fillNA_First_Iteration <- function(matrix, chanceMatrix, indexSet) {
  
  for (elm in indexSet) {

    if (elm < nrow(matrix))  # in first column
      
      matrix[elm] <-
        predict_State_Iteration1(chanceMatrix, 0)   # There is no previous state
    
    else{
      previousState <- matrix[elm - nrow(matrix)]
      matrix[elm] <- predict_State_Iteration1(chanceMatrix,
                                            previousState)  # The previous state exist in -nrow(matrix) indices before
    
    }
  }
  
  matrix
  
}

