predictState <- function(chanceMatrix){
  
  randomNumber<-runif(1) 
  state<-NULL
  
  if( randomNumber < chanceMatrix[1] )
    state <- 1
  else if( chanceMatrix[1] < randomNumber  & randomNumber < chanceMatrix[2] )
    state <- 2
  else
    state <- 3
  
  state
}


fillNA_After_First_Iteration<-function(matrix,psiNew,indexSet){
  
  
  for(elm in indexSet){                    # Fill all the NAs
    
    previousState <- matrix[elm-5]           # previous state in current data matrix
    
    previouslyPredictedState <- matrix[elm]  # previously PREDICTED state
    
    
    chanceMatrix <-cumsum( psiNew[previousState,] * psiNew[,previouslyPredictedState] / 
                             sum(psiNew[previousState,] * psiNew[,previouslyPredictedState]))
    
    
    matrix[elm] <- predictState(chanceMatrix)
  }
  
  matrix
  
}


isPSIsConverged<-function(psiNew,psiLast){
  
  all(abs(psiNew-psiLast)<0.01)  # return true if all the difference between all the enteries is less than 0.01 
}


printData<-function(psiNew,psiLast,counter){
  
  print("/////////////////////////////////////////////////////")
  print(paste0("Counter: ",counter   ))  
  
  print("Last PSI")
  print( psiLast)
  
  
  print("NEW PSI")
  print( psiNew)
  # print("Psi differences")
  # print( abs(psiNew-psiLast)<0.01)
  
  print(paste0("Is it converged? ",isPSIsConverged(psiNew,psiLast)))
  
  
}



