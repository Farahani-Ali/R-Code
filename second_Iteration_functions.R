predictState <- function(chanceMatrix){
  
  randomNumber<-runif(1) 
  state<-NULL

  print(paste0( "Random Number is: ",randomNumber ))
  
    
  if( randomNumber < chanceMatrix[1] )
    state <- 1
  else if( chanceMatrix[1] < randomNumber  & randomNumber < chanceMatrix[2] )
    state <- 2
  else
    state <- 3
  
  state
}


fillNA_After_First_Iteration<-function(matrix,psiNew,indexSet){
  
  print("Matrix Data in Last Iteration")
  print(matrix)
  
  print("New Psi")
  print(psiNew)
  
  
  for(elm in indexSet){                    # Fill all the NAs
    
    previousState <- matrix[elm-5]           # previous state in current data matrix
    
    previouslyPredictedState <- matrix[elm]  # previously PREDICTED state
    
    
    
    mul<-psiNew[previousState,] * psiNew[,previouslyPredictedState]
    chanceMatrix <-cumsum( mul /  sum(mul))
    
    matrix[elm] <- predictState(chanceMatrix)
  }
  
  
  print("Final Matrix")
  print(matrix)
  
  #matrix
  
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



