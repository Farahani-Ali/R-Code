predictState <- function(chanceMatrix){
  
  randomNumber<-runif(1) 
  state<-NULL

  # print(paste0( "Random Number is: ",randomNumber ))
  
#  print(paste0( "chanceMatrix is: ",chanceMatrix ))
    
  if( randomNumber < chanceMatrix[1] )
    state <- 1
  else if( chanceMatrix[1] < randomNumber  & randomNumber < chanceMatrix[2] )
    state <- 2
  else
    state <- 3
  
  state
}


fillNA_After_First_Iteration<-function(matrix,psiNew,indexSet){
  
#  print("Matrix Data in Last Iteration")
 # print(matrix)
  
#  print("___________________")
  
#  print("New Psi")
#  print(psiNew)
  
#  print("___________________")
  
 # print("indexSet")
#  print(indexSet)
  
#  print("___________________")
  
  for(elm in indexSet){                    # Fill all the NAs
    
    previousState <- matrix[elm-nrow(matrix)]           # previous state in current data matrix
    
    nextState <- matrix[elm+nrow(matrix)]        # previously PREDICTED state
    
  #  if (elm < ncol(matrix))  
  #     nextState <- matrix[elm+5] 
        
  #  else
  #    nextState <- matrix[elm] 
    
    
    mul<-psiNew[previousState,] * psiNew[,nextState]
    chanceMatrix <-cumsum( mul /  sum(mul) )
  
  #   print("chanceMatrix")
  #    print(chanceMatrix)
    
   #   print("___________________")
    
    
    
    
      
    matrix[elm] <- predictState(chanceMatrix)
  }
  
  
#  print("Final Matrix")
#  print(matrix)
        
        
 # print("______________________________________")

  
  matrix
  
}


isPSIsConverged<-function(psiNew,psiLast){
  
  print("Difference between 2 last Psies?")
  
  print(abs(psiNew-psiLast))
  
  
  all(abs(psiNew-psiLast)<0.03)  # return true if all the difference between all the enteries is less than 0.01 
}


printData<-function(psiNew,psiLast,counter){
  
 # print("________________________________________________________________________")
#  print(paste0("Counter: ",counter   ))  
  
  #print("Last PSI")
 # print( psiLast)
  
#  print("NEW PSI")
#  print( psiNew)
  
  # print("Psi differences")
  # print( abs(psiNew-psiLast)<0.01)
  
  print(paste0("Is it converged? ",isPSIsConverged(psiNew,psiLast)))
  
  
}
