fillNA_After_First_Iteration<-function(matrix,psiNew,indexSet){
  
  
  for(elm in indexSet){                # Fill all the NAs
    
    
    if ( length(matrix)-nrow(matrix)<elm  &&
         elm < length(matrix)){       # missed index in LAST column
      
      previousState <- matrix[elm-nrow(matrix)] 
      nextState <- 1
      
    }else if ( elm <nrow(matrix) ){           # missed index in First column
      
      nextState <- matrix[elm+nrow(matrix)]
      previousState <- 1
      
    }else{
      
      previousState <- matrix[elm-nrow(matrix)]   # previous state in current data matrix
      nextState <- matrix[elm+nrow(matrix)]      # next PREDICTED state      
      
    }
    
    chanceMatrix <- getChanceMatrix_After_First_Iteration(matrix,elm,
                                                          previousState,nextState,psiNew)
    
    
    matrix[elm] <- predictState(chanceMatrix)
  }
  
  
  matrix
  
}

getChanceMatrix_After_First_Iteration<-function(matrix,missedIndex,
                                                previousState,nextState,psi){
  
  if ( length(matrix)-nrow(matrix)<missedIndex  &&
       missedIndex <= length(matrix))         # missed index in last column
    
    mul <- psi[previousState,]
  
  else if ( missedIndex <nrow(matrix) )           # missed index in First column
    
    mul <- psi[nextState,]
  
  else                                      # minssed index is NoT in First ,or Last  Column
    mul<-psiNew[previousState,] * psiNew[,nextState]
  
  
  chanceMatrix <-cumsum( mul /  sum(mul) )
  chanceMatrix
  
}





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








isPSIsConverged<-function(psiNew,psiLast){
  
  print("Difference between 2 last Psies?")
  
  print(abs(psiNew-psiLast))
  
  
  all(abs(psiNew-psiLast)<0.02)  # return true if all the difference between all the enteries is less than 0.01 
}


printData<-function(psiNew,psiLast,counter){
  
  print("________________________________________________________________________")
  print(paste0("Counter: ",counter   ))  
  
  
  print(paste0("Is it converged? ",isPSIsConverged(psiNew,psiLast)))
  
  
}

