setwd("C:/Users/great/Desktop/R-dataSci/Salman/PaitentTracking")
library(MCMCpack)
source("functions.R")
source("basic_dataset.R")
source("first_Iteration_function.R")
source("second_Iteration_functions.R")


## Generate a random matrix(n,m)

psiLast<-NULL
psiNew <-getFirstPsi()


#mat<-matrix(sample(1:3,15,replace = T),5,3)

mat<- getMainDataSet()
#Omit 20% of matrix enteries

# indexSet <- selectRandomIndices(mat)
indexSet <- getMissingIndex(mat)
mat      <- omitEnteries(mat,indexSet)
mat



#For any given state (1,2,or 3) there might be 3 different range of probabilities. # Test 2  Tested and worked well


chanceMatrix<-obtainChanceMatrix(psiNew)
chanceMatrix


## First iteration

### Predict proper state for missed data
# 1. Generate a random number
# 2. Use the ***chance matrix*** , to predict the proper state for the missed entery. 
# 3. Fill the NAs with proper state




mat<-fillNA_First_Iteration(mat,chanceMatrix,indexSet )  # Test 3
mat


## Next Iterations

# 1. Update a the Psi
# 2. Compare the new psi with the previous iteration psi
# 3. As long as the 2 last psi matrices are not converged do the following
# 
#   3.1. Fill the missed enteries
# 
#   3.2. Update the psi
# 
#   3.3. Back to 3

psiLast <-psiNew                      # Store the previous Psi matrix
psiNew<-createTransitionMatrix(mat)   # Get a new Psi
counter <- 2
#printData(psiNew,psiLast,counter)



 while(!isPSIsConverged(psiNew,psiLast)){     #if it is not cpnverged then continue until convergance
#for(i in 1:3){
  print(paste0(" Counter: ",counter)  )
  mat <- fillNA_After_First_Iteration(mat,psiNew, indexSet )
  
  print("/__________________________________________________________________/")
  
  
  psiLast <- psiNew
  psiNew  <- createTransitionMatrix(mat)
  
  
  counter<- counter+1
  printData(psiNew,psiLast,counter)
  
}

  