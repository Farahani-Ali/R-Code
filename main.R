setwd("C:/Users/salma/OneDrive/Desktop/PaitentTracking")
library(MCMCpack)
source("functions.R")
source("basic_dataset.R")
source("first_Iteration_function.R")
source("after_First_Iteration_functions.R")


psiLast<-0
psiNew <-0
counter <- 2


mat<- getMainDataSet()

#Omit 20% of matrix enteries

indexSet <- getMissingIndex(mat)
mat      <- omitEnteries(mat,indexSet)
mat


psiNew<- createTransitionMatrix(mat)
chanceMatrix<-obtainChanceMatrix(psiNew)
chanceMatrix

##########################################################
############      First iteration    ##################### 
##########################################################

# 1. Generate a random number
# 2. Use the chance matrix , to predict the proper state for the missed entery. 
# 3. Fill the NAs with proper state

mat<-fillNA_First_Iteration(mat,chanceMatrix,indexSet )  
mat

##########################################################
##############     Next Iterations   #####################
##########################################################

# 1. Update a the Psi
# 2. Compare the new psi with the previous iteration psi
# 3. As long as the 2 last psi matrices are not converged do the following
# 
#   3.1. Fill the missed enteries
# 
#   3.2. Update the psi
# 
#   3.3. Back to 3



 while(!isPSIsConverged(psiNew,psiLast,0.01)){     #if it is not cpnverged then continue until convergance
   
   psiLast <-psiNew                      # Store the previous Psi matrix
   psiNew<-createTransitionMatrix(mat)   # Get a new Psi
    
  
  mat <- fillNA_After_First_Iteration(mat,psiNew, indexSet )
  
  
  counter<- counter+1
  printData(counter)

}

  