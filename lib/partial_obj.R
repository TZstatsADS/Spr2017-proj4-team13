#########################################################################################
#### This is the function of calculting the partial derivative of objective function ####
#### Input: tf-idf matrix, corresponding author info list, corresponding tag,        ####
####        parameter matrix, index of column to calculate the partial derivative    ####
#########################################################################################
partial_obj <- function(matrix,authorname,yl,A,m)
{
  par1 <- c()
  for(i in 1:nrow(matrix))
    {
      par2 <- c()
      x1 <- matrix[i,]
      for(j in 1:nrow(matrix))
        {
          x2 <- matrix[j,]
          par2[j] <- partial_D(x1,x2,A,m)*constraint(i,j,authorname)
        }
  
   par1[i] <- sum(par2)+partial_D(x1,yl[i,],A,m)
  }
  
  return(sum(par1))
}