#########################################################################################
#### This is the function of calculting the partial derivative of objective function ####
#### Input: tf-idf matrix, corresponding author info list, corresponding tag,        ####
####        parameter matrix, index of column to calculate the partial derivative    ####
#########################################################################################
partial_obj <- function(matrix,cluster,rawdata,A,m){
  centroids_unorm <- fcen_unorm(X,cluster,A)
  par1 <- c()
  for(i in 1:(nrow(matrix)-1))
    {
      par2 <- c()
      x1 <- matrix[i,]
      for(j in (i+1):nrow(matrix))
        {
          x2 <- matrix[j,]
          par2[j] <- partial_D(x1,x2,A,m)*Constraint[i,j]
        }
  
   par1[i] <- 2*sum(par2,na.rm=T)+partial_D(x1,centroids_unorm[cluster[i],],A,m)
  }
  return(sum(par1))
}

#m=1
#partial_obj(X,cluster,data,A,m)
