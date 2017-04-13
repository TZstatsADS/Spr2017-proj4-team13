#This function returns our misclasification rate baased on the results returned by different cluster resluts
Misclassification = function(filename,classify)
{
  filename$classify.result = classify
  filename$cluster.result = NA
  initial.class = c()
  Misclassification.rate = 0
  N = dim(filename)[1] # Total number of observations in filename
  
  class = unique(classify)
  
  for(i in class)
  {
    initial.class = filename$AuthorID[which(filename$classify.result == i)]
    initial.class = initial.class[order(initial.class,decreasing = FALSE)]
    filename$cluster.result[which(filename$classify.result==i)] = rep(as.numeric(levels(factor(initial.class))[which.max(tapply(initial.class,factor(initial.class),length))]),length(initial.class))
  }
  
  for(i in 1 : N)
  {
    if(filename$AuthorID[i] != filename$cluster.result[i])
    {
      Misclassification.rate = Misclassification.rate + 1
    }
  }
  
  return(Misclassification.rate/N)
}
