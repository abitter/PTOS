## exclusivity

# using LDAvis relevance score with lambda = 0 for putting emphasis on exclusivity
# Sievert, C., Shirley, K.E.: LDAvis: 
# A method for visualizing and interpreting topics. 
# In: Proceedings of the workshop on interactive language learning, visualization, and interfaces. 
# pp. 63–70 (2014).


# original codes by A. Niekler, G. Wiedemann, and A. Fischer

exclusivity <- function(lda, dfm, lambda = 0, num.words = 0){
  library(ldaPrototype)
  if (num.words == 0) 
    num.words = dim(dfm)[2]
  pwt <- as.matrix(getEstimators(getLDA(lda))$phi)
  pw <- colSums(dfm)/sum(dfm)
  res <- apply(pwt, 1, function(x, num.words, pw, lambda) {
    x <- lambda * log(x) + (1 - lambda) * log(x/pw)
    return((sort(x, decreasing = TRUE)[1:num.words]))
  }, num.words, pw, lambda)
  return(colMeans(res))
}