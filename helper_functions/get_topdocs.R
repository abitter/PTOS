get_topdocs <- function(model, texts, n = 10, text_ID = rownames(texts)){
  
  library(ldaPrototype)
  library(tosca)
  
  # model <- getLDA(model)
  theta <- t(getEstimators(model)$theta)
  K <- getK(lda)
  
  # 10 most representative docs
  theta_tmp <- as.data.frame(theta)
  theta_tmp$ID <- text_ID # for matching theta with texts
  
  topdocs <- list()
  for (i in 1:K){
    # get row indices of top n
    tmp <- theta_tmp[order(-theta_tmp[,i]),]
    ids <- tmp$ID[1:n]
      
    topdocs_tmp <- texts[text_ID == ids[1],]
    for (j in 2:length(ids)){
      topdocs_tmp <- rbind(topdocs_tmp, texts[text_ID == ids[j],])
    }
    
    topdocs_tmp$prob <- tmp[1:n,i] # add theta probabilities
    topdocs[[i]] <- topdocs_tmp
  }
  
  return(topdocs)
}