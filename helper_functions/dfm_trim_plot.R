# plot similar to plotRemoved in stm package

dfm_trim_plot <- function(DFM, s1, s2, s3, min_freq = "doc"){
  voc_list <- list()
  doc_list <- list()
  
  if (min_freq == "doc"){
    for(i in seq(s1, s2, s3)){
      trim <- quanteda::dfm_trim(DFM, min_docfreq = i)
      voc_list[[i]] <- dim(trim)[2]
      doc_list[[i]] <- sum(rowSums(trim) == 0)
    }
    
    par(mfrow = c(1, 2))
    plot(unlist(voc_list), type = "l", ylab = "voc size", xlab = "Minimal doc frequency", main = "Change in vocabulary size", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
    abline(h = round(dim(DFM)[2]/2, 0), col = "orange", lty = "dashed")
    text((s2-s1)/2, dim(DFM)[2]/75 + round(dim(DFM)[2]/2, 0), "half of max voc size", col = "orange")
    plot(unlist(doc_list), type = "l", ylab = "docs dropped", xlab = "Minimal doc frequency", main = "Number of docs dropped", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
    
  }
  
  if (min_freq == "term"){
    for(i in seq(s1, s2, s3)){
      trim <- quanteda::dfm_trim(DFM, min_termfreq = i)
      voc_list[[i]] <- dim(trim)[2]
      doc_list[[i]] <- sum(rowSums(trim) == 0)
    }
    
    par(mfrow = c(1, 2))
    plot(unlist(voc_list), type = "l", ylab = "voc size", xlab = "Minimal term frequency", main = "Change in vocabulary size", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
    abline(h = round(dim(DFM)[2]/2, 0), col = "orange", lty = "dashed")
    text((s2-s1)/2, dim(DFM)[2]/75 + round(dim(DFM)[2]/2, 0), "half of max voc size", col = "orange")
    plot(unlist(doc_list), type = "l", ylab = "docs dropped", xlab = "Minimal term frequency", main = "Number of docs dropped", xaxt = "n")
    axis(1, at = 1:length(seq(s1, s2, s3)), labels = seq(s1, s2, s3))
  }
  

}