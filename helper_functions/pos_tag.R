# Part-of-Speech tagging

# modified from: # https://tm4ss.github.io/docs/Tutorial_8_NER_POS.html

# Wiedemann, Gregor; Niekler, Andreas (2017).
# Hands-on: a five day text mining course for humanists and social scientists in R
# http://ceur-ws.org/Vol-1918/wiedemann.pdf.
# Proceedings of the 1st Workshop Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.


# "The openNLP package relies on the rjava package. For this to work properly, you need a version of Java installed
# (e.g. open-jdk) which matches your R-version w.r.t either the 32- or 64-bit installation. 
# Also the JAVA_HOME environment variable needs to be set, pointing to your Java installation directory."



# this function expects x as a vector of texts

pos_tag <- function(x, pos_filter = c("NNP", "NNPS", "NN", "NNS")){ # nouns are default
  
  options(stringsAsFactors = FALSE)
  library(quanteda)
  library(NLP)
  

  # Create corpus object
  text_corpus <- corpus(x)
  
  require(openNLP)
  require(openNLPdata)
  
  # openNLP annotator objects
  sent_token_annotator <- openNLP::Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  annotator_pipeline <- Annotator_Pipeline(
    sent_token_annotator,
    word_token_annotator,
    pos_tag_annotator
  )
  
  # function for annotation
  annotateDocuments <- function(doc, pos_filter = NULL) {
    doc <- as.String(doc)
    doc_with_annotations <- NLP::annotate(doc, annotator_pipeline)
    tags <- sapply(subset(doc_with_annotations, type=="word")$features, `[[`, "POS")
    tokens <- doc[subset(doc_with_annotations, type=="word")]
    if (!is.null(pos_filter)) {
      res <- tokens[tags %in% pos_filter]
    } else {
      res <- paste0(tokens, "_", tags)
    }
    res <- paste(res, collapse = " ")
    return(res)
  }
  

  # return filtered corpus
  return(sapply(texts(text_corpus), annotateDocuments, pos_filter = pos_filter))
  
}

