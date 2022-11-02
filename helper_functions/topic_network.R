library(igraph)
library(visNetwork)
library(ldaPrototype)
library(tosca)


topic_network <- function(model, thresh = 0.05){
  
  lda <- getLDA(model)
  K <- getK(lda)
  
  # theta (document-topic probabilities)
  theta <- t(getEstimators(lda)$theta)
  
  # beta (word-topic probabilities) a.k.a. phi
  beta <- t(getEstimators(lda)$phi)
  
  
  topwords <- tosca::topWords(getTopics(lda), 10)
  topwords <- apply(topwords, 2, paste, collapse = ", ")
  
  prevalence <- colMeans(theta)
  
  # number of docs with theta > .5 per topic
  n_docs <- apply(theta, 2, function(x){unname(table(x > 0.5)[2])})
  
  # use first two top terms as preliminary labels
  label <- sapply(topwords, function(x) {paste(strsplit(x, ", ")[[1]][1:2], collapse = " ")})
  label <- unname(label)
  
  # add ID for disambiguation
  for (i in 1:K){
    label[i] <- paste0("T", i, ": ", label[i])
  }
  
  
  
  
  # topic correlations
  cor_mat <- cor(beta)
  
  # omit small correlations
  cor_mat[cor_mat < thresh] <- 0
  diag(cor_mat) <- 0 # needed for network plot
  
  # Network of the Word Distributions Over Topics (Topic Relation)
  graph <- graph.adjacency(cor_mat, weighted = TRUE, mode = "lower")
  
  # edge labels
  edge_attr(graph, "name") <- round(E(graph)$weight, 2)
  # edge_attr(graph, "color") <- rep("green", gsize(graph)))
  edge_attr(graph, "label") <- E(graph)$name
  
  # line thickness
  E(graph)$edge.width <- E(graph)$weight*20
  
  # labels
  V(graph)$label <- label
  V(graph)$size <- 10
  
  # Detect the communities and add the community as a vertex attribute
  # multi-level modularity optimization algorithm for finding community structure
  cd <- cluster_louvain(graph)
  V(graph)$community <- cd$membership
  
  
  
  ## interactive plot
  
  nodes <- as.data.frame(1:K)
  names(nodes) <- "id"
  nodes$label <- label
  nodes$size <- colMeans(theta)*100 # size by topic prevalence
  
  nodes$group <- V(graph)$community
  
  edges <- get.data.frame(graph)
  names(edges)[3] <- "width"
  edges$label <- as.character(round(edges$width, 2))
  edges$label <- ifelse(nchar(edges$label) == 3, paste0(edges$label, "0"), edges$label)
  edges$width <- edges$width*2 # better visibility in plot
  
  # color edge label according to width
  # https://stackoverflow.com/a/9946970/11752986
  rbPal <- colorRampPalette(c("grey", "cornflowerblue"))
  edges$color <- rbPal(10)[as.numeric(cut(edges$width, breaks = 10))]
  
  set.seed(28281)
  visNetwork(nodes, edges) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visNodes(
      shape = "dot",
      font = list(size = 10, background = "white"),
      color = list(
        background = "#0085AF",
        border = "#013848",
        highlight = "#FF8000"
      ),
      shadow = list(enabled = TRUE, size = 10)
    ) %>%
    visEdges(
      label = edges$label,
      #font = list(color = "color", size = 20),
      font = list(color = "slategray", size = 10),
      smooth = list(enabled = TRUE, type = "diagonalCross"),
      shadow = FALSE,
      # color = list(color = "#0085AF", highlight = "#C62F4B", opacity = .5)
    ) %>%
    # visLegend() %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
  
  
}

