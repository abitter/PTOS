# Practices and Tools of Open Science
# https://leibniz-psychology.org/ptos/topic-modeling/
# André Bittermann, abi@leibniz-psychology.org



# Libraries ---------------------------------------------------------------

# web scraping
library(httr)
library(XML)

# text processing
library(textclean)
library(quanteda)

# topic modeling
library(ldaPrototype)
library(tosca)

# networks
library(igraph)
library(visNetwork)




# Get data ----------------------------------------------------------------


## retrieve from GithHub ----
# from https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-29
taylor_swift_lyrics <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv")
beyonce_lyrics <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv")

# inspect
names(taylor_swift_lyrics)
names(beyonce_lyrics)
head(taylor_swift_lyrics$Lyrics, 2)
head(beyonce_lyrics$line, 2)

# aggregate beyonce lyrics (one song per line)
beyonce_aggregate <- aggregate(beyonce_lyrics$line, 
                               by = list(id = beyonce_lyrics$song_id, 
                                         Title = beyonce_lyrics$song_name),
                               paste, collapse = "\n")
colnames(beyonce_aggregate)[3] <- "Lyrics"



## get year of song release ----

# retrieve data from Wikipedia
# https://stackoverflow.com/a/41984751/11752986


### Beyonce ----

url <- "https://en.wikipedia.org/wiki/List_of_songs_recorded_by_Beyonc%C3%A9"
doc <- readHTMLTable(doc = content(GET(url), "text"))
beyonce_songs <- doc[3][["NULL"]] # the table of interest

beyonce_songs <- beyonce_songs[-1,c(1,5)] # remove header and keep only song and year
names(beyonce_songs) <- c("Title", "Year")

# remove notes
# https://stackoverflow.com/a/24173271/11752986
beyonce_songs$Title <- gsub("\\s*\\[[^\\)]+\\]", "", beyonce_songs$Title)

# year as numeric
beyonce_songs$Year <- as.numeric(beyonce_songs$Year)

# remove cases with NA in Year
beyonce_songs <- beyonce_songs[!is.na(beyonce_songs$Year),]


# remove features in titles of aggregated dataset
remove_ft <- function(x){unlist(strsplit(x, " \\(Ft"))[1]} 
beyonce_aggregate$Title <- sapply(beyonce_aggregate$Title, remove_ft, USE.NAMES = FALSE)


# strip and lower case for merging
beyonce_songs$Title_low <- strip(beyonce_songs$Title, digit.remove = FALSE, apostrophe.remove = TRUE)
beyonce_songs$Title_low <- replace_non_ascii(beyonce_songs$Title_low)
beyonce_aggregate$Title_low <- strip(beyonce_aggregate$Title, digit.remove = FALSE, apostrophe.remove = TRUE)
beyonce_aggregate$Title_low <- replace_non_ascii(beyonce_aggregate$Title_low)

# merge publication year
beyonce_final <- merge(beyonce_aggregate[,3:4], beyonce_songs[,2:3], by = "Title_low")

# drop NAs
beyonce_final <- beyonce_final[!is.na(beyonce_final$Year),]
beyonce_final$Artist <- "Beyonce"


### Taylor Swift ----

url <- "https://en.wikipedia.org/wiki/List_of_songs_by_Taylor_Swift"
doc <- readHTMLTable(doc = content(GET(url), "text"))
taylor_songs <- doc$`Name of song, featured performers, writers, originating album, and year released.`
taylor_songs <- taylor_songs[-1,c(1,5)] # remove header and keep only song and year
names(taylor_songs) <- c("Title", "Year")

# remove year of rerelease
taylor_songs$Year <- substr(taylor_songs$Year, 1, 4)
taylor_songs$Year <- as.numeric(taylor_songs$Year)

# strip and lower case for merging
taylor_songs$Title_low <- strip(taylor_songs$Title, digit.remove = FALSE, apostrophe.remove = TRUE)
taylor_songs$Title_low <- replace_non_ascii(taylor_songs$Title_low)
taylor_swift_lyrics$Title_low <- strip(taylor_swift_lyrics$Title, digit.remove = FALSE, apostrophe.remove = TRUE)
taylor_swift_lyrics$Title_low <- replace_non_ascii(taylor_swift_lyrics$Title_low)

# merge publication year
taylor_final <- merge(taylor_swift_lyrics[,4:5], taylor_songs[,2:3], by = "Title_low")

# drop NAs
taylor_final <- taylor_final[!is.na(taylor_final$Year),]
taylor_final$Artist <- "Taylor"



## join datasets ----
texts <- rbind(beyonce_final, taylor_final)

# sort by date
# this is relevant for time series in ldaPrototype
texts <- texts[order(texts$Year),]
rownames(texts) <- NULL

# artist indices
ind_beyonce <- which(texts$Artist == "Beyonce")
ind_taylor <- which(texts$Artist == "Taylor")

# basic descriptives
table(texts$Year)
table(texts$Artist)
table(texts$Year[ind_beyonce])
table(texts$Year[ind_taylor])


# Text preprocessing ------------------------------------------------------


## a glimpse in the most frequent terms ----

topfeatures(dfm(tokens(texts$Lyrics, remove_punct = TRUE)), 20)
topfeatures(dfm(tokens(texts$Lyrics[ind_beyonce], remove_punct = TRUE)), 20)
topfeatures(dfm(tokens(texts$Lyrics[ind_taylor], remove_punct = TRUE)), 20)

## create document-feature matrix (DFM) and derivates ----


# Part-of-speech tagging: Include only nouns
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

source("./helper_functions/pos_tag.R")
corpus_nouns <- pos_tag(texts$Lyrics, pos_filter = c("NNP", "NNPS", "NN", "NNS"))
saveRDS(corpus_nouns, file = ".RDS/corpus_nouns.RDS")


# Tokenize
tokens <- tokens(corpus_nouns)

# to lower case
tokens <- tokens_tolower(tokens)

# remove stopwords
tokens <- tokens_remove(tokens, pattern = c(stopwords("en"), 
                                            "yo", "oh", "ooh", "www", "hey", "whoa", "ah", "uh", "na"))

# remove punctuation etc. 
tokens <- tokens(tokens, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)


# Lemmatization
# Get lemma table
lemma <- read.delim("https://github.com/michmech/lemmatization-lists/raw/master/lemmatization-en.txt", sep = "\t")
colnames(lemma) <- c("base", "variant")
lemma$base <- tolower(lemma$base)
lemma$variant <- tolower(lemma$variant)

# don't change "data" to "datum"
lemma <- lemma[-which(lemma$variant == "data"),]

# don't change "sentencing" to "sentence"
lemma <- lemma[-which(lemma$variant == "sentencing"),]

# remove duplicates
lemma <- lemma[!duplicated(lemma$variant),]


# lemmatize tokens
tokens_lemmatized <- tokens_replace(tokens, lemma$variant, lemma$base)


# DFM
DFM <- dfm(tokens_lemmatized)

# most frequent terms
topfeatures(DFM, 50)

# most frequent terms for Beyonce
topfeatures(DFM[ind_beyonce,])

# most frequent terms for Taylor Swift
topfeatures(DFM[ind_taylor,])


# see how min_docfreq / min_termfreq affects vocabulary size and number of dropped documents
# cf. Maier et al. (2020), https://doi.org/10.5117/CCR2020.2.001.MAIE
source("./helper_functions/dfm_trim_plot.R")
dfm_trim_plot(DFM, 1, 40, 1, min_freq = "doc")
dfm_trim_plot(DFM, 1, 40, 1, min_freq = "term")

# trim DFM
DFM <- dfm_trim(DFM, min_termfreq = 3)

# save DFM
saveRDS(DFM, file = "./RDS/DFM.RDS")

# DTM (document-term-matrix as in package topicmodels) for coherence function
DTM <- convert(DFM, to = "topicmodels")
saveRDS(DTM, file = "./RDS/DTM.RDS")


## Prepare for ldaPrototype ----

temp <- convert(DFM, to = "lda")
docs <- temp$documents
vocab <- temp$vocab
rm(temp)

# set all freqs to 1, as expected by LDArep of ldaPrototype
# https://github.com/JonasRieger/ldaPrototype/issues/10
docs <- lapply(docs, function(x) rbind(rep(x[1,], x[2,]), 1L))

saveRDS(docs, file = "./RDS/docs.RDS")
saveRDS(vocab, file = "./RDS/vocab.RDS")





# Model selection ---------------------------------------------------------


## 1. candidate models ----

# number of topics to inspect
K_range <- seq(4, 25, 1)

# Dirichlet hyperparameters
# default: alpha = eta = 1/K
# set alpha low if you assume only few topics per doc (probability for other topics almost 0)
# set eta low for well-separated topics

# number of prototypes = number of LDA models with different seeds
# Rieger et al. (2022), Fig. 6: Increase of reliability in dependence of the number of replications
n_proto <- 50

candidate_models <- list()
start <- Sys.time()
for (i in 1:length(K_range)){
  
  runif(1) # on linux machines; https://github.com/JonasRieger/ldaPrototype/issues/11
  
  candidate_models[[i]] <- LDAPrototype(docs = docs, vocabLDA = vocab, 
                                       K = K_range[i],
                                       n = n_proto, 
                                       seeds = 1:n_proto, # setting seeds makes the models reproducible!
                                       pm.backend = "multicore", # works only on linux machines
                                       ncpus = 8)
}
end <- Sys.time()
end - start

saveRDS(candidate_models, file = "./RDS/candidate_models.RDS")



## 2. metrics ----

# Inspect topic coherence and exclusivity of candidate models
# (calculated based on top 10 terms)

source("./helper_functions/coherence_LDAproto.R")
source("./helper_functions/exclusivity_LDAproto.R")

# Mimno, D., Wallach, H. M., Talley, E., Leenders, M., & McCallum, A. (2011, July). 
# "Optimizing semantic coherence in topic models." In Proceedings of the Conference on Empirical Methods in 
# Natural Language Processing (pp. 262-272). Association for Computational Linguistics. Chicago
coh <- lapply(candidate_models, topicCoherence, DTM, N = 10) # this is where the DTM is needed
coh_mean <- unlist(lapply(coh, mean))

# using LDAvis relevance score with lambda = 0 for putting emphasis on exclusivity
# Sievert, C., Shirley, K.E.: LDAvis: 
# A method for visualizing and interpreting topics. 
# In: Proceedings of the workshop on interactive language learning, visualization, and interfaces. 
# pp. 63–70 (2014).
exc <- lapply(candidate_models, exclusivity, DFM, num.words = 10)
exc_mean <- unlist(lapply(exc, mean))


# plot of scaled scores, including mean of semantic coherence & exclusivity
semcoh <- scale(coh_mean)
exclus <- scale(exc_mean)
semexc <- rowMeans(cbind(semcoh, exclus)) # mean of scaled sem & exc

#dev.off()
plot(semcoh, type = "l", col = "blue", xaxt = "n", lwd = 2, ylim = c(-3, 3), 
     main = "Topic Quality", ylab = "Scaled Score", xlab = "Number of Topics")
lines(exclus, type = "l", col = "black", lwd = 2)
lines(semexc, type = "l", col = "orange", lwd = 3)
abline(h = max(semexc), v = which.max(semexc), col = "gray", lwd = 2, lty = "dashed")
axis(1, at = 1:length(K_range), labels = K_range)
legend("bottomright", c("Semantic Coherence", "Exclusivity (LDAvis lambda = 0)", 
                        "Mean Coherence & Exclusivity"), 
       col = c("blue", "black", "orange"), lty = "solid", lwd = 2)



## 3. semantic granularity ----

# we select two models of different granularity for further inspection
# beyond this demonstration: always inspect several candidates!

select <- K_range %in% c(5,19) # get indices of desired k
candidates_inspect <- candidate_models[select]

for (i in 1:length(candidates_inspect)){
  print(paste("### Topics for k =", getK(getLDA((candidates_inspect[[i]]))), "###"))
  print(topWords(getTopics(getLDA(candidates_inspect[[i]])), 10))
}


## 4. quality of single topics ----

par(mfrow=c(1,2))
for (i in 1:length(candidates_inspect)){
  plot(coh[select][[i]], exc[select][[i]], main = paste("K =", length(coh[select][[i]]),"Topic Quality"),
       ylab = "Exclusivity", xlab = "Coherence", col = "white")
  text(coh[select][[i]], exc[select][[i]], labels = paste("", 1:length(coh[select][[i]])), cex = 1.5)
}


## 5. topic similarity ----

source("./helper_functions/topic_network.R")
topic_network(candidate_models[[which(K_range == 5)]])
topic_network(candidate_models[[which(K_range == 19)]])





## 6. select model ----

final_model <- candidate_models[[which(K_range == 19)]]
lda <- getLDA(final_model)
K <- getK(lda)




# Topic Validity ----------------------------------------------------------


## get probability matrices ----

# theta (document-topic probabilities)
theta <- t(getEstimators(lda)$theta)
colnames(theta) <- paste("Topic", 1:K)

# beta (word-topic probabilities) a.k.a. phi
beta <- t(getEstimators(lda)$phi)
colnames(beta) <- paste("Topic", 1:K)


saveRDS(lda, file = "./RDS/lda.RDS")
saveRDS(theta, file = "./RDS/theta.RDS")
saveRDS(beta, file = "./RDS/beta.RDS")


## top terms ----
topterms <- tosca::topWords(getTopics(lda), 10)
topterms <- apply(topterms, 2, paste, collapse = ", ")


## inspect texts ----

# with highest probability of addressing the topic (i.e., high theta)

source("./helper_functions/get_topdocs.R")
topdocs <- get_topdocs(lda, texts,
                       n = 10, text_ID = rownames(texts))

# you may need to set different text_ID
# Especially if you dropped docs during preprocessing! (cf. dfm_trim)


# show most representative texts
t <- 19 # select topic
topterms[t]
topdocs[[t]][,-2] # -2 to leave out the column with the lyrics

topdocs[[t]][1:10,2] # top ten lyrics



## assign labels ----

# mark uninterpretable topics as "REMOVE" for subsequent removal

labels <- c("Party & Dancing",
           "December & Signs",
           "Woman & Girl Power",
           "REMOVE",
           "Love",
           "REMOVE",
           "REMOVE",
           "Day & Night",
           "Dreams",
           "REMOVE",
           "Spanish & Power",
           "Self-Confidence",
           "REMOVE",
           "REMOVE",
           "Interpersonal Attraction",
           "Home & Men",
           "REMOVE",
           "REMOVE",
           "REMOVE")



## topic table ----

# topic prevalence
prevalence <- colMeans(theta)

# number of docs with theta > .5 per topic
n_docs <- apply(theta, 2, function(x){unname(table(x > 0.5)[2])})

# data frame
topic_table <- data.frame("ID" = 1:K, labels, topterms, prevalence, n_docs)
topic_table_full <- topic_table



## remove uninterpretable topics ----
remove <- which(labels == "REMOVE")

topic_table <- topic_table[-remove,]
# assign new IDs
rownames(topic_table) <- NULL
topic_table$ID <- rownames(topic_table)

theta <- theta[,-remove]
colnames(theta) <- topic_table$labels

beta <- beta[,-remove]
colnames(beta) <- topic_table$labels

saveRDS(topic_table, file = "./RDS/topic_table.RDS")

K <- nrow(topic_table)



# Topic analyses ----------------------------------------------------------


## Most prevalent topics ----

### overall ----
prevalence_sorted <- topic_table$prevalence
names(prevalence_sorted) <- topic_table$labels
prevalence_sorted <- sort(prevalence_sorted)

par(mar=c(2, 4.25, 1, 1)*2.5)
barplot(prevalence_sorted, horiz = TRUE, las = 1, xlim = c(0, 0.12),
        main = "Topic prevalence")


### by artist ----
prevalence_beyonce <- colMeans(theta[ind_beyonce,])
prevalence_taylor <- colMeans(theta[ind_taylor,])

# join and sort by overall prevalence
prevalence_joined <- as.data.frame(cbind(prevalence_beyonce, prevalence_taylor, topic_table$prevalence))
colnames(prevalence_joined)[3] <- "overall"
prevalence_joined <- prevalence_joined[order(prevalence_joined$overall),]
prevalence_joined <- as.matrix(t(prevalence_joined[,1:2]))

# plot
barplot(prevalence_joined, beside = TRUE, horiz = TRUE, las = 1,
        col = c("orange", "cornflowerblue"),
        legend.text = c("Beyoncé", "Taylor Swift"),
        args.legend = list(x = "bottomright"),
        main = "Topic Prevalence by Artist")




## prevalence by year ----


# original code by https://github.com/mponweiser/thesis-LDA-in-R/blob/master/application-pnas/trends.Rnw

# overall
theta_mean_by_year_by <- by(theta, texts$Year, colMeans)
theta_mean_by_year <- do.call("rbind", theta_mean_by_year_by)
years <- levels(factor(texts$Year))

# beyonce
theta_mean_by_year_by_beyonce <- by(theta[ind_beyonce,], texts$Year[ind_beyonce], colMeans)
theta_mean_by_year_beyonce <- do.call("rbind", theta_mean_by_year_by_beyonce)
years_beyonce <- levels(factor(texts$Year[ind_beyonce]))
ts_beyonce <- ts(theta_mean_by_year_beyonce, start = as.integer(years_beyonce[1]))

# taylor
theta_mean_by_year_by_taylor <- by(theta[ind_taylor,], texts$Year[ind_taylor], colMeans)
theta_mean_by_year_taylor <- do.call("rbind", theta_mean_by_year_by_taylor)
years_taylor <- levels(factor(texts$Year[ind_taylor]))
ts_taylor <- ts(theta_mean_by_year_taylor, start = as.integer(years_taylor[1]))


# plots
par(mfrow=c(4,3))
for (i in 1:K){
  plot(ts_beyonce[,i], col = "orange", ylim = c(0, 0.5),
       ylab = "", xlab = "", main = paste(i, topic_table$labels[i]), cex.main = 1, lwd = 2)
  lines(ts_taylor[,i], col = "cornflowerblue", lwd = 2)
  
}