---
title: 'A Topic Modeling Workflow'
subtitle: "Practices and Tools of Open Science"
author: "André Bittermann (abi@leibniz-psychology.org)"
date: "November 2, 2022"
output: 
  rmarkdown::github_document:
    toc: yes
    toc_depth: 2
toc-title: "Contents"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)
```

# Info

This is the markdown for the example analysis presented in the [PTOS Topic Modeling](https://leibniz-psychology.org/en/opensciencelectures/topic-modeling/) lecture. Here, a topic modeling workflow is presented using song lyrics as an example.

# Libraries

```{r libraries}
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
```

# Get data

## Retrieve from GitHub

We use the song lyrics datasets from [tidytuesday](https://github.com/rfordatascience/tidytuesday).

```{r}
# get data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-29
taylor_swift_lyrics <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv")
beyonce_lyrics <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv")

# inspect
names(taylor_swift_lyrics)
names(beyonce_lyrics)
head(taylor_swift_lyrics$Lyrics, 1)
head(beyonce_lyrics$line, 2)
```

The Beyoncé file has one song per line. We aggregate the lines by song.

```{r}
beyonce_aggregate <- aggregate(beyonce_lyrics$line, 
                               by = list(id = beyonce_lyrics$song_id, 
                                         Title = beyonce_lyrics$song_name),
                               paste, collapse = "\n")
colnames(beyonce_aggregate)[3] <- "Lyrics"
```

## Get year of song release

We scrape Wikipedia for some additional metadata.

```{r}
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

```

Let's inspect some basic information:

```{r}
table(texts$Year)
table(texts$Artist)
table(texts$Year[ind_beyonce])
table(texts$Year[ind_taylor])
```

# Text Preprocessing

Our workflow consists of four steps (cf. [Maier et al., 2020](https://doi.org/10.1080/19312458.2018.1430754): 

1. Preprocessing 
2. Model Selection 
3. Model Validity & Reliability 
4. Topic Analyses


In the preprocessing phase, clean the texts in order to keep only relevant information.
But what to do depends on your corpus and research goals!

## Inspect

First, we quickly inspect the most common terms in the corpus:

```{r}
topfeatures(dfm(tokens(texts$Lyrics, remove_punct = TRUE)), 20)
topfeatures(dfm(tokens(texts$Lyrics[ind_beyonce], remove_punct = TRUE)), 20)
topfeatures(dfm(tokens(texts$Lyrics[ind_taylor], remove_punct = TRUE)), 20)
```

## Create document-feature matrix (DFM)

In order to transform the text into a numerical representation, we perform several steps of preprocessing and text cleaning.

First, we use part-of-speech tagging to keep only nouns.

```{r}
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html

source("./helper_functions/pos_tag.R")
corpus_nouns <- pos_tag(texts$Lyrics, pos_filter = c("NNP", "NNPS", "NN", "NNS"))
```

Next, we tokenize, transform to lower case, and remove stopwords, punctuation etc.

```{r}
# Tokenize
tokens <- tokens(corpus_nouns)

# to lower case
tokens <- tokens_tolower(tokens)

# remove stopwords
tokens <- tokens_remove(tokens, pattern = c(stopwords("en"), 
                                            "yo", "oh", "ooh", "www", "hey", "whoa", "ah", "uh", "na"))

# remove punctuation etc. 
tokens <- tokens(tokens, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)
```

We use lemmatization to reduce the size of the vocabulary (= all different words in the corpus)

We can retrieve a lemma dictionary from GitHub:

```{r}
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

```

Then, we lemmatize by replacing word variants with their base form:

```{r}
# lemmatize tokens
tokens_lemmatized <- tokens_replace(tokens, lemma$variant, lemma$base)
```

Now we can create the DFM:

```{r}
# DFM
DFM <- dfm(tokens_lemmatized)

# most frequent terms
topfeatures(DFM, 50)

# most frequent terms for Beyonce
topfeatures(DFM[ind_beyonce,])

# most frequent terms for Taylor Swift
topfeatures(DFM[ind_taylor,])
```

Infrequent terms increase the computation time, although they are not relevant for the topics (cf. [Maier et al., 2020](https://doi.org/10.5117/CCR2020.2.001.MAIE). Hence, we remove infrequent terms:

```{r}
# see how min_docfreq / min_termfreq affects vocabulary size and number of dropped documents
source("./helper_functions/dfm_trim_plot.R")
dfm_trim_plot(DFM, 1, 40, 1, min_freq = "doc")
dfm_trim_plot(DFM, 1, 40, 1, min_freq = "term")
```

We remove terms that appear less than three times.

```{r}
# trim DFM
DFM <- dfm_trim(DFM, min_termfreq = 3)

# DTM (document-term-matrix as in package topicmodels) for coherence function
DTM <- convert(DFM, to = "topicmodels")
```

For the ldaProtoype package, we transform the DFM in two objects `docs` and `vocab`:

```{r}
temp <- convert(DFM, to = "lda")
docs <- temp$documents
vocab <- temp$vocab
rm(temp)

# set all freqs to 1, as expected by LDArep of ldaPrototype
# https://github.com/JonasRieger/ldaPrototype/issues/10
docs <- lapply(docs, function(x) rbind(rep(x[1,], x[2,]), 1L))
```

# Model Selection

This is most likely an iterative process, i.e., you may compute additional models after inspecting the first candidates.

## 1. Candidate models
For determining the number of topics (`K`) in the model, we compute a variety of candidate models.
[Banks et al. (2018)](https://doi.org/10.1007/s10869-017-9528-3) recommend considering between 1 and 100 topics, if you have no clue on how many topics to expect.
We begin with inspecting models comprising 5 to 20 topics (for the sake of brevity).

W.r.t. topic reliability, 50 models are computed for each value of `K`. This, of course, takes some time! On a 8x 1.6 GHz linux machine, computation took ~ 30 minutes (and this is a rather small text corpus!)

Here, we vary only `K`, but one could consider testing several values for LDA hyperparameter alpha as well (cf. [Maier et al., 2018](https://doi.org/10.1080/19312458.2018.1430754)).


```{r}
# number of topics to inspect
K_range <- seq(4, 25, 1)
```


```{r eval = FALSE, echo = TRUE}

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
                                       # setting seeds makes the models reproducible!
                                       seeds = 1:n_proto, 
                                       pm.backend = "multicore", # works only on linux machines
                                       ncpus = 8)
}
end <- Sys.time()
end - start
```

You can find the precomputed object in the `./RDS` folder:

```{r}
candidate_models <- readRDS("./RDS/candidate_models.RDS")
```

## 2. Metrics

Next, we inspect topic quality metrics.

We use two measures of topic quality:

**Semantic Coherence** ([Mimno et al., 2011](https://dl.acm.org/doi/abs/10.5555/2145432.2145462))
= Terms of the topic co-occur in the texts

**Exclusivity** ([Bischof & Airoldi, 2012](https://dl.acm.org/doi/abs/10.5555/3042573.3042578), [Sievert & Shirley, 2014](http://dx.doi.org/10.3115/v1/W14-3110))
= Terms of the topic have a low probability in other topics



```{r}
# Inspect topic coherence and exclusivity of candidate models
# (calculated based on top 10 terms)

source("./helper_functions/coherence_LDAproto.R")
source("./helper_functions/exclusivity_LDAproto.R")

coh <- lapply(candidate_models, topicCoherence, DTM, N = 10) # this is where the DTM is needed
coh_mean <- unlist(lapply(coh, mean))

exc <- lapply(candidate_models, exclusivity, DFM, num.words = 10)
exc_mean <- unlist(lapply(exc, mean))


# scaled scores, including mean of semantic coherence & exclusivity
semcoh <- scale(coh_mean)
exclus <- scale(exc_mean)
semexc <- rowMeans(cbind(semcoh, exclus)) # mean of scaled sem & exc
```

```{r}
plot(semcoh, type = "l", col = "blue", xaxt = "n", lwd = 2, ylim = c(-3, 3), 
     main = "Topic Quality", ylab = "Scaled Score", xlab = "Number of Topics")
lines(exclus, type = "l", col = "black", lwd = 2)
lines(semexc, type = "l", col = "orange", lwd = 3)
abline(h = max(semexc), v = which.max(semexc), col = "gray", lwd = 2, lty = "dashed")
axis(1, at = 1:length(K_range), labels = K_range)
legend("bottomright", c("Semantic Coherence", "Exclusivity (LDAvis lambda = 0)", 
                        "Mean Coherence & Exclusivity"), 
       col = c("blue", "black", "orange"), lty = "solid", lwd = 2)
```

`K = 19` has the overall best statistical fit (w.r.t. to both coherence and exclusivity).
`K = 5` might be worth a look, too.

**In your analysis, you should inspect more candidates!**

## 3. Semantic granularity

We select two models of different granularity for further inspection. Beyond this demonstration: always inspect several candidates!

```{r}
select <- K_range %in% c(5,19) # get indices of desired k
candidates_inspect <- candidate_models[select]

for (i in 1:length(candidates_inspect)){
  print(paste("### Topics for k =", getK(getLDA((candidates_inspect[[i]]))), "###"))
  print(topWords(getTopics(getLDA(candidates_inspect[[i]])), 10))
}
```

As we can see in `Topic 4` of `K = 5` and `Topic 11` of `K = 19`, our corpus is multilingual. The topics of `K = 5` seem to be too broad and mixed.

## 4. Quality of single topics

```{r}
par(mfrow=c(1,2))
for (i in 1:length(candidates_inspect)){
  plot(coh[select][[i]], exc[select][[i]], main = paste("K =", length(coh[select][[i]]),"Topic Quality"),
       ylab = "Exclusivity", xlab = "Coherence", col = "white")
  text(coh[select][[i]], exc[select][[i]], labels = paste("", 1:length(coh[select][[i]])), cex = 1)
}
```

The higher the scores, the higher the quality. `K = 19` adds topics of higher coherence and exclusivity. Not surprising: the Spanish topic is coherent and has exclusive terms.

## 5. Topic similarity

In each topic, every term has a specific word-topic probability (beta a.k.a. phi)*.
We can correlate the topics’ beta vectors and represent it as a semantic network.

* *Please note*: Ordering the topics’ words by probability yields a slightly different order as the top terms presented before. Reason: The top terms are lifted using importance score in `tosca` package, which ranks topic-specific terms higher.


```{r}
source("./helper_functions/topic_network.R")
topic_network(candidate_models[[which(K_range == 5)]])
topic_network(candidate_models[[which(K_range == 19)]])
```

Colors indicate groups of similar topics (via community detection). The network of `K = 5` indicates topics that are too similar, whereas the topics of `K = 19` seem to be well-separated.

## 6. Final model

In this workflow demonstration, we’ll keep the `K = 19` model.

However, we inspected only two candidates. This, of course, is not sufficient.
Always inspect several candidates and all of their topics!
Finding K is an iterative process! (*Maybe even computing new models with K > 20?*)

Which K is best depends on your research goals (e.g., overarching themes or specific topics).
For topic interpretation, a general knowledge of the corpus (and the people who produced the texts) is paramount!


```{r}
final_model <- candidate_models[[which(K_range == 19)]]
lda <- getLDA(final_model)
K <- getK(lda)
```

# Topic Validity

Model reliability is no concern, since we used ldaPrototype.
(the same applies to the default structural topic model)


Now we validate the model w.r.t. intra-topic semantic validity ([Maier et al., 2018](https://doi.org/10.1080/19312458.2018.1430754)).

Goal: Keeping only topics that refer to a coherent semantic concept.

Ideally, two analysis independently
+ inspect top terms
+ inspect most representative texts (minimum document-topic probability of 50 %)
+ find a suitable label
+ resolve conflicts


## Get probability matrices

```{r}
# theta (document-topic probabilities)
theta <- t(getEstimators(lda)$theta)
colnames(theta) <- paste("Topic", 1:K)
theta[1:5,1:5]

# beta (word-topic probabilities) a.k.a. phi
beta <- t(getEstimators(lda)$phi)
colnames(beta) <- paste("Topic", 1:K)
beta[1:5,1:5]
```

## Top terms

```{r}
topterms <- tosca::topWords(getTopics(lda), 10)
topterms <- apply(topterms, 2, paste, collapse = ", ")
head(topterms)
```

## Inspect texts

```{r}
# with highest probability of addressing the topic (i.e., high theta)
source("./helper_functions/get_topdocs.R")
topdocs <- get_topdocs(lda, texts,
                       n = 10, text_ID = rownames(texts))

# you may need to set different text_ID
# Especially if you dropped docs during preprocessing! (cf. dfm_trim)
```

Two examples (you should always inspect all topics!):

```{r}
# show most representative texts
t <- 2 # select topic
topterms[t]
topdocs[[t]][,-2] # -2 to leave out the column with the lyrics

topdocs[[t]][1:2,2] # top two lyrics

```

This topic mainly represents the songs "Signs" by Beyoncé and "Out of the woods" by Taylor Swift. Both share the term "december" → Label "December & Signs"

```{r}
# show most representative texts
t <- 5 # select topic
topterms[t]
topdocs[[t]][,-2] # -2 to leave out the column with the lyrics

topdocs[[t]][1:2,2] # top two lyrics

```

"Love" is a common term in the songs (as expected) → Label “Love”

Model validation takes some time. Take your time.

And it might even take you back to model selection.

Besides finding `K`, this is the most crucial “human-in-the-loop” phase in topic modeling.


## Assign labels

```{r}
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
```

## Topic table

```{r}
# topic prevalence
prevalence <- colMeans(theta)

# number of docs with theta > .5 per topic
n_docs <- apply(theta, 2, function(x){unname(table(x > 0.5)[2])})

# data frame
topic_table <- data.frame("ID" = 1:K, labels, topterms, prevalence, n_docs)
topic_table_full <- topic_table
topic_table_full[,1:3]
```

9 topics were found to be uninterpretable.They can be removed for subsequent analysis.

They may also reflect the need for setting a different `K`.

A way of *external validation* might be reading song summaries on Wikipedia or [genius.com](www.genius.com) and comparing them with the assigned topics.


## Remove uninterpretable topics

```{r}
remove <- which(labels == "REMOVE")

topic_table <- topic_table[-remove,]
# assign new IDs
rownames(topic_table) <- NULL
topic_table$ID <- rownames(topic_table)

theta <- theta[,-remove]
colnames(theta) <- topic_table$labels

beta <- beta[,-remove]
colnames(beta) <- topic_table$labels

K <- nrow(topic_table)
```

```{r}
topic_table
```


# Topic Analyses

Now it’s time for the actual topic analyses.
In this demonstration, we’ll have a look at 
+ topic prevalence by artist
+ topic prevalence by artist over time

**Topic prevalence** is the mean document-topic probability, i.e., the mean share of terms relating to this topic in a song.


## Most prevalent topics

Overall:

```{r}
prevalence_sorted <- topic_table$prevalence
names(prevalence_sorted) <- topic_table$labels
prevalence_sorted <- sort(prevalence_sorted)

par(mar=c(2, 4.25, 1, 1)*2.5)
barplot(prevalence_sorted, horiz = TRUE, las = 1, xlim = c(0, 0.12),
        main = "Topic prevalence")
```

By artist:

```{r}
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
```

## Prevalence by year

```{r}
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

```

```{r}
# plots
par(mfrow=c(2,2))
for (i in 1:K){
  plot(ts_beyonce[,i], col = "orange", ylim = c(0, 0.5),
       ylab = "", xlab = "", main = paste(i, topic_table$labels[i]), cex.main = 1, lwd = 2)
  lines(ts_taylor[,i], col = "cornflowerblue", lwd = 2)
  
}
```

# Limitations

+ Very small number of documents! (*n* = 276)
+ We did not account for multilingualism during preprocessing!
+ No in-depth validation and model inspection!

# Take home messages

**Topic modeling takes some time!**

+ For preprocessing
+ For computation (candidate models of this example on 8x 1.6 GHz machine: 30 min)
+ For model selection and validation

It’s an iterative process.
Once the final model is validated, the actual topic analyses can be done relatively quickly.

**Top Terms**

+ Topics are actually vectors of `length(vocab)`. There is no rule on how many top terms to inspect.
+ Not all of the top terms necessarily co-occur!
+ Labeling is helpful for communicating the results, but can be a source of misinterpretation!

**Topic modeling should be used for exploratory purposes!**

Avoid implying that your final model reflects the "true" semantic structure of the corpus!
Always refer to the original texts before making conclusions (e.g., look at the actual songs containing the word "love")

**Documentation of all workflow steps is key!**

+ There are too much details for a method section in research publications, but these details are paramount!
+ Thus, provide your R scripts (and markdown) as supplementary files to your studies to make your analysis reproducible.
+ Don’t forget to set seeds where necessary (e.g. LDA model inference, network layouts)!

**Sharing is caring**

If you can’t provide the texts due to copyright, at least provide the `DFM` and describe ways on how to retrieve the data (e.g., [rehydrating of Twitter data](https://github.com/JonasRieger/corona100d), [search queries for databases](https://www.nature.com/articles/s41558-019-0684-5#Sec15)).

# SessionInfo

## Everything except computation of candidate models

```{r}
sessionInfo()
```


## Candidate models

```{r}
# > sessionInfo()
# R version 4.2.1 (2022-06-23)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.4 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
#  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8    
#  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
#  [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] ldaPrototype_0.3.1
# 
# loaded via a namespace (and not attached):
#  [1] parallelMap_1.5.1 pillar_1.6.4      compiler_4.2.1    prettyunits_1.1.1 viridis_0.6.2     tools_4.2.1       progress_1.2.2   
#  [8] dendextend_1.15.2 lifecycle_1.0.1   tibble_3.1.6      gtable_0.3.0      checkmate_2.0.0   viridisLite_0.4.0 pkgconfig_2.0.3  
# [15] rlang_0.4.12      DBI_1.1.2         parallel_4.2.1    gridExtra_2.3     lda_1.4.2         dplyr_1.0.7       generics_0.1.1   
# [22] vctrs_0.3.8       fs_1.5.2          hms_1.1.1         grid_4.2.1        tidyselect_1.1.1  glue_1.6.0        data.table_1.14.2
# [29] R6_2.5.1          fansi_0.5.0       ggplot2_3.3.5     purrr_0.3.4       magrittr_2.0.1    backports_1.4.1   scales_1.1.1     
# [36] BBmisc_1.11       ellipsis_0.3.2    assertthat_0.2.1  colorspace_2.0-2  utf8_1.2.2        munsell_0.5.0     crayon_1.4.2   
```

