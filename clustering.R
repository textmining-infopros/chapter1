#Load libraries
library(readr)
library(proxy)
library(RTextTools)
library(fpc)
library(wordcloud)
library(cluster)
library(tm)
library(stringi)
library(textmineR)
library(factoextra)
library(ggplot2)
library(igraph)

#Load dataset from the github
data <- read.csv("https://raw.githubusercontent.com/textmining-utl/chapter1/master/dataset.csv?token=ARBWLQ2P3JUCDWUN775L2TTAFS6B6")

#avoid invalid multiple string error with tolower()
data[,sapply(data,is.character)] <- sapply(
     data[,sapply(data,is.character)],
     iconv,"WINDOWS-1252","UTF-8")

#Create Document Term Matrix
dtm <- CreateDtm(doc_vec = data$Abstract,
                 doc_names = data$S.No.,
                 ngram_window = c(1,2),
                 stopword_vec = c(stopwords::stopwords("en"),
                                  stopwords::stopwords(source = 'smart')),
                 lower = TRUE, 
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 verbose = FALSE,
                 cpus = 2)

#Construct matrix of term counts to get IDF vector
tf_mat <- TermDocFreq(dtm)

#TF-IDF
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)

#Convert TF-IDF matrix to standard R matrix
m <- as.matrix(tfidf)

#Determine the number of K for clustering
fviz_nbclust(m, kmeans, method = "wss")
fviz_nbclust(m, kmeans, method = "wss") + geom_vline(xintercept = 5, linetype = 2) + labs(subtitle = "Elbow Method")

#Compute Distance Matrix
require(stats)
res.dist <- dist(x=m, method = "euclidean")

#Visualizing distance matrices
fviz_dist(res.dist)

# Agglomerative hierarchical clustering
require(stats)
res.hc <- hclust(d = res.dist, 
                 method = "ward.D")
plot(x = res.hc)

#Coloring Dendogram
require(factorextra)
fviz_dend(x=res.hc, cex = 0.7, lwd = 0.7)
require("ggsci")
fviz_dend(x=res.hc, cex = 0.8, lwd = 0.8, k=5, rect = TRUE, k_colors = "jco", rect_border = "jco", rect_fill = TRUE, ggtheme = theme_void())

#Different types of dendogram
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k= 4,
          rect = TRUE,
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          type = "circular",
          repel = TRUE)

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k= 4,
          rect = TRUE,
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE)
