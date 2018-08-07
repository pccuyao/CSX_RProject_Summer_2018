library(tm)
library(tmcn)
library(factoextra)
library(Matrix)

docs.corpus <- Corpus(DirSource("./DATA"))
docs.seg <- tm_map(docs.corpus, segmentCN)
docs.tdm <- TermDocumentMatrix(docs.seg)

docs.tf <- apply(as.matrix(docs.tdm), 2, function(word) { word/sum(word) })
idf <- function(doc) {
  return ( log2( length(doc)+1 / nnzero(doc)) )
}
docs.idf <- apply(as.matrix(docs.tdm), 1, idf)
docs.tfidf <- docs.tf * docs.idf
docs.pca <- prcomp(docs.tfidf, scale = T)
fviz_eig(docs.pca)