library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(dplyr)
library(knitr)
library(Matrix)
library(factoextra)
library(ggplot2)
library(varhandle)


## TF-IDF
### 採集資料


Start <- FALSE # 開關

if(Start == TRUE){
  # 文本收集  
  data <- list()
  # PTT 用18禁處理
  curl <- getCurlHandle()
  curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)
  # 目標：C_Chat 版
  theme <- c("C_Chat") 
  # 範圍：2018/7/17~2018/7/24
  for( i in 16476:16611){ 
    tmp <- paste(as.character(i), '.html', sep='')
    url <- paste0('https://www.ptt.cc/bbs/',theme,'/index', tmp)
    html <- htmlParse(getURL(url,curl=curl),encoding = "utf-8")
    url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
    data <- rbind(data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')))
    print(paste("正在處理",url,"->",url.list))
  }
  data <- unlist(data)
  
  # 分檔函數
  
  getdoc <- function(url)
  {
    html <- htmlParse(getURL(url,curl=curl))
    doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
    time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
    temp <- gsub( "  ", " 0", unlist(time) )
    if(!identical(temp, character(0))) # 過濾無字串內容。
    {
      if(grepl("2018",temp)) ## 過濾非2018的內容，主要是暫時藉由此撇除無時間之資料。
      { 
        part <- strsplit( temp, split=" ", fixed=T )
        timestamp <- part[[1]][4]
        timestamp <- strsplit( timestamp, split=":", fixed=T )
        hour <- timestamp[[1]][1]
        name <- paste0('./DATA/', hour, ".txt")
        write(doc, name, append = TRUE)
        print(paste0("正在處理：'",time,"'"))
      } else {
        print(paste0("'",url,"' 該篇文章無效，無日期內容"))
      }
    } else {
      print(paste0("'",url,"' 該篇文章無效，無內容"))
    }
  }
  # 執行函數
  sapply(data, getdoc)
}


### 建立TDM文字矩陣


#設定資料來源，並分檔依序讀入
d.corpus <- Corpus(DirSource("./DATA"))
#進行基本文字清洗，移除部必要符號等元素
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

#分詞器
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist(segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

# 建立文字矩陣
count_token = function(d)
{
  as.data.frame(table(d)) # table 是計算次數，並且將它轉成data frame
}
tokens = lapply(seg, count_token)
n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0 # 將空白弄成0


### 生成TF-IDF


# TF-IDF
tf <- apply(as.matrix(TDM[,2:(as.numeric(n)+1)]), 2, sum) #所有字詞的出現次數和
idfCal <- function(word_doc)
{ 
  log2( as.numeric(n) / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(as.numeric(n)+1)]), 1, idfCal)
doc.tfidf <- TDM

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(as.numeric(n)+1)] <- (doc.tfidf[,2:(as.numeric(n)+1)] / tempY) * tempX
stopLine = rowSums(doc.tfidf[,2:(as.numeric(n)+1)])
delID = which(stopLine == 0) # 0 就是不需要的 (log=0)
TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,] #正式生成TF-IDF


###　檢視前十名


TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:5],1]))
  TopWords = rbind(TopWords, showResult)
}

rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)

### 畫圖


TDM$d = as.character(TDM$d)
AllTop = as.data.frame( table(as.matrix(TopWords)) )
AllTop = AllTop[order(AllTop$Freq, decreasing = TRUE),]
TopNo = 5
tempGraph = data.frame()
for( t in c(1:TopNo) )
{
  word = matrix( rep(c(as.matrix(AllTop$Var1[t])), each = n), nrow = n )
  temp = cbind( colnames(doc.tfidf)[2:(n+1)], t(TDM[which(TDM$d == AllTop$Var1[t]), 2:(n+1)]), word )
  colnames(temp) = c("hour", "freq", "words")
  tempGraph = rbind(tempGraph, temp)
  names(tempGraph) = c("hour", "freq", "words")
}
tempGraph$freq = unfactor(tempGraph$freq)
ggplot(tempGraph, aes(hour, freq)) + 
  geom_point(aes(color = words, shape = words), size = 5) +
  geom_line(aes(group = words, linetype = words))



## PCA
### 處理一下TF-IDF的資料以供之後使用

tfidf.t <- as.matrix(doc.tfidf[,-1],nrow = 3,byrow = FALSE)
rownames(tfidf.t)<-doc.tfidf[,1]
as.numeric(tfidf.t)

### PCA

docs.pca <- prcomp(tfidf.t, scale = T)
fviz_eig(docs.pca)
fviz_pca_ind(docs.pca, geom.ind = c("point"), col.ind = "cos2")
fviz_pca_var(docs.pca, col.var = "contrib")
fviz_pca_biplot(docs.pca, geom.ind = "point")
docs.eig <- get_eig(docs.pca)
docs.var <- get_pca_var(docs.pca)
docs.ind <- get_pca_ind(docs.pca)

## K-means

ind.coord2 <- docs.ind$coord[, 1:2]
wss <- c()
for (i in 1:10) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")
km <- kmeans(ind.coord2, 3)
plot(ind.coord2, col = km$cluster)
points(km$centers, col = 1:3, pch = 8, cex = 2)