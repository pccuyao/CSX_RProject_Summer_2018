source("pttTestFunction.R")
id = c(6430:6535)
name_post <- "MobileComm"
URL = paste0("https://www.ptt.cc/bbs/",name_post,"/index", id, ".html")
filename = paste0(id, ".txt")
#mapply(pttTestFunction, 
 #      URL = URL, filename = filename)


## 2
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "噓")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "被")
docs <- tm_map(docs, toSpace, "到")
docs <- tm_map(docs, toSpace, "好")
docs <- tm_map(docs, toSpace, "會")
docs <- tm_map(docs, toSpace, "但")
docs <- tm_map(docs, toSpace, "你")
docs <- tm_map(docs, toSpace, "跟")
docs <- tm_map(docs, toSpace, "能")
docs <- tm_map(docs, toSpace, "嗎")
docs <- tm_map(docs, toSpace, "更")
docs <- tm_map(docs, toSpace, "只")
docs <- tm_map(docs, toSpace, "出")
docs <- tm_map(docs, toSpace, "阿")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "吧")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "這")
docs <- tm_map(docs, toSpace, "為")
docs <- tm_map(docs, toSpace, "啊")
docs <- tm_map(docs, toSpace, "喔")
docs <- tm_map(docs, toSpace, "又")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "標題")
docs <- tm_map(docs, toSpace, "過")
docs <- tm_map(docs, toSpace, "再")
docs <- tm_map(docs, toSpace, "因")
docs <- tm_map(docs, toSpace, "網址")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "什麼")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs
#
library(jiebaR)

#
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)