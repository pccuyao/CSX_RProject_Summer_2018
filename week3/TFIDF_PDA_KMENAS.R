# 1
library(XML)
library(RCurl)

data <- list()
theme <- c("C_Chat")
for( i in 16501:16501){
  tmp <- paste(i, '.html', sep='')
  url <- paste('https://www.ptt.cc/bbs/',theme,'/index', tmp, sep='')
  html <- htmlParse(getURL(url),encoding = "utf-8")
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, paste('https://www.ptt.cc', url.list, sep=''))
}
data <- unlist(data)

# 2
library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse(getURL(url))
  doc  <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub( "  ", " 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  #date <- paste(part[[1]][2], part[[1]][3], part[[1]][5], sep="-")
  #date <- paste(part[[1]][2], part[[1]][5], sep="_")
  #date <- paste(part[[1]][1], part[[1]][2], sep="_")
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  #print(hour)
  name <- paste0('./DATA/', hour, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)
