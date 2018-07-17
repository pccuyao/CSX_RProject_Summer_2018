# ggplot 的練習
## 首先導入library

library(ggplot2)
library(ggmap)
library(mapproj)
library(GGally)
library(scales)
library(memisc)

## 內建的範例資料："mpg"

mpg

## 一些圖表的練習

# 車種的統計
ggplot(data = mpg ,aes(x = class)) + geom_bar(fill = "red", colour = "blue")
# Single 順序 
ggplot(data = mpg, aes(x = displ)) + geom_histogram(binwidth = 1)
# Double 類別 
ggplot(data = mpg, aes(x = year,y= cty)) + geom_line(size=8)
ggplot(data = mpg, aes(x = displ,y= cty,color=class)) + geom_point()


## ggmap 地圖的練習 - 雨量資料

### 從csv檔中導入資料


rn <- read.csv("Rain.csv")

### 設定地圖資料，並指定為台灣

map <- get_map(location = 'Taiwan', zoom = 8)

### 使用ggmap建立地圖，並配合geom_point對地圖上的相應點做顯示

ggmap(map) +geom_point(aes(x = TWD67Lon, y = TWD67Lat,color= Now ,alpha = Now * 10), data = rn)+ggtitle("日累積降雨量  \n 單位：毫米 \n 資料來源：政府資料公開平台")


