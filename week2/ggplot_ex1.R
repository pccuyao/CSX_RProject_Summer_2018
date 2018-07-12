library(ggplot2)
library(ggmap)
library(mapproj)
library(GGally)
library(scales)
library(memisc)
mpg
rn <- read.csv("Rain.csv")
# Single 類別
ggplot(data = mpg ,aes(x = class)) + geom_bar(fill = "red", colour = "blue")
# Single 順序 
ggplot(data = mpg, aes(x = displ)) + geom_histogram(binwidth = 1)
# Double 類別 
ggplot(data = mpg, aes(x = year,y= cty)) + geom_line(size=8)
ggplot(data = mpg, aes(x = displ,y= cty,color=class)) + geom_point()

# rain

map <- get_map(location = 'Taiwan', zoom = 8)
ggmap(map) +geom_point(aes(x = TWD67Lon, y = TWD67Lat,color= Now ,alpha = Now * 10), data = rn)+ggtitle("日累積降雨量  \n 單位：毫米 \n 資料來源：政府資料公開平台")