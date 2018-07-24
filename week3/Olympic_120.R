# EDA 練習
# 這個練習主要使用120年來奧運(冬夏)的資料來做練習。

# 資料來源: [https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results](https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results)

## 載入 Library


library(ggplot2)
library(readr)
library(knitr)


## 從csv檔讀入資料

rn <- read_csv("athlete_events.csv",
               col_types = cols(
                 ID = col_character(),
                 Name = col_character(),
                 Sex = col_factor(levels = c("M","F")),
                 Age =  col_integer(),
                 Height = col_double(),
                 Weight = col_double(),
                 Team = col_character(),
                 NOC = col_character(),
                 Games = col_character(),
                 Year = col_integer(),
                 Season = col_factor(levels = c("Summer","Winter")),
                 City = col_character(),
                 Sport = col_character(),
                 Event = col_character(),
                 Medal = col_factor(levels = c("Gold","Silver","Bronze"))
               )
)

## 稍微瀏覽一下~


head(rn)


## 欄位名稱

##欄位 | 名稱 | 類別 |
##  -----|------|------|
##  ID | 行號 | character|
##  Name |運動員姓名 | character|
##  Sex |男或女 | factor |
##  Age |年齡 |integer|
##  Height |身高(公分)| numeric |
##  Weight |體重(公斤)| numeric |
##  Team |團隊名稱 | character |
##  NOC |國家奧委會三字母代碼 | character |
##  Games |年分與季度 | character |
##  Year |年| integer |
##  Season |夏季或冬季 | factor |
##  City |主辦城市 | character |
##  Sport |比賽運動 | character |
##  Event |比賽項目 | character |
##  Medal |金、銀、銅或NA | factor|
  
  ## 建立BMI
  
  rn$BMI <- rn$Weight / ((rn$Height/100)^2)


## 重新檢視，可以看出多出了BMI

head(rn)

## 篩選出男女、以及不使用NA

rn_M<-subset(rn,Sex == "M")
rn_M_n<-subset(rn_M,BMI != "NA")
rn_F<-subset(rn,Sex == "F")
rn_F_n<-subset(rn_F,BMI != "NA")

## 歷年來男女的BMI平均值

mean_rn_f<-data.frame(trt = c("M","F"), outcome = c(mean(rn_M_n$BMI), mean(rn_F_n$BMI)))
mean_rn_f
ggplot(mean_rn_f, aes(trt, outcome)) + geom_col(width = 0.9)



## 歷年來年齡與得獎的統計


#建立一個新表單，移除沒有獎項的
rn_ns <-subset(rn,Medal != "NA")

#篩選出金牌
rn_MG <-subset(rn_ns,Medal == "Gold")
rn_Mx <- as.data.frame(table(rn_MG$Age))
colnames(rn_Mx) <- c("Age","Gold")

#篩選出銀牌
rn_MS <-subset(rn_ns,Medal == "Silver")
rn_MSx <- as.data.frame(table(rn_MS$Age))
colnames(rn_MSx) <- c("Age","Silver")

#篩選出銅牌
rn_MB <-subset(rn_ns,Medal == "Bronze")
rn_MBx <- as.data.frame(table(rn_MB$Age))
colnames(rn_MBx) <- c("Age","Bronze")

#合併資料框
rn_Mx <-  merge(rn_Mx, rn_MSx, by="Age", all = TRUE)
rn_Mx <-  merge(rn_Mx, rn_MBx, by="Age", all = TRUE)

#將合併後資料框中的NA值轉換為0
rn_Mx[is.na(rn_Mx)] <- 0

#輸出圖片
ggplot(rn_Mx, aes(x = Age)) +
  geom_line(aes(y = Gold, group = 1,colour = "red")) +
  geom_line(aes(y = Silver, group = 2,colour = "green")) +
  geom_line(aes(y = Bronze, group = 3,colour = "blue"))+ xlab("年齡") + ylab("數量") +
  scale_colour_discrete(name="獎項",breaks = c('red','green','blue'), labels = c('金牌','銀牌','銅牌')) +
  ggtitle("歷年來年齡與得獎的關係\n單位(人)")



# 由此可知，雖然年齡跟得獎人數有非常大的關係，主要集中在青壯年期時較高。但是若要論得甚麼獎，則是跟年齡沒有太大關係的。


# 下為為了產生上圖而重新整理出的資料，以供參考

kable(rn_Mx)



