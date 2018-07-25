# EDA 深入練習

#先使用老師的程式碼，套入自己的模型試試看

## 載入所需資源


library(ggplot2)
library(readr)
library(knitr)
library(Hmisc)
library(coefplot)
library(lattice)
library(MASS)


## 資料說明
# 這次用的資料暫時以 ggplot2 的 mpg 資料為範本。

### 基本檢視


kable(head(mpg))


### 資料性質


show_class <- data.frame(sapply(mpg,class))
colnames(show_class) <- c("data class")
kable(show_class)


### 欄位意義

#| 欄位 | 名稱 |
#|------|------|
#| manufacturer | 生產商 |
#| model | 型號 |
#| displ | 排氣量 |
#| year | 生產年 |
#| cyl | 汽缸數 |
#| trans | 變速器 |
#| drv | 幾輪驅動 |
#| cty | 每加侖城市里程 |
#| hwy | 每加侖高速公路里程 |
#| fl | 汽油種類 |
#| class | 車種 |

## 敘述統計

summary(mpg,summary.data.frame)


## 機率分布


# 因為mpg是ggplot2內建的資料集，而為了要做修改利用，另存成一個變量
mpga <- mpg
# 對要進行主要比對的class 做factor levels。 
mpga$class <- factor(mpga$class,levels= c("suv","subcompact","pickup","minivan","midsize","compact","2seater"))
# 機率分布
kable(var(mpga))


# 可以看到，機率分布的部分只能對數值與數值進行演算。

## 假設檢定
### T.TEST
# 我們來對year和hwy做t.test


t.test(hwy ~ year, data = mpga)


# 可以看到，由於p-value大於0.05，所以這兩筆資料並沒有關係。

### 對class做平均與信賴區間


ggplot(data = mpga, 
       aes(x = class, y = hwy)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1) +
  scale_y_continuous(breaks = seq(12, 44, by = 1)) +
  geom_hline(yintercept = mean(mpg$hwy) , 
             linetype = 'dotted') +
  labs(x = 'class', y = 'hwy') +
  coord_flip()

### anova

anova(m1 <- lm(hwy ~ class, data = mpga))


# 可以看到，由於p-value接近於0，所以這兩筆資料有關係。

### 觀察 class 、cyl 與 hwy 的關係


ggplot(data = mpga, 
       aes(group = class,
           x = cyl, y = hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(12, 44, by = 4)) +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = class, 
                  x = cyl, y = hwy), 
              method = 'lm', se = F) + 
  facet_grid( . ~  class) +
  labs(x = 'cyl', y = 'hwy')


### 看一下 cyl 是不是真的跟 hwy 有關係


anova(m2 <- update(m1, . ~ . + 
                     cyl, data = mpga))

### 畫圖

m2 <- lm(hwy ~ class+cyl- 1, 
         data = mpga)
coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = hwy')


## 線性回歸

fit_m2 <- data.frame(mpga[, c("hwy","class","cyl")], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat )
ggplot(data = fit_m2, aes(x = hwy, group = class)) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(mpga, tapply(hwy,class, mean))), linetype = 'dotted')+
  facet_grid(class ~ .) +
  scale_x_continuous(breaks = seq(200, 900, by = 100))+
  labs(x = 'hwy', y = '機率密度')

ggplot(data = fit_m2, aes(x = scale(resid)), group = class ) +
  stat_density(geom = 'path', position = 'identity', aes(linetype = class)) +
  scale_linetype_manual(values = 7:1) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  labs(x = '標準化殘差', y = '機率密度') +
  theme(legend.position = c(.15, .8))

qqmath(~ scale(resid) | class, data = fit_m2, type = c('p', 'g', 'r'),
       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),
       pch = '.', cex = 2)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = class )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(class ~ .) +
  labs(x = '數學預測值', y = '標準化殘差')

ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group = class)) +
  geom_text(aes(label = rownames(fit_m2)), cex = 2) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(class ~ .) +
  labs(x = '影響值', y = '標準化殘差')
summary(influence(m2)$hat)