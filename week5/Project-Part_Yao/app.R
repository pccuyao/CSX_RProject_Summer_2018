# 姚承佑的分碼：失業問題

# 必要資源載入------------------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(reshape2)
library(showtext)

# 基本變數宣告------------------------------------------------------------------------------------------

  # Showtext 
    showtext_auto(enable=TRUE)
    PlotThroughShowtext <- function(x)
    {
      showtext.begin()
      print(x)
      showtext.end()
    }
  # Data
    Y_UN_YTOTAL <-read.csv("Data_Yao/Y_UN_YEAR_TOTAL.csv")
    Y_UN_WHY <-read.csv("Data_Yao/Y_UN_WHY.csv", header=T, sep=",")
    colnames(Y_UN_WHY) = c("YEARS","AGE","初次工作","業務緊縮或歇業","對工作不滿意","健康不良","季節或臨時結束","女性結婚或生育","退休","家務太忙","其他")
    Y_UN_WHY_g <-gather(Y_UN_WHY,why, why_un,-c(AGE,YEARS))
    Y_UN_EDU <-read.csv("Data_Yao/Y_UN_EDU.csv", header=T, sep=",")
    Y_UN_EDU <-subset(Y_UN_EDU[,-3])
    colnames(Y_UN_EDU) = c("YEAR","AGE","國小及以下","國中","高中","高職","專科","大學","研究所")
    Y_UN_EDU_g <-gather(Y_UN_EDU,edu, edu_un,-c(AGE,YEAR))

#UI Part------------------------------------------------------------------------------------------
ui <- 
  navbarPage(
    "台灣青年勞工狀況",
      navbarMenu
      (
        "失業",
        tabPanel
        (
          "失業人口歷年統計",
          titlePanel
          (
            h2("青年失業人口總統計")
          ),
          sidebarLayout
          (
            sidebarPanel
            (
              sliderInput
              (
                "Y_UN_YTOTAL_I",
                h3("年份選擇"),
                min = 82,
                max = 106,
                value = 105
              ),
              h3("詳細資料"),
              verbatimTextOutput
              (
                "Y_UN_YTOTAL_O_T"
              )
            ),
            mainPanel
            (
              plotOutput("Y_UN_YTOTAL_O")
            )
          )
        ),
        tabPanel
        (
          "失業原因歷年統計",
          titlePanel
          (
            h2("青年失業原因總統計")
          ),
          sidebarLayout
          (
            sidebarPanel
            (
              sliderInput
              (
                "Y_UN_WHY_I",
                h3("年份選擇"),
                min = 82,
                max = 106,
                value = 105
              ),
              
              selectInput(
                "Y_UN_WHY_I_S",
                label = h3("個別原因歷年成長"),
                choices = levels(factor(Y_UN_WHY_g[,3])),
                selected = levels(factor(Y_UN_WHY_g[,3]))[3]
              )
            ),
            mainPanel
            (
              plotOutput("Y_UN_WHY_O"),
              plotOutput("Y_UN_WHY_O_S")
            )
          )
        ),
        tabPanel
        (
          "失業與教育程度",
          titlePanel
          (
            h2("失業與教育程度統計")
          ),
              selectInput(
                "Y_UN_EDU_I_S",
                label = h3("個別學歷歷年分佈"),
                choices = levels(factor(Y_UN_EDU_g[,3])),
                selected = levels(factor(Y_UN_EDU_g[,3]))[1]
              ),

              plotOutput("Y_UN_EDU_O")

        )
      )
    )

#Server Part------------------------------------------------------------------------------------------
server <- function(input, output)
{
  
  #失業人口歷年統計------------------------------------------------------------------------------------------
  output$Y_UN_YTOTAL_O <-renderPlot({
      g<-ggplot(Y_UN_YTOTAL, aes(x = YEAR)) +
        geom_area(aes(y = TOTAL, group = 1,fill = "總計",color="總計")) +
        geom_area(aes(y = MALE, group = 2,fill="男",color = "男")) +
        geom_area(aes(y = FEMALE, group = 3,fill="女",color = "女")) +
        scale_x_continuous(breaks=Y_UN_YTOTAL$YEAR) +
        scale_y_continuous(breaks=seq(44,271,by=8)) +
        scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
        scale_colour_manual(values=c("black", "black", "black")) +
        geom_vline(xintercept = input$Y_UN_YTOTAL_I,
                   color = "black", size=1) +
        geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,2], linetype="dashed",
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,3], linetype="dashed", 
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_YTOTAL[input$Y_UN_YTOTAL_I-81,4], linetype="dashed", 
                   color = "black", size=0.5) +
        labs(x="年(民國)",y="人數(千人)",title = "青年失業人口總統計\n\n民國82年至106年(15~29歲)\n\n單位：千人")+
        guides(color=FALSE)
      PlotThroughShowtext(g)
    }
  )
  output$Y_UN_YTOTAL_O_T <-renderText(
    {
      paste(
        "民國:",input$Y_UN_YTOTAL_I,"年\n",
        "總人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,2],"(千人)\n",
        "男性人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,3],"(千人)\n",
        "女性人口:",Y_UN_YTOTAL[input$Y_UN_YTOTAL_I -81,4],"(千人)\n"
      )
    }
  )

  #失業原因歷年統計------------------------------------------------------------------------------------------
  output$Y_UN_WHY_O <- renderPlot(
    {
      g<-ggplot(subset(Y_UN_WHY_g,YEARS==input$Y_UN_WHY_I&!AGE=="TOTAL"), aes(x=why, y=why_un, fill=AGE)) + 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
        coord_flip() +
        theme(legend.position = "top")+
        labs(x="人數(人)",y="原因",title = paste("民國",input$Y_UN_WHY_I,"年青年各原因失業人數\n\n單位：人"))
      PlotThroughShowtext(g)
    }
  )
  output$Y_UN_WHY_O_S <- renderPlot(
    {
      Y_UN_WHY_Data_X = subset(Y_UN_WHY,select = c("YEARS","AGE",input$Y_UN_WHY_I_S))
      Y_UN_WHY_Data_X = dcast(Y_UN_WHY_Data_X, YEARS~AGE,sum)
      colnames(Y_UN_WHY_Data_X) =  c("YEARS","FIF","TWEN","TWENFITH","TOTAL")
    #  Y_UN_WHY_Data_X = Y_UN_WHY_Data_X[-26,-2]
      g<-ggplot(Y_UN_WHY_Data_X,aes(x=YEARS)) +
        geom_line(aes(y = TOTAL,group = 1,color="TOTAL")) +
        geom_line(aes(y = FIF,group = 2,color="15~19")) +
        geom_line(aes(y = TWEN,group = 3,color="20~25")) +
        geom_line(aes(y = TWENFITH,group = 4,color="25~29")) +
        scale_x_continuous(breaks=Y_UN_WHY_Data_X$YEAR)+
        scale_y_continuous(
          breaks=seq
          (
            min(
                Y_UN_WHY_Data_X$FIF,
                Y_UN_WHY_Data_X$TWEN,
                Y_UN_WHY_Data_X$TWENFITH
                ),
            max(
                Y_UN_WHY_Data_X$TOTAL
               ),
            (
              as.integer
              (
                (
                  max
                  (
                    Y_UN_WHY_Data_X$TOTAL
                  )
                  -min
                  (
                    Y_UN_WHY_Data_X$FIF,
                    Y_UN_WHY_Data_X$TWEN,
                    Y_UN_WHY_Data_X$TWENFITH
                  )
                )
                /25
              )
            )
          )
        ) +
        #scale_fill_manual(values=c("RED", "BLUE", "GREEN"))+
        #s
        geom_vline(xintercept = input$Y_UN_WHY_I,
                   color = "black", size=1)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,5], linetype="dashed",
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,4], linetype="dashed", 
                   color = "black", size=0.5)+
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,3], linetype="dashed", 
                   color = "black", size=0.5) +
        geom_hline(yintercept=Y_UN_WHY_Data_X[input$Y_UN_WHY_I-81,2], linetype="dashed", 
                   color = "black", size=0.5)+
        labs(x="年(民國)",y="人數(人)",title = paste(input$Y_UN_WHY_I_S," 的歷年青年失業人數統計\n\n單位：人"))
      PlotThroughShowtext(g)
    }
  )
  #失業與教育程度統計------------------------------------------------------------------------------------------
  output$Y_UN_EDU_O <- renderPlot(
    {
      Y_UN_EDU_Data_X <-subset(Y_UN_EDU,Y_UN_EDU$AGE == "TOTAL")
      Y_UN_EDU_Data_X <-subset(Y_UN_EDU_Data_X[,-2])
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_g,!Y_UN_EDU_g$AGE == "TOTAL")
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_Data_Y,Y_UN_EDU_Data_Y$edu == input$Y_UN_EDU_I_S)
      Y_UN_EDU_Data_Y <-subset(Y_UN_EDU_Data_Y[,-3])
      ggplot() + 
        #直條圖
         geom_bar(data=Y_UN_EDU_Data_Y,aes(x= YEAR, y= edu_un, fill=AGE,alpha=12),stat="identity",position = position_stack(reverse = TRUE))+
        # 線
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,2],group = 1,color="國小及以下"))+
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,3],group = 2,color="國中")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,4],group = 3,color="高中")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,5],group = 4,color="高職")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,6],group = 5,color="專科")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,7],group = 6,color="大學")) +
        geom_line(data=Y_UN_EDU_Data_X,aes(x= YEAR,y = Y_UN_EDU_Data_X[,8],group = 7,color="研究所"))+
        #刻度
        scale_x_continuous(breaks=Y_UN_EDU_Data_X$YEAR)+
        scale_y_continuous(breaks=seq(min(Y_UN_EDU_g$edu_un),max(Y_UN_EDU_g$edu_un),as.integer((max(Y_UN_EDU_g$edu_un)-min(Y_UN_EDU_g$edu_un))/25))) +
        labs(x="年(民國)",y="人數(人)",title = paste(input$Y_UN_WHY_I_S," 的歷年失業與教育人數統計\n\n單位：人"))+
        #關閉圖示
        guides(alpha=FALSE)
         
    }
  )
}

#執行------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

