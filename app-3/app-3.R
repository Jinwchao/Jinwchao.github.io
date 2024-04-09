library(shiny)
library(ggplot2)
library(dplyr)

Crop_production <- read.csv("crop_production.csv")

ui <- fluidPage(
  titlePanel("Crop Production by Country"),
  sidebarLayout(
    sidebarPanel(
      # 创建一个滑动条来选择年份
      sliderInput("year",
                  "Select Year:",
                  min = 1990,
                  sep = "",
                  max = max(Crop_production$Time),
                  value = max(Crop_production$Time),
                  step = 1)
    ),
    mainPanel(
      # 显示柱状图
      plotOutput("cropPlot")
    )
  )
)

server <- function(input, output) {
  output$cropPlot <- renderPlot({
    # 根据选择的年份和作物类型过滤数据
    data_filtered <- Crop_production %>%
      filter(Time == input$year, Subject == "RICE") %>%
      group_by(Location) %>%
      summarize(AverageProduction = mean(Value, na.rm = TRUE))
    
    # 绘制柱状图
    options(scipen = 999)
    ggplot(data_filtered, aes(x = Location, y = AverageProduction + 10)) +
      geom_col() +  # 如果国家名称较长，可以旋转标签
      labs(x = "Country", y = "Average Crop Production", title = paste("Rice Production in", input$year)) +
      theme_bw() +
      scale_y_log10()  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}


# 运行Shiny应用
shinyApp(ui = ui, server = server)
