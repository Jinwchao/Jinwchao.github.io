library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

global_temp <- read_csv("GlobalLandTemperaturesByCountry.csv")

# 确保Dt列是日期类型
global_temp$dt <- as.Date(global_temp$dt)

# 添加年份列
global_temp$Year <- format(global_temp$dt, "%Y")

global_temp <- filter(global_temp, Year >= 1800)

# 按国家和年份聚合数据，计算每年的平均温度
annual_avg_temp <- global_temp %>%
  group_by(Country, Year) %>%
  summarize(AverageTemperature = mean(AverageTemperature, na.rm = TRUE), .groups = 'drop') # 确保分组后的数据不会保留在结果中

# UI定义
ui <- fluidPage(
  
  titlePanel("GlobalLand Temperature Trends by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:",
                  choices = unique(global_temp$Country))
    ),
    mainPanel(
      plotOutput("tempPlot")
    )
  )
)

# 服务器逻辑
server <- function(input, output) {
  output$tempPlot <- renderPlot({
    # 筛选选定国家的数据
    country_data <- filter(annual_avg_temp, Country == input$country)
    
    # 使用ggplot绘制折线图
    ggplot(country_data, aes(x = as.numeric(Year), y = AverageTemperature)) +
      geom_line() + 
      theme_minimal() +
      scale_x_continuous(breaks = seq(min(country_data$Year), max(country_data$Year), by = 10)) + # 每10年显示一次刻度
      labs(x = "Year", y = "Temperature (°C)", title = paste("Annual Temperature Trends in", input$country))
  })
}
# 运行应用
shinyApp(ui = ui, server = server)
