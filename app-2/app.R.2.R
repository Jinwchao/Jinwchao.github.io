library(tidyr)
library(dplyr)
library(rworldmap)
library(readr)

world_population <- read_csv("world_population.csv")
world_lon_lat <- read_csv("world_lon_lat.csv")

world_population <- world_population %>%
  rename(country = `Country/Territory`)

world_data <- merge(world_population, world_lon_lat, by = "country")

library(shiny)
library(leaflet)
library(dplyr)

ui <- fluidPage(
  titlePanel("World Population Map"),
  sidebarLayout(
    sidebarPanel(
      # 使用sliderInput代替selectInput来选择年份
      sliderInput("year",
                  "Select Year:",
                  min = 1970,
                  max = 2020,
                  value = 2020,
                  sep = "",
                  step = 10,  # 每隔十年为一个步长
                  ticks = TRUE  # 显示滑块下的刻度
      )
    ),
    mainPanel(
      leafletOutput("worldMap")
    )
  )
)

server <- function(input, output) {
  output$worldMap <- renderLeaflet({
    # 使用滑块的值作为年份
    year_col <- paste0(input$year, " Population") # 构造列名，例如"2020 Population"
    
    # 根据选择的年份和人口密度计算颜色
    data_filtered <- world_data %>%
      mutate(Population = get(year_col),
             # 使用case_when来根据人口密度设置颜色
             Color = case_when(
               `Density (per km)` <= 50 ~ "green",
               `Density (per km)` > 50 & `Density (per km)` <= 100 ~ "orange",
               `Density (per km)` > 100 ~ "red",
               TRUE ~ "gray" # 为了安全起见，添加一个默认值
             )) %>%
      filter(!is.na(Population))
    
    # 创建地图
    leaflet(data_filtered) %>%
      addTiles() %>%
      addCircles(
        lng = ~lon, lat = ~lat,
        color = ~Color,  # 使用前面计算的颜色
        weight = 1,
        radius = ~sqrt(Population) * 110, # 人口大小决定圆圈大小
        popup = ~paste(country, Population)
      )
  })
}

# 运行Shiny应用
shinyApp(ui, server)
