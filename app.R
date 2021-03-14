library(shiny)


library(tidyverse)
library(DT)
raw <- read.csv("./data/owid-co2-data.csv")
dat <- raw %>% select(iso_code,     country, year ,  co2, gdp)
dat <- na.omit(dat)
dat <- dat %>% gather(key=dimension, value=value, co2:gdp) %>% na.omit()



ui <- fluidPage(
  titlePanel("Climate Analysis"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("country", "Country", choices = unique(dat$country)),
      radioButtons("year", h3("Year"),
                   choices = list("<=2000" = 1, "2000" = 2),selected = 1)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot"),
                  textOutput("text")),
        tabPanel("Table", DTOutput("table"),
                 textOutput("text2"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$year==1){
      df <- dat %>% filter(year<=2000)
    }else{
      df <- dat %>% filter(year>2000)
    }
    df <- df %>% filter(country==input$country)
    df %>% filter(dimension %in% c("co2", "gdp")) %>% spread(key=dimension, value=value) %>%
      ggplot(aes(x=gdp, y=co2)) + geom_point() + geom_smooth()
  })
  
  output$text <- renderText({
    req(input$year)
    req(input$country)
    if(input$year==1){
      paste0("The scatter plot of co2 and gdp of ", input$country, " in 1750-2000")
    }else{
      paste0("The scatter plot of co2 and gdp of ", input$country, " in 2001-now")
    }
    
  })

  output$table <- renderDataTable({
    if(input$year==1){
      df <- dat %>% filter(year<=2000)
    }else{
      df <- dat %>% filter(year>2000)
    }
    df <- df %>% filter(country==input$country)
    df
   
  })  
  output$text2 <- renderText({
    req(input$year)
    req(input$country)
    if(input$year==1){
      paste0("The data table of ", input$country, " in 1750-2000")
    }else{
      paste0("The data table of ", input$country, " in 2001-now")
    }
    
  })
}

shinyApp(ui = ui, server = server)
