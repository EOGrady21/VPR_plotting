library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)

# Data ------------------------------------------------------------------
dt <- data.frame(fruits = c("apple","banana","oranges"),
                 rank = c(11, 22, 33),
                 image_url = c('https://images.unsplash.com/photo-1521671413015-ce2b0103c8c7?ixlib=rb-0.3.5&s=45547f67f01ffdcad0e33c8417b840a9&auto=format&fit=crop&w=667&q=80',
                               "https://images.unsplash.com/photo-1520699697851-3dc68aa3a474?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=ef15aee8bcb3f5928e5b31347adb6173&auto=format&fit=crop&w=400&q=80",
                               "https://images.unsplash.com/photo-1501925873391-c3cd73416c5b?ixlib=rb-0.3.5&ixid=eyJhcHBfaWQiOjEyMDd9&s=379e4a0fffc6d11cd5794806681d0211&auto=format&fit=crop&w=750&q=80"
                 ))

# img_dt <- dt %>%
#   mutate(img = paste0("<a target='_blank' href='", image_url, "'><img src=\'", image_url, "' height='40'></img></a>")) %>%
#   mutate(link = paste0("<a href='", image_url,"' target='_blank'>","View photo","</a>"))

# Dashboard ----------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "Test"),
    
    dashboardSidebar(),
    
    dashboardBody(
        tags$head(
            tags$style(
                HTML(
                    "img.small-img {
          max-width: 75px;
          }")
            )
        ),
        
        plotlyOutput("hoverplot")
    )
)

server <- function(input, output) {
    
    output$hoverplot <- renderPlotly({
        ed <- event_data("plotly_hover")
        
        xnudgeRightMost <- 0.2
        xnudge <- 0.05
        ynudgeBottomMost <- 3
        ynudge <- 1
        imageSizeX <- 2
        imageSizeY <- 2
        
        fruitLv <- levels(factor(dt$fruits))
        xpos <- which(fruitLv == ed$x)
        x <- ifelse(xpos != length(fruitLv), xpos + xnudge - 1, xpos - xnudgeRightMost - 1)
        y <- ifelse(ed$y == min(dt$rank), ed$y + ynudgeBottomMost, ed$y - ynudge)
        
        plot_ly(dt, x = ~fruits, y = ~rank, type = 'scatter', mode = 'markers',
                hoverinfo = "text",
                text = ~fruits) %>%
            layout(images = list(source = dt[(ed$pointNumber + 1), 3],
                                 xref = "x",
                                 yref = "y",
                                 x = x,
                                 y = y,
                                 sizex = imageSizeX,
                                 sizey = imageSizeY,
                                 opacity = 0.8,
                                 layer = "above"
            ))
    })
}

shinyApp(ui = ui, server = server)