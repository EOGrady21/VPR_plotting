
library(shiny)

library(shinythemes)
library(dplyr)
library(hashids)
library(gallerier)


images <- data.frame(src = list.files('E:/VPR_plotting/shinyApp/www/', full.names = TRUE)) #%>%
    #tidyr::separate(col = 'src', c('txt', 'date', 'time', 'msec'), sep = '_|\\.', remove = FALSE) %>%
    #rowwise() #%>%
    #mutate(date = lubridate::ymd(date),
           #key = hashids::encode(1e3 + as.integer(msec), hashid_settings(salt = 'VPRR'))
          # )

# Define UI for application that draws a histogram
ui <- fluidPage(
                tabsetPanel(
                    tabPanel('Lightbox',
                             fluidRow(
                                 column(12, 
                                        uiOutput('lb')
                                 ))
                             
                    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$lb <- renderUI({
        
        # gallery DIV
        lightbox_gallery(df = images, 'gallery', display = TRUE)
        
    })
    
    output$ps <- renderUI({
        
        # gallery DIV
        photoswipe_gallery(images[sample(1:nrow(images), 12, replace = TRUE),], 'ps-gallery', display = TRUE)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)