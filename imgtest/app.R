#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        
        fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
        textInput("size", "Size", value = "500x500"),
        sliderInput("rotation", "Rotation", 0, 360, 0),
        sliderInput("blur", "Blur", 0, 20, 0),
        sliderInput("implode", "Implode", -1, 1, 0, step = 0.01),
        
        checkboxGroupInput("effects", "Effects",
                           choices = list("negate", "charcoal", "edge", "flip", "flop"))
    ),
    mainPanel(
        imageOutput("img")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(magick)
    
    # Start with placeholder img
    image <- image_read("https://images-na.ssl-images-amazon.com/images/I/81fXghaAb3L.jpg")
    observeEvent(input$upload, {
        if (length(input$upload$datapath))
            image <<- image_read(input$upload$datapath)
        info <- image_info(image)
        updateCheckboxGroupInput(session, "effects", selected = "")
        updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
    })
    
    # A plot of fixed size
    output$img <- renderImage({
        
        # Boolean operators
        if("negate" %in% input$effects)
            image <- image_negate(image)
        
        if("charcoal" %in% input$effects)
            image <- image_charcoal(image)
        
        if("edge" %in% input$effects)
            image <- image_edge(image)
        
        if("flip" %in% input$effects)
            image <- image_flip(image)
        
        if("flop" %in% input$effects)
            image <- image_flop(image)
        
        # Numeric operators
        tmpfile <- image %>%
            image_rotate(input$rotation) %>%
            image_implode(input$implode) %>%
            image_blur(input$blur, input$blur) %>%
            image_resize(input$size) %>%
            image_write(tempfile(fileext='jpg'), format = 'jpg')
        
        # Return a list
        list(src = tmpfile, contentType = "image/jpeg")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
