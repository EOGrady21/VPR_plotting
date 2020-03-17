
# https://stackoverflow.com/questions/51531743/image-slideshow-in-r-shiny


library(shiny)
library(shinyFiles)

ui <- fluidPage(
    
    titlePanel("Slideshow"),
    
    sidebarLayout(
        sidebarPanel(
          
            shinyDirButton("dir", "Chose directory", "Upload"),
            textInput('tow', 'Tow', value = '0'),
            textInput('hour', 'Hour', value = '22'),
            textInput('day', 'Day', value = '286'),
            textInput('cruise', 'Cruise', value = 'IML2018051'),
            numericInput('num', 'Number of images shown', value = 24, step = 2),
           
            
           # h4("output$dir"),
           # verbatimTextOutput("dir"), br(),
           # h4("Files in that dir"),
           # verbatimTextOutput("files") 
        ),
        
        mainPanel(
        fluidRow(
            column( 1, 
            imageOutput("image")
            
            #textOutput('text')
            ),
            
        column(4, offset = 5, 
               imageOutput("image2")
        )
        
        )
        
        )
        
    )
)

server <- function(input, output, session) {
    
   
    shinyDirChoose(input, 'dir',roots = c('C:/' = 'C:/', 'D:/' = 'D:/', 'E:/' = 'E:/', 'F:/' = 'F:/'  )  )
    dir <- reactive(input$dir)
    output$dir <- renderPrint(dir())
    
    # path
    path <- reactive({
        home <- input$dir$root
        file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    })
    
    # files
    output$files <- renderPrint(list.files(path()))
    
    
    values <- reactiveValues(
        upload_state = NULL
    )
    
    observeEvent(input$dir, {
        values$upload_state <- 'uploaded'
    })
    
    imgs_path <- reactive({
        if (is.null(values$upload_state)) {
            
            imgs_path <-  paste0("E:/VP_data/", input$cruise, "/rois/vpr", input$tow,"/d", input$day, "/h", input$hour, "/")
            
            
           
        } else if (values$upload_state == 'uploaded') {
            imgs_path <- path()
            
        } 
    })
    
  
    ipgmax <- reactive(input$num)
    num_col <- reactive(ipgmax()/2)
    index <- reactive(seq(1,num_col()))
    index2 <- reactive(seq(num_col()+1, num_col()*2))
    
    
    
    output$image <- renderImage({
        
        
        imgss <- list.files(imgs_path(), pattern = '.tif', full.names = TRUE)
            

        roi_ids <- vpr_roi(imgss[index()])
        roi_id_string <- stringr::str_c(roi_ids, sep = ',')
        
        
        
        image <- image_read(imgss[index()])
        
        image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
        
        image <- image_append(image_border(image, color = 'white', geometry = '10x8'), stack = TRUE)
        
        
        tmpfile <- image %>%
            image_write(tempfile(fileext='png'), format = 'png')
     
        list(src = tmpfile, contentType = "image/png")
        
    }, deleteFile = FALSE)
    
    
    output$image2 <- renderImage({
        
        
        imgss <- list.files(imgs_path(), pattern = '.tif', full.names = TRUE)
        
        roi_ids <- vpr_roi(imgss[index2()])
        roi_id_string <- stringr::str_c(roi_ids, sep = ',')
        
        image <- image_read(imgss[index2()])
        
        image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
        
        
        image <- image_append(image_border(image, color = 'white', geometry = '10x8'), stack = TRUE)
        
        tmpfile <- image %>%
            image_write(tempfile(fileext='png'), format = 'png')
        
        list(src = tmpfile, contentType = "image/png")
        
    }, deleteFile = FALSE)
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)