library(shiny)
library(vprr)
library(ggplot2)
library(metR)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Video Plankton Recorder"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input Metadata."),
      
      fileInput('ctd_files', label = 'CTD Files', multiple = FALSE, accept = '.dat'),
      
      textInput('cruise', 'Cruise ID', placeholder = 'eg. IML2018051'),
      
      textInput('tow', label = 'VPR Tow ID', placeholder = 'eg. 1'),
      
      textInput('day', 'Day', placeholder = 'eg. 283'),
      
      textInput('hour', 'Hour', placeholder = 'eg. 06'),
      
      # checkboxInput("multiple", "Multiple Hours"),
      # conditionalPanel(
      #   condition = "input.multiple == true",
      #   textInput('hour2', 'Hour', placeholder = 'eg. 06'),
      #   textInput('hour3', 'Hour', placeholder = 'eg. 06')
      # ),
      
      textInput('station', label = 'Station ID', placeholder = 'eg. CAP1-2'),
      
      
      textInput('event', label = 'Event ID', placeholder = 'eg. 001'),
      
      
      numericInput('imageVolume', label = 'Image Volume ', value = 123456 ),
      
      
      numericInput('binSize', label = 'Bin Size', value = 5),
      
      
      textInput('basepath', label = 'Base Path', placeholder = 'E:/VP_data'),
      
      
      ##OPTIONAL QC PARAMETERS##
      #min and max values of each parameter
      
      sliderInput("sal_range", label = h3("Salinity Range"), min = 0, 
                  max = 50, value = c(28, 35)),
      
      sliderInput("temp_range", label = h3("Temperature Range"), min = 0, 
                  max = 50, value = c(0, 15)),
      
      sliderInput("pres_range", label = h3("Pressure Range"), min = 0, 
                  max = 500, value = c(0, 500)),
      
      
      
      
      #,
      
      # sliderInput("hr_range", label = h3("Time Range (hr)"), min = 0, 
                 # max = 4, value = c(0, 4), step = 0.1)
    ),
    
    mainPanel(
      #textOutput('samp')
      #tableOutput('test')
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           fluidRow(
                           column(12, plotOutput("plot")), 
                           column(12, plotOutput('plot2')),
                           column(12, plotOutput('plot3')),
                           column(12, plotOutput('plot4'))
                           )),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", DT::dataTableOutput("ctdroi")),
                  tabPanel("Images", imageOutput('rois'))
      )
    )
  )
)


server <- function(input, output) {

  
  
  datasetInput <- reactive({
     
           ctd_fns <- input$ctd_files
           
           if (is.null(ctd_fns))
             return(NULL)
           
           ctd_dat <- vpr_ctd_read(ctd_fns$datapath, station_of_interest = input$station, day = input$day, hour = input$hour)
           
           vpr_oce_create(ctd_dat)
           #get ROI data
           
           roi_files <- list.files(file.path(input$basepath, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) ))
           roi_num <- substr(roi_files, 5, nchar(roi_files) - 4)
           
           
           
           #get roi number that will match ctd time_ms
           rois <- as.numeric(substr(roi_num, 1, 8))
           #format as table to get frequency
           roi_table <- as.data.frame(table(rois))
           
           
           #subset ctd and roi data where time/roi identifier match
          ctd_sub <- which(ctd_dat$time_ms %in% rois)
           roi_sub <- which(rois %in% ctd_dat$time_ms)
           #subset data individually
           ctd_dat_sub <- ctd_dat[ctd_sub,]
           
           #EC & KS fix 2019/08/08 due to error producing NA roi numbers
           roi_dat_sub <- rois[!duplicated(rois)]
           roi_dat_sub <- rois[ctd_sub]
           
           #combine roi and ctd data
           all_dat <- ctd_dat_sub %>%
             dplyr::mutate(., roi = roi_dat_sub) %>%
             dplyr::mutate(., n_roi = roi_table$Freq) #add n_roi (count of rois per second)
           
           
           #add  time(hr) to combined data frame
           all_dat <- all_dat %>%
             dplyr::mutate(., avg_hr = time_ms/3.6e+06)
           
           
           all_dat_o <- all_dat #save original data as seperate object for comparison
           all_dat <- all_dat %>% #filter data based on parameter ranges set in step 1
             dplyr::filter(., salinity > min(input$sal_range)) %>%
             dplyr::filter(., salinity < max(input$sal_range)) %>%
             dplyr::filter(., temperature > min(input$temp_range)) %>%
             dplyr::filter(., temperature < max(input$temp_range)) %>%
             dplyr::filter(., pressure > min(input$pres_range)) %>%
             dplyr::filter(., pressure < max(input$pres_range)) #%>%
             #dplyr::filter(., avg_hr > min(input$hr_range)) %>%
            # dplyr::filter(., avg_hr < max(input$hr_range))
           
           
  })
  
  
  binnedData <- reactive({
    
    all_dat <- datasetInput()
    
    ctd_roi_oce <- vpr_oce_create(all_dat)
    
    
    vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce , imageVolume = input$imageVolume, binSize = input$binSize, rev = TRUE)
    
    
  })
  
  output$ctdroi <- renderDataTable({
    
    
    return(datasetInput())
  })
  
  output$summary <- renderPrint({
    #print(paste('Reading CTD file:', input$ctd_files))

    all_dat <- datasetInput()

    print(paste('                  Data Summary Report '));
     print(paste('Report processed:', as.character(Sys.time())));
     print(paste('Cast: ', input$tow, '   Day: ', input$day, '   Hour: ', input$hour));
     
     
     
     print(paste(' >>>>  Time '));
     print(paste('Data points: ', length(all_dat$time_ms)));
     print(paste('Range: ', min(all_dat$time_ms),' - ', max(all_dat$time_ms), ' (ms) '));
     print(paste('Range: ', min(all_dat$avg_hr),' - ', max(all_dat$avg_hr), ' (hr) '))
    
     
     
    # cat(' >>>>  Conductivity \n')
    # cat('Data points: ', length(all_dat$conductivity),'\n')
    # cat('Range: ', min(all_dat$conductivity),' - ', max(all_dat$conductivity), '  \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Temperature \n')
    # cat('Data points: ', length(all_dat$temperature),'\n')
    # cat('Range: ', min(all_dat$temperature),' - ', max(all_dat$temperature), ' (c) \n')
    # cat('QC: ', length(all_dat[all_dat$temperature < 0 ]), 'points below zero deg c \n')
    # cat('QC: ', length(all_dat[all_dat$temperature > 10]), 'points above ten deg c \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Pressure \n')
    # cat('Data points: ', length(all_dat$pressure),'\n')
    # cat('Range: ', min(all_dat$pressure),' - ', max(all_dat$pressure), ' (db) \n')
    # cat('QC: ', length(all_dat[all_dat$pressure < 0 ]), 'below zero db \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Salinity \n')
    # cat('Data points: ', length(all_dat$salinity),'\n')
    # cat('Range: ', min(all_dat$salinity),' - ', max(all_dat$salinity), ' (PSU) \n')
    # cat('QC: ', length(all_dat[all_dat$salinity < 28 ]), 'points below twenty-eight PSU \n')
    # cat('QC: ', length(all_dat[all_dat$salinity > 34 ]), 'points above thirty-four PSU \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Fluorescence \n')
    # cat('Data points: ', length(all_dat$fluorescence_mv),'\n')
    # cat('Range: ', min(all_dat$fluorescence_mv),' - ', max(all_dat$fluorescence_mv), ' (mv) \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Turbidity \n')
    # cat('Data points: ', length(all_dat$turbidity_mv),'\n')
    # cat('Range: ', min(all_dat$turbidity_mv),' - ', max(all_dat$turbidity_mv), ' (mv) \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  ROI count \n')
    # cat('Data points: ', length(all_dat$n_roi),'\n')
    # cat('Range: ', min(all_dat$n_roi),' - ', max(all_dat$n_roi), ' (counts) \n')
    # cat('\n')
    # cat('\n')
    # cat(' >>>>  Sigma T \n')
    # cat('Data points: ', length(all_dat$sigmaT),'\n')
    # cat('Range: ', min(all_dat$sigmaT),' - ', max(all_dat$sigmaT), '  \n')
    # cat('QC: ', length(all_dat[all_dat$sigmaT < 22 ]), 'points below twenty-two  \n')
    # cat('QC: ', length(all_dat[all_dat$sigmaT > 28 ]), 'points above twenty-eight  \n')

  })
  
  
  # TODO add QC range
  output$plot <- renderPlot({
    
    if (is.null(binnedData()))
      return(NULL)
    
    vpr_depth_bin <- binnedData()
    
    vpr_plot_profile(taxa_conc_n = vpr_depth_bin, taxa_to_plot = NULL)
    
    
  })
  
  output$plot2 <- renderPlot({
    
    if (is.null(binnedData()))
      return(NULL)
    
    vpr_depth_bin <- binnedData()
    
    
    cmpalf <- cmocean::cmocean('matter')
    cmpal <- cmpalf(n = 100)
    p <- vpr_plot_contour(vpr_depth_bin, var = 'conc_m3', dup = 'strip', method = 'oce', bw = 1, labels = FALSE)+ #plot contours
      scale_fill_gradientn(colours = cmpal)
    #add concentration bubbles and path line
    p + geom_line(data = vpr_depth_bin, aes(x = avg_hr, y = min_depth), col = 'snow4', inherit.aes = FALSE) +
      geom_point(data = vpr_depth_bin, aes(x = avg_hr, y = min_depth, size = conc_m3), alpha = 0.1)+
      ggtitle('Concentration') +
      labs(size = "")+ #size scale null (same as contour scale units)
      scale_size_continuous(range = c(0, 10)) #enlarge bubbles
    
  })
  
  
  output$plot3 <- renderPlot({
    
    vpr_depth_bin <- binnedData()
    all_dat <- datasetInput()
    #temperature
    #interpolate data
    vpr_int <- akima::interp(x = vpr_depth_bin$avg_hr, y = vpr_depth_bin$depth, z = vpr_depth_bin$temperature, duplicate= 'strip')
    
    #plot
    #set consistent x and y limits
    y_limits <- rev(range(vpr_int$y))
    x_limits <- range(vpr_int$x)
    
    cmpalf <- cmocean::cmocean('thermal')
    #make contour plot
    filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
                   color.palette = cmpalf,
                   #color.palette = colorRampPalette(c( "blue", 'red')),
                   ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Temperature',
                   #add anotations
                   plot.axes = {
                     #add bubbles
                     points(vpr_depth_bin$avg_hr, vpr_depth_bin$depth, pch = ".")
                     #add vpr path
                     points(all_dat$avg_hr - min(all_dat$avg_hr), all_dat$depth, type = 'l')
                     #add axes
                     axis(1)
                     axis(2)
                     #add contour lines
                     contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                     #enlarge bubble size based on concentration
                     symbols(vpr_depth_bin$avg_hr, vpr_depth_bin$depth, circles = vpr_depth_bin$conc_m3, 
                             fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                     
                     
                   }) 
    
  })
  
  
  output$plot4 <- renderPlot({
    
    vpr_depth_bin <- binnedData()
    all_dat <- datasetInput()
    vpr_int <- akima::interp(x = vpr_depth_bin$avg_hr, y = vpr_depth_bin$depth, z = vpr_depth_bin$salinity, duplicate = 'strip')
    
    #plot
    #set consistent x and y limits
    y_limits <- rev(range(vpr_int$y))
    x_limits <- range(vpr_int$x)
    
    cmpalf <- cmocean::cmocean('haline')
    #make contour plot
    filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
                   color.palette = cmpalf,
                   #color.palette = colorRampPalette(c( "blue", 'red')),
                   ylim = y_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Salinity',
                   #add annotations
                   plot.axes = {
                     #add bubbles
                     points(vpr_depth_bin$avg_hr, vpr_depth_bin$depth, pch = ".")
                     #add vpr path
                     points(all_dat$avg_hr - min(all_dat$avg_hr), all_dat$depth, type = 'l')
                     #add axes
                     axis(1)
                     axis(2)
                     #add contour lines
                     contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                     #enlarge bubbles based on concentration
                     symbols(vpr_depth_bin$avg_hr, vpr_depth_bin$depth, circles = vpr_depth_bin$conc_m3, 
                             fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                     
                     
                   }) 
    
  })
  
  
  # library(magick)
  # 
  # # Start with placeholder img
  # roi_files <- list.files(file.path(input$basepath, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) ), full.names = TRUE)
  # images <- list()
  # for(i in 1:length(roi_files)){
  # images[[i]] <- image_read(roi_files[i])
  # }
  # 
  # observeEvent(input$upload, {
  #   if (length(input$upload$datapath))
  #     image <<- image_read(input$upload$datapath)
  #   info <- image_info(image)
  #     })
  
  output$rois <- renderImage({
    
    roi_files <- list.files(file.path(input$basepath, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) ), full.names = TRUE)
    
    #imgpath <- file.path(input$basepath, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) )
    
    roi_files <- list.files(pattern = '.tif' )
    
    roi_nums <- vpr_roi(roi_files)
    
  
    list(src = roi_files[1], contentType = 'image/tif', alt = roi_nums[1])
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)
