

#get ROI data

roi_files <- reactive(list.files(file.path(ctd_path(), hour())))
roi_num <- reactive(substr(roi_files(), 5, nchar(roi_files()) - 4))


####STEP 3: Format data####

#make oce ctd object
#easier for plotting
ctd <- reactive(vpr_oce_create(ctd_dat()))


#get roi number that will match ctd time_ms
rois <- reactive(as.numeric(substr(roi_num(), 1, 8)))
#format as table to get frequency
roi_table <- reactive(as.data.frame(table(rois())))



#subset ctd and roi data where time/roi identifier match
ctd_sub <- which(ctd_dat$time_ms %in% rois)
roi_sub <- which(rois %in% ctd_dat$time_ms)
#subset data individually
ctd_dat_sub <- ctd_dat[ctd_sub,]

#EC & KS fix 2019/08/08 due to error producing NA roi numbers
roi_dat_sub <- rois[!duplicated(rois)]
#roi_dat_sub <- rois[ctd_sub]


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
  dplyr::filter(., pressure < max(input$pres_range))


return(all_dat)



#})

renderTable({
  data.frame(dt_tab())
})

```


### Plots
```{r}
#output$all_dat <- renderTable({
# all_dat
# })

#ggplot(dt_tab())+
# geom_point(aes(x = temperature, y = depth))
#reactive(ctd_roi_oce <- vpr_oce_create(all_dat))


#vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce , imageVolume = imageVolume, binSize = binSize, rev = TRUE)

#renderPlot({
# vpr_plot_profile(taxa_conc_n = vpr_depth_bin, taxa_to_plot = NULL)
#})

```
#===============================================

# shiny testing

output$ctd_table <- renderTable({
  
  ctd_fns <- input$ctd_files
  
  if (is.null(ctd_fns))
    return(NULL)
  
  #readLines(ctd_fns$datapath)
  ctd_dat <- vpr_ctd_read(ctd_fns$datapath, station_of_interest = input$station, day = input$day, hour = input$hour)
  
  vpr_oce_create(ctd_dat)
  
})

output$roi_table <- renderTable({
  
  #get ROI data
  
  roi_files <- list.files(file.path(input$basepath, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) ))
  #roi_files <- list.files(file.path(ctd_path, hour))
  roi_num <- substr(roi_files, 5, nchar(roi_files) - 4)
  
  
  
  #get roi number that will match ctd time_ms
  rois <- as.numeric(substr(roi_num, 1, 8))
  #format as table to get frequency
  roi_table <- as.data.frame(table(rois))
  
  
  
})



observeEvent(input$myFile, {
  inFile <- input$myFile
  if (is.null(inFile))
    return()
  
  b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/tif")
  insertUI(
    selector = "#image-container",
    where = "afterBegin",
    ui = img(src = b64, width = 250, height = 250)
  )
})

div(id = "image-container", style = "display:flexbox"))
