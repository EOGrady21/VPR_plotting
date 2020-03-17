lightbox_gallery <- function(df, gallery, display = 'block'){
  
  tags$div(style = sprintf('display: %s;', display),
           tagList(tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "lightbox-2.10.0/lightbox.min.css"),
             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
           ),
           tags$div(class = 'card-deck',
                    lapply(seq_len(nrow(df)), function(i){
                      tags$div(`data-type`="template", class = 'card',
                               tags$a(id = df$key[i],
                                      href = paste0('img/', df$src[i]),
                                      `data-lightbox` = gallery, # this identifies gallery group
                                      `data-title` = df$date[i],
                                      tags$img(class = 'card-img-top',
                                               src = paste0("img/", df$src[i]),
                                               width = '80px',
                                               height = 'auto'))
                      )
                    })
           ),
           includeScript("www/lightbox-2.10.0/lightbox.min.js")
           ))
  
}

photoswipe_gallery <- function(df, gallery, display = 'block'){
  
  tags$div(style = sprintf('display: %s;', display),
           tagList(
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "photoswipe-4.1.2/photoswipe.css"),
               tags$link(rel = "stylesheet", type = "text/css", href = "photoswipe-4.1.2/default-skin/default-skin.css"),
               tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
               tags$script(src = 'photoswipe-4.1.2/photoswipe.min.js'),
               tags$script(src = 'photoswipe-4.1.2/photoswipe-ui-default.min.js')
             ),
             tags$div(class="my-gallery card-deck", 
                      `itemscope itemtype`="http://schema.org/ImageGallery",
                      htmltools::htmlTemplate('www/photoswipe-4.1.2/template.html'),
                      lapply(seq_len(nrow(df)), function(i){
                        tags$div(`data-type`="template", class = 'card',
                                 
                                 tags$figure(itemprop="associatedMedia", `itemscope itemtype`="http://schema.org/ImageObject",
                                             tags$a(id = df$key[i],
                                                    href = paste0('img/', df$src[i]),
                                                    itemprop="contentUrl", `data-size`="800x800",
                                                    tags$img(class = 'card-img-top',
                                                             src = paste0("img/", df$src[i]),
                                                             itemprop="thumbnail", 
                                                             alt="Image description",
                                                             width = '80px',
                                                             height = '80px')),
                                             tags$figcaption(itemprop="caption description"#, caption under thumbnail
                                                             #df$src[i])
                                             )
                                 )    
                        )
                      })
             ),
             includeScript("www/photoswipe-4.1.2/photoswipe_run.js")
           ))
  
}