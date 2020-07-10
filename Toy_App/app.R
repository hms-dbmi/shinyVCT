library(shiny)
library(htmltools)
library(dplyr)
library(magrittr)
library(magick)


library(ggplot2)
create_waffle_plot <- function (input_val) {
  i <- 0
  
  i <- i + 1
  rs <- c()
  cs <- c()
  num <- as.numeric(input_val)
  
  clrs <- c()
  for (v in seq(from = min(0), to = max(99))) {
    rs <- c(rs, v %% 10)
    cs <- c(cs, v %/% 10)
    clrs <- c(clrs, (v + 1) <= num)
  }
  df <- data.frame(rs, cs, clrs)
  as.integer(as.logical(df$clrs))
  
  
  plot <- ggplot(df, aes(x = rs, y = cs)) +
    geom_point(
      aes(fill = factor(clrs)),
      size = 19,
      colour = "black",
      shape = 21,
      stroke = 1,
    ) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("white", "black"))
  
  #theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
  return(plot)
}
create_plot <- function () {
  img1 <- image_read_svg('www/Background_1.svg')
  img2 <- image_read_svg('www/bot_small.svg')
  img3 <- image_read_svg('www/Background_3.svg')
  
  out <- image_composite(img1, img2)
  out <- image_composite(out, img3)
  #out <- image_composite(out, fig, offset = "+190+250")
  
  #out <- image_annotate(final_plot1, "Renal (Kidney) Infection", size = 30, color = "purple")
  
return(out)}
  

ui <- shinyUI(
  mainPanel(
    sidebarLayout(
      
      sidebarPanel(
        downloadButton('downloadImage', 'Download modified image')
      ),
        
    imageOutput("myImage")
  ))
  
)

server <- shinyServer(function(input, output, clientData) {
  
  
  
  img1 <- image_read_svg('www/Background_1.svg')
  img2 <- image_read_svg('www/Background_3.svg')
  img3 <- image_read_svg('www/bot_small.svg')
  output$myImage <- renderImage({
    
    width  <- clientData$output_plot_width
    height <- clientData$output_plot_height
    mysvgwidth <- width/96
    mysvgheight <- height/96
    
    plot2 <- image_graph(width = 100, height = 100, res = 72)
    create_waffle_plot(50)
    dev.off()
    #fig <- image_flatten(fig)
    #all_images <- c(img1, img3, img2)
    #out <- image_composite(all_images, fig, offset = "+190+250")
    #img <- image_composite(out, fig, offset = "+190+250")
    # A temp file to save the output.
    # This file will be removed later by renderImage
    #outfile <- tempfile(fileext='.jpg')
    print('hello')
    plot2 <- create_waffle_plot(50)
    plot_temp_file <- tempfile(fileext='png')
    #ggsave(plot=plot2, filename=plot_temp_file, device='png')
    final_plot <- create_plot()
    #print(plot_temp_file)
    #plot2 <- image_read(plot_temp_file)
    #plot2 <- image_resize(plot2, "100x100")
    final_plot1 <- image_composite(final_plot, plot2, offset = "+190+250")
    #final_plot1 <- image_annotate(final_plot1, "Renal (Kidney) Infection", size = 30, color = "purple")
    
    tmpfile <- final_plot1 %>%
      image_write(tempfile(fileext='svg'), format = 'svg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/svg+xml")
  })
  output$downloadImage <- downloadHandler(
    filename = "Modified_image.jpeg",
    contentType = "image/jpeg",
    content = function(file) {
      ## copy the file from the updated image location to the final download location
      plot2 <- create_waffle_plot(50)
      plot_temp_file <- tempfile(fileext='png')
      ggsave(plot=plot2, filename=plot_temp_file, device='png')
      final_plot <- create_plot()
      print(plot_temp_file)
      plot2 <- image_read(plot_temp_file)
      plot2 <- image_resize(plot2, "100x100")
      final_plot1 <- image_composite(final_plot, plot2, offset = "+190+250")
      tmpfile <- final_plot1 %>%
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      file.copy(tmpfile, file)
    }
  ) 
})
shinyApp(ui = ui, server = server)