page_font = 'Arial'
functional_levels = c("Independent", "Partially Independent", "Totally Dependent")
names(functional_levels) = c( "1", "2","3")

patient_levels = c("ASA 1: Normal healthy patient", "ASA 2: Patient with mild systemic disease", "ASA 3: Patient with severe systemic disease",
                   "ASA 4: Patient with severe systemic disease that is a constant threat to life",
                   "ASA V:	A moribund patient who is not expected to survive without the operation")
names(patient_levels) = c("1", "2", "3", "4", "5")


valid_operations = c(55866, 38571, 50543, 52234, 52235, 52240)
valid_operations_names = c("55866 - Minimally Invasive Radical Prostatectomy",
                           "38571 - Minimally Invasive Radical Prostatectomy with Lymph Node Dissection",
                           "50543 - Minimally Invasive Partial Nephrectomy",
                           "52234 - Transurethral Resection of Bladder Tumor of <2 cm",
                           "52235 - Transurethral Resection of Bladder Tumor of 2-5 cm",
                           "52240 - Transurethral Resection of Bladder Tumor of >5 cm")
names(valid_operations) = valid_operations_names


reverse_valid_operations = valid_operations_names
names(reverse_valid_operations) = valid_operations

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

#Development of Waffle Plot
create_waffle_plot <- function (input_val) {
  i <- 0
  i <- i + 1
  rs <- c()
  cs <- c()
  num <- round(as.numeric(input_val))
  clrs <- c()
  #Dataframe of points in a 10x10 grid (each point assigned a color value)
  for (v in seq(from = min(0), to = max(99))) {
    rs <- c(rs, v %% 10)
    cs <- c(cs, v %/% 10)
    clrs <- c(clrs, (v + 1) <= num)
  }
  df <- data.frame(rs, cs, clrs)
  as.integer(as.logical(df$clrs))
  
  #Plotting and formatting the dataframe
  plot <- ggplot(df, aes(x = rs, y = cs)) +
    geom_point(
      aes(fill = factor(clrs)),
      size = 2.25,
      colour = "black",
      shape = 21,
      stroke = 1,
    ) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("white", "black"))
  
  return(plot)
}

create_plot<- function (percents) {
  arrow_cuttoffs <- c(0.05, 0.25, 0.5, 0.75)
  arrow_vals = c("tiny", "small", "medium", "large", "huge")
  locations <- c('top_', 'middle_', 'bot_')
  mosaic_imgs = c()
  imgs = c()
  for (x in seq(3,1)){
    val = max(which(arrow_cuttoffs <= percents[x]), 0) +1
    img = paste("www/magick_imgs/", locations[x], arrow_vals[val], ".svg", sep = "" )
    imgs = c(imgs, img)
  }
  img1 <- image_read_svg('www/magick_imgs/Background_1.svg')
  arrows = image_mosaic(c(image_read_svg(imgs[1]), 
                          image_read_svg(imgs[2]),
                          image_read_svg(imgs[3])))
  arrows = image_transparent(arrows, 'white')
  img2 <- image_read_svg('www/magick_imgs/bot_small.svg')
  img3 <- image_read_svg('www/magick_imgs/Background_3.svg')
  out <- image_composite(img1, arrows)
  out <- image_composite(out, img3)
  #out <- image_composite(out, fig, offset = "+190+250")
  return(out)
  
}

add_discharge_percents = function(percents, t_image){
  image= t_image
  image = image_annotate(image, percent(as.numeric(percents[1])), font = 'Trebuchet', size = 24, gravity = "northeast", location = "+60+285") 
  image = image_annotate(image, percent(as.numeric(percents[2])),  font = 'Trebuchet', size = 24, gravity = "northeast",  location = "+60+427") 
  image = image_annotate(image, percent(as.numeric(percents[3])), font = 'Trebuchet', size = 24, gravity = "northeast", location = "+60+570") 
  return (image)
}

log_plot_gen = function (lolli_df){
  
  plot1 <- ggplot(lolli_df, aes(x = reorder(lolli_x, lolli_y), y = lolli_y)) +
    geom_segment(
      aes(
        x = reorder(lolli_x, lolli_y),
        xend = reorder(lolli_x, lolli_y),
        y = -4,
        yend = lolli_y
      ),
      color = "skyblue",
      size = 2
    ) +
    geom_point(color = "blue",
               size = 6,
               alpha = 1) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9, color = "#1b1862", hjust = 0.5),
      axis.text.x = element_text(size = 12),
    )  +
    
    scale_y_continuous(
      position = "right",
      limits = c(-4, 0),
      label = function(x) {
        return(paste("1 in", 10 ^ (-1 * x)))
      }
    ) + 
    scale_x_discrete(labels = lolli_df$lolli_x)
  
  return(plot1)
}

create_log_plot <- function(events,top = TRUE){
  i <- 0
  lolli_y <- c()
  lolli_x <- c()
  for (row_num in nrow(events):1) {
    x = events[row_num, ]
    
    i <- i + 1
    str_label = paste(x[[1]],"\n",
                      x[[2]],"\n",
                      paste(format(round(x[[3]], 1), nsmall = 1), "%", sep = ""))
    lolli_x  = c(lolli_x, str_label )
    lolli_y  = c(lolli_y, log10(as.numeric(x[[3]])/100))
  }
  lolli_df = data.frame(lolli_x, lolli_y)
  if (top){
    lolli_df$lolli_y =  rev(lolli_df$lolli_y )
  }
  return (log_plot_gen(lolli_df))

}




create_logarithmic_ggplot <- function(params, image){
  i <- 1
  offsets <- c("+139+175", "+139+522")
  top_plot <- image_graph(width = 587, height = 210, res = 96)
  print(create_log_plot(params$event_data[1:3,], TRUE))
  dev.off()
  image <- image_composite(image, top_plot, offset = "+139+175")
  bot_plot <- image_graph(width = 587, height = 210, res = 96)
  print(create_log_plot(params$event_data[4:6,], FALSE))
  dev.off()
  image <- image_composite(image, bot_plot, offset = "+139+522")
  
  return(image)
}





create_waffle_and_caption = function(input_row){
  value = as.numeric(input_row[[3]]) 
  type = as.character(input_row[[1]])
  av = input_row[[2]]
  plot <- image_graph(width = 90, height = 90, res = 72)
  print(create_waffle_plot(value))
  dev.off()
  white_left = image_blank(width = 53, height= 90, color = "none")
  white_right = image_blank(width = 53, height= 90, color = "none")
  plot = image_append(c(white_left, plot, white_right))
  text = image_blank(width = 196, height= 60, color = "none")
  text = image_annotate(text, type, font = 'Trebuchet', size = 15, gravity = "center",  location = "+0-17", color = "#1b1862")
  text = image_annotate(text, av, font = 'Trebuchet', style = "oblique", size = 12, gravity = "center",  location = "+0+0", color = "#1b1862" )
  text = image_annotate(text, paste(format(round(value, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 14, gravity = "center",  location = "+0+15", color = "#1b1862")
  plot2 = image_append(c(plot, text), stack = TRUE)
  return (plot2)
}


bar_strength_cutoffs <- c(0, .01, .10, .20, .30, .50)
get_bar_img <- function(value) {
  return (paste("www/bar_imgs/bar", max(
    which(bar_strength_cutoffs <= value/100) - 1
  ), ".png", sep = ""))
}
create_bar_and_caption = function(input_row){
  value = as.numeric(input_row[[3]])
  type = as.character(input_row[[1]])
  av = input_row[[2]]
  plot <- image_read(get_bar_img(value))
  plot <- image_scale(plot,"90x90")
  white_left = image_blank(width = 53, height= 90, color = "none")
  white_right = image_blank(width = 53, height= 90, color = "none")
  plot = image_append(c(white_left, plot, white_right))
  text = image_blank(width = 196, height= 60, color = "none")
  text = image_annotate(text, type, font = 'Trebuchet', size = 15, gravity = "center",  location = "+0-17", color = "#1b1862")
  text = image_annotate(text, av, font = 'Trebuchet', style = "oblique", size = 12, gravity = "center",  location = "+0+0", color = "#1b1862")
  text = image_annotate(text, paste(format(round(value, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 14, gravity = "center",  location = "+0+15", color = "#1b1862")
  plot2 = image_append(c(plot, text), stack = TRUE)
  return (plot2)
}
create_waffle_ggplot <- function(params, final_image) {
  
  upperplots = image_append(c(create_waffle_and_caption(params$event_data[1, ]),
                              create_waffle_and_caption(params$event_data[2, ]),
                              create_waffle_and_caption(params$event_data[3, ])
  ))
  bottomplots = image_append(c(create_waffle_and_caption(params$event_data[4,]),
                               create_waffle_and_caption(params$event_data[5,]),
                               create_waffle_and_caption(params$event_data[6,])
  ))
  final_image = image_composite(final_image, upperplots, offset = "+132+238")
  final_image = image_composite(final_image, bottomplots, offset = "+132+543")
  
  return(final_image)
}

create_dot_ggplot <- function(params, final_image) {
  upperplots = image_append(c(create_bar_and_caption(params$event_data[1, ]),
                              create_bar_and_caption(params$event_data[2, ]),
                              create_bar_and_caption(params$event_data[3, ])
  ))
  bottomplots = image_append(c(create_bar_and_caption(params$event_data[4,]),
                               create_bar_and_caption(params$event_data[5,]),
                               create_bar_and_caption(params$event_data[6,])
  ))
  final_image = image_composite(final_image, upperplots, offset = "+132+238")
  final_image = image_composite(final_image, bottomplots, offset = "+132+543")
  
  return(final_image)
}




add_profile <- function(params, basic_image) {
  offset = 25
  if(params$plot_type == 'logarithmic'){
    offset = 0
  }
  info_font_size = 12 
  line_sep = 15
  temp_blank = image_blank(width = image_info(basic_image)[['width']][[1]], height= image_info(basic_image)[['height']][[1]], color = "none")
  
  temp_blank = image_annotate(temp_blank, paste("Procedure:", params$cpt_full),  location = paste("+40+",50 + line_sep * 2 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  temp_blank = image_annotate(temp_blank, paste("Patient Age:",params$age),  location = paste("+40+",50 + line_sep * 3.5 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  temp_blank = image_annotate(temp_blank, paste("ASA Class:", patient_levels[[params$asa_level]]),  location = paste("+600+",50 + line_sep * 4.5 + offset, sep=""), font = page_font,size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  
  temp_blank = image_annotate(temp_blank, paste("Functional Status:",functional_levels[[params$functional_level]]),  location = paste("+330+",50 + line_sep * 4.5 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  temp_blank = image_annotate(temp_blank, paste("Surgeon Specialty:",params$specialty),  location = paste("+330+",50 + line_sep * 3.5 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  emergency = "Non-Emergency"
  if(params$emergency == "1"){
    emergency = "Emergency"
  }
  temp_blank = image_annotate(temp_blank, paste("Emergency Case:",emergency),  location = paste("+40+",50 + line_sep * 4.5 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  in_out = "Inpatient"
  if(params$in_out_patient == "0"){
    in_out = "Outpatient"
  }
  temp_blank = image_annotate(temp_blank, paste("In/Out Patient:",in_out),  location = paste("+600+",50 + line_sep * 3.5 + offset, sep=""), font = page_font, size = info_font_size, gravity = "northwest", color = "#1b1862") 
  
  basic_image = image_composite(basic_image, temp_blank)
  return(basic_image)
}

plot_function_dict <- c(create_waffle_ggplot, create_dot_ggplot, create_logarithmic_ggplot)
names(plot_function_dict) <- c("waffle", "bar", "logarithmic")

generate_final_image <- function(params) {
  shinyjs::logjs(params)
  
  plot_function <- plot_function_dict[[params$plot_type]]

  basic_image <- generate_basic_image(params)
  shinyjs::logjs("Loaded Basic IMG")
  
  final_image <- plot_function(params, basic_image)
  shinyjs::logjs("Loaded Final IMG")
  
  return(final_image)
}



generate_basic_image <- function(params){
  
 

  
  #start with the background (log background vs dot and bar background)
  if(params$plot_type == 'logarithmic'){
    basic_image <- image_read_svg('www/magick_imgs/Background_1.svg')
  }
  else {
    basic_image <- image_read_svg('www/magick_imgs/Background_2.svg')
  } 

  
  #add the appropriately sized arrows to the image
  basic_image <- image_composite(basic_image, image_read_svg(params$top_arrow))
  basic_image <- image_composite(basic_image, image_read_svg(params$middle_arrow))
  basic_image <- image_composite(basic_image, image_read_svg(params$bot_arrow))
  
  #insert cover destinations
  blank_cover <- image_blank(width = 180, height= 450, color = "white")
  basic_image <- image_composite(basic_image, blank_cover, offset = "+825+200")
  cover_image <- image_read_svg('www/magick_imgs/Background_4.svg')
  basic_image <- image_composite(basic_image, cover_image)

  #add the discharge destination risk scores
  temp_blank = image_blank(width = image_info(basic_image)[['width']][[1]], height= image_info(basic_image)[['height']][[1]], color = "none")
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_home, 1), nsmall = 1), "%", sep = ""),  font = 'Trebuchet', size = 24,location = "+890+290")
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_readmit, 1), nsmall = 1), "%", sep = ""), font = 'Trebuchet', size = 24, location = "+890+433") 
  temp_blank = image_annotate(temp_blank, paste(format(round(params$destination_death, 1), nsmall = 1), "%", sep = ""),  font = 'Trebuchet', size = 24, location = "+890+576")
  basic_image = image_composite(basic_image, temp_blank)

  #add the patient information to top right of image
  basic_image = add_profile(params, basic_image)


  
  #insert cover over SURGERY bubble
  cover_image <- image_read_svg('www/magick_imgs/Background_3.svg')
  basic_image <- image_composite(basic_image, cover_image)

  
  return(basic_image)
}

create_all_dot_plots <- function (final_img, input_percents) {
  i <- 1
  offsets <- c("+184+245", "+384+245", "+584+245", "+184+540", "+384+540", "+584+540")
  for(percent in input_percents) {
    img_plot <- image_graph(width = 90, height = 90, res = 96)
    print(create_waffle_plot(percent))
    dev.off()
    final_img <- image_composite(final_img, img_plot, offset = offsets[[i]])
    i <- i + 1
  }
  return(final_img)
}


create_param_list <-
  function(input_df,
           destination_vals,
           risk_inputs,
           waffle_func) {
    arrow_cuttoffs <- c(0.05, 0.25, 0.5, 0.75)
    locations <- c('top', 'middle', 'bot')
    input_df$V3 = input_df$V3 * 100
    arrow_locs <- c()
    params <- list(
      event_data = input_df,
      plot_type =  waffle_func,
      destination_home = destination_vals[1] * 100,
      destination_readmit = destination_vals[2] * 100,
      destination_death = destination_vals[3] * 100,
      cpt = risk_inputs[["cpt"]],
      age = risk_inputs[["age"]],
      asa_level = risk_inputs[["asa"]],
      emergency = risk_inputs[["emer"]],
      functional_level = risk_inputs[["func"]],
      in_out_patient = risk_inputs[["inout"]],
      specialty = risk_inputs[["spec"]],
      cpt_full = reverse_valid_operations[[risk_inputs[["cpt"]]]]
      
      
    )
    
    arrow_cuttoffs <- c(0.05, 0.25, 0.5, 0.75)
    arrow_vals = c("tiny", "small", "medium", "large", "huge")
    locations <- c('top_', 'middle_', 'bot_')
    for (x in seq(3,1)){
      val = max(which(arrow_cuttoffs <= destination_vals[x]), 0) +1
      arrow = paste("www/magick_imgs/", locations[x], arrow_vals[val], ".svg", sep = "" )
      params[[paste(locations[x], "arrow", sep = "")]] <- arrow
    }
    save(params, file="fname.RData")
    return(params)
  }

