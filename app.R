library(shiny)
library(ggplot2)
library(formattable)
library(shinyjs)
library(rmarkdown)
library(Cairo)
library(png)
library(pdftools)
library(shinycssloaders)



# put a message in console or server log; note this happens only when the app is started!
cat("uiStub application started...\n")

#Creation of a Temporary Folder
temp_folder <- tempfile()

#Cleaning of Temp Folder
onStop(function() {
  cat("Removing Temporary Files and Folders\n")
  unlink(temp_folder, recursive = TRUE)
})
dir.create(temp_folder)


#Basic UI
ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/css/style.css"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.8.2/css/all.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.3.1/css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/mdbootstrap/4.8.11/css/mdb.min.css"),
    tags$script(src = "js/main.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    
  ),
  
  #The HTML NavBar
  HTML(
    '<nav class="navbar navbar-expand navbar-dark default-color">
      <span class="navbar-text white-text my-0">
          <h4> Data Driven Visual Consent </h4>
      </span>
      <div class="collapse navbar-collapse navbar-right text-center" id="myCustomNavbar">
        <ul class="navbar-nav navbar-right navbar-nav ml-auto">
          <li class="nav-item active">
            <a class="nav-link py-3" href="?home">Home <span class="sr-only">(current)</span></a>
          </li>
          <li class="nav-item">
            <a class="nav-link py-3" href="?home">About</a>
          </li>
          <li class="nav-item">
            <a class="nav-link py-3" href="?">Team</a>
          </li>
        </ul>
      </div>
  </nav>'
  ),
  
  # single-output stub ui
  withSpinner(type = 7, color = "#2BBBAD", uiOutput("uiStub")),
  
  #Download Button (Will be reposistioned)
  downloadButton('downloadData', 'Download data'),
)

#List of all the operations
valid_operations =  list(
  "55866 - Minimally Invasive Radical Prostatectomy" = 1,
  "38571 - Minimally Invasive Radical Prostatectomy with Lymph Node Dissection" = 2,
  "50543 - Minimally Invasive Partial Nephrectomy" = 3,
  "52234 - Transurethral Resection of Bladder Tumor of <2 cm" = 4,
  "52235 - Transurethral Resection of Bladder Tumor of 2-5 cm" = 5,
  "52240 - Transurethral Resection of Bladder Tumor of >5 cm" = 6
)


#the bar_strength values
bar_strength_cutoffs <- c(0, .01, .10, .20, .30, .50)

#An inital df for all the events
events <- c()
events[[1]] <- c("Pulmonary Complications", "Above Average", 0.125)
events[[2]] <- c("Mortality ", "Above Average", 0.091)
events[[3]] <- c("Cardiac Complications", "Average", 0.068)
events[[4]] <-
  c("Renal (Kidney) complications", "Above Average", 0.03)
events[[5]] <- c("Stroke complications", "Average", 0.028)
events[[6]] <- c("Urinary Tract Infection", "Below Average", 0.002)
events[[7]] <-
  c("Bleeding or transfusion Complications", "Average", 0.001)
events[[8]] <- c("Risk of Infection", "Average", 0.001)
events[[9]] <- c("Venous thromboembolism", "Average", 0.001)
events_df = as.data.frame(do.call(rbind, events))
events_df[["V3"]] = as.numeric(as.character(events_df[["V3"]]))

chosen_risk  = as.vector(events_df[seq(4, nrow(events_df)),][['V1']])
chosen_risk_1 = as.list(split(chosen_risk, c(1, 2))[1])[[1]]
chosen_risk_2  = as.list(split(chosen_risk, c(1, 2))[2])[[1]]
high_risk = as.vector(events_df[seq(1, 3),][['V1']])


create_param_list <- function(input_df, waffle_func) {
  arrow_cuttoffs <- c(0.05, 0.25, 0.5, 0.75)
  destination_vals = c(0.9, 0.09, 0.01)
  locations <- c('top', 'middle', 'bot')
  input_df$V3 = input_df$V3 * 100
  arrow_locs <- c()
  params <- list(event_data = input_df,
                 plot_func =  waffle_func)
  for (i in 1:3) {
    arrow = paste("New Images/", locations[i], max(which(arrow_cuttoffs <= destination_vals[i]), 0) + 1,
                  ".png", sep = "")
    params[[paste(locations[i], "_arrow", sep = "")]] <- arrow
  }
  print(params)
  return(params)
}

#Development of Waffle Plot
create_waffle_plot <- function (input_val) {
  i <- 0
  i <- i + 1
  rs <- c()
  cs <- c()
  num <- as.numeric(input_val)
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
      size = 1,
      colour = "black",
      shape = 21,
      stroke = 1,
    ) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("white", "black"))
  
  return(plot)
}

addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}

#Developing DataFrames to create a dot array



#development of the lollipop graph

create_lollipop <- function (events_df) {
  lolli_y <- c()
  lolli_x <- c()
  for (i in 1:nrow(events_df)) {
    row <- events_df[i, ]
    lolli_x  = c(lolli_x, as.character(row[["V1"]]))
    lolli_y  = c(lolli_y, log10(as.numeric(row[["V3"]])))
    
  }
  lolli_df = data.frame(lolli_x, lolli_y)
  print(lolli_x)
  ggplot(lolli_df, aes(x = lolli_x, y = lolli_y)) +
    geom_segment(
      aes(
        x = lolli_x,
        xend = lolli_x,
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
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
    )  +
    
    scale_y_continuous(
      position = "right",
      limits = c(-4, 0),
      label = function(x) {
        return(paste("1 in", 10 ^ (-1 * x)))
      }
    ) +
    scale_x_discrete(labels = addline_format(lolli_x))
}

bars_filled = c()
for (x in events) {
  bars_filled = c(bars_filled, paste("bar_imgs/bar", max(
    which(bar_strength_cutoffs <= as.numeric(x[3])) - 1
  ), ".png", sep = ""))
}

get_bar_img <- function(value) {
  return (paste("bar_imgs/bar", max(
    which(bar_strength_cutoffs <= as.numeric(value)) - 1
  ), ".png", sep = ""))
}


server <- function(input, output, session) {
  cat("Session started.\n") # this prints when a session starts
  
  
  onSessionEnded(function() {
    cat("Session ended.\n\n")
  })  # this prints when a session ends
  
  
  
  # build menu; same on all pages
  output$uiStub <-
    renderUI(tagList(# a single-output stub ui basically lets you
      fluidPage(# move the ui into the server function
        fluidRow(
          column(12,
                 uiOutput("pageStub"))
        ),)))
  
  
  
  
  # load server code for page specified in URL
  validFiles = c(
    "r_pages/complications.R",
    "r_pages/graph_view.R",
    "r_pages/home.R",
    "r_pages/form.R",
    "r_pages/complications_single_col.R",
    "r_pages/alternative_view1.R",
    "r_pages/alternative_view2.R",
    "r_pages/alternative_view3.R",
    "r_pages/alternative_view4.R"
  )
  
  
  fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
  # blank means home page
  if (nchar(fname) == 0) {
    fname = "?home"
  }
  fname = paste0("r_pages/", substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
  # print the URL for this session
  cat(paste0("Session filename: ", fname, ".\n"))
  
  if(fname == "r_pages/graph_view.R" ){
    rmarkdown::render("pdf_creation/download_handler.Rmd", 
                      output_dir = temp_folder,
                      output_file = "rendered_report.pdf", 
                      params = create_param_list(events_df, create_waffle_plot)
    )

    bitmap <- pdf_render_page(paste(temp_folder,"/rendered_report.pdf", sep = "") , page = 1, dpi = 300)
    png::writePNG(bitmap, paste(temp_folder,"/rendered_pic.png", sep = ""))
    #png::writePNG(bitmap,"temp.png")
    print(list.files(path =temp_folder))
    
  }
  
  output$img1 <- renderImage({
    list(src = paste(temp_folder,"/rendered_pic.png", sep = ""), width = "100%", height= "auto")
    
  })
  # is that one of our files?
  if (!fname %in% validFiles) {
    output$pageStub <-
      renderUI(tagList(# 404 if no file with that name
        fluidRow(column(
          5,
          HTML(
            "<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
            "menu above to navigate to the page you were looking for.</p>"
          )
        ))))
    return()    # to prevent a "file not found" error on the next line after a 404 error
  }
  source(fname, local = TRUE)
  
  
  
  print(create_waffle_plot(events_df[["V3"]][1]))
  output$dot_array_1 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][1]) * 100)
    }, height = 100, width = 100)
  output$dot_array_2 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][2]) * 100)
    }, height = 100, width = 100)
  output$dot_array_3 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][3]) * 100)
    }, height = 100, width = 100)
  output$dot_array_4 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][4]) * 100)
    }, height = 100, width = 100)
  output$dot_array_5 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][5]) * 100)
    }, height = 100, width = 100)
  output$dot_array_6 <-
    renderPlot({
      create_waffle_plot(as.numeric(events_df[["V3"]][6]) * 100)
    }, height = 100, width = 100)
  output$lollipop <-
    renderPlot({
      create_lollipop(events_df[1:3, ])
    }, height = 200)
  output$lollipop_small <-
    renderPlot({
      create_lollipop(events_df[4:6, ])
    }, height = 75)
  output$lollipop_secondary <-
    renderPlot({
      create_lollipop(events_df[4:6, ])
    }, height = 200)
  
  
  observe({
    if (is.null(input$asa_status) ||
        input$asa_status == 1 ||
        input$surg_spec == 1) {
      shinyjs::hideElement(id = "to_user")
    }
    else{
      shinyjs::showElement(id = "to_user")
    }
  })
  observe({
    x = input$user_chosen_risk_1
    y = input$user_chosen_risk_2
    if ((length(x) + length(y)) == 3 ||
        (length(x) + length(y)) == 0) {
      shinyjs::showElement(id = "to_graph")
    } else{
      shinyjs::hideElement(id = "to_graph")
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = "rendered_report.pdf",
    
    content = function(file) {
      res <- rmarkdown::render("pdf_creation/download_handler.Rmd",
                               params = create_param_list(events_df, create_waffle_plot))
      file.rename(res, file)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)