library(shiny)
library(ggplot2)
library(formattable)
library(shinyjs)
library(magick)
library(shinycssloaders)
library(rsconnect)
library(readxl)
library(tidyverse)
library(hash)
library(rsvg)
source("calculation/beta_values.R")
source("img_functions.R")

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
            <a class="nav-link py-3" href="?">Home <span class="sr-only">(current)</span></a>
          </li>
          <li class="nav-item">
            <a class="nav-link py-3" href="?">About</a>
          </li>
          <li class="nav-item">
            <a class="nav-link py-3" href="?">Team</a>
          </li>
        </ul>
      </div>
  </nav>'
  ),
  
  # single-output stub ui
  withSpinner(type = 8, color = "#2BBBAD", uiOutput("uiStub")),
  
  
)

#List of all the operations
valid_operations =  list(
  "55866 - Minimally Invasive Radical Prostatectomy" = 55866,
  "38571 - Minimally Invasive Radical Prostatectomy with Lymph Node Dissection" = 38571,
  "50543 - Minimally Invasive Partial Nephrectomy" = 50543,
  "52234 - Transurethral Resection of Bladder Tumor of <2 cm" = 52234,
  "52235 - Transurethral Resection of Bladder Tumor of 2-5 cm" = 52235,
  "52240 - Transurethral Resection of Bladder Tumor of >5 cm" = 52240
)

#List of all the specialities
valid_specialities =  list(
  "Please Select One" = 0,
  "Gynecology" = "Gynecology",
  "Neurosurgery" = "Neurosurgery",
  "Orthopedics" = "Orthopedics",
  "Otolaryngology (ENT)" = "Otolaryngology (ENT)",
  "Plastics" = "Plastics",
  "Thoracic" = "Thoracic",
  "Urology" = "Urology",
  "Vascular" = "Vascular"
)


#the bar_strength values
bar_strength_cutoffs <- c(0, .01, .10, .20, .30, .50)


addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}


server <- function(input, output, session) {
  risk_inputs <- reactiveVal({
    risk_inputs = hash()
    risk_inputs[["cpt"]] = NULL
    risk_inputs[["age"]] = NULL
    risk_inputs[["asa"]] = NULL
    risk_inputs[["func"]] = NULL
    risk_inputs[["inout"]] = NULL
    risk_inputs[["emer"]] = NULL
    risk_inputs[["spec"]] = NULL
    risk_inputs
  })
  events_df = reactiveVal({0})
  chosen_risk  = reactiveVal({0})
  high_risk = reactiveVal({0})
  selected_events_df = reactiveVal({0})
  discharge_data = reactiveVal({0})
  
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
    "r_pages/graph_view.R",
    "r_pages/home.R",
    "r_pages/form.R",
    "r_pages/complications_single_col.R",
    "r_pages/error.R"
    
  )
  for (x in validFiles) {
    source(x, local = TRUE)
  }
  

  output$pageStub <- home



  

  output$downloadDot <- downloadHandler(
    filename = "rendered_report.pdf",
    content = function(file) {
      res <-
        image_write(image_convert(generate_final_image(
          create_param_list(selected_events_df(),
                            discharge_data(),
                            risk_inputs(),
                            "waffle")
        ), "PDF"), "hold.pdf")
      
      file.copy(res, file)
    }
  )
  output$downloadLog <- downloadHandler(
    filename = "rendered_report.pdf",
    content = function(file) {
      res <-
        image_write(image_convert(generate_final_image(
          create_param_list(
            selected_events_df(),
            discharge_data(),
            risk_inputs(),
            "logarithmic"
          )
        ), "PDF"), "hold.pdf")
      
      file.copy(res, file)
    }
  )
  output$downloadBar <- downloadHandler(
    filename = "rendered_report.pdf",
    content = function(file) {
      res <-
        image_write(image_convert(generate_final_image(
          create_param_list(selected_events_df(),
                            discharge_data(),
                            risk_inputs(),
                            "bar")
        ), "PDF"), "hold.pdf")
      
      file.copy(res, file)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)