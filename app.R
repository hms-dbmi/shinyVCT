library(shiny)
library(ggplot2)
library(formattable)
library(shinyjs)
library(rmarkdown)
library(Cairo)
library(png)
library(pdftools)
library(magick)

library(shinycssloaders)
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

#all inputs required to create a Risk Dataframe
risk_inputs = hash()
risk_inputs[["cpt"]] = NULL
risk_inputs[["age"]] = NULL
risk_inputs[["asa"]] = NULL
risk_inputs[["func"]] = NULL
risk_inputs[["inout"]] = NULL
risk_inputs[["emer"]] = NULL
risk_inputs[["spec"]] = NULL




#the bar_strength values
bar_strength_cutoffs <- c(0, .01, .10, .20, .30, .50)

#An inital df for all the events
events_df = NULL
chosen_risk  = NULL
high_risk = NULL
selected_events_df = NULL
discharge_data = NULL



addline_format <- function(x, ...) {
  gsub('\\s', '\n', x)
}

#Developing DataFrames to create a dot array







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
        ), )))
  
  
  
  
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
  
  if (fname == "r_pages/graph_view.R") {
    if (is.null(risk_inputs[["cpt"]]) || is.null(risk_inputs[["asa"]])) {
      fname = "?home"
    }
  }
  
  if (fname == "r_pages/complications_single_col.R") {
    if (is.null(risk_inputs[["cpt"]]) || is.null(risk_inputs[["asa"]])) {
      fname = "?home"
    } else{
      events_df <<- make_risk_df(
        risk_inputs[["cpt"]],
        risk_inputs[["age"]],
        risk_inputs[["asa"]],
        risk_inputs[["emer"]],
        risk_inputs[["func"]],
        risk_inputs[["inout"]],
        risk_inputs[["spec"]]
      )
      discharge_data <<- make_discharge_list(
        risk_inputs[["cpt"]],
        risk_inputs[["age"]],
        risk_inputs[["asa"]],
        risk_inputs[["emer"]],
        risk_inputs[["func"]],
        risk_inputs[["inout"]],
        risk_inputs[["spec"]]
      )
      print(discharge_data)
      chosen_risk  <<-
        as.vector(events_df[seq(4, nrow(events_df)), ][['V1']])
      high_risk <<- as.vector(events_df[seq(1, 3), ][['V1']])
    }
  }
  
  # is that one of our files?
  if (!fname %in% validFiles) {
    output$pageStub <-
      renderUI(tagList(# 404 if no file with that name
        fluidRow(column(
          12,
          HTML(
            "<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
            "menu above to navigate to the page you were looking for.</p>"
          ),
          column(12, align = "center", div(
            id = "to_graph", tags$a(
              h4("Return to Home",  class = "btn btn-default btn-info",
                 style = "fontweight:600"),
              href = "?home"
            )
          )
          )
        ))))
    return()    # to prevent a "file not found" error on the next line after a 404 error
  }
  source(fname, local = TRUE)
  
  
  
  output$dot <- renderImage({
    params = create_param_list(selected_events_df,
                      discharge_data,
                      risk_inputs,
                      "waffle"
    )
    final_plot <- generate_final_image(params)

        tmpfile <- final_plot %>%
      image_write(tempfile(fileext='svg'), format = 'svg')
    
    # Return a list
    list(
      src = tmpfile, contentType = "image/svg+xml",
      
      width = "100%",
      height = "auto"
    )
  })
  output$log <- renderImage({
    params = create_param_list(selected_events_df,
                               discharge_data,
                               risk_inputs,
                               "logarithmic"
    )
    final_plot <- generate_final_image(params)
    
    tmpfile <- final_plot %>%
      image_write(tempfile(fileext='svg'), format = 'svg')
    
    # Return a list
    list(
      src = tmpfile, contentType = "image/svg+xml",
      
      width = "100%",
      height = "auto"
    )
  })
  output$bar <- renderImage({
    params = create_param_list(selected_events_df,
                               discharge_data,
                               risk_inputs,
                               "bar"
    )
    final_plot <- generate_final_image(params)
    
    tmpfile <- final_plot %>%
      image_write(tempfile(fileext='svg'), format = 'svg')
    
    # Return a list
    list(
      src = tmpfile, contentType = "image/svg+xml",
      
      width = "100%",
      height = "auto"
    )
  })
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
      create_lollipop(events_df[1:3,])
    }, height = 200)
  output$lollipop_small <-
    renderPlot({
      create_lollipop(events_df[4:6,])
    }, height = 75)
  output$lollipop_secondary <-
    renderPlot({
      create_lollipop(events_df[4:6,])
    }, height = 200)
  
  
  observe({
    if (is.null(input$asa_status) ||
        input$asa_status == 0 ||
        input$surg_spec == 0) {
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
      inputs_of_interest = high_risk
      if (length(x) + length(y) == 3) {
        for (index in x) {
          inputs_of_interest = c(inputs_of_interest, chosen_risk[as.numeric(index)])
        }
      } else{
        inputs_of_interest = c(inputs_of_interest,
                               chosen_risk[1],
                               chosen_risk[2],
                               chosen_risk[3])
      }
      selected_events_df <<-
        events_df[events_df$V1 %in% inputs_of_interest, ]
      shinyjs::showElement(id = "to_graph")
    } else{
      shinyjs::hideElement(id = "to_graph")
    }
  })
  
  observe({
    if (!is.null(input$procedure)) {
      risk_inputs[["cpt"]] <<- input$procedure
    }
    if (!is.null(input$asa_status) && input$asa_status != 0) {
      risk_inputs[["age"]] <<- input$age
      risk_inputs[["asa"]] <<- input$asa_status
      risk_inputs[["func"]] <<- input$func_status
      risk_inputs[["inout"]] <<- input$oper
      risk_inputs[["emer"]] <<- input$em_case
      risk_inputs[["spec"]] <<- input$surg_spec
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = "rendered_report.pdf",
    content = function(file) {
      res <- rmarkdown::render(
        "pdf_creation/download_handler.Rmd",
        params = create_param_list(
          selected_events_df,
          discharge_data,
          risk_inputs,
          create_waffle_plot
        )
      )
      file.rename(res, file)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)