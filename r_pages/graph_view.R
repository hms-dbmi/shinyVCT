graph_view <- renderUI(tagList(#column page
  column(
    12,
    
    #DOT ARRAY TAB PANNEL
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Dot Array",
        br(),
        column(
          12,
          align = "center",
          style = "margin-top:-10px;padding:0px",
          tags$style(type = "text/css",
                     #"#image img {width: auto; max-height: 500px; height: auto}"
                  ),
                     withSpinner(type = 8, color = "#2BBBAD", imageOutput("dot")),
          
          
          br(),
          br(),
          br(),
          br(),
          
          fluidRow(
            style = "overflow-y: auto;",
            column(6, align = "center", div(
              actionButton("graph_complications_page", "Back")
            )),
            column(
              6,
              align = "center",
              downloadButton('downloadDot', 'Download PDF Report'),
            )
          ),
          
          
        )),
      tabPanel(
        "Logarithmic Lollipop",
        br(),
        column(
          12,
          align = "center",
          style = "margin-top:-10px;padding:0px",
          tags$style(type = "text/css",
                     #"#image img {width: auto; max-height: 500px; height: auto}"
          ),
                     withSpinner(type = 8, color = "#2BBBAD", imageOutput("log")),
         
          
          br(),
          br(),
          br(),
          br(),
          fluidRow(
            style = "overflow-y: auto;",
            column(6, align = "center", div(
              actionButton("graph_complications_page", "Back")
            )),
            column(
              6,
              align = "center",
              downloadButton('downloadLog', 'Download PDF Report'),
            )
          ),
          
        )),
      tabPanel(
        "Bar Strength",
        br(),
        column(
          12,
          align = "center",
          style = "margin-top:-10px;padding:0px",
          tags$style(type = "text/css",
                     #"#image img {width: auto; max-height: 500px; height: auto}"
          ),
                     withSpinner(type = 8, color = "#2BBBAD", imageOutput("bar")),
        
          br(),
          br(),
          br(),
          br(),
          fluidRow(
            style = "overflow-y: auto;",
            column(6, align = "center", div(
              actionButton("graph_complications_page", "Back")
            )),
            column(
              6,
              align = "center",
              downloadButton('downloadBar', 'Download PDF Report'),
            )
          ),
        )
      )
      
      
    )),
    
    
    
    
    
  ))

output$dot <- renderImage({
  print(selected_events_df())
  print(events_df()[["V3"]])
  
  print(discharge_data())
  print(risk_inputs())
  params = create_param_list(selected_events_df(),
                             discharge_data(),
                             risk_inputs(),
                             "waffle")
  shinyjs::logjs(params)
  final_plot <- generate_final_image(params)
  shinyjs::logjs(final_plot)
  
  tmpfile <- final_plot %>%
    image_write(tempfile(fileext = 'svg'), format = 'svg')
  
  # Return a list
  list(
    src = tmpfile,
    contentType = "image/svg+xml",
    width = "100%",
    height = "auto"
  )
})
output$log <- renderImage({
  params = create_param_list(selected_events_df(),
                             discharge_data(),
                             risk_inputs(),
                             "logarithmic")
  final_plot <- generate_final_image(params)
  
  tmpfile <- final_plot %>%
    image_write(tempfile(fileext = 'svg'), format = 'svg')
  
  # Return a list
  list(
    src = tmpfile,
    contentType = "image/svg+xml",
    
    width = "100%",
    height = "auto"
  )
})
output$bar <- renderImage({
  params = create_param_list(selected_events_df(),
                             discharge_data(),
                             risk_inputs(),
                             "bar")
  final_plot <- generate_final_image(params)
  
  tmpfile <- final_plot %>%
    image_write(tempfile(fileext = 'svg'), format = 'svg')
  
  # Return a list
  list(
    src = tmpfile,
    contentType = "image/svg+xml",
    
    width = "100%",
    height = "auto"
  )
})


observeEvent(input$graph_complications_page, {
  output$pageStub <- complications
})