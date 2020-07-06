output$pageStub <- renderUI(tagList(
  
  tags$head(
    #import js and css + dependancies
    tags$script(src = "js/jcanvas.min.js"),
    tags$script(src = "js/main.js"),
  ),
  
  #column page
  column(
    12,
    fluidRow(
      offset = 0,
      
      #three plots
      column(6, align = "center", tags$h2(tags$strong("Dot Array")),
             column(12, plotOutput("dot_array_1", height = "100px")),
             column(12,align = "center",
                    tags$h3(events[[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                    tags$h5(events[[1]][2], style = "margin:0px;padding:0px"),
                    tags$h6(percent(as.numeric(events[[1]][3])), style = "margin-top:2px;padding:5px")
             ) ),
      column(6, align = "center", tags$h2(tags$strong("Bar Strength")),
            column(12, img(src=bars_filled[1], height="100px", width="100px", align="center")),
            column(12,align = "center",
                   tags$h3(events[[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                   tags$h5(events[[1]][2], style = "margin:0px;padding:0px"),
                   tags$h6(percent(as.numeric(events[[1]][3])), style = "margin-top:2px;padding:5px")
            ) )
      
    ),
    fluidRow( #column(2,
      column(12, align = "center",tags$h2(tags$strong("Logarithmic Lollipop"))),
      column(12, align = "center", plotOutput("lollipop_small", height = "100px", width = "100%")),
    ),
    column(12, align = "center", tags$h2(tags$strong("Select Visualization Method"))),
    column(
      12,
      div( style = "padding-left:30%",
           radioButtons(
             "em_case",
             label = NULL,
             choices = list("Dot Array" = 1, "Bar Strength" = 2,"Logarithmic Lollipop" = 3 ),
             selected = 2,
           ),
      ),
    ),  
   
    
    fluidRow(column(12, align = "center", div(
      id = "generate_pdf", tags$a(
        h4("Generate Report",  class = "btn btn-danger",
           style = "fontweight:600"),
        href = "?graph_view"
      )
    ))
    
    
    ),
    
  )
  
  
))
