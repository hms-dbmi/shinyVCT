output$pageStub <- renderUI(tagList(
  tags$head(
    #import js and css + dependancies
    tags$script(src = "js/jcanvas.min.js"),
    tags$script(src = "js/main.js"),
  ),
  
  #column page
  fluidRow(style = "max-height: 50vh; overflow-y: auto;",
  column(
    12,
    column(12, align = "center", tags$h2(tags$strong("Dot Array")), br()),
    
      offset = 0,
      
      #three plots
      column(4, align = "center", plotOutput("dot_array_1", height = "100px")),
      column(4, align = "center", plotOutput("dot_array_2", height = "100px")),
      column(4, align = "center", plotOutput("dot_array_3", height = "100px")),
      
    

      
      #footers on those three plots - CSS formatting to be done on text\
      column(
        4,
        align = "center",
        tags$h3(events[[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[1]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[1]][3])), style = "margin-top:2px;padding:5px")
      ),
      column(
        4,
        align = "center",
        tags$h3(events[[2]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[2]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[2]][3])), style = "margin-top:2px;padding:5px")
      ),
      column(
        4,
        align = "center",
        tags$h3(events[[3]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[3]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[3]][3])), style = "margin-top:2px;padding:5px")
      ),


      br(),
      column(12, align = "center", tags$h2(tags$strong(
        "Logarithmic Lollipop"
      )), br()),
      
      column(
        12,
        align = "center",
        plotOutput("lollipop", height = "200px", width = "100%")
      ),
      
   
    
    br(),
    column(12, align = "center", tags$h2(tags$strong(
      "Bar Strength"
    )), br()),
    
  

      #three plots
      column(
        4,
        align = "center",
        img(
          src = bars_filled[1],
          height = "100px",
          width = "100px",
          align = "center"
        )
      ),
      column(
        4,
        align = "center",
        img(
          src = bars_filled[2],
          height = "100px",
          width = "100px",
          align = "center"
        )
      ),
      column(
        4,
        align = "center",
        img(
          src = bars_filled[3],
          height = "100px",
          width = "100px",
          align = "center"
        )
      ),
      
  

      
      #footers on those three plots - CSS formatting to be done on text\
      column(
        4,
        align = "center",
        tags$h3(events[[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[1]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[1]][3])), style = "margin-top:2px;padding:5px")
      ),
      column(
        4,
        align = "center",
        tags$h3(events[[2]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[2]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[2]][3])), style = "margin-top:2px;padding:5px")
      ),
      column(
        4,
        align = "center",
        tags$h3(events[[3]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
        tags$h5(events[[3]][2], style = "margin:0px;padding:0px"),
        tags$h6(percent(as.numeric(events[[3]][3])), style = "margin-top:2px;padding:5px")
      ),
 
    
    
    
    column(12, align = "center", tags$h2(
      tags$strong("Select Visualization Method")
    )),
    column(12,
           div(
             style = "padding-left:30%",
             radioButtons(
               "em_case",
               label = NULL,
               choices = list(
                 "Dot Array" = 1,
                 "Bar Strength" = 2,
                 "Logarithmic Lollipop" = 3
               ),
               selected = 2,
             ),
           ),),
    
    
    fluidRow(column(
      12, align = "center", div(id = "generate_pdf", tags$a(
        h4("Generate Report",  class = "btn btn-danger",
           style = "fontweight:600"),
        href = "?graph_view"
      ))
    )),
    
  )
  
  
)))
