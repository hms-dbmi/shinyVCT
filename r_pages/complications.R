output$pageStub <- renderUI(tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  
    column(
      12,
      align = "center",
      
      h4("These are your most likely complications"),
      div(  class="panel panel-default",style= "margin-bottom:20px; margin-top:10px; padding-top:10px; padding-bottom:30px" ,
           
      column(
        4,
        tags$h5(events[[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      column(
        4,
        tags$h5(events[[2]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      column(
        4,
        tags$h5(events[[3]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      br()
    )),
    column(12, align = "center", div(style=  "padding:5px;", tags$h4("Choose three complications that would concern you"))),
    column(
      6,
      checkboxGroupInput(
        "user_chosen_risk_1",
        label = NULL,
        choiceNames = chosen_risk_1,
        choiceValues = seq(1, length(chosen_risk_1)),
        width = '100%'
      ),
    ),  
    column(
      6,
      checkboxGroupInput(
        "user_chosen_risk_2",
        label = NULL,
        choiceNames = chosen_risk_2,
        choiceValues = seq(1, length(chosen_risk_2)),
        width = '100%'
      ),
    ),
  column(12, align = "center", div(
    id = "to_graph", tags$a(
      h4("Next",  class = "btn btn-default btn-info",
         style = "fontweight:600"),
      href = "?graph_view"
    )
  )
  ),
  
))