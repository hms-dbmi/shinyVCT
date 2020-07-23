complications <- renderUI(tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  
  column(
    12,
    align = "center",
    
    h4("These are your most likely complications"),
    div(
      class = "panel panel-default",
      style = "margin-bottom:20px; margin-top:10px; padding-top:10px; padding-bottom:30px" ,
      
      column(
        4,
        tags$h5(high_risk()[1], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      column(
        4,
        tags$h5(high_risk()[2], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      column(
        4,
        tags$h5(high_risk()[3], style = "margin-bottom:2px; margin-top: 5px; padding:0px")
      ),
      br()
    )
  ),
  column(12, align = "center", div(
    style =  "padding:5px;",
    tags$h4("Choose three complications that would concern you")
  )),
  column(12,
         div(
           style = "padding-left:30%",
           checkboxGroupInput(
             "user_chosen_risk_1",
             label = NULL,
             choiceNames = chosen_risk(),
             choiceValues = seq(1, length(chosen_risk())),
             width = '100%'
           ),
         ), br(), br(), br()),
  column(6, align = "center", div(
    actionButton("complications_form_page", "Back")
  )),
  column(6, align = "center", div(
    id = "to_graph", actionButton("complications_graph_page", "Next")
  )),
  
))
return_df = function(df){
  return(df)
}
observe({
  x = input$user_chosen_risk_1
  y = input$user_chosen_risk_2
  if (!is.null(events_df()) && events_df() != 0 ) {
    if ((length(x) + length(y)) == 3 ||
        (length(x) + length(y)) == 0) {
      inputs_of_interest = high_risk()
      if (length(x) + length(y) == 3) {
        for (index in x) {
          inputs_of_interest = c(inputs_of_interest, chosen_risk()[as.numeric(index)])
        }
      } else{
        inputs_of_interest = c(inputs_of_interest,
                               chosen_risk()[1],
                               chosen_risk()[2],
                               chosen_risk()[3])
      }
      t_df = events_df()[events_df()$V1 %in% inputs_of_interest, ]
      print(t_df)
      print(selected_events_df())
      selected_events_df(return_df(t_df))
      
      shinyjs::showElement(id = "to_graph")
    } else{
      shinyjs::hideElement(id = "to_graph")
    }
  } else{
    shinyjs::hideElement(id = "to_graph")
  }
})


observeEvent(input$complications_graph_page, {
  if (is.null(risk_inputs()[["cpt"]]) ||
      is.null(risk_inputs()[["asa"]])) {
    output$pageStub <- error
  } else{
    output$pageStub <- graph_view
  }
})
observeEvent(input$complications_form_page, {
  output$pageStub <- form
})