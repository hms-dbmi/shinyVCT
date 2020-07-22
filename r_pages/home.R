home <- renderUI(tagList(fluidRow(
    column(12, align = "center", div(style=  "padding:5px;", tags$h4("Please Enter Procedure By Name or CPT Code"))),
    
    column(
      12,
      align = "left",
      div( style = "padding-left:20%; padding-bottom:40%",
           selectizeInput(inputId = 'procedure', label = NULL, 
                          choices = c("Please Select Operation" = 0,valid_operations), selected = NULL,  
                          options = list(placeholder = "Please Select Operation", 'persist' = TRUE)),
           

           
           
      )),
    column(12, align = "center",
           shinyjs::hidden(div(
             id = "to_form",   actionButton("home_form_page", "Next")
            )))
  )))

observe({
  if (is.null(input$procedure) ||
      input$procedure == 0) {
    shinyjs::hideElement(id = "to_form")
  }
  else{
    print(input$procedure)
    shinyjs::showElement(id = "to_form")
  }
})

observeEvent(input$home_form_page, {
  if (!is.null(input$procedure) && input$procedure != 0) {
    t_hash = risk_inputs()
    t_hash[['cpt']] = input$procedure
    risk_inputs(t_hash)
    print(risk_inputs()[['cpt']])
  }
  output$pageStub <- form
})