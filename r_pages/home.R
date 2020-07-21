output$pageStub <- renderUI(tagList(fluidRow(
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
             id = "to_form", tags$a(
               h4("Next",  class = "btn btn-default btn-info action-button",
                  style = "fontweight:600"),
               href = "?form"
             ) )))
  )))
