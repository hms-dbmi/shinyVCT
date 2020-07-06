output$pageStub <- renderUI(tagList(fluidRow(
    column(12, align = "center", div(style=  "padding:5px;", tags$h4("Please Enter Procedure By Name or CPT Code"))),
    
    column(
      12,
      align = "left",
      div( style = "padding-left:20%",
           radioButtons(
             "procedure",
             width = "75%",
             label= NULL,
             choices = valid_operations,
             selected = 1
           ),
           
           
      )),
    column(12, align = "center",
           div(
             id = "to_form", tags$a(
               h4("Next",  class = "btn btn-default btn-info action-button",
                  style = "fontweight:600"),
               href = "?form"
             ) ))
  )))
