output$pageStub <- renderUI(tagList(


  #column page
  column(
    12,
    
    #DOT ARRAY TAB PANNEL
    tabsetPanel(type = "tabs",
                tabPanel("Dot Array", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), withSpinner(type = 8, color = "#2BBBAD", imageOutput("dot"))),
                         
                         br(), br(),br(),br(),
                         
                         fluidRow(style = "overflow-y: auto;", 
                                  column(6, align = "center", div(tags$a(
                                    h4("Back",  class = "btn btn-default btn-secondary action-button",
                                       style = "fontweight:600"),
                                    href = "?complications_single_col"
                                  ))),
                                  column(6, align = "center",
                          downloadButton('downloadDot', 'Download PDF Report'),
                         )),
                         

                ),
                tabPanel("Logarithmic Lollipop", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), withSpinner(type = 8, color = "#2BBBAD", imageOutput("log"))),

                         br(), br(),br(),br(),
                         fluidRow(style = "overflow-y: auto;", column(6, align = "center", div(tags$a(
                           h4("Back",  class = "btn btn-default btn-secondary action-button",
                              style = "fontweight:600"),
                           href = "?complications_single_col"
                         ))), column(6, align = "center",
                             downloadButton('downloadLog', 'Download PDF Report'),
                         )),
                         
                ),
                tabPanel("Bar Strength", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), withSpinner(type = 8, color = "#2BBBAD", imageOutput("bar"))),
                         br(), br(),br(),br(),
                         fluidRow(style = "overflow-y: auto;", column(6, align = "center", div(tags$a(
                           h4("Back",  class = "btn btn-default btn-secondary action-button",
                              style = "fontweight:600"),
                           href = "?complications_single_col"
                         ))),
                                  column(6, align = "center",
                              downloadButton('downloadBar', 'Download PDF Report'),
                         )),
                         
                )

                
    ),


    
    
    
    )
)


)
