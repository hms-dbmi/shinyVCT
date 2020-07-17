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
                         ), imageOutput("dot")),

                ),
                tabPanel("Logarithmic Lollipop", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), imageOutput("log")),
                         
                ),
                tabPanel("Bar Strength", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), imageOutput("bar")),
                         
                )

                
    ),
    
    fluidRow(style = "overflow-y: auto;", column(12, align = "center",
                                                 
    #div(id = "generate_pdf", tags$a(h4("Generate Report",  class = "btn btn-danger", style = "fontweight:600"),href = "?graph_view"))
    #Download Button (Will be reposistioned)
    downloadButton('downloadData', 'Download data'),
    ),

    
    
    
    )
)


))
