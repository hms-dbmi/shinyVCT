output$pageStub <- renderUI(tagList(


  #column page
  column(
    12,
    
    #DOT ARRAY TAB PANNEL
    tabsetPanel(type = "tabs",
                tabPanel("Img", br(),
                         column(12, align = "center",  style = "margin-top:-10px;padding:0px", tags$style(
                           type="text/css",
                           #"#image img {width: auto; max-height: 500px; height: auto}"
                         ), imageOutput("img1")),

                ),
                tabPanel("Dot Array",br(),
                         fluidRow(
                           offset = 0,
                           
                           #three plots
                           column(4, align = "center", plotOutput("dot_array_1", height = "100px")),
                           column(4, align = "center", plotOutput("dot_array_2", height = "100px")),
                           column(4, align = "center", plotOutput("dot_array_3", height = "100px")),
                           
                         ),
                         fluidRow(
                           offset = 0,
                           
                           #footers on those three plots - CSS formatting to be done on text\
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][1], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][1])), style = "margin-top:2px;padding:5px")
                           ),
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][2], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][2], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][2])), style = "margin-top:2px;padding:5px")
                           ),
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][3], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][3], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][3])), style = "margin-top:2px;padding:5px")
                           )
                         )),
                
                tabPanel("Logarithmic Lollipop",br(),
                        fluidRow( #column(2,
                                # tags$div(class ="row", tags$div(class="col-md-6", border="1", tags$h4("ASd"))),
                                # tags$div(class ="row", tags$div(class="col-md-6", border="1", tags$h4("ASd"))),
                                # tags$div(class ="row", tags$div(class="col-md-6", border="1", tags$h4("ASd"))),
                          #      tags$div(class ="row", tags$div(class="col-md-6", border="1", tags$h4("label formatting needed")))),
                         column(12, align = "center", plotOutput("lollipop", height = "200px", width = "100%")),
                        )   
                ),
                tabPanel("Bar Strength",br(),
                         
                         fluidRow(
                           offset = 0,
                           
                           #three plots
                           column(4, align = "center", img(src=get_bar_img(selected_events_df["V3"][[1]][1]), height="100px", width="100px", align="center")),
                           column(4, align = "center", img(src=get_bar_img(selected_events_df["V3"][[1]][2]), height="100px", width="100px", align="center")),
                           column(4, align = "center", img(src=get_bar_img(selected_events_df["V3"][[1]][3]), height="100px", width="100px", align="center")),
                           
                         ),
                         fluidRow(
                           offset = 0,
                           
                           #footers on those three plots - CSS formatting to be done on text\
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][1], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][1], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][1])), style = "margin-top:2px;padding:5px")
                           ),
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][2], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][2], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][2])), style = "margin-top:2px;padding:5px")
                           ),
                           column(
                             4,
                             align = "center",
                             tags$h3(selected_events_df["V1"][[1]][3], style = "margin-bottom:2px; margin-top: 5px; padding:0px"),
                             tags$h5(selected_events_df["V2"][[1]][3], style = "margin:0px;padding:0px"),
                             tags$h6(percent(as.numeric(selected_events_df["V3"][[1]][3])), style = "margin-top:2px;padding:5px")
                           )
                         )
                         
                         
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
