error <-
  renderUI(tagList(# 404 if no file with that name
    fluidRow(column(
      12,
      HTML(
        "<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
        "menu above to navigate to the page you were looking for.</p>"
      ),
      column(12, align = "center", div(id = "to_graph", tags$a(
        h4("Return to Home",  class = "btn btn-default btn-info",
           style = "fontweight:600"),
        href = "?home"
      )))
    ))))