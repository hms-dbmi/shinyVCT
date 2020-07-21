output$pageStub <- renderUI(tagList(fluidRow(
  column(
    12,
    column(
      6,
      align = "left",
      
      numericInput(inputId = "age",
                   h4("Enter Patient Age"),
                   value = 18),
      br(),
      selectInput(
        inputId = "asa_status",
        h4("American Society of Anesthesiology Class"),
        choices = list(
          "Please Select One" = 0,
          "ASA 1: Normal healthy patient." = 1,
          "ASA 2: Patient with mild systemic disease" = 2,
          "ASA 3: Patient with severe systemic disease" = 3,
          "ASA 4: Patient with severe systemic disease that is a constant threat to life" = 4,
          "ASA V:	A moribund patient who is not expected to survive without the operation" = 5
        ),
        selected = 0
      ),
      selectInput(
        inputId = "func_status",
        h4("Functional Status"),
        choices = list(
          "Independent" = 1,
          "Partially Dependent" = 2,
          "Totally Dependent" = 3
        ),
        selected = 1
      ),
      
    ),
    column(
      6,
      align = "left",
      selectInput(
        inputId =  "surg_spec",
        h4("Surgeon Specialty"),
        choices = valid_specialities,
        selected = 1
      ),
      selectInput(
        inputId = "em_case",
        h4("Emergency Case"),
        choices = list("Yes" = 1, "No" = 0),
        selected = 0,
      ),
      
      
      selectInput(
        inputId ="oper",
        h4("In-/Outpatient Operation"),
        choices = list("Inpatient" = 1, "Outpatient" = 0),
        selected = 0,
      ),
    ),

    column(12, br(), br(), br(), br(), br(), br(),br(),br() ),
    column(6, align = "center", div(tags$a(
      h4("Back",  class = "btn btn-default btn-secondary action-button",
         style = "fontweight:600"),
      href = "?home"
    ))),
    column(6, align = "center", div(id = "to_user", tags$a(
      h4("Next",  class = "btn btn-default btn-info action-button",
         style = "fontweight:600"),
      href = "?complications_single_col"
    ))),
  ),
  
)))
