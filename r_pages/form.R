form <- renderUI(tagList(fluidRow(
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
    column(6, align = "center", div(
      
      actionButton("form_home_page", "Back")
    )),
    column(6, align = "center", div(id = "to_user", actionButton("form_complications_page", "Next"))),
  ),
  
)))

observe({
  if (is.null(input$asa_status) ||
      input$asa_status == 0 ||
      input$surg_spec == 0) {
    shinyjs::hideElement(id = "to_user")
  }
  else{
    shinyjs::showElement(id = "to_user")
  }
})

observeEvent(input$form_complications_page, {
  t_hash = risk_inputs()
  t_hash[["age"]] = input$age
  t_hash[["asa"]] = input$asa_status
  t_hash[["emer"]] = input$em_case
  t_hash[["func"]] = input$func_status
  t_hash[["inout"]] = input$oper
  t_hash[["spec"]] = input$surg_spec
  risk_inputs(t_hash)
  print(risk_inputs())
  if (is.null(risk_inputs()[["cpt"]]) ||
      is.null(risk_inputs()[["asa"]])) {
      output$pageStub <- error
  }else{
    output$pageStub <- complications
  }
  events_df(make_risk_df(
    risk_inputs()[["cpt"]],
    risk_inputs()[["age"]],
    risk_inputs()[["asa"]],
    risk_inputs()[["emer"]],
    risk_inputs()[["func"]],
    risk_inputs()[["inout"]],
    risk_inputs()[["spec"]]
  ))
  discharge_data(make_discharge_list(
    risk_inputs()[["cpt"]],
    risk_inputs()[["age"]],
    risk_inputs()[["asa"]],
    risk_inputs()[["emer"]],
    risk_inputs()[["func"]],
    risk_inputs()[["inout"]],
    risk_inputs()[["spec"]]
  ))
  print(discharge_data)
  chosen_risk(as.vector(events_df()[seq(4, nrow(events_df())),][['V1']]))
  high_risk(as.vector(events_df()[seq(1, 3),][['V1']]))
  
})
observeEvent(input$form_home_page, {
  output$pageStub <- home
})
