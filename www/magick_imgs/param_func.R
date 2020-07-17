create_param_list <-
  function(input_df,
           destination_vals,
           risk_inputs,
           waffle_func) {
    print(risk_inputs)
    arrow_cuttoffs <- c(0.05, 0.25, 0.5, 0.75)
    locations <- c('top', 'middle', 'bot')
    input_df$V3 = input_df$V3 * 100
    arrow_locs <- c()
    params <- list(
      event_data = input_df,
      plot_func =  waffle_func,
      destination_home = destination_vals[1] * 100,
      destination_readmit = destination_vals[2] * 100,
      destination_death = destination_vals[3] * 100,
      cpt_ = risk_inputs[["cpt"]],
      age = risk_inputs[["age"]],
      asa_level = risk_inputs[["asa"]],
      emergency = risk_inputs[["emer"]],
      functional_level = risk_inputs[["func"]],
      in_out_patient = risk_inputs[["inout"]],
      specialty = risk_inputs[["spec"]]


    )
    for (i in 1:3) {
      arrow = paste("New Images/", locations[i], max(which(arrow_cuttoffs <= destination_vals[i]), 0) + 1,
                    ".png", sep = "")
      params[[paste(locations[i], "_arrow", sep = "")]] <- arrow
    }
    print(params)
    save(params, file="fname.RData")
    return(params)
  }