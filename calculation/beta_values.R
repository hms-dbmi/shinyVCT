library(readxl)
library(tidyverse)
library(hash)
beta_values <- read.table("calculation/2020-01-07Beta100.csv",  sep = ",", header = TRUE)
cpt_wru <- read_excel("calculation/2019-11-19 NSQIP CPT Rates.xlsx")
cpt_wru <- cpt_wru[cpt_wru$CPT %in% c(55866,38571,50543,52234,52235,52240), ]
spec_list = as.character(beta_values[beta_values$Variable == "SURGSPEC", ]$ClassVal0)
format_inputs = function(cpt, age, asa_class, emergency, fn_status, in_out, spec, risk){
  data_vals = cpt_wru %>% filter(CPT == cpt)
  wRVU = data_vals$AMAWorkRVU[[1]]
  risk_val = data_vals[paste(risk, "Rate", sep = "")][[1]][1]
  asa_l =  c()
  for (x in seq(1:4)){
    if (asa_class == (x+1)){
      asa_l = c(asa_l,1)
    }else{
      asa_l = c(asa_l,0)
    }
  }
  fn_l =  c()
  for (x in seq(1:2)){
    if (fn_status == (x+1)){
      fn_l = c(fn_l,1)
    }else{
      fn_l = c(fn_l,0)
    }
  }
  spec_l = c()
  for (x in spec_list){
    if (spec == x){
      spec_l = c(spec_l,1)
    }else{
      spec_l = c(spec_l,0)
    }
  }
  return (c(risk, age, wRVU, asa_l, risk_val, emergency,fn_l,in_out, spec_l ))
}

calculate_risk = function(inputs){
  temp_beta = beta_values[inputs[1]][[1]]
  value = temp_beta[1]
  i = 2
  for (x in inputs[seq(2, length(inputs))]){
    value = value + (temp_beta[i]*as.numeric(x))
    i = i+1
  }
  return (plogis(value))
}

compare_chance = function(risk, value, cpt){
  data_vals = cpt_wru %>% filter(CPT == cpt)
  risk_val = data_vals[paste(risk, "Rate", sep = "")][[1]][1]
  #print(plogis(risk_val))
  #if (as.numeric(value) > (1.5*as.numeric(risk_val))){
  #  return ("Above Average")
  #}else if (as.numeric(value) < (0.5*as.numeric(risk_val))){
  #  return ("Below Average")
  #}else{
    return ("Average")
  #}
}

calculate_risk(format_inputs(52235, 40, 3, 1, 3, 1, "Orthopedics", "UTI"))
risk = c("Respiratory", "Infection", "UTI", "VTE", "Cardiac", "Renal", "Stroke")
discharge = c("Death30Day", "Unplannedreadmission", "NotHome")
risk_name_dictionary = hash()
risk_name_dictionary[["Respiratory"]] = "Respiratory Complications"
risk_name_dictionary[["Infection"]] = "Infection"
risk_name_dictionary[["UTI"]] = "Urinary Tract Infection"
risk_name_dictionary[["VTE"]] = "Venous thromboembolism"
risk_name_dictionary[["Cardiac"]] = "Cardiac Complications"
risk_name_dictionary[["Renal"]] = "Renal Complications"
risk_name_dictionary[["Stroke"]] = "Stroke Complications"


make_risk_df = function(cpt, age, asa_class, emergency, fn_status, in_out, spec){
  events <- c()
  i = 0
  for (x in risk){
    i = i+1
    value = calculate_risk(format_inputs(cpt, age, asa_class, emergency, fn_status, in_out, spec, x))
    events[[i]] = c(risk_name_dictionary[[x]], compare_chance(x, value, cpt), value)
  }
  events_df = as.data.frame(do.call(rbind, events))
  events_df[["V3"]] = as.numeric(as.character(events_df[["V3"]]))
  events_df <- events_df[order(-events_df$V3),]
  return (events_df)
}
