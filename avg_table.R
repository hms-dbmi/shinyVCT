source("calculation/beta_values.R")

specialities =  c(
  "Gynecology",
  "Neurosurgery",
  "Orthopedics",
  "Otolaryngology (ENT)",
  "Plastics",
  "Thoracic",
   "Urology",
  "Vascular" 
)
mat<-matrix(list(), nrow=7)
each_cpt = list()
inputs = 0
for ( cpt in cpt_wru$CPT ) {
  j = 1
  tot = list(c(), c(), c(),c(),c(),c(),c())
  for (age in seq(10, 65, 5)) {
    for (asa in seq(1,4)) {
      for (emer in seq(0,1)) {
        for (fn in seq(1,3)) {
          for(in_out in seq(0,1)) {
            for (spec in specialities) {
              inputs = inputs+1
              if(inputs %% 100 == 0){
                print(age)
              }
              df = make_risk_df(cpt, age, asa, emer, fn, in_out, spec)
              df = df[order(as.numeric(rownames(df))),,drop=FALSE ]["V3"]
              i = 1
              for (x in df[[1]]){
                if(x>0.9){
                  print(list(cpt, age, asa, emer, fn, in_out, spec))
                }
                tot[[i]] = c(tot[[i]], x)
                i = i+1
              }
            }
          }
        }
      }
    }
  }
  each_cpt[[j]] =  tot
  j = j+1
}
save(each_cpt, file = "under65.RData")

each_cpt2 = list()
inputs = 0
for ( cpt in cpt_wru$CPT ) {
  j = 1
  tot2 = list(c(), c(), c(),c(),c(),c(),c())
  for (age in seq(65, 110, 5)) {
    for (asa in seq(1,4)) {
      for (emer in seq(0,1)) {
        for (fn in seq(1,3)) {
          for(in_out in seq(0,1)) {
            for (spec in specialities) {
              inputs = inputs+1
              if(inputs %% 100 == 0){
                print(age)
              }
              df = make_risk_df(cpt, age, asa, emer, fn, in_out, spec)
              df = df[order(as.numeric(rownames(df))),,drop=FALSE ]["V3"]
              i = 1
              for (x in df[[1]]){
                tot2[[i]] = c(tot2[[i]], x)
                i = i+1
              }
            }
          }
        }
      }
    }
  }
  each_cpt2[[j]] =  tot2
  j = j+1
}
save(each_cpt2, file = "over65.RData")

comps = c("Respiratory", "Infection", "UTI","VTE", "Cardiac", "Renal","Stroke")
sds = c()
means = c()
risk = c()
cpts = c()
j = 1
for ( cpt in cpt_wru$CPT ) {
  i = 1
  for (x in each_cpt[1][[1]]){
    cpts = c(cpts, cpt)
    risk = c(risk, comps[i])
    sds = c(sds, sd(x))
    means = c(means, mean(x))
    i =i+1
  }
  j = j+1
}
under_65 <- data.frame(cpts,risk, means, sds)

write.csv(under_65,"under_65_risk.csv")

sds2 = c()
means2 = c()
risk = c()
cpts = c()
j = 1
for ( cpt in cpt_wru$CPT ) {
  i = 1
  for (x in each_cpt2[1][[1]]){
    cpts = c(cpts, cpt)
    risk = c(risk, comps[i])
    sds2 = c(sds2, sd(x))
    means2 = c(means2, mean(x))
    i =i+1
  }
  j = j+1
}
over_65 <- data.frame(cpts,risk, means2, sds2)

write.csv(over_65,"over_65_risk.csv")


load("over65.RData")
load("under65.RData")
