#######################################################################################################################################
# R SCRIPT FOR ALL FUNCTIONS INCLUDED IN RMD FILES 

# Functions are used to obtain descriptive statistics from the affective priming paradigm and food choice task data
# We have included functions for repeating confirmatory analyses under different data exclusion criteria, as part of 
# several robustness checks (i.e., sensitivity analyses)

#ABBREVIATIONS:

#APP:   Affective priming paradigm
#ER:    Error rate(s)
#RT:    Reaction time(s)
#F:     Food prime trials 
#NF:    Non-food prime trials 
#con:   Congruent
#inc:   Incongruent

#ml:    most liked
#ll:    least liked
#hml:   healthy most liked
#uml:   unhealthy most liked
#h:     healthy
#u:     unhealthy

#FCT:   Food choice task

#######################################################################################################################################


#Functions for APP descriptives####

#Function to get ERs from non-food prime trials

NF_ER <- function (x) {
  c(ER = mean(x$accuracy),
    con_ER = mean(x$accuracy[x$congruence=="congruent"]), 
    inc_ER = mean(x$accuracy[x$congruence=="incongruent"]))
}

#Function to get ERs from food prime trials

F_ER <- function (x) {
  c(ER = mean(x$accuracy),
    con_ER = mean(x$accuracy[x$congruence=="congruent"]), 
    inc_ER = mean(x$accuracy[x$congruence=="incongruent"]), 
    con_h_ER = mean(x$accuracy[x$congruence=="congruent" & x$healthiness=="healthy"]),
    inc_h_ER = mean(x$accuracy[x$congruence=="incongruent" & x$healthiness=="healthy"]),
    con_u_ER = mean(x$accuracy[x$congruence=="congruent" & x$healthiness=="unhealthy"]),
    inc_u_ER = mean(x$accuracy[x$congruence=="incongruent" & x$healthiness=="unhealthy"]),
    con_ml_ER = mean(x$accuracy[(x$trial=="HML_con"|x$trial=="UML_con")]),
    inc_ml_ER = mean(x$accuracy[(x$trial=="HML_inc"|x$trial=="UML_inc")]),
    con_ll_ER = mean(x$accuracy[(x$trial=="HLL_con"|x$trial=="ULL_con")]),
    inc_ll_ER = mean(x$accuracy[(x$trial=="HLL_inc"|x$trial=="ULL_inc")]),
    con_hml_ER = mean(x$accuracy[x$trial=="HML_con"]),
    inc_hml_ER = mean(x$accuracy[x$trial=="HML_inc"]),
    con_uml_ER = mean(x$accuracy[x$trial=="UML_con"]),
    inc_uml_ER = mean(x$accuracy[x$trial=="UML_inc"]),
    con_hll_ER = mean(x$accuracy[x$trial=="HLL_con"]),
    inc_hll_ER = mean(x$accuracy[x$trial=="HLL_inc"]),
    con_ull_ER = mean(x$accuracy[x$trial=="ULL_con"]),
    inc_ull_ER = mean(x$accuracy[x$trial=="ULL_inc"]))
}

#Function to get median RTs for correct responses from non-food prime trials

NF_RT <- function (x) {
  c(con_RT = median(x$rt[x$congruence=="congruent"]),
    inc_RT = median(x$rt[x$congruence=="incongruent"]))
}

#Function to get median RTs for correct responses from food prime trials

F_RT <- function (x) {
  c(con_RT = median(x$rt[x$congruence=="congruent"]),
    inc_RT = median(x$rt[x$congruence=="incongruent"]),
    con_h_RT = median(x$rt[x$congruence=="congruent" & x$healthiness=="healthy"]),
    inc_h_RT = median(x$rt[x$congruence=="incongruent" & x$healthiness=="healthy"]),
    con_u_RT = median(x$rt[x$congruence=="congruent" & x$healthiness=="unhealthy"]),
    inc_u_RT = median(x$rt[x$congruence=="incongruent" & x$healthiness=="unhealthy"]),
    con_ml_RT = median(x$rt[(x$trial=="HML_con"|x$trial=="UML_con")]),
    inc_ml_RT = median(x$rt[(x$trial=="HML_inc"|x$trial=="UML_inc")]),
    con_ll_RT = median(x$rt[(x$trial=="HLL_con"|x$trial=="ULL_con")]),
    inc_ll_RT = median(x$rt[(x$trial=="HLL_inc"|x$trial=="ULL_inc")]),
    con_hml_RT = median(x$rt[x$trial=="HML_con"]),
    inc_hml_RT = median(x$rt[x$trial=="HML_inc"]),
    con_uml_RT = median(x$rt[x$trial=="UML_con"]),
    inc_uml_RT = median(x$rt[x$trial=="UML_inc"]),
    con_hll_RT = median(x$rt[x$trial=="HLL_con"]),
    inc_hll_RT = median(x$rt[x$trial=="HLL_inc"]),
    con_ull_RT = median(x$rt[x$trial=="ULL_con"]),
    inc_ull_RT = median(x$rt[x$trial=="ULL_inc"]))
}

#Functions for FCT descriptives####

#Function for choice probability calculation
FCT_p <- function(x) {
  c(p.mh = mean(x$choice[x$trial=="LC_HF"]),
    p.mu = mean(x$choice[x$trial=="LC_UF"]),
    p.um = mean(x$choice[x$trial=="HC_ML"]))
}

#Function to calculate proportion of missed responses

FCT_miss <- function(x) {
  c(LC_HF <- mean(x$response[x$trial=="LC_HF"]),
  LC_UF <- mean(x$response[x$trial=="LC_UF"]),
  HC_ML <- mean(x$response[x$trial=="HC_ML"]))
}

#Functions for normality tests and data transformations####

#Function that runs Shapiro Wilk tests for RT-related hypotheses
#H1a, H2a, H2b, H2c, H2d

Shapiro_wilk_RTs <- function(food_data, nonfood_data) {
H1a.SW <- shapiro.test(nonfood_data$diff_RT)  
H2a.SW <- shapiro.test(food_data$diff_RT)
H2b.SW <- shapiro.test(food_data$diff_RT_h)
H2c.SW <- shapiro.test(food_data$diff_RT_u)
H2d.SW <- shapiro.test(food_data$diff_DRT)
SW_RT <- cbind(H1a.SW[2],H2a.SW[2],H2b.SW[2],H2c.SW[2],H2d.SW[2])
return(SW_RT)
}

#Function that runs Shapiro Wilk tests for ER-related hypotheses
#H1b, H3a, H3b, H3c, H3d

Shapiro_wilk_ERs <- function(food_data, nonfood_data) {
  H1b.SWa <- shapiro.test(nonfood_data$diff_ER)  
  H3a.SWa <- shapiro.test(food_data$diff_ER)
  H3b.SWa <- shapiro.test(food_data$diff_ER_h)
  H3c.SWa <- shapiro.test(food_data$diff_ER_u)
  H3d.SWa <- shapiro.test(food_data$diff_DER)
  SW_ER <- cbind(H1b.SW[2],H3a.SW[2],H3b.SW[2],H3c.SW[2],H3d.SW[2])
  return(SW_ER)
}

#Function that log-transforms RTs for H1a based on Shapiro Wilk test results ("sw")

log_H1a <- function(data, sw) {
  if (sw[1]<=0.005) {
    data$con_rt = log(data$con_rt)
    data$inc_rt = log(data$inc_rt)
  }
  return(data)
}

#Function that log-transforms RTs and DRTs for H2 based on Shapiro Wilk test results ("sw")
log_H2 <- function(data, sw) {
  if (sw[2]<=0.005 | sw[3]<=0.005 | sw[4]<=0.005 | sw[5]<=0.005) {
    data$con_rt = log(data$con_rt)
    data$inc_rt = log(data$inc_rt)
    data$con_h_rt = log(data$con_h_rt)
    data$inc_h_rt = log(data$inc_h_rt)
    data$con_u_rt = log(data$con_u_rt)
    data$inc_u_rt = log(data$inc_u_rt)
    data$DRT_h = log(data$inc_hml_RT) - log(data$con_hml_RT)
    data$DRT_u = log(data$inc_uml_RT) - log(data$con_uml_RT)
    data$DRT_hml = data$DRT_u - data$DRT_h
  }
  return(data)
}

#Functions for robustness checks####

#Function to get mean RTs for correct responses from non-food prime trials

NF_means <- function (x) {
  c(con_RT = mean(x$rt[x$congruence=="congruent"]),
    inc_RT = mean(x$rt[x$congruence=="incongruent"]))
}

#Function to get mean RTs for correct responses from food prime trials

F_means <- function (x) {
  c(con_RT = mean(x$rt[x$congruence=="congruent"]),
    inc_RT = mean(x$rt[x$congruence=="incongruent"]),
    con_h_RT = mean(x$rt[x$congruence=="congruent" & x$healthiness=="healthy"]),
    inc_h_RT = mean(x$rt[x$congruence=="incongruent" & x$healthiness=="healthy"]),
    con_u_RT = mean(x$rt[x$congruence=="congruent" & x$healthiness=="unhealthy"]),
    inc_u_RT = mean(x$rt[x$congruence=="incongruent" & x$healthiness=="unhealthy"]),
    con_ml_RT = mean(x$rt[(x$trial=="HML_con"|x$trial=="UML_con")]),
    inc_ml_RT = mean(x$rt[(x$trial=="HML_inc"|x$trial=="UML_inc")]),
    con_ll_RT = mean(x$rt[(x$trial=="HLL_con"|x$trial=="ULL_con")]),
    inc_ll_RT = mean(x$rt[(x$trial=="HLL_inc"|x$trial=="ULL_inc")]),
    con_hml_RT = mean(x$rt[x$trial=="HML_con"]),
    inc_hml_RT = mean(x$rt[x$trial=="HML_inc"]),
    con_uml_RT = mean(x$rt[x$trial=="UML_con"]),
    inc_uml_RT = mean(x$rt[x$trial=="UML_inc"]),
    con_hll_RT = mean(x$rt[x$trial=="HLL_con"]),
    inc_hll_RT = mean(x$rt[x$trial=="HLL_inc"]),
    con_ull_RT = mean(x$rt[x$trial=="ULL_con"]),
    inc_ull_RT = mean(x$rt[x$trial=="ULL_inc"]))
}
