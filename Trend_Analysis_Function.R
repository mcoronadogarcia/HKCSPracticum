# Load Packages -----------------------------------------------------------

library('survey')

# Creare Function ---------------------------------------------------------

hkcs.trend.analysis =  function(hkcs.data, hkcs.cluster,
                                hkcs.strata, hkcs.weight,
                                prefix = NULL, variables = NULL){
  # Prepare Dataset
  hkcs.data$time.v1 = (hkcs.data$Survey_Year - 2013)/2
  
  # create different datasets for each year
  hkcs.data.2013 = hkcs.data[which(hkcs.data$Survey_Year == 2013), ]
  hkcs.data.2015 = hkcs.data[which(hkcs.data$Survey_Year == 2015), ]
  hkcs.data.2017 = hkcs.data[which(hkcs.data$Survey_Year == 2017), ]
  
  # Set Up Survey Design
  hkcs.des = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                       weight = as.formula(paste0("~", hkcs.weight)),
                       strata = as.formula(paste0("~", hkcs.strata)),
                       data = hkcs.data,
                       nest = TRUE)
  
  # create the design for the each year
  hkcs.des.2013 = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                            weight = as.formula(paste0("~", hkcs.weight)),
                            strata = as.formula(paste0("~", hkcs.strata)),
                            data = hkcs.data.2013,
                            nest = TRUE)
  
  hkcs.des.2015 = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                            weight = as.formula(paste0("~", hkcs.weight)),
                            strata = as.formula(paste0("~", hkcs.strata)),
                            data = hkcs.data.2015,
                            nest = TRUE)
  
  hkcs.des.2017 = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                            weight = as.formula(paste0("~", hkcs.weight)),
                            strata = as.formula(paste0("~", hkcs.strata)),
                            data = hkcs.data.2017,
                            nest = TRUE)
  
  # create the results table
  # locate all the instances where there are QN variables
  if(!is.null(prefix) & is.null(variables)){
    variables_of_interest.loc = grep(
      paste0("^", prefix), colnames(hkcs.data))
    variables_of_interest = colnames(cc_hs_final)[variables_of_interest.loc]
  } else if(!is.null(variables) & is.null(prefix)){
    variables_of_interest = variables
  }
  
  # create a table to fill out
  results.complete = data.frame(
    variable_name =  rep(NA, length(variables_of_interest)),
    prop.2013 = rep(NA, length(variables_of_interest)),
    prop.2015 = rep(NA, length(variables_of_interest)),
    prop.2017 = rep(NA, length(variables_of_interest)),
    trend = rep(NA, length(variables_of_interest))
  )
  for(i in 1:length(variables_of_interest)){
    # create formula
    freq.formula = as.formula(paste0("~", variables_of_interest[i]))
    
    # results
    # 2013
    ci.2013 = svyciprop(freq.formula, hkcs.des.2013,
                        method = "logit")
    coef.2013 = round(as.vector(ci.2013), digits = 3)*100 # coefficient
    ci.coef.2013 = round(attr(ci.2013, "ci"), digits = 3)*100 # CI
    
    # 2015
    ci.2015 = svyciprop(freq.formula, hkcs.des.2015,
                        method = "logit")
    coef.2015 = round(as.vector(ci.2015), digits = 3)*100 # coefficient
    ci.coef.2015 = round(attr(ci.2015, "ci"), digits = 3)*100 # CI
    
    # 2017
    ci.2017 = svyciprop(freq.formula, hkcs.des.2017,
                        method = "logit")
    coef.2017 = round(as.vector(ci.2017), digits = 3)*100 # coefficient
    ci.coef.2017 = round(attr(ci.2017, "ci"), digits = 3)*100 # CI
    
    # trend 
    trend.formula = as.formula(paste0(variables_of_interest[i],
                                      " ~ time.v1"))
    trend.model = svyglm(trend.formula, family = quasibinomial(),
                         design = hkcs.des)
    result.summary = summary(trend.model)$coefficients # can convert this to data frame
    
    if(result.summary["time.v1", "Pr(>|t|)"] <= 0.05){
      trend = ifelse(result.summary["time.v1", "Estimate"] > 0,
                     "Positive Increase", "Negative Increase") 
    } else {
      trend = ""
    }
    
    # add results to data frame
    results.complete$variable_name[i] = variables_of_interest[i]
    results.complete$prop.2013[i] = paste0(coef.2013, "% (",
                                           ci.coef.2013[1],"%, ",
                                           ci.coef.2013[2],"%)")
    results.complete$prop.2015[i] = paste0(coef.2015, "% (",
                                           ci.coef.2015[1],"%, ",
                                           ci.coef.2015[2],"%)")
    results.complete$prop.2017[i] = paste0(coef.2017, "% (",
                                           ci.coef.2017[1],"%, ",
                                           ci.coef.2017[2],"%)")
    results.complete$trend[i] = trend
  }
  
  colnames(results.complete) <- c("Variable of Interest",
                                  "2013 Proportion (95% CI)",
                                  "2015 Proportion (95% CI)",
                                  "2017 Proportion (95% CI)",
                                  "Trend")
  
  return(results.complete)
}