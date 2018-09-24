# Load Packages -----------------------------------------------------------

library('survey')

# Load Data ---------------------------------------------------------------
cc_hs_final = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/cc_hs_final.csv')

# Prepare Dataset ---------------------------------------------------------
cc_hs_final$commonWeight = ifelse(cc_hs_final$Survey_Year == 2017, 
                                  cc_hs_final$Wt_School_modb, 
                                  cc_hs_final$Wt_School_moda)

cc_hs_final$time.v1 = (cc_hs_final$Survey_Year - 2013)/2
table(cc_hs_final$time.v1)
table(cc_hs_final$Survey_Year) # this matches the new variable

# create different datasets for each year
cc_hs_final.2013 = cc_hs_final[which(cc_hs_final$Survey_Year == 2013), ]
cc_hs_final.2015 = cc_hs_final[which(cc_hs_final$Survey_Year == 2015), ]
cc_hs_final.2017 = cc_hs_final[which(cc_hs_final$Survey_Year == 2017), ]
# Weights -----------------------------------------------------------------
# which weight variable has missing
summary(cc_hs_final$Wt_School)
summary(cc_hs_final$Wt_School_moda)
summary(cc_hs_final$Wt_School_modb)

moda_na.location = which(is.na(cc_hs_final$Wt_School_moda))
modb_na.location = which(is.na(cc_hs_final$Wt_School_modb))
# are there classes that have missing in two weight variables
(moda_modb_na = which(is.na(cc_hs_final$Wt_School_moda) & 
        is.na(cc_hs_final$Wt_School_modb)))
(school_moda_na = which(is.na(cc_hs_final$Wt_School) & 
                        is.na(cc_hs_final$Wt_School_moda)))
(school_modb_na = which(is.na(cc_hs_final$Wt_School) & 
                          is.na(cc_hs_final$Wt_School_modb)))

# Look at the data
head(cc_hs_final[moda_na.location, c("Wt_School", "Wt_School_moda",
                                "Wt_School_modb", "Survey_Year")])
head(cc_hs_final[modb_na.location, c("Wt_School", "Wt_School_moda",
                                     "Wt_School_modb", "Survey_Year")])

# how many weights are missing between moda and modb in 2013
sum(is.na(cc_hs_final$Wt_School_moda) & cc_hs_final$Survey_Year == 2013) +
  sum(is.na(cc_hs_final$Wt_School_modb) & cc_hs_final$Survey_Year == 2013)
# 0

# how many weights are missing between moda and modb in 2015
sum(is.na(cc_hs_final$Wt_School_moda) & cc_hs_final$Survey_Year == 2015) +
sum(is.na(cc_hs_final$Wt_School_modb) & cc_hs_final$Survey_Year == 2015)
# 241 + 238 = 479

# how many weights are missing between moda and modb in 2017
sum(is.na(cc_hs_final$Wt_School_moda) & cc_hs_final$Survey_Year == 2017) +
  sum(is.na(cc_hs_final$Wt_School_modb) & cc_hs_final$Survey_Year == 2017)
# 171 + 173 = 344

length(moda_na.location) + length(modb_na.location) # 823
479 + 344 # 823

# Survey Design -----------------------------------------------------------
# use school weight untill you can figure out what to do with schools missing 
# weights in moda and modb

# this is where you would specify the fpc
hkcs.des = svydesign(id = ~CLASSID, weight = ~Wt_School,
                     strata = ~SchoolID, data = cc_hs_final,
                     nest = TRUE)
# create the design for the each year
hkcs.des.2013 = svydesign(id = ~CLASSID, weight = ~Wt_School,
                          strata = ~SchoolID,
                          data = cc_hs_final.2013,
                          nest = TRUE)
hkcs.des.2015 = svydesign(id = ~CLASSID, weight = ~Wt_School,
                          strata = ~SchoolID,
                          data = cc_hs_final.2015,
                          nest = TRUE)
hkcs.des.2017 = svydesign(id = ~CLASSID, weight = ~Wt_School,
                          strata = ~SchoolID,
                          data = cc_hs_final.2017,
                          nest = TRUE)

# Frequency Example -------------------------------------------------------
# computes confidence intervals for proportions using methods that may be
# more accurate near 0 and 1
# 2013
ci.2013 = svyciprop(~QN_ACTIVITY, hkcs.des.2013, method = "logit")
coef.2013 = round(as.vector(ci.2013), digits = 4)*100 # coefficient
ci.coef.2013 = round(attr(ci.2013, "ci"), digits = 4)*100 # CI

# 2015
ci.2015 = svyciprop(~QN_ACTIVITY, hkcs.des.2015, method = "logit")
coef.2015 = round(as.vector(ci.2015), digits = 4)*100 # coefficient
ci.coef.2015 = round(attr(ci.2015, "ci"), digits = 4)*100 # CI

# 2017
ci.2017 = svyciprop(~QN_ACTIVITY, hkcs.des.2017, method = "logit")
coef.2017 = round(as.vector(ci.2017), digits = 4)*100 # coefficient
ci.coef.2017 = round(attr(ci.2017, "ci"), digits = 4)*100 # CI

# create data frame with this information
results = data.frame(variable_name = "QN_ACTIVITY",
                     prop.2013 = paste0(coef.2013, "% (",
                                        ci.coef.2013[1],"%, ",
                                        ci.coef.2013[2],"%)"),
                     prop.2015 = paste0(coef.2015, "% (",
                                        ci.coef.2015[1],"%, ",
                                        ci.coef.2015[2],"%)"),
                     prop.2017 = paste0(coef.2017, "% (",
                                        ci.coef.2017[1],"%, ",
                                        ci.coef.2017[2],"%)")
                     )
# Model Example -----------------------------------------------------------
QN_ACTIVITY_MODEL_EX = svyglm(QN_ACTIVITY ~ time.v1, family = quasibinomial(),
       design = hkcs.des)
summary(QN_ACTIVITY_MODEL_EX)
result.summary = summary(QN_ACTIVITY_MODEL_EX)$coefficients # can convert this to data frame

# note results match the results of proc surveylogistic using the same
# weight and time variable
# p-value here is based on t statistic and the p-value in the sas
# program is based on the wald chi-sqaure test statistic

# Add Result Information to Table -----------------------------------------

if(result.summary["time.v1", "Pr(>|t|)"] <= 0.05){
 trend = ifelse(result.summary["time.v1", "Estimate"] > 0,
                "Positive Increase", "Negative Increase") 
} else {
  trend = "Insignificant"
}
trend
results$trend = trend

# Create a Results Table Combining Code -----------------------------------
# locate all the instances where there are QN variables
variables_of_interest.loc = grep("^QN_", colnames(cc_hs_final))
variables_of_interest = colnames(cc_hs_final)[variables_of_interest.loc]
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
  coef.2013 = round(as.vector(ci.2013), digits = 4)*100 # coefficient
  ci.coef.2013 = round(attr(ci.2013, "ci"), digits = 4)*100 # CI

  # 2015
  ci.2015 = svyciprop(freq.formula, hkcs.des.2015,
                      method = "logit")
  coef.2015 = round(as.vector(ci.2015), digits = 4)*100 # coefficient
  ci.coef.2015 = round(attr(ci.2015, "ci"), digits = 4)*100 # CI
  
  # 2017
  ci.2017 = svyciprop(freq.formula, hkcs.des.2017,
                      method = "logit")
  coef.2017 = round(as.vector(ci.2017), digits = 4)*100 # coefficient
  ci.coef.2017 = round(attr(ci.2017, "ci"), digits = 4)*100 # CI
  
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
    trend = "Insignificant"
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
results.complete


# Create Function For Trend Analysis --------------------------------------
# set variables
test.data = cc_hs_final
test.cluster = "CLASSID"
test.weight = "Wt_School"
test.strata = "SchoolID"
prefix.1 = "QN_"
prefix.2 = NULL 
variables.1 = NULL 
variables.2 = c("QN_ACTIVITY", "QN_ALC_30")

hkcs.trend.analysis =  function(hkcs.data, hkcs.cluster,
                                hkcs.strata, hkcs.weight,
                                prefix, variables){
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
    coef.2013 = round(as.vector(ci.2013), digits = 4)*100 # coefficient
    ci.coef.2013 = round(attr(ci.2013, "ci"), digits = 4)*100 # CI
    
    # 2015
    ci.2015 = svyciprop(freq.formula, hkcs.des.2015,
                        method = "logit")
    coef.2015 = round(as.vector(ci.2015), digits = 4)*100 # coefficient
    ci.coef.2015 = round(attr(ci.2015, "ci"), digits = 4)*100 # CI
    
    # 2017
    ci.2017 = svyciprop(freq.formula, hkcs.des.2017,
                        method = "logit")
    coef.2017 = round(as.vector(ci.2017), digits = 4)*100 # coefficient
    ci.coef.2017 = round(attr(ci.2017, "ci"), digits = 4)*100 # CI
    
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
      trend = "Insignificant"
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

test.1 = hkcs.trend.analysis(hkcs.data = test.data,
                    hkcs.cluster = test.cluster,
                    hkcs.strata = test.strata,
                    hkcs.weight = test.weight,
                    prefix = prefix.1,
                    variables = variables.1)
test.2 = hkcs.trend.analysis(hkcs.data = test.data,
                             hkcs.cluster = test.cluster,
                             hkcs.strata = test.strata,
                             hkcs.weight = test.weight,
                             prefix = prefix.2,
                             variables = variables.2)

# Create function that uses the correct weight ----------------------------
# also automate this so that when the following waves come, the data is 
# ready for it

# read in variable information
variable_info = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/HS Snapshot trend variables.csv')


hkcs.trend.analysis =  function(district_id, data, hkcs.cluster, hkcs.strata,
                                moda.weight, modb.weight, core.weight,
                                prefix, variables, trend){
  # Prepare Dataset
  hkcs.data = data[which(data$district_id == district_id), ]
  hkcs.data$time.v1 = (hkcs.data$Survey_Year - 2013)/2
  
  # find unique years and order them in ascending order
  years_list = unique(hkcs.data$Survey_Year)
  years = years_list[order(years_list)]
  survey_years = NULL
  for(i in 1:length(years)){
    survey_years[i] = paste0("Survey_", years[i] %% 100)
  }
  
  # locate all the instances where there are QN variables
  if(!is.null(prefix) & is.null(variables)){
    variables_of_interest.loc = grep(
      paste0("^", prefix), colnames(hkcs.data))
    variables_of_interest = colnames(cc_hs_final)[variables_of_interest.loc]
  } else if(!is.null(variables) & is.null(prefix)){
    variables_of_interest = variables
  }
  
  # create dataset for each variable so that the common weight is there
  variables_datasets = list()
  for(i in 1:length(variables_of_interest)){
    # creates dataset with survey information and variable of interest
    variables_df = hkcs.data[, c(variables_of_interest[i], "Survey_Year", "time.v1", hkcs.cluster, hkcs.strata, moda.weight,
                  modb.weight, core.weight)]
    # stores the wiehgt information for variable of interest
    weight_info = variable_info[which(variable_info$QN_Variable == variables_of_interest[i]),]
    
    #creates common_weight variable 
    variables_df$common_weight = NA
    for(j in 1:length(years)){
      loc = which(variables_df$Survey_Year == years[j])
      if(weight_info[survey_years[j]] == "AB"){
        variables_df$common_weight[loc] = variables_df[loc, core.weight]
      } else if(weight_info[survey_years[j]] == "A"){
        variables_df$common_weight[loc] = variables_df[loc, moda.weight]
      } else if(weight_info[survey_years[j]] == "B"){
        variables_df$common_weight[loc] = variables_df[loc, modb.weight]
      }
    }
    
    # saves dataset in a list
    variables_datasets[[i]] = variables_df[which(is.na(variables_df[,variables_of_interest[i]]) == FALSE),
                                           c(variables_of_interest[i], "Survey_Year", "time.v1",
                                               hkcs.cluster, hkcs.strata, "common_weight")]
  }
  
  # create table
  final.table = NULL
  for(i in 1:length(variables_of_interest)){
    # create formula for variable of interest
    freq.formula = as.formula(paste0("~", variables_of_interest[i]))
    
    # stores the wiehgt information for variable of interest
    weight_info = variable_info[which(variable_info$QN_Variable == variables_of_interest[i]),]
    variable_description = as.character(weight_info$variable_description)
    # proportions per year
    combined.results = NULL
    for(j in 1:length(years)){
      # if then statement to surpress year information 
      
      # create study design for data during jth year of ith variable
      hkcs.des.year = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                           weight = ~common_weight,
                           strata = as.formula(paste0("~", hkcs.strata)),
                           data = variables_datasets[[i]][
                             which(variables_datasets[[i]][, "Survey_Year"] == years[j]), ],
                           nest = TRUE)
      # calculate the proportion estimates and CI for jth year of ith variable
      ci = svyciprop(freq.formula, hkcs.des.year, method = "logit")
      coef = format(as.vector(ci)*100, digits = 1, nsmall = 1, trim = TRUE) # coefficient
      ci.coef = format(attr(ci, "ci")*100, digits = 1, nsmall = 1, trim = TRUE) # CI
      # reformat the structure of the output so that everything is lined up
      coef.char = ifelse(length(strsplit(coef, "")[[1]]) == 4, coef, paste0(" ", coef))
      ci.low.char = ifelse(length(strsplit(ci.coef[1], "")[[1]]) == 4,
                           as.character(ci.coef[1]),
                           paste0(" ", ci.coef[1]))
      ci.hi.char = ifelse(length(strsplit(ci.coef[2], "")[[1]]) == 4,
                          as.character(ci.coef[2]),
                          paste0(" ", ci.coef[2]))
      asterisk = ifelse(weight_info[survey_years[j]] != "AB",
                        "*", "")
      # combine estimate and CI to be one string
      prop.results = paste0(coef.char, " (",
                            ci.low.char,", ",
                            ci.hi.char,")", asterisk)
      # append the jth results to the j-1 results (if j-1 = 0 then append to null)
      combined.results = c(combined.results, prop.results)
    }
    
    # add a trend column
    if(trend == "No"){
      final.table = rbind(final.table,
                          c(variable_description, combined.results))
    } else if(trend == "Yes"){
      hkcs.des = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                                weight = ~common_weight,
                                strata = as.formula(paste0("~", hkcs.strata)),
                                data = variables_datasets[[i]],
                                nest = TRUE)
      trend.formula = as.formula(paste0(variables_of_interest[i],
                                        " ~ time.v1"))
      trend.model = svyglm(trend.formula, family = quasibinomial(),
                           design = hkcs.des)
      result.summary = summary(trend.model)$coefficients
      if(result.summary["time.v1", "Pr(>|t|)"] <= 0.05){
        trend.result = ifelse(result.summary["time.v1", "Estimate"] > 0,
                       "Increase", "Decrease") 
      } else {
        trend.result = ""
      }
      final.table = rbind(final.table,
                          c(variable_description, combined.results, trend.result))
    }
  }
  
  # add column names to final table
  if(trend == "No"){
    colnames(final.table) <- c("Behavior of Interest", years) 
  } else if(trend == "Yes"){
    colnames(final.table) <- c("Behavior of Interest", years, "Trend")
  }
  
  # calculate sample size for each year
  years.ss =  data.frame(
    year = years,
    n = rep(NA, length(years)) 
  )
  for(i in 1:length(years)){
    years.ss$n[i] =  sum(hkcs.data$Survey_Year == years[i])
  }
  
  # return final table and sample size's
  return(list(years.ss, final.table))
}

# test run
test = hkcs.trend.analysis(district_id = 1140, data = cc_hs_final, hkcs.cluster = "CLASSID", hkcs.strata = "SchoolID",
                    moda.weight = "Wt_School_moda", modb.weight = "Wt_School_modb",
                    core.weight = "Wt_School", trend = "No", prefix = "QN_", variables = NULL)
test[[1]]
test[[2]]
