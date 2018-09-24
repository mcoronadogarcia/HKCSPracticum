# Load Packages -----------------------------------------------------------
# function that checks to make sure packages are installed
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 

# check if package "survey" is installed
if (!is.installed("survey")){install.packages("survey")}
if (!is.installed("knitr")){install.packages("knitr")}
if (!is.installed("xtable")){install.packages("xtable")}
if (!is.installed("kableExtra")){install.packages("kableExtra")}
if (!is.installed("dplyr")){install.packages("dplyr")}

# once installed, then load package
library('survey')
library("knitr")
library("xtable")
library("kableExtra")
library('dplyr')
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
  surpress = ". (.-.)"
  for(i in 1:length(variables_of_interest)){
    # create formula for variable of interest
    freq.formula = as.formula(paste0("~", variables_of_interest[i]))
    
    # stores the wiehgt information for variable of interest
    weight_info = variable_info[which(variable_info$QN_Variable == variables_of_interest[i]),]
    variable_description = as.character(weight_info$variable_description)
    # proportions per year
    combined.results = NULL
    for(j in 1:length(years)){
      # year dataset to be used
      year.data = variables_datasets[[i]][which(variables_datasets[[i]][, "Survey_Year"] == years[j]), ]
      
      # if then statement to surpress year information 
      if(nrow(year.data) < 30 | # there are fewer than 30 students responding or
         sum(year.data[, variables_of_interest[i]]) <= ceiling(nrow(year.data)*0.03) | # there are less than or equal to 3% of students responded yes
         # this would also mean that there are 0 students
         # responding yes or
         sum(year.data[, variables_of_interest[i]]) == nrow(year.data) # everyone responded yes
      ){ 
        prop.results = surpress
      } else {
        # create study design for data during jth year of ith variable
        hkcs.des.year = svydesign(id = as.formula(paste0("~", hkcs.cluster)),
                                  weight = ~common_weight,
                                  strata = as.formula(paste0("~", hkcs.strata)),
                                  data = year.data,
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
        # determine if ith variable during jth year needs an asterisk
        asterisk = ifelse(weight_info[survey_years[j]] != "AB",
                          "*", " ")
        # combine estimate and CI to be one string
        prop.results = paste0(coef.char, asterisk, "(",
                              ci.low.char," - ",
                              ci.hi.char,")")
      }
          
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
  colnames(years.ss) <- c("Survey Year", "Number of Respondents")
  
  # return final table and sample size's
  return(list(years.ss, final.table))
}
