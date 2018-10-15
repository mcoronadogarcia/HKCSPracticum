# Load Packages -----------------------------------------------------------
# function that checks to make sure packages are installed
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 

# check if package all necessary packages are is installed. If they aren't
# then it installs them. 
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


# Create Function to calcualte proportions and trend analysis -------------
# read in variable information
variable_info = read.csv('/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/HS Snapshot trend variables.csv')

hkcs.trend.analysis =  function(district_name,
                                data,
                                hkcs.survey.year,
                                hkcs.schoolid,
                                hkcs.classid,
                                moda.weight,
                                modb.weight,
                                core.weight,
                                prefix,
                                variables,
                                trend){
  # subset the data for the current district
  hkcs.data = data[which(data$District_Name == district_name), ]
  
  # Create a new time variable for trend analysis
  hkcs.data$time.v1 = (hkcs.data[, hkcs.survey.year] - 2013)/2
  
  # find unique years and order them in ascending order
  years_list = unique(hkcs.data[, hkcs.survey.year])
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
    # Only keep data where the variable of interest doesn't have any missing
    # responses. Also only keep the following columns: Variable of interest,
    # survey year, time variable, school id, class id, and weight variable.
    variables_df = hkcs.data[which(!is.na(hkcs.data[, variables_of_interest[i]])),
                             c(variables_of_interest[i], hkcs.survey.year,
                                 "time.v1", hkcs.schoolid, hkcs.classid,
                                 moda.weight, modb.weight, core.weight)] 
    # stores the weight information for variable of interest
    weight_info = variable_info[which(variable_info$QN_Variable == variables_of_interest[i]),]
    
    # creates common_weight and cluster variable for each year of the
    # ith variable
    variables_df$common_weight = NA
    variables_df$cluster = NA
    for(j in 1:length(years)){
      # find the rows for jth year
      loc = which(variables_df$Survey_Year == years[j])
      
      # determine what common weight to use for jth year by using the
      # information from weight_info. If "AB" is present then set common
      # weight to the core weight, if "A" is present then set common
      # weight to be moduale A weight, but if "B" is present then set
      # common weight to be moduale B weight.
      if(weight_info[survey_years[j]] == "AB"){
        variables_df$common_weight[loc] = variables_df[loc, core.weight]
      } else if(weight_info[survey_years[j]] == "A"){
        variables_df$common_weight[loc] = variables_df[loc, moda.weight]
      } else if(weight_info[survey_years[j]] == "B"){
        variables_df$common_weight[loc] = variables_df[loc, modb.weight]
      }
      
      # to create the cluster variable calculate the number of schools in a 
      # district. If the number of schools is 1 then set cluster to be 
      # class id, if the number of schools is greater than 1 then set
      # cluster to be school id. 
      num.school = length(unique(variables_df[loc,
                                              hkcs.schoolid]))
      if(num.school == 1){
        variables_df$cluster[loc] = paste0(variables_df[loc, hkcs.classid],
                                           "_",
                                           years[j])
      } else if(num.school > 1){
        variables_df$cluster[loc] = paste0(variables_df[loc, hkcs.schoolid],
                                           "_",
                                           years[j])
      }
    }
    
    # saves dataset in a list
    variables_datasets[[i]] = variables_df[, c(variables_of_interest[i],
                                               hkcs.survey.year, "time.v1",
                                               "cluster", "common_weight")]
  }
  
  # create table
  final.table = data.frame(
    'indicator' = rep(NA, length(variables_of_interest)),
    '2013' = rep(NA, length(variables_of_interest)),
    '2015' = rep(NA, length(variables_of_interest)),
    '2017' = rep(NA, length(variables_of_interest))
  )
  
  # add trend variable to dataset if they want the trend.
  if(trend == 'Yes'){
    final.table$trend = NA
  }
  
  # suppression result
  surpress = ". (.-.)"
  
  # define the year labels for the dataframe
  year.colnames = c("X2013", "X2015", "X2017")
  for(i in 1:length(variables_of_interest)){
    # create formula for variable of interest
    freq.formula = as.formula(paste0("~", variables_of_interest[i]))
    
    # stores the weight information for variable of interest
    weight_info = variable_info[which(variable_info$QN_Variable == variables_of_interest[i]),]
    
    # Stores variable description
    final.table[i, 'indicator'] = as.character(weight_info$variable_description)
    
    # calculate proportions per year
    for(j in 1:length(years)){
      # save the data for jth year to be used in for loop
      year.data = variables_datasets[[i]][which(variables_datasets[[i]][, "Survey_Year"] == years[j]), ]
      
      # If there are fewer than 30 students responding or less than 3, or
      # 0 students responded yes this would also mean that there are 0
      # students responding yes or everyone responded yes then surpress the
      # years proportion information else calculate the proportion of yes' 
      # for that year
      if(nrow(year.data) < 30 |
         sum(year.data[, variables_of_interest[i]]) < 3 | 
         sum(year.data[, variables_of_interest[i]]) == nrow(year.data) 
      ){ 
        final.table[i, year.colnames[j]] = surpress
      } else {
        # create study design for data during jth year of ith variable
        # where we use id equal to cluster, weight equal to common weight,
        # strata equal to survey_year and use the jth years data.
        hkcs.des.year = svydesign(id = ~cluster,
                                  weight = ~common_weight,
                                  strata = as.formula(paste0("~",
                                                             hkcs.survey.year)),
                                  data = year.data,
                                  nest = FALSE)
        # calculate the proportion estimates and CI for jth year of ith variable
        ci = svyciprop(freq.formula, hkcs.des.year, method = "xlogit")
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
        final.table[i, year.colnames[j]] = paste0(
          coef.char, asterisk, "(",
          ci.low.char," - ",
          ci.hi.char,")")
      }
    }
    
    # if district is requesting trend analysis, then trend = "Yes" and the
    # logistic model will be evaluated to see if the variable of interest 
    # has a significant linear trend. 
    if(trend == "Yes"){
      # create design variable using cluster, the common weight, survey year
      # and the variable's data set.
      hkcs.des = svydesign(id = ~cluster,
                           weight = ~common_weight,
                           strata = as.formula(paste0("~", 
                                                      hkcs.survey.year)),
                           data = variables_datasets[[i]],
                           nest = FALSE)
      # create formula using time.v1 as the time variable
      trend.formula = as.formula(paste0(variables_of_interest[i],
                                        " ~ time.v1"))
      
      # create the trend model using the trend formula, study design for the
      # variable, and the quasibinomial distribution.
      trend.model = svyglm(trend.formula, family = quasibinomial(link = "logit"),
                           design = hkcs.des)
      
      # Determine if the time variable of the trend model was a significant increase
      # or decrease. If the estimate is not significant (> 0.05) then send column 
      # a blank, however, if the estimate is significant and greater than zero,
      # then trend is set to "Increase" otherwise the trend is less than zero,
      # and we set trend to equal "Decease"
      result.summary = summary(trend.model)$coefficients
      if(result.summary["time.v1", "Pr(>|t|)"] <= 0.05){
        final.table[i, "trend"] = ifelse(result.summary["time.v1", "Estimate"] > 0,
                              "Increase", "Decrease") 
      } else {
        final.table[i, "trend"] = ""
      }
    }
  }
  
  # Rename column names to final table
  if(trend == "No"){
    colnames(final.table) <- c("Indicator", years) 
  } else if(trend == "Yes"){
    colnames(final.table) <- c("Indicator", years, "Trend")
  }
  
  # calculate sample size for each year
  years.ss =  data.frame(
    year = years,
    n = rep(NA, length(years)) 
  )
  for(i in 1:length(years)){
    years.ss$n[i] =  sum(hkcs.data[, hkcs.survey.year]== years[i])
  }
  colnames(years.ss) <- c("Survey Year", "Number of Respondents")
  
  # return final table and sample size's
  return(list(years.ss, final.table, district_name))
}
