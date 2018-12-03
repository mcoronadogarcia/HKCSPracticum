# Load Data ---------------------------------------------------------------
# Load in Data for District Reports
district.data.all = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/trend_hs_final.csv')
district_names = as.character(unique(district.data.all$District_Name))

# Load in Data for State Report
state.fpc = read.csv("/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/State_trend_fpc_final.csv")
state.hkcs = read.csv("/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/State_Trend_HS_Final.csv")
adamsVars = read.csv("adamsCountyVariables.csv")

# Load HS snapshot variable information
variable_info = read.csv('/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/HS Snapshot trend variables.csv')

# Create Reports for Multiple Districts -----------------------------------
# set variables for District Data
hkcs.data.complete = district.data.all
state.var = NULL
fpc.data.var = NULL
fpc.var = NULL
hkcs.strata.var = NULL
survey.year.var = "Survey_Year"
classid.var = "CLASSID"
schoolid.var = "SchoolID"
moda.weight.var = "Weight_moda"
modb.weight.var = "Weight_modb"
core.weight.var = "Weight_both"

# Run for loop that creates reports for each district
# when create multiple reports, this is the only way to create them without
# running into a vector overload error that stops the process. 
for(i in 1:length(district_names)){
  for (district in district_names[i]) {
    rmarkdown::render(input = "/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/rMarkdownFiles/trend_report.Rmd",
                      output_format = "pdf_document",
                      output_file = paste0(district, "_", "District Report.pdf"),
                      output_dir = "/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/District_Report/")
  }
  i = i + 1
}

# Create State Report -----------------------------------------------------
# create a list of variables we have information for adams county
adamsVarsList <- as.character(adamsVars$QN_VARIABLE) 
snapShotVarsList <- as.character(variable_info$QN_Variable)

# which snap shot variables do we have that adams county has?
adamsVarLocation <- which(snapShotVarsList %in% adamsVarsList)
(variables_want <- snapShotVarsList[adamsVarLocation])

# set variables
hkcs.data.complete = state.hkcs
district = NULL
state.var = "Colorado"
fpc.data.var = state.fpc
fpc.var = "X_rate_"
hkcs.strata.var = "Strata_Year"
survey.year.var = "Survey_Year"
classid.var = NULL
schoolid.var = "SchoolID"
moda.weight.var = "Weight_moda"
modb.weight.var = "Weight_modb"
core.weight.var = "Weight_both"
prefix.var = NULL
variables.var = variables_want

rmarkdown::render(input = "/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/rMarkdownFiles/trend_report.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0(state.var, "_", "Trend Report.pdf"),
                  output_dir = "/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/State_Report/")
