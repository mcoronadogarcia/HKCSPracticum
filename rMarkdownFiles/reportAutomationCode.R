# Load Data ---------------------------------------------------------------
hkcs.data.complete = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/trend_hs_final.csv')
district_names = as.character(unique(hkcs.data.complete$District_Name))

# Set Variables -----------------------------------------------------------
survey.year.var = "Survey_Year"
classid.var = "CLASSID"
schoolid.var = "SchoolID"
moda.weight.var = "Weight_moda"
modb.weight.var = "Weight_modb"
core.weight.var = "Weight_both"

# Create Report for Each District -----------------------------------------
for(i in 1:length(district_names)){
  j = i + 2
  if(j > length(district_names)){
    j = length(district_names)
  }
  for (district in district_names[i:j]) {
    rmarkdown::render(input = "/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/rMarkdownFiles/trend_test_report.Rmd",
                      output_format = "pdf_document",
                      output_file = paste0(district, "_", "District Report.pdf"),
                      output_dir = "/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/District_Report/")
  }
  i = j + 1
}

