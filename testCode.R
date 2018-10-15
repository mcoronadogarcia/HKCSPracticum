source("/Users/mayracoronadogarcia/Documents/GitHub/HKCSPracticum/Trend_Analysis_Function.R")

cc_hs_final = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/cc_hs_final.csv')
hs_final = read.csv('/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/trend_hs_final.csv')

colnames(cc_hs_final)
colnames(hs_final)

# find all unique district names
district_names = as.character(unique(hs_final$District_Name))
# district_names = c("DENVER COUNTY 1",
#                    "CANON CITY RE-1",
#                    "GUNNISON WATERSHED RE1",
#                    "DELTA COUNTY 50(J)", 
#                    "IGNACIO 11 JT",
#                    "DURANGO 9-R",
#                    "MONTEZUMA-CORTEZ RE-1",
#                    "ARCHULETA COUNTY 50 JT",
#                    "ROARING FORK RE-1",
#                    "MESA COUNTY VALLEY 51"
#                    )

# create new dataset for ming with two problem matrix
df.ming = hs_final[which(
  hs_final$District_Name == "DURANGO 9-R" |
    hs_final$District_Name == "MONTEZUMA-CORTEZ RE-1"), ]
df.ming$District_Name =  as.character(df.ming$District_Name)
df.ming$SchoolID = as.numeric(df.ming$SchoolID)
df.ming$CLASSID = as.numeric(df.ming$CLASSID)

# write.csv(df.ming, file = "/Users/mayracoronadogarcia/Documents/Practicum/Data Analysis/hs_final_districtSubset.csv",
#           row.names = FALSE)

# save each individual district data
district.df =  list()
for(i in 1:length(district_names)){
  district.df[[i]] =  hs_final[which(
    hs_final$District_Name == district_names[i]), ]
}

# see how many schools are in each district
n.schools = list()
for(i in 1:length(district_names)){
  num.schools = length(unique(district.df[[i]]$SchoolID))
  district = district_names[i]
  n.schools[[i]] = c(district, num.schools)
}

# test function against each district

for(i in 1:length(district_names)){
  hkcs.results = hkcs.trend.analysis(district_name =  district_names[i],
                                     data = hs_final,
                                     hkcs.survey.year = "Survey_Year",
                                     hkcs.schoolid = "SchoolID",
                                     hkcs.classid = "CLASSID",
                                     moda.weight = "Weight_moda",
                                     modb.weight = "Weight_modb",
                                     core.weight = "Weight_both",
                                     trend = "Yes",
                                     prefix = "QN_",
                                     variables = NULL)
  print(hkcs.results)
}

hkcs.results = hkcs.trend.analysis(district_name =  district_names[3],
                                   data = district.df[[3]],
                                   hkcs.survey.year = "Survey_Year",
                                   hkcs.schoolid = "SchoolID",
                                   hkcs.classid = "CLASSID",
                                   moda.weight = "Weight_moda",
                                   modb.weight = "Weight_modb",
                                   core.weight = "Weight_both",
                                   trend = "Yes",
                                   prefix = "QN_",
                                   variables = NULL)
print(hkcs.results)