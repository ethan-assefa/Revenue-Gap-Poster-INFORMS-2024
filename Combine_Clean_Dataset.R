########################################################################################
# Script Purpose: Create pipeline for bringing in census data and cleaning for our use #
########################################################################################

# Load necessary packages
library(tidyverse)
library(censusapi) # Install if necessary
library(readr)

# Loads in the census api key
census_api <- Sys.getenv("CENSUS_API_KEY")


#################################################
#### Step 1: Use Census APIs to pull in data ####
#################################################


###########################################################
# DONT NEED TO RUN THIS PART UNLESS CHANGING TABLE CONTENT

# Provides list of avaliable APIs
# meta_info <- listCensusApis()

# Gives the variable information for the ABS survey (for 2018 but variables carry over the years)
#abs_var_2018 <- listCensusMetadata(name="abscs", vintage = 2018)

# DONT NEED TO RUN THIS PART UNLESS CHANGING TABLE CONTENT
###########################################################

# Create empty table to append data in loop
all_abs_state <- tibble()

# Loops through each year of interest and pulls state data, combining into single table (FYI this takes a while to run)
for (yr in 2017:2021){
  # Pulls the current year data
  current_abs <- getCensus(name="abscs", 
                           vars = c("GEOCOMP", "GEO_ID", "YEAR", "STATE", # Location info
                                    "ETH_GROUP", "ETH_GROUP_LABEL", "RACE_GROUP", "RACE_GROUP_LABEL", "NAICS2017", "NAICS2017_LABEL", "EMP", "EMP_S", # Business characteristics
                                    "PAYANN", "PAYANN_S", "RCPPDEMP", "RCPPDEMP_S", "RCPPDEMP_F", "RCPSZFI", "FIRMPDEMP", "FIRMPDEMP_S"), # Profit info
                           region = "state:*",
                           vintage = yr, 
                           key = census_api)
  # Appends to full table
  all_abs_state <- bind_rows(all_abs_state, current_abs)
}

# Create empty table to append data in loop
all_abs_natl <- tibble()

# Loops through each year of interest and pulls national data, combining into single table (FYI this takes a while to run)
for (yr in 2017:2021){
  # Pulls the current year data
  current_abs <- getCensus(name="abscs", 
                           vars = c("GEOCOMP", "GEO_ID", "YEAR", # Location info
                                    "ETH_GROUP", "ETH_GROUP_LABEL", "RACE_GROUP", "RACE_GROUP_LABEL", "NAICS2017", "NAICS2017_LABEL", "EMP", "EMP_S", # Business characteristics
                                    "PAYANN", "PAYANN_S", "RCPPDEMP", "RCPPDEMP_S", "RCPPDEMP_F", "RCPSZFI", "FIRMPDEMP", "FIRMPDEMP_S"), # Profit info
                           region = "us",
                           vintage = yr, 
                           key = census_api)
  # Appends to full table
  all_abs_natl <- bind_rows(all_abs_natl, current_abs)
}

####################################################
#### Step 2: Clean data for greater readability ####
####################################################


# Remove unnecessary column
# For state data
all_abs_state <- all_abs_state %>% select(-STATE)

# For national data
all_abs_natl <- all_abs_natl %>% select(-us)

# Gives each column a name that matches meaning [all estimates end with "_Est"; all standard error measures end with "_StdErr"]
# For state data
colnames(all_abs_state) <- c("State", # state
                       "Geo_ID_Component", # GEOCOMP
                       "Geo_ID_Code", # GEO_ID
                       "Year", # YEAR
                       "Ethnicity_Code", # ETH_GROUP
                       "Ethnicity_Label", # ETH_GROUP_LABEL
                       "Race_Code", # RACE_GROUP
                       "Race_Label", # RACE_GROUP_LABEL
                       "Industry_NAICS_Code", # NAICS2017
                       "Industry_NAICS_Label", # NAICS2017_LABEL
                       "Num_Employee_Est", # EMP
                       "Num_Employee_StdErr", # EMP_S
                       "Ann_Payroll_1k_Est", # PAYANN
                       "Ann_Payroll_1k_StdErr", # PAYANN_S
                       "Revenue_1k_Est", # RCPPDEMP
                       "Revenue_1k_StdErr", # RCPPDEMP_S
                       "Revenue_1k_Flag", # RCPPDEMP_F
                       "Revenue_1k_Code", # RCPSZFI
                       "Num_Firms_Est", # FIRMPDEMP
                       "Num_Firm_StdErr" # FIRMPDEMP_S
                       )

# For national data
colnames(all_abs_natl) <- c("Geo_ID_Component", # GEOCOMP
                             "Geo_ID_Code", # GEO_ID
                             "Year", # YEAR
                             "Ethnicity_Code", # ETH_GROUP
                             "Ethnicity_Label", # ETH_GROUP_LABEL
                             "Race_Code", # RACE_GROUP
                             "Race_Label", # RACE_GROUP_LABEL
                             "Industry_NAICS_Code", # NAICS2017
                             "Industry_NAICS_Label", # NAICS2017_LABEL
                             "Num_Employee_Est", # EMP
                             "Num_Employee_StdErr", # EMP_S
                             "Ann_Payroll_1k_Est", # PAYANN
                             "Ann_Payroll_1k_StdErr", # PAYANN_S
                             "Revenue_1k_Est", # RCPPDEMP
                             "Revenue_1k_StdErr", # RCPPDEMP_S
                             "Revenue_1k_Flag", # RCPPDEMP_F
                             "Revenue_1k_Code", # RCPSZFI
                             "Num_Firms_Est", # FIRMPDEMP
                             "Num_Firm_StdErr" # FIRMPDEMP_S
                             )

# Recode variables as needed to change number to description
# Mapping for states
state_codes <- c("01" = "Alabama", "02" = "Alaska", "04" = "Arizona", "05" = "Arkansas", "06" = "California", "08" = "Colorado",
                 "09" = "Connecticut", "10" = "Delaware", "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia",
                 "15" = "Hawaii", "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa", "20" = "Kansas",
                 "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan",
                 "27" = "Minnesota", "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", "32" = "Nevada",
                 "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico", "36" = "New York", "37" = "North Carolina",
                 "38" = "North Dakota", "39" = "Ohio", "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island",
                 "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont",
                 "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming", "72" = "Puerto Rico")

# Apply the mapping to our combined table
all_abs_state$State_Name <- state_codes[as.character(all_abs_state$State)]

# Reorder the State_Name column so it is next to the codes
all_abs_state <- all_abs_state %>% select(State, State_Name, everything())

#########################################
#### Step 3: Filter to proper groups ####
#########################################

# Filter to only the totals
# For state data
abs_state_totals <- all_abs_state %>% 
  filter(Industry_NAICS_Code =="00", # Totals across industries
         `Revenue_1k_Code` == "001") # Totals across all revenue brackets

# For national data
abs_natl_totals <- all_abs_natl %>% 
  filter(Industry_NAICS_Code =="00", # Totals across industries
         `Revenue_1k_Code` == "001") # Totals across all revenue brackets

# Filter to the appropriate race/ethnicity groups (White, NH; Black, NH; Asian, NH; NHPI, NH; AIAN, NH; Hispanic, any race)
# For state data
abs_state_totals <- abs_state_totals %>% 
  filter((Race_Label=="Black or African American" & Ethnicity_Label=="Non-Hispanic") | 
         (Race_Label=="White" & Ethnicity_Label=="Non-Hispanic") |
         (Race_Label=="Asian" & Ethnicity_Label=="Non-Hispanic") |
         (Race_Label=="Native Hawaiian and Other Pacific Islander" & Ethnicity_Label=="Non-Hispanic") |
         (Race_Label=="American Indian and Alaska Native" & Ethnicity_Label=="Non-Hispanic") |
         (Race_Label=="Total" & Ethnicity_Label=="Hispanic"))

# For national data
abs_natl_totals <- abs_natl_totals %>% 
  filter((Race_Label=="Black or African American" & Ethnicity_Label=="Non-Hispanic") | 
           (Race_Label=="White" & Ethnicity_Label=="Non-Hispanic") |
           (Race_Label=="Asian" & Ethnicity_Label=="Non-Hispanic") |
           (Race_Label=="Native Hawaiian and Other Pacific Islander" & Ethnicity_Label=="Non-Hispanic") |
           (Race_Label=="American Indian and Alaska Native" & Ethnicity_Label=="Non-Hispanic") |
           (Race_Label=="Total" & Ethnicity_Label=="Hispanic"))

#########################################################
#### Step 4: Convert Data Type & Transform Variables ####
#########################################################

# Combine race and ethnicity columns
# For state data
abs_state_totals <- abs_state_totals %>% mutate(RaceEthn=paste0(Race_Label, ", ", Ethnicity_Label), .after = Race_Label) %>% 
  select(-c(Race_Label, Race_Code, Ethnicity_Label, Ethnicity_Code, Industry_NAICS_Code, Industry_NAICS_Label, 
            Geo_ID_Component, Geo_ID_Code, State, Revenue_1k_Code)) # Removes all uneeded columns

# For national data
abs_natl_totals <- abs_natl_totals %>% mutate(RaceEthn=paste0(Race_Label, ", ", Ethnicity_Label), .after = Race_Label) %>% 
  select(-c(Race_Label, Race_Code, Ethnicity_Label, Ethnicity_Code, Industry_NAICS_Code, Industry_NAICS_Label, 
            Geo_ID_Component, Geo_ID_Code, Revenue_1k_Code)) # Removes all uneeded columns

# Convert character to numeric where appropriate
# For state data
abs_state_totals[,c(4:9, 11, 12)] <- lapply(abs_state_totals[, c(4:9, 11, 12)], function(x) as.numeric(as.character(x)))

# For national data
abs_natl_totals[,c(3:8, 10, 11)] <- lapply(abs_natl_totals[, c(3:8, 10, 11)], function(x) as.numeric(as.character(x)))

# Convert group variables to factor data type
# For state data
abs_state_totals[,c(1,3)] <- lapply(abs_state_totals[,c(1,3)], function(x) as.factor(x))

# For national data
abs_natl_totals[,2] <- lapply(abs_natl_totals[,2], function(x) as.factor(x))

# Recode the race ethnicity variable
# For state data
abs_state_totals$RaceEthn <- recode(abs_state_totals$RaceEthn,
                        "American Indian and Alaska Native, Non-Hispanic"          = "AIAN, NH",
                        "Asian, Non-Hispanic"                                      = "Asian, NH",
                        "Black or African American, Non-Hispanic"                  = "Black/AA, NH",
                        "Native Hawaiian and Other Pacific Islander, Non-Hispanic" = "NHOPI, NH",
                        "Total, Hispanic"                                          = "Hispanic, any race",
                        "White, Non-Hispanic"                                      = "White, NH")

# For national data
abs_natl_totals$RaceEthn <- recode(abs_natl_totals$RaceEthn,
                                    "American Indian and Alaska Native, Non-Hispanic"          = "AIAN, NH",
                                    "Asian, Non-Hispanic"                                      = "Asian, NH",
                                    "Black or African American, Non-Hispanic"                  = "Black/AA, NH",
                                    "Native Hawaiian and Other Pacific Islander, Non-Hispanic" = "NHOPI, NH",
                                    "Total, Hispanic"                                          = "Hispanic, any race",
                                    "White, Non-Hispanic"                                      = "White, NH")

# Create new column with mappings for state abbreviations
abs_state_totals <- abs_state_totals %>% mutate(State_Abbr=State_Name, .after = State_Name)

abs_state_totals$State_Abbr <- recode(abs_state_totals$State_Abbr,
                                      "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR", "California" = "CA",
                                      "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA",
                                      "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
                                      "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
                                      "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC",
                                      "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA",
                                      "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN",
                                      "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
                                      "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY", "District of Columbia" = "D.C.")

# Create an average revenue variable (divide total revenue by number of firms)
# Also calculate approx. SE of average 
# For state data
abs_state_totals <- abs_state_totals %>% 
  mutate(Avg_Rev_1k_Est = Revenue_1k_Est/Num_Firms_Est,
         Avg_Rev_1k_StdErr = Avg_Rev_1k_Est * sqrt(((Revenue_1k_StdErr/Revenue_1k_Est)^2)+((Num_Firm_StdErr/Num_Firms_Est)^2)))

# For national data
abs_natl_totals <- abs_natl_totals %>% 
  mutate(Avg_Rev_1k_Est = Revenue_1k_Est/Num_Firms_Est,
         Avg_Rev_1k_StdErr = Avg_Rev_1k_Est * sqrt(((Revenue_1k_StdErr/Revenue_1k_Est)^2)+((Num_Firm_StdErr/Num_Firms_Est)^2)))

# Create an average payroll variable (divide total payroll by number of employees)
# Also calculate approx. SE of average 
# For state data
abs_state_totals <- abs_state_totals %>% 
  mutate(Avg_Ann_Payroll_1k_Est = Ann_Payroll_1k_Est/Num_Employee_Est,
         Avg_Ann_Payroll_1k_StdErr = Avg_Ann_Payroll_1k_Est * sqrt(((Ann_Payroll_1k_StdErr/Ann_Payroll_1k_Est)^2)+((Num_Employee_StdErr/Num_Employee_Est)^2)))

# For national data
abs_natl_totals <- abs_natl_totals %>% 
  mutate(Avg_Ann_Payroll_1k_Est = Ann_Payroll_1k_Est/Num_Employee_Est,
         Avg_Ann_Payroll_1k_StdErr = Avg_Ann_Payroll_1k_Est * sqrt(((Ann_Payroll_1k_StdErr/Ann_Payroll_1k_Est)^2)+((Num_Employee_StdErr/Num_Employee_Est)^2)))


####################################
#### Step 4: Output as CSV file ####
####################################

# Output our cleaned table as a csv file into R environment
# For state data
write_csv(abs_state_totals, "abs_2017-2021_state_clean.csv")

# For national data
write_csv(abs_natl_totals, "abs_2017-2021_natl_clean.csv")

########################################################################################
# Script Purpose: Create pipeline for bringing in census data and cleaning for our use #
########################################################################################

# Load necessary packages
library(tidyverse)
library(censusapi) # Install if necessary
library(readr)

# Loads in the census api key
census_api <- Sys.getenv("CENSUS_API_KEY")


#################################################
#### Step 1: Use Census APIs to pull in data ####
#################################################


###########################################################
# DONT NEED TO RUN THIS PART UNLESS CHANGING TABLE CONTENT

# Provides list of avaliable APIs
# meta_info <- listCensusApis()

# Gives the variable information for the ABS survey (for 2018 but variables carry over the years)
#abs_var_2018 <- listCensusMetadata(name="abscs", vintage = 2018)

# DONT NEED TO RUN THIS PART UNLESS CHANGING TABLE CONTENT
###########################################################

# Create empty table to append data in loop
all_abs <- tibble()

# Loops through each year of interest and pulls data, combining into single table (FYI this takes a while to run)
for (yr in 2017:2021){
  # Pulls the current year data
  current_abs <- getCensus(name="abscs", 
                           vars = c("GEOCOMP", "GEO_ID", "YEAR", "STATE", # Location info
                                    "ETH_GROUP", "ETH_GROUP_LABEL", "RACE_GROUP", "RACE_GROUP_LABEL", "NAICS2017", "NAICS2017_LABEL", "EMP", "EMP_S", # Business characteristics
                                    "PAYANN", "PAYANN_S", "RCPPDEMP", "RCPPDEMP_S", "RCPSZFI", "FIRMPDEMP", "FIRMPDEMP_S"), # Profit info
                           region = "state:*",
                           vintage = yr, 
                           key = census_api)
  # Appends to full table
  all_abs <- bind_rows(all_abs, current_abs)
}


####################################################
#### Step 2: Clean data for greater readability ####
####################################################


# Remove unnecessary column
all_abs <- all_abs %>% select(-STATE)

# Gives each column a name that matches meaning [all estimates end with "_Est"; all standard error measures end with "_StdErr"]
colnames(all_abs) <- c("State", # state
                       "Geo_ID_Component", # GEOCOMP
                       "Geo_ID_Code", # GEO_ID
                       "Year", # YEAR
                       "Ethnicity_Code", # ETH_GROUP
                       "Ethnicity_Label", # ETH_GROUP_LABEL
                       "Race_Code", # RACE_GROUP
                       "Race_Label", # RACE_GROUP_LABEL
                       "Industry_NAICS_Code", # NAICS2017
                       "Industry_NAICS_Label", # NAICS2017_LABEL
                       "Num_Employee_Est", # EMP
                       "Num_Employee_StdErr", # EMP_S
                       "Ann_Payroll_$1k_Est", # PAYANN
                       "Ann_Payroll_$1k_StdErr", # PAYANN_S
                       "Revenue_$1k_Est", # RCPPDEMP
                       "Revenue_$1k_StdErr", # RCPPDEMP_S
                       "Revenue_$1k_Code", # RCPSZFI
                       "Num_Firms_Est", # FIRMPDEMP
                       "Num_Firm_StdErr" # FIRMPDEMP_S
                       )

# Recode variables as needed to change number to description
# Mapping for states
state_codes <- c("01" = "Alabama", "02" = "Alaska", "04" = "Arizona", "05" = "Arkansas", "06" = "California", "08" = "Colorado",
                 "09" = "Connecticut", "10" = "Delaware", "11" = "District of Columbia", "12" = "Florida", "13" = "Georgia",
                 "15" = "Hawaii", "16" = "Idaho", "17" = "Illinois", "18" = "Indiana", "19" = "Iowa", "20" = "Kansas",
                 "21" = "Kentucky", "22" = "Louisiana", "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts", "26" = "Michigan",
                 "27" = "Minnesota", "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", "32" = "Nevada",
                 "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico", "36" = "New York", "37" = "North Carolina",
                 "38" = "North Dakota", "39" = "Ohio", "40" = "Oklahoma", "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island",
                 "45" = "South Carolina", "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont",
                 "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", "55" = "Wisconsin", "56" = "Wyoming", "72" = "Puerto Rico")

# Apply the mapping to our combined table
all_abs$State_Name <- state_codes[as.character(all_abs$State)]

# Reorder the State_Name column so it is next to the codes
all_abs <- all_abs %>% select(State, State_Name, everything())


####################################
#### Step 3: Output as CSV file ####
####################################

# Output our cleaned table as a csv file into R environment
write_csv(all_abs, "abs_2017-2021_data_clean.csv")
