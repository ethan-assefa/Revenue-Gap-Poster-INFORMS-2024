# Ethan Assefa
## Analysis of the US Census: Nonemployer Statistics by Demographics (NES-D)
### Source Link: https://www.census.gov/programs-surveys/abs/data/nesd.2019.List_2041042974.html#list-tab-List_2041042974


################################## Packages ################################


library(tidyverse) # For useful functions in tidyverse environment
library(naniar) # Getting a visualization of missing values
library(readxl) # Allows excel files to be imported
library(scales) # For working with different units
library(reshape2) # For converting long/wide table formats
library(urbnmapr) # Provides geometries for maps

################################ Data Cleaning #############################


#########################
#### Import Datasets ####
#########################

# Imports in Receipt Data (NES-D 2017); skips first 2 rows (title and subtitle)
NESD_2017_Reciepts <- read_excel("NES-D_Tbl2_Reciepts_2017.xlsx", skip = 2, col_names = T)

# Imports in Receipt Data (NES-D 2019); skips first row (title); only selects same columns as in NESD 2017
NESD_2019_Reciepts <- read_csv("NES-D_Tbl2_Reciepts_Data_2019.csv", skip = 1, col_names = T, 
                               col_select = -c(1,3,5,7,9,11,13,16,17,18,19,22:27))

#########################
#### Remove Unneeded ####
#########################

# Removing columns not of interest
NESD_2017_Reciepts <- NESD_2017_Reciepts[,-c(2,10)] # NAICS codes column & Noise Flag column

# Removes last couple rows from 2017 NES-D (which are notes)
NESD_2017_Reciepts <- NESD_2017_Reciepts[-c(330642:330654),]

# Remove micro area and metro areas from 2017 NES-D locations
NESD_2017_Reciepts <- NESD_2017_Reciepts[!grepl("area", NESD_2017_Reciepts$`Geographic Area Name`, ignore.case = TRUE), ]

############################
#### Match Column Names ####
############################

# Rename columns in NES-D 2017
NESD_2017_Reciepts <- NESD_2017_Reciepts %>% 
  rename(Location = `Geographic Area Name`,
         `NAICS_Code` = `Meaning of 2017 NAICS Code`, 
         Sex = `Meaning of Sex Code`,
         Ethnicity = `Meaning of Ethnicity Code`,
         Race = `Meaning of Race Code`,
         Veteran_Status = `Meaning of Veteran Code`,
         Firm_Size = `Sales, Value of Shipments, or Revenue Size of Firm`,
         NonEmployer_Firms_Number = `Number of Nonemployer Firms`, 
         `Sales_Values_Rev` = `Sales, Value of Shipments, or Revenue of Nonemployer Firms ($1,000)`)

# Rename columns in NES-D 2019
NESD_2019_Reciepts <- NESD_2019_Reciepts %>% 
  rename(Location = `Geographic Area Name`,
         `NAICS_Code` = `Meaning of NAICS Code`, 
         Sex = `Meaning of Sex code`,
         Ethnicity = `Meaning of Ethnicity code`,
         Race = `Meaning of Race code`,
         Veteran_Status = `Meaning of Veteran code`,
         Firm_Size = `Meaning of Sales, value of shipments, or revenue size of firms code`,
         NonEmployer_Firms_Number = `Number of nonemployer firms`, 
         `Sales_Values_Rev` = `Sales, value of shipments, or revenue of nonemployer firms ($1,000)`)

######################
#### Data Merging ####
######################

# Merge the 2017 and 2019 dataset into a single dataset
NESD_Merge_Reciepts <- rbind(NESD_2017_Reciepts, NESD_2019_Reciepts)

########################
#### Re-code Values ####
########################

# "S" - Data is unreliable (does not meet publication standards) 
NESD_Merge_Reciepts[NESD_Merge_Reciepts=="S"] <- NA

# "N" - Not available or not comparable
NESD_Merge_Reciepts[NESD_Merge_Reciepts=="N"] <- NA

# "X" - Not applicable
NESD_Merge_Reciepts[NESD_Merge_Reciepts=="X"] <- NA

#############################
#### Data Transformation ####
#############################

# Changes NonEmployer_Firms_Number variable from character to numeric data type
NESD_Merge_Reciepts$NonEmployer_Firms_Number <- gsub(",", "", NESD_Merge_Reciepts$NonEmployer_Firms_Number)
NESD_Merge_Reciepts$NonEmployer_Firms_Number <- as.numeric(NESD_Merge_Reciepts$NonEmployer_Firms_Number)

# Changes Sales_Values_Rev variable from character to numeric data type
NESD_Merge_Reciepts$`Sales_Values_Rev` <- gsub(",", "", NESD_Merge_Reciepts$`Sales_Values_Rev`)
NESD_Merge_Reciepts$`Sales_Values_Rev` <- as.numeric(NESD_Merge_Reciepts$`Sales_Values_Rev`)

# Converts the Sales_Values_Rev variable (by $1,000s) to the full amount
NESD_Merge_Reciepts <- NESD_Merge_Reciepts %>% 
  mutate(`Actual_Sales_Values_Rev`=`Sales_Values_Rev` * 1000)

# Create vector of the categorical vaiable names
cat_vars <- c("Location", "NAICS_Code", "Sex", "Ethnicity", "Race", "Veteran_Status", "Firm_Size")

# Changes all categorical variables to factor data type
NESD_Merge_Reciepts[, cat_vars] <- lapply(NESD_Merge_Reciepts[, cat_vars], factor)

# Recode the race groups
NESD_Merge_Reciepts$Race <- fct_recode(NESD_Merge_Reciepts$Race, 
                                       `AIAN` = "American Indian and Alaska Native", 
                                       `NHOPI` = "Native Hawaiian and Other Pacific Islander", 
                                       `Black/AA` = "Black or African American")


# Collapse the Firm_Size factor to two groups: small business (< $1 m.) and large business (>= $1 m.)
NESD_Merge_Reciepts$Firm_Size <- fct_collapse(NESD_Merge_Reciepts$Firm_Size, 
                                              `Small Business`=c("Firms with sales/receipts of less than $5,000", 
                                                                 "Firms with sales/receipts of $5,000 to $9,999", 
                                                                 "Firms with sales/receipts of $10,000 to $24,999", 
                                                                 "Firms with sales/receipts of $25,000 to $49,999", 
                                                                 "Firms with sales/receipts of $50,000 to $99,999", 
                                                                 "Firms with sales/receipts of $100,000 to $249,999", 
                                                                 "Firms with sales/receipts of $250,000 to $499,999", 
                                                                 "Firms with sales/receipts of $500,000 to $999,999"),
                                              `Large Business`=c("Firms with sales/receipts of $1,000,000 or more"))


####################### Exploratory Data Analysis ##########################


# Visualize proportion of missing data
vis_miss(NESD_Merge_Reciepts,warn_large_data = FALSE)

#############################
#### Create Data Subsets ####
#############################

# Subsets for Black or White-owned businesses (counting only totals for other categories)
NESD_BlackWhiteData <- NESD_Merge_Reciepts %>% 
  subset((Race=="Black/AA" | Race=="White") & Sex=="Total" & Ethnicity=="Total" & Veteran_Status=="Total")

# Considers only the national totals for Black & White businesses across all industries
NESD_BlackWhiteData_Natl <- NESD_BlackWhiteData %>% 
  subset(`NAICS_Code`=="Total for all sectors" & Location=="United States")

# Subset to all race groups nationally (counting only totals for other categories)
NESD_RaceData_Natl <- NESD_Merge_Reciepts %>% 
  subset(Sex=="Total" & Ethnicity=="Total" & Veteran_Status=="Total" & NAICS_Code=="Total for all sectors" & Location=="United States")

# Groups by location, year, and race; adding the sums of firm number and revenue
NESD_RaceData_Natl <- aggregate(cbind(`Actual_Sales_Values_Rev`, `NonEmployer_Firms_Number`) ~ Location + Race + Year, data = NESD_RaceData_Natl, sum)

# Creates new column with totals so we can determine percentages
NESD_RaceData_Natl$Total_Rev <- NESD_RaceData_Natl$`Actual_Sales_Values_Rev`[NESD_RaceData_Natl$Race=="Total"]
NESD_RaceData_Natl$Total_Firms <- NESD_RaceData_Natl$`NonEmployer_Firms_Number`[NESD_RaceData_Natl$Race=="Total"]

# Adds new columns with percentages
NESD_RaceData_Natl <- NESD_RaceData_Natl %>% 
  mutate(Revenue_Percent=(`Actual_Sales_Values_Rev`/Total_Rev)*100, Firms_Percent=(NonEmployer_Firms_Number/Total_Firms)*100)

##################################
#### National Totals & Counts ####
##################################

# Creates table of the totals for Black & White businesses by year
Totals <- NESD_BlackWhiteData_Natl %>% group_by(Year, Race) %>% 
  summarise(`Total Annual Revenue`=sum(Actual_Sales_Values_Rev, na.rm = T),
            `Total Number of Firms`=sum(NonEmployer_Firms_Number, na.rm = T)) %>% 
  mutate(`Avg. Annual Revenue`=`Total Annual Revenue`/`Total Number of Firms`)

# Converts to dollar value
Totals$`Total Annual Revenue` <- dollar(Totals$`Total Annual Revenue`)
Totals$`Avg. Annual Revenue` <- dollar(Totals$`Avg. Annual Revenue`)

# Adds commas to firm number value
Totals$`Total Number of Firms` <- comma(Totals$`Total Number of Firms`)

# Print out the totals
Totals

#######################################################################
#### Race/Minority/Ethnicity Revenue & Number of Firms Proportions ####
#######################################################################

# Removing uneeded columns
NESD_RaceNatl_Prop <- NESD_RaceData_Natl[,-c(1,4:7)]

# Melts the dataset down 
NESD_RaceNatl_Prop <- melt(NESD_RaceNatl_Prop, id.vars=c('Year','Race'))

# Renames Variable names
NESD_RaceNatl_Prop$variable <- fct_recode(NESD_RaceNatl_Prop$variable, `Revenue` = "Revenue_Percent", `Number of Firms` = "Firms_Percent")

# Removes redundant categories
NESD_RaceNatl_Prop <- NESD_RaceNatl_Prop %>% 
  subset(Race!="Total")

# Just Minority
NESD_MinorNatl_Prop <- NESD_RaceNatl_Prop %>% 
  subset(Race=="Minority" | Race=="Nonminority" | Race=="Equally minority/nonminority")

# Just Races
NESD_RaceNatl_Prop <- NESD_RaceNatl_Prop %>% 
  subset(Race!="Minority" & Race!="Nonminority" & Race!="Equally minority/nonminority")

# Create bar graph of percentages (minority/nonminorities)
MinorityProportions <- ggplot(NESD_MinorNatl_Prop, aes(x=Race, y=value, fill=variable)) +
  theme_bw() +
  geom_bar(stat='identity', position='dodge') +
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) +
  labs(fill = "Variable") +
  xlab("Minority Status") +
  ylab("Percentages") +
  geom_text(aes(label=percent(value, scale = 1)), position=position_dodge(width=0.9), vjust=-0.25) + 
  scale_fill_manual(values=c("darkgreen","steelblue"), labels=c("Proportion of Revenue", "Proportion of Firms")) +
  facet_wrap(Year ~ .) +
  theme_classic() +
  theme(panel.background = element_blank(), plot.background = element_blank(), 
        panel.grid.major.y = element_line(), panel.grid.minor.y = element_line(),
        legend.position = "top", legend.background = element_blank())

# Print plot
MinorityProportions
# Save the ggplot object as a PNG image
ggsave("MinorityProportions.png", plot = MinorityProportions, width = 10, height = 8, dpi = 300)

# Create bar graph of percentages (races)
RacesProportions <- ggplot(NESD_RaceNatl_Prop, aes(x=Race, y=value, fill=variable)) + 
  theme_bw() + 
  geom_bar(stat='identity', position='dodge') + 
  scale_y_continuous(breaks = seq(0, 100, by=10), limits = c(0,100)) + 
  labs(fill = "Variable") + 
  xlab("Race") + 
  ylab("Percentages") + 
  geom_text(aes(label=percent(value, scale = 1, accuracy = .1)), position=position_dodge(width=0.9), vjust=-0.25) + 
  scale_fill_manual(values=c("darkgreen","steelblue"), labels=c("Proportion of Revenue", "Proportion of Firms")) +
  facet_wrap(Year ~ .) +
  theme_classic() +
  theme(panel.background = element_blank(), plot.background = element_blank(), 
        panel.grid.major.y = element_line(), panel.grid.minor.y = element_line(),
        legend.position = "top", legend.background = element_blank())

# Print plot
RacesProportions
# Save the ggplot object as a PNG image
ggsave("RacesProportions.png", plot = RacesProportions, width = 10, height = 8, dpi = 300)


###############################
#### Avg. Revenue by State ####
###############################

# Removes national totals so only individual states are considered
NESD_BlackWhiteData_State <- NESD_BlackWhiteData %>% 
  subset(`NAICS_Code`=="Total for all sectors" & Location != "United States")

# Create table of info for businesses by state, year, race, and revenue range
ByState <- NESD_BlackWhiteData_State %>% 
  group_by(Year=Year, Race=Race, State=Location, `Firm_Size`=Firm_Size) %>% 
  summarize(`Total Revenue` = sum(`Actual_Sales_Values_Rev`, na.rm = T),
            `Total Number of Firms`= sum(NonEmployer_Firms_Number, na.rm = T)) %>% 
  mutate(`Avg. Revenue` = `Total Revenue`/`Total Number of Firms`)

# Removes all rows with Inf (usually caused when number of firms is 0; can't divide by 0)
ByState <- ByState %>% 
  filter_all(~!any(is.infinite(.)))

# Lists out the state abbreviations
# Creating a list with state names matched to abbreviations
State_abbrev <- list(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT",
  "Delaware" = "DE", "District of Columbia" = "DC", "Florida" = "FL",
  "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL",
  "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS", "Kentucky" = "KY",
  "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN",
  "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE",
  "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ",
  "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC",
  "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR",
  "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"
)

# Replaces full state names with abbreviated ones
ByState <- ByState %>%
  mutate(State_Full = tolower(State)) %>% 
  mutate(State = as.character(State_abbrev[State]))

# Create line graph of avg. revenue for small Black & White Businesses across States
BW_State_S_AvgRev <- ggplot(ByState %>% filter(Firm_Size=="Small Business"), aes(x=`State`, y=`Avg. Revenue`, group=Race)) +
  geom_line(aes(color=`Race`)) +
  geom_point() +
  xlab("State") +
  ylab("Amount ($)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = dollar) +
  facet_wrap(Year ~ .) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent"), plot.background = element_blank(), 
        panel.grid.major.y = element_line(), panel.grid.minor.y = element_line(),
        legend.position = "top", legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

# Print plot
BW_State_S_AvgRev
# Save the ggplot object as a PNG image
ggsave("BW_State_S_AvgRev.png", plot = BW_State_S_AvgRev, width = 22, height = 8, dpi = 300)

# Create line graph of avg. revenue for large Black & White Businesses across States
BW_State_L_AvgRev <- ggplot(ByState %>% filter(Firm_Size=="Large Business") %>% na.omit(), aes(x=`State`, y=`Avg. Revenue`, group=Race)) +
  geom_line(aes(color=`Race`)) +
  geom_point() +
  xlab("State") +
  ylab("Amount ($)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(labels = dollar) +
  facet_wrap(Year ~ .) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent"), plot.background = element_blank(), 
        panel.grid.major.y = element_line(), panel.grid.minor.y = element_line(),
        legend.position = "top", legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

# Print plot
BW_State_L_AvgRev
# Save the ggplot object as a PNG image
ggsave("BW_State_L_AvgRev.png", plot = BW_State_L_AvgRev, width = 22, height = 8, dpi = 300)


######################### US States Map #############################


# Download map data for US states
US_map <- urbnmapr::states

# Creates wide form table for small businesses; considers most recent data (2019)
S_ByStateWide <- ByState %>% 
  filter(Firm_Size=="Small Business") %>% 
  subset(Year == 2019) %>% 
  pivot_wider(names_from = c("Race", "Firm_Size"), 
              values_from = c("Total Revenue", "Total Number of Firms", "Avg. Revenue"))

# Calculates the differences between average revenues of Black/White small businesses
S_ByStateWide <- S_ByStateWide %>% 
  mutate(Rev_Diff = `Avg. Revenue_White_Small Business` - `Avg. Revenue_Black/AA_Small Business`,
         `Times_Greater` = `Avg. Revenue_White_Small Business` / `Avg. Revenue_Black/AA_Small Business`) %>% 
  select(State, State_Full, Year, Rev_Diff, Times_Greater)

# Merge the small Black & White Businesses data with the map data
BW_StateMap_S_AvgRev2019 <- left_join(x = US_map, y = S_ByStateWide, join_by("state_abbv" == "State"))

# Create choropleth map of revenue difference between Black and White States
RevDiff_Map <- ggplot(data = BW_StateMap_S_AvgRev2019, aes(x = long, y = lat, group = group, fill = Rev_Diff)) + 
  geom_polygon(color = "white", size = 0.5) +
  scale_fill_gradient(low = "steelblue", high = "black", name = "Revenue Diff. (2019)") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(panel.background = element_blank(), plot.background = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), panel.border = element_blank(),
        legend.position = "top", legend.background = element_blank(), legend.key.width = unit(1.2, "cm"))

# Print plot
RevDiff_Map
# Save the ggplot object as a PNG image
ggsave("RevDiff_Map.png", plot = RevDiff_Map, width = 8, height = 6, dpi = 300)

