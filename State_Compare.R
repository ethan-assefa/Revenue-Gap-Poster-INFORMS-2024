###################################################################
# Script Purpose: Transform state-level data to allow comparisons #
###################################################################

# Load necessary packages
library(tidyverse)

# Load in cleaned data
abs_state <- read_csv("abs_2017-2021_state_clean.csv")

# Create a new data frame for White, NH data
white_nh_state <- abs_state %>%
  filter(RaceEthn == "White, NH", Year == 2017) %>% 
  select(Year, State_Name, State_Abbr, RaceEthn, Avg_Rev_1k_Est, Avg_Rev_1k_StdErr)

# Create a new data frame for other racial groups
other_state <- abs_state %>%
  filter(RaceEthn == "Black/AA, NH",  Year == 2017) %>% 
  select(Year, State_Name, State_Abbr, RaceEthn, Avg_Rev_1k_Est, Avg_Rev_1k_StdErr)

# Merge data frames to have comparison of white v. other
abs_compare_state <- merge(white_nh_state, other_state, by = c("Year", "State_Name", "State_Abbr"))

# Rename columns
colnames(abs_compare_state) <- c("Year",
                                "State_Name",
                                "State_Abbr",
                                "RaceEthn_W", # All White, NH
                                "Avg_Rev_1k_Est_W", # Point estimate for White, NH
                                "Avg_Rev_1k_StdErr_W", # SE for White, NH
                                "RaceEthn_B", # All Black/AA, NH
                                "Avg_Rev_1k_Est_B", # Point estimate for each of Black/AA, NH
                                "Avg_Rev_1k_StdErr_B") # SE for each of Black/AA, NH

# Transform data for visualization
abs_compare_state <- abs_compare_state %>% 
  select(-c(RaceEthn_W, RaceEthn_B, Year)) %>% # Remove unecessary column
  mutate(Avg_Rev_WB_Ratio = Avg_Rev_1k_Est_W / Avg_Rev_1k_Est_B) # Create

# Export as csv to use in other places
write_csv(abs_compare_state, "abs_compare_state.csv")

