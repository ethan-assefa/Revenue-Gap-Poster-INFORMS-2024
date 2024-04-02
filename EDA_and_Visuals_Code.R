#################################################################################
# Script Purpose: Apply Exploratory Data Analysis and Create Visuals for Poster #
#################################################################################

# Load necessary packages
library(tidyverse)
library(scales)
#library(forecast)
library(ggthemes)
library(kableExtra)
library(sjPlot)

# Load in cleaned data
abs_natl <- read_csv("abs_2017-2021_natl_clean.csv")
abs_state <- read_csv("abs_2017-2021_state_clean.csv")

# Creates custom label that 
label_custom<- function(x){
  paste0("$", format(x*1000 / 1e6, big.mark = ",", scientific = FALSE), " M")
}

# Create a new data frame for White, NH data
white_nh_data <- abs_natl %>%
  filter(RaceEthn == "White, NH") %>% 
  select(Year, RaceEthn, Avg_Rev_1k_Est, Avg_Rev_1k_StdErr)

# Create a new data frame for other racial groups
other_data <- abs_natl %>%
  filter(RaceEthn != "White, NH") %>% 
  select(Year, RaceEthn, Avg_Rev_1k_Est, Avg_Rev_1k_StdErr)

# Merge data frames to have comparison of white v. other
abs_compare_natl <- merge(white_nh_data, other_data, by = "Year")

# Rename columns
colnames(abs_compare_natl) <- c("Year", 
                                "RaceEthn.W", # All White, NH
                                "Avg_Rev_1k_Est.W", # Point estimate for White, NH
                                "Avg_Rev_1k_StdErr.W", # SE for White, NH
                                "RaceEthn.O", # The 5 other race categories
                                "Avg_Rev_1k_Est.O", # Point estimate for each of the other race categories
                                "Avg_Rev_1k_StdErr.O") # SE for each of the other race categories

# Plotting facet grid line graph
plot1 <- ggplot(data = abs_compare_natl, aes(x = Year)) +
  geom_line(aes(y = Avg_Rev_1k_Est.W, color = "White, NH"), size = 1.2) +
  geom_line(aes(y = Avg_Rev_1k_Est.O, color = RaceEthn.O), size = 1.2) +
  geom_point(aes(y = Avg_Rev_1k_Est.W), size = 1.5) +
  geom_point(aes(y = Avg_Rev_1k_Est.O), size = 1.5) +
  geom_ribbon(aes(ymin = Avg_Rev_1k_Est.W - Avg_Rev_1k_StdErr.W, ymax = Avg_Rev_1k_Est.W + Avg_Rev_1k_StdErr.W), fill = "#788FF5", alpha = 0.5) +
  geom_ribbon(aes(ymin = Avg_Rev_1k_Est.O - Avg_Rev_1k_StdErr.O, ymax = Avg_Rev_1k_Est.O + Avg_Rev_1k_StdErr.O, fill = RaceEthn.O), alpha = 0.5) +
  scale_y_continuous(labels = label_custom) +
  labs(x = 'Year', y = "Avg. Annual Revenue") +
  scale_color_manual(values = c("White, NH" = "#788FF5", 
                                "Hispanic, any race"="#107C97",
                                "AIAN, NH"="#F2BF27",
                                "Black/AA, NH"="#E63000",
                                "Asian, NH"="#77C956",
                                "NHOPI, NH"="#BC22C0"), name = "Race/Ethnicity") +
  scale_colour_tableau(name = "Race/Ethn.") +
  theme_fivethirtyeight() +
  facet_wrap(~ RaceEthn.O, ncol = 1, labeller = labeller(RaceEthn.O = function(x){paste0("White, NH vs. ", x)})) +
  guides(fill = 'none') + theme(text = element_text(size = 28))

ggsave("plot1.png", plot = plot1, width = 10, height = 22, dpi = 300)

# Calculate difference between other and white for each year
abs_compare_natl <- abs_compare_natl %>% 
  mutate(Avg_Rev_1k_Diff = Avg_Rev_1k_Est.W - Avg_Rev_1k_Est.O)

# Graph the difference in avg rev between white and other race over years
ggplot(data = abs_compare_natl, aes(x = Year, y = Avg_Rev_1k_Diff)) +
  geom_line(aes(color = RaceEthn.O), size = 1.2) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = label_custom) +
  labs(x = 'Year', y = "Difference in Avg. Annual Revenue") +
  scale_color_manual(values = c("White, NH" = "#788FF5", 
                                "Hispanic, any race"="#107C97",
                                "AIAN, NH"="#F2BF27",
                                "Black/AA, NH"="#E63000",
                                "Asian, NH"="#77C956",
                                "NHOPI, NH"="#BC22C0"), name = "Race/Ethnicity") +
  scale_colour_tableau(name = "Race/Ethnicity") +
  theme_fivethirtyeight() 



# Create linear prediction of next 5 years
# Calculate overall linear model
lm_model <- lm(Avg_Rev_1k_Diff ~ Year, data = abs_compare_natl)

# Create new data frame for future years
future_years <- tibble(Year = seq(min(abs_compare_natl$Year), max(abs_compare_natl$Year) + 5, by = 1))

# Predict y-values for future years
future_years <- cbind(future_years, as_tibble(predict(lm_model, newdata = future_years, interval = "confidence")))

# Create table for the poster
future_years_tbl <- future_years
future_years_tbl[,2:4] <- future_years_tbl[,2:4] * 1000

future_years_tbl <- future_years_tbl %>%
  mutate(`Margin Err.` = (upr - lwr) / 2) %>% 
  select(Year, fit, `Margin Err.`) %>% 
  rename(`Predicted Estimate`=fit)


# Graph the difference in avg rev between white and other race over years
plot2 <- ggplot(data = abs_compare_natl, aes(x = Year, y = Avg_Rev_1k_Diff)) +
  geom_line(aes(color = RaceEthn.O), size = 1.2) +
  geom_point(size = 1.5) +
  geom_line(data = future_years, aes(x = Year, y = fit), linetype = "dashed", color = "black", size = 1) +
  geom_ribbon(data = future_years, aes(x = Year, y = fit, ymin = lwr, ymax = upr), fill = "gray", alpha = 0.3) +
  scale_y_continuous(labels = label_custom) +
  labs(x = 'Year', y = "Difference in Avg. Annual Revenue") +
  scale_color_manual(values = c("White, NH" = "#788FF5", 
                                "Hispanic, any race"="#107C97",
                                "AIAN, NH"="#F2BF27",
                                "Black/AA, NH"="#E63000",
                                "Asian, NH"="#77C956",
                                "NHOPI, NH"="#BC22C0"), name = "Race/Ethnicity") +
  scale_colour_tableau(name = "Race/Ethn.") +
  theme_fivethirtyeight() + theme(text = element_text(size = 28))

ggsave("plot2.png", plot = plot2, width = 18, height = 14, dpi = 300)

tab_model(lm_model)
