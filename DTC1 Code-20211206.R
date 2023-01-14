#interested in what effect household educational attainment and the 
#characteristics of the local area has on profit
#coding

library(haven)
library(tidyverse)
library(scales) # formatting ggplot axes
library(boot) # K-fold cross validation methods


#Education Data ----
#create large data frame with all education data
general_education <- read_dta("Raw data/glss4_new/sec2a.dta")
career_education <- read_dta("Raw data/glss4_new/sec2b.dta")
literacy_apprenticeship <- read_dta("Raw data/glss4_new/sec2c.dta")


#join each of the data frames to have one large set of all education data
education_data <- general_education %>%
  select(-s1q23) %>% #get rid of unnecessary variables 
  left_join(career_education, by = c('nh', 'pid', 'clust')) %>%
  left_join(literacy_apprenticeship, by = c('nh', 'pid', 'clust')) %>%
  select(-(s2aq4:s1q23.x), -s1q23.y, -(s2cq1:s2cq6), -s2aq1, -(s2bq1:s2bq4), -(s2cq7:s2cq13),
         -(s2bq5:s2bq7)) #get rid of more unnecessary variables


#add in each type of education from no education to doctorate using if else to create true false values
education_data$no_education <- ifelse(education_data$s2aq3 == 1, 1, 0)
education_data$mslc_bece <- ifelse(education_data$s2aq3 == 2, 1, 0)
education_data$voc_comm <- ifelse(education_data$s2aq3 == 3, 1, 0)
education_data$o_level <- ifelse(education_data$s2aq3 == 4, 1, 0)
education_data$sss <- ifelse(education_data$s2aq3 == 5, 1, 0)
education_data$a_level <- ifelse(education_data$s2aq3 == 6, 1, 0)
education_data$teach_cert_b <- ifelse(education_data$s2aq3 == 7, 1, 0)
education_data$teach_cert_a <- ifelse(education_data$s2aq3 == 8, 1, 0)
education_data$nursing <- ifelse(education_data$s2aq3 == 9, 1, 0)
education_data$tech_prof_cert <- ifelse(education_data$s2aq3 == 10, 1, 0)
education_data$tech_prof_dip <- ifelse(education_data$s2aq3 == 11, 1, 0)
education_data$other <- ifelse(education_data$s2aq3 == 96, 1, 0)
education_data$certificate <- ifelse(education_data$s2bq8 == 1, 1, 0)
education_data$diploma <- ifelse(education_data$s2bq8 == 2, 1, 0)
education_data$bachelor <- ifelse(education_data$s2bq8 == 3, 1, 0)
education_data$masters <- ifelse(education_data$s2bq8 == 4, 1, 0)
education_data$doctorate <- ifelse(education_data$s2bq8 == 5, 1, 0)
education_data$other_tertiary <- ifelse(education_data$s2bq8 == 6, 1, 0)


#get rid of unnecessary variables and unite nh and clust to create a hhid_clust variable to simplify
education_data <- education_data %>%
  select(-(s2aq2:s2aq3), -s2bq8) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>%
  gather('education', 'true/false', no_education:other_tertiary) #%>%
#rename("true/false" = 'hhid_id') #rename variable to be more appropriately named


#convert household education per person to appropriate values for when we take the maximum 
#education per household, the values are as follows
#doctorate: 17
#masters: 16
#bachelors: 15
#diploma: 14
#certificate: 13
#other tertiary: 12
#tech_prof_dip: 11
#tech_prof_cert: 10
#nursing: 9
#teach_cert_a: 8
#teach_cert_b: 7
#a_level: 6
#sss: 5
#o_level: 4
#voc_comm: 3
#mslc_bece: 2
#other: 1
#no_education: 0

# re-categorized to 1:4 
highest_household_education_data <- education_data %>%
  mutate(highest_household_edu = case_when(education == 'doctorate' & `true/false` == 1 ~ 4,
                                           education == 'masters' & `true/false` == 1 ~ 4,
                                           education == 'bachelor' & `true/false` == 1 ~ 3,
                                           education == 'diploma' & `true/false` == 1 ~ 4,
                                           education == 'certificate' & `true/false` == 1 ~ 4,
                                           education == 'other_tertiary' & `true/false` == 1 ~ 4,
                                           education == 'tech_prof_dip' & `true/false` == 1 ~ 3,
                                           education == 'tech_prof_cert' & `true/false` == 1 ~ 3,
                                           education == 'nursing' & `true/false` == 1 ~ 3,
                                           education == 'teach_cert_a' & `true/false` == 1 ~ 3,
                                           education == 'teach_cert_b' & `true/false` == 1 ~ 3,
                                           education == 'a_level' & `true/false` == 1 ~ 2,
                                           education == 'sss' & `true/false` == 1 ~ 2,
                                           education == 'o_level' & `true/false` == 1 ~ 2,
                                           education == 'voc_comm' & `true/false` == 1 ~ 2,
                                           education == 'mslc_bece' & `true/false` == 1 ~ 1,
                                           education == 'other' & `true/false` == 1 ~ 1,
                                           education == 'no_education' & `true/false` == 1 ~ 0)) %>%
  na.omit(highest_household_education_data) #get rif of N/A values in data frame
highest_household_education_data <- highest_household_education_data %>% 
  group_by(hhid_clust) %>% #group by hhid_clust
  summarise(highest_household_education = max(highest_household_edu)) %>% #summarise the data by hhid_clust 
  #take the max value of each hhid_clust value
  arrange(hhid_clust) #arrange data by hhid_clust


#added in total school expenses 
school_expenses <- general_education %>%
  select(nh, clust, pid, s2aq13a) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>%
  group_by(hhid_clust) %>%
  na.omit() %>%
  summarise(total_household_school_expense = sum(s2aq13a)) %>%
  arrange(hhid_clust)


#added in scholarship amounts 
scholarship <- general_education %>%
  select(nh, clust, pid, s2aq16) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>%
  group_by(hhid_clust) %>%
  na.omit() %>%
  summarise(scholarship_amount = sum(s2aq16)) %>%
  arrange(hhid_clust)




#added in total educational expenses for each household
highest_household_education_data <- highest_household_education_data %>%
  left_join(school_expenses, by = 'hhid_clust') %>%
  left_join(scholarship, by = 'hhid_clust')
education_distribution <- factor(highest_household_education_data$highest_household_education)
summary.factor(education_distribution) #add in education summary statistics

education_model <- lm(highest_household_education ~
                        total_household_school_expense +
                        scholarship_amount,
                      data = highest_household_education_data)

summary(education_model) 

income <- read_dta("Raw data/glss4_new/aggregates/agg2.dta") %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>%
  select(-agri1, -(agri2:hhagdepn)) %>%
  rename('ag_income' = 'agri1c')



#This next section created variables for the household, respondent, and location variables ----

loc <- read_dta("Raw data/glss4_new/sec0a.dta") %>% #Region and location metrics
  select(region, district, nh, clust, loc5, ez) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) # Creates unique household id


hh <- read_dta("Raw data/glss4_new/sec1.dta") %>%  #Individual and household variables
  select(nh, pid, agey, clust) %>% # Personal identifier and age
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  mutate(pid_hhid_clust = paste(pid, clust, nh)) %>% # Creates unique household respondent id
  group_by(nh, clust, hhid_clust) %>% 
  summarise(avg_age = mean(agey), # Returns average age and count of household members
            hh_members = n())


hh_loc <- hh %>% #Joins location variables to each household
  left_join(loc, on = c(nh, clust, hhid_clust)) %>% 
  mutate(eco_zone = recode(ez,
                           "1" = "Coastal",
                           "2" = "Forest",
                           "3" = "Savannah")) %>% 
  mutate(loc5 = recode(loc5,
                       "1" = "Accra",
                       "2" = "Other Urban",
                       "3" = "Rural Coastal",
                       "4" = "Rural Forest",
                       "5" = "Rural Savannah")) %>% 
  mutate(region = recode(region,
                         "1" = "Western",
                         "2" = "Central", 
                         "3" = "Greater Accra",
                         "4" = "Eastern",
                         "5" = "Volta",
                         "6" = "Ashanti",
                         "7" = "Brong Ahafo",
                         "8" = "Northern",
                         "9" = "Upper East",
                         "10" = "Upper West")) %>% 
  mutate(rural_forest = case_when(loc5 == "Rural Forest" ~ 1, # Creates dummy variable for rural forest
                                  TRUE ~ 0)) %>% 
  mutate(rural_coastal = case_when(loc5 == "Rural Coastal" ~ 1, # Creates dummy variable for rural coastal
                                   TRUE ~ 0)) %>% 
  select(-ez)


# Health Variables ----
health <- read_dta("Raw data/glss4_new/sec3a.dta") %>% 
  select(nh,clust, s3aq1) %>% # Filters to illness/injury metric
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  mutate(illness_injury = case_when(s3aq1 != 1 ~ 1, # Metric originally coded with factors 0 to 4 
                                    TRUE ~ 0)) %>% # Created dummy with any factor > 0 illness, injury, or both
  select(-s3aq1) %>% 
  group_by(nh, clust, hhid_clust) %>% 
  summarise(illness_injury = sum(illness_injury)) %>% 
  mutate(illness_injury = case_when(illness_injury > 0 ~ 1,
                                    TRUE ~ 0)) # Summed dummy variables per household and re-coded to binary value


# Vaccination status of any household member variables ----
vaccination <- read_dta("Raw data/glss4_new/sec3b.dta") %>% 
  select(nh, clust, s3bq1) %>% # Subsets to vaccination status
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  mutate(vac = case_when(s3bq1 == 1 ~ 1,
                         TRUE ~ 0)) %>% # Re-coded 2 to 0
  select(-s3bq1) %>% 
  group_by(nh, clust, hhid_clust) %>% 
  summarise(vac = sum(vac)) %>% 
  mutate(vac = case_when(vac > 0 ~ 1,
                         TRUE ~ 0)) # Summed dummy variables per household and re-coded to binary value


# Housing-specific variables ----
housing <- read_dta("Raw data/glss4_new/sec7.dta") %>% 
  select(nh, s7bq1, s7cq6,s7dq2a, s7dq2b, s7dq8, clust) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  mutate(occ_own = case_when(s7bq1 == 1 ~ 1, TRUE ~ 0)) %>% # Dummy Variable for owning home
  mutate(occ_rent = case_when(s7bq1 == 2 ~ 1, TRUE ~ 0)) %>% # Dummy variable for renting home
  mutate(occ_rent_free = case_when(s7bq1 == 3 ~ 1, TRUE ~ 0)) %>% # Dummy variable for renting home for free
  mutate(occ_perch = case_when(s7bq1 == 4 ~ 1, TRUE ~ 0)) %>% #Dummy variable for perching home
  mutate(construct_cost = s7cq6) %>% # Renames construction cost variable
  mutate(source_of_water_distance_m = # Coverts water distances to meters
           case_when(s7dq2b == 1 ~ s7dq2a * 0.9144,
                     s7dq2b == 2 ~ s7dq2a * 1,
                     s7dq2b == 3 ~ s7dq2a * 1000,
                     s7dq2b == 4 ~ s7dq2a * 1609.433,
                     TRUE ~ 0)) %>% 
  select(-c(s7bq1, s7dq2a, s7dq2b)) %>% 
  mutate(main_light_source_electric = # Creates dummy variable for households using electricity
           case_when(s7dq8 == 1 ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(main_light_source_gen = # Creates dummy variable for households using generators
           case_when(s7dq8 == 2 ~ 1,
                     TRUE ~ 0)) %>% 
  mutate(main_light_source_kero = # Creates dummy variable for households using kerosene
           case_when(s7dq8 == 3 ~ 1,
                     TRUE ~ 0)) %>% 
  select(-c(s7cq6,s7dq8))


# Agriculture ----
agri_plot_rent <- read_dta("Raw data/glss4_new/sec8a1.dta") %>% 
  select(nh, s8aq3, s8aq14, clust) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>% 
  mutate(rented_out_acres = case_when(s8aq3 == "1" ~ s8aq14 * 1, # Converts rented out land to acres
                                      s8aq3 == "2" ~ s8aq14 * 0.25,
                                      s8aq3 == "3" ~ s8aq14 * (1/9),
                                      s8aq3 == "4" ~ s8aq14 * 0,
                                      TRUE ~ 0)) %>%
  mutate(rented_out_acres = replace(rented_out_acres, is.na(rented_out_acres), 0)) %>% 
  filter(s8aq3 != 8) %>% # Removes incorrectly inputted entry
  select(-c(s8aq3, s8aq14)) %>% # Removes unit non-converted rented out acre columns
  group_by(nh, clust, hhid_clust) %>% 
  summarise(rented_out_acres = sum(rented_out_acres)) #Sums total rented out acres


livestock <- read_dta("Raw data/glss4_new/sec8a2.dta") %>%
  select(nh, livstcd, s8aq22a, clust) %>% 
  mutate(total_livestock = s8aq22a) %>%  # Total livestock count
  mutate(hhid_clust = paste(nh, "_", clust)) %>% 
  filter(livstcd < 10) %>% # Removes non-livestock rows
  mutate(livstcd = recode(livstcd,
                          "1" = "Draught_animal",
                          "2" = "Cattle_incl_cow",
                          "3" = "Sheep",
                          "4" = "Goat",
                          "5" = "Pig",
                          "6" = "Rabbit",
                          "7" = "Chicken",
                          "8" = "Other_poultry",
                          "9" = "Other_livestock")) 


livestock %>% # Returns list of most commonly listed livestock
  group_by(livstcd) %>% 
  na_if("NA") %>% 
  drop_na() %>% 
  summarise(n()) %>% 
  rename(livestock_count = "n()") %>% 
  arrange(desc(livestock_count)) #most popular livestock: chicken, goats, sheep


livestock <- livestock %>% # Creates dummy variables for most popular livestock
  mutate(chicken = case_when(livstcd == "Chicken" ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(goat = case_when(livstcd == "Goat" ~ 1,
                          TRUE ~ 0)) %>% 
  mutate(sheep = case_when(livstcd == "Sheep" ~ 1,
                           TRUE ~ 0)) %>% 
  select(-c(livstcd, s8aq22a, total_livestock)) %>% # Removes factor columns for livestock
  group_by(nh, clust, hhid_clust) %>% 
  summarise(chicken = sum(chicken),
            goat = sum(goat),
            sheep = sum(sheep))


agri_equip <- read_dta("Raw data/glss4_new/sec8a3.dta") %>% # Agriculture equipment
  select(nh,s8aq35, clust) %>% # Returns value of agricultural equipment variable
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  rename(value_agr_equip = s8aq35) %>%  
  group_by(nh, clust, hhid_clust) %>% 
  summarise(value_agr_equip = sum(value_agr_equip)) # Sums total agricultural equipment value


agri_plot_farm <- read_dta("Raw data/glss4_new/sec8b.dta")%>% 
  select(nh, s8bq4a, s8bq4b,s8bq12a, s8bq12b, clust) %>%
  mutate(hhid_clust = paste(nh, "_", clust)) %>% # Creates unique household id
  mutate(farm_land_size_acres = case_when(s8bq4b == "1" ~ s8bq4a * 1, 
                                          s8bq4b == "2" ~ s8bq4a * 0.25,
                                          s8bq4b == "3" ~ s8bq4a * (1/9),
                                          TRUE ~ 0)) %>% # Converts total farmland (excl rented) to acres
  mutate(farm_land_size_acres = na_if(farm_land_size_acres, 0)) %>% 
  select(-s8bq4b) %>%
  drop_na(farm_land_size_acres) %>% 
  gather("crops", "crop_id", s8bq12a:s8bq12b) %>% # Creates single column of primary and secondary crops 
  select(-crops) %>% 
  mutate(crop = recode(crop_id,
                       "1" = "Cocoa",
                       "2" = "Coffee",
                       "3" = "Rubber",
                       "4" = "Coconut",
                       "5" = "Oil Palm",
                       "6" = "Plantain",
                       "7" = "Banana",
                       "8" = "Oranges",
                       "10" = "Wood",
                       "11" = "Cola nut",
                       "12" = "Kenef",
                       "13" = "Cotton",
                       "14" = "Groundnut/peanut",
                       "15" = "Tobacco",
                       "16" = "Pineapple",
                       "17" = "Sugar cane",
                       "18" = "Cassava",
                       "19" = "Yam",
                       "21" = "Potatoes",
                       "22" = "Maize",
                       "23" = "Rice",
                       "24" = "Guinea corn/sorghum/millet ropes",
                       "25" = "Tomatoes",
                       "26" = "Okro",
                       "27" = "Garden egg/Egg plant",
                       "28" = "Beans/peas",
                       "29" = "Pepper",
                       "30" = "Leafy vegetables",
                       "31" = "Other vegetables",
                       "32" = "Other crops",
                       "33" = "Onion",
                       "34" = "Avocado pear",
                       "35" = "Mango",
                       "36" = "Pawpaw", .default = "NA"
  )) %>% 
  select(-crop_id)


agri_plot_farm %>% # Returns list of most common crops
  group_by(crop) %>% 
  na_if("NA") %>% 
  drop_na() %>% 
  summarise(n()) %>% 
  rename(crop_count = "n()") %>% 
  arrange(desc(crop_count)) # Most popular crops: Cassava, Maize, Plantain, Cocoa


agri_plot_farm <- agri_plot_farm %>% # Create dummy variables for specific crops
  mutate(cassava = case_when(crop == "Cassava" ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(maize = case_when(crop == "Maize" ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(plantain = case_when(crop == "Plantain" ~ 1,
                              TRUE ~ 0)) %>% 
  mutate(cocoa = case_when(crop == "Cocoa" ~ 1,
                           TRUE ~ 0)) %>%
  mutate(banana = case_when(crop == "Banana" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(coffee = case_when(crop == "Coffee" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(tobacco = case_when(crop == "Tobacco" ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(oil_palm = case_when(crop == "Oil Palm" ~ 1,
                              TRUE ~ 0)) %>% 
  group_by(nh, clust, hhid_clust) %>% 
  summarise(farm_land_size_acres = sum(farm_land_size_acres), # Sums crop dummy variables for each household and
            cassava = sum(cassava),                           # recodes for binary values
            maize = sum(maize),
            plantain = sum(plantain),
            cocoa = sum(cocoa),
            banana = sum(banana),
            coffee = sum(coffee),
            tobacco = sum(tobacco),
            oil_palm = sum(oil_palm)
  ) %>%
  mutate(cassava = case_when(cassava > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(maize = case_when(maize > 0 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(plantain = case_when(plantain > 0 ~ 1, 
                              TRUE ~ 0)) %>%
  mutate(cocoa = case_when(cocoa > 0 ~ 1,
                           TRUE ~ 0)) %>% 
  mutate(banana = case_when(banana > 0 ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(coffee = case_when(coffee > 0 ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(tobacco = case_when(tobacco > 0 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(oil_palm = case_when(oil_palm > 0 ~ 1,
                              TRUE ~ 0))

# Agriculture aggregations and joined data sets ----

df_agg2 <- read_dta("Raw data/glss4_new/aggregates/agg2.dta") 
df_agg2_sub <- df_agg2 %>% 
  select(clust, nh, agri1c)  %>% # Creates unique household id
  mutate(hhid_clust = paste(nh, "_", clust)) %>% 
  rename(profit = agri1c) # Variable for total household agriculture income (corrected)

agri_hh_health <- hh_loc %>% # Joins health, vaccination, housing, plot rental, livestock, agriculture and farming data frames
  left_join(health, on = c(nh, clust, hhid_clust)) %>% 
  left_join(vaccination, on = c(nh, clust, hhid_clust)) %>%
  left_join(housing, on = c(nh, clust, hhid_clust)) %>%
  left_join(agri_plot_rent, on = c(nh, clust, hhid_clust)) %>%
  left_join(livestock, on = c(nh, clust, hhid_clust)) %>% 
  left_join(agri_equip, on = c(nh, clust, hhid_clust)) %>%
  left_join(agri_plot_farm, on = c(nh, clust, hhid_clust)) %>% 
  left_join(df_agg2_sub, on = c(nh, clust, hhid_clust)) %>% 
  filter(!is.na(farm_land_size_acres)) %>% 
  mutate(rented_out_acres = replace_na(rented_out_acres, 0)) %>% 
  mutate(total_farm_acres = farm_land_size_acres + rented_out_acres) %>% # Sums Farmland and rented out acres for total acreage variable 
  mutate(total_farm_acres = replace_na(total_farm_acres, 0)) %>% 
  filter(total_farm_acres > 0) %>% # Removes households with no farm acreage
  mutate(perc_farm_acres_rented = rented_out_acres/total_farm_acres) %>% # Creates percentage of total land acreage
  mutate(any_land_rented = case_when(perc_farm_acres_rented > 0 ~ 1,
                                     TRUE ~ 0)) %>% # Creates dummy variable for households with any rented land
  mutate(construct_cost_per_acre = construct_cost/total_farm_acres) %>% # Creates construction cost per total farm acres for each household
  mutate(profit_per_acre = profit/total_farm_acres) %>% # Creates metric for profit per acre
  filter(!is.na(total_farm_acres))

# Agriculture Modeling ----

# The purpose of this section was create a viable model for the agriculture variables
# that could then be added to education. Given the high number of variables, we wanted to get
# a better sense of which ones were statistically significant and contributed to the model

df_agri_sub <- agri_hh_health %>% # Subsets agriculture variables that will be used in models
  select(nh, 
         clust, 
         hhid_clust, 
         profit_per_acre,
         illness_injury,
         vac,
         source_of_water_distance_m,
         main_light_source_electric,
         chicken, 
         goat, 
         cassava,
         maize,
  ) %>% 
  drop_na() %>% 
  filter(profit_per_acre > 0) # Filters table to only farms that were profitable


ln_reg_prf_agri_nl <- lm(profit_per_acre ~ # Test agricultural model with no transformations
                           illness_injury +
                           vac +
                           source_of_water_distance_m +
                           main_light_source_electric +
                           chicken +
                           goat +
                           cassava +
                           maize,
                         data = df_agri_sub)

summary(ln_reg_prf_agri_nl)

ggplot(ln_reg_prf_agri_nl) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_prf_agri_nl)), bins = 50)

ggplot(ln_reg_prf_agri_nl) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_prf_agri_nl), resid(ln_reg_prf_agri_nl))) +
  xlab("Fitted") +
  ylab("Residuals") +
  geom_abline(slope = 0, color = "blue")


ln_reg_prf_agri_ply2 <- lm(profit_per_acre ~ # Test agriculture model with second degree polynomial
                             illness_injury +
                             vac +
                             source_of_water_distance_m +
                             I(source_of_water_distance_m^2) +
                             main_light_source_electric +
                             chicken +
                             goat +
                             cassava +
                             maize,
                           data = df_agri_sub)

summary(ln_reg_prf_agri_ply2)

ggplot(ln_reg_prf_agri_ply2) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_prf_agri_ply2)), bins = 50)

ggplot(ln_reg_prf_agri_ply2) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_prf_agri_ply2), resid(ln_reg_prf_agri_ply2))) +
  xlab("Fitted") +
  ylab("Residuals") +
  geom_abline(slope = 0, color = "blue")

ln_reg_prf_agri_ply3 <- lm(profit_per_acre ~ # Test agriculture model with third degree polynomial
                             illness_injury +
                             vac +
                             source_of_water_distance_m +
                             I(source_of_water_distance_m^3) +
                             main_light_source_electric +
                             chicken +
                             goat +
                             cassava +
                             maize,
                           data = df_agri_sub)

summary(ln_reg_prf_agri_ply3)

ggplot(ln_reg_prf_agri_ply3) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_prf_agri_ply3)), bins = 50)

ggplot(ln_reg_prf_agri_ply3) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_prf_agri_ply3), resid(ln_reg_prf_agri_ply3))) +
  xlab("Fitted") +
  ylab("Residuals") +
  geom_abline(slope = 0, color = "blue")


ln_reg_prf_agri_1 <- lm(log(profit_per_acre) ~ # Test log model on health variables
                          illness_injury +
                          vac,
                        data = df_agri_sub)

summary(ln_reg_prf_agri_1)

ln_reg_prf_agri_2 <- lm(log(profit_per_acre) ~ # Test log model adding in source of water
                          illness_injury +
                          vac +
                          source_of_water_distance_m,
                        data = df_agri_sub)

summary(ln_reg_prf_agri_2)

ln_reg_prf_agri_3 <- lm(log(profit_per_acre) ~ # Test log model adding in electricity variable
                          illness_injury +
                          vac +
                          source_of_water_distance_m +
                          main_light_source_electric,
                        data = df_agri_sub)

summary(ln_reg_prf_agri_3)

ln_reg_prf_agri <- lm(log(profit_per_acre) ~ # Test log model with livestock and crop variables
                        illness_injury +
                        vac +
                        source_of_water_distance_m +
                        main_light_source_electric +
                        chicken +
                        goat +
                        cassava +
                        maize,
                      data = df_agri_sub)

summary(ln_reg_prf_agri)

ggplot(ln_reg_prf_agri) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_prf_agri)), bins = 50)

ggplot(ln_reg_prf_agri) + # c=Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_prf_agri), resid(ln_reg_prf_agri))) +
  xlab("Fitted") +
  ylab("Residuals") +
  geom_abline(slope = 0, color = "blue")

df_agri_sub_lg <- agri_hh_health %>% # Variables for agriculture model 
  select(nh,                        # with zeros removed from source of water distance  
         clust,                     # to test effect of log on variable
         hhid_clust, 
         profit_per_acre,
         illness_injury,
         vac,
         source_of_water_distance_m,
         main_light_source_electric,
         chicken, 
         goat, 
         cassava,
         maize,
  ) %>% 
  drop_na() %>% 
  filter(profit_per_acre > 0) %>% 
  filter(source_of_water_distance_m > 0)

ln_reg_prf_agri_lg <- lm(log(profit_per_acre) ~ # Test double log agriculture model
                           illness_injury +
                           vac +
                           log(source_of_water_distance_m) +
                           main_light_source_electric +
                           chicken +
                           goat +
                           cassava +
                           maize,
                         data = df_agri_sub_lg)

summary(ln_reg_prf_agri_lg)

ggplot(ln_reg_prf_agri_lg) + # check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_prf_agri_lg)), bins = 50)

ggplot(ln_reg_prf_agri_lg) + # check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_prf_agri_lg), resid(ln_reg_prf_agri_lg))) +
  xlab("Fitted") +
  ylab("Residuals") +
  geom_abline(slope = 0, color = "blue")


# Education, agriculture, household, and health data frame---- 
# In this section, all of the education, agriculture and household variables were combined 
# into single data frame to do modeling across all of the sections

agri_hh_health <- agri_hh_health %>% 
  select(-profit) # Remove duplicate variable

df_final <- education_income %>%
  left_join(agri_hh_health, by = c('hhid_clust', 'nh', 'clust')) %>%
  filter(!is.na(farm_land_size_acres)) %>%
  filter(total_farm_acres > 0)

ggplot(df_final) + # Spread of profit per acre
  geom_boxplot(aes(profit_per_acre)) +
  scale_x_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = "."))
ggplot(df_final) +
  geom_boxplot(aes(total_farm_acres)) + # spread of total farm acres
  scale_x_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = "."))

ggplot(df_final) + # Profit/acre per region
  geom_bar(aes(profit_per_acre, region, fill = loc5), stat = "Identity") +
  scale_x_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = ".")) +
  labs(fill = "Location") +
  xlab("Profit per Acre") +
  ylab("")


ggplot(df_final) + # Total acres per region and location type
  geom_bar(aes(total_farm_acres, region, fill = loc5), stat = "Identity") +
  scale_x_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = ".")) +
  labs(fill = "Location") +
  xlab("Total Farm Acres") +
  ylab("")


ggplot(df_final %>% filter(loc5 != "Accra")) + # Profit/acre per location type
  geom_bar(aes(loc5, profit_per_acre), stat = "Identity") +
  scale_y_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = ".")) +
  xlab("") +
  ylab("Profit per Acre") +
  ggtitle("Profit per Location") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(df_final %>% filter(loc5 != "Accra")) + # Total farm acres per location type
  geom_bar(aes(loc5, total_farm_acres), stat = "Identity") +
  scale_y_continuous(labels = comma_format(big.mark = ",",
                                           decimal.mark = ".")) +
  xlab("") +
  ylab("Total Farm Acres") +
  ggtitle("Farm Acreage per Location") +
  theme(plot.title = element_text(hjust = 0.5))

# Model data frame ----
# Subset of variables used in cross-section modeling

df_model <- df_final %>%
  select(profit_per_acre,
         highest_household_education,
         avg_age,
         hh_members,
         rural_coastal,
         rural_forest,
         source_of_water_distance_m,
         chicken,
         goat,
         sheep,
         cassava,
         maize,
         cocoa,
  ) %>% 
  drop_na() %>% 
  filter(profit_per_acre > 0)
nrow(df_model) # return observation count



# Final Model testing ----

# Explore effects of household variables on profitability
ln_reg_profit_model_1 <- lm(profit_per_acre ~
                              avg_age +
                              q   highest_household_education +
                              hh_members,
                            data = df_model)

summary(ln_reg_profit_model_1)

# Add in log to profit-per-acre
ln_reg_profit_model_1_2 <- lm(log(profit_per_acre) ~
                                avg_age +
                                highest_household_education +
                                hh_members,
                              data = df_model)

summary(ln_reg_profit_model_1_2)

ggplot(ln_reg_profit_model_1_2) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_profit_model_1_2)), bins = 50) +
  xlab("Standardized Residuals") 


ggplot(ln_reg_profit_model_1_2) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_profit_model_1_2), resid(ln_reg_profit_model_1_2))) +
  xlab("Fitted") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 0, color = "blue")


# Add in location variables
ln_reg_profit_model_2 <- lm(log(profit_per_acre) ~ 
                              avg_age +
                              highest_household_education +
                              rural_forest +
                              rural_coastal,
                            data = df_model)

summary(ln_reg_profit_model_2)

ggplot(ln_reg_profit_model_2) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_profit_model_2)), bins = 50) +
  xlab("Standardized Residuals") 


ggplot(ln_reg_profit_model_2) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_profit_model_2), resid(ln_reg_profit_model_2))) +
  xlab("Fitted") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 0, color = "blue")


# Add source of water, livestock, and crop metrics
ln_reg_profit_model_3 <- lm(log(profit_per_acre) ~ 
                              avg_age +
                              highest_household_education +
                              rural_forest +
                              rural_coastal +
                              source_of_water_distance_m +
                              chicken +
                              goat +
                              sheep +
                              cassava +
                              maize +
                              cocoa,
                            data = df_model)

summary(ln_reg_profit_model_3)


# Remove chickens due to low statistical significance
ln_reg_profit_model_3_1 <- lm(log(profit_per_acre) ~ 
                                avg_age +
                                highest_household_education +
                                rural_forest +
                                rural_coastal +
                                source_of_water_distance_m +
                                goat +
                                sheep +
                                cassava +
                                maize +
                                cocoa,
                              data = df_model)

summary(ln_reg_profit_model_3_1)

ggplot(ln_reg_profit_model_3_1) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_profit_model_3)), bins = 50) +
  xlab("Standardized Residuals") 


ggplot(ln_reg_profit_model_3_1) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_profit_model_3_1), resid(ln_reg_profit_model_3_1))) +
  xlab("Fitted") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 0, color = "blue")

# Test polynomials on average age and household education

ln_reg_profit_model_3_2_1 <- lm(log(profit_per_acre) ~ # Test average age squared
                                  avg_age +
                                  I(avg_age^2) +
                                  highest_household_education +
                                  rural_forest +
                                  rural_coastal +
                                  source_of_water_distance_m +
                                  goat +
                                  sheep +
                                  cassava +
                                  maize +
                                  cocoa,
                                data = df_model)

summary(ln_reg_profit_model_3_2_1)

ln_reg_profit_model_3_2_2 <- lm(log(profit_per_acre) ~ # Test average age cubed
                                  avg_age +
                                  I(avg_age^3) +
                                  highest_household_education +
                                  rural_forest +
                                  rural_coastal +
                                  source_of_water_distance_m +
                                  goat +
                                  sheep +
                                  cassava +
                                  maize +
                                  cocoa,
                                data = df_model)

summary(ln_reg_profit_model_3_2_2)

ln_reg_profit_model_3_2_3 <- lm(log(profit_per_acre) ~ # Test household education squared
                                  avg_age +
                                  highest_household_education +
                                  I(highest_household_education^2) + 
                                  rural_forest +
                                  rural_coastal +
                                  source_of_water_distance_m +
                                  goat +
                                  sheep +
                                  cassava +
                                  maize +
                                  cocoa,
                                data = df_model)

summary(ln_reg_profit_model_3_2_3)

ln_reg_profit_model_3_2_4 <- lm(log(profit_per_acre) ~ # Test household education cubed
                                  avg_age +
                                  highest_household_education +
                                  I(highest_household_education^3) +
                                  rural_forest +
                                  rural_coastal +
                                  source_of_water_distance_m +
                                  goat +
                                  sheep +
                                  cassava +
                                  maize +
                                  cocoa,
                                data = df_model)

summary(ln_reg_profit_model_3_2_4)



# Explores interactions of rural forest and rural coastal and highest household education

ln_reg_profit_model_3_4<- lm(log(profit_per_acre) ~
                               avg_age +
                               highest_household_education +
                               rural_forest +
                               highest_household_education * rural_forest +
                               rural_coastal +
                               source_of_water_distance_m +
                               goat +
                               sheep +
                               cassava +
                               maize +
                               cocoa,
                             data = df_model)

summary(ln_reg_profit_model_3_4)


ln_reg_profit_model_3_5 <- lm(log(profit_per_acre) ~ 
                                avg_age +
                                highest_household_education +
                                rural_forest +
                                rural_coastal +
                                highest_household_education * rural_coastal +
                                source_of_water_distance_m +
                                goat +
                                sheep +
                                cassava +
                                maize +
                                cocoa,
                              data = df_model)

summary(ln_reg_profit_model_3_5)

ggplot(ln_reg_profit_model_3_5) + # Check for normally distributed residuals
  geom_histogram(aes(rstandard(ln_reg_profit_model_3_5)), bins = 50) +
  xlab("Standardized Residuals") 


ggplot(ln_reg_profit_model_3_5) + # Check for heteroskedasticity
  geom_point(aes(fitted(ln_reg_profit_model_3_5), resid(ln_reg_profit_model_3_5))) +
  xlab("Fitted") +
  ylab("Residuals") +
  ggtitle("Residuals vs. Fitted") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(slope = 0, color = "blue")

# Additional Final Model Validation ----
# After determining the logarithmic model with no interactions or polynomials
# and the logarithmic model with interactions between household education and rural coastal
# performed the best in terms of statistical significance and variation explained,
# we ran k-folds cross validation to find out which model had the lowest mean squared error 
# as a final step to choosing our final model

# Create general linear regression models for k-folds cross-validation
g_ln_reg_profit_model_3_1 <- glm(log(profit_per_acre) ~ 
                                   avg_age +
                                   highest_household_education +
                                   rural_forest +
                                   rural_coastal +
                                   source_of_water_distance_m +
                                   goat +
                                   sheep +
                                   cassava +
                                   maize +
                                   cocoa,
                                 data = df_model)

g_ln_reg_profit_model_3_5 <- glm(log(profit_per_acre) ~ 
                                   avg_age +
                                   highest_household_education +
                                   rural_forest +
                                   rural_coastal +
                                   highest_household_education * rural_coastal +
                                   source_of_water_distance_m +
                                   goat +
                                   sheep +
                                   cassava +
                                   maize +
                                   cocoa,
                                 data = df_model)

models <- list(g_ln_reg_profit_model_3_1,
               g_ln_reg_profit_model_3_5)

set.seed(100)

kf_iter <- function(model_list, df, K, iter) {
  #### Function runs n iterations of K k-folds cross-validation and returns mean
  df_iter <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(df_iter) <- c("model_1", "model_2")
  n = 1
  while (n <= iter) {
    kf <- sapply(model_list, function(x) cv.glm(df, x, K = K)$delta[1])
    df_iter <- rbind(df_iter, kf)
    n = n + 1
  }
  df_iter <- df_iter %>% drop_na()
  colMeans(df_iter)
}

kf_iter(models, df_model, 10, 100) # Mean MSE scores for 100 iterations of k-folds validation with 10 folds

# Final model correlation table

final_model <- df_final %>%
  select(profit_per_acre,
         highest_household_education,
         avg_age,
         rural_coastal,
         rural_forest,
         source_of_water_distance_m,
         goat,
         sheep,
         cassava,
         maize,
         cocoa,
  ) %>% 
  drop_na() %>% 
  filter(profit_per_acre > 0)

final_model_cor <- cor(final_model) # correlation table
final_model_cor

