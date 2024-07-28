options(tigris_use_cache = TRUE)

## install.packages(c("tidycensus", "tidyverse"))

## install.packages(c("mapview", "survey", "srvyr"))

library(tidyverse)
library(mapview)
library(survey)
library(srvyr)

## library(tidycensus)
## 
## census_api_key("YOUR KEY GOES HERE", install = TRUE)

library(tidycensus)

# This defaults to five year acs estimates if you put the end year estimate
# test note
median_value <- get_acs(
  geography = "county",
  variables = "B25077_001",
  year = 2022
)
# 
# median_value |> 
#   arrange(desc(estimate)) |> 
#   View()




median_value_1yr <- get_acs(
  geography = "place",
  variables = "B25077_001",
  year = 2022,
  survey = "acs1"
)

median_value_1yr

income_table <- get_acs(
  geography = "county", 
  table = "B19001", 
  year = 2022
)

income_table

sd_value <- get_acs(
  geography = "tract", 
  variables = "B25077_001", 
  state = "CA", 
  county = "San Diego",
  year = 2022
)

sd_value

vars <- load_variables(2022, "acs5")

#View(vars)

vars_profile <- load_variables(2022, "acs5/profile")

 

age_sex_table <- get_acs(
  geography = "state", 
  table = "B01001", 
  year = 2022,
  survey = "acs1",
)


age_sex_table

# You can also get wide data using the output argument.

age_sex_table_wide <- get_acs(
  geography = "state", 
  table = "B01001", 
  year = 2022,
  survey = "acs1",
  output = "wide" 
)

age_sex_table_wide

# You can replace the identifiers for variable names as you call the api.
# Just make sure you know what you're pulling before you label it.

ca_education <- get_acs(
  geography = "county",
  state = "CA",
  variables = c(percent_high_school = "DP02_0062P", 
                percent_bachelors = "DP02_0065P",
                percent_graduate = "DP02_0066P"), 
  year = 2021
)

ca_education

get_acs(
  geography = "state",
  variables = "B16001_054",
  year = 2022,
  survey = "acs1"
)

# Let's search for median household income for census blocks
# in Orlando


orange_value <- get_acs(
  geography = "tract", 
  variables = "B19049_001", 
  state = "FL", 
  county = "Orange",
  year = 2022
)

# this is identical to orange_value.
# the same information may appear in multiple tables in acs data.
orange_value2 <- get_acs(
  geography = "tract", 
  variables = "B19013_001", 
  state = "FL", 
  county = "Orange",
  year = 2022
)



get_acs(
  geography = "state",
  variables = "B16001_054",
  year = 2022,
  survey = "acs5"
)

utah_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "UT",
  year = 2022
) 

library(ggplot2)

utah_plot <- ggplot(utah_income, aes(x = estimate, y = NAME)) + 
  geom_point()

utah_plot

utah_plot <- ggplot(utah_income, aes(x = estimate, 
                                y = reorder(NAME, estimate))) + 
  geom_point(color = "darkblue", size = 2)

utah_plot

library(scales)
library(stringr)

utah_plot <- utah_plot + 
  scale_x_continuous(labels = label_dollar()) + 
  scale_y_discrete(labels = function(x) str_remove(x, " County, Utah")) 

utah_plot

utah_plot <- utah_plot + 
  labs(title = "Median household income, 2018-2022 ACS",
       subtitle = "Counties in Utah",
       caption = "Data acquired with R and tidycensus",
       x = "ACS estimate",
       y = "") + 
  theme_minimal(base_size = 12)

utah_plot

## View(utah_income)

utah_plot_errorbar <- ggplot(utah_income, aes(x = estimate, 
                                        y = reorder(NAME, estimate))) + 
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), #<<
                width = 0.5, linewidth = 0.5) + #<<
  geom_point(color = "darkblue", size = 2) + 
  scale_x_continuous(labels = label_dollar()) + 
  scale_y_discrete(labels = function(x) str_remove(x, " County, Utah")) + 
  labs(title = "Median household income, 2018-2022 ACS",
       subtitle = "Counties in Utah",
       caption = "Data acquired with R and tidycensus. Error bars represent margin of error around estimates.",
       x = "ACS estimate",
       y = "") + 
  theme_minimal(base_size = 12)

utah_plot_errorbar

# The geometry = TRUE is what gives you information for mapping.

cook_education <- get_acs(
  geography = "tract",
  variables = "DP02_0068P",
  state = "IL",
  county = "Cook",
  year = 2022,
  geometry = TRUE
)

cook_education

library(mapview)

mapview(cook_education)

mapview(cook_education, zcol = "estimate")

# Mapping 1 year ACS data is dangerous because of 
# data sparsity, and may imply patterns that only
# exist because of the pattern of missing data,
# not a pattern in the data.

tx_education <- get_acs(
  geography = "county",
  variables = "DP02_0068P",
  state = "TX",
  year = 2022,
  survey = "acs1",
  geometry = TRUE
)

mapview(tx_education, zcol = "estimate")

# Using public use microdata to get maps of variables
# that may be sparse in the 1 year acs.

wa_wfh <- get_acs(
  geography = "puma",
  variables = "DP03_0024P",
  state = "WA",
  survey = "acs1",
  year = 2022,
  geometry = TRUE
)

library(mapview)

mapview(wa_wfh, zcol = "estimate")

ct_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "CT",
  year = 2022,
  survey = "acs1",
  geometry = TRUE
)

mapview(ct_income, zcol = "estimate")


# Part 2 exercises

# Pay attention to differences in margins of error
# of median home values in the 5 year rolling average
# versus the one year estimates, and the actual estimates
# of home values. More recent estimates are much higher
# reflecting post covid rises in prices.

florida_median_value <- get_acs(
  geography = "county",
  variables = "B25077_001",
  state = "FL",
  year = 2022,
  geometry = TRUE
)

florida_median_value_one_year <- get_acs(
  geography = "county",
  variables = "B25077_001",
  state = "FL",
  year = 2022,
  survey = "acs1",
  geometry = TRUE
)

oc_median_value <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  county = c("Orange","Seminole"),
  state = "FL",
  year = 2022,
  geometry = TRUE
)

mapview(oc_median_value, zcol = "estimate")


bc_median_value <- get_acs(
  geography = "tract",
  variables = "B25077_001",
  county = c("Baltimore City","Baltimore County"),
  state = "MD",
  year = 2022,
  geometry = TRUE
)

mapview(bc_median_value, zcol = "estimate")


# Part 3 Working with ACS Microdata

# Here we are showing how census tract designations, but not boundaries,
# have changed between 2021 and 2022.

library(tigris)
library(mapview)
ct_tract_21 <- tracts("CT", cb = TRUE, year = 2021)
ct_tract_22 <- tracts("CT", cb = TRUE, year = 2022)

# This vertical | character is how you make a swipe map.

mapview(ct_tract_21) | mapview(ct_tract_22)

library(tidycensus)

# Be careful with get pums, because it defaults to
# 5 year acs data, which could be 16 million rows

# PUMS = PUBLIC USE MICRODATA SERIES


or_pums <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "OR",
  survey = "acs1",
  year = 2022
)

or_pums

library(tidyverse)

# How can we figure out how many 40 year olds there are in Oregon?
# Essentially summing over the person weights for all people and
# then only for 40 year olds and getting that percentage.

or_age_40 <- filter(or_pums, AGEP == 40)

#PWGTP is person weights

print(sum(or_pums$PWGTP))
print(sum(or_age_40$PWGTP))

# Let's use acs data 

get_acs("state", "B01003_001", state = "OR", survey = "acs1", year = 2022)

#View(pums_variables)

# recode = TRUE is very useful, no need to waste time recoding
# household type or sex or anything else, it's automated.

or_pums_recoded <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "OR",
  survey = "acs1",
  year = 2022,
  recode = TRUE
)

or_pums_recoded

# Using the variables argument with the variables_filter
# argument allows you to pull down only the data you really
# need, reducing download times. This example code is 
# pulling women in oregon between ages 30 and 49 only.

or_pums_filtered <- get_pums(
  variables = c("SEX", "AGEP", "HHT"),
  state = "OR",
  survey = "acs5",
  variables_filter = list(
    SEX = 2,
    AGEP = 30:49
  ),
  year = 2022
)

or_pums_filtered

or_age_by_puma <- get_pums(
  variables = c("PUMA", "AGEP"),
  state = "OR",
  survey = "acs1",
  year = 2022
)


# Note that PUMAS change every 10 years. The first
# 1 year ACS to use 2020 PUMAS is the 2022 ACS dataset.

or_age_by_puma

# How do we handle uncertainty in PUMS data?. The Census Bureau
# recommends using successive difference replication to calculate
# standard errors, and provides replicate wieghts to do this.

# tidycensus includes tools to help you get replicate wieghts and
# format your data for appropriate survey-weighted analysis

or_pums_replicate <- get_pums(
  variables = c("AGEP", "PUMA"),
  state = "OR",
  survey = "acs1",
  year = 2022,
  rep_weights = "person" 
)


or_pums_replicate

# Note that we know have many columns corresponding to a 
# variety of different person weights. We use all of these
# to generate a distribution of possibilities.

# The "to_survey" function takes this dataframe and converts
# to an object that is ready for use with R packages built
# for analyzing survey data.

or_survey <- to_survey(
  or_pums_replicate,
  type = "person"
)

class(or_survey)

library(srvyr)

# Now we can generate margins of error for PUMS data.

or_survey %>%
  filter(AGEP == 40) %>%
  survey_count() %>%
  mutate(n_moe = n_se * 1.645)

# We can also do something similar grouping by PUMA

or_survey %>%
  group_by(PUMA) %>%
  summarize(median_age = survey_median(AGEP)) %>%
  mutate(median_age_moe = median_age_se * 1.645)

or_age_puma <- get_acs(
  geography = "puma",
  variables = "B01002_001",
  state = "OR",
  year = 2022,
  survey = "acs1"
)
