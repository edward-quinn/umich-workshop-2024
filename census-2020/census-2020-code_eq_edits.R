
# The workshop covers the decennial US CENSUS, which occures
# every 10 years and last occurred in 2020. "Census Day"
# is April 1. The census represents the population on that day.
# The decennial census is a complete count of data, mainly
# used to redistricting and ensuring representation.


library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)

## install.packages(c("tidycensus", "tidyverse", "mapview"))

## library(tidycensus)
## census_api_key("", overwrite = TRUE, install = TRUE)
## readRenviron("~/.Renviron")
## Sys.getenv("CENSUS_API_KEY")

# total population variable

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020
)

pop20

# differential privacy explanation. error is added
# at small geographies to preserve privacy.

table_p2 <- get_decennial(
  geography = "state", 
  table = "P2", 
  year = 2020
)

table_p2

# population for all counties in texas

tx_population <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  state = "TX",
  sumfile = "dhc",
  year = 2020
)

tx_population

# Population for specific census blocks within a specific county. 
# Must specify state because there are so many counties with the same name.

matagorda_blocks <- get_decennial(
  geography = "block",
  variables = "P1_001N",
  state = "TX",
  county = "Matagorda",
  sumfile = "dhc",
  year = 2020
)

matagorda_blocks

 vars <- load_variables(2020, "dhc")
## 
 View(vars)
## the concept column is the subject of the table that
 # the variable is located within
 
 # Understand that variables starting with H
 # are household level variables, P variables are
 # available down to the census block. PCT available at census
 # tract and larger

single_year_age <- get_decennial(
  geography = "state",
  table = "PCT12",
  year = 2020,
  sumfile = "dhc" # Note the meanings of the different types
  # of sum files: dhc = dhc data, dp = The Demographic Profile,
  # which contains pre tabulated values, cd118 or the 118th
  # congressional districts file, and the detailed
  # DHC-A or "ddhca" file.
  # DP comes from DHC, but DP gives precomputed tabulations.
)

# list summary files like this: summary_files(year = 2020)

dp_vars <- load_variables(2020, "dp")


single_year_age

# If you want wide data, just specify it in the output argument.

single_year_age_wide <- get_decennial(
  geography = "state",
  table = "PCT12",
  year = 2020,
  sumfile = "dhc",
  output = "wide" 
)

single_year_age_wide

# Use the variables argument to get nice columns immediately
# upon calling the data from the API.

ca_samesex <- get_decennial(
  geography = "county",
  state = "CA",
  variables = c(married = "DP1_0116P",
                partnered = "DP1_0118P"),
  year = 2020,
  sumfile = "dp",
  output = "wide"
)

ca_samesex

# Part 1 exercise

renter_households <- get_decennial(
  geography = "tract",
  state = "FL",
  year = 2020,
  sumfile = "dhc",
  county = "Orange",
  variables = "H4_004N"
)



# Part 2 starts here

library(tidyverse)

tidyverse_logo()

library(tidycensus)
library(tidyverse)

tx_population <- get_decennial(
  geography = "county",
  variables = "P1_001N",
  year = 2020,
  state = "TX",
  sumfile = "dhc"
)


arrange(tx_population, value)


arrange(tx_population, desc(value))

below1000 <- filter(tx_population, value < 1000)

below1000

# computing derived columns for custom tabulations that
# do not exist in the demographic profile. Do
# This using summary_var argument, which will add the denominator
# for your estimates.

# start by specifying a named vector.

race_vars <- c(
  Hispanic = "P5_010N",
  White = "P5_003N",
  Black = "P5_004N",
  Native = "P5_005N",
  Asian = "P5_006N",
  HIPI = "P5_007N"
)

cd_race <- get_decennial(
  geography = "congressional district",
  variables = race_vars,
  summary_var = "P5_001N", # this is the appropriate denominator for a table
  year = 2020,
  sumfile = "cd118" # note this is the only sumfile with congressional district
)

cd_race

# Note the "summary_value" column. 
# Now we can calculate our own race percentages for congressional districts

cd_race_percent <- cd_race %>%
  mutate(percent = 100 * (value / summary_value)) %>% 
  select(NAME, variable, percent) 

cd_race_percent |> arrange(NAME)


# get largest racial group in each district

largest_group <- cd_race_percent %>%
  group_by(NAME) %>% 
  filter(percent == max(percent)) 

# Optionally, use `.by`: 
# largest_group <- cd_race_percent %>%
#   filter(percent == max(percent), .by = NAME) 

largest_group

cd_race_percent %>%
  group_by(variable) %>% 
  summarize(median_pct = median(percent, na.rm = TRUE)) 


# Note the use of geometry = TRUE to get census tract boundaries

iowa_over_65 <- get_decennial(
  geography = "tract",
  variables = "DP1_0024P",
  state = "IA",
  geometry = TRUE,
  sumfile = "dp",
  year = 2020
)

# Note the geometry shape file data set
iowa_over_65

library(mapview)

mapview(iowa_over_65)

mapview(iowa_over_65, zcol = "value")

# adding a title to legend
mapview(iowa_over_65, zcol = "value",
        layer.name = "% age 65 and up<br>Census tracts in Iowa")


library(viridisLite)

# just adding a different color palette here
mapview(iowa_over_65, zcol = "value",
        layer.name = "% age 65 and up<br>Census tracts in Iowa",
        col.regions = inferno(100))

# How can you embed an interactive map like this in a website?


library(htmlwidgets)

m1 <- mapview(iowa_over_65, zcol = "value",
        layer.name = "% age 65 and up<br>Census tracts in Iowa",
        col.regions = inferno(100))

saveWidget(m1@map, "iowa_over_65.html") # You can plug this into an iframe
# on a website


########################################################

# Part 2 exercise

two_race <- get_decennial(
  geography = "county",
  variable = "P10_009N",
  state = "FL",
  geometry = TRUE,
  sumfile = "dhc",
  year = 2020
)

# Note the geometry shape file data set
two_race

library(mapview)

mapview(two_race)

mapview(two_race, zcol = "value")




###################################################


# This is the start of Part 3 - detailed DHC-A and
# time series analysis

# tabulation of 2020 decennial census results
# for population by sex and age
# Key feature: break-outs for thousands of racial and ethnic groups


ddhca_vars <- load_variables(2020,"ddhca")

# only 82 variables, always broken out by population group.
# there is true total column because of this.

# some of these categories are within the total column, but
# the hierarchy is unclear. so there sort of is a total column?

mn_population_groups <- get_decennial(
  geography = "state",
  variables = "T01001_001N",
  state = "MN",
  year = 2020,
  sumfile = "ddhca",
  pop_group = "all", # this argument is required
  pop_group_label = TRUE # the label for that population group
)


mn_population_groups

# get_pop_groups to answer how many groups

available_groups <- get_pop_groups(2020, "ddhca")

# This code does not work on purpose. Meant
# to show the adaptive design error message.

get_decennial(
  geography = "county",
  variables = "T02001_001N",
  state = "MN",
  county = "Hennepin",
  pop_group = "1325", # somali in any combination
  year = 2020,
  sumfile = "ddhca"
)

# How to get the tables that are available for 
# a given group for a given geography with the
# check_ddhca_groups function

check_ddhca_groups(
  geography = "county", 
  pop_group = "1325", 
  state = "MN", 
  county = "Hennepin"
)

# seen above, sometimes we can get a more
# detailed breakdown of a racial group by age
# in 23 categories for example, but not in less
# detailed categories, like groups of 4 or 9.

library(tidycensus)

# mapping sparse data is always risky.

hennepin_somali <- get_decennial(
  geography = "tract",
  variables = "T01001_001N",
  state = "MN",
  county = "Hennepin",
  year = 2020,
  sumfile = "ddhca",
  pop_group = "1325",
  pop_group_label = TRUE,
  geometry = TRUE
)


mapview(hennepin_somali, zcol = "value")


# a dot density map - specifying how many
# people a single dot represents
somali_dots <- as_dot_density(
  hennepin_somali,
  value = "value",
  values_per_dot = 25
)

mapview(somali_dots, cex = 0.01, layer.name = "Somali population<br>1 dot = 25 people",
        col.regions = "navy", color = "navy")



##############
# Time Series
##############

# Looking at how populations have changed over time
# in different areas of the US. You can compare
# 2010 to 2020 census data for example, but you
# need to be careful since some variable codes have
# changed over time.


county_pop_10 <- get_decennial(
  geography = "county",
  variables = "P001001", # Total Population
  year = 2010,
  sumfile = "sf1" # summary file 1 from the 2010 census
)


county_pop_10

# Now we need to preprocess this data to make a comparison to 
# 2020.

county_pop_10_clean <- county_pop_10 %>%
  select(GEOID, value10 = value) # just renaming the value column

county_pop_10_clean

county_pop_20 <- get_decennial(
  geography = "county",
  variables = "P1_001N", # Note the change in variable code from 2010
  year = 2020,
  sumfile = "dhc" # Note the change in the summary file since 2010
) %>%
  select(GEOID, NAME, value20 = value)

county_joined <- county_pop_20 %>%
  left_join(county_pop_10_clean, by = "GEOID") # GEOID is the consistent,
# unique identifier

county_joined

county_change <- county_joined %>%
  mutate( 
    total_change = value20 - value10, 
    percent_change = 100 * (total_change / value10) 
  ) 


county_change

filter(county_change, is.na(value10))
