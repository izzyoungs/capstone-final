library(tidyverse)
library(naniar)
library(tidycensus)
library(tigris)
library(ggthemes)
library(janitor)
options(scipen=999)
options(tigris_use_cache = TRUE)

# Get the variables used from ACS
variable_names <- read_csv("Data/Raw/vars_needed.csv")
variables <- variable_names[2] %>% pull()
colnames <- variable_names[5] %>% pull()

# Get ACS data
acs <- get_acs(geography = "county", 
               variables = variables, 
               year = 2016,
               output = "wide",
               geometry = FALSE)

# Clean ACS data
acs2 <- acs %>%
  rename_at(vars(variables), ~ colnames) %>%
  select(fips = GEOID, county = NAME,  all_of(colnames))

# Write ACS data to file
write_csv(acs2, "Data/Raw/ACS_2016_raw.csv")
