library(tidyverse)
library(naniar)
library(janitor)
library(tidycensus)
library(zoo)
library(sf)
library(tidycensus)
library(albersusa)
library(units)
library(here)
library(psych)

# Read in ACS data from 2012-2016 and 2007-2011
acs16 <- read_csv("Data/Raw/ACS_2016.csv") %>%
  mutate(geoid = str_pad(fips, 5, pad = "0"), .keep = "unused") %>%
  select(geoid, everything())

acs11 <- read_csv("Data/Raw/ACS_2011.csv") %>%
  mutate(geoid = str_pad(fips, 5, pad = "0"), .keep = "unused") %>%
  select(geoid, everything())

# Ensure the data is copied for all years of the ACS 5 year range
acs11years <- 2007:2011%>% 
  as_tibble %>%
  transmute(year = as.character(value))

acs16years <-2012:2016 %>% 
  as_tibble %>%
  transmute(year = as.character(value))

acs16_v2 <- full_join(acs16years, acs16, by = character())
acs11_v2 <- full_join(acs11years, acs11, by = character())

years <- 2007:2016 %>% 
  as_tibble %>%
  transmute(year = as.character(value))

# Rowbind all years together
acs <- rbind(acs11_v2, acs16_v2)

geo <- acs16 %>% select(geoid, county)

geo_years <- full_join(years, geo, by = character())

acs2 <- left_join(geo_years, acs, by = c("geoid", "county", "year")) %>%
  mutate(state_fips = substr(geoid, start = 1, stop = 2),
         county_fips = substr(geoid, start = 3, stop = 5),
         median_year_structure_built_bin = case_when(median_year_structure_built < 1950 ~ "Before 1950",
                                                     median_year_structure_built >= 1950 & median_year_structure_built < 1970 ~ "1950 - 1969",
                                                     median_year_structure_built >= 1970 & median_year_structure_built < 1990 ~ "1970 - 1989",
                                                     median_year_structure_built >= 1990 & median_year_structure_built < 2010 ~ "1990 - 2010",
                                                     median_year_structure_built > 2010 ~ "After 2010")) %>%
  rename(full_name = county) %>%
  select(-median_year_structure_built)

# Read in and join population data from ACS API
years_pop <- 2009:2016
population <- c()
for(i in seq_along(years_pop)){
acs <- get_acs(geography = "county", 
               variables = "B00001_001E", 
               year = years_pop[i],
               output = "wide",
               geometry = FALSE) %>%
  select(geoid = GEOID, population = B00001_001E) %>%
  mutate(year = as.character(years_pop[i]))

population <- rbind(population, acs)
}

# Join population and ACS data and create a column that calculates if the county experienced a pop loss between years
full2 <- left_join(acs2, population, by = c("geoid", "year")) %>%
  group_by(geoid) %>%
  arrange(geoid) %>%
  mutate(prevyrpop = lag(population, n = 1, order_by = year),
         d_pop_loss = ifelse(prevyrpop > population, 1, 0)) %>%
  ungroup()

# Read in and join Build Permit Survey data from HUD
bps <- read_csv("Data/Raw/bps.csv") %>%
  clean_names() %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"),
         county_fips = str_pad(county_fips, 3, pad = "0"),
         geoid = paste0(state_fips, county_fips),
         year = as.character(year)) %>% 
  select(geoid, everything(), -state_fips, -county_fips, -region, -division, -county_name) %>%
  distinct()

regions <- read_csv("Data/Raw/regions.csv") %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0")) %>%
  select(-region)

full3 <- left_join(full2, regions, by = "state_fips")
  
full4 <- left_join(full3, bps, by = c("geoid", "year")) %>%
  mutate(share_total_units_new_units = total_new_units/total_housing_units) %>%
  select(-total_new_units, -new_1_units, -new_2_units, -new_3to4_units, -new_5_units)

# Join dissimilarity index data from FRED
disindex <- read_csv("Data/Raw/dissimilarity_index.csv", col_types = "cccdddddddd") %>%
  select(-`Series ID`, -`Region Name`) %>%
  mutate(geoid = str_pad(county_fips, 5, pad = "0"), .keep = "unused") %>%
  pivot_longer(!geoid, names_to = "year", values_to = "dissimilarity_index")

full5 <- left_join(full4, disindex, by = c("geoid", "year")) %>% 
  mutate(segregation = case_when(dissimilarity_index < 31 ~ "Low",
                                 dissimilarity_index > 30 & dissimilarity_index < 61 ~ "Moderate",
                                 dissimilarity_index > 60 ~ "High"))

# Read in GDP data from BLS
gdp <- read_csv("Data/Raw/gdp_counties.csv") %>%
  select(-GeoName, -Region, geoid = GeoFIPS) %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0"), .keep = "unused") %>%
  pivot_longer(!geoid, names_to = "year", values_to = "gdp")

full6 <- left_join(full5, gdp, by = c("geoid", "year"))

# Read in which counties have housing trust funds
funds <- read_csv("Data/Raw/housing_funds_counties.csv") %>%
  clean_names %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"),
         county_fips = str_pad(county_fips, 3, pad = "0"),
         geoid = paste0(state_fips, county_fips)) %>%
  select(geoid, year_enacted)

funds_years <- full_join(years, funds, by = character())

# If the housing trust fund was created by the row year, set to 1
funds2 <- funds_years %>% 
  mutate(d_housing_trust_fund = case_when(year_enacted <= year ~ 1,
                                        year_enacted > year ~ 0)) %>%
  select(year, geoid, d_housing_trust_fund)

full7 <- left_join(full6, funds2, by = c("geoid", "year"))

full8 <- full7 %>% mutate(d_housing_trust_fund = ifelse(is.na(d_housing_trust_fund), 0, d_housing_trust_fund))

# Read in HUD data on subsidized housing programs
hud <- read_csv("Data/Raw/hud_programs.csv") %>%
  filter(program == "All HUD Programs") %>%
  mutate(geoid = str_pad(fips, 5, pad = "0"),
         year = as.character(Quarter)) %>%
  select(geoid, year, everything(), -program, -fips, -Quarter)

full9 <- left_join(full8, hud, by = c("geoid", "year")) %>%
  mutate(share_total_housing_units_subsidized = total_subsidized_units/total_housing_units,
         share_pop_subsidized = people_total_subsidized/population) %>%
  select(-people_total_subsidized, -total_subsidized_units, -total_housing_units, -people_per_unit_subsidized, -rent_per_month_subsidized)

# Read in urban influence codes from USDA
uicode <- read_csv("Data/Raw/urban_influence_codes.csv") %>%
  filter(Year == 2013) %>%
  mutate(urban_type = case_when(Description == "Large-in a metro area with at least 1 million residents or more" ~ "Metro",
                                Description == "Small-in a metro area with fewer than 1 million residents" ~ "Metro",
                                Description == "Micropolitan adjacent to a large metro area" ~ "Mid-sized",
                                Description == "Micropolitan adjacent to a small metro area" ~ "Mid-sized",
                                Description == "Micropolitan not adjacent to a metro area" ~ "Mid-sized",
                                Description == "Noncore adjacent to a large metro area" ~ "Exurban",
                                Description == "Noncore adjacent to a small metro with town of at least 2,500 residents" ~ "Exurban",
                                Description == "Noncore adjacent to a small metro and does not contain a town of at least 2,500 residents" ~ "Exurban",
                                Description == "Noncore adjacent to micro area and contains a town of 2,500-19,999 residents" ~ "Exurban",
                                Description == "Noncore adjacent to micro area and does not contain a town of at least 2,500 residents" ~ "Exurban",
                                Description == "Noncore not adjacent to a metro/micro area and does not contain a town of at least 2,500 residents" ~ "Rural",
                                Description == "Noncore not adjacent to a metro/micro area and contains a town of 2,500  or more residents" ~ "Rural")) %>%
  select(geoid = FIPS, urban_type)

full10 <- left_join(full9, uicode, by = "geoid")

# Read in creative class data from USDA
cc_share <- read_csv("Data/Raw/share_cc.csv") %>%
  select(geoid, share_creative_class) %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0"))

full11 <- left_join(full10, cc_share, by = c("geoid"))

# Read in elections data from MIT
dem_sway <- read_csv("Data/Raw/MIT_elections.csv") %>%
  filter(party == "democrat") %>%
  select(year, geoid = FIPS, dem_sway = pol_sway) %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0"),
         year = as.character(year))

full12 <- left_join(full11, dem_sway, by = c("geoid", "year"))

# Interpolate democratic sway
full12$dem_sway <- na.approx(full12$dem_sway, rule = 2, method = "constant")

full12 <- full12 %>% 
  mutate(d_democratic = ifelse(dem_sway > .5, 1, 0))

# Read in county typology data from the USDA
types <- read_csv("Data/Raw/county_typologies.csv") %>%
  clean_names() %>%
  filter(year == 2015) %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0")) %>%
  select(-state, -county_name, -year, -d_metro)

full13 <- left_join(full12, types, by = "geoid")

# Read in homerule data
homerule <- read_csv("Data/Raw/county_homerule.csv") %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0")) %>%
  select(geoid, d_homerule)

full14 <- left_join(full13, homerule, by = "geoid")

# Read in minimum wage data by county
min_wage_c <- read_csv("Data/Raw/minimum_wage_counties.csv") %>%
  clean_names %>%
  mutate(state_fips = str_pad(state_fips_code, 2, pad = "0"),
         county_fips = str_pad(fips, 3, pad = "0"),
         geoid = paste0(state_fips, county_fips),
         year = as.character(year)) %>%
  select(year, geoid, min_wage)

full15 <- left_join(full14, min_wage_c, by = c("geoid", "year"))

# Read in minimum wage data by state
min_wage_state <- read_csv("Data/Raw/minimum_wage_state_annual.csv") %>%
  clean_names %>%
  filter(year > 1999) %>%
  mutate(year = as.character(year)) %>%
  select(year, state_name, min_wage)

full16 <- left_join(full15, min_wage_state, by = c("state_name", "year"))

full17 <- full16 %>% mutate(min_wage.x = ifelse(is.na(min_wage.x), min_wage.y, min_wage.x))

# Read data on state discrimination laws from State Policy Database
disc <- read_csv("Data/Raw/discrimination_state.csv") %>%
  clean_names %>%
  mutate(year = as.character(year))

full18 <- left_join(full17, disc, by = c("state_name", "year"))

# Read data on state labor laws from State Policy Database
labor <- read_csv("Data/Raw/state_labor.csv") %>%
  clean_names %>%
  mutate(year = as.character(year))

full19 <- left_join(full18, labor, by = c("state_name", "year"))

# Read in tenant policies and laws from Apartments and Nolo
tenants <- read_csv("Data/Raw/state_tenant_policies.csv") %>%
  clean_names() %>%
  select(-d_statewide_rent_control)

full20 <- left_join(full19, tenants, by = "state_name") %>%
  mutate(d_coastal_state = ifelse(state_fips == 11, 0, d_coastal_state),
         d_state_prohibits_government_sex_discrimination = ifelse(state_fips == 11, 1, d_state_prohibits_government_sex_discrimination),
         d_right_to_work_law = ifelse(state_fips == 11, 0, d_right_to_work_law),
         d_prevailing_wage = ifelse(state_fips == 11, 1, d_prevailing_wage),
         d_affirmative_action_ban = ifelse(state_fips == 11, 0, d_affirmative_action_ban))

# Read in rent control data
rentcontrol <- read_csv("Data/Raw/rent_control.csv") %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"),
         county_fips = str_pad(county_fips, 3, pad = "0"),
         geoid = paste0(state_fips, county_fips)) %>%
  select(geoid, d_specific_rent_control)

full21 <- left_join(full20, rentcontrol, by = "geoid")

# Read in medicaid expansion data
medicaid <- read_csv("Data/Raw/medicaid_expansion.csv") %>%
  clean_names %>%
  mutate(year = as.character(year))

full22 <- left_join(full21, medicaid, by = c("state_name", "year")) %>%
  arrange(geoid)

# Interpolate paid family leave program with constant method
full22$d_paid_family_leave_program <- na.approx(full22$d_paid_family_leave_program, rule = 2, method = "constant")

# Interpolate paid family leave program with constant method
full22$d_employment_anti_discrimination <- na.approx(full22$d_employment_anti_discrimination, rule = 2, method = "constant")

# Read data on evictions from the Eviction Lab (Vintage March 2021)
evictions <- read_csv("Data/Raw/eviction_data_counties.csv") %>%
  clean_names %>%
  mutate(geoid = str_pad(geoid, 5, pad = "0"),
         year = as.character(year)) %>%
  select(-name, -parent_location, -imputed, -subbed, -renter_occupied_households, -rent_burden, -median_property_value, -population, -pct_renter_occupied)

full23 <- left_join(full22, evictions, by = c("geoid", "year"))

# Create a column for the delta between filing rates and judgements
full24 <- full23 %>%
  mutate(delta_eviction_rates = eviction_filing_rate - eviction_rate)

# Create means for the eviction rates
avg_rate <- mean(full24$eviction_rate, na.rm = TRUE)
avg_filing_rate <- mean(full24$eviction_filing_rate, na.rm = TRUE)
avg_delta <- mean(full24$delta_eviction_rates, na.rm = TRUE)

# Create categorical eviction rate and eviction filing rate bins
full25 <- full24 %>% 
  mutate(d_above_avg_eviction_rate = case_when(eviction_rate > avg_rate ~ 1,
                                               eviction_rate < avg_rate ~ 0),
         d_above_avg_eviction_filing_rate = case_when(eviction_filing_rate > avg_filing_rate ~ 1,
                                                      eviction_filing_rate < avg_filing_rate ~ 0),
         d_above_avg_delta_rate = case_when(delta_eviction_rates > avg_delta ~ 1,
                                            delta_eviction_rates < avg_delta ~ 0)) %>%
  select(-eviction_filings, -evictions)

county_names <- read_csv("Data/Raw/county_names.csv") %>%
  mutate(state_fips = str_pad(state_fips, 2, pad = "0"),
         county_fips = str_pad(county_fips, 3, pad = "0"),
         geoid = paste0(state_fips, county_fips)) %>%
  select(geoid, county)

full26 <- left_join(full25, county_names, by = "geoid")

cty_sf <- counties_sf("aeqd") %>%
  mutate(geoid = as.character(fips), .keep = "unused") %>%
  select(state_abb = iso_3166_2, land_area = census_area, geoid)

full27 <- left_join(cty_sf, full26, by= "geoid")

full28 <- full27 %>%
  mutate(density = population / land_area) %>%
  st_drop_geometry()

# Clean up data names and set schema structure
final_frame <- full28 %>%
  select(year, geoid, state_fips, state = state_name, state_abb, county_fips, county, full_name, division, d_coastal_state, population, d_pop_loss, ends_with(".y"), everything(), -prevyrpop, -population, -median_contract_rent, -median_monthly_housing_costs, -months_from_movein_subsidized, -eviction_rate, -eviction_filing_rate, -d_above_avg_eviction_filing_rate, -d_democratic, -pct_movein_subsidized, -segregation, -d_above_avg_delta_rate, -ends_with(".x")) %>%
  filter(year > 2009)

colnames(final_frame) <- gsub(".y","",colnames(final_frame),  fixed = TRUE)

final_frame

# Save final data frame
write_csv(final_frame, "Data/Output/final_frame.csv")

# Create a summary stats csv
var_summary <- describe(final_frame)

var <- final_frame %>% 
  names

var_summary2 <- cbind(var, var_summary)

var_summary3 <- var_summary2 %>% mutate(missingness = (1 - (n/nrow(final_frame)))* 100) %>%
  select(var, n, missingness, everything(), -trimmed, -mad, -vars)

write_csv(var_summary3, "Data/Output/var_summary.csv")