library(here)
library(tidyverse)
library(vip)
library(gridExtra)
library(pdp)
library(parallel)
library(doParallel)
library(rsample)
library(yardstick)
library(recipes)
library(beepr)
library(stringr)

set.seed(1992)

# First RF model
mod_rf <- read_rds("Data/Output/RDS_data/rf.rds")

# Training data set 1
train_data_processed <- read_csv("Data/Output/CSV_data/train_data_processed.csv")

aspects <- read_csv("Data/Output/var_aspects.csv")

# Variable importance assessment ------------------------------------------

possible_profiles <- # generate a tibble of the different typology codes
  tibble(tibble(urban_type_Metro = c(1,0,0,0)),
         tibble(urban_type_Mid.sized = c(0,1,0,0)),
         tibble(urban_type_Rural = c(0,0,1,0)))

for (r in 1:nrow(possible_profiles)){
  data <- train_data_processed %>%
    inner_join(possible_profiles[r,]) # filters the data by the possible profiles 
  
  type <- case_when(r == 1 ~ "Metro",
                    r == 2 ~ "Midsized",
                    r == 3 ~ "Rural",
                    TRUE ~ "Exurban")
  
  obj <- vi_permute(mod_rf, # Machine learning model
                    train = data,
                    nsim = 10, # Number of times to permute each variable
                    target = "evictions", # outcome
                    reference_class = "Above_Average", # what class are you predicting
                    metric = "accuracy", # metric 
                    pred_wrapper = predict) # prediction function
  
  
  write_rds(obj,  paste0("Data/Output/RDS_data/", paste0("vip_", type), ".rds"))
}

beep()

# Read in vip data for every typology
vip_metro <- read_rds("Data/Output/RDS_data/vip_Metro.rds")
vip_exurban <- read_rds("Data/Output/RDS_data/vip_Exurban.rds")
vip_midsized <- read_rds("Data/Output/RDS_data/vip_Midsized.rds")
vip_rural <-  read_rds("Data/Output/RDS_data/vip_Rural.rds")



# Join all frames together and process - Step 1
vip_frame <- bind_rows(
  vip_rural %>% mutate(type="Rural"),
  vip_metro %>% mutate(type = "Metro"),
  vip_exurban %>% mutate(type = "Exurban"),
  vip_midsized %>% mutate(type = "Midsized")) %>% 
  left_join(aspects, by = "Variable") %>%
  group_by(Variable) %>%
  summarize(Importance, var_mean = mean(Importance), StDev, type, Aspect) %>%
  ungroup() %>%
  filter(Importance >= 0.002) %>% 
  mutate(Variable = fct_reorder(Variable, var_mean, .fun='median'))


top_vi <- vip_frame %>%
  select(Variable) %>%
  distinct(Variable) %>%
  pull() %>%
  as.character()


# Plot by typology
vip_frame %>%
  mutate(Variable = gsub("^d_|_X1", "", Variable),
         Variable = gsub("_|\\.", " ", Variable),
         Variable = gsub("avg", "average", Variable),
         Variable = gsub("pct", "percent", Variable),
         Variable = gsub("min", "minimum", Variable),
         Variable = gsub("af am", "african american", Variable),
         Variable = str_to_title(Variable),
         Variable = gsub("Gdp", "GDP", Variable),
         type = factor(type, levels = c("Metro", "Midsized", "Exurban", "Rural"))) %>%
  arrange(Importance) %>%
  mutate(Variable = fct_reorder(Variable, var_mean, .fun='median')) %>%
  ggplot(aes(y=Variable,x=Importance,color=type)) +
  geom_errorbarh(aes(xmin=Importance-(StDev*2),
                     xmax=Importance+(StDev*2)),
                 height =.05,size=1,
                 position =  position_dodge(width = 0.2)) +
  geom_point(size=2,position =  position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "RdYlBu") + 
  theme_minimal() +
  labs(title = "Variable Importance", 
       color="Urban Typology") +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 20, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16))

# Save the top variables for removal in the final random forest model
write_rds(top_vi, "Data/Output/RDS_data/vip_top_vars.rds")



# Top variable importance - Step 2

aspects <- read_csv("Data/Output/var_aspects.csv")

# Finalized RF model (top vars only)
mod_rf_final <- read_rds("Data/Output/RDS_data/rf_final.rds")

# Finalized training data
train_data_processed2 <- read_csv("Data/Output/CSV_data/train_data_processed2.csv") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year))


possible_profiles <- # generate a tibble of the different typology codes
  tibble(tibble(urban_type_Metro = c(1,0,0,0)),
         tibble(urban_type_Mid.sized = c(0,1,0,0)),
         tibble(urban_type_Rural = c(0,0,1,0)))

for (r in 1:nrow(possible_profiles)){
  data <- train_data_processed2 %>%
    inner_join(possible_profiles[r,]) # filters the data by the possible profiles 
  
  type <- case_when(r == 1 ~ "Metro",
                    r == 2 ~ "Midsized",
                    r == 3 ~ "Rural",
                    TRUE ~ "Exurban")
  
  obj <- vi_permute(mod_rf_final, # Machine learning model
                    train = data,
                    nsim = 10, # Number of times to permute each variable
                    target = "evictions", # outcome
                    reference_class = "Above_Average", # what class are you predicting
                    metric = "accuracy", # metric 
                    pred_wrapper = predict) # prediction function
  
  
  write_rds(obj,  paste0("Data/Output/RDS_data/", paste0("vip_", type), "2.rds"))
}

beep()

# Read in vip data for every typology
vip_metro2 <- read_rds("Data/Output/RDS_data/vip_Metro2.rds")
vip_exurban2 <- read_rds("Data/Output/RDS_data/vip_Exurban2.rds")
vip_midsized2 <- read_rds("Data/Output/RDS_data/vip_Midsized2.rds")
vip_rural2 <-  read_rds("Data/Output/RDS_data/vip_Rural2.rds")



vip_frame2 <- bind_rows(
  vip_rural2 %>% mutate(type="Rural"),
  vip_metro2 %>% mutate(type = "Metro"),
  vip_exurban2 %>% mutate(type = "Exurban"),
  vip_midsized2 %>% mutate(type = "Midsized")) %>%
  filter(Importance > 0) %>%
  left_join(aspects, by = "Variable") %>%
  group_by(Variable) %>%
  summarize(Importance, var_mean = mean(Importance), StDev, type, Aspect) %>%
  ungroup() %>%
  mutate(Variable = fct_reorder(Variable, var_mean, .fun='median'))


# Plot by typology - Final model
vip_frame2 %>%
  mutate(Variable = gsub("^d_|_X1", "", Variable),
         Variable = gsub("_|\\.", " ", Variable),
         Variable = gsub("avg", "average", Variable),
         Variable = gsub("pct", "percent", Variable),
         Variable = gsub("min", "minimum", Variable),
         Variable = gsub("af am", "african american", Variable),
         Variable = str_to_title(Variable),
         Variable = gsub("Gdp", "GDP", Variable),
         type = factor(type, levels = c("Metro", "Midsized", "Exurban", "Rural"))) %>%
  group_by(Aspect, type) %>%
  summarize(Variable, StDev, var_mean, std_mean = mean(StDev), Importance, aspect_mean = mean(Importance)) %>%
  ungroup() %>%
  arrange(Importance) %>%
  mutate(Variable = fct_reorder(Variable, var_mean, .fun='median')) %>%
  ggplot(aes(y=Variable,x=Importance,color=type)) +
  geom_errorbarh(aes(xmin=Importance-(StDev*2),
                     xmax=Importance+(StDev*2)),
                 height =.05,size=1,
                 position =  position_dodge(width = 0.2)) +
  geom_point(size=2,position =  position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "RdYlBu") + 
  theme_minimal() +
  labs(title = "Variable Importance", 
       color="Urban Typology") +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 20, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16))

ggsave("Plots/vip_imp.png", height = 8, width = 13)




# Plot by aspect

vip_frame2 %>%
  filter(Aspect != "Quality control") %>%
  group_by(Aspect, type) %>%
  summarize(Variable, StDev, Importance, aspect_mean = mean(Importance)) %>%
  ungroup() %>%
  mutate(Aspect = fct_reorder(Aspect, aspect_mean, .fun='median'),
         type = factor(type, levels = c("Metro", "Midsized", "Exurban", "Rural"))) %>%
  ggplot(aes(y=Aspect,x=aspect_mean,color=type)) +
  geom_errorbarh(aes(xmin=aspect_mean-(StDev*2),
                     xmax=aspect_mean+(StDev*2)),
                 height =.05,size=1,
                 position =  position_dodge(width = 0.2)) +
  geom_point(size=2,position =  position_dodge(width = 0.2)) +
  scale_color_brewer(palette = "RdYlBu") + 
  theme_minimal() +
  labs(title = "", 
       color="Urban Typology",
       x = "Mean Feature Importance",
       y = "Cluster") +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 20, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16))



ggsave("Plots/vip_imp_aspects.png", height = 3, width = 10)







# Partial Dependence Plots ------------------------------------------

var_x <- vip_frame2 %>% 
  distinct(Variable) %>%
  mutate(Variable = as.character(Variable)) %>%
  pull()

# var_x <- "delta_eviction_rates"

for(x in seq_along(var_x)){
if(grepl("X1", var_x[x])){
   seq_var <- as.factor(c(0,1)) # if the feature is a dummy variable, create a 0-1 sequence
}else{ # if it isn't, create a 5 step plot
  range_of_var <- range(train_data_processed2 %>% select(var_x[x])) # get the range of the feature
  seq_var <- seq(min(range_of_var), max(range_of_var), by = (max(range_of_var)-min(range_of_var))/4) # create a sequence for plotting
}

possible_profiles <- # generate a tibble of the different typology codes
  tibble(tibble(urban_type_Metro = c(1,0,0,0)),
         tibble(urban_type_Mid.sized = c(0,1,0,0)),
         tibble(urban_type_Rural = c(0,0,1,0)))

pdp_by_type <- c() # create an empty vector to store the results

# Filter by each typology
for (r in 1:nrow(possible_profiles)){
  data <- train_data_processed2 %>%
    inner_join(possible_profiles[r,]) # filters the data by the possible profiles 
  
  # Predict across the sequence
  for (i in seq_var){
    
    pred <- mean(predict(mod_rf_final,
                         data %>%
                           mutate("{var_x[x]}" := i),
                         type = "prob")$Above_Average) # get the mean of the predictions across the variable
    pdp_by_type <- bind_rows(
      tibble(val=i,pred=pred,type=case_when(r == 1 ~ "Metro",
                                            r == 2 ~ "Midsized",
                                            r == 3 ~ "Rural",
                                            TRUE ~ "Exurban")), pdp_by_type) # bind each prediction together by typology
  }
}

# calculate the baseline 
baseline <- pdp_by_type %>%
  filter(val == min(val)) %>%
  mutate(baseline = pred) %>%
  select(type, baseline) %>%
  distinct()

# Join the baseline to the pdp frame and find the difference between the predictions and the baseline
pdp_by_type <- left_join(pdp_by_type, baseline, by = "type") %>%
  mutate(val,
         pred = pred-baseline,
         type = factor(type, levels = c("Metro", "Midsized", "Exurban", "Rural")),
         .keep = "none")

# plot
p <-
  pdp_by_type %>%
  mutate(val = as.numeric(val)) %>%
  ggplot(aes(x=val,y=pred, color = type)) +
  geom_hline(yintercept=0,lty=2,color="#bcbdc0",size=.5) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "RdYlBu",
                     labels = c("Metro", "Midsized", "Exurban", "Rural")) +
  scale_y_continuous(limits=c(-.2, .25),
                     breaks=seq(-.2, .25, by = .15)) +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 12, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 7),
        legend.position="none"
  ) +
  labs(title = "",
       color = "Urban Typology",
       x= var_x[x]%>%
         gsub("^d_|_X1", "", .) %>%
         gsub("_|\\.", " ", .)%>%
         gsub("avg", "average", .)%>%
         gsub("pct", "percent", .)%>%
         gsub("min", "minimum", .)%>%
         gsub("af am", "african american", .)%>%
         str_to_title(.)%>%
         gsub("Gdp", "GDP", .),
       y="Probability of Above Average Evictions")

p

# save
write_rds(p, paste0("Data/Output/RDS_data/", var_x[x], "_plot.rds"))
ggsave(paste0("Plots/PDP/", var_x[x], "_plot.png"), width = 2, height = 2)
}










