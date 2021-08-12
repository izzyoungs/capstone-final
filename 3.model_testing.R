library(here)
library(tidyverse)
library(caret)
library(parallel)
library(doParallel)
library(rsample)
library(yardstick)
library(recipes)
library(vip)
library(forcats)
library(albersusa)
library(ggthemes)

set.seed(1992) # Ensure reproducibility

# Read in the data 
evictions_data <- read_csv("Data/Output/final_frame.csv") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year)) %>%
  select(-3:-8) #remove state and county data, only keep geoid and year


# We will only be using the eviction filing rate categorical variable
ind_var <- "d_above_avg_eviction_rate"


evictions_cleaned <- evictions_data %>%
  
  # Select all columns and remove vars we won't use
  select(evictions = all_of(ind_var), everything()) %>%
  
  # Rename variables so they are not dummy variables
  mutate(evictions = as.factor(ifelse(evictions == 0, "Below_Average", "Above_Average")))

# Stratify and split the data ---------------------------------------------


# Create the split for counties
split <- initial_split(evictions_cleaned, 
                       prop = .8, 
                       strata = "evictions") # Make sure the class is equal in each split

# Create test and train objects
test_data  <- testing(split)
train_data <- training(split)


# Preprocess data ---------------------------------------------------------


# Initialize our recipe
our_recipe <- recipe(evictions ~ ., data = train_data) %>%
  step_knnimpute(all_nominal()) %>%
  step_knnimpute(all_numeric()) %>% 
  step_log(all_numeric(), signed = TRUE) %>%
  step_dummy(all_nominal(), -evictions, -geoid, -year) %>%
  prep() %>% suppressMessages()

# Apply the recipe to the training and test data
train_data_processed <- suppressMessages(bake(our_recipe,train_data))
test_data_processed <- suppressMessages(bake(our_recipe,test_data))

# write_csv(train_data_processed, "Data/Output/CSV_data/train_data_processed.csv") # Save data to expedite future analyses
# write_csv(test_data_processed, "Data/Output/CSV_data/test_data_processed.csv") # Save data to expedite future analyses


# train_data_processed <- read_csv("Data/Output/CSV_data/train_data_processed.csv") # Read in data to expedite future analyses

# Partition the data into 5 folds
folds <- createFolds(train_data_processed$evictions, k = 5) 

# Cross validation settings as an object
control_conditions <- 
  trainControl(method='cv',
               summaryFunction = twoClassSummary, 
               classProbs = TRUE,
               sampling = "up",
               index = folds)

tunegrid <- expand.grid(mtry=11, 
                        splitrule = "gini", 
                        min.node.size = 1)

# Random Forest Model
mod_rf <-
  train(evictions ~ . -geoid -year,
        data=train_data_processed,
        method = "ranger",
        num.trees = 100,
        metric = "ROC",
        tuneGrid=tunegrid,
        trControl = control_conditions)


# write_rds(mod_rf, "Data/Output/RDS_data/rf.rds") # Save data to expedite future analyses


# Rerun model after VI ------------------------------------------

# mod_rf <- read_rds("Data/Output/RDS_data/rf.rds") # Read in data to expedite future analyses

# train_data_processed <- read_csv("Data/Output/CSV_data/train_data_processed.csv") # Read in data to expedite future analyses
# test_data_processed <- read_csv("Data/Output/CSV_data/test_data_processed.csv") # Read in data to expedite future analyses

top_vi <- read_rds("Data/Output/RDS_data/test/vip_top_vars.rds") # read in the top  variables after variable importance permutation (step 5)

# clean up variable types and select top vars
train_data_processed2 <- train_data_processed %>%
  select(geoid, year, evictions, contains("urban_type"), all_of(top_vi)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year))

# clean up variable types and select top vars
test_data_processed2 <- test_data_processed %>%
  select(geoid, year, evictions, contains("urban_type"), all_of(top_vi))%>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year))



# write_csv(train_data_processed2, "Data/Output/CSV_data/train_data_processed2.csv") # Save data to expedite future analyses
# write_csv(test_data_processed2, "Data/Output/CSV_data/test_data_processed2.csv") # Save data to expedite future analyses


# Partition the data into 5 folds
folds <- createFolds(train_data_processed2$evictions, k = 5) 

# Cross validation settings as an object
control_conditions <- 
  trainControl(method='cv',
               summaryFunction = twoClassSummary, 
               classProbs = TRUE,
               savePredictions = TRUE,
               sampling = "up",
               index = folds)

tunegrid <- expand.grid(mtry=5, # reduce mtry since number of vars dropped
                        splitrule = "gini", 
                        min.node.size = 1)

# Random Forest Model
mod_rf_final <-
  train(evictions ~ . -geoid -year,
        data=train_data_processed2,
        method = "ranger",
        num.trees = 100,
        metric = "ROC",
        tuneGrid=tunegrid,
        trControl = control_conditions)

# write_rds(mod_rf_final, "Data/Output/RDS_data/rf_final.rds") # Save data to expedite future analyses


# Test Data ------------------------------------------

mod_rf_final <- read_rds("Data/Output/RDS_data/rf_final.rds")
train_data_processed2 <- read_csv("Data/Output/CSV_data/train_data_processed2.csv")%>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year))
test_data_processed2 <- read_csv("Data/Output/CSV_data/test_data_processed2.csv")%>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(vars(starts_with("d_")), as.factor) %>%
  mutate(year = as.factor(year))

# Predict on test data
pred_prob <- predict(mod_rf_final, newdata = test_data_processed2, type="prob")
pred_raw <- predict(mod_rf_final, newdata = test_data_processed2, type="raw")

performance <- tibble(truth = test_data_processed2$evictions,
                      prob = pred_prob$Above_Average,
                      pred = pred_raw) 

test_data_predictions <- test_data_processed2
test_data_predictions$predictions <- pred_raw 

test_data_predictions <- test_data_predictions %>%
  mutate(performance = case_when(predictions == "Above_Average" & evictions == "Above_Average" ~ "TP",
                          predictions == "Above_Average" & evictions == "Below_Average" ~ "FP",
                          predictions == "Below_Average" & evictions == "Above_Average" ~ "FN",
                          predictions == "Below_Average" & evictions == "Below_Average" ~ "TN"))


# Calculate performance metrics 
bind_rows(
  performance %>% roc_auc(truth,prob),
  performance %>% spec(truth,pred),
  performance %>% sens(truth,pred),
  performance %>% accuracy(truth,pred)
)

confusionMatrix(performance$pred, performance$truth)

write_csv(performance, "Data/Output/RDS_data/performance.rds")

write_csv(test_data_predictions, "Data/Output/CSV_data/test_data_predictions.csv")

# Map the inaccuracies
cty_sf <- counties_sf("aeqd") %>%
  mutate(geoid = as.character(fips)) %>%
  select(geoid, geometry)

cty_evictions <- right_join(cty_sf, test_data_predictions, by= "geoid")

state_sf <- usa_sf("aeqd")

# Discrete scale
f <- 
  cty_evictions %>%
  drop_na(predictions) %>%
  ggplot() +
  geom_sf(size = 0.1, color = NA,
          aes(fill = performance)) +
  scale_fill_brewer(palette = "RdYlBu", na.value="#3e87ba",
                    labels = c("False Negative", "False Positive", "True Negative", "True Positive")) +
  geom_sf(data = state_sf, fill=NA, color = "black", size = .5) +
  theme_map() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 18, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 14),
        legend.position="right") +
  labs(title = "Predictions Spatial Distribution",
       fill = "Predictions") 


f

ggsave("Plots/inaccurate.png")
