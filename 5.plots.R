library(tidyverse)
library(naniar)
library(tidycensus)
library(tigris)
library(ggthemes)
library(janitor)
library(albersusa)
library(gridExtra)
library(corrplot)
library(RColorBrewer)
options(scipen=999)
options(tigris_use_cache = TRUE)


# Correlation Matrix
evictions_data <- read_csv("Data/Output/CSV_data/train_data_processed.csv") %>%
  select(-evictions, -year, -geoid)


corr_simple <- function(data=df,sig=0.5){

  data <- data %>%
    select_if(is.numeric)
  
  #run a correlation and drop the insignificant ones
  corr <- cor(data)
  
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  
  #drop perfect correlations
  corr[corr == 1] <- NA 
  
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  
  #remove the NA values from above 
  corr <- na.omit(corr) 
  
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  
  #print table
  print(corr)
  
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, col=brewer.pal(n=8, name="RdYlBu"), na.label=" ", tl.cex=.5)
}


# Save correlation as pdf
pdf(file = "Plots/Corrplots/corr_all.pdf")

corr_simple(evictions_data)

dev.off()





# Read in evictions data and top variables
evictions_data <- read_csv("Data/Output/final_frame.csv")

cty_sf <- counties_sf("aeqd") %>%
  mutate(geoid = as.character(fips), .keep = "unused")

cty_evictions <- left_join(cty_sf, evictions_data, by= "geoid")

state_sf <- usa_sf("aeqd")


# Continuous scale
p <- 
  ggplot() +
  geom_sf(data = cty_evictions, size = 0.1, color = NA,
          aes(fill = eviction_rate)) +
  scale_fill_gradient2(high="#d73126", mid = "#ffe090",
                        guide="colorbar",na.value="#f2f1ed") +
  geom_sf(data = state_sf, fill=NA, color = "black", size = .5) +
  theme_map() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 18, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 14),
        legend.position="right") +
  labs(title = "Eviction Rates Spatial Distribution",
       fill = "Eviction Rate") 


# Discrete scale
p <- 
  cty_evictions %>%
  mutate(evictions = as.character(d_above_avg_eviction_rate)) %>%
  ggplot() +
  geom_sf(size = 0.1, color = NA,
          aes(fill = evictions)) +
  scale_fill_manual(values = c("#fdae6b", "#e6550d"), 
                    labels = c("Below Average", "Above Average", "No Data"), 
                    na.value="#f2f1ed") +
  geom_sf(data = state_sf, fill=NA, color = "black", size = .5) +
  theme_map() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 18, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 14),
        legend.position="right") +
  labs(title = "Eviction Rates Spatial Distribution",
       fill = "Eviction Rate") 


p

# save map
ggsave("Plots/map.png")









# Plot missingness of variables
final_frame <- read_csv("Data/Output/final_frame.csv")

final_frame %>%
  select(urban_type, all_of(aspects$variable)) %>%
  gg_miss_fct(fct = urban_type) +
  labs(title = "Missing Demographic Predictors by Urban Typology",
       x = "", y = "", fill = "Percent Missing") +
  theme_minimal() +
  scale_fill_gradientn(colors = c("#fae4dd", "#de4815")) +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 20, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16),
        legend.position="right")

ggsave("Plots/missingness_demographic.png", width = 10, height = 4)



# Plot missingness of aspects
aspects <- read_csv("Data/Output/var_aspects_simple.csv") %>%
  janitor::clean_names()



aspect_missing_metro <- 
  final_frame %>%
  filter(urban_type == "Metro") %>%
  miss_var_summary() %>%
  left_join(aspects) %>%
  drop_na(aspect) %>%
  group_by(aspect) %>%
  summarize(aspect_miss = sum(n_miss)) %>%
  mutate(share_aspect_miss = aspect_miss/8162,
         type = "Metro")

aspect_missing_midsized <- 
  final_frame %>%
  filter(urban_type == "Mid-sized") %>%
  miss_var_summary() %>%
  left_join(aspects) %>%
  drop_na(aspect) %>%
  group_by(aspect) %>%
  summarize(aspect_miss = sum(n_miss)) %>%
  mutate(share_aspect_miss = aspect_miss/4487,
         type = "Midsized")

aspect_missing_rural <- 
  final_frame %>%
  filter(urban_type == "Rural") %>%
  miss_var_summary() %>%
  left_join(aspects) %>%
  drop_na(aspect) %>%
  group_by(aspect) %>%
  summarize(aspect_miss = sum(n_miss)) %>%
  mutate(share_aspect_miss = aspect_miss/2142,
         type = "Rural")

aspect_missing_exurban <- 
  final_frame %>%
  filter(urban_type == "Exurban") %>%
  miss_var_summary() %>%
  left_join(aspects) %>%
  drop_na(aspect) %>%
  group_by(aspect) %>%
  summarize(aspect_miss = sum(n_miss)) %>%
  mutate(share_aspect_miss = aspect_miss/7189,
         type = "Exurban")

aspect_missing <- rbind(aspect_missing_metro, aspect_missing_midsized, aspect_missing_rural, aspect_missing_exurban) %>%
  mutate(type = factor(type, levels = c("Metro", "Midsized", "Exurban", "Rural")))


aspect_missing %>%
  filter(aspect != "Quality control" & aspect != "Classification") %>%
  ggplot(aes(x = aspect, y = share_aspect_miss, fill = type)) +
  geom_bar(stat = "identity", position = "dodge2") +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_y_continuous(limits = c(0,.5),
                     breaks=seq(0, .5, by = .125),
                     label = c("0%", "12.5", "25%", "37.5", "50%")) +
  #coord_flip() +
  theme_minimal() +
  labs(title = "Relative Share of Missing Variables within Clusters",
       x = "Clusters", y = "Share of Missing Variables",
       fill = "") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 16, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 14),
        legend.position="right")


ggsave("Plots/missingness_cluster.png", width = 11, height = 3)


# Plot class imbalance of outcome variable
train_data_processed <- read_csv("Data/Output/CSV_data/train_data_processed.csv")


train_data_processed %>%
  mutate(evictions = ifelse(evictions == "Above_Average", "Above Average", "Below Average")) %>%
  ggplot(aes(evictions)) +
  geom_bar(fill = "#000000") +
  scale_y_continuous(limits = c(0,17586),
                      breaks=seq(0, 17586, by = 4396.5),
                     label = c("0%", "25%", "50%", "75%", "100%")) +
  labs(title = "Outcome Variable Class Balance",
       x = "", y = "Share of Total") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 20, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16),
        legend.position="right")

ggsave("Plots/class_balance.png", width = 5, height = 4)












# Plot share of flagged counties
final_frame %>%
  mutate(urban_type = factor(urban_type, levels = c("Metro", "Mid-sized", "Exurban", "Rural"))) %>%
  ggplot(aes(x = urban_type, y = d_low_flag, fill = urban_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0,2747.5),
                   breaks=seq(0, 2747.5, by = 1373.75),
                   label = c("0%", "5%", "10%")) +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_map() +
  labs(title = "Low Estimates Flag",
       x = "", y = "",
       fill = "Urban Typology") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 16, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16),
        line = element_blank(),
        legend.position="none")

ggsave("Plots/flagged_counties.png", width = 4, height = 3)




# Plot flagged counties
cty_evictions %>%
  filter(d_low_flag == 1) %>%
  mutate(d_low_flag = as.character(d_low_flag),
         urban_type = factor(urban_type, levels = c("Metro", "Mid-sized", "Exurban", "Rural"))) %>%
  ggplot() +
  geom_sf(data = state_sf, fill= "#f2f1ed") +
  geom_sf(size = 0.1, color = NA,
          aes(fill = urban_type)) +
  scale_fill_brewer(palette = "RdYlBu") +
  geom_sf(data = state_sf, fill= "NA", color = "black", size = .5) +
  theme_map() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 18, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 14),
        legend.position="right") +
  labs(title = "Low Eviction Estimates Spatial Distribution",
       fill = "Low Estimates Flag")

ggsave("Plots/map_flag.png")





# Plot share of flagged counties
final_frame %>%
  mutate(evictions = as.character(d_above_avg_eviction_rate)) %>%
  group_by(urban_type) %>%
  count(evictions) %>% 
  summarize(urban_type, evictions, n, count = sum(n)) %>%
  ungroup() %>%
  mutate(rate = n/count,
         urban_type = factor(urban_type, levels = c("Metro", "Mid-sized", "Exurban", "Rural"))) %>%
  ggplot(aes(x = urban_type, y = rate, fill = evictions)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks=seq(0, 1, by = .5),
                     label = c("0%", "50%", "100%")) +
  scale_fill_manual(labels = c("Below Average", "Above Average", "No Data"),
                    values = c("#fdae6b", "#e5550e"), na.value = "#f2f1ed") +
  theme_minimal() +
  labs(title = "Relative Share of Eviction Rates by Typology",
       fill = "Eviction Rates",
       x = "", y = "") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 16, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 12),
        legend.position="right")

ggsave("Plots/eviction_rates_typology.png", width = 5, height = 3)






# Minimum wage by typology
evictions_data %>%
  mutate(year = as.numeric(as.character(year)),
         urban_type = factor(urban_type, levels = c("Metro", "Mid-sized", "Exurban", "Rural"))) %>%
  group_by(year, urban_type) %>%
  summarize(avg = mean(min_wage, na.rm = TRUE)) %>%
  ggplot(aes(y = avg, x = year, color = urban_type)) +
  geom_line(size = 1)+
  scale_color_brewer(palette = "RdYlBu") +
  theme_map() +
  labs(title = "",
       x = "", y = "Average Minimum Wage",
       color = "Urban Typology") +
  theme_minimal() +
  theme(plot.title=element_text(hjust = 0.5,
                                size = 16, 
                                family = "serif"),
        text = element_text(family = "serif",
                            size = 16),
        line = element_blank(),
        legend.position="right")

ggsave("Plots/min_wage_typology.png", width = 7, height = 4)
