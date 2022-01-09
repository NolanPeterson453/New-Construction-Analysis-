################################################################################
# Exploritory data analysis and visualizations for New Const. Home Analysis    #
# Nolan Peterson                                                               #
# 12/28/2021                                                                   #
# https://github.com/NolanPeterson453/New-Construction-Analysis-               #
################################################################################

# Library statements 
library(tidyverse)
library(hablar)
library(scales)
library(ggthemes)
library(ggrepel)

# Read in data 
soc_data <- read_csv(file = "https://raw.githubusercontent.com/NolanPeterson453/New-Construction-Analysis-/main/02_data/cleaned_soc.csv")

# Assign column types 
factor_vars <- c("ACS", "AGER", "ASSOC", "BASE", "CON", "DECK", "DET", 
                 "DIV", "FNBS", "FOYER", "FRAME", "GAR", "HEAT", "HEAT2", 
                 "LNDR", "MFGS", "PATI", "PRCH", "SEWER", "STOR", "WAL1", 
                 "WAL2", "WALS","WATER", "BEDR", "FPLS", "FULB", "HAFB", "FUEL",
                 "FUEL2")

contin_vars <- c("FSLPR", "FSQFS", "LOTV", "FFNSQ", "AREA")

soc_data <- soc_data %>% 
  convert(fct(factor_vars),
          num(contin_vars))

# Summary statistics for response
summary(soc_data$FSLPR)

# Distribution of Home Sale Price
soc_data %>% 
  ggplot(aes(x = FSLPR)) +
  geom_bar(stat="bin", 
           fill = "#244747") +
  scale_x_continuous(labels = dollar_format()) +
  labs(title = "Distribution of Home Sale Price",
       x = "Final Sale Price of Home", 
       y = "Count of Homes") +
  theme_economist() 

# Examine correlation between response and key variables 
econ_palette <- c("#e3120b", "#4a4a4a", "#91b8bd",
                  "#acc8d4", "#9ae5de", "#244747",
                  "#336666", "#8abbd0", "#660663")
soc_data %>% 
  group_by(DIV) %>% 
  summarise(FSLPR = mean(FSLPR),
            FSQFS = mean(FSQFS),
            WEIGHT = sum(WEIGHT)) %>% 
  ggplot(aes(x = FSQFS, y = FSLPR, 
             color = DIV, 
             size = WEIGHT, 
             label = c("New England",
                       "Middle Atlantic",
                       "East North Central",
                       "West North Central",
                       "South Atlantic",
                       "East South Central",
                       "West South Central",
                       "Mountain",
                       "Pacific"))) + 
  geom_point() + 
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Average Sale Price vs Average Area Grouped By Region",
       x = "Square Foot Area of Home",
       y = "Final Sale Price",
       color = "Region",
       size = "Sample Size") +
  scale_color_manual(values = econ_palette, labels = c("New England",
                                                   "Middle Atlantic",
                                                   "East North Central",
                                                   "West North Central",
                                                   "South Atlantic",
                                                   "East South Central",
                                                   "West South Central",
                                                   "Mountain",
                                                   "Pacific")) + 
  geom_text_repel(size = 3.88)
