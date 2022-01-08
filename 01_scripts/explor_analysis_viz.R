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
