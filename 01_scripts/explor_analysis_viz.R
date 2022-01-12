################################################################################
# Exploritory data analysis and visualizations for New Const. Home Analysis    #
# Nolan Peterson                                                               #
# 12/28/2021                                                                   #
# https://github.com/NolanPeterson453/New-Construction-Analysis-               #
################################################################################

# Library statements 
library(tidyverse)
library(splitstackshape)
library(hablar)
library(scales)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(lubridate)

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

# Expand by Sample weight 
soc_data_expanded <- expandRows(soc_data, 'WEIGHT')

# Summary statistics for response
summary(soc_data_expanded$FSLPR) 
cat(paste0('Mean Sale Price $',round(mean(soc_data_expanded$FSLPR), 2), '.\n'))
cat(paste0('Median Sale Price $',round(median(soc_data_expanded$FSLPR), 2), '.\n'))

# Color Palette for ploting 
econ_palette <- c("#e3120b", "#A37C27", "#91b8bd",
                  "#F05837", "#9ae5de", "#244747",
                  "#6A8A82", "#A7414A", "#660663")

# Distribution of Home Sale Price plot
cuts <- data.frame(ref = c("Mean Sale Price", "Median Sale Price"),
                   vals = c(mean(soc_data_expanded$FSLPR), median(soc_data_expanded$FSLPR)),
                   stringsAsFactors = FALSE)
soc_data_expanded %>% 
  ggplot(aes(x = FSLPR)) +
  geom_bar(stat="bin", 
           fill = "#244747") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of Home Sale Price",
       x = "Final Sale Price of Home", 
       y = "Count of Homes") + 
  geom_vline(aes(xintercept = vals, color = ref),
             data = cuts,
             show.legend = FALSE) +
  geom_text(aes(x = vals,
                y = c(50000,55000), 
                label = ref,
                color = ref,
                angle = 90, 
                vjust = 0.90),
            data = cuts,
            show.legend = FALSE)

# Boxplot of sale price grouped by region
regions <- c("New England",
             "Middle Atlantic",
             "East North Central",
             "West North Central",
             "South Atlantic",
             "East South Central",
             "West South Central",
             "Mountain",
             "Pacific")

soc_data_expanded %>% 
  ggplot(aes(y = FSLPR, x = DIV, color = DIV)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_discrete(breaks = c(1:9),
                   labels = regions) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=8, angle=90)) +
  labs(title = "Sale Price Grouped By Region",
       x = "Region",
       y = "Sale Price",
       color = "Region") +
  scale_color_manual(values = econ_palette, labels = regions)

# Anova for association between DIV and FSLPR
ANOVA_FSLPR_DIV <- anova(lm(FSLPR ~ DIV, 
         data = soc_data,
         weights = WEIGHT))
ANOVA_FSLPR_DIV



# Examine correlation between response and key variables 
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

## Explore association between SQFS and FSPLR 

# Correlation 
r_SQFS_FSLPR <- round(cor(soc_data_expanded$FSLPR, soc_data_expanded$FSQFS), 2)
cat(paste0('The correlation between sale price and square footage is ', r_SQFS_FSLPR, '.'))

# Linear Model 
FSLPR_FSQFS_mod <- lm(FSLPR ~ FSQFS, 
                     data = soc_data,
                     weights = WEIGHT)
summary(FSLPR_FSQFS_mod)

## Box plots for the sale price grouped by, bedrooms, bathrooms, and half baths
 
# Bedroom boxplot
bedroom_labs <- c("not reported",
                  "2 bedrooms or less",
                  "3 bedrooms",
                  "4 bedrooms",
                  "5 bedrooms or more")
bedr_plot <- soc_data_expanded %>% 
  ggplot(aes(y = FSLPR, x = BEDR, color = BEDR)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_discrete(breaks = c(0, 2:5),
                   labels = bedroom_labs) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=8, angle=90)) +
  labs(title = "Sale Price Grouped By Number of Bedrooms",
       x = "Bedrooms",
       y = "Sale Price",
       color = "Bedrooms") +
  scale_color_manual(values = econ_palette[1:5], labels = bedroom_labs)
bedr_plot

# Full bathroom boxplot 
fullbath_labs <- c("1 bathroom or less",
                   "2 bathrooms",
                   "3 bathrooms",
                   "4 bathrooms",
                   "not reported")
fulbath_plot <- soc_data_expanded %>% 
  ggplot(aes(y = FSLPR, x = FULB, color = FULB)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_discrete(breaks = c(1:4, 9),
                   labels = fullbath_labs) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=8, angle=90)) +
  labs(title = "Sale Price Grouped By Number of Full Bathrooms",
       x = "Full Bathrooms",
       y = "Sale Price",
       color = "Full Bathrooms") +
  scale_color_manual(values = econ_palette[1:5], labels = fullbath_labs)
fulbath_plot 

# Half bathroom boxplot
halfbath_labs <- c("0 half bathrooms",
                   "1 half bathroom",
                   "2 or more half bathrooms",
                   "not reported")
halfbath_plot <- soc_data_expanded %>% 
  ggplot(aes(y = FSLPR, x = HAFB, color = HAFB)) +
  geom_boxplot() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_discrete(breaks = c(0:2, 9),
                   labels = halfbath_labs) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=8, angle=90)) +
  labs(title = "Sale Price Grouped By Number of Half Bathrooms",
       x = "Half Bathrooms",
       y = "Sale Price",
       color = "Half Bathrooms") +
  scale_color_manual(values = econ_palette[1:5], labels = halfbath_labs)
halfbath_plot

## length of construction time as predictor 
soc_data %>% 
  mutate(CONST_TIME = difftime(COMP, STRT, units = "days")) %>% 
  ggplot(aes(x = CONST_TIME, y = FSLPR)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar_format())

# Construction time vs size of house 
soc_data %>% 
  mutate(CONST_TIME = difftime(COMP, STRT, units = "days")) %>% 
  ggplot(aes(x = CONST_TIME, y = FSQFS)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
