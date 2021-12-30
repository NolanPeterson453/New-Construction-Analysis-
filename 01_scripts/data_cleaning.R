################################################################################
# Data Cleaning for New Const. Home Analysis                                   #
# Nolan Peterson                                                               #
# 12/28/2021                                                                   #
# https://github.com/NolanPeterson453/New-Construction-Analysis-               #
################################################################################

# Library statements 
library(tidyverse)
library(hablar)
library(lubridate)
library(splitstackshape)
library(Amelia)

# Read in data and display the structure 
setwd("~/projects/New-Construction-Analysis-/02_data")
soc_data <- read_csv(file = "https://raw.githubusercontent.com/NolanPeterson453/New-Construction-Analysis-/main/02_data/soc20.csv")
str(soc_data)

# Remove unwanted variables 
unwanted_vars <- c("CLOS", "FCONPR", "AUTH", "ID", "PVALU", "FNSQ_F", 
                   "CONPR", "FINC", "SLPR_F", "FSLPR_F", "FCONPR_F", "LOTV_F", 
                   "SQFS_F", "FSQFS_F", "PVALU_F", "AREA_F", "CONPR_F", 
                   "FFNSQ_F", "CAT")
soc_data <- select(soc_data, -unwanted_vars)


# Convert date variables to lubridate date type
soc_data <- soc_data %>%
  mutate(COMP = ym(COMP),
         SALE = ym(SALE),
         STRT = ym(STRT))

# Combine double reported variables 
soc_data <- soc_data %>%
  mutate(FSLPR = ifelse(test = (FSLPR == 0 & SLPR != 0), 
                        yes = SLPR, no = FSLPR),
         FFNSQ = ifelse(test = (FFNSQ == 0 & FNSQ != 0), 
                        yes = FNSQ, no = FFNSQ),
         FSQFS = ifelse(test = (FSQFS == 0 & SQFS != 0), 
                        yes = SQFS, no = FSQFS)) 

soc_data <- soc_data %>%
  select(-c(SLPR, FNSQ, SQFS))

# Remove observations where house was not sold or completed
soc_data <- soc_data %>%
  filter(FSLPR != 0,
         !is.na(COMP))

# Impute missing with mean 
soc_data <- soc_data %>% 
  mutate(FSQFS = ifelse(test = (FSQFS == 0), 
                        yes = mean(FSQFS[FSQFS != 0]), no = FSQFS),
         LOTV = ifelse(test = (LOTV == 0), 
                       yes = mean(LOTV[LOTV != 0]), no = LOTV),
         AREA = ifelse(test = (AREA == 0), 
                       yes = mean(AREA[AREA != 0]), no = AREA))

# Write csv file of cleaned data
write_csv(soc_data, 
          file = "cleaned_soc.csv" )

