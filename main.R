#------------------------------------------------------------------------------
# Clustering - PMIM402
# Michael Johns - 853369
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Install required packages (un-comment if not already installed)
#------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("cluster")

#------------------------------------------------------------------------------
# Load required Libraries
#------------------------------------------------------------------------------
library(tidyverse) # data manipulation
library(cluster) # Hierarchical clustering algorithms

#------------------------------------------------------------------------------
# Load required data set
#------------------------------------------------------------------------------
heart <- read.csv("heart-c.csv")

# View data set
view(heart)

# Check data for NA's
heart %>% summary

# Remove labels and identifier columns to simulate unsupervised learning
# 'X' label and 'num' identifier
heart <- heart %>% select(-X, -num)
