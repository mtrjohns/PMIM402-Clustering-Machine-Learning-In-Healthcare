#------------------------------------------------------------------------------
# Clustering - PMIM402
# Michael Johns - 853369
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Install required packages (un-comment if not already installed)
#------------------------------------------------------------------------------
#install.packages("tidyverse")
#install.packages("factoextra")
#install.packages("cluster")

#------------------------------------------------------------------------------
# Load required Libraries
#------------------------------------------------------------------------------
library(tidyverse) # Data manipulation
library(factoextra) # Cluster output
library(cluster) # Hierarchical clustering algorithms

#------------------------------------------------------------------------------
# Load required data set
#------------------------------------------------------------------------------
heart <- read.csv("heart-c.csv")

# View data set
view(heart)

#------------------------------------------------------------------------------
# Check data and remove rows containing NA
#------------------------------------------------------------------------------
heart %>% summary
heart <- heart %>% drop_na()
heart %>% summary

#------------------------------------------------------------------------------
# Remove labels and identifier columns to simulate unsupervised learning
# 'X' label and 'num' identifier
#------------------------------------------------------------------------------
heart <- heart %>% select(-X, -num)

#------------------------------------------------------------------------------
# Convert any columns containing a non-numeric value to a factor
# Convert all columns to a numeric
#------------------------------------------------------------------------------
heart$age <- as.numeric(heart$age)

heart$sex <- as.factor(heart$sex)
heart$sex <- as.numeric(heart$sex)

heart$cp <- as.factor(heart$cp)
heart$cp <- as.numeric(heart$cp)

heart$trestbps <- as.numeric(heart$trestbps)

heart$chol <- as.numeric(heart$chol)

heart$fbs <- as.factor(heart$fbs)
heart$fbs <- as.numeric(heart$fbs)

heart$restecg <- as.factor(heart$restecg)
heart$restecg <- as.numeric(heart$restecg)

heart$thalach <- as.numeric(heart$thalach)

heart$exang <- as.factor(heart$exang)
heart$exang <- as.numeric(heart$exang)

heart$oldpeak <- as.numeric(heart$oldpeak)

heart$slope <- as.factor(heart$slope)
heart$slope <- as.numeric(heart$slope)

heart$ca <- as.numeric(heart$ca)

heart$thal <- as.factor(heart$thal)
heart$thal <- as.numeric(heart$thal)

#------------------------------------------------------------------------------
# scale values to aid plotting
#------------------------------------------------------------------------------
heartNormalised <- as.data.frame(scale(heart))
View(head(heartNormalised))

#------------------------------------------------------------------------------
# Plot histograms of each feature to view distribution
#------------------------------------------------------------------------------
heartNormalised %>%
  gather(attributes, value, 1:13) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = 'lightblue2', color = 'black', bins = 15) +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x="Values", y="Frequency") +
  ggtitle("Normalised Heart Data Histogram") +
  theme_bw()

# run k-means with different amount of clusters
heartKMeans2 <- kmeans(heartNormalised, centers = 2)
heartKMeans2$cluster

fviz_cluster(heartKMeans2, geom = "point", data = heartNormalised)

heartKMeans3 <- kmeans(heartNormalised, centers = 3)
#View(heartKMeans3)
fviz_cluster(heartKMeans3, geom = "point", data = heartNormalised)

heartKMeans4 <- kmeans(heartNormalised, centers = 4)
#View(heartKMeans4)
fviz_cluster(heartKMeans4, geom = "point", data = heartNormalised)

heartKMeans5 <- kmeans(heartNormalised, centers = 5)
#View(heartKMeans5)
fviz_cluster(heartKMeans5, geom = "point", data = heartNormalised)

#------------------------------------------------------------------------------
# Plot histograms of each feature to view distribution
#------------------------------------------------------------------------------
error<-vector()

for (i in 1:15){
  error[i]<-kmeans(heartNormalised,i)$tot.withinss 
}

# error plot to determine optimum cluster amount
plot(error)


