# Ecology Homework 3, Author: 陈凯星, Student ID: SA22234107

rm(list = ls())

# The goal of this assignment is to learn how to perform exploratory data analysis. 
# It consists of two data sets. One is the site x species, and another the site x environment. 
# Using everything you have learned to execute exploratory data analysis and answer the questions. 

# Submission
# 1. Export your R script as a Homework_03.R file 
# 2. Submit your homework through your own github
# 3. Due Date: April 21, 2023

library('tidyverse') # Load tool package
library('ade4') # Load data package



### Section I

# Read in the doubs into your R

data('doubs') 
doubs_fish <- doubs$fish
doubs_env <- doubs$env

# Delete the site 8 which has no fishes. 

doubs_fish <- doubs_fish[-8,] 
doubs_env <- doubs_env[-8,]

# Write code and answer the questions: Which site has the most species (and how many species)?  

doubs_fish_sum1 <- rowSums(doubs_fish != 0)
doubs_fish_sum1[which.max(doubs_fish_sum1)] # 29: 26

# Which species is the most widespread (i.e., found in the most sites)?

doubs_fish_sum2 <- colSums(doubs_fish !=0 )
doubs_fish_sum2[which.max(doubs_fish_sum2)] # Lece: 25



### Section II

# write code for clusters and answer: 
# In terms of the fish community composition, which groups of species can you identify? 
# Which groups of species are related to these groups of sites?

library('vegan') # Load package for cluster

### Ref to Textbook-P91

### Cluster for fish
assoc_measure <- vegdist(doubs_fish %>% t(), method = "bray") # Select a suitable association measure of species, here using "Bray-Curtis dissimilarity"
clust_result <- hclust(assoc_measure, method = "ward.D2") # Cluster

# options(repr.plot.width=12, repr.plot.height=6)
# plot(clust_result, hang = -1) # Visualizing the clustering result

# Identify the groups of species, 4 groups
cluster_groups <- cutree(clust_result, k = 4)
cluster_groups_df <- data.frame(species = colnames(doubs_fish), group = cluster_groups)
species_groups_summary <- cluster_groups_df %>% group_by(group) %>% summarize(species = paste(species, collapse = ", "))

# species_groups_summary
# A tibble: 4 × 2
# group	species
# <int>	<chr>
# 1	Cogo, Thth, Teso
# 2	Satr, Phph, Neba
# 3	Chna, Chto, Spbi, Rham, Legi, Scer, Cyca, Abbr, Icme, Acce, Blbj, Anan
# 4	Lele, Lece, Baba, Gogo, Eslu, Pefl, Titi, Ruru, Alal

### Cluster for sites
assoc_measure <- vegdist(doubs_fish, method = "bray") # Select a suitable association measure of species, here using "Bray-Curtis dissimilarity"
clust_result <- hclust(assoc_measure, method = "ward.D2") # Cluster

# options(repr.plot.width=12, repr.plot.height=6)
# plot(clust_result, hang = -1) # Visualizing the clustering result

# Identify the groups of species, 4 groups
cluster_groups <- cutree(clust_result, k = 4)
cluster_groups_df <- data.frame(sites = rownames(doubs_fish), group = cluster_groups)
sites_groups_summary <- cluster_groups_df %>% group_by(group) %>% summarize(sites = paste(sites, collapse = ", "))

# sites_groups_summary
# A tibble: 4 × 2
# group	sites
# <int>	<chr>
# 1	1, 2, 3, 4, 6, 7, 10, 11, 12, 13, 14
# 2	5, 9, 15, 16, 17, 18, 19
# 3	20, 21, 22, 26, 27, 28, 29, 30
# 4	23, 24, 25

### Cluster for sites
assoc_measure <- vegdist(doubs_env, method = "bray") # Select a suitable association measure of species, here using "Bray-Curtis dissimilarity"
clust_result <- hclust(assoc_measure, method = "ward.D2") # Cluster

# options(repr.plot.width=12, repr.plot.height=6)
# plot(clust_result, hang = -1) # Visualizing the clustering result

# Identify the groups of species, 4 groups
cluster_groups <- cutree(clust_result, k = 4)
cluster_groups_df <- data.frame(sites = rownames(doubs_env), group = cluster_groups)
sites_groups_summary <- cluster_groups_df %>% group_by(group) %>% summarize(sites = paste(sites, collapse = ", "))

# sites_groups_summary
# A tibble: 4 × 2
# group	sites
# <int>	<chr>
# 1	1, 2, 3, 4, 5, 6, 7
# 2	9, 10
# 3	11, 12, 13, 14, 15, 16, 17, 18, 19
# 4	20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30

# Group 3 of fish was in Group 4 of env



### Section III

# Do RDA analysis, and then write code and answer: 
# Which environmental variables cause a community to vary across a landscape?

rda <- rda(doubs_fish~., data = doubs_env,scale = TRUE) # rda() analysis to env variables and fish species

summary(rda)

plot(rda) # length of arrow stands for importance

### stepforward for explanation variables

step.forward <- ordistep(rda(doubs_fish ~ 1, data=doubs_env), scope = formula(rda), direction="forward", pstep = 1000)

# Start: doubs_fish ~ 1 

#       Df    AIC       F Pr(>F)   
# + dfs  1 108.53 21.9008  0.005 **
# + flo  1 110.30 19.0071  0.005 **
# + alt  1 114.84 12.3341  0.005 **
# + slo  1 118.55  7.6070  0.005 **
# + nit  1 120.41  5.4606  0.010 **
# + har  1 121.01  4.7942  0.010 **
# + oxy  1 120.04  5.8839  0.020 * 
# + bdo  1 123.69  1.9920  0.125   
# + pho  1 124.13  1.5551  0.140   
# + amm  1 124.17  1.5143  0.140   
# + pH   1 125.23  0.4924  0.705 




