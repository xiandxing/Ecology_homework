# Ecology Homework 1, Author: 陈凯星, Student ID: SA22234107

# Homework_1: Exploring doubs dataset with Tidyverse

# Requirements:
# 1. turning your homework before March 29, otherwise reject it
# 2. Representing the work as R script, i.e. homework_01.R. Note: adding comments
# 3. completing the work step by step as follows, don’t skip any one

rm(list = ls()) # Delete environment variables

# 1. 

library('tidyverse') # Load tool package 'tidyverse'
library('ade4') # Load data package 'ade4'

data('doubs') # Then 'doubs' can be used as an object containing several data frames

# 'head(doubs$env)' to see what the data looks like
# 'class(doubs$env)' to see the class of the data



# 2.

env <- doubs$env
env <- rownames_to_column(env, var = "site") # Turning the row names into a column called site
env_tb <- as_tibble(env) # then convert the data frame to a tibble, named it env_tb.



# 3.

env_final<- env_tb %>% # Use %>% pipe, and name the final variable as env_final
            subset(dfs > 1000) %>% # Extract and remain the data of the dfs with more than 1000 km
            dplyr::select(site, dfs, slo, flo, pH, nit, oxy) %>% # Select these columns for further analysis, select() may be confused by it from MASS
            rename(distsour = dfs, slope = slo, flowrate = flo, nitrogen = nit, oxygen = oxy) %>% # Rename 5 columns
            arrange(slope, desc(pH)) # Arrange the data first by slope in ascending order, and then by pH in descending order
