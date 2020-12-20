### Project: Effect of Crime on GDP-Growth - Criminology, HSG
### Load outcome and control variables


##### Preamble ####
## librarys
library(readr)
library(zoo)
library(stats)
library(TSPred)
library(forecast)
library(lubridate)
library(data.table)
library(tidyverse)
library(hdm)
library(GGally)
library(ggcorrplot)
library(xtable)
library(fastDummies)
library(pcaMethods) # different pca-methods to deal with missing values
library(readxl)


## functions
filter <- dplyr::filter
select <- dplyr::select
ggpairs <- GGally::ggpairs
ggcorrplot <- ggcorrplot::ggcorrplot

## constants
significance_level = 0.05

## paths
setwd("D:/GitHub/crime_on_gdp") # setwd
outpath <- "./output/" # output
plots <- "./output/plots/"
tables <- "./output/tables/"
datapath <- "./data/" # data files (input datafile from package)



####  1) Import Data ####
## import special datasets manually 
data_name_vector_other <- c(
  "migration",
  "unemployment",
  "population_abs"
)

unemployment_original <- read_csv("data/unemployment.csv", 
                         col_types = cols(Value = col_double()))
unemployment <- unemployment_original

population_abs_original <- read_csv("data/population_abs.csv") # only to get relative migration to pop
population_abs <- population_abs_original

migration_original <- read_csv("data/migration.csv")
migration<- migration_original


## import similar datasets via function
data_name_vector_standard <- c(
  "educ_university",
  "educ_secondary",
  "gdp",
  "inflation",
  "investment",
  "population_and_populatin_share_male",
  "poverty"
)

import_function_standard <- function(data_name, year_vector) {
  data_import <- read_csv(paste0("data/",data_name,".csv"), 
                          col_types = cols(`2008` = col_number(), 
                                           `2009` = col_number(), `2010` = col_number(), 
                                           `2011` = col_number(), `2012` = col_number(), 
                                           `2013` = col_number(), `2014` = col_number(), 
                                           `2015` = col_number(), `2016` = col_number(), 
                                           `2017` = col_number(), `2018` = col_number()))
  colnames(data_import)[1] <- "id_vars"
  return(data_import)
}
standard_data_sets <- lapply(data_name_vector_standard, import_function_standard)
names(standard_data_sets) <- data_name_vector_standard

##  Edit standard import datasets that contain multiple variables 
# gdp: two measures, split in 2 datasets
standard_data_sets$gdp_head <- standard_data_sets$gdp[grepl("CLV_PCH_PRE_HAB", standard_data_sets$gdp$id_vars, fixed = TRUE),]
standard_data_sets$gdp <- standard_data_sets$gdp[grepl("CLV_PCH_PRE_HAB", standard_data_sets$gdp$id_vars, fixed = TRUE)==F,]

# population and male share
standard_data_sets$young_males <-  standard_data_sets$population_and_populatin_share_male[grepl("25-49;M", standard_data_sets$population_and_populatin_share_male$id_vars, fixed = TRUE),]
standard_data_sets <- standard_data_sets[names(standard_data_sets)!="population_and_populatin_share_male"]

#### 2) Preprocessing for each dataset
## Goal: Format to panel data with year, country as id cols, variables as cols

## special datasets
# unemployment
colnames(unemployment)[colnames(unemployment)=="TIME"] <- "year"
colnames(unemployment)[colnames(unemployment)=="GEO"] <- "country"
colnames(unemployment)[colnames(unemployment)=="Value"] <- "unemployment_rate"
unemployment <- select(unemployment, c("year","country","unemployment_rate"))

# population_abs
colnames(population_abs)[colnames(population_abs)=="TIME"] <- "year"
colnames(population_abs)[colnames(population_abs)=="GEO"] <- "country"
colnames(population_abs)[colnames(population_abs)=="Value"] <- "population_abs"
population_abs <- select(population_abs, c("year","country","population_abs"))

population_abs$population_abs[population_abs$population_abs==":"] <- NA # recode NAs
population_abs$population_abs <- as.numeric(str_replace_all(population_abs$population_abs,",","")) # remove "," in value

# migration
colnames(migration)[colnames(migration)=="TIME"] <- "year"
colnames(migration)[colnames(migration)=="GEO"] <- "country"
colnames(migration)[colnames(migration)=="Value"] <- "migration"

migration$migration[migration$migration==":"] <- NA # recode NAs
migration$migration <- as.numeric(str_replace(migration$migration,",","")) # remove "," in value

# calculate relative migration measure
migration_pop_merged <- left_join(migration, population_abs, by=c("year","country"))%>%
  mutate(migration_relative = migration / population_abs*100) %>%
  select(c("year","country","migration_relative"))


## similar datasets via function
standard_transform <- function (data,variable_name){
  colnames(data)[1] <- c("id_vars")
  # Split first variable string
  str_split_first_col <- str_split(data$id_vars,";",simplify=FALSE)
  # crime_data$freq <- NA # freq only has possible value "A"
  data$country <- NA
  
  for (i in 1:nrow(data)){
    # crime_data$freq[i] <- str_split_first_col[[i]][1] # freq only has possible value "A"
    data$country[i] <- str_split_first_col[[i]][length(str_split_first_col[[1]])] # last entry is usually country
  }
  data <- select(data, -id_vars)
  
  # change years from wide to long
  data <- select(data, c("country","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")) %>%
    pivot_longer(cols = c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                             names_to = "year") %>%
    mutate(value = as.numeric(value))
  
  return(data)
}

standard_data_sets_transformed <- lapply(standard_data_sets, standard_transform) 

# name "value" after dataset
for (i in 1:length(standard_data_sets_transformed)){
  colnames(standard_data_sets_transformed[[i]])[ colnames(standard_data_sets_transformed[[i]])=="value"] = names(standard_data_sets_transformed)[i]
}

##### 3) Merge Variables
all_data <- standard_data_sets_transformed[[1]]

for (i in 2:length(standard_data_sets_transformed)){
  all_data <- full_join(all_data, standard_data_sets_transformed[[i]],by=c("year", "country"))
}

# replace NAs by country means, then by overall means if no country value (this will be part of fixed effect)
replace_na_by_country_mean <- function(vector, country_vector) {
  # get means per country
  country_means <- rep(NA, length(unique(country_vector)))
  for (i in 1:length(country_means)){
    country_means[i] <- mean(vector[country_vector == unique(country_vector)[i]],na.rm=T)
  }
  names(country_means) = unique(country_vector)
  
  # insert means per country if NA
  country_means_to_vector <- rep(NA, length(vector))
  for (i in 1:length(country_means_to_vector)){
    country_means_to_vector[i] <- country_means[names(country_means)==country_vector[i]]
  }
  vector[is.na(vector)] <- country_means_to_vector[is.na(vector)]
  
  # insert overall mean if no value for a country
  vector[is.nan(vector)] <- mean(vector, na.rm=T)
  
  return(vector)
}

all_data[,3:ncol(all_data)] <- sapply(all_data[,3:ncol(all_data)], replace_na_by_country_mean, country_vector = all_data$country)


#### create 1 year lags #####
all_data_lag <- group_by(all_data, country) %>%
  mutate_at(.funs=lag, .vars = colnames(all_data)[3:ncol(all_data)])
colnames(all_data_lag)[3:ncol(all_data_lag)] <- paste0(colnames(all_data_lag)[3:ncol(all_data_lag)],"_lag1")

all_data <- left_join(all_data, all_data_lag, by = c("year","country"))

##### add fixed effects and time fixed effects #####
country_fixed_effect = fastDummies::dummy_cols(all_data$country,remove_first_dummy=T)
time_fixed_effect = fastDummies::dummy_cols(all_data$year,remove_first_dummy=T)

all_data <- cbind(all_data,country_fixed_effect[,-1],time_fixed_effect[,-1])

#### save data #####
other_variables <- all_data
save(other_variables, file = "other_variables.R")
  
  