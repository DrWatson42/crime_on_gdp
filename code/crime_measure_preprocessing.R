### Project: Effect of Crime on GDP-Growth - Criminology, HSG

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

# pca function
pcatr <- function(data){
  col.means = as.vector(colMeans(data))
  m = matrix(col.means, nrow(data), NROW(col.means), byrow = T)
  x = data - m
  eig = eigen(cov(x))  # spectral decomposition  
  eig.v = eig$vectors
  x.m = as.matrix(x)
  pcs = x.m %*% eig.v
  return(as.data.frame(pcs))
}

## constants
significance_level = 0.05

## paths 
setwd("D:/GitHub/crime_on_gdp") # setwd
outpath <- "./output/" # output
plots <- "./output/plots/"
tables <- "./output/tables/"
datapath <- "./data/" # data files (input datafile from package)


####  1) Import Data ####
# crime data (parsing errors are transformation of ; to NA)
crime_data_original <- read_csv("data/crim_off_cat__custom_128689_20201028_155532.csv", 
                                                          col_types = cols(`2008` = col_number(), 
                                                                              `2009` = col_number(), `2010` = col_number(), 
                                                                              `2011` = col_number(), `2012` = col_number(), 
                                                                              `2013` = col_number(), `2014` = col_number(), 
                                                                              `2015` = col_number(), `2016` = col_number(), 
                                                                              `2017` = col_number(), `2018` = col_number()))
# 
# # corruption data
# #csvs
# CPI_2008 <- read_csv("D:/GitHub/crime_on_gdp/data/corruption/CPI-Archive-2008-2.csv")
# CPI_2009 <- read_csv("D:/GitHub/crime_on_gdp/data/corruption/CPI-2009-new_200601_120052.csv")
# CPI_2010 <- read_csv("D:/GitHub/crime_on_gdp/data/corruption/CPI-2010-new_200601_105629.csv")
# CPI_2011 <- read_csv("D:/GitHub/crime_on_gdp/data/corruption/CPI-2011-new_200601_104308.csv")
# CPI_2012 <- read_delim("D:/GitHub/crime_on_gdp/data/corruption/CPI2012_Results.csv", 
#                                                    ";", escape_double = FALSE, trim_ws = TRUE)
# CPI_2014 <- read_delim("data/corruption/CPI2014_FullDataSet.csv", 
#                        ";", escape_double = FALSE, trim_ws = TRUE)
# CPI_2015<- read_delim("data/corruption/CPI2015_FullDataSet.csv", 
#                                    ";", escape_double = FALSE, trim_ws = TRUE)
# CPI_2016 <- read_delim("data/corruption/CPI2016_FullDataSet.csv", 
#                                    ";", escape_double = FALSE, trim_ws = TRUE)
# CPI_2017 <- read_delim("data/corruption/CPI2017_FullDataSet.csv", 
#                        ";", escape_double = FALSE, trim_ws = TRUE)
# CPI_2018 <- read_delim("data/corruption/CPI2018_FullDataSet.csv", 
#                        ";", escape_double = FALSE, trim_ws = TRUE)
# 
# # need to reformat, convert country code  
# 
# 
# #xlsx
# CPI_2013 <- read_excel("data/corruption/CPI2013_GLOBAL_WithDataSourceScores.xls")


# controls


#### 2) Preprocessing Crime Data ####
crime_data <- crime_data_original
colnames(crime_data)[1] <- c("id_vars")

# Split first variable string
str_split_first_col <- str_split(crime_data$id_vars,";",simplify=FALSE)
# crime_data$freq <- NA # freq only has possible value "A"
crime_data$iccs <- NA
crime_data$unit <- NA
crime_data$country <- NA
for (i in 1:nrow(crime_data)){
  # crime_data$freq[i] <- str_split_first_col[[i]][1] # freq only has possible value "A"
  crime_data$iccs[i] <- str_split_first_col[[i]][2]
  crime_data$unit[i] <- str_split_first_col[[i]][3]
  crime_data$country[i] <- str_split_first_col[[i]][4]
}

crime_data <- select(crime_data, -id_vars) %>%
  relocate(iccs, .before='2008') %>%
  relocate(unit, after=iccs) %>%
  relocate(country, after=iccs)

# only take rates / 100.000 and drop num variable
crime_data <- filter(crime_data, unit=="P_HTHAB") %>%
  select(-unit)

# transform iccs to crime
iccs_to_crime <- cbind(paste0("ICCS",c("0101","0102","02011","020221","0301","03011","03012",
                                      "0401","0501","05012","0502","050211","0601")),
                       c("intentional_homicide_1","attempted_intentional_homicide_1",
                         "assault_2","kidnapping_2","sexual_violence_3","rape_3","sexual_assault_3",
                         "robbery_4","burglary_5","burgraly_private_residence_5",
                         "theft_5","theft_motor_cycle_5","drugs_6"))

.fun_iccs_to_crime <- function(x,crime_vector){
  return(crime_vector[x])
}
index_iccs <- match(crime_data$iccs, iccs_to_crime[,1])
crime_data$type_of_crime <- unlist(lapply(index_iccs,.fun_iccs_to_crime, crime_vector=iccs_to_crime[,2]))

main_crimes <-  c("intentional_homicide_1",
                  "assault_2","sexual_violence_3",
                  "robbery_4","burglary_5",
                  "theft_5","drugs_6")

# change years from wide to long
crime_data <- pivot_longer(crime_data, cols = c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
                                       names_to = "year")
# change crime from long to wide
crime_data <- pivot_wider(crime_data,id_cols = c("country","year"),names_from = "type_of_crime",values_from = "value")
summary(crime_data)
summary(is.na(crime_data))

# remove non-countries and kosovo
crime_data <- filter(crime_data, !country %in% c("UKC-L", "UKM","UKN","XK"))


# idea for creating a crime index: 

# only take main crimes 
crime_data <- crime_data[,c("year","country",main_crimes)]

# check for missing values & create subset with no NAs for main_crimes
summary(!is.na(crime_data$intentional_homicide_1) & !is.na(crime_data$assault_2) & !is.na(crime_data$rape_3) & !is.na(crime_data$robbery_4) & !is.na(crime_data$burglary_5) &  !is.na(crime_data$theft_5) & !is.na(crime_data$drugs_6))
crime_data_no_na_main_crimes <- crime_data[!is.na(crime_data$intentional_homicide_1)  & !is.na(crime_data$assault_2)  & !is.na(crime_data$sexual_violence_3)  & !is.na(crime_data$robbery_4) & !is.na(crime_data$burglary_5) & !is.na(crime_data$theft_5)  &!is.na(crime_data$drugs_6),]

table(crime_data$country)
table(crime_data_no_na_main_crimes$country)




#### 3) Descriptives Crime ####
important_countries <- c("DE","CH","SE","FR","IT","ES","AT","HR","ES","AL")
# line plots
ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=intentional_homicide_1, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))
ggsave(file=paste0(plots,"des_line_intentional_homicide_1_",".png"), width=6, height=4, dpi=300)

ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=assault_2, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_assault_2_",".png"), width=6, height=4, dpi=300)


ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=sexual_violence_3, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_sexual_violence_3_",".png"), width=6, height=4, dpi=300)

ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=robbery_4, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_sexual_robbery_4_",".png"), width=6, height=4, dpi=300)

ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=burglary_5, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_burglary_5_",".png"), width=6, height=4, dpi=300)

ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=theft_5, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_theft_5_",".png"), width=6, height=4, dpi=300)


ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
  geom_line(aes(x=year,y=drugs_6, group = as.factor(country),color = country))+
  theme_bw()+
  theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
  ggsave(file=paste0(plots,"des_line_drugs_6_",".png"), width=6, height=4, dpi=300)

# summary all countries
plot_all_countries <- crime_data_no_na_main_crimes %>%
  group_by(year) %>%
  summarize(intentional_homicide_1_sd = sd(intentional_homicide_1,na.rm=T), 
            assault_2_sd = sd(assault_2,na.rm=T), 
            sexual_violence_3_sd = sd(sexual_violence_3,na.rm=T), 
            robbery_4_sd = sd(robbery_4,na.rm=T), 
            burglary_5_sd = sd(burglary_5,na.rm=T), 
            theft_5_sd = sd(theft_5,na.rm=T), 
            drugs_6_sd = sd(drugs_6,na.rm=T),
            intentional_homicide_1 = mean(intentional_homicide_1,na.rm=T), 
            assault_2 = mean(assault_2,na.rm=T), 
            sexual_violence_3 = mean(sexual_violence_3,na.rm=T), 
            robbery_4 = mean(robbery_4,na.rm=T), 
            burglary_5 = mean(burglary_5,na.rm=T), 
            theft_5 = mean(theft_5,na.rm=T), 
            drugs_6 = mean(drugs_6,na.rm=T)) %>%
  mutate(intentional_homicide_1_ci_up = intentional_homicide_1 +intentional_homicide_1_sd*qnorm(1-significance_level/2),
         intentional_homicide_1_ci_low = intentional_homicide_1 -intentional_homicide_1_sd*qnorm(1-significance_level/2),
         assault_2_ci_up = assault_2 +assault_2_sd*qnorm(1-significance_level/2),
         assault_2_ci_low = assault_2 -assault_2_sd*qnorm(1-significance_level/2),
         sexual_violence_3_ci_up = sexual_violence_3 +sexual_violence_3_sd*qnorm(1-significance_level/2),
         sexual_violence_3_ci_low = sexual_violence_3 -sexual_violence_3_sd*qnorm(1-significance_level/2),
         robbery_4_ci_up = robbery_4 +robbery_4_sd*qnorm(1-significance_level/2),
         robbery_4_ci_low = robbery_4 -robbery_4_sd*qnorm(1-significance_level/2),
         burglary_5_ci_up = burglary_5 +burglary_5_sd*qnorm(1-significance_level/2),
         burglary_5_ci_low = burglary_5 -burglary_5_sd*qnorm(1-significance_level/2),
         theft_5_ci_up = theft_5 +theft_5_sd*qnorm(1-significance_level/2),
         theft_5_ci_low = theft_5 -theft_5_sd*qnorm(1-significance_level/2),
         drugs_6_ci_up = drugs_6 +drugs_6_sd*qnorm(1-significance_level/2),
         drugs_6_ci_low = drugs_6 -drugs_6_sd*qnorm(1-significance_level/2))

plot_all_countries <- ungroup(plot_all_countries, year)
            
for (i in colnames(crime_data_no_na_main_crimes)[3:ncol(crime_data_no_na_main_crimes)]) {
    print(
  ggplot(data=plot_all_countries, aes(x=year)) +
    geom_line(aes_(y=as.name(i), group = 1))+
    geom_line(aes_(y=as.name(paste0(i,"_ci_up")), group = 1), linetype = "dashed")+
    geom_line(aes_(y=as.name(paste0(i,"_ci_low")), group = 1),linetype = "dashed")+
    theme_bw()+
    theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black")) +
  ggsave(file=paste0(plots,"all_countries_line_",i,".png"), width=6, height=4, dpi=300)
  )
}

# PCA
  # singular value decomp
    pca_with_loadings = princomp(crime_data_no_na_main_crimes[,3:ncol(crime_data_no_na_main_crimes)], cor = TRUE, scores = TRUE)
    crime_data_no_na_main_crimes = cbind(crime_data_no_na_main_crimes, pca_with_loadings$scores)
    # change sign for first pc for easier interpretabiility
    crime_data_no_na_main_crimes$Comp.1 <- -crime_data_no_na_main_crimes$Comp.1
    
    print("factor loadings")
    print(pca_with_loadings$loadings)
    print("First PC has positive factor loadings (apart from homicide) -> maybe good crime index")
    
    summary(crime_data_no_na_main_crimes)
  
  # spectral decomposition
    #crime_data_no_na_main_crimes<- cbind(crime_data_no_na_main_crimes,pcatr(crime_data_no_na_main_crimes[,3:ncol(crime_data_no_na_main_crimes)]))
    #plot(crime_data_no_na_main_crimes$V1, crime_data_no_na_main_crimes$V2)
    
  # plot PC for countries
    ggplot(data=crime_data_no_na_main_crimes[crime_data_no_na_main_crimes$country %in% important_countries,]) +
      geom_line(aes(x=year,y=Comp.1, group = as.factor(country),color = country))+
      theme_bw()+
      theme(legend.position = "bottom", legend.box.background = element_rect(colour = "black"))+
      ggsave(file=paste0(plots,"princial_component_1_",i,".png"), width=6, height=4, dpi=300)
    
  # average pc 1 for country to check validity of index
    mean_pc1 <- group_by(crime_data_no_na_main_crimes, country) %>%
            summarize(mean_pc1 = mean(Comp.1)) %>%
      arrange(mean_pc1)
  
# Crime index ideas
  # first or first and second principal component
  
  # binary first principal component
  crime_data_no_na_main_crimes$Comp.1_bin <- as.numeric(crime_data_no_na_main_crimes$Comp.1>mean(crime_data_no_na_main_crimes$Comp.1))
  
  # binary first principal component per country
  crime_data_no_na_main_crimes<-group_by(crime_data_no_na_main_crimes, country) %>%
    mutate(Comp.1_bin_country = as.numeric(Comp.1>mean(Comp.1)))
  
# rename measures for interpretability
  colnames(crime_data_no_na_main_crimes)[ colnames(crime_data_no_na_main_crimes)=="Comp.1"] <- "pc_crime_1"
  colnames(crime_data_no_na_main_crimes)[ colnames(crime_data_no_na_main_crimes)=="Comp.2"] <- "pc_crime_2"
  colnames(crime_data_no_na_main_crimes)[ colnames(crime_data_no_na_main_crimes)=="Comp.1_bin"] <- "binary_crime"
  colnames(crime_data_no_na_main_crimes)[ colnames(crime_data_no_na_main_crimes)=="Comp.1_bin_country"] <- "binary_crime_country"
  
  crime_measures <- crime_data_no_na_main_crimes
#### OUTPUTS
# save data
  save(crime_measures, file = "crime_measures.R")
  
# latex tables
  # factor loadings
  pca_loadings.txt <- xtable(as.data.frame(unclass(pca_with_loadings$loadings)))
  print(pca_loadings.txt, file=paste0(tables,"pca_loadings.txt"))
  
  

      