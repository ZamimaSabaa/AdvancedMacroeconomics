


directory <- setwd("/Users/zamimaislam/Documents/SFU courses/Spring 2022/Econ807 - Macroeconomics/termpaper ideas/Data cleaning and working")
install.packages("wbstats")
library(wbstats)
library(WDI)
library(tidyverse)
library(dplyr)

my_indicators = c("FDI" = "BX.KLT.DINV.CD.WD",                 #Net FDI Inflows (Measured is USD)
                  "GDP_deflator" = "NY.GDP.DEFL.ZS",           #GDP Deflator 
                  "Voice_and_Accountability" = "VA.EST",        #Voice and accountability (Measure of Institutional Quality)
                  "Political_stability" = "PV.EST",           #political stability (Measure of Institutional Quality)
                  "government_effectiveness"= "GE.EST",        #government effectiveness (Measure of Institutional Quality) 
                  "Regulatory_Quality" = "RQ.EST",           #regulatory quality (Measure of Institutional Quality)
                  "Rule_of_Law" = "RL.EST",                  #Rule of law (Measure of Institutional Quality)
                  "Control_of_Corruption" = "CC.EST",      #Control of Corruption (Measure of Institutional Quality) 
                  "GDP Per Capita" = "NY.GDP.PCAP.KD",    #GDP per capita, PPP (constant 2017) 
                  "Trade_open" = "NE.TRD.GNFS.ZS", #Trade openness: Total imports and exports (% of GDP) 
                  "UP" = "SP.URB.TOTL.IN.ZS",            #Urban population (% of the total population)
                  "Infrastructure" = "EG.USE.ELEC.KH.PC", #per capita electricity use (in kilowatt hours), as proxy for infrastructure 
                  "Total Population" = "SP.POP.TOTL",  #population of a country in a given year
                  "intpenetra" = "IT.NET.USER.ZS") #Individuals using internet (% of total population)

#Making my data_frame
my_data_unclean <- wb_data(my_indicators, start_date = 1996, end_date = 2016) #view data                 


#the log of net real FDI inflow, 
lagged value of FDI inflow, 
INSit stands for institutional quality that includes six indicators, 
#INFit stand for real inflation rate that is measured in percentage and calculated by using CPI used as a proxy for use inflation as a proxy of macroeconomic instability and economic tension, as there is a negative relationship between inflation and FDI , 
#use log of total population, 
#uses the inverse of the log of per capita real GDP as a proxy for rate of return on investment, 
#the infrastructure uses natural log of per capita electricity use (in kilowatt hours) as a proxy for the availability of infrastructure, 
#we The natural log of per capita real GDP (adjusted for purchasing power parity) is used as a proxy for the market size
#regional difference between South Asia and East Asia & Pacific 
#include trade openness variable


#(a)calculat inflation rate 

my_data_unclean <- my_data_unclean %>% 
  group_by(country) %>% 
  mutate(inflation = (GDP_deflator/lag(GDP_deflator)-1)*100) ###################

#(b) The log of total population 

my_data_unclean <- my_data_unclean %>% 
  mutate(log_pop = log(`Total Population`)) #################

#(c) Natural log of the inverse of Real GDP per capita  as a proxy for rate of return on 

  #calculat real GDP per capita 

my_data_unclean <- my_data_unclean %>% 
  group_by(country) %>%
  mutate(real_GDP_pc= (`GDP Per Capita` / (GDP_deflator/ 100))) 

  #calculate the log of real GDP using the log real GDP per capita

my_data_unclean <- my_data_unclean %>%
  mutate(log_real_gdp = log(real_GDP_pc))  ###############

#(d) infrastructure proxy 

my_data_unclean <- my_data_unclean %>% 
  rename(poweruse= 'Infrastructure') %>% 
  mutate(infrastructure = log(poweruse)) ################

#(e) rename per capita real GDP to be the market 

my_data_unclean <- my_data_unclean %>%
  rename(market= 'log_real_gdp') ####################

#(f) Use Urban Population and Internet Penetration 

??????????????

#(g) Creat the required FDI variables 

my_data_unclean <- my_data_unclean %>% 
  mutate(FDI_ch= FDI - min(na.omit(FDI)-1))

my_data_unclean <- my_data_unclean %>% 
  group_by(country) %>% 
  mutate(lagFDI = lag(FDI_ch))

my_data_unclean <- my_data_unclean %>% 
  mutate(log_FDI= log(FDI_ch),
         log_lag_FDI= log(lagFDI))

my_data_unclean <- my_data_unclean %>% 
  group_by(country) %>% 
  mutate(RFDI_change = log_FDI - log_lag_FDI)




my_data_unclean_Asia <- my_data_unclean %>% filter(country %in% c("China", "Fiji", "Indonesia", "Japan", "Malaysia", "Papua New Guinea", "Philippines", "Singapore", "South Korea", "Thailand", "Tonga, Vietnam","Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Sri Lanka")) 




#Dataset for regression analysis in STATA

write.csv(my_data_unclean_Asia, "STATA_Asia.csv") 






###To get the income level indicators 
Data <- c("GDP Per Capita" = "NY.GDP.PCAP.KD") #GDP per capita, PPP (constant 2017)
unclean <- WDI(country = "all", indicator = Data, extra = T, start=1996, end=2016)
no_aggr <- unclean %>% filter(region != 'Aggregates')
no_aggr_ne <- no_aggr %>% select(iso2c,income,year)
no_aggr_ne <- no_aggr_ne %>% rename(date='year')

#left join with original dataframe to add income levels

####THE FINAL TABLE OF VARIABLES############################################
df_joint <-
  my_data_unclean %>% 
  left_join(no_aggr_ne, by = 'iso2c')

merged_df <- merge(my_data_unclean, no_aggr_ne, by = c("iso2c", "date"), all.x = FALSE)
#changing the income to factors
class(df_joint$income) 
df_joint$income <- factor(df_joint$income)


#separating into rich and poor countries 
rich <- df_joint %>% filter(income %in% c("High income","Upper middle income"))
poor <-df_joint %>% filter(income %in% c("Low income","Lower middle income"))

stargazer(summary = T) 