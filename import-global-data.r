#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
source("config.r")

#
# import global infections via web API
#

infocolumnnames = c("Province/State","Country/Region", "Lat","Long")
infocols = length(infocolumnnames)

global_cases_raw <- read_csv(global_infections_file)

global_cases <- global_cases_raw %>% 
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) 

global_cases_growth <- global_cases_raw %>% 
                     dailydifference(., infocols) %>% 
                     tidydata(., dataname="cases", exclusionvector=infocolumnnames) #%>% 
                     #mutate(time=time+1)

#
# import global deaths via web API
#

global_deaths_raw <- read_csv(global_deaths_file)

global_deaths <- global_deaths_raw %>% 
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) 


global_deaths_growth <- global_deaths_raw %>% 
                     dailydifference(., infocols) %>%
                     tidydata(., dataname="deaths", exclusionvector=infocolumnnames) #%>% 
                     #mutate(time=time+1)

# combining all global data
global_casesdeaths <- global_cases_growth %>% 
                     inner_join(global_deaths_growth)

lastreadglobal = today()
save(global_casesdeaths, lastreadglobal, file="Rdata/global_casesdeaths.Rdata")
