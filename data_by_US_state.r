# 
# 
# 
# 
# 
library(tidyverse)
library(lubridate)
library(zoo)
source("config.r")

load("Rdata/us_casesdeaths.Rdata")

# assign the region in the US to all locations
locations = read_csv("sources/USstateslist.csv")
us_casesdeaths <- us_casesdeaths %>% 
                        full_join(locations) %>%
                        mutate(location = ifelse(is.na(location), "Other", location))

correction = 60
avdays = 7
capt=""

selected_state = "South Carolina"
selected_state = "Florida"
selected_state = "Louisiana"
selected_state = "Alabama"
selected_state = "Mississippi"

us_casesdeaths %>% filter(state==selected_state) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation


casesdeathsbylocation %>% filter(date > today() %m-% months(18)) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(limit=c(0,400), breaks=c(0,2,5,10,20, 50,100*1:10), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="1 week", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste(selected_state,"daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 




selected_state = "Florida"
us_casesdeaths %>% filter(state==selected_state, county=="Alachua") %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> casesdeathsbylocation


casesdeathsbylocation %>% filter(date > today() %m-% months(3)) %>%
                ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                        geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(limit=c(0,20), breaks=c(0,2,5,10,20, 50,100), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                        scale_x_date(date_breaks="1 week", date_labels = "%b %d") + 
                        labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                        ggtitle(paste(selected_state,"daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                        geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 
