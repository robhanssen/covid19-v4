library(tidyverse)
library(lubridate)
library(zoo)

load("Rdata/global_casesdeaths.Rdata") 

locations = read_csv("sources/countryinformation.csv")

# summarize country info
global_casesdeaths %>% rename(province="Province/State", country="Country/Region") %>% 
                       group_by(country, date, time) %>% 
                       summarize(deaths=sum(deaths), cases=sum(cases)) %>%
                       full_join(locations, by=c(country="region")) -> casesdeaths
correction = 50
avdays=7
totalcasecomment=""
capt="insert caption here"

# pick one
selected_country = "Netherlands"
selected_country = "United Kingdom"
selected_country = "Sweden"
selected_country = "Namibia"
selected_country = "Israel"
selected_country = "US"


        casesdeaths %>% filter(country==selected_country) %>% filter(date>as.Date("2020-03-01")) %>%
                                group_by(date) %>%
                                summarize(population = sum(population),
                                          cases = sum(cases),
                                          deaths = sum(deaths),
                                          casesper100k = cases / population * 1e5,
                                          deathsper100k = deaths / population * 1e5
                                          ) %>% ungroup() -> casesdeathsbylocation

        ylimit_max = (max(casesdeathsbylocation$casesper100k) %/% 10 + 1) * 10

        ylimit_max = ifelse(ylimit_max > 150, ylimit_max, 150)

        casesdeathsbylocation %>% filter(date > today() %m-% months(18)) %>%
                        ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + 
                                geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), size=2, color="blue") + 
                                scale_y_continuous(limit=c(0,ylimit_max), breaks=c(0,2,5,10,20,50,100,150,200,300), sec.axis = sec_axis(~ ./correction, breaks=seq(0,5,1))) + 
                                scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                                labs(caption=capt, x="Date", y="Daily incremental number of confirmed cases or deaths") + 
                                ggtitle(paste(selected_country, "daily cases and deaths with", avdays,"days average line")) + 
                                geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted")  + 
                                geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), size=2, color="red") 