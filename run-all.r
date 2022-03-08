library(lubridate)

if(file.exists("Rdata/us_casesdeaths.Rdata")) load("Rdata/us_casesdeaths.Rdata") else lastreadus <- 0 
if (lastreadus != today()) {
        print("READING NEW DATA FOR US")
        source("import-us-data.r")
        }

source("process_us_data.r")
source("extrapolation-sc-with-clean-up.r")
source("USstates_models.r")

if(file.exists("Rdata/global_casesdeaths.Rdata")) load("Rdata/global_casesdeaths.Rdata") else lastreadglobal <- 0
if (lastreadglobal != today()) {
        print("READING NEW DATA FOR GLOBAL")
        source("import-global-data.r")
        }

source("process_global_data.r")
source("worldmodels.r")

# misc. script runs
#source("hospitalizations/hosp-run-all.r")
source("misc/covid_and_2020elections.r")
#source("misc/comparison_of_countries_and_states.r")
#source("UScounty_models.r")
