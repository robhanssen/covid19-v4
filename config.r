#
#
#
#
#
source = "\U00A9 2021, Rob Hanssen\nData source: Johns Hopkins University"


# data files
us_cases_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
us_deaths_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
global_infections_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
global_deaths_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"


dailydifference <- function(df, infocols)
{
    totalcols <- ncol(df)
    dfdiff <- cbind(df[1:infocols], df[(infocols+2):totalcols] - df[(infocols+1):(totalcols-1)]) 
    return(dfdiff)
}


tidydata <- function(df, dataname, exclusionvector)
{
   tidydf <- df %>% pivot_longer(!all_of(exclusionvector), 
                                    names_to = "date",
                                    values_to = dataname)
    
    tidydf$date = as.Date(tidydf$date, format="%m/%d/%y")
    tidydf$time = tidydf$date - min(tidydf$date) + 1

    return(tidydf)
}


# colnames(covid_raw) = c("province","region","lat","long","date","infections")
# covid_raw$date = as.Date(covid_raw$date, format="%m/%d/%y")
# lastupdated = max(covid_raw$date)
# covid_raw$time = covid_raw$date - min(covid_raw$date) + 1



