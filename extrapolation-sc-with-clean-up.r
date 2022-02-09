library(broom)
library(tidyverse)
library(ggfortify)
library(lubridate)

theme_set(theme_light())

load("Rdata/us_casesdeaths.Rdata")

find_value <- function(x,y,target=c(0, 2,5,10)) {
    aa <- approx(y,x,xout=target)$y
    as.Date(aa,origin="1970-01-01")  ## convert back to a date (ugh)
}

# 
#  extrapolocation for SC data
# 

FILTERDATE = today() - days(100)

cdbl <-
    us_casesdeaths %>% 
        filter(state=="South Carolina", date>FILTERDATE) %>%
        group_by(date) %>%
        summarize(population = sum(population),
                                cases = sum(cases),
                                deaths = sum(deaths),
                                casesper100k = cases / population * 1e5,
                                deathsper100k = deaths / population * 1e5,
                                .groups = "drop")


cdbl_cleanup <-
    cdbl %>%
        mutate(dow = weekdays(date)) %>%
        filter(!dow %in% c("Saturday", "Sunday")) %>%
        mutate(ccasesper100k = ifelse(dow == "Monday", casesper100k / 3, casesper100k)) %>%
        mutate(cases14 = zoo::rollmeanr(ccasesper100k, 14, na.pad = TRUE))        

# 
#  extrapolocation for GSP data
# 

us_casesdeaths %>% filter(state=="South Carolina", county=="Greenville" | county=="Spartanburg", date>FILTERDATE) %>%
                        group_by(date) %>%
                        summarize(population = sum(population),
                                  cases = sum(cases),
                                  deaths = sum(deaths),
                                  casesper100k = cases / population * 1e5,
                                  deathsper100k = deaths / population * 1e5
                                  ) %>% ungroup() -> cdbl_gsp

cdbl_gsp_cleanup <-
    cdbl_gsp %>%
        mutate(dow = weekdays(date)) %>%
        filter(!dow %in% c("Saturday", "Sunday")) %>%
        mutate(ccasesper100k = ifelse(dow == "Monday", casesper100k / 3, casesper100k)) %>%
        mutate(cases14 = zoo::rollmeanr(ccasesper100k, 14, na.pad = TRUE))


#
# 14 day trends in GSP
#

twoweeksago <- today() - weeks(3)
twoweeksfromnow <- today() + weeks(4)

next2weeks <- tibble(date = seq.Date(from = twoweeksago,
                                    to = twoweeksfromnow,
                                    by = "1 day"))

predict_gsp <- cdbl_gsp_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(ccasesper100k ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

predict_gsp_milliken <- cdbl_gsp_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(cases14 ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

date10 = find_value(predict_gsp_milliken$date, predict_gsp_milliken$.upper, target=10)

maskmandate = format(date10, format= "%B %d")

gspplot <-
ggplot(data = cdbl_gsp_cleanup) +
    aes(x = date, y = ccasesper100k) +
    geom_point() +
    geom_line(aes(y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "right")), color = "blue", lty = 2) +
    geom_line(aes(y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "center")), color = "darkgreen", lty = 2) +        
    expand_limits(x = max(cdbl_gsp_cleanup$date + 20)) +
    scale_y_continuous(limit = c(0, NA), breaks = c(0,2,5,10,20,50,100 * 1:20)) +
    scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
    geom_line(data = predict_gsp, aes(y = .fitted), color = "darkgreen", lty = 1) + 
    geom_ribbon(data = predict_gsp, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "darkgreen", alpha = 0.4) +
    geom_line(data = predict_gsp_milliken, aes(y = .fitted), color = "blue") + 
    geom_ribbon(data = predict_gsp_milliken, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "blue", alpha = 0.4) +
    labs(x = "Date",
        y = "Cases per 100k population",
        title = "Cases in GSP Area (South Carolina)",
        subtitle = paste("Mask mandate expected to change on", maskmandate),
        caption = "Blue line: rolling mean (14 days) prediction\nGreen line: prediction based on 14 previous days") + 
    geom_vline(xintercept =  date10, color = "red", lty = 2) + 
    geom_vline(xintercept =  today(), color = "gray50", lty = 2) + 
    geom_hline(yintercept = 10, color = "black", lty = 2)

ggsave("projections/covid19-SCGSP-linearpredict14days.pdf", width=11, height=8, plot = gspplot)
#
# 14 day trends in SC
#

predict_sc <- cdbl_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(ccasesper100k ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

predict_sc_milliken <- cdbl_cleanup %>%
        filter(date >= twoweeksago) %>%
        lm(cases14 ~ date, data=.) %>%
        augment(newdata = next2weeks, interval = "confidence")

date10 = find_value(predict_sc_milliken$date, predict_sc_milliken$.upper, target=10)

maskmandate = format(date10, format= "%B %d")

scplot <-
ggplot(data = cdbl_cleanup) +
    aes(x = date, y = ccasesper100k) +
    geom_point() +
    geom_line(aes(y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "right")), color = "blue", lty = 2) +
    geom_line(aes(y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "center")), color = "darkgreen", lty = 2) +        
    expand_limits(x = max(cdbl_gsp_cleanup$date + 20)) +
    scale_y_continuous(limit = c(0, NA), breaks = c(0,2,5,10,20,50,100 * 1:20)) +
    scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
    geom_line(data = predict_sc, aes(y = .fitted), color = "darkgreen", lty = 1) + 
    geom_ribbon(data = predict_sc, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "darkgreen", alpha = 0.4) +
    geom_line(data = predict_sc_milliken, aes(y = .fitted), color = "blue") + 
    geom_ribbon(data = predict_sc_milliken, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "blue", alpha = 0.4) +
    labs(x = "Date",
        y = "Cases per 100k population",
        title = "Cases in SC",
        subtitle = paste("Mask mandate expected to change on", maskmandate),
        caption = "Blue line: rolling mean (14 days) prediction\nGreen line: prediction based on 14 previous days") + 
    geom_vline(xintercept =  date10, color = "red", lty = 2) + 
    geom_vline(xintercept =  today(), color = "gray50", lty = 2) + 
    geom_hline(yintercept = 10, color = "black", lty = 2)

ggsave("projections/covid19-SC-linearpredict14days.pdf", width=11, height=8, plot = scplot)

library(patchwork)

p = scplot + gspplot
ggsave("projections/covid19-SCGSP-linearpredict14days.png", width=12, height=6, plot = p)