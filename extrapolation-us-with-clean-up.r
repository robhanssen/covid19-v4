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

FILTERDATE = as.Date("2021-09-01")

cdbl <-
    us_casesdeaths %>% 
        filter(date > FILTERDATE) %>%
        inner_join(read_csv("sources/USstateslist.csv")) %>%
        group_by(date, location) %>%
        summarize(population = sum(population),
                                cases = sum(cases),
                                deaths = sum(deaths),
                                casesper100k = cases / population * 1e5,
                                deathsper100k = deaths / population * 1e5,
                                .groups = "drop")


cdbl_cleanup <-
    cdbl %>%
        mutate(dow = weekdays(date)) %>%
        # filter(!dow %in% c("Saturday", "Sunday")) %>%
        mutate(ccasesper100k = casesper100k) %>%
        mutate(cases14 = zoo::rollmeanr(ccasesper100k, 28, na.pad = TRUE))        


twoweeksago <- today() - weeks(3)
twoweeksfromnow <- today() + weeks(4)

next2weeks <- tibble(date = seq.Date(from = twoweeksago,
                                    to = twoweeksfromnow,
                                    by = "1 day"))

#
# 14 day trends in SC
#

casemodel <- function(tbl) {
    lm(ccasesper100k ~ date, data = tbl)
}

augmentcasemodel <- function(mdl) {
    augment(mdl, newdata = next2weeks, interval = "confidence")
}

case14model <- function(tbl) {
    lm(cases14 ~ date, data = tbl)
}

augmentcasemodel <- function(mdl) {
    augment(mdl, newdata = next2weeks, interval = "confidence")
}



predict_sc <- cdbl_cleanup %>%
        filter(date >= twoweeksago) %>%
        group_by(location) %>%
        nest() %>%
        mutate(casesmodel = map(data, casemodel),
               cases14model = map(data, case14model)
              ) %>%
        # lm(ccasesper100k ~ date, data=.) %>%
        mutate(augdata = map(casesmodel, augmentcasemodel),
               aug14data = map(cases14model, augmentcasemodel)) %>%
        unnest(augdata)
        

predict_sc_milliken <- cdbl_cleanup %>%
        filter(date >= twoweeksago) %>%
        group_by(location) %>%
        nest() %>%
        mutate(casesmodel = map(data, casemodel),
               cases14model = map(data, case14model)
              ) %>%
        # lm(ccasesper100k ~ date, data=.) %>%
        mutate(augdata = map(casesmodel, augmentcasemodel),
               aug14data = map(cases14model, augmentcasemodel)) %>%
        unnest(aug14data)


predict_sc_milliken %>% 
    select(location, data, cases14model) %>%
    mutate(modeldata = map(cases14model, tidy)) %>%
    unnest(modeldata) %>%
    filter(term == "date") %>%
    distinct(location, estimate)


# date10 = find_value(predict_sc_milliken$date, predict_sc_milliken$.upper, target=10)

maskmandate = NA # format(date10, format= "%B %d")

# scplot <-
ggplot(data = cdbl_cleanup) +
    aes(x = date, y = cases14, color = location) +
    facet_wrap(~location) + 
    # geom_point() +
    # geom_line(aes(y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "right")), color = "blue", lty = 2) +
    geom_line(aes(y = cases14), color = "blue", lty = 2) +
    #geom_point(aes(color = location, y = zoo::rollmean(ccasesper100k, 14, na.pad = TRUE, align = "center")), color = "darkgreen", lty = 2) +        
    expand_limits(x = max(cdbl_cleanup$date + 20)) +
    scale_y_continuous(limit = c(0, NA), breaks = seq(0, 200, 10)) +
    scale_x_date(breaks = "2 weeks", date_labels = "%b %d") +
    #geom_line(data = predict_sc, aes(y = .fitted), lty = 1) + 
     #geom_ribbon(data = predict_sc, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "darkgreen", alpha = 0.4) +
     geom_line(data = predict_sc_milliken, aes(y = .fitted)) + 
    # geom_ribbon(data = predict_sc_milliken, aes(y = .fitted, ymin = .lower, ymax = .upper), fill = "blue", alpha = 0.4) +
    labs(x = "Date",
        y = "Cases per 100k population",
        title = "Cases in the US",
        # subtitle = paste("Mask mandate expected to change on", maskmandate),
        caption = "Blue line: rolling mean (14 days) prediction\nGreen line: prediction based on 14 previous days") + 
    # geom_vline(xintercept =  date10, color = "red", lty = 2) + 
    geom_vline(xintercept =  today(), color = "gray50", lty = 2) + 
    geom_hline(yintercept = 10, color = "black", lty = 2)

# ggsave("projections/covid19-SC-linearpredict14days.pdf", width=11, height=8, plot = scplot)

