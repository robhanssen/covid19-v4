library(tidyverse)
library(lubridate)

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

vaccinations <- read_csv("https://data.cdc.gov/resource/8xkx-amqh.csv?&$limit=4000") %>%
        filter(date == max(date))

electionresults = read_csv("sources/2020electionresults.csv") %>%
        rename(fips = "county_fips") %>%
        select(fips, per_gop) %>%
        mutate(per_gop_discrete = factor(10*floor(per_gop*10))) %>%
        mutate(fips = ifelse(fips < 10000, paste0("0",as.character(fips)), as.character(fips)))

statenames = tibble(stateabb = state.abb, statename = state.name)
statenames = bind_rows(statenames, tibble(stateabb = "DC", statename = "District of Columbia"))

vax <-
    vaccinations %>%
    select(date, fips, recip_county, recip_state, administered_dose1_pop_pct) %>%
    mutate(recip_county = str_sub(recip_county, end = -8)) %>%
    inner_join(statenames, by = c("recip_state" = "stateabb")) %>%
    mutate(countyid = paste0(recip_county, ", ", statename)) %>%
    inner_join(electionresults, by = "fips") %>%
    rename(dose1 = "administered_dose1_pop_pct") %>%
    filter(dose1 != 0)

#date = pivot_wider(vax, c(date)) %>% mutate(date = format(date, format = "%b %d, %Y")) %>% pull(date)
date <- distinct(vax, date) %>% mutate(date = format(date, format = "%b %d, %Y")) %>% pull(date)

spavax <-
    vax %>%
        filter(recip_state == "SC",
               recip_county %in% c("Spartanburg", "Greenville", "Anderson", "Cherokee", "York")
               ) %>%
        mutate(label = recip_county)


scatterplot <-
    vax %>%
    mutate(label = "") %>%
    ggplot() +
    aes(per_gop, dose1 / 100, label = label) +
    geom_point(alpha = .7, aes(color = per_gop_discrete)) +
    scale_color_manual(values = colorscale) +
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    geom_smooth(method = "loess", fullrange = TRUE, se = FALSE, color = "black", lty = 2) +
    labs (x = "Percentage votes for Trump",
          y = "First dose of vaccines received",
        #   title = paste("Vaccination status on ", date)
          ) +
    theme_light() +
    theme(legend.position = "none") + 
    geom_point(data = spavax, size = 3, color = "darkgreen") + 
    ggrepel::geom_label_repel(data = spavax)

barplot <-
    vax %>%
    group_by(per_gop_discrete) %>%
    summarise(vax_av = mean(dose1)/100, sd = sd(dose1)/100, n = n(), err = sd/sqrt(n)*qt(0.05/2, df = n, lower.tail = F), ymi = vax_av - err, yma = vax_av + err) %>%
    ggplot() +
    aes(per_gop_discrete, vax_av) + 
    geom_col(aes(fill = per_gop_discrete)) +
    geom_errorbar(aes(ymin = ymi, ymax = yma), width = .2)  +
    scale_y_continuous(labels = scales::percent_format(), breaks = .2 * 0:5, limits = c(NA, 1)) +
    labs(x = "Votes for Trump in 2020 (nearest 10%)",
         y = "Average vaccination rate (first dose)",
         title = paste("Vaccination status on ", date),
         caption = "Error bar indicate 95% confidence interval") +
    scale_fill_manual(values = colorscale) +
    theme_light() +
    theme(legend.position = "none")

library(patchwork)

p <- barplot + scatterplot

ggsave("misc/vaccination-state-by-vote.png", plot = p, width = 12, height = 6)


# and now for something completely different

load("Rdata/us_casesdeaths.Rdata")

electionresults_pop <- 
    read_csv("sources/2020electionresults.csv") %>%
    rename(fips = "county_fips") %>%
    mutate(countyid = paste(county, state, sep = ", ")) %>%
    # select(fips, per_gop) %>%
    mutate(per_gop_discrete = factor(10*floor(per_gop*10))) %>%
    mutate(fips = ifelse(fips < 10000, paste0("0",as.character(fips)), as.character(fips)))


countypop <-
    us_casesdeaths %>%
    # pivot_wider(c(county, state, population)) %>%
    distinct(county, state, population) %>%
    mutate(countyid = paste(county, state, sep = ", ")) %>%
    left_join(electionresults_pop, by = c("countyid"))

countypop %>%
    filter(!is.na(per_gop_discrete)) %>%
    ggplot + 
    aes(x = per_gop_discrete, y = population) + 
    geom_violin(aes(fill = per_gop_discrete)) + 
    scale_y_log10(labels = scales::comma_format(), breaks = 10^(3:7)) +
    labs(x = "Votes for Trump in 2020 (nearest 10%)",
         y = "County population",
         #title = paste("Vaccination status on ", date),
         #caption = "Error bar indicate 95% confidence interval"
         ) +
    scale_fill_manual(values = colorscale) +
    theme_light() +
    theme(legend.position = "none")


countypop %>%
    group_by(per_gop_discrete) %>%
    summarize(population = sum(population)) %>%
    filter(!is.na(per_gop_discrete)) %>%
    ggplot + 
    aes(x = per_gop_discrete, y = population) + 
    geom_col(aes(fill = per_gop_discrete)) + 
    # scale_y_log10(labels = scales::comma_format(), breaks = 10^(3:7)) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "Votes for Trump in 2020 (nearest 10%)",
         y = "County population",
         #title = paste("Vaccination status on ", date),
         #caption = "Error bar indicate 95% confidence interval"
         ) +
    scale_fill_manual(values = colorscale) +
    theme_light() +
    theme(legend.position = "none")


countypop %>%
    filter(!is.na(per_gop_discrete)) %>%
    ggplot + 
    aes(x = per_gop_discrete) + 
    geom_bar(aes(fill = per_gop_discrete)) + 
#    scale_y_log10(labels = scales::comma_format(), breaks = 10^(1:7)) +
    labs(x = "Votes for Trump in 2020 (nearest 10%)",
         y = "# of counties",
         #title = paste("Vaccination status on ", date),
         #caption = "Error bar indicate 95% confidence interval"
         ) +
    scale_fill_manual(values = colorscale) +
    theme_light() +
    theme(legend.position = "none")
##

