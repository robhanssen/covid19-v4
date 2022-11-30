info <- us_cases_raw %>% select(1:11)
info2 <- us_deaths_raw %>% select(1:12)

t0 <- Sys.time()

us_cases3 <-
    us_cases_raw %>%
    select(-c(1:4, 6:11)) %>%
    pivot_longer(!FIPS) %>%
    group_by(FIPS) %>%
    mutate(date = mdy(name),
            cases = value - lag(value)) %>%
    ungroup()

us_deaths <-
    us_deaths_raw %>%
    select(-c(1:4, 6:12)) %>%
    pivot_longer(!FIPS) %>%
    group_by(FIPS) %>%
    mutate(date = mdy(name),
            deaths = value - lag(value)) %>%
    ungroup()


Sys.time() - t0

ncol <- ncol(us_cases2)

t0 <- Sys.time()
us_cases2 <- us_cases_raw[-c(1:4, 6:11)]
ncol <- ncol(us_cases2)
us_cases3 <- bind_cols(us_cases2[1], us_cases2[3:(ncol)] - us_cases2[2:(ncol-1)]) %>% pivot_longer(!FIPS) %>% mutate(date = mdy(name))
Sys.time() - t0
# Time difference of 1.767363 secs

t0 <- Sys.time()
us_cases2 <- us_cases_raw[-c(1:4, 6:11)]
ncol <- ncol(us_cases2)
us_cases3 <- cbind(us_cases2[1], us_cases2[3:(ncol)] - us_cases2[2:(ncol-1)]) %>% pivot_longer(!FIPS) %>% mutate(date = mdy(name))
Sys.time() - t0
# Time difference of 1.550864 secs

t0 <- Sys.time()
us_cases3 <-
    us_cases_raw %>%
    select(-c(1:4, 6:11)) %>%
    pivot_longer(!FIPS) %>%
    # group_by(FIPS) %>%
    mutate(date = mdy(name),
            cases = value - lag(value))  %>%
    ungroup()
Sys.time() - t0
# Time difference of 24.52673 secs
# Without group_by(): Time difference of 1.519198 secs

library(furrr)
plan(multisession)

t0 <- Sys.time()
us_cases4 <-
    us_cases_raw %>%
    select(-c(1:4, 6:11)) %>%
    pivot_longer(!FIPS)

    us_cases3 <- future_map_dfr(unique(us_cases4$FIPS), ~ subset(us_cases4, FIPS == .x) %>% mutate(cases = value - lag(value))) %>%
    mutate(date = mdy(name))
Sys.time() - t0
# Time difference of 1.580935 mins


t0 <- Sys.time()
us_cases4 <-
    us_cases_raw %>%
    select(-c(1:4, 6:11)) %>%
    pivot_longer(!FIPS)

    us_cases3 <- map_dfr(unique(us_cases4$FIPS), ~ subset(us_cases4, FIPS == .x) %>% mutate(cases = value - lag(value))) %>%
    mutate(date = mdy(name))
Sys.time() - t0
# Time difference of 3.801137 mins