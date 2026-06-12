# ==========================================
# SALMONSYS FULL DEMO DATA (WORKING VERSION)
# ==========================================

library(dplyr)
library(sf)
library(xts)
library(zoo)

set.seed(123)

# -------------------------------
# 1. BASIC FARM DATA
# -------------------------------

n_farms <- 50
locations <- paste0("Farm_", 1:n_farms)

dates <- seq(as.Date("2019-01-01"),
             as.Date("2020-12-01"),
             by = "month")

df_noExc <- expand.grid(
  location = locations,
  date = dates
) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(
    mnd.count = row_number(),
    mort = runif(n(), 0.5, 5),
    
    group = case_when(
      runif(n()) < 0.05 ~ "Weight upon stocking at sea > 500g (exclusion)",
      runif(n()) < 0.10 ~ "Innacurate counts (exclusion)",
      mort < 2 ~ "Baseline mortality < 2%",
      TRUE ~ "High mortality > 2%"
    ),
    
    start.mnd = first(date),
    bade.treat = sample(0:3, n(), replace = TRUE),
    mekanisk.treat = sample(0:3, n(), replace = TRUE),
    lus.count = runif(n(), 0, 5),
    pd.cohort = sample(c("Yes", "No"), n(), replace = TRUE),
    ila.cohort = sample(c("Yes", "No"), n(), replace = TRUE)
  ) %>%
  ungroup()

# -------------------------------
# 1b. ANALYSIS DATASET
# -------------------------------

dfanalysis <- df_noExc %>%
  mutate(
    year = format(date, "%Y"),
    month = format(date, "%m"),
    high_mort_flag = ifelse(mort >= 2, 1, 0)
  ) %>%
  group_by(location) %>%
  arrange(date) %>%
  mutate(
    mort_roll3 = zoo::rollmean(mort, 3, fill = NA, align = "right")
  ) %>%
  ungroup()

# -------------------------------
# 2. SPATIAL DATA
# -------------------------------

coords <- data.frame(
  lon = runif(n_farms, 5, 30),
  lat = runif(n_farms, 58, 71)
)


group_map <- df_noExc %>%
  filter(date == "2020-12-01") %>%
  distinct(location, group) %>%
  bind_cols(coords) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Simple "Zones" polygon
Zones <- data.frame(
  id = 1,
  lon = 15,
  lat = 65
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_buffer(dist = 10)


# -------------------------------
# 3. MAP DATASETS
# -------------------------------

consec_high_mort_map <- group_map %>%
  mutate(
    n_cat = sample(1:3, n(), replace = TRUE),
    mort = runif(n(), 1, 5),
    start.mnd = as.Date("2019-01-01"),
    mnd.count = sample(3:24, n(), replace = TRUE),
    bade.treat = sample(0:3, n(), replace = TRUE),
    mekanisk.treat = sample(0:3, n(), replace = TRUE),
    lus.count = runif(n(), 0, 5),
    pd.cohort = sample(c("Yes", "No"), n(), replace = TRUE),
    ila.cohort = sample(c("Yes", "No"), n(), replace = TRUE)
  )

consec_innac_map <- consec_high_mort_map

bas_mort_map <- df_noExc %>%
  filter(date == "2020-12-01") %>%
  select(
    location, group, mort, start.mnd, mnd.count,
    bade.treat, mekanisk.treat, lus.count,
    pd.cohort, ila.cohort
  ) %>%
  bind_cols(st_coordinates(group_map)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326)


# -------------------------------
# 4. ALERT DATA
# -------------------------------

consec_high_mort <- df_noExc %>%
  mutate(n_cat = sample(0:3, n(), replace = TRUE))

consec_innac <- consec_high_mort

# -------------------------------
# 5. TIME SERIES (BASELINE)
# -------------------------------

ts_dates <- seq(as.Date("2018-01-01"),
                as.Date("2020-12-01"),
                by = "month")

n <- length(ts_dates)

trend <- seq(1.5, 3.5, length.out = n)
season <- sin(seq(0, 3*pi, length.out = n)) * 0.5
noise <- rnorm(n, 0, 0.3)

observed <- trend + season + noise

# Add shocks
shock_idx <- sample(10:(n-5), 3)
for (i in shock_idx) {
  observed[i:(i+2)] <- observed[i:(i+2)] + c(1.5, 2, 1.2)
}


med_iqr_mort_no <- xts(
  cbind(
    V1 = observed - 0.5,   # lower IQR
    V2 = observed,         # median
    V3 = observed + 0.5    # upper IQR
  ),
  order.by = ts_dates
)


med_mort_no_z1 <- xts(
  cbind(
    V1 = observed,                            
    V2 = observed + rnorm(n, 0, 0.3)          
  ),
  order.by = ts_dates
)

# -------------------------------
# 6. MODEL DATA
# -------------------------------


ts_hs <- as.numeric(stats::filter(observed, rep(1/3, 3), sides = 1))
ts_hs[is.na(ts_hs)] <- observed[is.na(ts_hs)]


p_fit <- trend + season
p_lwr <- p_fit - 0.6
p_upr <- p_fit + 0.6

ts_no <- xts(
  cbind(
    ts_no = observed,
    ts_hs = ts_hs,
    p_hw.lwr = p_lwr,
    p_hw.fit = p_fit,
    p_hw.upr = p_upr
  ),
  order.by = ts_dates
)

# -------------------------------
# 7. FARM MODEL OUTPUTS
# -------------------------------

farm_obs <- observed + rnorm(n, 0, 0.2)


farm_exp <- as.numeric(stats::filter(farm_obs, rep(1/5, 5), sides = 1))
farm_exp[is.na(farm_exp)] <- farm_obs[is.na(farm_exp)]


upper <- farm_exp + runif(n, 0.4, 0.8)

farm_x_m1b <- xts(
  cbind(
    V1 = upper,
    V2 = farm_obs,
    V3 = farm_exp
  ),
  order.by = ts_dates
)

# Variation for model 2
farm_exp2 <- farm_exp + rnorm(n, 0, 0.1)
upper2 <- farm_exp2 + runif(n, 0.5, 1)

farm_x_m2 <- xts(
  cbind(
    V1 = upper2,
    V2 = farm_obs,
    V3 = farm_exp2
  ),
  order.by = ts_dates
)