library(fredr)
library(readr)
library(ustfd)
library(lubridate)
library(tidyverse)
library(zoo)
library(rdbnomics)


setwd("~/../Documents/Evaluating_Trump_Tariffs/trump_tariff_tracker/")

# Goal One Data

# Manufacturing Employment Data

MANEMP <- fredr(
  series_id = "MANEMP",
  observation_start = as.Date("2010-01-01")
) 
MANEMP$date <- ymd(MANEMP$date)

MANEMP <- MANEMP %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(MANEMP, "./Data/MANEMP.csv")

# New Manufacturing Orders Data

AMTMNO <- fredr(
  series_id = "AMTMNO",
  observation_start = as.Date("2010-01-01")
) 
AMTMNO$date <- ymd(AMTMNO$date)

AMTMNO <- AMTMNO %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(AMTMNO, "Data/AMTMNO.csv")

# Value of Shipments Data

AMTMVS <- fredr(
  series_id = "AMTMVS",
  observation_start = as.Date("2010-01-01")
) 
AMTMVS$date <- ymd(AMTMVS$date)

AMTMVS <- AMTMVS %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(AMTMVS, "Data/AMTMVS.csv")

# Industrial Production Data

IPMAN <- fredr(
  series_id = "IPMAN",
  observation_start = as.Date("2010-01-01")
) 
IPMAN$date <- ymd(IPMAN$date)

IPMAN <- IPMAN %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(IPMAN, "Data/IPMAN.csv")

# Industrial Capacity Data

CAPGMFS <- fredr(
  series_id = "CAPGMFS",
  observation_start = as.Date("2010-01-01")
) 
CAPGMFS$date <- ymd(CAPGMFS$date)

CAPGMFS <- CAPGMFS %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(CAPGMFS, "Data/CAPGMFS.csv")

# Manufacturing Factory Investment

C307RX1Q020SBEA <- fredr(
  series_id = "C307RX1Q020SBEA",
  observation_start = as.Date("2010-01-01")
) 
C307RX1Q020SBEA$date <- ymd(C307RX1Q020SBEA$date)

C307RX1Q020SBEA <- C307RX1Q020SBEA %>% 
  select(-realtime_start, -realtime_end) %>% 
  mutate(month_seq = purrr::map(date, ~ seq(.x, by = "1 month", length.out = 3))) %>%
  unnest(month_seq) %>%
  select(-date) %>% 
  rename(date = month_seq) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(C307RX1Q020SBEA, "Data/C307RX1Q020SBEA.csv")

# ISM PMI

ism_pmi <- rdb(ids = "ISM/pmi/pm")
ism_pmi$period <- ymd(ism_pmi$period)
ism_pmi <- ism_pmi %>% 
  unite(series_id, c(provider_code, series_name, series_code), sep = "") %>% 
  select(-Frequency, -dataset_code, -frequency, -original_value, -original_period, -dataset_name, -indexed_at, -`@frequency`) %>% 
  rename(date = period) %>% 
  # mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>% 
  # mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>% 
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>% 
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>% 
  #  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>% 
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg))
write_csv(ism_pmi, "Data/ism_pmi.csv")

# Combining all Goal One Datasets

manufacturing_datasets <- list(ism_pmi, MANEMP, AMTMNO, AMTMVS, IPMAN, CAPGMFS, C307RX1Q020SBEA)
weights <- c(1/2, 1/12, 1/12, 1/12, 1/12, 1/12, 1/12)
weighted_datasets <- mapply(function(df, w, i) {
  df %>%
    mutate(weighted_z = zscore_1yravg * w) %>%
    select(date, weighted_z) %>%
    rename(!!paste0("weighted_z_", i) := weighted_z)
}, manufacturing_datasets, weights, seq_along(manufacturing_datasets), SIMPLIFY = FALSE)

goal_one_tracker <- Reduce(function(x, y) full_join(x, y, by = "date"), weighted_datasets)

goal_one_tracker <- goal_one_tracker %>%
  mutate(total_weighted_z = rowSums(select(., starts_with("weighted_z_")), na.rm = TRUE))
write_csv(goal_one_tracker, "Data/goal_one_data.csv")

# Goal Two Data

# Nominal Broad US Dollar Index

DTWEXBGS <- fredr( 
  series_id = "DTWEXBGS",
  observation_start = as.Date("2010-01-01"))
DTWEXBGS$date <- ymd(DTWEXBGS$date)

DTWEXBGS <- DTWEXBGS %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  drop_na(value) %>% 
  summarise(value = mean(value), .groups = "drop") %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)*-1) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)*-1) 
write_csv(DTWEXBGS, "./Data/DTWEXBGS.csv")

# US Allies Defense Spending

gdp <- read_csv("./Data/gdp.csv")
milex <- read_csv("./Data/sipri_gdp.csv")
gdp <- gdp %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "GDP")
milex <- milex %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Milex")

military_spending <- gdp %>% 
  left_join(milex) %>% 
  mutate(GDP = GDP*1000000000) %>% 
  mutate(Milex = Milex*1000000)

military_spending <- military_spending %>% 
  group_by(Year) %>% 
  summarise(GDP = mean(GDP),
            Milex = mean(Milex)) %>%
  mutate(Share = Milex/GDP)
military_spending$Year <- as.numeric(military_spending$Year) 
write_csv(military_spending, "./Data/military_spending.csv")

# Goal Four Data

# St. Louis Fed Financial Stress Index Data

STLFSI4 <- fredr( 
  series_id = "STLFSI4",
  observation_start = as.Date("2010-01-01"))
STLFSI4$date <- ymd(STLFSI4$date)

STLFSI4 <- STLFSI4 %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(value = mean(value), .groups = "drop") %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)*-1) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)*-1) 
write_csv(STLFSI4, "./Data/STLFSI4.csv")

# Unemployment (U5) Data

U5RATE <- fredr( 
  series_id = "U5RATE",
  observation_start = as.Date("2010-01-01"))
U5RATE$date <- ymd(U5RATE$date)

U5RATE <- U5RATE %>%  
  select(-realtime_start, -realtime_end) %>%  
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)*-1) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)*-1) 
write_csv(U5RATE, "./Data/U5RATE.csv")

# Real PCE per Capita

A794RX0Q048SBEA <- fredr( 
  series_id = "A794RX0Q048SBEA",
  observation_start = as.Date("2010-01-01"))
A794RX0Q048SBEA$date <- ymd(A794RX0Q048SBEA$date)

A794RX0Q048SBEA <- A794RX0Q048SBEA %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(month_seq = purrr::map(date, ~ seq(.x, by = "1 month", length.out = 3))) %>%
  unnest(month_seq) %>%
  select(-date) %>% 
  rename(date = month_seq) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(A794RX0Q048SBEA, "./Data/A794RX0Q048SBEA.csv")


# Median Usual Weekly Earnings

LES1252881600Q <- fredr( 
  series_id = "LES1252881600Q",
  observation_start = as.Date("2010-01-01"))
LES1252881600Q$date <- ymd(LES1252881600Q$date)

LES1252881600Q <- LES1252881600Q %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(month_seq = purrr::map(date, ~ seq(.x, by = "1 month", length.out = 3))) %>%
  unnest(month_seq) %>%
  select(-date) %>% 
  rename(date = month_seq) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(LES1252881600Q, "./Data/LES1252881600Q.csv")

# Consumer Sentiment

UMCSENT <- fredr( 
  series_id = "UMCSENT",
  observation_start = as.Date("2010-01-01"))
UMCSENT$date <- ymd(UMCSENT$date)

UMCSENT <- UMCSENT %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(UMCSENT, "./Data/UMCSENT.csv")

# Median CPI

MEDCPIM158SFRBCLE <- fredr( 
  series_id = "MEDCPIM158SFRBCLE",
  observation_start = as.Date("2010-01-01"))
MEDCPIM158SFRBCLE$date <- ymd(MEDCPIM158SFRBCLE$date)

MEDCPIM158SFRBCLE <- MEDCPIM158SFRBCLE %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)*-1) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)*-1) 
write_csv(MEDCPIM158SFRBCLE, "./Data/MEDCPIM158SFRBCLE.csv")

# Combining all Goal Four Data

economic_datasets <- list(STLFSI4, U5RATE, A794RX0Q048SBEA, LES1252881600Q, UMCSENT, UMCSENT)
weights_2 <- c(1/2, 1/10, 1/10, 1/10, 1/10, 1/10)
weighted_datasets_2 <- mapply(function(df, w, i) {
  df %>%
    mutate(weighted_z = zscore_1yravg * w) %>%
    select(date, weighted_z) %>%
    rename(!!paste0("weighted_z_", i) := weighted_z)
}, economic_datasets, weights_2, seq_along(economic_datasets), SIMPLIFY = FALSE)

goal_four_tracker <- Reduce(function(x, y) full_join(x, y, by = "date"), weighted_datasets_2)

goal_four_tracker <- goal_four_tracker %>%
  mutate(total_weighted_z = rowSums(select(., starts_with("weighted_z_")), na.rm = TRUE))
write_csv(goal_four_tracker, "Data/goal_four_data.csv")

# Goal Six Data

# Balance of Trade (Utilized Metric)

BOPGSTB <- fredr( 
  series_id = "BOPGSTB",
  observation_start = as.Date("2010-01-01"))
BOPGSTB$date <- ymd(BOPGSTB$date)

goal_six_tracker <- BOPGSTB %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 120, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 120, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(goal_six_tracker, "Data/goal_six_data.csv")

# Balance of Payments

USABCAGDPBP6 <- fredr( 
  series_id = "USABCAGDPBP6",
  observation_start = as.Date("2010-01-01"))
USABCAGDPBP6$date <- ymd(USABCAGDPBP6$date)

USABCAGDPBP6 <- USABCAGDPBP6 %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 10, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 10, FUN = sd, fill = NA, align = "right")) %>%  
#  mutate(value_1yravg = rollmean(value, k = 1, fill = NA, align = "right")) %>%  
#  mutate(sd_1yravg = rollapply(value, width = 1, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
#  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) %>% 
  filter(date < "2025-01-01") 
write_csv(USABCAGDPBP6, "Data/USABCAGDPBP6.csv")

# Current Account

IEABC <- fredr( 
  series_id = "IEABC",
  observation_start = as.Date("2010-01-01"))
IEABC$date <- ymd(IEABC$date)

IEABC <- IEABC %>%  
  select(-realtime_start, -realtime_end) %>% 
  mutate(value_10yravg = rollmean(value, k = 40, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 40, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 4, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 4, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(IEABC, "Data/IEABC.csv")

# Goal Seven Data

GDPC1 <- fredr( 
  series_id = "GDPC1",
  observation_start = as.Date("2010-01-01"))
GDPC1$date <- ymd(GDPC1$date)

A091RC1Q027SBEA <- fredr( 
  series_id = "A091RC1Q027SBEA",
  observation_start = as.Date("2010-01-01"))
A091RC1Q027SBEA$date <- ymd(A091RC1Q027SBEA$date)

goal_seven_data <- A091RC1Q027SBEA %>% 
  rename(payment_value = value) %>% 
  full_join(GDPC1, by = join_by(date)) %>% 
  select(-realtime_start.y, -realtime_start.x, -realtime_end.x, -realtime_end.y, -series_id.x, -series_id.y) %>% 
  rename(gdp_value = value) %>% 
  mutate(value = payment_value/gdp_value) %>% 
  mutate(value_10yravg = rollmean(value, k = 30, fill = NA, align = "right")) %>%  
  mutate(sd_10yravg = rollapply(value, width = 30, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 3, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 3, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_10yravg = ((value - value_10yravg)/sd_10yravg)*-1) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)*-1) 
write_csv(goal_seven_data, "Data/goal_seven_data.csv")

# Goal Eight Data

goal_eight <- ustfd_simple(
  'v1/accounting/mts/mts_table_3',
  fields = c(
    "record_date", "classification_id", "classification_desc","current_month_rcpt_outly_amt"
  ),
  filter = list(
    record_date = c('>=' = '2010-01-01')
  ),
  page_size = 10000
)
goal_eight <- select(goal_eight$data, -classification_id)
goal_eight <- goal_eight %>% 
  filter(classification_desc %in% c("Customs Duties", "Total Receipts")) %>% 
  pivot_wider(names_from = classification_desc, values_from = current_month_rcpt_outly_amt) %>% 
  mutate(value = (`Customs Duties`/`Total Receipts`)*100) %>% 
  mutate(value_5yravg = rollmean(value, k = 60, fill = NA, align = "right")) %>%  
  mutate(sd_5yravg = rollapply(value, width = 60, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(value_1yravg = rollmean(value, k = 12, fill = NA, align = "right")) %>%  
  mutate(sd_1yravg = rollapply(value, width = 12, FUN = sd, fill = NA, align = "right")) %>%  
  mutate(zscore_5yravg = ((value - value_5yravg)/sd_5yravg)) %>%  
  mutate(zscore_1yravg = ((value - value_1yravg)/sd_1yravg)) 
write_csv(goal_eight, "Data/goal_eight_data.csv")














