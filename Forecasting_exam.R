# ────────────────────────────────────────────────────────
# DEL 1 – PAKKER OG DATA
# ────────────────────────────────────────────────────────

# Læs nødvendige pakker – pacman installerer dem automatisk hvis de mangler
pacman::p_load(
  tidyverse, tsibble, lubridate, feasts, fable, fabletools,
  ggplot2, janitor, readr
)

# Indlæs datasættet – det er gemt som en RDS-fil og ligger i mappen 'data'
data <- read_rds("data/Airidk_long.rds")

# Se struktur og de første rækker i datasættet
glimpse(data)

# Tjek at det ER en tsibble (tidsserieformat fra fpp3)
is_tsibble(data)  # Skal give TRUE

# Tjek index og key-kolonner (de skal være: yearmonth, region, kon)
key_vars(data)
index_var(data)


# ────────────────────────────────────────────────────────
# DEL 2 – EKSPLORATIV DATAANALYSE (EDA)
# ────────────────────────────────────────────────────────

# VISUALISERING 1: Udvikling i arbejdsløshed over tid
# Facet per region og køn for at identificere mønstre
data |> 
  autoplot(svalue) +
  labs(title = "Udvikling i arbejdsløshed pr. region og køn",
       y = "Arbejdsløshed (%)", x = "Tid") +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# VISUALISERING 2: Sæsonmønster for hver serie
data |> 
  gg_season(svalue, period = "year") +
  labs(title = "Sæsonmønstre fordelt på år",
       y = "Arbejdsløshed (%)") +
  facet_wrap(~ kon + region)

# VISUALISERING 3: Subserieplot til månedlig mønsteranalyse
data |> 
  gg_subseries(svalue) +
  labs(title = "Subserieplot – månedlig arbejdsløshed", 
       subtitle = "Fremhæver systematiske sæsonmønstre") +
  facet_wrap(~ kon + region)

# DESKRIPTIV STATISTIK: Centrale mål pr. tidsserie
# Henter statistiske beskrivelser for hver kombination af køn og region
data |>
  features(svalue, features = list(
    mean = mean,
    variance = var,
    sd = sd,
    min = min,
    max = max
  ))

# STL-DEKOMPOSITION: Adskiller trend, sæson og remainder
# En STL-model pr. serie – vi bruger 'window = periodic' for stabil sæson
model_stl <- data |>
  model(STL = STL(svalue ~ season(window = "periodic")))

# Uddrag komponenter (trend, season, remainder)
komponenter <- model_stl |> components()

# Visualiser eksempel: Kvinder i Region Hovedstaden
komponenter |> 
  filter(region == "Region Hovedstaden", kon == "Kvinder") |> 
  autoplot() +
  labs(title = "STL-dekomposition – Region Hovedstaden, Kvinder",
       subtitle = "Svalue opdelt i trend, sæson og remainder")

# (Valgfrit) Transformationstjek: Har vi brug for log(svalue)?
# Vi undersøger om log-transform giver et mere stabilt mønster
data |> 
  ggplot(aes(x = yearmonth, y = log(svalue))) +
  geom_line() +
  facet_wrap(~ kon + region, scales = "free_y") +
  labs(title = "Log-transformeret arbejdsløshed pr. serie", y = "log(arbejdsløshed)")


# ────────────────────────────────────────────────────────
# DEL 3 – MODELLERING: ARIMA, ETS OG BENCHMARK
# ────────────────────────────────────────────────────────

# 🎯 FORMÅL:
# Vi estimerer tre modeltyper for hver serie (region × køn):
# - ARIMA (autoregressiv model m. mulig sæson)
# - ETS (eksponentiel glatning)
# - Naive benchmark (simpleste model: bruger sidste observation)

# ARIMA: Automatisk modelvalg med fable::ARIMA()
model_arima <- data |>
  model(ARIMA = ARIMA(svalue))

# Se et eksempel på automatisk valgt ARIMA-model
report(model_arima)

# ETS: Automatisk valg af glatningsmodel (Error-Trend-Seasonal)
model_ets <- data |>
  model(ETS = ETS(svalue))

# Se et eksempel på valgt ETS-model
report(model_ets)

# BENCHMARK: Naiv model (forvent næste værdi = sidste)
model_naive <- data |>
  model(Naive = NAIVE(svalue))

# SAMLEDE MODELLER I ÉT OBJEKT
models <- model_arima |>
  left_join(model_ets, by = c("kon", "region")) |>
  left_join(model_naive, by = c("kon", "region"))

# VISUALISÉR FORSKELLIGE MODELLER – ét eksempel
models |> 
  forecast(h = "12 months") |>
  filter(region == "Region Midtjylland", kon == "Mænd") |> 
  autoplot(data) +
  labs(title = "Modeller og forecast: Region Midtjylland, Mænd",
       y = "Arbejdsløshed (%)")


# ────────────────────────────────────────────────────────
# DEL 4 – MODELVALIDERING OG TVÆRSVALIDERING
# ────────────────────────────────────────────────────────

# Formål:
# Evaluere præcision af modellerne og vurdere residualernes tilfældighed


# 4.1 Lav training data – vi bruger kun frem til 2019
train_data <- data |> 
  filter_index(. ~ "2019 Dec")

# 4.2 Træn modeller
model_train <- train_data |> 
  model(
    ARIMA = ARIMA(svalue),
    ETS   = ETS(svalue),
    Naive = NAIVE(svalue)
  )

# 4.3 Forecast: 12 måneder frem
fc <- model_train |> forecast(h = "12 months")

# 4.4 Eksempel: Visualisering for én serie (fx Region Midtjylland, Mænd)
fc |> 
  filter(kon == "Mænd", region == "Region Midtjylland") |> 
  autoplot(train_data) +
  labs(title = "Modeller og forecast: Region Midtjylland, Mænd",
       y = "Arbejdsløshed (%)")

# 4.5 Residualanalyse for én model og én serie
model_train |> 
  filter(kon == "Kvinder", region == "Region Syddanmark") |> 
  select(ARIMA) |>  # Vælg én model
  gg_tsresiduals()

# 4.6 Ljung-Box test for hvid støj
model_train |> 
  select(ARIMA, ETS, Naive) |> 
  augment() |> 
  features(.resid, ljung_box, lag = 24, dof = 0)


augment(fit_arima) |>
  features(.innov, ljung_box, lag = 24, dof = 5)
