# ────────────────────────────────────────────────────────
# DEL 1 – PAKKER OG DATA
# ────────────────────────────────────────────────────────

# Læs nødvendige pakker – pacman installerer dem automatisk hvis de mangler
pacman::p_load(
  tidyverse, tsibble, lubridate, feasts, fable, fabletools,
  ggplot2, janitor, readr
)

# Indlæs datasættet – gemt som en RDS-fil
data <- read_rds("data/Airidk_long.rds")

# Se struktur og de første rækker i datasættet
glimpse(data)

# Tjek at det er en tsibble (fpp3-tidsserieformat)
is_tsibble(data)  # Forvent: TRUE

# Tjek hvilke kolonner der er key og index
key_vars(data)    # Forvent: "kon", "region"
index_var(data)   # Forvent: "yearmonth"

# Tjek værdierne i svalue – repræsenterer arbejdsløshed pr. 10.000
summary(data$svalue)


# ────────────────────────────────────────────────────────
# DEL 2 – EKSPLORATIV DATAANALYSE (EDA)
# ────────────────────────────────────────────────────────

# FORMÅL:
# - Forstå trends og sæsonmønstre i arbejdsløsheden
# - Sammenligne regioner og køn
# - Forberede modellerne ved at tjekke struktur og variation

# VISUALISERING 1: Udvikling i arbejdsløshed over tid
# Facet per region og køn for at identificere mønstre
data |> 
  autoplot(svalue) +
  labs(
    title = "Udvikling i arbejdsløshed pr. region og køn",
    y = "Arbejdsløse pr. 10.000 personer", x = "Tid"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# VISUALISERING 2: Sæsonmønstre fordelt på år
# Viser hvordan arbejdsløsheden varierer hen over måneder i forskellige år
data |> 
  gg_season(svalue, period = "year") +
  labs(
    title = "Sæsonmønstre i arbejdsløshed – fordelt på år og køn",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Måned"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# VISUALISERING 3: Subserieplot til månedlig mønsteranalyse
# Giver en alternativ visning til at identificere sæsonmønstre
data |> 
  gg_subseries(svalue) +
  labs(
    title = "Subserieplot – månedlig arbejdsløshed",
    subtitle = "Fremhæver systematiske sæsonmønstre over tid",
    y = "Arbejdsløse pr. 10.000 personer"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# DESKRIPTIV STATISTIK: Centrale mål pr. tidsserie (middelværdi, spredning mv.)
data |>
  features(svalue, features = list(
    mean = mean,
    variance = var,
    sd = sd,
    min = min,
    max = max
  ))

# STL-DEKOMPOSITION: Trend, sæson og remainder for hver serie
model_stl <- data |>
  model(STL = STL(svalue ~ season(window = "periodic")))

# Udtræk komponenter
komponenter <- model_stl |> components()

# VISUALISER EKSEMPEL: Region Hovedstaden, Kvinder
komponenter |> 
  filter(region == "Region Hovedstaden", kon == "Kvinder") |> 
  autoplot() +
  labs(
    title = "STL-dekomposition – Region Hovedstaden, Kvinder",
    subtitle = "Trend, sæson og remainder i arbejdsløshed",
    y = "Arbejdsløse pr. 10.000 personer"
  ) +
  theme_minimal()

# ────────────────────────────────────────────────────────
# DEL 3 – MODELLERING: ARIMA, ETS OG SEASONAL NAIVE
# ────────────────────────────────────────────────────────

# FORMÅL:
# Estimer modeller for hver serie (region × køn) til forecasting:
# - ARIMA: Autoregressiv med evt. sæson
# - ETS: Eksponentiel glatning
# - SNaive: Seasonal Naive som benchmark

# 3.1 ESTIMÉR MODELLER PÅ HELE DATA (2007–2019)
models <- data |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 3.2 FORECAST FOR 12 MÅNEDER (2020)
fc <- models |> forecast(h = "12 months")

# 3.3 VISUALISERING: Forecasts for alle regioner og begge køn
fc |> 
  autoplot(data) +
  labs(
    title = "Forecasts for 2020 – pr. region og køn",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# 3.4 (Valgfrit) Se modelrapporter
report(models)


# ────────────────────────────────────────────────────────
# DEL 4 – MODELVALIDERING OG RESIDUALANALYSE
# ────────────────────────────────────────────────────────

# FORMÅL:
# - Evaluere modelkvalitet
# - Validere mod kendte data fra 2019
# - Undersøge residualer og forecast-fejl

# 4.1 SPLIT: Træningsdata = 2007–2018, test = 2019
train_data <- data |> filter_index(. ~ "2018 Dec")
test_data  <- data |> filter_index("2019 Jan" ~ "2019 Dec")

# 4.2 TRÆN MODELLER PÅ TRÆNINGSDATA
model_train <- train_data |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 4.3 FORECAST 12 MÅNEDER (MOD TESTDATA)
forecast_2019 <- model_train |> forecast(h = "12 months")

# 4.4 PRÆCISIONSMÅL: RMSE og MAPE mod test (2019)
forecast_2019 |> 
  accuracy(test_data) |> 
  select(kon, region, .model, RMSE, MAPE) |> 
  arrange(region, kon, RMSE)

# 4.5 VISUALISÉR: Forecast mod faktiske værdier (fx Region Sjælland)
forecast_2019 |> 
  filter(region == "Region Sjælland") |> 
  autoplot(bind_rows(train_data, test_data)) +
  facet_wrap(~ kon) +
  labs(
    title = "Forecast-validering mod 2019 – Region Sjælland",
    y = "Arbejdsløse pr. 10.000 personer"
  )

# 4.6 TJEK RESIDUALER – fx ARIMA for Region Syddanmark, Kvinder
model_train |> 
  filter(kon == "Kvinder", region == "Region Syddanmark") |> 
  select(ARIMA) |> 
  gg_tsresiduals()

# 4.7 LJUNG-BOX TEST: Er residualerne white noise?
model_train |> 
  select(ARIMA, ETS, SNaive) |> 
  augment() |> 
  features(.resid, ljung_box, lag = 24, dof = 0)

# ────────────────────────────────────────────────────────
# DEL 4B – KRYDSVALIDERING (GROWING WINDOW)
# ────────────────────────────────────────────────────────

# FORMÅL:
# - Teste modellernes forecast-evne over flere rolling origins
# - Mere robust validering end blot én testperiode (2019)

# 4B.1 LAV STRETCHED TSIBBLE
# Starter med 60 måneder (5 år) og ruller frem én måned ad gangen
data_cv <- data |>
  stretch_tsibble(.init = 60, .step = 1)

# 4B.2 ESTIMÉR MODELLER OG FORECAST 1 MÅNED FREM
# Vi bruger samme modeller som før
cv_models <- data_cv |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 4B.3 FORECAST-FEJL (MAPE og RMSE) PR. MODEL
cv_accuracy <- cv_models |> 
  forecast(h = 1) |> 
  accuracy(data) |> 
  select(kon, region, .model, RMSE, MAPE)

# 4B.4 SAMMENLIGN MODELLERNE PÅ TVÆRS AF REGION OG KØN
cv_accuracy |> 
  group_by(.model) |> 
  summarise(across(c(RMSE, MAPE), mean, na.rm = TRUE)) |> 
  arrange(RMSE)

#write_rds(cv_accuracy, "data/cv_accuracy.rds")

# Find den bedste model pr. køn og region baseret på laveste RMSE
vinder_modeller <- cv_accuracy |> 
  group_by(kon, region) |> 
  slice_min(order_by = RMSE, n = 1, with_ties = FALSE) |> 
  ungroup()

vinder_modeller

xx <- cv_accuracy |> arrange(kon, region, .model, RMSE) 

# ────────────────────────────────────────────────────────
# DEL 5 – FORECASTING: ARBEJDSLØSHED I 2020
# ────────────────────────────────────────────────────────

# 5.1 Vi bruger hele datasættet op til 2019 til at træne modellen
data_train <- data |> 
  filter_index(. ~ "2019 Dec")

# 5.2 Træn modeller på data op til 2020 (ARIMA, ETS, SNaive)
models_final <- data_train |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 5.3 Forecast 12 måneder frem (dvs. hele 2020)
forecast_2020 <- models_final |> 
  forecast(h = "12 months")

# Filtrér forecast_2020 til kun at inkludere den bedste model pr. serie
forecast_best <- forecast_2020 |> 
  inner_join(vinder_modeller, by = c("kon", "region", ".model"))

# 5.4 Visualiser
forecast_best |> 
  autoplot(data) +
  labs(
    title = "Forecast for 2020 – bedste model pr. serie",
    subtitle = "Med 80% og 95% prædiktionsintervaller",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()

# 5.5 Centrale forudsigelser
forecast_best |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .model, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(region, kon, yearmonth)



# ────────────────────────────────────────────────────────
# DEL 6 – SAMMENLIGNING OG KONKLUSION
# ────────────────────────────────────────────────────────

# 6.1 BEREGN SAMMENLIGNENDE FORECAST-TABEL
# Udvælg ét modelestimat per region og køn og måned (fx januar, april, juli, oktober)
forecast_best |> 
  filter(month(yearmonth) %in% c(1, 4, 7, 10)) |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .model, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(region, kon, yearmonth) |> 
  mutate(across(where(is.numeric), round, 2)) |> 
  knitr::kable(caption = "Forecasts for 2020 (jan, apr, jul, okt) – bedste model pr. serie")


# 6.2 VISUEL SAMMENLIGNING – EN MODEL FORDELT PÅ REGIONER OG KØN
# Eksempel: ARIMA's forecasts for 2020
forecast_best |> 
  autoplot(data) +
  labs(
    title = "Forecast for 2020 – bedste model pr. serie",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid"
  ) +
  facet_wrap(~ kon + region, scales = "free_y") +
  theme_minimal()




