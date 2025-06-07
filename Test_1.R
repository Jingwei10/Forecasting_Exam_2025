# ────────────────────────────────────────────────────────
# DEL 1 – PAKKER OG DATA
# ────────────────────────────────────────────────────────

# Læs nødvendige pakker – pacman installerer dem automatisk hvis de mangler
pacman::p_load(
  tidyverse, tsibble, lubridate, feasts, fable, fabletools,
  ggplot2, janitor, readr, dplyr
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
# ➜ Én graf pr. region, farver adskiller køn
data |> 
  ggplot(aes(x = yearmonth, y = svalue, color = kon)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Udvikling i arbejdsløshed over tid – sammenligning af køn",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid",
    color = "Køn"
  ) +
  theme_minimal()

# VISUALISERING 2: Sæsonmønstre – mænd og kvinder i samme plot
data |>
  gg_season(svalue, period = "year") +
  aes(color = kon, group = interaction(kon, year(yearmonth))) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +  # Diskret farveskala!
  labs(
    title = "Sæsonmønstre i arbejdsløshed – sammenligning af køn",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Måned",
    color = "Køn"
  ) +
  theme_minimal()


# VISUALISERING 3: Subserieplot – mænd og kvinder i samme plot
data |> 
  gg_subseries(svalue) +
  aes(color = kon) +
  facet_wrap(~ region, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Subserieplot – månedlig arbejdsløshed",
    subtitle = "Mænd og kvinder vises i samme plot for hver region",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Måned",
    color = "Køn"
  ) +
  theme_minimal()


# DESKRIPTIV STATISTIK: Centrale mål pr. tidsserie
# (middelværdi, spredning mv. pr. køn og region)
data |>
  features(svalue, features = list(
    mean = mean,
    variance = var,
    sd = sd,
    min = min,
    max = max
  ))

# STL-DEKOMPOSITION: Eksempel for én serie (kvinder i Region Hovedstaden)
model_stl <- data |>
  model(STL = STL(svalue ~ season(window = "periodic")))

komponenter <- model_stl |> components()

komponenter |> 
  filter(region == "Region Midtjylland", kon %in% c("Kvinder", "Mænd")) |> 
  autoplot() +
  labs(
    title = "STL-dekomposition – Region Midtjylland",
    subtitle = "Trend, sæson og remainder opdelt på køn",
    y = "Arbejdsløse pr. 10.000 personer"
  ) +
  facet_wrap(~ kon) +
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

# 4.4.1 IDENTIFICÉR BEDSTE MODEL PR. SERIE (laveste RMSE)
validering <- forecast_2019 |> 
  accuracy(test_data) |> 
  group_by(region, kon) |> 
  slice_min(RMSE, n = 1) |> 
  ungroup()

validering |> knitr::kable(caption = "Bedste model pr. serie baseret på RMSE (2019)")


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

# 5.4 Filtrér forecast for kun den bedste model pr. serie
# Vi bruger resultaterne fra valideringstabellen (DEL 4)
forecast_bedste <- forecast_2020 |> 
  semi_join(validering, by = c("region", "kon", ".model"))

# 5.5 Visualisér kun de bedste modeller pr. serie
forecast_bedste |> 
  autoplot(data) +
  aes(color = kon) +
  labs(
    title = "Forecast 2020 – bedste model pr. serie",
    subtitle = "Udvalgt ud fra laveste RMSE i validering (2019)",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid", color = "Køn"
  ) +
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal()

# 5.6 Udtræk tabel med forecasts og prediktionsintervaller
forecast_bedste |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .model, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(region, kon, yearmonth) |> 
  mutate(across(where(is.numeric), round, 2)) |> 
  knitr::kable(caption = "Forecast for 2020 (bedste model pr. serie)")



# ────────────────────────────────────────────────────────
# DEL 6 – SAMMENLIGNING OG KONKLUSION
# ────────────────────────────────────────────────────────

# 6.1 BEREGN SAMMENLIGNENDE FORECAST-TABEL
# Udvælg ét modelestimat per region og køn og måned (fx januar, april, juli, oktober)
forecast_2020 |> 
  filter(month(yearmonth) %in% c(1, 4, 7, 10)) |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .model, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(region, kon, yearmonth) |> 
  mutate(across(where(is.numeric), round, 2)) |> 
  knitr::kable(caption = "Udvalgte forecasts for 2020 (jan, apr, jul, okt) pr. model, region og køn")

# 6.2 VISUEL SAMMENLIGNING – EN MODEL FORDELT PÅ REGIONER OG KØN
# Eksempel: ARIMA's forecasts for 2020
forecast_2020 |> 
  filter(.model == "ARIMA") |> 
  autoplot(data) +
  aes(color = kon) +
  labs(
    title = "Forecast med ARIMA – mænd vs. kvinder pr. region",
    y = "Arbejdsløse pr. 10.000 personer", x = "Tid",
    color = "Køn"
  ) +
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal()


# 6.3 KONKLUSION (skriv i tekstform i rapporten, fx)
# Eksempel i tekstform:
# - ARIMA og ETS giver generelt ens resultater for de fleste serier
# - SNaive har tydeligt bredere prædiktionsintervaller
# - Arbejdsløsheden er generelt højere for mænd end kvinder i alle regioner
# - Hovedstaden og Nordjylland har lavest niveau, mens Sjælland og Syddanmark er højere
# - Der ses tydelige sæsonmønstre især i mænds arbejdsløshed

