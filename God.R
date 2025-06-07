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

# FÆLLES TEMA
tema_graf <- theme_minimal(base_size = 13)

# VISUALISERING 1: Udvikling over tid – mænd og kvinder i samme plot
data |> 
  ggplot(aes(x = yearmonth, y = svalue, color = kon)) +
  geom_line() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Udvikling i arbejdsløshed over tid",
    subtitle = "Mænd og kvinder vises med farver, én graf pr. region",
    y = "Arbejdsløse pr. 10.000 personer", x = "Tid",
    color = "Køn"
  ) +
  scale_color_brewer(palette = "Set1") +
  tema_graf

# VISUALISERING 2: Sæsonmønstre (gg_season)
data |> 
  gg_season(svalue, period = "year") +
  aes(color = kon, group = interaction(kon, year(yearmonth))) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Sæsonmønstre i arbejdsløshed",
    subtitle = "Farver = køn, linjer viser månedlig udvikling pr. år",
    y = "Arbejdsløse pr. 10.000 personer", x = "Måned",
    color = "Køn"
  ) +
  scale_color_brewer(palette = "Set1") +
  tema_graf

# VISUALISERING 3: Subserieplot
data |> 
  gg_subseries(svalue) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Subserieplot – månedlig arbejdsløshed",
    subtitle = "Én graf pr. region – farver angiver måned",
    y = "Arbejdsløse pr. 10.000 personer", x = "Måned"
  ) +
  tema_graf

# VISUALISERING 4: STL-dekomposition – Region Midtjylland, begge køn
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
  tema_graf

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

# 3.3 VISUALISERING: Forecasts pr. model, region og køn
# Sammenligning mellem faktiske værdier og forecasts
fc |> 
  autoplot(data, level = c(80, 95)) +
  aes(color = kon) +  # Farver på køn
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Forecasts for 2020 – ARIMA, ETS og SNaive",
    subtitle = "Pr. region – farver viser køn, bånd viser modelusikkerhed",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid",
    color = "Køn"
  ) +
  scale_color_manual(values = c("Kvinder" = "red", "Mænd" = "blue")) +
  theme_minimal(base_size = 13)


# 3.4 MODELINSPEKTION (valgfrit)
report(models)


# ────────────────────────────────────────────────────────
# DEL 4 – MODELVALIDERING OG RESIDUALANALYSE
# ────────────────────────────────────────────────────────

# 4.1 SPLIT: Træningsdata = 2007–2018, test = 2019

# Træningsdata: 2007–2018
train_data <- data |> filter_index(. ~ "2018 Dec")

# Testdata: 2019
test_data  <- data |> filter_index("2019 Jan" ~ "2019 Dec")


# 4.2 TRÆN MODELLER PÅ TRÆNINGSDATA
model_train <- train_data |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 4.3 FORECAST 12 MÅNEDER MOD TESTDATA
forecast_2019 <- model_train |> forecast(h = "12 months")

# 4.4 PRÆCISIONSMÅL: RMSE og MAPE
# Find bedste model pr. serie (laveste RMSE)
validering <- forecast_2019 |> 
  accuracy(test_data) |> 
  group_by(region, kon) |> 
  slice_min(RMSE, n = 1) |> 
  ungroup()

# Tabel med eksakte værdier
validering |> 
  knitr::kable(caption = "Bedste model pr. serie (2019) baseret på RMSE")

# Tibble visning
validering |> 
  count(.model)

# Plots der viser hvilken model, der vinder baseret på laveste RMSE
validering |> 
  ggplot(aes(x = interaction(region, kon), fill = .model)) +
  geom_bar() +
  labs(
    title = "Antal gange hver model er valgt som bedst (laveste RMSE)",
    x = "Serie (region × køn)", fill = "Model"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 4.5 VISUALISÉR: Forecast vs. faktiske værdier (Region Sjælland)
forecast_2019 |> 
  filter(region == "Region Sjælland") |> 
  autoplot(bind_rows(train_data, test_data)) +
  aes(color = kon) +
  facet_wrap(~ kon) +
  labs(
    title = "Forecast-validering 2019 – Region Sjælland",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid",
    color = "Køn"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# 4.6 TJEK RESIDUALER: 

# ARIMA – god model
model_train |> 
  filter(region == "Region Syddanmark", kon == "Kvinder") |>
  select(ARIMA) |>
  gg_tsresiduals() +
  labs(
    title = "Residualanalyse – ARIMA",
    subtitle = "Region Syddanmark, Kvinder"
  )

# SNaive – også valgt
model_train |> 
  filter(region == "Region Hovedstaden", kon == "Mænd") |>
  select(SNaive) |>
  gg_tsresiduals() +
  labs(
    title = "Residualanalyse – SNaive",
    subtitle = "Region Hovedstaden, Mænd"
  )

# ETS – ikke valgt
model_train |> 
  filter(region == "Region Sjælland", kon == "Kvinder") |>
  select(ETS) |>
  gg_tsresiduals() +
  labs(
    title = "Residualanalyse – ETS",
    subtitle = "Region Sjælland, Kvinder"
  )

# 4.7 WHITE NOISE TEST: Ljung-Box-test for alle modeller
model_train |> 
  select(ARIMA, ETS, SNaive) |> 
  augment() |> 
  features(.resid, ljung_box, lag = 24, dof = 0) |> 
  knitr::kable(caption = "Ljung-Box-test: Er residualerne white noise?")


# ────────────────────────────────────────────────────────
# DEL 5 – FORECASTING: ARBEJDSLØSHED I 2020
# ────────────────────────────────────────────────────────

# 5.1 TRÆN SLUTMODELLER PÅ DATA OP TIL 2019
data_train <- data |> 
  filter_index(. ~ "2019 Dec")

models_final <- data_train |> 
  model(
    ARIMA  = ARIMA(svalue),
    ETS    = ETS(svalue),
    SNaive = SNAIVE(svalue)
  )

# 5.2 FORECAST FOR 2020 (12 MÅNEDER)
forecast_2020 <- models_final |> 
  forecast(h = "12 months")

# 5.3 FILTRÉR KUN DEN BEDSTE MODEL PR. SERIE
# Bruger resultaterne fra valideringstabellen i DEL 4
forecast_bedste <- forecast_2020 |> 
  semi_join(validering, by = c("region", "kon", ".model"))

# 5.4 VISUALISÉR FORCAST – BEDSTE MODEL PR. SERIE
forecast_bedste |> 
  autoplot(data) +
  aes(color = kon) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Forecast 2020 – bedste model pr. serie",
    subtitle = "Udvalgt ud fra laveste RMSE i validering (2019)",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid",
    color = "Køn"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# 5.5 TABEL MED FORUDSIGELSER OG 95% PRÆDIKTIONSINTERVALLER
forecast_bedste |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .model, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(region, kon, yearmonth) |> 
  mutate(across(where(is.numeric), round, 2)) |> 
  knitr::kable(caption = "Forecast for 2020 – bedste model pr. serie (med 95% intervaller)")


# ────────────────────────────────────────────────────────
# DEL 6 – SAMMENLIGNING OG KONKLUSION
# ────────────────────────────────────────────────────────

# 6.1 FORECAST-TABEL FOR UDVALGTE MÅNEDER (jan, apr, jul, okt)
# VISUALISÉR: Forecasts fra den bedste model pr. serie (valgt via RMSE)
# Først: pak 95% intervaller ud til separate kolonner
forecast_plotdata <- forecast_bedste |> 
  hilo(level = 95) |> 
  unpack_hilo("95%")

# Derefter: lav plot med korrekt aes
forecast_plotdata |> 
  ggplot(aes(x = yearmonth, y = .mean)) +
  geom_line(aes(color = kon)) +
  geom_ribbon(aes(ymin = `95%_lower`, ymax = `95%_upper`, fill = .model), alpha = 0.3) +
  scale_color_manual(values = c("Kvinder" = "red", "Mænd" = "blue")) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Forecast 2020 – bedste model pr. serie",
    subtitle = "Modeller valgt ud fra laveste RMSE i validering",
    x = "Tid", y = "Arbejdsløse pr. 10.000 personer",
    color = "Køn", fill = "Model"
  ) +
  facet_wrap(~ region, scales = "free_y") +
  theme_minimal()




