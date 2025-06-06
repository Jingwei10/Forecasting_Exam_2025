
# ────────────────────────────────────────────────────────
# 📦 1. PAKKER OG DATAFORBEREDELSE
# ────────────────────────────────────────────────────────

pacman::p_load(tidyverse, tsibble, fable, feasts, lubridate, readr, janitor, patchwork)

# Indlæs og rens data
data <- read_rds("data/Airidk_long.rds") |>
  clean_names()

# Data er en tsibble: indeholder 'yearmonth' som index og 'kon' og 'region' som keys
glimpse(data)

# ────────────────────────────────────────────────────────
# 🔍 2. EKSPLORATIV DATAANALYSE (EDA)
# ────────────────────────────────────────────────────────

# 1. Visualisering af arbejdsløsheden over tid for alle regioner og køn
data |> 
  ggplot(aes(x = yearmonth, y = svalue)) +
  geom_line() +
  facet_grid(kon ~ region) +
  labs(
    title = "Figur 1: Arbejdsløshed over tid per region og køn",
    x = "Tid",
    y = "Arbejdsløshed (%)"
  )

# 2. Visualisering af sæsonmønstre for alle serier
data |> 
  gg_season(svalue) +
  facet_grid(kon ~ region) +
  labs(
    title = "Figur 2: Sæsonmønstre i arbejdsløshed",
    y = "Arbejdsløshed (%)"
  )

# 3. Subseries-plot for at vise ændringer i sæsoner over tid
data |> 
  gg_subseries(svalue) +
  facet_grid(kon ~ region) +
  labs(
    title = "Figur 3: Subseries plot for arbejdsløshed",
    y = "Arbejdsløshed (%)"
  )

# 4. Beregn STL-baserede deskriptive statistikker
data |>
  features(svalue, feat_stl) |>
  arrange(desc(trend_strength)) |>
  select(kon, region, trend_strength, seasonal_strength_year, spikiness)

# 5. STL-dekomponering for én serie (Kvinder i Region Hovedstaden)
data |> 
  filter(kon == "Kvinder", region == "Region Hovedstaden") |> 
  model(STL(svalue)) |> 
  components() |> 
  autoplot() +
  labs(
    title = "Figur 4: STL-dekomponering – Kvinder, Region Hovedstaden",
    y = "Komponentværdi"
  )

# 6. (Valgfrit) Transformation hvis varians er ikke-stationær
data <- data |> 
  mutate(svalue_log = log(svalue))


# ────────────────────────────────────────────────────────
# 📊 3. MODELVALG: ARIMA, ETS, NAIVE
# ────────────────────────────────────────────────────────

# Automatisk modellering for alle serier
models <- data |> 
  model(
    ets = ETS(svalue),
    arima = ARIMA(svalue),
    naive = NAIVE(svalue)
  )

# Kort oversigt over hver models parametre
models |> glance()

# ────────────────────────────────────────────────────────
# 🧪 4. MODELVALIDERING – CV OG RESIDUALER
# ────────────────────────────────────────────────────────

# Rolling origin cross-validation
cv <- data |> 
  stretch_tsibble(.init = 60, .step = 1) |> 
  model(
    ets = ETS(svalue),
    arima = ARIMA(svalue),
    naive = NAIVE(svalue)
  )

# Forecast 12 måneder frem og mål præcision
cv_accuracy <- cv |> 
  forecast(h = 12) |> 
  accuracy(data)

# Find bedste model pr. serie baseret på laveste RMSE
best_models <- cv_accuracy |> 
  group_by(kon, region) |> 
  slice_min(RMSE)

# Tjek residualer for hvid støj med Ljung-Box
models |> 
  augment() |> 
  features(.resid, ljung_box, lag = 24)

# ────────────────────────────────────────────────────────
# 🔮 5. FORECASTING: 12 MÅNEDER (2020)
# ────────────────────────────────────────────────────────

# Forecast med de modeller vi har trænet
fc <- models |> 
  forecast(h = "12 months")

# Visualiser eksempel
fc |> 
  filter(region == "Region Hovedstaden", kon == "Kvinder") |> 
  autoplot(data) +
  labs(title = "Forecast – Region Hovedstaden, Kvinder", y = "Arbejdsløshed (%)")

# Prediktionsintervaller til tabel
fc |> 
  hilo(level = 95) |> 
  unpack_hilo("95%") |> 
  select(kon, region, yearmonth, .mean, `95%_lower`, `95%_upper`) |> 
  arrange(kon, region, yearmonth)


# ────────────────────────────────────────────────────────
# 📈 6. MODEL SAMMENLIGNING OG KONKLUSION
# ────────────────────────────────────────────────────────

# Sammenlign modeller pr. serie – træf beslutning
accuracy_summary <- models |> 
  forecast(h = 12) |> 
  accuracy(data) |> 
  group_by(kon, region) |> 
  summarise(
    best_model = model[which.min(RMSE)],
    rmse = min(RMSE),
    mape = MAPE[which.min(RMSE)],
    .groups = "drop"
  )

# Udskriv konklusion pr. serie
print(accuracy_summary)

# Eventuelt: Gem som CSV hvis det ønskes til bilag
# write_csv(accuracy_summary, "results/model_accuracy_summary.csv")

# ────────────────────────────────────────────────────────
# 📝 7. EKSTRA: Sammenlign forecast plots
# ────────────────────────────────────────────────────────

# Plot forecasts for udvalgte serier
regions_to_plot <- c("Region Hovedstaden", "Region Sjælland")
gender_to_plot <- c("Kvinder", "Mænd")

fc |> 
  filter(region %in% regions_to_plot, kon %in% gender_to_plot) |> 
  autoplot(data) +
  facet_grid(kon ~ region) +
  labs(title = "Forecasts: Udvalgte regioner og køn", y = "Arbejdsløshed (%)")

# Klar til at blive brugt i rapporten og PDF
