# ─────────────────────────────────────────────
# 1. PAKKER OG DATA
# ─────────────────────────────────────────────

# Laster nødvendige pakker med pacman (installerer dem hvis de ikke er til stede)
pacman::p_load(tidyverse, gghighlight, hrbrthemes, tsibble, lubridate, 
               ggplot2, feasts, fable, tsibbledata, forecast, fpp2, fpp3,
               fabletools, knitr, ggpubr, gridExtra, GGally, slider,
               forecast, car, shiny, glue, broom, kableExtra, gt, seasonal, 
               latex2exp, ggfortify, janitor, readxl, rlang)


# Læser Excel-filen ind. Den har to kolonner: dato og antal flyvninger
Flyvninger <- read_excel("data/Flyvninger.xlsx", sheet = "Ark1")

# Rydder op i data:
flyvninger <- Flyvninger |> 
  clean_names() |>                  # Gør kolonnenavne ensartede og små (snake_case)
  mutate(date = yearmonth(date)) |> # Konverterer datoer til år-måned format (fx "2018 jan")
  as_tsibble(index = date) |>       # Gør det til en tidsserie (tsibble) med "date" som indeks
  filter_index(. ~ "2019 dec")      # Fjerner data efter december 2019 (valgfrit)

# ─────────────────────────────────────────────
# 2. VISUALISERING AF TIDSSERIEN
# ─────────────────────────────────────────────

# Simpel visning af tidsserien
flyvninger |> autoplot()

# Brug log-transform for at dæmpe store udsving og stabilisere varians
flyvninger |> autoplot(log(antal_flyvninger_i_1000))

# Vis mønstre pr. måned på tværs af år (sæsonmønstre)
flyvninger |> gg_season(log(antal_flyvninger_i_1000))

# Vis hvert månedsforløb separat over tid (subseries)
flyvninger |> gg_subseries(log(antal_flyvninger_i_1000))

# ─────────────────────────────────────────────
# 3. STL-MODEL: Dele serien i trend, sæson og støj
# ─────────────────────────────────────────────

# Vi laver STL-model (Seasonal-Trend decomposition with Loess)
model_flyv <- flyvninger |> 
  model(STL = STL(log(antal_flyvninger_i_1000), robust = TRUE)) 
# robust = TRUE gør modellen mindre følsom overfor outliers

# Udtræk komponenterne (trend, sæson, remainder)
komponenter <- model_flyv |> components()

# Visualiser både log-serien og trendkomponenten i samme plot
komponenter |> 
  ggplot(aes(x = date)) +
  geom_line(aes(y = `log(antal_flyvninger_i_1000)`), colour = "grey60") +
  geom_line(aes(y = trend), colour = "blue") +
  labs(title = "Trend i log-transformeret antal flyvninger")

# ─────────────────────────────────────────────
# 4. MÅL FOR TREND OG SÆSON-STYRKE
# ─────────────────────────────────────────────

# Vi beregner hvor stærk trenden og sæsonen er
flyvninger |>
  features(log(antal_flyvninger_i_1000), feat_stl)

# Det giver fx:
# - trend_strength = hvor tydeligt er den langsigtede udvikling?
# - seasonal_strength_year = hvor stærk er årstidsvariationen?
# - spikiness = pludselige udsving?

# ─────────────────────────────────────────────
# 5. RESIDUALER OG DIAGNOSTIK
# ─────────────────────────────────────────────

# Vi kigger på hvor godt modellen passer
# Augment tilføjer kolonner med residualer og fitted values
flyvninger |> 
  model(STL = STL(log(antal_flyvninger_i_1000))) |>
  augment()

# Du kan visualisere residualerne for at se, om de er tilfældige

# ─────────────────────────────────────────────
# 6. KRYDSVALIDERING (GROWING WINDOW)
# ─────────────────────────────────────────────

# Vi laver flere træningsvinduer over tid (rolling forecast origin)
flyvningerstretch <- flyvninger |>
  stretch_tsibble(.init = 10, .step = 1) 
# starter med 10 observationer og tilføjer 1 hver gang

# Brug en simpel Random Walk model med drift som baseline
flyvningerstretch |> 
  model(RW(log(antal_flyvninger_i_1000) ~ drift())) |>
  forecast(h = 1)

# ─────────────────────────────────────────────
# 7. TRAIN / TEST SPLIT FOR MODELLER
# ─────────────────────────────────────────────

# Brug kun data indtil 2017 til at træne modellen
flyvninger_train <- flyvninger |> 
  filter_index(. ~ "2017 dec")

# ─────────────────────────────────────────────
# 8. ARIMA OG ETS MODELLER
# ─────────────────────────────────────────────

# Træn ARIMA-model (med log-transform)
fit_arima <- flyvninger_train |>
  model(ARIMA(log(antal_flyvninger_i_1000)))

# Træn ETS-model (eksponentiel glatning)
fit_ets <- flyvninger_train |>
  model(ETS(log(antal_flyvninger_i_1000)))

# ─────────────────────────────────────────────
# 9. RESIDUAL-ANALYSE OG FEJLTEST
# ─────────────────────────────────────────────

# Visualisér fejlled (residualer), autocorrelation, histogram
fit_arima |> gg_tsresiduals()
fit_ets   |> gg_tsresiduals()

# Statistisk test: Ljung-Box
augment(fit_arima) |> 
  features(.innov, ljung_box, lag = 24, dof = 5)

augment(fit_ets) |> 
  features(.innov, ljung_box, lag = 24)

# God model → residualer ser tilfældige ud og tester IKKE signifikant

# ─────────────────────────────────────────────
# 10. MODELACCURACY – HVILKEN MODEL ER BEDST?
# ─────────────────────────────────────────────

# Sammenlign modelpræcision (på både træning og test)
bind_rows(
  fit_arima |> accuracy(),                                   # ARIMA på træning
  fit_ets   |> accuracy(),                                   # ETS på træning
  fit_arima |> forecast(h = 24) |> accuracy(flyvninger),     # ARIMA på test (forecast)
  fit_ets   |> forecast(h = 24) |> accuracy(flyvninger)      # ETS på test
)

# Kig fx på:
# - RMSE: gennemsnitlig fejl (kvadratrod)
# - MAPE: procentuel fejl
# Mindst fejl = bedst model

# ─────────────────────────────────────────────
# 11. FORECAST: SE 3 ÅR FREM
# ─────────────────────────────────────────────

flyvninger |> 
  model(ARIMA(log(antal_flyvninger_i_1000))) |> 
  forecast(h = "3 years") |> 
  autoplot(flyvninger)

# Forudsiger 3 år frem og plotter sammen med historiske data
