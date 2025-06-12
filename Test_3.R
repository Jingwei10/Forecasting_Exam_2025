# Læs nødvendige pakker – pacman installerer dem automatisk hvis de mangler
pacman::p_load(
  tidyverse, tsibble, lubridate, feasts, fable, fabletools,
  ggplot2, janitor, readr, future
)

# Indlæs datasættet – gemt som en RDS-fil
data <- read_rds("data/Airidk_long.rds")

View(data)

# NYT ------------------------------------------------------------------------
# --- Train / test -----------------------------------------------------------
train_data <- data |> filter_index(. ~ "2018 Dec")
test_data  <- data |> filter_index("2019 Jan" ~ "2019 Dec")

# Cross-validation kun med træningsdataen
plan(multisession)

# ét vindue = 60 mdr, rul en måned ad gangen
cv_data <- train_data |>
  stretch_tsibble(.init = 60, .step = 1)

cv_models <- cv_data |>
  model(
    ARIMA  = ARIMA(log(svalue)),
    ETS    = ETS(log(svalue)),
    SNaive = SNAIVE(log(svalue))
  )

write_rds(cv_models, "cv_models.rds")

cv_accuracy <- cv_models |>
  accuracy() 

plan(sequential)

vinder_cv <- cv_accuracy |>
  group_by(kon, region) |>
  slice_min(RMSE, n = 1, with_ties = FALSE) |>
  ungroup()

# Gen-træn vindermodellen på hele træningsperioden (2010-2018)
# Fit alle tre modeller én gang …
model_train <- train_data |>
  model(
    ARIMA  = ARIMA(log(svalue)),
    ETS    = ETS(log(svalue)),
    SNaive = SNAIVE(log(svalue))
  )

# Training accuracy på hele træningsperioden (2007–2018)
train_accuracy <- model_train |> 
  accuracy() |> 
  select(kon, region, .model, RMSE_tr = RMSE, MAPE_tr = MAPE)

forecast_2019 <- model_train |>
  forecast(h = "12 months") |>
  inner_join(vinder_cv,                      
             by = c("kon", "region", ".model"))

# Endelig test-evaluation (RMSE / MAPE på test_data 2019)
test_accuracy <- forecast_2019 |>
  accuracy(test_data) |>
  select(kon, region, .model, RMSE, MAPE)

# Det er disse test-fejl, du rapporterer som den endelige objektive vurdering af modelkvaliteten.
test_accuracy

# 3. Sammenlign side om side
train_vs_test <- left_join(train_accuracy, test_accuracy,
                           by = c("kon", "region", ".model"))

# 5. Vis som tabel
train_vs_test <- train_vs_test |> 
  filter(!is.na(RMSE)) |>
  relocate(RMSE, .after = RMSE_tr)

# write_rds(train_vs_test, "train_vs_test_12.rds")
write_rds(train_vs_test, "train_vs_test_1.rds")

tt_12 <- read_rds("train_vs_test_12.rds") |> summarise(
  mean_rmse = mean(RMSE)
)
tt_1 <- read_rds("train_vs_test_1.rds") |> summarise(
  mean_rmse = mean(RMSE)
)

vinder_model <- vinder_cv |>
  filter(kon == "Mænd", region == "Region Hovedstaden") |>
  pull(.model)

model_train |>
  filter(kon == "Mænd", region == "Region Hovedstaden") |>
  select(all_of(vinder_model)) |>   # vælger modellen programmatisk
  gg_tsresiduals()

ljung_box_results <- model_train |> 
  augment() |>  # tilføjer residualer pr. observation
  features(.innov, ljung_box, lag = 24, dof = 3)

ljung_box_vindere <- ljung_box_results |>
  inner_join(vinder_cv, by = c("kon", "region", ".model"))

# Kombinér forecasts og faktiske værdier
forecast_vs_actual <- forecast_2019 |>
  left_join(
    test_data |> select(kon, region, yearmonth, actual = svalue),
    by = c("kon", "region", "yearmonth")
  )

# Visualisér
forecast_vs_actual |>
  ggplot(aes(x = yearmonth)) +
  geom_line(aes(y = actual), color = "black", size = 0.9, linetype = "solid") +
  geom_line(aes(y = .mean), color = "blue", size = 0.9, linetype = "dashed") +
  facet_grid(region ~ kon, scales = "free_y") +
  labs(
    title = "Forecast vs. Faktiske værdier – 2019",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid",
    caption = "Sort = Faktisk | Blå = Forecast"
  ) +
  theme_minimal(base_size = 13)

data_train_final <- data

models_final <- data_train_final |> 
  model(
    ARIMA  = ARIMA(log(svalue)),
    ETS    = ETS(log(svalue)),
    SNaive = SNAIVE(log(svalue))
  )

# Forecast de næste 12 måneder (2020) med den bedste model pr. serie
forecast_2020 <- models_final |> 
  forecast(h = "12 months") |> 
  inner_join(vinder_cv, by = c("kon", "region", ".model"))

forecast_2020 |> 
  autoplot(level = c(80, 95)) +
  facet_grid(region ~ kon, scales = "free_y") +
  labs(
    title = "Forecast for 2020 – bedste model pr. serie",
    subtitle = "Med 80 % og 95 % prædiktionsintervaller",
    y = "Arbejdsløse pr. 10.000 personer",
    x = "Tid"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )
