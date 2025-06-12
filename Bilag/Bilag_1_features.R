
data <- read_rds("data/Airidk_long.rds")

# Beregn nødvendige feature-blokke
features_acf <- data |>
  group_by(kon, region) |>
  features(log(svalue), feat_acf)

features_spec <- data |>
  group_by(kon, region) |>
  features(log(svalue), feat_spectral)

features_kpss <- data |>
  group_by(kon, region) |>
  features(log(svalue), unitroot_kpss)

features_ndiffs <- data |>
  group_by(kon, region) |>
  features(log(svalue), unitroot_ndiffs)

features_hurst <- data |>
  group_by(kon, region) |>
  features(log(svalue), coef_hurst)

features_var <- data |>
  group_by(kon, region) |>
  features(log(svalue), var_tiled_mean, var_tiled_var)

features_shift <- data |>
  group_by(kon, region) |>
  features(log(svalue), shift_level_max)

features_trend <- data |>
  group_by(kon, region) |>
  features(log(svalue), feat_stl)

# Join det hele til én samlet tabel
features_all <- features_acf |>
  left_join(features_spec,  by = c("kon", "region")) |>
  left_join(features_kpss,  by = c("kon", "region")) |>
  left_join(features_ndiffs,by = c("kon", "region")) |>
  left_join(features_hurst, by = c("kon", "region")) |>
  left_join(features_var,   by = c("kon", "region")) |>
  left_join(features_shift, by = c("kon", "region")) |>
  left_join(features_trend, by = c("kon", "region"))

# Vis tabel med præcis de features du skal bruge
features_all |>
  select(kon, region,
         acf1,              # korttidsafhængighed
         acf10,             # afhængighed over 10 måneder
         season_acf1,       # sæsonstyrke (fx 12 måneder)
         spectral_entropy,  # uforudsigelighed
         var_tiled_mean,    # variation/stabilitet over tid
         shift_level_max,   # pludselige niveauskift
         ndiffs,            # nødvendige differenser for stationaritet
         coef_hurst,        # langtidshukommelse
         kpss_stat,          # stationaritetstest
         trend_strength
  ) |>
  knitr::kable(
    format = "markdown",
    digits = 2,
    caption = "Udvalgte tidsseriefunktioner pr. region og køn"
  ) |>
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover")
  ) |>
  kableExtra::row_spec(0, bold = TRUE, background = "#f0f0f0")

saveRDS(features_all, file = "data/feature_table.rds")
