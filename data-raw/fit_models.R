# Author: Kevin See
# Purpose: Fit redd observer net error models
# Created: 1/27/23
# Last Modified: 3/8/23
# Notes: one observer steelhead data comes from the Wenatchee, two observer steelhead from the Methow

#-----------------------------------------------------------------
# load needed libraries
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(forcats)
library(magrittr)
library(janitor)

#-----------------------------------------------------------------
# One observer steelhead model
#-----------------------------------------------------------------
# read in data
one_obs_sthd <- read_csv("data-raw/one_obs_sthd_redd_data.csv") |>
  clean_names() |>
  rename(lwd_total = lw_dtotal,
         mean_discharge = mean_dis) |>
  mutate(
    across(
      stream,
      str_to_title),
    across(
      date,
      mdy),
    across(
      stream,
      ~ case_match(.,
                   "Pes" ~ "Peshastin",
                   "Wen" ~ "Wenatchee",
                   .default = .)
    )
  ) |>
  mutate(reach_length_km = reach_length_m / 1000) |>
  mutate(naive_density_km = total_features / reach_length_km,
         exp_sp_total_log = log(exp_sp_total),
         net_error = (total_features / visible_redds) - 1) |>
  select(year:reach,
         surveyor,
         date,
         total_redds:total_features,
         starts_with("exp_"),
         everything())

# adjust some of the original data based on subsequent years
# this data was compiled by Andrew Murdoch at WDFW
updated_data <- tribble(
  ~reach, ~mean_thalweg_cv, ~reach_length_km,
  "I1", 61.27, 4.94,
  "N3", 54.41, 9.25,
  "P1", 27.15, 5.96,
  "W9", 44.87, 17.4,
  "W10", 37.95, 7.57)

# one_obs_sthd |>
#   select(year, stream, reach,
#          old_rch_length = reach_length_km,
#          old_thalweg_cv = mean_thalweg_cv) |>
#   distinct() |>
#   left_join(updated_data) |>
#   select(year:reach,
#          contains("length"),
#          contains("thalweg"))

one_obs_sthd |>
  select(-c(reach_length_km,
            mean_thalweg_cv)) |>
  left_join(updated_data,
            by = "reach") |>
  mutate(naive_density_km = total_features / reach_length_km) |>
  select(all_of(names(one_obs_sthd))) -> one_obs_sthd

# normalize the covariates
one_obs_covar_center <- one_obs_sthd |>
  select(exp_sp_total:exp_sp_total_log,
         gradient,
         precip:naive_density_km) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "values"
  ) |>
  group_by(metric) |>
  summarize(
    across(
      values,
      list(mean = mean,
           sd = sd),
      na.rm = TRUE,
      .names = "{.fn}"
    ),
    .groups = "drop")

one_obs_mod_df = one_obs_sthd |>
  mutate(id = 1:n()) %>%
  select(id,
         year:date,
         total_features,
         visible_redds,
         net_error,
         mean_depth,
         mean_thalweg_cv,
         naive_density_km) |>
  pivot_longer(cols = -c(id:net_error),
               names_to = "metric",
               values_to = "value") |>
  left_join(one_obs_covar_center) |>
  mutate(across(value,
                ~ (. - mean) / sd)) |>
  select(-c(mean:sd)) |>
  pivot_wider(names_from = metric,
              values_from = value)

one_obs_net_mod <- glm(net_error ~ mean_depth + mean_thalweg_cv + naive_density_km,
                       data = one_obs_mod_df,
                       family = gaussian)


#-----------------------------------------------------------------
# Two observer steelhead model
#-----------------------------------------------------------------
# read in data
two_obs_sthd <- read_csv('data-raw/two_obs_sthd_redd_data.csv') |>
  clean_names() |>
  rename(
    exp_sp_total = exp_total,
    omitted_redds = omitted_redds_visible) |>
  mutate(
    across(
      -c(stream,
         reach,
         surveyor,
         date),
      as.numeric),
    across(date,
           mdy)) |>
  mutate(across(reach_length_m,
                ~ if_else(stream == "Methow" & reach == "M10", 5380, .))) |>
  mutate(reach_length_km = reach_length_m / 1000,
         naive_density_km = total_features / reach_length_km,
         exp_sp_total_log = log(exp_sp_total),
         net_error = (total_features / visible_redds) - 1) |>
  group_by(stream, reach) |>
  mutate(
    across(
      mean_thalweg_cv,
      mean)) |>
  ungroup() |>
  select(year:surveyor,
         date,
         total_redds:super_imposed_features,
         starts_with("exp_"),
         everything())

two_obs_covar_center <- two_obs_sthd |>
  select(exp_sp_total:exp_sp_total_log,
         effort:naive_density_km) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "values"
  ) |>
  group_by(metric) |>
  summarize(
    across(
      values,
      list(mean = ~ mean(., na.rm = T),
           sd = ~ sd(., na.rm = T)),
      .names = "{.fn}"
    ),
    .groups = "drop")

two_obs_mod_df = two_obs_sthd |>
  mutate(id = 1:n()) %>%
  select(id,
         year:date,
         total_features,
         visible_redds,
         net_error,
         starts_with('exp_sp_total'),
         mean_discharge,
         mean_thalweg_cv,
         naive_density_km) |>
  pivot_longer(cols = -c(id:net_error),
               names_to = "metric",
               values_to = "value") |>
  left_join(two_obs_covar_center) |>
  mutate(across(value,
                ~ (. - mean) / sd)) |>
  select(-c(mean:sd)) |>
  pivot_wider(names_from = metric,
              values_from = value)

two_obs_net_mod <-
  glm(net_error ~
        exp_sp_total_log +
        mean_discharge +
        mean_thalweg_cv +
        naive_density_km,
                       data = two_obs_mod_df,
                       family = gaussian)

#-----------------------------------------------------------------
# using a random forest model
#-----------------------------------------------------------------
library(randomForestSRC)

one_obs_net_rf = rfsrc(formula(one_obs_net_mod),
                       data = one_obs_sthd |>
                         select(year:total_features,
                                net_error,
                                all_of(attr(one_obs_net_mod$terms, "term.labels"))) |>
                         as.data.frame(),
                       forest = T,
                       importance = T,
                       membership = T)

two_obs_net_rf = rfsrc(update(formula(two_obs_net_mod), . ~ . - exp_sp_total_log + exp_sp_total),
                       data = two_obs_sthd |>
                         select(year:total_features,
                                net_error,
                                exp_sp_total,
                                all_of(attr(two_obs_net_mod$terms, "term.labels"))) |>
                         as.data.frame(),
                       forest = T,
                       importance = T,
                       membership = T)

# look at partial dependence plots
plot.variable(one_obs_net_rf,
              partial = T,
              smooth.lines = T)

plot.variable(two_obs_net_rf,
              partial = T,
              smooth.lines = T)

# look at prediction standard errors
library(forestError)

findOOBErrors(two_obs_net_rf,
              X.train = two_obs_net_rf$xvar)

err_df <-
  quantForestError(two_obs_net_rf,
                   X.train = two_obs_net_rf$xvar,
                   X.test = two_obs_net_rf$xvar)
err_df$estimates |>
  as_tibble()


#-----------------------------------------------------------------
# Spring Chinook
#-----------------------------------------------------------------
# read in data
one_obs_chnk <- read_csv("data-raw/one_obs_spchnk_redd_data.csv") |>
  clean_names() |>
  rename(exp_sp_total = exp_total_spngrd) |>
  select(-naive_density_per_km,
         -net_error) |>
  group_by(stream, reach) |>
  mutate(
    across(
      mean_thalweg_cv,
      mean)) |>
  ungroup() |>
  mutate(reach_length_km = reach_length_m / 1000,
         naive_density_km = total_features / reach_length_km,
         exp_sp_total_log = log(exp_sp_total),
         omit_error = omitted_redds / visible_redds,
         commit_error = false_id / total_features,
         net_error = (total_features / visible_redds) - 1) |>
  select(year:surveyor,
         visible_redds:total_features,
         super_imposed_features,
         starts_with("exp"),
         everything())

# # look for break points where experience seems to matter so we can turn experience into a factor
# library(ggplot2)
# one_obs_chnk |>
#   mutate(exp_factor = cut(exp_sp_total,
#                          breaks = c(0, 1, 3, 5, 10, max(one_obs_chnk$exp_sp_total) + 1),
#                          right = T,
#                          include.lowest = T,
#                          ordered_result = F)) |>
#   select(exp_sp_total, exp_factor, ends_with('error')) |>
#   pivot_longer(ends_with("error"),
#                names_to = "type",
#                values_to = "error") |>
#   mutate(
#     across(
#       type,
#       str_remove,
#       "_error$"
#     ),
#     across(
#       type,
#       fct_relevel,
#       "net",
#       after = Inf
#     )
#   ) |>
#   ggplot(aes(x = exp_sp_total,
#              y = error,
#              color = exp_factor)) +
#   geom_point() +
#   geom_smooth(method = lm,
#               formula = y ~ 1,
#               fullrange = T,
#               se = F) +
#   scale_color_brewer(palette = 'Set1') +
#   facet_wrap(~ type,
#              scales = 'free') +
#   theme_bw() +
#   labs(x = "Total Spawning Ground Experience",
#        y = "Error",
#        color = "Experience\nFactor")


# looks like less than 1 full season makes a difference, after that it's pretty similar
exp_thres = 1

one_obs_chnk %<>%
  mutate(exp_factor = cut(exp_sp_total,
                         breaks = c(0, exp_thres, max(one_obs_chnk$exp_sp_total) + 1),
                         labels = c('Rookie',
                                    'Experienced'),
                         right = T,
                         include.lowest = T,
                         ordered_result = F)) |>
  mutate(
    across(
      exp_factor,
      fct_rev)) |>
  relocate(exp_factor,
           .after = "exp_sp_total_log")

# normalize the numeric covariates
chnk_covar_center <- one_obs_chnk |>
  select(exp_sp_total:exp_sp_total_log,
         mean_survey_frequency:naive_density_km) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "values"
  ) |>
  group_by(metric) |>
  summarize(
    across(
      values,
      list(mean = mean,
           sd = sd),
      na.rm = TRUE,
      .names = "{.fn}"
    ),
    .groups = "drop")

chnk_mod_df = one_obs_chnk |>
  mutate(id = 1:n()) %>%
  select(id,
         year:surveyor,
         total_features,
         visible_redds,
         exp_factor,
         net_error,
         mean_thalweg_cv,
         naive_density_km) |>
  pivot_longer(cols = -c(id:net_error),
               names_to = "metric",
               values_to = "value") |>
  left_join(chnk_covar_center) |>
  mutate(across(value,
                ~ (. - mean) / sd)) |>
  select(-c(mean:sd)) |>
  pivot_wider(names_from = metric,
              values_from = value)

chnk_net_mod <- glm(net_error ~ exp_factor + mean_thalweg_cv + naive_density_km,
                    data = chnk_mod_df,
                    family = gaussian)

#-----------------------------------------------------------------
# save to be available in package (e.g. to compare range of covariates)
usethis::use_data(one_obs_sthd,
                  overwrite = T)
usethis::use_data(two_obs_sthd,
                  overwrite = T)
usethis::use_data(one_obs_chnk,
                  overwrite = T)

# save for use in package functions
usethis::use_data(one_obs_covar_center,
                  one_obs_net_mod,
                  two_obs_covar_center,
                  two_obs_net_mod,
                  chnk_covar_center,
                  chnk_net_mod,
                  internal = T,
                  overwrite = T,
                  version = 2)

