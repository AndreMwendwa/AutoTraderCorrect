library(tidyverse)
library(MASS)
library("geoR")
library(car)
library(readxl)
library(forcats)

# Read params and census variables file
params_variables_census <- read_csv('.\\Revision_2\\params_variables_old_05_01_26_sales.csv') # Much better performance than new zones
weather_data <- read_csv('weather_vars_zones.csv')
gas_station_counts <- read_csv('gas_stations_count_old_zones.csv')
province <- read_csv('province.csv')
cma_summarized <- read_csv('cma_summarized.csv')
charging_stns_counts <- read_csv('charging_stations_count_zones_old.csv')
oldzone_access <- read_csv('oldzone_access.csv')
land_use <- read_delim('LU_Areas_on_NewZones.csv', delim = '\t')
incentives_regression_input <- read_csv('incentives_regression_input_grouped_25_07.csv')
charging_stns_new_input <- read_csv('charging_stations_per_area.csv')
search_distances <- read_csv("EV_Zonal_Search_Distances_Summary.csv")

# Renaming zones
search_distances <- search_distances |> 
  mutate(
    ZoneID = str_replace_all(ZoneID, "BC", "BritishColumbia"),
    ZoneID = str_replace_all(ZoneID, "ON", "Ontario"),
    ZoneID = str_replace_all(ZoneID, "QC", "Quebec")
  )

params_variables_census <- inner_join(params_variables_census, weather_data) 
params_variables_census <- inner_join(params_variables_census, province)
params_variables_census <- inner_join(params_variables_census, gas_station_counts)
params_variables_census <- inner_join(params_variables_census, cma_summarized)
params_variables_census <- inner_join(params_variables_census, charging_stns_counts)
params_variables_census <- inner_join(params_variables_census, oldzone_access)
params_variables_census <- inner_join(params_variables_census, land_use)
params_variables_census <- inner_join(params_variables_census, incentives_regression_input)
params_variables_census <- inner_join(params_variables_census, charging_stns_new_input)
params_variables_census <- inner_join(params_variables_census, search_distances)


# ## File for storing results
txt_conn <- file("bass_model_explanatory_vars_07_01_26.txt", open = "wt")
date <- "_06_01_26.csv"


# Initial log-normal regression
params_variables_census <- params_variables_census |> 
  filter(q > 0) |> 
  mutate(
    logp = log(p + 1E-9),
    logq = log(q + 1E-9), 
    prop_engineers = COL38 / COL35, 
    prop_arts = COL41 / COL35,
    prop_agric = COL45 / COL35,
    PR_factor = as.factor(Province),
    PR_factor = fct_relevel(PR_factor, "BritishColumbia"),
    pr_BC = if_else(Province == 'BritishColumbia', 1, 0), 
    pr_ON = if_else(Province == 'Ontario', 1, 0),
    pr_QC = if_else(Province == 'Quebec', 1, 0), 
    pr_NS = if_else(Province == "NovaScotia", 1, 0),
    inc_BC = if_else(Province == 'BritishColumbia', COL5, 0), 
    inc_ON = if_else(Province == 'Ontario', COL5, 0), 
    inc_QC = if_else(Province == 'Quebec', COL5, 0), 
    gas_stations_sufficient = if_else(gas_stations_count  > 20, 1, 0), 
    gas_stations_per_capita = gas_stations_count / COL31, 
    gas_stations_per_sqkm = gas_stations_count / COL32, 
    charging_stations_per_capita = charging_stations_count / COL31, 
    charging_stations_per_sqkm = charging_stations_count / COL31, 
    median_zonal_age = COL33,
    residential_commercial_ratio = Residentia / Commercial,
    residential_open_ratio = Residentia / OpenArea_O,
    commercial_open_ratio = Commercial / OpenArea_O
  )

# write.csv2(params_variables_census, "all_vars.csv")

## equation vars
p_vars <- c(
  "median_zonal_age",
  "avg_distance_weighted"
  #, "COL5"    # income: vif too high with this
  # , "popn_density"  # vif too high with this
  # , "PR_factor"   # all provinces not significant
  # , "MEAN_TEMPERATURE"   # min temp performs slightly better
  , "MIN_TEMPERATURE"
  # , "charging_stations_per_capita"  # not significant, high VIF
  # , "charging_stations_per_sqkm"   # not significant, high VIF
  # , "gas_stations_count"   # high VIF
  # , "LANDAREA"    # just a control, wasn't meant as a serious variable
  # , "weighted_avg_subsidy"  # high VIF
  # , "prop_engineers"    # wrong sign, high VIF
  # , "prop_agric"    # wrong sign, high VIF
  # , "prop_arts"   # wrong sign, high VIF
  # , "pct_condominium"   # wrong sign, high VIF
  # , "pct_new"    # high VIF
  # , "Residentia"  # high VIF
  # , "OpenArea_O"  # high VIF
  # , "Government"  # high VIF
  # , "Commercial"  # high VIF
  # , "Access"   # Accessibility to charging stations: calculated by Billie   # high VIF, wrong sign
)
q_vars <- c(
  "PR_factor"
  ,"Charging_stations"    # Number of public charging stations
  # , "charging_stations_count"   # Number of charging stns in general: not significant 
  ,"avg_distance_weighted"
  # , "COL5"    # income: not significant
  # , "popn_density"   # not significant
  # , "MEAN_TEMPERATURE"   # min temp performs slightly better
  , "MIN_TEMPERATURE"
  # , "charging_stations_per_capita"   # not significant
  # , "charging_stations_per_sqkm"   # not significant
  # , "gas_stations_per_capita"   # not significant
  # , "gas_stations_per_sqkm"  # not significant
  , "gas_stations_count"
  # , "LANDAREA"   # just a control, wasn't meant as a serious variable
  # , "Area"  # just a control, wasn't meant as a serious variable
  # , "weighted_avg_subsidy"  # wrong sign
  # , "prop_engineers"    # not significant
  # , "prop_agric"   # not significant
  # , "prop_arts"   # not significant
  # , "pct_condominium"   # not significant
   # , "pct_new"   # not significant
  # , "Residentia" # not significant
  # , "OpenArea_O"  # not significant
  # , "Government"   # makes gas station count less significant, but I can't explain it as 
  # well as gas station count
  # , "Commercial"  # not significant
  # , "Access"   # Accessibility to charging stations: calculated by Billie  # not significant
)
common_rhs_p <- paste(p_vars, collapse = " + ")
common_rhs_q <- paste(q_vars, collapse = " + ")
form_p_bc  <- as.formula(paste("p ~", common_rhs_p))
form_q_bc  <- as.formula(paste("q ~", common_rhs_q))
form_p_lm  <- as.formula(paste("p_tr ~", common_rhs_p))
form_q_lm  <- as.formula(paste("q_tr ~", common_rhs_q))

# Boxcox
bcp <- boxcox(form_p_bc, data = params_variables_census)
(lambda_p <- bcp$x[which.max(bcp$y)])

bcq <- boxcox(form_q_bc, data = params_variables_census)
(lambda_q <- bcq$x[which.max(bcq$y)])

## Mutate variables and define weights
params_variables_census <- params_variables_census |> 
  mutate(
    p_tr = (p ^ lambda_p - 1) / lambda_p,
    q_tr = (q ^ lambda_q - 1) / lambda_q,
    weights_p = 1 / p_se,
    weights_q = 1 / q_se, 
    log_weights_p = log(weights_p)
  )

# Initial regression 
weighted_reg_p <- lm(
  form_p_lm,
  data = params_variables_census,
  weights = weights_p
)
summary(weighted_reg_p)

vif(weighted_reg_p)

# Log lin version q
weighted_reg_q <- lm(
  form_q_lm,
  weights = weights_q,
  data = params_variables_census
)
summary(weighted_reg_q)

vif(weighted_reg_q)

# --- Save VIFs (readable) ---
vif_p <- vif(weighted_reg_p)
cat("VIFs (weighted_reg_p):\n", file = txt_conn)
if (is.matrix(vif_p)) {
  vif_p_df <- as.data.frame(vif_p) |> tibble::rownames_to_column("term")
  vif_p_out <- capture.output(print(vif_p_df, row.names = FALSE))
} else {
  vif_p_df <- tibble(term = names(vif_p), VIF = as.numeric(vif_p)) |> arrange(desc(VIF))
  vif_p_out <- capture.output(print(vif_p_df, n = Inf))
}
cat(paste(vif_p_out, collapse = "\n"), "\n\n", file = txt_conn)
# ----------------------------



# Write summary
p_summary <- summary(weighted_reg_p)
capture_output <- capture.output(print(p_summary))
cat(paste(capture_output, collapse = "\n"), "\n\n", file = txt_conn)

# Save coeffs to csv
coef_mat_p <- p_summary$coefficients
write.csv(coef_mat_p, paste0("p_regression_weighted_results", date))




# --- Save VIFs (readable) ---
vif_q <- vif(weighted_reg_q)
cat("VIFs (weighted_reg_q):\n", file = txt_conn)
if (is.matrix(vif_q)) {
  vif_q_df <- as.data.frame(vif_q) |> tibble::rownames_to_column("term")
  vif_q_out <- capture.output(print(vif_q_df, row.names = FALSE))
} else {
  vif_q_df <- tibble(term = names(vif_q), VIF = as.numeric(vif_q)) |> arrange(desc(VIF))
  vif_q_out <- capture.output(print(vif_q_df, n = Inf))
}
cat(paste(vif_q_out, collapse = "\n"), "\n\n", file = txt_conn)
# ----------------------------

# Write summary
q_summary <- summary(weighted_reg_q)
capture_output <- capture.output(print(q_summary))
cat(paste(capture_output, collapse = "\n"), "\n\n", file = txt_conn)

# Save coeffs to csv
coef_mat_q <- q_summary$coefficients
write.csv(coef_mat_q, paste0("q_regression_weighted_results", date))


# # Close the text file connection
close(txt_conn)
