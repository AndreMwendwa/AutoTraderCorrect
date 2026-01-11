library(dplyr)
library(readr)
library(ggplot2)
library(MASS)       # for mvrnorm()

# Column name parameters
time_var  <- "months_since_start"
sales_var <- "VALUE"

# 0) Set up folders and file names
# Info for files to be saved
date <- "10_01_26"

# 0) Set up folders and file names
output_folder <- paste0("gomp_all_canada_m_estimated_", date)
if (!dir.exists(output_folder)) dir.create(output_folder)

# File names
txt_file_name <- paste0("results_gomp_all_canada_m_estimated_", date, ".txt")
csv_name      <- paste0("results_gomp_all_canada_m_estimated_", date, ".csv")

# 1) Read input data
bass_input <- read_csv("Revision_2\\canada_bev_data_train.csv")
start_params <- read_csv("best_parameter_start_values_reg_data_gomp.csv")

# 2) Define the gompertz cumulative‐adoption function
gompertz <- function(t, a, b, c) {
  a * exp(-c * exp(-b * t))
}

# 3) Prepare a tibble to collect results
results_df <- tibble(
  ZoneID = character(),
  a      = double(), a_se = double(), a_t = double(),
  b      = double(), b_se = double(), b_t = double(),
  c      = double(), c_se = double(), c_t = double(),
  R2     = double(),
  RMSE   = double()
)

# 4) Filter out t = 0 (still keep cumulative col for fitting)
input_data <- bass_input %>%
  filter((.data[[time_var]] > 0) | (.data[[sales_var]] > 0)) |> 
  mutate(cumsum_21_24 = cumsum(.data[[sales_var]]))

input_data[[time_var]] = input_data[[time_var]] + abs(min(input_data[[time_var]]))

# 5) Fit the three parameters a, b, and c
fit <- tryCatch(
  nls(
    formula   = as.formula(
      paste0("cumsum_21_24 ~ gompertz(", time_var, ", a, b, c)")
    ),
    data      = input_data,
    start     = list(
      a = 1335324,
      b = 0.02,
      c = 6.41
    ),
    algorithm = "port",
    control   = nls.control(maxiter = 1e7, minFactor = 1e-4)
  ),
  error = function(e) {
    message("nls failed: ", e$message)
    NULL
  }
)

# 6) Save the fit summary
if (!is.null(fit)) {
  txt_conn <- file(txt_file_name, open = "wt")
  fit_sum  <- summary(fit)
  capture.output(fit_sum, file = txt_conn, append = TRUE)
  close(txt_conn)
}

# 7) Compute R² and RMSE using period values (ignore first period)
if (!is.null(fit)) {
  predicted_cum <- predict(fit, newdata = input_data)
  predicted     <- c(predicted_cum[1], diff(predicted_cum))  # per-period
  observed      <- input_data[[sales_var]]                    # per-period actual
  
  # Ignore first period
  predicted <- predicted[-1]
  observed  <- observed[-1]
  
  sse  <- sum((observed - predicted)^2)
  sst  <- sum((observed - mean(observed))^2)
  r2   <- 1 - sse / sst
  rmse <- sqrt(mean((observed - predicted)^2))
}

# 8) Extract coefficients and append to results_df
if (!is.null(fit)) {
  coefs <- summary(fit)$coefficients
  results_df <- results_df %>% add_row(
    a    = coefs["a","Estimate"],
    a_se = coefs["a","Std. Error"],
    a_t  = coefs["a","t value"],
    b    = coefs["b","Estimate"],
    b_se = coefs["b","Std. Error"],
    b_t  = coefs["b","t value"],
    c    = coefs["c","Estimate"],
    c_se = coefs["c","Std. Error"],
    c_t  = coefs["c","t value"],
    R2   = r2,
    RMSE = rmse
  )
}

# 9) Simulate 1,000 draws
if (!is.null(fit)) {
  beta_hat   <- coef(fit)[c("a","b","c")]
  Sigma      <- vcov(fit)[c("a","b","c"), c("a","b","c")]
  param_sims <- MASS::mvrnorm(n = 1000, mu = beta_hat, Sigma = Sigma)
  colnames(param_sims) <- c("a","b","c")
  sim_df <- as_tibble(param_sims)
  write_csv(sim_df,
            file.path(output_folder, "all_canada_param_sims_gompertz.csv"))
}

# 10) Plot observed vs. fitted (per-period, ignore first period)
if (!is.null(fit)) {
  plot_df <- input_data %>% 
    mutate(predicted = c(predicted_cum[1], diff(predicted_cum))) %>%
    slice(-1)  # remove first period
  
  p_plot <- ggplot(plot_df, aes(x = .data[[time_var]])) +
    geom_line(aes(y = .data[[sales_var]], color = "Observed")) +
    geom_line(aes(y = predicted,    color = "Fitted")) +
    labs(
      title = "All Canada: gompertz Fit (Per-Period, excluding first)",
      x     = "Months since Jan 2021",
      y     = "Per-Period Adoption"
    ) +
    theme_minimal() +
    scale_color_manual("", values = c(
      Observed = "black",
      Fitted   = "blue"
    ))
  ggsave(
    filename = file.path(output_folder, "all_canada_gompertz.png"),
    plot     = p_plot,
    width    = 6, height = 4
  )
}

# 11) Write the summary CSV
write_csv(results_df, csv_name)
