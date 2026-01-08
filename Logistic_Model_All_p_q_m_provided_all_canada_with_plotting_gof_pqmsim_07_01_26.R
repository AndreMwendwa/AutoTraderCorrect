library(dplyr)
library(readr)
library(ggplot2)
library(MASS)       # for mvrnorm()

set.seed(123)

# 0) Set up folders and file names
output_folder <- "all_canada_logistic_07_01_2026"
if (!dir.exists(output_folder)) dir.create(output_folder)

txt_file_name <- "results_txt_logistic_all_canada_07_01_2026.txt"
csv_name      <- "results_csv_logistic_all_canada_07_01_2026.csv"

# 1) Read input data
bass_input <- read_csv("Revision_2\\bass_input_all_canada_new_used_with_tesla.csv")
start_params <- read_csv("best_parameter_start_values_all_canada_logistic.csv")

# 2) Define the logistic cumulative‐adoption function
logistic <- function(t, a, b, c) {
  # browser()
  a / (1 + c * exp(-b * t))
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

# 4) Filter out t = 0
input_data <- bass_input %>%
  filter((months_passed_01_2021 > 0) | (`2021-2024_Total_EV_Sales` > 0)) |> 
  mutate(cumsum_21_24 = cumsum(`2021-2024_Total_EV_Sales`))

input_data$months_passed_01_2021 = input_data$months_passed_01_2021 + abs(min(input_data$months_passed_01_2021))

a_start <- start_params %>% pull(a)
b_start <- start_params %>% pull(b)
c_start <- start_params |> pull(c)

# 5) Fit the three parameters a, b, and c
fit <- tryCatch(
  nls(
    formula   = cumsum_21_24 ~ logistic(months_passed_01_2021, a, b, c),
    data      = input_data,
    start     = list(
      a = a_start,  
      b = b_start,    
      c = c_start      
    ),
    algorithm = "port",
    control   = nls.control(maxiter = as.integer(1e8), minFactor = 1e-4)
  ),
  error = function(e) {
    message("nls failed: ", e$message)
    NULL
  }
)

# 6) Save the fit summary to text
if (!is.null(fit)) {
  txt_conn <- file(txt_file_name, open = "wt")
  fit_sum  <- summary(fit)
  capture.output(fit_sum, file = txt_conn, append = TRUE)
  close(txt_conn)
}

# 7) Compute R² and RMSE using per-period values, ignoring first period
if (!is.null(fit)) {
  predicted_cum <- predict(fit, newdata = input_data)
  # browser()
  # write.csv(predicted, "log_predictions_05_01_2026_b.csv")
  predicted     <- c(predicted_cum[1], diff(predicted_cum))  # per-period
  observed      <- input_data$`2021-2024_Total_EV_Sales`                    # per-period actual
  
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
            file.path(output_folder, "all_canada_param_sims_logistic.csv"))
}

# 10) Plot observed vs. fitted (per-period, ignoring first period)
if (!is.null(fit)) {
  plot_df <- input_data %>%
    mutate(predicted = c(predicted_cum[1], diff(predicted_cum))) %>%
    slice(-1)  # drop first period
  
  p_plot <- ggplot(plot_df, aes(x = months_passed_01_2021)) +
    geom_line(aes(y = `2021-2024_Total_EV_Sales`, color = "Observed")) +
    geom_line(aes(y = predicted,    color = "Fitted")) +
    labs(
      title = "All Canada: Logistic Fit (Per-Period, excluding first)",
      x     = "Months since Jan 2021",
      y     = "Per-Period Adoption"
    ) +
    theme_minimal() +
    scale_color_manual("", values = c(
      Observed = "black",
      Fitted   = "blue"
    ))
  ggsave(
    filename = file.path(output_folder, "all_canada_logistic.png"),
    plot     = p_plot,
    width    = 6, height = 4
  )
}

# 11) Write the overall summary CSV
write_csv(results_df, csv_name)
