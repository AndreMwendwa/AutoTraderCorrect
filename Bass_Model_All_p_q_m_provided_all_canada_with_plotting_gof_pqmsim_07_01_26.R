library(dplyr)
library(readr)
library(ggplot2)
library(MASS)       # for mvrnorm()

set.seed(123)

# Info for files to be saved
date <- "10_01_26"

# Column name parameters
time_var  <- "months_since_start"
sales_var <- "VALUE"

# Folder to save plots and sims
output_folder <- paste0("bass_all_canada_m_estimated_", date)
if (!dir.exists(output_folder)) dir.create(output_folder)

# File names
txt_file_name <- paste0("results_bass_all_canada_m_estimated_", date, ".txt")
csv_name      <- paste0("results_bass_all_canada_m_estimated_", date, ".csv")

# Read input data
bass_input   <- read_csv("Revision_2\\canada_bev_data_train.csv")
start_params <- read_csv("best_parameter_start_values_reg_data_bass.csv")
# m_value    <- 38646      

# Bass model function
c_t <- function(x, p, q, m) {
  num   <- 1 - (p + q)
  denom <- 1 + (p + q)
  m * (
    ((1 - ((num/denom)^(x + 1))/2) / (1 + (q/p) * ((num/denom)^(x + 1))/2)) -
      ((1 - ((num/denom)^(x - 1))/2) / (1 + (q/p) * ((num/denom)^(x - 1))/2))
  )
}

# Initialize results DataFrame
results_df <- tibble(
  ZoneID = character(),
  p      = double(), p_se = double(), p_t = double(),
  q      = double(), q_se = double(), q_t = double(),
  m      = double(), m_se = double(), m_t = double(),
  R2     = double(),
  RMSE   = double()
)

input_data <- bass_input %>%
  filter(.data[[time_var]] > 0)

input_data[[time_var]] <- input_data[[time_var]] +
  abs(min(input_data[[time_var]]))

p_start <- start_params %>% pull(p)
q_start <- start_params %>% pull(q)
m_start <- start_params %>% pull(m)

# 1) Fit all three parameters p, q, and m
fit <- tryCatch(
  nls(
    formula   = as.formula(
      paste0("`", sales_var, "` ~ c_t(", time_var, ", p, q, m)")
    ),
    data      = input_data,
    start     = list(p = p_start, q = q_start, m = m_start),
    algorithm = "port",
    control   = nls.control(maxiter = 1e8, minFactor = 1e-4)
  ),
  error = function(e) {
    message("nls failed: ", e$message)
    NULL
  }
)

# Save summary
txt_conn <- file(txt_file_name, open = "wt")
fit_sum  <- summary(fit)
capture.output(fit_sum, file = txt_conn, append = TRUE)
close(txt_conn)

# 2) compute R2 & RMSE
observed  <- input_data[[sales_var]]
predicted <- predict(fit, newdata = input_data)
sse       <- sum((observed - predicted)^2)
sst       <- sum((observed - mean(observed))^2)
r2        <- 1 - sse / sst
rmse      <- sqrt(mean((observed - predicted)^2))

# 3) extract coefficients including m
coefs <- fit_sum$coefficients
results_df <- results_df %>% add_row(
  p      = coefs["p","Estimate"],
  p_se   = coefs["p","Std. Error"],
  p_t    = coefs["p","t value"],
  q      = coefs["q","Estimate"],
  q_se   = coefs["q","Std. Error"],
  q_t    = coefs["q","t value"],
  m      = coefs["m","Estimate"],
  m_se   = coefs["m","Std. Error"],
  m_t    = coefs["m","t value"],
  R2     = r2,
  RMSE   = rmse
)

# 4) simulate 1,000 draws for p, q, and m
beta_hat <- coef(fit)[c("p","q","m")]
Sigma    <- vcov(fit)[c("p","q","m"), c("p","q","m")]
param_sims <- MASS::mvrnorm(n = 1000, mu = beta_hat, Sigma = Sigma)
colnames(param_sims) <- c("p","q","m")

sim_df <- as_tibble(param_sims)
write_csv(
  sim_df,
  file.path(output_folder, "all_canada_param_sims.csv")
)

# 5) plot observed vs predicted
plot_df <- input_data %>% mutate(predicted = predicted)

p_plot <- ggplot(plot_df, aes(.data[[time_var]])) +
  geom_line(aes(y = .data[[sales_var]], color = "Observed")) +
  geom_line(aes(y = predicted,          color = "Predicted")) +
  labs(
    title = "ZoneID: All Canada",
    x     = "Months since Jan 2021",
    y     = "Adoption"
  ) +
  theme_minimal() +
  scale_color_manual("", values = c("Observed"="black","Predicted"="red"))

ggsave(
  filename = file.path(output_folder, "all_canada.png"),
  plot     = p_plot,
  width    = 6,
  height   = 4
)

# 6) write overall summary
write_csv(results_df, csv_name)
