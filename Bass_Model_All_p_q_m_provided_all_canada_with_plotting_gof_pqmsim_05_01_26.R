library(dplyr)
library(readr)
library(ggplot2)
library(MASS)       # for mvrnorm()

# Folder to save plots and sims
output_folder <- "bass_all_canada_m_estimated_05_06_26"
if (!dir.exists(output_folder)) dir.create(output_folder)

# File names
txt_file_name <- "results_bass_all_canada_m_estimated_05_06_26.txt"
csv_name      <- "results_bass_all_canada_m_estimated_05_06_26.csv"

# Read input data
bass_input <- read_csv("Revision_2\\bass_input_all_canada_new_with_tesla.csv")
start_params <- read_csv("best_parameter_start_values_all_canada.csv")
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
  filter((months_passed_01_2021 > 0) | (`2021-2024_Total_EV_Sales` > 0))

input_data$months_passed_01_2021 = input_data$months_passed_01_2021 + abs(min(input_data$months_passed_01_2021))

p_start <- start_params %>% pull(p)
q_start <- start_params %>% pull(q)
m_start <- start_params |> pull(m)

# 1) Fit all three parameters p, q, and m
fit <- tryCatch(
  nls(
    formula   = `2021-2024_Total_EV_Sales` ~ c_t(months_passed_01_2021, p, q, m),
    data      = input_data,
    start     = list(p = p_start, q = q_start, m = 100000),
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
fit_sum <- summary(fit)
capture.output(fit_sum, file = txt_conn, append = TRUE)
close(txt_conn)

# 2) compute R2 & RMSE
observed  <- input_data$`2021-2024_Total_EV_Sales`
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
write_csv(sim_df,
          file.path(output_folder, "all_canada_param_sims_05_01_26.csv"))

# 5) plot observed vs predicted
plot_df <- input_data %>% mutate(predicted = predicted)
p_plot <- ggplot(plot_df, aes(months_passed_01_2021)) +
  geom_line(aes(y = `2021-2024`, color = "Observed")) +
  geom_line(aes(y = predicted,    color = "Predicted")) +
  labs(title = "ZoneID: All Canada",
       x = "Months since Jan 2021",
       y = "Adoption") +
  theme_minimal() +
  scale_color_manual("", values = c("Observed"="black","Predicted"="red"))

ggsave(
  filename = file.path(output_folder, "all_canada.png"),
  plot     = p_plot, width = 6, height = 4
)

# 6) write overall summary
write_csv(results_df, csv_name)
