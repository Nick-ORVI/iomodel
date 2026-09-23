# Report how the model's estimated state output compares with the Economic
# Census, before and after calibration (see R/calibrate_output.R).
#
#   share_ratio_before  model (GDP-based) share / Census share, census year
#   share_ratio_after   same, after the calibration factor
#   1.00 = agreement; 1.20 = model output 20% too high relative to Census
#
# Writes output/output_check_<year>.csv.

source("R/00_config.R")

national   <- readRDS(file.path(model_dir, "national.rds"))
state_data <- readRDS(file.path(model_dir, "state_data.rds"))
factors_file <- file.path(model_dir, "output_factors.rds")
if (!file.exists(factors_file)) stop("No calibration factors; set calibrate_output <- TRUE and rerun 03_state_data.R")

check <- readRDS(factors_file) |>
  group_by(bea) |>
  mutate(share_after = gdp_share * factor / sum(gdp_share * factor)) |>
  ungroup() |>
  left_join(select(state_data, state, bea, output_unadjusted, output), by = c("state", "bea")) |>
  transmute(state, bea, industry = unname(national$industry_names[bea]),
            census_share, share_ratio_before = share_ratio,
            factor, share_ratio_after = share_after / census_share,
            output_unadjusted, output_adjusted = output) |>
  arrange(state, bea)

write_csv(check, file.path(out_dir, sprintf("output_check_%d.csv", year)))

within <- \(r, lo, hi) sum(check$output_unadjusted[r >= lo & r <= hi]) / sum(check$output_unadjusted)
message(sprintf(paste0(
  "Output vs Economic Census, %d state x industry cells (share of output within range)\n",
  "  before calibration: %.0f%% within +/-10%%, %.0f%% within +/-25%%\n",
  "  after calibration:  %.0f%% within +/-10%%, %.0f%% within +/-25%%\n",
  "  %d cells hit the calibration bounds"),
  nrow(check),
  100 * within(check$share_ratio_before, 1 / 1.1, 1.1), 100 * within(check$share_ratio_before, 0.8, 1.25),
  100 * within(check$share_ratio_after,  1 / 1.1, 1.1), 100 * within(check$share_ratio_after,  0.8, 1.25),
  sum(check$factor <= calibration_bounds[1] * 1.001 | check$factor >= calibration_bounds[2] * 0.999)))
