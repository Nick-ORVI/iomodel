# Output calibration factors from the Economic Census.
#
# BEA publishes GDP by state and industry but not output (sales), so the
# model starts from output = state GDP x the national output/GDP ratio.
# That makes every state's share of national output equal its share of
# national GDP. The Economic Census (every 5 years) reports actual sales by
# state and industry. Where they disagree, the state's industry has a
# different output/GDP ratio than the nation, e.g. a chemical industry that
# is mostly high-margin pharmaceuticals.
#
# Factors are measured in the census year (GDP shares for that year vs
# census shares), so price swings between years cancel out, then applied to
# the model year's output:
#
#   factor = (census share / GDP share), renormalized so each industry's
#            national total is unchanged, and bounded by calibration_bounds
#
# Cells the census doesn't cover completely keep factor = 1.

# Industries the Economic Census covers poorly or not at all
ec_not_comparable <- c(
  "111CA", "113FF",             # crop and animal production, forestry & fishing: out of scope
  "482", "525", "55",           # rail, funds & trusts, management of companies
  "61", "HS",                   # most schools and colleges out of scope; imputed housing
  "GFGD", "GFGN", "GFE", "GSLG", "GSLE"
)

read_economic_census <- function(ec_year) {
  ec <- read_csv(file.path(raw_dir, sprintf("economic_census_%d.csv", ec_year)),
                 col_types = cols(.default = col_character())) |>
    transmute(state = if_else(state == "US", "US",
                              names(state_fips)[match(paste0(state, "000"), state_fips)]),
              naics = NAICS2022,
              receipts = suppressWarnings(as.numeric(RCPTOT)) / 1e3) |>   # $M
    filter(!is.na(state), !is.na(receipts), receipts > 0) |>
    inner_join(naics_to_bea, by = "naics")

  # A state's total is "complete" if the NAICS pieces it publishes make up at
  # least 90% of the industry's national receipts
  us_components <- ec |> filter(state == "US") |> select(naics, bea, us_receipts = receipts)
  ec |>
    filter(state != "US") |>
    left_join(us_components, by = c("naics", "bea")) |>
    group_by(state, bea) |>
    summarise(receipts = sum(receipts), covered = sum(us_receipts), .groups = "drop") |>
    left_join(us_components |> group_by(bea) |> summarise(us_receipts = sum(us_receipts)), by = "bea") |>
    filter(covered / us_receipts >= 0.9, !bea %in% ec_not_comparable) |>
    select(state, bea, census_receipts = receipts)
}

# va_census_year: tibble(state, bea, va) for the census year
output_factors <- function(va_census_year, ec_year, bounds = calibration_bounds) {
  read_economic_census(ec_year) |>
    inner_join(va_census_year, by = c("state", "bea")) |>
    filter(va > 0) |>
    group_by(bea) |>
    mutate(census_share = census_receipts / sum(census_receipts),
           gdp_share    = va / sum(va),
           share_ratio  = gdp_share / census_share,        # >1: model output too high
           factor       = pmin(pmax(census_share / gdp_share, bounds[1]), bounds[2]),
           # keep the covered states' combined output unchanged
           factor       = factor * sum(va) / sum(va * factor)) |>
    ungroup() |>
    select(state, bea, census_share, gdp_share, share_ratio, factor)
}
