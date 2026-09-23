# State economic accounts by BEA summary industry, for every state + DC:
#   jobs          QCEW annual average employment (wage & salary, covered)
#   va, comp      BEA SAGDP2 GDP and SAGDP4 compensation
#   output        va scaled by the national output / value-added ratio
#   spend_ratio   share of an added $ of labor income spent on PCE
#   prop_jobs, prop_income   self-employed jobs and proprietors' income
# Produces data/models/state_data.rds.
#
# Both sources suppress cells to protect confidentiality. Suppressed cells
# are filled with what is left of their published parent total, split in
# proportion to a proxy (national employment for QCEW; state employment
# times national value added per job for BEA).

source("R/00_config.R")
source("R/mappings.R")

national <- readRDS(file.path(model_dir, "national.rds"))
ind      <- national$industries

# Split what is left of `parent` among the missing children by `weight`.
fill_residual <- function(value, missing, parent, weight) {
  if (!any(missing)) return(value)
  residual <- max(parent - sum(value[!missing]), 0)
  w <- weight[missing]
  w <- if (sum(w) > 0) w / sum(w) else rep(1 / sum(missing), sum(missing))
  value[missing] <- residual * w
  value
}

# ---- QCEW jobs ---------------------------------------------------------------

read_qcew <- function(fips) {
  read_csv(file.path(raw_dir, "qcew", year, paste0(fips, ".csv")),
           col_types = cols(.default = col_character())) |>
    transmute(own_code, industry_code, agglvl_code,
              emp        = as.numeric(annual_avg_emplvl),
              suppressed = disclosure_code %in% "N")
}

us_qcew <- read_qcew("US000")
# National private employment for each code, used as allocation weights
us_weight <- us_qcew |>
  filter(own_code == "5") |>
  select(industry_code, us_emp = emp) |>
  distinct(industry_code, .keep_all = TRUE)

naics4_parents <- c("336", "541")

state_jobs <- function(fips) {
  q <- read_qcew(fips)
  priv <- q |> filter(own_code == "5") |> left_join(us_weight, by = "industry_code")
  total_private <- priv$emp[priv$agglvl_code == "51" & priv$industry_code == "10"]

  # 1. sectors within total private
  sectors <- priv |> filter(agglvl_code == "54")
  sectors$emp <- fill_residual(sectors$emp, sectors$suppressed, total_private, sectors$us_emp)

  # 2. 3-digit subsectors within each sector
  sub <- priv |>
    filter(agglvl_code == "55") |>
    mutate(sector = naics_sector(industry_code)) |>
    group_by(sector) |>
    group_modify(\(d, k) {
      parent <- sectors$emp[sectors$industry_code == k$sector]
      if (length(parent) == 0) return(d)
      d$emp <- fill_residual(d$emp, d$suppressed, parent, d$us_emp)
      d
    }) |>
    ungroup()

  # 3. 4-digit detail where a BEA industry splits a 3-digit code
  four <- priv |>
    filter(agglvl_code == "56", substr(industry_code, 1, 3) %in% naics4_parents) |>
    mutate(parent3 = substr(industry_code, 1, 3)) |>
    group_by(parent3) |>
    group_modify(\(d, k) {
      d$emp <- fill_residual(d$emp, d$suppressed, sub$emp[sub$industry_code == k$parent3], d$us_emp)
      d
    }) |>
    ungroup()

  private <- bind_rows(sub |> filter(!industry_code %in% naics4_parents), four) |>
    inner_join(naics_to_bea, by = c("industry_code" = "naics")) |>
    group_by(bea) |>
    summarise(jobs = sum(emp), .groups = "drop")

  # Government: postal service -> federal enterprises; state/local utilities,
  # transit, and transportation support -> S&L enterprises; the rest general
  # government. Federal defense civilians are split out of GFGN below; QCEW
  # has no uniformed military.
  gov <- q |> filter(own_code %in% c("1", "2", "3"))
  gov_total <- \(own) sum(gov$emp[gov$own_code %in% own & gov$agglvl_code == "51"])
  gov_sub   <- \(own, codes) sum(gov$emp[gov$own_code %in% own & gov$agglvl_code == "55" &
                                           gov$industry_code %in% codes])
  gfe  <- gov_sub("1", "491")
  gsle <- gov_sub(c("2", "3"), c("221", "485", "488"))
  government <- tibble(
    bea  = c("GFGD", "GFGN", "GFE", "GSLG", "GSLE"),
    jobs = c(0, gov_total("1") - gfe, gfe, gov_total(c("2", "3")) - gsle, gsle)
  )

  tibble(bea = ind) |>
    left_join(bind_rows(private, government), by = "bea") |>
    mutate(jobs = coalesce(jobs, 0))
}

message("Reading QCEW for ", length(state_fips), " states")
jobs <- imap(state_fips, \(fips, st) state_jobs(fips) |> mutate(state = st)) |> list_rbind()
us_jobs <- jobs |> group_by(bea) |> summarise(us_jobs = sum(jobs), .groups = "drop")

# ---- BEA state GDP and compensation -----------------------------------------

# BEA regional CSVs end with footnote lines; drop them quietly.
read_bea_regional <- function(table) {
  # trim_ws = FALSE keeps the Description indentation that encodes the
  # industry hierarchy; GeoFIPS then arrives as ` "42000"`, so clean it.
  suppressWarnings(read_csv(file.path(raw_dir, paste0(table, ".csv")), trim_ws = FALSE,
                            col_types = cols(.default = col_character()))) |>
    mutate(across(-Description, \(x) str_remove_all(str_trim(x), '"'))) |>
    filter(!is.na(LineCode))
}

# Values in $ millions. (D) suppressed cells become NA.
read_sagdp <- function(table) {
  d <- read_bea_regional(table) |>
    filter(GeoFIPS %in% state_fips, as.integer(LineCode) <= 86)
  unit_scale <- if (all(d$Unit == "Thousands of dollars")) 1e-3 else 1
  d |>
    transmute(state = names(state_fips)[match(GeoFIPS, state_fips)],
              line  = as.integer(LineCode),
              depth = nchar(Description) - nchar(str_trim(Description, "left")),
              value = suppressWarnings(as.numeric(.data[[as.character(year)]])) * unit_scale)
}

# Proxy for each BEA industry's size in a state: its share of national jobs
# (times the national total of whichever measure is being filled). Owner-occupied housing (no jobs) uses the state's share of
# other real estate; military (not in QCEW) uses its share of federal jobs.
state_share <- jobs |>
  left_join(us_jobs, by = "bea") |>
  mutate(share = if_else(us_jobs > 0, jobs / us_jobs, NA_real_)) |>
  group_by(state) |>
  mutate(share = case_when(bea == "HS"   ~ share[bea == "ORE"],
                           bea == "GFGD" ~ sum(jobs[bea %in% c("GFGN", "GFE")]) /
                                           sum(us_jobs[bea %in% c("GFGN", "GFE")]),
                           TRUE          ~ share)) |>
  ungroup() |>
  mutate(share = coalesce(share, 0)) |>
  select(state, bea, share)

# Share of national federal defense value added that is civilian (DoD
# civilians sit in SAGDP's federal civilian line, uniformed military in its
# military line).
us_military  <- read_sagdp("SAGDP2") |> filter(line == 85) |> pull(value) |> sum(na.rm = TRUE)
defense_civ  <- 1 - us_military / national$va[["GFGD"]]
sagdp_to_bea <- sagdp_to_bea |>
  mutate(scale = if_else(line == 84 & bea == "GFGD", defense_civ, 1))

# Fill suppressed lines top-down through the SAGDP hierarchy, then split
# leaf lines into BEA summary industries.
fill_sagdp <- function(d, w) {
  d <- arrange(d, line)
  d$parent <- map_int(seq_len(nrow(d)), \(i) {
    up <- which(d$depth[seq_len(i - 1)] < d$depth[i])
    if (length(up)) d$line[max(up)] else NA_integer_
  })
  leaf_w  <- sagdp_to_bea |> left_join(w, by = "bea") |> mutate(weight = weight * scale)
  # weight of a line = total weight of the BEA industries beneath it
  descend <- \(l) { kids <- d$line[d$parent %in% l]; if (length(kids)) c(l, descend(kids)) else l }
  d$weight <- map_dbl(d$line, \(l) sum(leaf_w$weight[leaf_w$line %in% descend(l)]))

  for (p in d$line[order(d$depth)]) {
    kids <- which(d$parent %in% p)
    if (!length(kids)) next
    d$value[kids] <- fill_residual(d$value[kids], is.na(d$value[kids]),
                                   d$value[d$line == p], d$weight[kids])
  }

  leaf_w |>
    left_join(select(d, line, line_value = value), by = "line") |>
    group_by(line) |>
    mutate(value = line_value * if (sum(weight) > 0) weight / sum(weight) else 1 / n()) |>
    group_by(bea) |>
    summarise(value = sum(value), .groups = "drop")
}

state_accounts <- function(table, national_total) {
  sagdp   <- read_sagdp(table)
  weights <- state_share |> mutate(weight = share * national_total[bea])
  map(names(state_fips), \(st) {
    fill_sagdp(filter(sagdp, state == st), filter(weights, state == st)) |>
      mutate(state = st)
  }) |> list_rbind()
}

message("Filling BEA state GDP and compensation")
va   <- state_accounts("SAGDP2", national$va)   |> rename(va = value)
comp <- state_accounts("SAGDP4", national$comp) |> rename(comp = value)

# ---- Household spending ratio ------------------------------------------------
# Of each $ of compensation: drop contributions for government social
# insurance, then apply the state's PCE-to-personal-income ratio.

read_regional <- function(table, lines) {
  read_bea_regional(table) |>
    filter(GeoFIPS %in% state_fips, LineCode %in% lines) |>
    transmute(state = names(state_fips)[match(GeoFIPS, state_fips)],
              key   = paste0(table, "_", LineCode),
              value = as.numeric(.data[[as.character(year)]]))
}

spend <- bind_rows(read_regional("SAINC4", c("10", "35", "36")),
                   read_regional("SAPCE1", "1")) |>
  pivot_wider(names_from = key, values_from = value) |>
  transmute(state,
            spend_ratio = (1 - SAINC4_36 / SAINC4_35) * SAPCE1_1 / SAINC4_10)

# ---- Self-employment ---------------------------------------------------------
# BEA stopped publishing state employment by industry after 2022, so
# proprietors are built from what it still publishes plus Census data:
#   jobs    SAINC4 total proprietors. The farm share comes from the last
#           CAEMP25N (2022) and goes to Farms; the rest is spread across
#           industries by the state's Census nonemployer establishments
#           (individual proprietorships + partnerships, BEA's definition).
#   income  SAINC5N earnings minus SAINC6N compensation, by sector, split
#           within each sector by nonemployer receipts. Farm proprietors'
#           income goes to Farms.

# Values in $ millions whatever the table's unit; suppressed cells -> NA
regional_values <- function(table, lines, yr = year) {
  read_bea_regional(table) |>
    filter(GeoFIPS %in% state_fips, LineCode %in% lines) |>
    mutate(scale = if_else(str_detect(Unit, regex("^thousands", ignore_case = TRUE)), 1e-3, 1)) |>
    transmute(state = names(state_fips)[match(GeoFIPS, state_fips)],
              line  = LineCode,
              value = suppressWarnings(as.numeric(.data[[as.character(yr)]])) * scale)
}

nes <- read_csv(file.path(raw_dir, sprintf("nonemployer_%d.csv", year)),
                col_types = cols(.default = col_character())) |>
  transmute(state    = names(state_fips)[match(paste0(state, "000"), state_fips)],
            naics    = NAICS2022,
            estab    = as.numeric(NESTAB),
            receipts = as.numeric(NRCPTOT)) |>
  inner_join(naics_to_bea, by = "naics") |>
  filter(!is.na(state)) |>
  mutate(sector = naics_sector(naics)) |>
  group_by(state, bea, sector) |>
  summarise(across(c(estab, receipts), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# Jobs
prop_total <- regional_values("SAINC4", "7040") |> select(state, total = value)
farm_share <- read_bea_regional("CAEMP25N") |>
  filter(GeoFIPS %in% state_fips, LineCode %in% c("40", "50")) |>
  transmute(state = names(state_fips)[match(GeoFIPS, state_fips)], LineCode,
            value = as.numeric(`2022`)) |>
  pivot_wider(names_from = LineCode, values_from = value, names_prefix = "l") |>
  transmute(state, farm_share = l50 / l40)

prop_jobs <- nes |>
  filter(bea != "111CA") |>
  left_join(prop_total, by = "state") |>
  left_join(farm_share, by = "state") |>
  group_by(state) |>
  mutate(prop_jobs = total * (1 - farm_share) * estab / sum(estab)) |>
  ungroup() |>
  select(state, bea, prop_jobs) |>
  bind_rows(prop_total |> left_join(farm_share, by = "state") |>
              transmute(state, bea = "111CA", prop_jobs = total * farm_share))

# Income: sector proprietors' income = earnings - compensation. Suppressed
# sectors share what's left of total nonfarm proprietors' income by receipts.
sector_lines <- c("11" = "100", "21" = "200", "22" = "300", "23" = "400", "31-33" = "500",
                  "42" = "600", "44-45" = "700", "48-49" = "800", "51" = "900", "52" = "1000",
                  "53" = "1100", "54" = "1200", "55" = "1300", "56" = "1400", "61" = "1500",
                  "62" = "1600", "71" = "1700", "72" = "1800", "81" = "1900")

nes_nonfarm <- nes |> filter(bea != "111CA")
sector_receipts <- nes_nonfarm |>
  group_by(state, sector) |>
  summarise(receipts = sum(receipts), .groups = "drop")

sector_income <- regional_values("SAINC5N", sector_lines) |>
  rename(earnings = value) |>
  left_join(regional_values("SAINC6N", sector_lines) |> rename(comp = value), by = c("state", "line")) |>
  mutate(sector = names(sector_lines)[match(line, sector_lines)],
         income = earnings - comp) |>
  left_join(sector_receipts, by = c("state", "sector")) |>
  left_join(regional_values("SAINC4", "72") |> select(state, nonfarm = value), by = "state") |>
  group_by(state) |>
  mutate(income = fill_residual(income, is.na(income), first(nonfarm), coalesce(receipts, 0))) |>
  ungroup() |>
  select(state, sector, income)

prop_income <- nes_nonfarm |>
  left_join(sector_income, by = c("state", "sector")) |>
  group_by(state, sector) |>
  mutate(prop_income = income * if (sum(receipts) > 0) receipts / sum(receipts) else estab / sum(estab)) |>
  group_by(state, bea) |>
  summarise(prop_income = sum(prop_income), .groups = "drop") |>
  bind_rows(regional_values("SAINC4", "71") |> transmute(state, bea = "111CA", prop_income = value))

# ---- Assemble ----------------------------------------------------------------

output_per_va <- national$g / national$va

state_data <- jobs |>
  left_join(va, by = c("state", "bea")) |>
  left_join(comp, by = c("state", "bea")) |>
  mutate(va     = pmax(coalesce(va, 0), 0),
         comp   = pmax(coalesce(comp, 0), 0),
         output = va * output_per_va[bea]) |>
  left_join(spend, by = "state") |>
  left_join(prop_jobs, by = c("state", "bea")) |>
  left_join(prop_income, by = c("state", "bea")) |>
  mutate(prop_jobs   = coalesce(prop_jobs, 0),
         prop_income = pmax(coalesce(prop_income, 0), 0)) |>   # losses don't lower spending
  select(state, bea, output, va, comp, jobs, prop_jobs, prop_income, spend_ratio)

# QCEW doesn't cover railroads (they report to the Railroad Retirement
# Board), so give each state national CES rail jobs in proportion to its
# rail output.
rail_jobs  <- read_csv(file.path(raw_dir, sprintf("ces_rail_%d.csv", year)), show_col_types = FALSE)$rail_jobs
state_data <- state_data |>
  mutate(jobs = if_else(bea == "482", rail_jobs * output / sum(output[bea == "482"]), jobs))

# QCEW can't tell DoD civilians from other federal workers, so split
# federal (non-postal) civilian jobs between GFGD and GFGN by their
# civilian value added.
state_data <- state_data |>
  group_by(state) |>
  mutate(
    fed_jobs = sum(jobs[bea %in% c("GFGD", "GFGN")]),
    civ_va   = case_when(bea == "GFGD" ~ va * defense_civ, bea == "GFGN" ~ va),
    jobs     = if_else(bea %in% c("GFGD", "GFGN"),
                       fed_jobs * civ_va / sum(civ_va, na.rm = TRUE), jobs)
  ) |>
  ungroup() |>
  select(-fed_jobs, -civ_va)

saveRDS(state_data, file.path(model_dir, "state_data.rds"))

check <- state_data |>
  group_by(state) |>
  summarise(output = sum(output), jobs = sum(jobs), prop_jobs = sum(prop_jobs),
            spend_ratio = first(spend_ratio))
message(sprintf(paste("State data: %d states, $%.1fT output, %.1fM wage & salary jobs,",
                      "%.1fM self-employed, spend ratio %.2f-%.2f"),
                nrow(check), sum(check$output) / 1e6, sum(check$jobs) / 1e6,
                sum(check$prop_jobs) / 1e6, min(check$spend_ratio), max(check$spend_ratio)))
