# Download and cache every raw input. Safe to re-run: files already on disk
# are skipped. Delete data/raw/ to force a fresh pull.
#
#   BEA Input-Output Accounts, summary level (71 industries), producers'
#     prices, before redefinitions (establishment basis, matching state GDP
#     and QCEW): Make and Use tables
#   BEA Regional: SAGDP2 (GDP by state and industry), SAGDP4 (compensation),
#     SAINC4 (personal income components), SAPCE1 (state PCE),
#     SAINC5N / SAINC6N (earnings and compensation by industry, whose
#     difference is proprietors' income), CAEMP25N (farm vs nonfarm
#     proprietors; BEA stopped publishing it after 2022)
#   BLS QCEW annual averages for each state and the U.S.
#   BLS CES national rail employment (QCEW does not cover railroads)
#   Census Nonemployer Statistics by state and NAICS (self-employment)
#
# The Census API needs a free key in CENSUS_API_KEY; keep it in this
# project's .Renviron (git-ignored), which R reads at startup.

source("R/00_config.R")

options(timeout = 600)

fetch <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading ", basename(dest))
    download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  invisible(dest)
}

# ---- BEA national I-O --------------------------------------------------------

for (f in c("IOMake_Before_Redefinitions_PRO_Summary.xlsx",
            "IOUse_Before_Redefinitions_PRO_Summary.xlsx")) {
  fetch(paste0("https://apps.bea.gov/industry/release/xlsx/", f), file.path(raw_dir, f))
}

# ---- BEA regional ------------------------------------------------------------
# Member names carry the table's last year, so look them up in the archive.

regional_zip <- function(table, zip) {
  dest <- file.path(raw_dir, paste0(table, ".csv"))
  if (file.exists(dest)) return(invisible(dest))
  zip_path <- file.path(tempdir(), paste0(zip, ".zip"))
  fetch(sprintf("https://apps.bea.gov/regional/zip/%s.zip", zip), zip_path)
  member <- grep(sprintf("^%s__ALL_AREAS_.*\\.csv$", table),
                 unzip(zip_path, list = TRUE)$Name, value = TRUE)
  unzip(zip_path, files = member, exdir = tempdir())
  invisible(file.copy(file.path(tempdir(), member), dest))
}

regional_zip("SAGDP2", "SAGDP")
regional_zip("SAGDP4", "SAGDP")
regional_zip("SAINC4", "SAINC")
regional_zip("SAPCE1", "SAPCE")
regional_zip("SAINC5N", "SAINC")
regional_zip("SAINC6N", "SAINC")
regional_zip("CAEMP25N", "CAEMP25N")

# ---- BLS QCEW ----------------------------------------------------------------

qcew_dir <- file.path(raw_dir, "qcew", year)
dir.create(qcew_dir, showWarnings = FALSE, recursive = TRUE)

for (fips in c("US000", state_fips)) {
  fetch(sprintf("https://data.bls.gov/cew/data/api/%d/a/area/%s.csv", year, fips),
        file.path(qcew_dir, paste0(fips, ".csv")))
}

# ---- BLS CES rail employment -------------------------------------------------

rail_file <- file.path(raw_dir, sprintf("ces_rail_%d.csv", year))
if (!file.exists(rail_file)) {
  resp <- httr2::request("https://api.bls.gov/publicAPI/v1/timeseries/data/") |>
    httr2::req_body_json(list(seriesid = list("CES4348200001"),
                              startyear = as.character(year), endyear = as.character(year))) |>
    httr2::req_perform() |>
    httr2::resp_body_json(check_type = FALSE)   # BLS labels JSON as text/plain
  months <- keep(resp$Results$series[[1]]$data, \(d) d$period != "M13")
  write_csv(tibble(year = year, rail_jobs = mean(map_dbl(months, \(d) as.numeric(d$value))) * 1000),
            rail_file)
}

# ---- Census Nonemployer Statistics ------------------------------------------
# Individual proprietorships (LFO 920) and partnerships (930), matching BEA's
# definition of proprietors. Receipts are in $ thousands.

nes_file <- file.path(raw_dir, sprintf("nonemployer_%d.csv", year))
if (!file.exists(nes_file)) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (key == "") stop("Set CENSUS_API_KEY in .Renviron (free key: https://api.census.gov/data/key_signup.html)")
  message("Downloading Census nonemployer statistics")
  nes <- map(c("920", "930"), \(lfo) {
    rows <- httr2::request(sprintf("https://api.census.gov/data/%d/nonemp", year)) |>
      httr2::req_url_query(get = "NAICS2022,NESTAB,NRCPTOT", `for` = "state:*",
                           LFO = lfo, RCPSZES = "001", key = key) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
    as_tibble(rows[-1, , drop = FALSE], .name_repair = \(x) rows[1, ])
  }) |> list_rbind()
  write_csv(nes, nes_file)
}

# ---- Census Economic Census --------------------------------------------------
# Sales / receipts by state and NAICS from the most recent Economic Census
# (years ending in 2 or 7). Used only to check the model's estimated output.

ec_year <- year - (year - 2) %% 5
ec_file <- file.path(raw_dir, sprintf("economic_census_%d.csv", ec_year))
if (!file.exists(ec_file)) {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (key == "") stop("Set CENSUS_API_KEY in .Renviron")
  message("Downloading ", ec_year, " Economic Census")
  ec <- map(c("state:*", "us:1"), \(geo) {
    rows <- httr2::request(sprintf("https://api.census.gov/data/%d/ecnbasic", ec_year)) |>
      httr2::req_url_query(get = "NAICS2022,RCPTOT,EMP", `for` = geo,
                           TAXSTAT = "00", TYPOP = "00", key = key) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)
    out <- as_tibble(rows[-1, , drop = FALSE], .name_repair = \(x) rows[1, ])
    if (geo == "us:1") out <- rename(out, state = us) |> mutate(state = "US")
    out
  }) |> list_rbind()
  write_csv(ec, ec_file)
}

message("Raw data ready in ", raw_dir)
