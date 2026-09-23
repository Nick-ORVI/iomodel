# Export the state models as JSON for the static web calculator in docs/.
# The page does the impact math in the browser, so each state file carries
# its Type I and Type II Leontief inverses plus per-$ coefficients, for
# both job definitions (wage & salary only, and incl. self-employed).
#   docs/data/meta.json   industries, sector groups, states, model settings
#   docs/data/<ST>.json   one per state

source("R/00_config.R")
source("R/io_functions.R")

web_dir <- "docs/data"
dir.create(web_dir, showWarnings = FALSE, recursive = TRUE)

national <- readRDS(file.path(model_dir, "national.rds"))
ind      <- national$industries

# Sector groups for the industry dropdown
sector_of <- function(code) {
  case_when(
    code %in% c("111CA", "113FF")                           ~ "Agriculture, forestry & fishing",
    code %in% c("211", "212", "213")                        ~ "Mining",
    code == "22"                                            ~ "Utilities",
    code == "23"                                            ~ "Construction",
    code %in% ind[match("321", ind):match("326", ind)]      ~ "Manufacturing",
    code == "42"                                            ~ "Wholesale trade",
    code %in% c("441", "445", "452", "4A0")                 ~ "Retail trade",
    code %in% c("481", "482", "483", "484", "485", "486", "487OS", "493") ~ "Transportation & warehousing",
    code %in% c("511", "512", "513", "514")                 ~ "Information",
    code %in% c("521CI", "523", "524", "525")               ~ "Finance & insurance",
    code %in% c("HS", "ORE", "532RL")                       ~ "Real estate & rental",
    code %in% c("5411", "5415", "5412OP", "55", "561", "562") ~ "Professional & business services",
    code == "61"                                            ~ "Educational services",
    code %in% c("621", "622", "623", "624")                 ~ "Health care & social assistance",
    code %in% c("711AS", "713", "721", "722")               ~ "Arts, recreation, accommodation & food",
    code == "81"                                            ~ "Other services",
    TRUE                                                    ~ "Government"
  )
}

state_names <- c(setNames(state.name, state.abb), DC = "District of Columbia")[names(state_fips)]

meta <- list(
  year       = year,
  flq_delta  = flq_delta,
  industries = tibble(code = ind, name = unname(national$industry_names), sector = sector_of(ind)),
  states     = tibble(code = names(state_fips), name = unname(state_names))
)
jsonlite::write_json(meta, file.path(web_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE)

# Matrices go out row-major, rounded to 5 significant digits (~80 KB/state)
for (st in names(state_fips)) {
  m <- load_state_model(st)
  jsonlite::write_json(
    list(state = st,
         L1 = unname(signif(m$L1, 5)),
         L2 = unname(signif(m$L2, 5)),
         L2_se = unname(signif(m$L2_se, 5)),
         jobs  = signif(m$coef$jobs, 5),    # wage & salary jobs per $1M output
         labor = signif(m$coef$labor, 5),   # compensation per $ output
         jobs_se  = signif(m$coef$jobs_se, 5),   # incl. self-employed
         labor_se = signif(m$coef$labor_se, 5),  # incl. proprietors' income
         va    = signif(m$coef$va, 5)),     # value added per $ output
    file.path(web_dir, paste0(st, ".json")), digits = NA, auto_unbox = TRUE)
}

message("Wrote web data for ", length(state_fips), " states to ", web_dir)
