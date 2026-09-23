# Build every state model from scratch. Takes about a minute once the raw
# data is cached (the first run downloads ~60 MB).

source("R/01_download.R")
source("R/02_national_tables.R")
source("R/03_state_data.R")
source("R/04_build_models.R")
source("R/05_export_web.R")
