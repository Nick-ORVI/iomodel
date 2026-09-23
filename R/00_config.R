# Configuration shared by every script. Run scripts from the project root
# (open inputoutputmodel.Rproj in RStudio, or setwd() to this folder).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(purrr)
  library(stringr)
  library(tibble)
})

# One year for everything, so national technology, state GDP, compensation,
# and QCEW jobs all describe the same economy. BEA's summary I-O tables
# currently run through 2023; bump this when BEA publishes 2024.
year <- 2023

# Flegg location quotient (FLQ) size parameter: the single biggest judgment
# call in the model. Higher delta = more leakage to other states = smaller
# multipliers, most of all in small states. delta = 0 is the plain
# cross-industry LQ (an upper bound). The literature uses 0.1-0.3, with
# lower values for larger regions; 0.1 gives Pennsylvania Type II output
# multipliers of about 2.0 for construction and hospitals. Calibrate against
# RIMS II or IMPLAN multipliers for your state if you have them.
flq_delta <- 0.1

# API keys live in the project's git-ignored .Renviron. R reads it at
# startup when launched from this folder; this covers other launch paths.
if (file.exists(".Renviron")) readRenviron(".Renviron")

raw_dir   <- "data/raw"
model_dir <- "data/models"
out_dir   <- "output"
for (d in c(raw_dir, model_dir, out_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# 50 states + DC, as BEA / QCEW 5-digit area FIPS
state_fips <- c(
  AL = "01000", AK = "02000", AZ = "04000", AR = "05000", CA = "06000",
  CO = "08000", CT = "09000", DE = "10000", DC = "11000", FL = "12000",
  GA = "13000", HI = "15000", ID = "16000", IL = "17000", IN = "18000",
  IA = "19000", KS = "20000", KY = "21000", LA = "22000", ME = "23000",
  MD = "24000", MA = "25000", MI = "26000", MN = "27000", MS = "28000",
  MO = "29000", MT = "30000", NE = "31000", NV = "32000", NH = "33000",
  NJ = "34000", NM = "35000", NY = "36000", NC = "37000", ND = "38000",
  OH = "39000", OK = "40000", OR = "41000", PA = "42000", RI = "44000",
  SC = "45000", SD = "46000", TN = "47000", TX = "48000", UT = "49000",
  VT = "50000", VA = "51000", WA = "53000", WV = "54000", WI = "55000",
  WY = "56000"
)
