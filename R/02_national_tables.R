# National industry-by-industry direct requirements from the BEA summary
# Make and Use tables (industry-technology assumption), net of foreign
# imports. Produces data/models/national.rds.

source("R/00_config.R")

# Read one year of a BEA summary Make/Use sheet into a numeric matrix with
# IOCode row and column names. BEA writes suppressed/zero cells as "...".
read_bea_sheet <- function(file, year) {
  x <- suppressMessages(read_excel(file.path(raw_dir, file), sheet = as.character(year),
                                   col_names = FALSE, col_types = "text"))
  col_codes <- unlist(x[6, -(1:2)])
  body      <- x[-(1:7), ]
  row_codes <- body[[1]]
  keep      <- !is.na(row_codes) & row_codes != ""
  m <- as.matrix(body[keep, -(1:2)])
  m <- suppressWarnings(matrix(as.numeric(m), nrow = nrow(m)))
  m[is.na(m)] <- 0
  dimnames(m) <- list(row_codes[keep], col_codes)
  m[, !is.na(col_codes), drop = FALSE]
}

make <- read_bea_sheet("IOMake_Before_Redefinitions_PRO_Summary.xlsx", year)
use  <- read_bea_sheet("IOUse_Before_Redefinitions_PRO_Summary.xlsx", year)

# Industries are the Make table's rows (71 summary industries). Commodities
# share the same codes; scrap ("Used") and noncomparable imports ("Other")
# are treated as leakages.
ind  <- intersect(rownames(make), colnames(use))
comm <- ind

# Total rows/columns carry no IOCode in these files, so rebuild them.
V <- make[ind, comm]                    # make: industry x commodity
U <- use[comm, ind]                     # use: commodity x industry
g <- rowSums(make[ind, ])               # industry output (incl. scrap)
q <- colSums(V)                         # commodity output

va   <- colSums(use[c("V001", "V002", "V003"), ind])  # value added
comp <- use["V001", ind]                              # compensation of employees

# Foreign import share of each commodity's domestic absorption
imports <- -use[comm, "F050"]
exports <-  use[comm, "F040"]
m_share <- pmin(pmax(imports / (q - exports + imports), 0), 1)
m_share[!is.finite(m_share)] <- 0

B <- sweep(U, 2, g, "/")                # commodity inputs per $ industry output
D <- sweep(V, 2, q, "/")                # industry market shares of each commodity
D[!is.finite(D)] <- 0
A <- D %*% ((1 - m_share) * B)          # domestic industry x industry coefficients
dimnames(A) <- list(ind, ind)

# Household spending pattern: domestically produced PCE by industry, per $
# of total PCE (so imports and noncomparable items leak out)
pce     <- use[, "F010"]
pce_ind <- as.vector(D %*% ((1 - m_share) * pce[comm])) / sum(pce[c(comm, "Used", "Other")])
names(pce_ind) <- ind

names_tbl <- suppressMessages(read_excel(file.path(raw_dir, "IOMake_Before_Redefinitions_PRO_Summary.xlsx"),
                                         sheet = as.character(year), col_names = FALSE, col_types = "text"))
ind_names <- setNames(names_tbl[[2]], names_tbl[[1]])[ind]

# Inputs + value added should reproduce industry output
stopifnot(all(colSums(A) < 1),
          all(abs(colSums(use[c(comm, "Used", "Other"), ind]) + va - g) / g < 0.01))

national <- list(year = year, industries = ind, industry_names = ind_names,
                 A = A, g = g, va = va, comp = comp, pce_ind = pce_ind)
saveRDS(national, file.path(model_dir, "national.rds"))
message(sprintf("National %d table: %d industries, total output $%.1fT",
                year, length(ind), sum(g) / 1e6))
