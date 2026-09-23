# Core input-output functions: regionalize the national table for one state,
# and run an impact scenario against a state model.

# ---- Build a state model -----------------------------------------------------
#
# Regional purchase coefficients come from Flegg's location quotient:
#   SLQ_i  = (state output share of i) / (national output share of i)
#   CILQ_ij = SLQ_i / SLQ_j
#   lambda  = log2(1 + state output / national output) ^ delta
#   FLQ_ij  = lambda * CILQ_ij  (lambda * SLQ_i on the diagonal)
#   a^r_ij  = a_ij * min(FLQ_ij, 1)
# Households are closed into the model for Type II (induced) effects: a
# household row of labor income per $ output, and a household column of
# in-state PCE per $ labor income.
#
# Two job/income definitions are built side by side:
#   wage & salary   QCEW jobs; labor income = employee compensation
#   self-employed   adds proprietors' jobs and income, as IMPLAN does
#                   (coef$jobs_se, coef$labor_se, L2_se)
#
# Jobs, compensation and value added per $ output use the state's own
# ratios, bounded to `ratio_bounds` x the national ratio. Suppression fills
# can leave a tiny industry in a small state with an implausible ratio
# (e.g. more pay than output); the bounds keep those from driving results.

build_state_model <- function(st, national, state_data, delta = flq_delta,
                              ratio_bounds = c(1 / 3, 3)) {
  ind <- national$industries
  s   <- state_data |> filter(state == st) |> slice(match(ind, bea))
  x   <- setNames(s$output, ind)
  us  <- state_data |>
    group_by(bea) |>
    summarise(across(c(output, jobs, comp, va, prop_jobs, prop_income), sum), .groups = "drop") |>
    slice(match(ind, bea))

  slq <- (x / sum(x)) / (national$g / sum(national$g))
  lambda <- log2(1 + sum(x) / sum(national$g))^delta

  cilq <- outer(slq, slq, "/")
  cilq[, slq == 0] <- slq               # industry absent from state: fall back to SLQ
  diag(cilq) <- slq
  flq <- pmin(lambda * cilq, 1)
  A <- national$A * flq

  per_output <- \(v, us_v) {
    nat <- ifelse(us$output > 0, us_v / us$output, 0)
    r   <- ifelse(x > 0, v / x, nat)
    setNames(pmin(pmax(r, nat * ratio_bounds[1]), nat * ratio_bounds[2]), ind)
  }
  labor <- per_output(s$comp, us$comp)  # compensation per $ output
  jobs  <- per_output(s$jobs, us$jobs)  # jobs per $1M output
  va    <- per_output(s$va,   us$va)    # value added per $ output
  jobs_se  <- per_output(s$jobs + s$prop_jobs,   us$jobs + us$prop_jobs)
  labor_se <- per_output(s$comp + s$prop_income, us$comp + us$prop_income)
  # Labor income is part of value added, so it can't exceed it
  labor    <- pmin(labor, va)
  labor_se <- pmin(labor_se, va)

  # Households buy in-state in proportion to SLQ (capped at 1)
  hh <- national$pce_ind * pmin(slq, 1) * s$spend_ratio[1]

  n  <- length(ind)
  closed <- \(labor_row) {
    L <- solve(diag(n + 1) - rbind(cbind(A, hh), c(labor_row, 0)))[1:n, 1:n]
    dimnames(L) <- list(ind, ind)
    L
  }
  L1 <- solve(diag(n) - A)
  dimnames(L1) <- list(ind, ind)

  list(state = st, year = national$year, delta = delta, lambda = lambda,
       industries = ind, industry_names = national$industry_names,
       output = x, A = A, L1 = L1, L2 = closed(labor), L2_se = closed(labor_se),
       coef = tibble(bea = ind, jobs = jobs, labor = labor, va = va,
                     jobs_se = jobs_se, labor_se = labor_se))
}

# ---- Multipliers -------------------------------------------------------------
# Per $1M of final demand delivered by each industry.

model_multipliers <- function(m) {
  k <- m$coef
  wage_salary <- tibble(
    state          = m$state,
    bea            = m$industries,
    industry       = unname(m$industry_names),
    output_type1   = colSums(m$L1),
    output_type2   = colSums(m$L2),
    direct_jobs    = k$jobs,
    jobs_type1     = colSums(m$L1 * k$jobs),
    jobs_type2     = colSums(m$L2 * k$jobs),
    labor_type2    = colSums(m$L2 * k$labor),
    va_type2       = colSums(m$L2 * k$va)
  ) |>
    mutate(jobs_multiplier = if_else(direct_jobs > 0, jobs_type2 / direct_jobs, NA_real_))
  wage_salary |>
    mutate(direct_jobs_se  = k$jobs_se,
           jobs_type2_se   = colSums(m$L2_se * k$jobs_se),
           labor_type2_se  = colSums(m$L2_se * k$labor_se),
           output_type2_se = colSums(m$L2_se))
}

# ---- Impact analysis ---------------------------------------------------------
#
# `spending` is new final demand in $ millions, as a named vector keyed by
# BEA summary code, e.g. c("624" = 100). Alternatively give `jobs` (direct
# jobs, same naming) and they are converted to spending at the state's
# jobs-per-$1M ratio.
#
# `self_employed = TRUE` counts proprietors' jobs and income the way IMPLAN
# does; FALSE counts wage and salary jobs and employee compensation only.
#
# `in_state` is the share of each spending line bought from in-state
# businesses (a single number, or a vector named like `spending`); the rest
# leaks out and creates no in-state activity. `years` is the build period:
# jobs are job-years, and `jobs_per_year` spreads them over the period.
#
# Returns list(summary, by_industry):
#   Direct   = the spending itself
#   Indirect = in-state supply chain purchases (Type I - direct)
#   Induced  = in-state household spending of new pay (Type II - Type I)

run_impact <- function(m, spending = NULL, jobs = NULL, in_state = 1, years = 1,
                       self_employed = TRUE) {
  ind <- m$industries
  k   <- m$coef
  L2  <- m$L2
  if (self_employed) {
    k  <- mutate(k, jobs = jobs_se, labor = labor_se)
    L2 <- m$L2_se
  }
  k <- select(k, bea, jobs, labor, va)
  if (!is.null(jobs)) {
    spending <- jobs / k$jobs[match(names(jobs), ind)]
  } else {
    if (!is.null(names(in_state))) in_state <- coalesce(in_state[names(spending)], 1)
    stopifnot(all(in_state >= 0 & in_state <= 1))
    spending <- spending * in_state
  }
  stopifnot(!is.null(names(spending)), all(names(spending) %in% ind), years > 0)

  y <- setNames(numeric(length(ind)), ind)
  sums <- tapply(spending, names(spending), sum)   # same industry may appear twice
  y[names(sums)] <- sums

  out <- tibble(
    bea      = ind,
    industry = unname(m$industry_names),
    Direct   = y,
    Indirect = as.vector(m$L1 %*% y) - y,
    Induced  = as.vector(L2 %*% y) - as.vector(m$L1 %*% y)
  ) |>
    pivot_longer(Direct:Induced, names_to = "effect", values_to = "output") |>
    left_join(k, by = "bea") |>
    transmute(state = m$state, bea, industry, effect,
              output,                         # $M
              jobs        = output * jobs,    # job-years
              jobs_per_year = jobs / years,
              labor_income = output * labor,  # $M
              value_added  = output * va)     # $M

  summary <- out |>
    group_by(effect) |>
    summarise(across(c(jobs, jobs_per_year, labor_income, value_added, output), sum), .groups = "drop") |>
    arrange(match(effect, c("Direct", "Indirect", "Induced")))
  summary <- bind_rows(summary, summary |> summarise(across(-effect, sum)) |> mutate(effect = "Total"))

  list(state = m$state, summary = mutate(summary, state = m$state, .before = 1),
       by_industry = out)
}

load_state_model <- function(st) readRDS(file.path(model_dir, paste0("model_", st, ".rds")))
