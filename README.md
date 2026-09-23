# State input-output model

Estimates the **direct, indirect, and induced** jobs, labor income, value
added, and output from new spending in any industry, for all 50 states and
DC. Built entirely from public data (BEA and BLS), at the BEA summary level
of 71 industries.

## Usage

The Census download needs a free API key
([sign up](https://api.census.gov/data/key_signup.html)). Put it in a file
named `.Renviron` in this folder. Git ignores that file, so the key never
reaches GitHub:

```
CENSUS_API_KEY=your-key-here
```

```r
source("run_all.R")   # download data + build all 51 state models (~1 min first time)
source("scenario.R")  # edit the inputs at the top: state, industry, $ or jobs
```

Or run impacts directly:

```r
source("R/00_config.R"); source("R/io_functions.R")
m <- load_state_model("OH")
run_impact(m, spending = c("624" = 50, "23" = 20))$summary  # $M of final demand
run_impact(m, jobs = c("623" = 500))$summary               # sized by direct jobs

# A 4-year build where only 20% of the equipment is bought in-state
run_impact(m, spending = c("23" = 450, "333" = 325),
           in_state = c("333" = 0.2), years = 4)$summary      # adds jobs_per_year
```

Spending and jobs are totals over the build period. Jobs are job-years (one
job for one year), and `jobs_per_year` spreads them over the period.

`output/state_multipliers_2023.csv` has Type I and Type II output, jobs,
labor income, and value added multipliers for every state × industry.

## Web calculator (GitHub Pages)

`docs/` is a self-contained static site: `index.html` plus one JSON file per
state in `docs/data/`, written by `R/05_export_web.R` (the last step of
`run_all.R`). The impact math runs in the browser, so no server is needed.

Preview locally:

```bash
python3 -m http.server 8765 --directory docs
```

Publish: push this folder to a GitHub repo, then go to **Settings → Pages →
Build and deployment**, choose **Deploy from a branch**, and pick `main` with
the `/docs` folder. Results pages are shareable. For example,
`…/#PA/usd/624:100,23:20` reopens that scenario.

After changing the model (new year, different `flq_delta`), rerun
`run_all.R` and commit the updated `docs/data/`.

## Method

1. **National technology** (`R/02_national_tables.R`). BEA summary Make and
   Use tables (producers' prices, before redefinitions) → industry-by-industry
   direct requirements `A = D·B`, under the industry-technology assumption.
   Foreign imports are removed commodity by commodity.
2. **State economies** (`R/03_state_data.R`).
   - Jobs: BLS QCEW annual averages by NAICS, mapped to BEA industries
     (railroads from BLS CES, since QCEW doesn't cover them).
   - Value added and compensation: BEA SAGDP2 / SAGDP4.
   - Output: state value added × national output/value-added ratio.
   - Suppressed cells in both sources are filled with what's left of the
     published parent total, split by a size proxy.
   - Self-employment: BEA's state total of proprietors' jobs (SAINC4) is split
     across industries by the state's Census Nonemployer Statistics
     establishments (sole proprietorships and partnerships). Farm
     proprietors go to Farms, using the share from the last CAEMP25N (2022);
     BEA has stopped publishing employment by industry. Proprietors' income is
     earnings (SAINC5N) minus compensation (SAINC6N) by sector, split within
     each sector by nonemployer receipts.
3. **Regionalization** (`R/io_functions.R`). Flegg location quotients
   (FLQ) scale national coefficients down to in-state purchases. δ
   (`flq_delta` in `R/00_config.R`) controls how much leaks to other states.
4. **Induced effects**. Households are closed into the model: compensation per
   $ of output, times the share of compensation spent (after social insurance
   contributions, at the state's PCE-to-personal-income ratio), times the
   in-state share of each industry's goods.

Direct = the spending; Indirect = Type I − direct; Induced = Type II − Type I.

Every model is built two ways: payroll (wage & salary) jobs only, and with
the self-employed added as IMPLAN does. Adding them raises jobs, labor income,
and induced effects. `run_impact(..., self_employed = TRUE)` is the default;
the web calculator has a checkbox for it.

## Assumptions and limitations

- **δ is a judgment call.** The default of 0.1 gives Pennsylvania Type II output
  multipliers around 2.0 for construction and hospitals. At 0.3 those drop to about
  1.6. If you have RIMS II or IMPLAN multipliers for a state, calibrate δ
  against them.
- **Self-employed jobs are counts of jobs, not full-time equivalents.** Many
  are part-time or side work (rideshare, real estate, arts). That makes
  jobs per $1M very high in those industries, as in IMPLAN. For a large
  industrial project built by big contractors, payroll-only is often the
  more realistic count. Military jobs are not included either way.
- Federal defense is split between military and civilian workers using national
  shares. Retail and real estate are split into BEA industries using the state's
  employment.
- These are standard I-O assumptions: fixed coefficients, no supply
  constraints, and no price effects. The model shows gross activity supported,
  not net new jobs. It doesn't account for spending displaced to pay for the
  investment.
- If the "investment" is building something, model it as construction (`23`)
  or equipment spending. If it's operating funds (e.g. child care subsidies),
  model it as final demand in the service industry (`624`).

## Data

| Source | Table | Use |
|---|---|---|
| BEA Input-Output Accounts | Make & Use, summary, 2023 | national technology, PCE pattern |
| BEA Regional | SAGDP2, SAGDP4 | state value added, compensation |
| BEA Regional | SAINC4, SAPCE1 | household spending ratio |
| BLS QCEW | annual averages by state | state jobs by industry |
| BLS CES | CES4348200001 | rail employment |
| BEA Regional | SAINC5N, SAINC6N, CAEMP25N | proprietors' income; farm vs. nonfarm proprietors |
| Census | Nonemployer Statistics (API) | self-employment by state & industry |

To update the year, change `year` in `R/00_config.R` and rerun `run_all.R`.
All four sources need to have published that year.
