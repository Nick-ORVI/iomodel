# Impact scenario: edit the inputs below, then source this file.
# Requires the models built by run_all.R.
#
# Industry codes are BEA summary codes; see output/state_multipliers_<year>.csv
# for the full list. Care-economy codes:
#   621 Ambulatory health care   622 Hospitals
#   623 Nursing & residential care   624 Social assistance (incl. child care)

source("R/00_config.R")
source("R/io_functions.R")
suppressPackageStartupMessages({ library(ggplot2); library(scales) })

# ---- Inputs ------------------------------------------------------------------

scenario_name  <- "child_care_100m"                        # used in file names
scenario_label <- "$100M in new child care / social assistance"  # chart title
state         <- "PA"
spending      <- c("624" = 100)   # new final demand, $ millions
jobs          <- NULL             # or size it in direct jobs: c("624" = 1000)
in_state      <- 1                # share bought in-state; per line: c("23" = 1, "333" = 0.2)
years         <- 1                # build period; spending and jobs are totals over it
self_employed <- TRUE             # count self-employed jobs & income (IMPLAN-style)

# ---- Run ---------------------------------------------------------------------

model  <- load_state_model(state)
result <- if (!is.null(jobs)) run_impact(model, jobs = jobs, years = years, self_employed = self_employed) else
  run_impact(model, spending = spending, in_state = in_state, years = years, self_employed = self_employed)

cat(sprintf("\n%s, %s ($ millions; %d BEA I-O, FLQ delta = %.2f)\n\n",
            state, scenario_name, model$year, model$delta))
print(result$summary |> mutate(across(where(is.numeric), \(x) round(x, 1))))

top_industries <- result$by_industry |>
  filter(effect != "Direct") |>
  group_by(industry) |>
  summarise(jobs = sum(jobs), .groups = "drop") |>
  slice_max(jobs, n = 10)
cat("\nIndustries gaining the most indirect + induced jobs:\n")
print(top_industries |> mutate(jobs = round(jobs)))

# ---- Save --------------------------------------------------------------------

stem <- file.path(out_dir, paste(state, scenario_name, sep = "_"))
write_csv(result$summary, paste0(stem, "_summary.csv"))
write_csv(result$by_industry, paste0(stem, "_by_industry.csv"))

navy <- "#0B1575"
tan  <- "#EBC77F"

plot_df <- result$summary |>
  filter(effect != "Total") |>
  mutate(effect = factor(effect, c("Induced", "Indirect", "Direct")))

p <- ggplot(plot_df, aes(x = jobs, y = effect, fill = effect == "Direct")) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(jobs, 1)), hjust = -0.15, size = 4.2, color = "grey15") +
  scale_fill_manual(values = c(`TRUE` = navy, `FALSE` = tan), guide = "none") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = sprintf("%s jobs from %s",
                       if (state == "DC") "District of Columbia" else state.name[match(state, state.abb)],
                       scenario_label),
    subtitle = sprintf("Total: %s jobs", comma(sum(plot_df$jobs), 1)),
    x = NULL, y = NULL,
    caption  = sprintf(paste0("Source: ORVI input-output model built from BEA %d Input-Output ",
                              "Accounts, BEA regional accounts, and BLS QCEW"), model$year)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title            = element_text(face = "bold", color = navy, size = 16),
    plot.title.position   = "plot",
    plot.caption          = element_text(hjust = 0, color = "grey30", size = 9),
    plot.caption.position = "plot",
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    plot.background       = element_rect(fill = "white", color = NA)
  )

ggsave(paste0(stem, "_jobs.png"), p, width = 8, height = 4, dpi = 300)
message("Saved ", stem, "_{summary,by_industry}.csv and _jobs.png")
