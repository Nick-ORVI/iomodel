# Build and save a model for every state, and write a table of multipliers
# for every state x industry.

source("R/00_config.R")
source("R/io_functions.R")

national   <- readRDS(file.path(model_dir, "national.rds"))
state_data <- readRDS(file.path(model_dir, "state_data.rds"))

models <- map(set_names(names(state_fips)), \(st) build_state_model(st, national, state_data))
iwalk(models, \(m, st) saveRDS(m, file.path(model_dir, paste0("model_", st, ".rds"))))

multipliers <- map(models, model_multipliers) |> list_rbind()
write_csv(multipliers, file.path(out_dir, sprintf("state_multipliers_%d.csv", year)))

message(sprintf("Built %d state models; multipliers in %s", length(models),
                file.path(out_dir, sprintf("state_multipliers_%d.csv", year))))
