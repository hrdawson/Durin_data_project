library(traitstrap)
library(tidyverse)
library(tidylog)
# For randomisation below
library(data.table)

# Check the vignette for tips https://cran.r-project.org/web/packages/traitstrap/vignettes/traitstrap-workflow.html

# Simulate community data ----
ts.comm = tibble(species = c("Vaccinium vitis-idaea", "Empetrum nigrum"),
                 cover = 1) |>
  # To hack the package, we need to duplicate species to use as the level
  mutate(level = species)

# Modify d13C data to match filler format ----
ts.d13C = d13C |>
  mutate(level = species) |>
  # Get rid of extra columns
  select(species, trait, value, level)

library(rstatix)
ts.d13C.summary = d13C |>
  mutate(level = species) |>
  group_by(species, dataset) |>
  get_summary_stats(type = "mean")

# Non-parametric bootstrap data ----
## Not sure I need to use trait_fill, ----
#  but rolling with it
ts.d13C.traitfill = trait_fill(
  # input data (mandatory)
  comm = ts.comm,
  traits = ts.d13C,

  # specifies columns in your data (mandatory)
  abundance_col = "cover",
  taxon_col = "species",
  trait_col = "trait",
  value_col = "value",

  # specifies sampling hierarchy
  scale_hierarchy = c("level"),

  # min number of samples
  min_n_in_sample = 9
)

## Non-parametric bootstrap ----
ts.d13C.nonPar <- trait_np_bootstrap(
  ts.d13C.traitfill,
  # Sample size is ten times larger than the number of leaf samples in DURIN
  sample_size = 18000,
  # Keep the raw data
  raw = TRUE
)

## See what it looks like ----
ggplot(ts.d13C.nonPar, aes(x = value, fill = species))  +
  geom_histogram() +
  theme_bw()

# Randomly resample bootstrapped data with DURIN leaves ----
set.seed(2023)

## Make a list of all envelope_ID values I need ----
# ts.envelopes = durin |>
#   # Filter to relevant data
#   filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
#   filter(siteID == "Sogndal") |>
#   filter(project == "Field - Traits") |>
#   select(envelope_ID, species) |>
#   # Venturing into data.table (Jonas, wish me luck...)
#   data.table()
#
# ts.data = ts.d13C.nonPar |>
#   ungroup() |>
#   select(species, value) |>
#   data.table()
#
# # Code from https://stackoverflow.com/questions/56559559/join-data-frames-and-select-random-row-when-there-are-multiple-matches
# ts.d13C.random =
#   ts.data[ts.envelopes, on = .(species),
#                     {ri <- sample(.N, 1L)
#                     .(envelope_ID = envelope_ID,
#                       value = value[ri])}, by = .EACHI] |>
#   rename(d13C = value)

# It's quite likely this isn't actually random resampling
# But it works for now
# And there is an issue open to make it better in the future
# Also, the shape of the sampled data roughly matches the shape of the bootstrapped data

# Try a different way that's more random
# Has to be done differently for each species
# How many do we need of each species?

table(DURIN.lit$species)

ts.subset = ts.data |>
  # Slice the appropriate number of samples in random order
  group_by(species) |>
  slice_sample(n = 190) |>
  # Assign number for joining
  # nrow() would make more sense, but doesn't work right now (no idea why not)
  arrange(species) |>
  rownames_to_column() |>
  rename(d13C = value)

# Join back to the original data ----
ts.durin = DURIN.lit |>
  # Select columns for quick comparison
  relocate(c(leaf_area, SLA, dry_mass_g, LDMC), .after = leaf_age) |>
  # Bind in d13C data (use for first way of randomising)
  # left_join(ts.d13C.random) |>
  # Assign number for joining
  # nrow() would make more sense, but doesn't work right now (no idea why not)
  arrange(species) |>
  rownames_to_column() |>
  # Use for second way of randomising
  left_join(ts.subset) |>
  # Make the key traits long form
  select(envelope_ID, species, dataset, source, leaf_age:LDMC, d13C) |>
  # Tidy in long form
  pivot_longer(cols = leaf_area:LDMC, names_to = "trait", values_to = "value") |>
  # Remove outliers
  mutate(value = case_when(
    species == "Empetrum nigrum" & trait == "dry_mass_g" & value > 0.0015 ~ NA,
    species == "Empetrum nigrum" & trait == "LDMC" & value > 500 ~ NA,
    species == "Empetrum nigrum" & trait == "leaf_area" & value > 0.1 ~ NA,
    species == "Vaccinium vitis-idaea" & trait == "SLA" & value > 250 ~ NA,
    TRUE ~ value
  ))

# Visualize ----
library(ggh4x)

ggplot(ts.durin,
       aes(x = value, y = d13C, color = leaf_age)) +
  geom_point() +
  geom_smooth(inherit.aes = FALSE,
              aes(x = value, y = d13C), color = "grey10",
              method = "lm") +
  geom_smooth(method = "lm") +
  facet_grid2(trait~species, scales = "free", independent = "all") +
  theme_bw()

ggsave("visualizations/2024.01.01_FormFunction_Age.png", width = 10, height = 10, units = "in")
