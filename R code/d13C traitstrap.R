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
  sample_size = 1800,
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
ts.envelopes = durin |>
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Sogndal") |>
  filter(project == "Field - Traits") |>
  select(envelope_ID, species) |>
  # Venturing into data.table (Jonas, wish me luck...)
  data.table()

ts.data = ts.d13C.nonPar |>
  ungroup() |>
  select(species, value) |>
  data.table()

# Code from https://stackoverflow.com/questions/56559559/join-data-frames-and-select-random-row-when-there-are-multiple-matches
ts.d13C.random =
  ts.data[ts.envelopes, on = .(species),
                    {ri <- sample(.N, 1L)
                    .(envelope_ID = envelope_ID,
                      value = value[ri])}, by = .EACHI] |>
  rename(d13C = value)

# It's quite likely this isn't actually random resampling
# But it works for now
# And there is an issue open to make it better in the future
# Also, the shape of the sampled data roughly matches the shape of the bootstrapped data

# Join back to the original data ----
ts.durin = DURIN.lit |>
  # Select columns for quick comparison
  relocate(c(leaf_area, SLA, dry_mass_g, wet_mass_g, LDMC, leaf_thickness_1_mm:leaf_thickness_3_mm), .after = leaf_age) |>
  # Bind in d13C data
  left_join(ts.d13C.random)

# Visualize ----
ggplot(ts.durin, aes(x = SLA, y = d13C, color = leaf_age)) +
  geom_point() +
  geom_smooth(inherit.aes = FALSE,
              aes(x = SLA, y = d13C), color = "grey70",
              method = "lm") +
  geom_smooth(method = "lm") +
  facet_grid(~species, scales = "free") +
  theme_bw()
