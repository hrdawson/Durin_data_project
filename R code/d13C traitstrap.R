library(traitstrap)
library(tidyverse)
library(tidylog)

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
## Not sure I need to use trait_fill, but rolling with it
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

## Non-parametric bootstrap
ts.d13C.nonPar <- trait_np_bootstrap(
  ts.d13C.traitfill,
  # Sample size is ten times larger than the number of leaf samples in DURIN
  sample_size = 1800,
  # Keep the raw data
  raw = TRUE
)

