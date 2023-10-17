library(BIEN)

# Load EN data ----
BIEN.EN = BIEN_trait_species("Empetrum nigrum")

# Inspect the data
table(BIEN.EN$trait_name)

# Load VV data ----
BIEN.VV = BIEN_trait_species("Vaccinium vitis-idaea")

# Inspect the data
table(BIEN.VV$trait_name)

# Join the two species ----
BIEN.join = BIEN.EN |>
  bind_rows(BIEN.VV) |>
  # Rename variables
  mutate(trait = case_when(
    trait_name == 'leaf area' ~ 'leaf_area',
    trait_name == 'leaf area per leaf dry mass' ~ "SLA",
    trait_name == 'leaf dry mass' ~ 'dry_mass_g',
    trait_name == "whole plant height" ~ "plant_height",
    TRUE ~ NA
  )) |>
  # Filter out variables not of interest
  drop_na(trait) |>
  # Drop duplicates from other database source
  # filter(url_source != "http://www.leda-traitbase.org/LEDAportal/") |>
  # Convert units
  mutate(trait_value = as.numeric(trait_value),
    value = case_when(
    trait == "plant_height" ~ trait_value * 100,
    trait == "SLA" ~ trait_value * 10,
    trait == "leaf_area" ~ trait_value / 100,
    TRUE ~ trait_value
  )) |>
  # Select only the relevant variables
  rename(species = scrubbed_species_binomial)
  select(species, trait, value) |>
  # Add in relevant metadata
    mutate(leaf_age = )

BIEN.sources = as.data.frame(table(BIEN$url_source))
BIEN.traits = as.data.frame(table(BIEN$trait_name))
BIEN.units = as.data.frame(table(BIEN$unit))
