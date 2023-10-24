# Tundra Traits d13C -----
TTT.d13C = tundratraits |>
  # Filter to relevant species
  filter(AccSpeciesName == "Empetrum nigrum" | AccSpeciesName == "Vaccinium vitis-idaea") |>
  # Filter to relevant trait
  filter(Trait == "Leaf carbon (C) isotope discrimination (delta 13C)") |>
  # Assign leaf age and metadata
  mutate(dataset = "TTT",
         leaf_age = case_when(
           Comments == "new leaves" ~ "current",
           Comments == "old leaves" ~ "previous"
         ),
         trait = "d13C",
         # Force envelope_ID type
         IndividualID = as.character(IndividualID)) |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age, DataContributor) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName, source = DataContributor)

# TRY d13C ----
trydata.d13C = read.csv("raw_data/TRY/29435.csv") |>
  filter(TraitID %in% c(89)) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    TraitID == 89 ~ "d13C",
    TRUE ~ "Unknown"
  ),
  # Force envelope_ID to character
  ObservationID = as.character(ObservationID)
  ) |>
  # Standardize species names
  mutate(species = case_when(
    AccSpeciesID == 20484 ~ "Empetrum nigrum",
    AccSpeciesID == 55754 ~ "Vaccinium vitis-idaea"
  )) |>
  # Specify dataset
  mutate(dataset = "TRY",
         leaf_age = "database") |>
  # Filter based on error risk < 4
  # filter(ErrorRisk < 4) |>
  # Remove duplicates
  rtry_remove_dup() |>
  # Remove datasets we're extracting directly
  filter(!Dataset %in% c("Tundra Trait Team")) |>
  # Select relevant columns
  select(ObservationID, species, trait, StdValue, dataset, leaf_age) |>
  # Standardize column names
  rename(envelope_ID = ObservationID, value = StdValue)

# Combined datasets
d13C = TTT.d13C |>
  bind_rows(trydata.d13C)

table(d13C$species)
