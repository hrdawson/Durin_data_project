# Examine this new dataset ----
tundratraits = read.csv("raw_data/TundraTraitTeam/TTT_cleaned_dataset_v1.csv") |>
  # Filter based on error risk < 4
filter(ErrorRisk < 4)

TTT.EN = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum")

table(TTT.EN$Trait)

TTT.VV = tundratraits |>
  filter(AccSpeciesName == "Vaccinium vitis-idaea")

table(TTT.VV$Trait)

# Make dataframe that matches other datasets ----
tundratraits.join = tundratraits |>
  # Filter to relevant species
  filter(AccSpeciesName == "Empetrum nigrum" | AccSpeciesName == "Vaccinium vitis-idaea") |>
  ## To be used in the future but DURIN doesn't have all of these yet
  filter(Trait %in% c("Leaf area", "Leaf area per leaf dry mass (specific leaf area, SLA)", "Leaf dry mass",
                      "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC", "Leaf fresh mass",
                      "Plant height, vegetative")) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    Trait == "Leaf area" ~ "leaf_area",
    Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Plant height, vegetative" ~ "plant_height",
    Trait == "Leaf fresh mass" ~ "wet_mass_g",
    TRUE ~ "Unknown"
  )) |>
  # Filter out irrelevant traits
  filter(!trait %in% c("plant_height", "Unknown")) |>
  # Convert units
  mutate(Value = case_when(
    trait == "plant_height" ~ Value * 100,
    trait == "leaf_area" ~ Value / 100,
    trait == "SLA" ~ Value * 10,
    TRUE ~ Value
  ),
  # Force envelope_ID type
  IndividualID = as.character(IndividualID)) |>
  # Specify dataset
  mutate(dataset = "TTT",
         leaf_age = "database") |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age, DataContributor) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName)


# Look at variance ----
## Empetrum ----
### TundraTrait dataset ----
TTT.EN = tundratraits |>
  # Focal species
  filter(AccSpeciesName == "Empetrum nigrum") |>
  # Only measurements from single plants
  filter(ValueKindName == "Single") |>
  # Traits of interest
  ## To be used in the future but DURIN doesn't have all of these yet
  # filter(Trait %in% c("Leaf area", "Leaf area per leaf dry mass (specific leaf area, SLA)", "Leaf dry mass",
  #                     "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC", "Leaf fresh mass",
  #                     "Plant height, vegetative")) |>
  # Placeholder
  filter(Trait %in% c("Leaf fresh mass", "Plant height, vegetative")) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    # Trait == "Leaf area" ~ "leaf_area",
    # Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Plant height, vegetative" ~ "plant_height",
    Trait == "Leaf fresh mass" ~ "wet_mass_g",
    TRUE ~ "Unknown"
  )) |>
  # Convert plant height to centimeters
  mutate(Value = case_when(
    trait == "plant_height" ~ Value * 100,
    TRUE ~ Value
  )) |>
  # Specify dataset
  mutate(dataset = "TTT",
         leaf_age = "database") |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName)

### DURIN dataset ----
durin.subset.EN = read.csv("output/2023.08.16_cleanDURIN.csv") %>%
  # Filter to relevant data
  filter(species %in% c("Empetrum nigrum")) |>
  filter(project == "Field - Traits") |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  filter(habitat %in% c(NA, "Open")) |>
  # Add DURIN field
  mutate(dataset = "DURIN") |>
  # Select columns for quick comparison
  select(envelope_ID, species, dataset, leaf_age:leaf_thickness_3_mm) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness"))
### Make single dataset ----
durin.TTT.EN = durin.subset.EN |>
  bind_rows(TTT.EN)

#### Troubleshoot ----
durin.TTT.nas = durin.TTT.EN |>
  filter(is.na(leaf_age))

### Visualize ----
library(ggh4x)

#### Plant height ----
ggplot(durin.TTT.EN |> filter(trait %in% c("plant_height", "wet_mass_g")),
       aes(x = leaf_age, y = value)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  facet_grid(~trait, scales = "free_y") +
  # labs(title = "Plant height") +
  theme_bw()

# Export authors to see if I can find pubs ----
ttt.authors = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum") |>
  select(Year, DataContributor) |>
  distinct()

# write.csv(ttt.authors, "output/TTT_authors.csv")
