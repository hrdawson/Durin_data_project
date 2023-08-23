# Examine this new dataset ----
tundratraits = read.csv("raw_data/TundraTraitTeam/TTT_cleaned_dataset_v1.csv")

TTT.EN = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum")

table(TTT.EN$Trait)


TTT.VV = tundratraits |>
  filter(AccSpeciesName == "Vaccinium vitis-idaea")

table(TTT.VV$Trait)

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
  rename(Envelope_ID = IndividualID, value = Value, species = AccSpeciesName)

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
