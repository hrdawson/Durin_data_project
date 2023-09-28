library(rtry)

# Read in TRY data ----
trydata = read.csv("raw_data/TRY/28661.csv") |>
  mutate(OrigUncertaintyStr = as.character(OrigUncertaintyStr)) |>
  bind_rows(read.csv("raw_data/TRY/28371.csv")) |>
  bind_rows(read.csv("raw_data/TRY/28390.csv")) |>
  filter(TraitID %in% c(1, 3110, 3112, 3114, 11, 3116, 3117, 3106, 46, 47, 55)) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    TraitID %in% c(1, 3110, 3112, 3114) ~ "leaf_area",
    TraitID %in% c(11, 3116, 3117) ~ "SLA",
    TraitID == 3106 ~ "plant_height",
    TraitID == 46 ~ "leaf_thickness",
    TraitID == 47 ~ "LDMC",
    TraitID == 55 ~ "dry_mass_g",
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
  # Standardize units
  mutate(StdValue = case_when(
    trait == "plant_height" ~ StdValue * 100,
    trait == "SLA" ~ StdValue * 10,
    trait == "leaf_area" ~ StdValue / 100,
    trait == "dry_mass_g" ~ StdValue / 1000,
    trait == "LDMC" ~ StdValue * 1000,
    TRUE ~ StdValue
  )) |>
  # Remove plant height (for now)
  filter(trait != "plant_height") |>
  # Specify dataset
  mutate(dataset = "TRY",
         leaf_age = "database") |>
  # Filter based on error risk < 4
  filter(ErrorRisk < 4) |>
  # Remove duplicates
  rtry_remove_dup() |>
  # Remove datasets we're extracting directly
  filter(!Dataset %in% c("The LEDA Traitbase", "Tundra Trait Team")) |>
  # Select relevant columns
  select(ObservationID, species, trait, StdValue, dataset, leaf_age) |>
  # Standardize column names
  rename(envelope_ID = ObservationID, value = StdValue) |>
  # Flag datapoints for removal
  mutate(flag = case_when(
    trait == "leaf_area" & value > 0.2 ~ "above maximum",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay")

table(trydata$trait)
table(trydata$Dataset)

TRY.EN = trydata |>
  filter(species == "Empetrum nigrum") |>
  group_by(trait) |>
  summarize(n = length(trait))

TRY.VV = trydata |>
  filter(species == "Vaccinium vitis-idaea") |>
  group_by(trait) |>
  summarize(n = length(trait))

# Read in DURIN data ----
durin.subset.try = read.csv("output/2023.09.08_cleanDURIN.csv") %>%
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Lygra") |>
  filter(project == "Field - Traits") |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  # Add DURIN field
  mutate(dataset = "DURIN") |>
  # Select columns for quick comparison
  relocate(c(leaf_area, bulk_nr_leaves_clean, SLA.wet), .after = plant_height) |>
  select(-bulk_nr_leaves) |>
  select(envelope_ID, species, dataset, leaf_age:leaf_thickness_3_mm) |>
  # Calculate individual leaf values
  mutate(leaf_area = leaf_area/bulk_nr_leaves_clean,
         wet_mass_g = wet_mass_g/bulk_nr_leaves_clean) |>
  # Rename SLA for now
  rename(SLA = SLA.wet) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness"))

# Make single dataset ----
durin.try = durin.subset.try |>
  bind_rows(trydata)

## Troubleshoot ----
durin.try.nas = durin.try |>
  filter(is.na(leaf_age))

# Visualize ----
library(ggh4x)

# Leaf thickness
ggplot(durin.try |> filter(trait == "leaf_thickness") |> drop_na(leaf_age),
       aes(interaction(leaf_age, species), y = value)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  labs(title = "Leaf thickness") +
  theme_bw()

ggsave("visualizations/durin.try_leafthickness.png")

# Plant height
ggplot(durin.try |> filter(trait == "plant_height"),
       aes(x = dataset, y = value)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  facet_grid(~species, scales = "free_y") +
  labs(title = "Plant height") +
  theme_bw()

ggsave("visualizations/durin.try_plantheight.png")

# Specific leaf area
## CAUTION: SLA for DURIN is calculated with wet mass

ggplot(durin.try |> filter(trait == "SLA") |>
         drop_na(leaf_age) |> filter(value < 100),
       aes(interaction(leaf_age, species), y = value)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  labs(title = "Leaf thickness") +
  theme_bw()
