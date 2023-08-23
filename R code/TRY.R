# Read in TRY data ----
trydata = read.csv("raw_data/TRY/28371.csv") |>
  bind_rows(read.csv("raw_data/TRY/28390.csv")) |>
  filter(TraitID %in% c(1, 3110, 3112, 3114, 11, 3116, 3117, 3106, 46)) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    TraitID %in% c(1, 3110, 3112, 3114) ~ "leaf_area",
    TraitID %in% c(11, 3116, 3117) ~ "SLA",
    TraitID == 3106 ~ "plant_height",
    TraitID == 46 ~ "leaf_thickness",
    TRUE ~ "Unknown"
  )) |>
  # Standardize species names
  mutate(species = case_when(
    AccSpeciesID == 20484 ~ "Empetrum nigrum",
    AccSpeciesID == 55754 ~ "Vaccinium vitis-idaea"
  )) |>
  # Convert plant height to centimeters
  mutate(StdValue = case_when(
    trait == "plant_height" ~ StdValue * 100,
    TRUE ~ StdValue
  )) |>
  # Specify dataset
  mutate(dataset = "TRY",
         leaf_age = "database") |>
  # Select relevant columns
  select(ObservationID, species, trait, StdValue, dataset, leaf_age) |>
  # Standardize column names
  rename(Envelope_ID = ObservationID, value = StdValue)

table(trydata$TraitName)
table(trydata$DataName)

# Read in DURIN data ----
durin.subset.try = read.csv("output/2023.08.16_cleanDURIN.csv") %>%
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Lygra") |>
  filter(project == "Field - Traits") |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
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
