# Bring together the outside datasets with the DURIN dataset

DURIN.lit = read.csv("output/2023.09.11_cleanDURIN.csv") %>%
  select(-dry_mass_g) |>
  # Add in prelim dry mass data
  left_join(read.csv("raw_data/2023.09.11_DURIN_drymass.csv")) |>
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # filter(siteID == "Lygra") |>
  # filter(project == "Field - Traits") |>
  # filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  # Add DURIN field
  mutate(dataset = "DURIN") |>
  # Select columns for quick comparison
  relocate(c(leaf_area, bulk_nr_leaves_clean, SLA.wet, dry_mass_g), .after = plant_height) |>
  select(-bulk_nr_leaves) |>
  select(envelope_ID, species, dataset, leaf_age:leaf_thickness_3_mm) |>
  # Calculate individual leaf values
  mutate(leaf_area = leaf_area/bulk_nr_leaves_clean,
         wet_mass_g = wet_mass_g/bulk_nr_leaves_clean,
         dry_mass_g = dry_mass_g/bulk_nr_leaves_clean) |>
  # Rename SLA for now
  rename(SLA = SLA.wet) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Add in external datasets
  bind_rows(tundratraits.join, leda.join, trydata)

# Visualize ----
library(ggh4x)
## Leaf area
ggplot(DURIN.lit |> filter(trait == "leaf_area") |>
         drop_na(leaf_age) |> filter(value < 10),
       aes(interaction(leaf_age, species), y = value,fill = dataset)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  scale_y_log10() +
  facet_wrap(~ species, scales = "free") +
  # labs(title = "Leaf thickness") +
  theme_bw()

# Specific leaf area
## CAUTION: SLA for DURIN is calculated with wet mass
ggplot(DURIN.lit |> filter(trait == "SLA") |>
         drop_na(leaf_age) |> filter(value < 500),
       aes(interaction(leaf_age, species), y = value,fill = dataset)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  # labs(title = "Leaf thickness") +
  theme_bw()

## Leaf thickness
ggplot(DURIN.lit |> filter(trait == "leaf_thickness") |>
         drop_na(leaf_age) |> filter(value < 1000),
       aes(interaction(leaf_age, species), y = value,fill = dataset)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  # scale_y_log10() +
  # facet_wrap(~ species, scales = "free") +
  # labs(title = "Leaf thickness") +
  theme_bw()

## Dry mass
ggplot(DURIN.lit |> filter(trait == "dry_mass_g") |>
         drop_na(leaf_age),
       aes(interaction(leaf_age, species), y = value,fill = dataset)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  # scale_y_log10() +
  facet_wrap(~ species, scales = "free") +
  # labs(title = "Leaf thickness") +
  theme_bw()
