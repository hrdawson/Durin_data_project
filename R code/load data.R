# Load data for all analyses ----
durin = read.csv("clean_data/durin_clean.csv")

leaf.ages = durin |>
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Sogndal") |>
  filter(project == "Field - Traits") |>
  # Add DURIN field
  mutate(dataset = "DURIN",
         source = "DURIN") |>
  # Temp code to replace missing plot nr
  mutate(DURIN_plot = replace_na(DURIN_plot, "SO_F_EN_1")) |>
  # Select columns for quick comparison
  relocate(c(leaf_area, SLA, dry_mass_g, wet_mass_g, LDMC, leaf_thickness_1_mm:leaf_thickness_3_mm), .after = leaf_age) |>
  select(envelope_ID, siteID, DURIN_plot, plant_nr, leaf_nr, species, dataset, habitat, source, leaf_age:leaf_thickness_3_mm) |>
  # Tidy in long form
  pivot_longer(cols = leaf_area:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # replace outliers with NA
  mutate(value = case_when(
    trait == "SLA" & value > 400 ~ NA,
    trait == "wet_mass_g" & species == "Empetrum nigrum" & value > 0.005 ~ NA,
    trait == "dry_mass_g" & species == "Empetrum nigrum" & value > 0.0025 ~ NA,
    trait == "LDMC" & value > 600 ~ NA,
    TRUE ~ value
  )) |>
  drop_na(value)
