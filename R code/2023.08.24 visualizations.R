durin.open = durin |>
  # Replace mislabeled site
  mutate(siteID = replace(siteID, siteID == "Senje", "Senja")) |>
  # Filter to relevant data
  # This dataset is open/control at all coastal sites
  filter(project == "Field - Traits") |>
  # filter(siteID %in% c("Lygra", "Tjøtta", "Senje")) |>
  filter(habitat %in% c(NA, "Open")) |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  # Filter out resampling
  mutate(sampling = case_when(
    day == "27" & month == "July" ~ "T2",
    TRUE ~ "T1"
  )) |>
  filter(sampling == "T1") |>
  # Only Empetrum
  filter(species == "Empetrum nigrum") |>
  # Create joined treatment column
  mutate(treatment = case_when(
    DroughtTrt == "Amb (0)" ~ "No grazing",
    habitat == "Open" ~ "Grazing",
    TRUE ~ "Unknown"
  )) |>
  # Select columns for quick comparison
  select(envelope_ID, siteID, treatment, species, leaf_age:leaf_thickness_3_mm) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal", "Tjøtta", "Senja", "Kautokeino")))

# Visualize ----
## Leaf traits ----
ggplot(durin.open %>% filter(leaf_age != "") |> filter(trait %in% c("leaf_thickness", "wet_mass_g")),
       aes(interaction(treatment, siteID), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN leaf thickness and leaf mass") +
  theme_bw()

ggsave("visualizations/2023.08.24_grazing_EN_leaftraits.png")

## Plant height ----
ggplot(durin.open |> filter(trait == "plant_height"),
       aes(interaction(treatment, siteID), y = value)) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN plant height") +
  theme_bw()

ggsave("visualizations/2023.08.24_grazing_EN_plantheight.png")

# Try the same with VV ----
durin.open = durin |>
  # Replace mislabeled site
  mutate(siteID = replace(siteID, siteID == "Senje", "Senja")) |>
  # Filter to relevant data
  # This dataset is open/control at all coastal sites
  filter(project == "Field - Traits") |>
  # filter(siteID %in% c("Lygra", "Tjøtta", "Senje")) |>
  filter(habitat %in% c(NA, "Open")) |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  # Remove outlier
  filter(leaf_thickness_1_mm > 0.1) |>
  # Filter out resampling
  mutate(sampling = case_when(
    day == "27" & month == "July" ~ "T2",
    TRUE ~ "T1"
  )) |>
  filter(sampling == "T1") |>
  # Only vitis-idaea
  filter(species == "Vaccinium vitis-idaea") |>
  # Create joined treatment column
  mutate(treatment = case_when(
    DroughtTrt == "Amb (0)" ~ "No grazing",
    habitat == "Open" ~ "Grazing",
    TRUE ~ "Unknown"
  )) |>
  # Select columns for quick comparison
  select(envelope_ID, siteID, treatment, species, leaf_age:leaf_thickness_3_mm) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal", "Tjøtta", "Senja", "Kautokeino")))

# Visualize ----
## Leaf traits ----
ggplot(durin.open %>% filter(leaf_age != "") |> filter(trait %in% c("leaf_thickness", "wet_mass_g")),
       aes(interaction(treatment, siteID), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN leaf thickness and leaf mass") +
  theme_bw()

ggsave("visualizations/2023.08.24_grazing_VV_leaftraits.png")

## Plant height ----
ggplot(durin.open |> filter(trait == "plant_height"),
       aes(interaction(treatment, siteID), y = value)) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN plant height") +
  theme_bw()

ggsave("visualizations/2023.08.24_grazing_VV_plantheight.png")
