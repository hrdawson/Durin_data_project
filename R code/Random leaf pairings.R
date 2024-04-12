set.seed(2023)
# set.seed(10)

durin.random = durin |>
  # Filter to only DURIN plots
  drop_na(DURIN_plot) |>
  # Filter to just my species
  filter(species %in% c("Empetrum nigrum", "Vaccinium vitis-idaea")) |>
  # Add random row numbers for sorting
  # Modified from https://stackoverflow.com/questions/15077515/random-number-generation-for-each-row
  mutate(random = runif(n())) |>
  # Assign indivdual plant numbers
  mutate(plantID.unique = paste0(DURIN_plot, "_plant", plant_nr)) |>
  # Order to assign pair IDs
  arrange(plantID.unique, leaf_age, random) |>
  group_by(plantID.unique, leaf_age) |>
  mutate(paired = 1:n(),
         pairedID = paste0(plantID.unique, "_", paired)) |>
  relocate(plantID.unique, leaf_age, paired, pairedID) |>
  # Tidy in long form
  relocate(c(dry_mass_g, wet_mass_g, leaf_area, SLA, LDMC, leaf_thickness_1_mm:leaf_thickness_3_mm),
           .after = leaf_nr) |>
  pivot_longer(cols = dry_mass_g:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  filter(!trait %in% c("bulk_nr_leaves_clean", "plant_height")) |>
  # replace outliers with NA
  mutate(value = case_when(
    trait == "SLA" & value > 400 ~ NA,
    trait == "wet_mass_g" & species == "Empetrum nigrum" & value > 0.005 ~ NA,
    trait == "dry_mass_g" & species == "Empetrum nigrum" & value > 0.0025 ~ NA,
    trait == "LDMC" & value > 600 ~ NA,
    TRUE ~ value
  )) |>
  # Remove extraneous columns
  select(-c(bulk_nr_leaves_original, wet_mass_g_original, dry_mass_g_original, bulk_nr_leaves_scanned,
            leaf_area_original, priority, bulk_nr_leaves)) |>
  distinct()


# Calculate correlation directions
durin.random.corr.notthick = durin.random |>
  # Filter out leaf thickness because that's a puzzle
  filter(trait != "leaf_thickness") |>
  # Select only the relevant columns
  select(pairedID, trait, leaf_age, value) |>
  group_by(pairedID, trait) |>
  # Pivot out so we can compare old and new
  pivot_wider(names_from = leaf_age, values_from = value) |>
  # Calculate correlation
  mutate(corr = case_when(
    old > young ~ "negative",
    young > old ~ "positive"
  )) |>
  # Pivot back to tidy mode
  pivot_longer(cols = old:young, values_to = "value", names_to = "leaf_age")

durin.random.corr.thickness = durin.random |>
  # Filter to only leaf thickness to unpuzzle
  filter(trait == "leaf_thickness") |>
  # Add random row numbers for sorting
  # Seed: 2023
  # Modified from https://stackoverflow.com/questions/15077515/random-number-generation-for-each-row
  mutate(random_thick = runif(n())) |>
  # Order to assign pair IDs for thickness measures
  arrange(plantID.unique, leaf_age, random, random_thick) |>
  group_by(plantID.unique, leaf_age, trait) |>
  mutate(paired = 1:n(),
         pairedID_thick = paste0(plantID.unique, "_", paired)) |>
  relocate(plantID.unique, leaf_age, random, paired, pairedID_thick) |>
  # Filter to only relevant variables
  select(pairedID_thick, trait, leaf_age, value) |>
  # Pivot out so we can compare old and new
  pivot_wider(names_from = leaf_age, values_from = value) |>
  # Calculate correlation
  mutate(corr_thick = case_when(
    old > young ~ "negative",
    young > old ~ "positive"
  )) |>
  # Pivot back to tidy mode
  pivot_longer(cols = old:young, values_to = "value", names_to = "leaf_age")

durin.random.corr = durin.random |>
  # Add correlation from object below
  left_join(durin.random.corr.notthick) |>
  left_join(durin.random.corr.thickness) |>
  # Merge thickness correlations
  mutate(pairedID = case_when(
    trait == "leaf_thickness" ~ pairedID_thick,
    TRUE ~ pairedID
  ),
  corr = case_when(
    trait == "leaf_thickness" ~ corr_thick,
    TRUE ~ corr
  )
  ) |>
  select(-c(pairedID_thick, corr_thick)) |>
  # Factor leaf age levels
  mutate(leaf_age = factor(leaf_age, levels = c("young", "old", "unspecified"),
                           labels = c("current", "previous", "unspecified")),
         trait = factor(trait, levels = c( "SLA", "leaf_area", "dry_mass_g", "wet_mass_g",
                                           "LDMC", "leaf_thickness"),
                        labels = c("SLA (cm^2/g)", "Leaf area (cm^2)", "Dry mass (g)",
                                   "Wet mass (g)","LDMC (mg/g)", "Leaf thickness (mm)")))

# Visualize ----
library(viridis)
library(ggh4x)

# All sites
ggplot(durin.random.corr, aes(x = leaf_age, y = value)) +
  geom_boxplot(alpha = 0.5, fill = "grey70") +
  geom_line(aes(group=pairedID, color = corr), alpha = 0.1) +
  scale_color_manual(values = c("blue", "red", "grey")) +
  geom_point(aes(fill=leaf_age, group=pairedID)) +
  scale_x_discrete(guide = "axis_nested") +
  facet_nested(~ species + trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "Leaf year") +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

# ggsave("visualizations/2023.10.17_AgePaired.png", width = 14, height = 10, units = "in")

# Sogndal only
ggplot(durin.random.corr |> filter(siteID == "Sogndal"),
       aes(x = interaction(leaf_age, habitat), y = value)) +
  geom_line(aes(group=pairedID, color = corr), alpha = 0.3) +
  scale_color_manual(values = c("blue", "red", "grey")) +
  geom_point(aes(fill=leaf_age, group=pairedID)) +
  geom_boxplot(alpha = 0.5, fill = "grey90") +
  scale_x_discrete(guide = "axis_nested") +
  facet_nested(species ~ trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "Leaf year") +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

# ggsave("visualizations/2023.10.18_Sogndal_AgePaired.png", width = 14, height = 10, units = "in")
# ggsave("visualizations/2023.04_04_Sogndal_AgePaired_Seed10.png", width = 14, height = 10, units = "in")
# ggsave("visualizations/2023.04_04_Sogndal_AgePaired_Seed2023.png", width = 14, height = 10, units = "in")
