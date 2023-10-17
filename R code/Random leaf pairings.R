set.seed(2023)

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
  # Factor leaf age levels
  mutate(leaf_age = factor(leaf_age, levels = c("young", "old"))) |>
  # Tidy in long form
  relocate(c(dry_mass_g, wet_mass_g, leaf_area, SLA, leaf_thickness_1_mm:leaf_thickness_3_mm),
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
    TRUE ~ value
  )) |>
  # Add correlation from object below
  left_join(durin.random.corr)


# Calculate correlation directions
durin.random.corr = durin.random |>
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
  # Hold onto only useful variables
  select(-c(old, young))

library(viridis)
library(ggh4x)

ggplot(durin.random, aes(x = leaf_age, y = value)) +
  geom_boxplot() +
  geom_line(aes(group=pairedID, color = corr), alpha = 0.1) +
  scale_color_manual(values = c("blue", "red", "grey")) +
  geom_point(aes(fill=leaf_age, group=pairedID)) +
  theme(legend.position = "none") +
  scale_x_discrete(guide = "axis_nested") +
  # scale_y_log10() +
  facet_nested(~ species + trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "Leaf year") +
  theme_bw() +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggsave("visualizations/2023.10.16_AgePaired.png", width = 14, height = 10, units = "in")
