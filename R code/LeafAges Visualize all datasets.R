# Bring together the outside datasets with the DURIN dataset

# Make big tibble ----
DURIN.lit = durin |>
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  filter(siteID == "Sogndal") |>
  filter(project == "Field - Traits") |>
  # Add DURIN field
  mutate(dataset = "DURIN",
         source = "DURIN") |>
  # Select columns for quick comparison
  relocate(c(leaf_area, SLA, dry_mass_g, wet_mass_g, LDMC, leaf_thickness_1_mm:leaf_thickness_3_mm), .after = leaf_age) |>
  select(envelope_ID, species, dataset, source, leaf_age:leaf_thickness_3_mm) |>
  # Tidy in long form
  pivot_longer(cols = leaf_area:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Add in external datasets
  bind_rows(tundratraits.join, leda.join, trydata) |>
  bind_rows(LitReview.datasets |> select(dataset, leaf_age, trait, value.converted, species, source) |>
              rename(value = value.converted)) |>
  filter(!trait %in% c("bulk_nr_leaves_clean", "plant_height")) |>
  # Filter out erroneous values
  mutate(value = case_when(
    trait == "wet_mass_g" & value > 0.1 ~ NA,
    trait == "wet_mass_g" & species == "Empetrum nigrum" & value > 0.02 ~ NA,
    trait == "dry_mass_g" & species == "Empetrum nigrum" & value > 0.002 ~ NA,
    TRUE ~ value
  )) |>
  # Standardize leaf_ages to reduce chaos
  mutate(leaf_age = case_when(
    leaf_age == "database" ~ "unspecified",
    leaf_age == "both" ~ "unspecified",
    TRUE ~ leaf_age
  ),
  leaf_age = factor(leaf_age, levels = c("young", "old", "unspecified"),
                    labels = c("current", "previous", "unspecified")),
  # dataset = factor(dataset, levels = c("DURIN", "Literature", "LEDA", "TRY", "TTT"),
                   # labels = c("DURIN", "Literature", "Database", "Database", "Database")),
  trait = factor(trait, levels = c("leaf_area", "SLA", "LDMC", "dry_mass_g", "leaf_thickness",
                                   "wet_mass_g"),
                 labels = c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)", "Dry mass (g)",
                            "Leaf thickness (mm)", "Wet mass (g)"))) |>
  # Remove plant height measurements
  # filter(trait != "plant_height") |>
  # Drop traits that can't be calculated yet
  drop_na(value)

## Find NA leaf_age
error.leafage = DURIN.lit |>
  filter(is.na(leaf_age))

# Check for duplicate datasources ----
DURIN.lit.sources = DURIN.lit |>
  summarize(n = length(value), .by = c(dataset, source))


# Visualize ----
library(ggh4x)
library(viridis)
library(patchwork)

## Three traits together ----
ggplot(DURIN.lit |>
         filter(trait %in% c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)")) |>
         drop_na(leaf_age),
       aes(x = leaf_age, y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_x_discrete(guide = "axis_nested") +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  # scale_y_log10() +
  facet_nested(~ species + trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "Leaf year") +
  theme_bw() +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggsave("visualizations/2023.10.16_TraitsTogether_Sogndal.png", width = 10, height = 8, units = "in")
