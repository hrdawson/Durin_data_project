# Bring together the outside datasets with the DURIN dataset

# Make big tibble ----
DURIN.lit = durin |>
  # read.csv("output/2023.09.11_cleanDURIN.csv") %>%
  select(-dry_mass_g) |>
  # Add in prelim dry mass data
  left_join(read.csv("raw_data/2023.09.11_DURIN_drymass.csv")) |>
  # Filter to relevant data
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out erroneous leaf scans
  filter(!envelope_ID %in% c("ATG1962", "AVY7377", "AVI9865", "AXY0067")) |>
  # filter(siteID == "Lygra") |>
  # filter(project == "Field - Traits") |>
  # filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  # Add DURIN field
  mutate(dataset = "DURIN",
         # Calculate SLA with dry weights
         SLA.wet = leaf_area/wet_mass_g,
         # Calculate SLA with dry weights
         SLA.dry = leaf_area/dry_mass_g,
         # Calculate LDMC
         LDMC = (dry_mass_g*1000)/wet_mass_g) |>
  # Select columns for quick comparison
  relocate(c(leaf_area, bulk_nr_leaves_clean, SLA.wet, SLA.dry, dry_mass_g, LDMC), .after = plant_height) |>
  select(envelope_ID, species, dataset, leaf_age:leaf_thickness_3_mm) |>
  select(-bulk_nr_leaves) |>
  # Calculate individual leaf values
  mutate(leaf_area = leaf_area/bulk_nr_leaves_clean,
         wet_mass_g = wet_mass_g/bulk_nr_leaves_clean,
         dry_mass_g = dry_mass_g/bulk_nr_leaves_clean) |>
  # Rename SLA for now
  # rename(SLA = SLA.wet) |>
  rename(SLA = SLA.dry) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") |>
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  filter(!trait %in% c("bulk_nr_leaves_clean", "plant_height")) |>
  # Add in external datasets
  bind_rows(tundratraits.join, leda.join, trydata) |>
  bind_rows(LitReview.datasets |> select(dataset, leaf_age, trait, value.converted, species) |>
              rename(value = value.converted)) |>
  # Standardize leaf_ages to reduce chaos
  mutate(leaf_age = case_when(
    leaf_age == "database" ~ "unspecified",
    leaf_age == "both" ~ "unspecified",
    TRUE ~ leaf_age
  ),
  leaf_age = factor(leaf_age, levels = c("young", "old", "unspecified"),
                    labels = c("current", "previous", "unspecified")),
  dataset = factor(dataset, levels = c("DURIN", "Literature", "LEDA", "TRY", "TTT"),
                   labels = c("DURIN", "Literature", "Database", "Database", "Database")),
  trait = factor(trait, levels = c("leaf_area", "SLA", "LDMC", "dry_mass_g", "leaf_thickness",
                                   "wet_mass_g", "SLA.wet"),
                 labels = c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)", "Dry mass (g)",
                            "Leaf thickness (mm)", "Wet mass (g)", "SLA (cm^2/g)"))) |>
  # Remove plant height measurements
  filter(trait != "plant_height") |>
  # Drop traits that can't be calculated yet
  drop_na(value)

## Find NA leaf_age
error.leafage = DURIN.lit |>
  filter(is.na(leaf_age))

## Check which leaves have dry mass so far
durin.dry.check = read.csv("output/2023.09.11_cleanDURIN.csv") %>%
  select(-dry_mass_g) |>
  # Add in prelim dry mass data
  left_join(read.csv("raw_data/2023.09.11_DURIN_drymass.csv")) |>
  drop_na(dry_mass_g)

table(durin.dry.check$siteID)
table(durin.dry.check$species)

table(durin$siteID)
table(durin$species)

# Visualize ----
library(ggh4x)
library(viridis)
library(patchwork)

## Leaf area
ggplot(DURIN.lit |> filter(trait == "leaf_area") |>
         drop_na(leaf_age) |> filter(value < 20),
       aes(interaction(leaf_age, species), y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge()) +
  scale_x_discrete(guide = "axis_nested") +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  scale_y_log10() +
  facet_wrap(~ species, scales = "free") +
  # labs(title = "Leaf thickness") +
  theme_bw()

# Specific leaf area
## CAUTION: SLA for DURIN is sometimes calculated with wet mass
ggplot(DURIN.lit |> filter(trait == "SLA") |>
         drop_na(leaf_age) |> filter(value < 250),
       aes(interaction(leaf_age, species), y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  scale_x_discrete(guide = "axis_nested") +
  # labs(title = "Leaf thickness") +
  theme_bw()

## Leaf thickness
ggplot(DURIN.lit |> filter(trait == "leaf_thickness") |>
         drop_na(leaf_age) |> filter(value < 1000),
       aes(interaction(leaf_age, species), y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge()) +
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

## LDMC
ggplot(DURIN.lit |> filter(trait == "LDMC") |>
         filter(value > 100 & value < 700) |>
         drop_na(leaf_age),
       aes(interaction(leaf_age, species), y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge()) +
  scale_x_discrete(guide = "axis_nested") +
  # scale_y_log10() +
  facet_wrap(~ species, scales = "free") +
  # labs(title = "Leaf thickness") +
  theme_bw()

## Three traits together ----
ggplot(DURIN.lit |> filter(trait %in% c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)")) |>
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

ggsave("visualizations/2023.09.27_TraitsTogether.png", width = 10, height = 8, units = "in")
