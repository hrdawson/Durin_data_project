# Make table of available database info ----
# Bring together the outside datasets with the DURIN dataset
# Make dataset of outside datasets
DURIN.database = tundratraits.join |>
  bind_rows(leda.join, trydata) |>
  bind_rows(LitReview.datasets |> select(dataset, leaf_age, trait, value.converted, species, source) |>
              rename(value = value.converted)) |>
  filter(trait %in% c("dry_mass_g", "LDMC", "leaf_area", "SLA")) |>
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
  ))

# write.csv(DURIN.database, "clean_data/DURIN_database.data.csv")

# Make big tibble ----

DURIN.lit = leaf.ages |>
  # Add in external datasets
  bind_rows(tundratraits.join, leda.join, trydata) |>
  bind_rows(LitReview.datasets |> select(dataset, leaf_age, trait, value.converted, species, source) |>
              rename(value = value.converted)) |>
  filter(trait %in% c("dry_mass_g", "LDMC", "leaf_area", "SLA", "leaf_thickness")) |>
  # Filter out erroneous values
  mutate(value = case_when(
    trait == "wet_mass_g" & value > 0.1 ~ NA,
    trait == "wet_mass_g" & species == "Empetrum nigrum" & value > 0.02 ~ NA,
    trait == "dry_mass_g" & species == "Empetrum nigrum" & value > 0.002 ~ NA,
    TRUE ~ value
  )) |>
  drop_na(value) |>
  # Standardize leaf_ages to reduce chaos
  mutate(leaf_age = case_when(
    leaf_age == "database" ~ "unspecified",
    leaf_age == "both" ~ "unspecified",
    TRUE ~ leaf_age
  ),
  leaf_age = factor(leaf_age, levels = c("young", "old", "unspecified"),
                    labels = c("current", "previous", "unspecified")),
  database = dataset,
  dataset = factor(dataset, levels = c("DURIN", "Literature", "LEDA", "TRY", "TTT"),
                   labels = c("DURIN", "Dataset", "Dataset", "Dataset", "Dataset")),
  trait = factor(trait, levels = c("leaf_area", "SLA", "LDMC", "dry_mass_g", "leaf_thickness",
                                   "wet_mass_g"),
                 labels = c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)", "Dry mass (g)",
                            "Leaf thickness (mm)", "Wet mass (g)"))) |>
  # Drop traits that can't be calculated yet
  drop_na(value)

durin.traitCount = DURIN.lit |>
  filter(dataset == "DURIN") |>
  group_by(trait) |>
  summarize(n = length(value))

## Summarise sources
DURIN.lit.sources.sum = DURIN.lit |>
  group_by(database, species, trait) |>
  summarise(n = length(dataset)) |>
  pivot_wider(names_from = species, values_from = n) |>
  # write.csv("output/2024.04.18_DURIN.lit.sources.csv")

## Find NA leaf_age
error.leafage = DURIN.lit |>
  filter(is.na(leaf_age))

# Check for duplicate datasources ----
DURIN.lit.sources = DURIN.lit |>
  summarize(n = length(value), .by = c(dataset, database, source)) |>
  mutate(percent = n/sum(n))

# Count DURIN measurements
DURIN.ct.leaves = leaf.ages |>
  select(envelope_ID) |>
  distinct()

# Visualize ----
library(ggh4x)
library(viridis)
library(patchwork)

## Three traits together ----
compareTraits.EN =
  ggplot(DURIN.lit |>
         filter(trait %in% c("SLA (cm^2/g)", "Leaf area (cm^2)", "Dry mass (g)", "LDMC (mg/g)")) |>
         filter(species == "Empetrum nigrum") |>
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
  labs(x = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

compareTraits.VV =
  ggplot(DURIN.lit |>
           filter(trait %in% c("SLA (cm^2/g)", "Leaf area (cm^2)", "Dry mass (g)", "LDMC (mg/g)")) |>
           filter(species == "Vaccinium vitis-idaea") |>
           drop_na(leaf_age),
         aes(x = leaf_age, y = value, fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  # geom_point(alpha = 0.5) +
  # gghighlight(dataset == "DURIN",
  #             unhighlighted_params = list(shape = NA)) +
  geom_point(inherit.aes = FALSE,
             data = DURIN.lit |>
               filter(trait %in% c("SLA (cm^2/g)", "Leaf area (cm^2)", "Dry mass (g)", "LDMC (mg/g)")) |>
               filter(species == "Vaccinium vitis-idaea") |>
               filter(dataset == "DURIN"),
             aes(x = leaf_age, y = value, fill = dataset, color = dataset), position = position_dodge(),
             # position = position_jitterdodge(),
             alpha = 0.5) +
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

compareTraits.EN + compareTraits.VV +
  plot_layout(ncol = 1, guides = 'collect')

ggsave("visualizations/2024.04.23_TraitsTogether_Sogndal.png", width = 8, height = 12, units = "in")
# ggsave("visualizations/2024.04.18_TraitsTogether_Sogndal.png", width = 12, height = 6, units = "in")

## Calculate coverage ---
## Code is from Julia and Pekka's paper
## https://github.com/poniitty/ITV_grids/blob/main/scripts/09_global_versus_local_ITV.R

perce_leafAge <- DURIN.lit %>%
  group_by(species, trait) %>%
  mutate(maxv = max(value)) %>%
  group_by(species, leaf_age, trait, dataset) %>%
  summarise(range = round(max(value)-min(value),3),
            maxv = max(maxv)) |>
  pivot_wider(id_cols = c(species,trait,leaf_age, maxv), names_from = dataset, values_from = range) |>
  # Bring dataset data to DURIN
  group_by(species, trait) |>
  fill(Database, .direction = "updown") |>
  drop_na(DURIN) |>
  mutate(diff = round(DURIN/Database*100)) %>%
  mutate(diff = paste0(diff,"%")) |>
  mutate(dataset = "DURIN")

perce <- DURIN.lit %>%
  group_by(species, trait) %>%
  mutate(maxv = max(value)) %>%
  group_by(species, trait, dataset) %>%
  summarise(range = round(max(value)-min(value),3),
            maxv = max(maxv)) |>
  pivot_wider(id_cols = c(species,trait,maxv), names_from = dataset, values_from = range) |>
  # Bring dataset data to DURIN
  group_by(species, trait) |>
  fill(Database, .direction = "updown") |>
  drop_na(DURIN) |>
  mutate(diff = round(DURIN/Database*100)) %>%
  mutate(diff = paste0(diff,"%"))

## Three traits together ----
ggplot(DURIN.lit |>
         filter(trait %in% c("SLA (cm^2/g)", "Leaf area (cm^2)", "Dry mass (g)", "LDMC (mg/g)")) |>
         drop_na(leaf_age),
       aes(x = leaf_age, y = value,fill = dataset, color = dataset)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  geom_text(data = perce_leafAge, aes(label=diff, y = maxv, color = "black"), vjust=1, size=4, color = "black") +
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

## ARCHIVE
TRY.sum = trydata |>
  group_by(species, trait) |>
  summarize(n = length(trait)) |>
  pivot_wider(names_from = species, values_from = n) |>
  arrange(trait)

TRY.sum.spp = TRY.sum |>
  group_by(`Empetrum nigrum`, `Vaccinium vitis-idaea`) |>
  summarize(n = sum())

# write.csv(TRY.sum, "output/2023.09.08_TRY summary.csv")

TTT.sum = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum" | AccSpeciesName == "Vaccinium vitis-idaea") |>
  group_by(AccSpeciesName, Trait) |>
  summarize(n = length(Trait)) |>
  pivot_wider(names_from = AccSpeciesName, values_from = n) |>
  mutate(trait = case_when(
    Trait == "Leaf area" ~ "leaf_area",
    Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)" ~ "LDMC",
    Trait == "Leaf dry mass" ~ "dry_mass_g",
    TRUE ~ "Unknown"
  )) |>
  filter(trait != "Unknown") |>
  select(trait, "Empetrum nigrum", "Vaccinium vitis-idaea") |>
  arrange(trait)

# write.csv(TTT.sum, "output/2023.09.08_TTT summary.csv")

leda.sum = leda |>
  filter(species == "Vaccinium vitis-idaea" | genus == "Empetrum")  |>
  filter(general.method %in% c("actual measurement", "actual measurement (following LEDA data standards)")) |>
  filter(trait != "plant_height") |>
  group_by(genus, trait) |>
  summarize(n = length(trait)) |>
  pivot_wider(names_from = genus, values_from = n) |>
  arrange(trait)

# write.csv(leda.sum, "output/2023.09.08_LEDA summary.csv")

