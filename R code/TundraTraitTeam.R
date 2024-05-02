# Examine this new dataset ----
tundratraits = read.csv("raw_data/TundraTraitTeam/TTT_cleaned_dataset_v1.csv") |>
  # Filter based on error risk < 4
filter(ErrorRisk < 4 | is.na(ErrorRisk))

TTT.EN = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum")

table(TTT.EN$Trait)

TTT.VV = tundratraits |>
  filter(AccSpeciesName == "Vaccinium vitis-idaea")

table(TTT.VV$Trait)

# Make dataframe that matches other datasets ----
tundratraits.join = tundratraits |>
  # Filter to relevant species
  filter(AccSpeciesName == "Empetrum nigrum" | AccSpeciesName == "Vaccinium vitis-idaea") |>
  ## To be used in the future but DURIN doesn't have all of these yet
  filter(Trait %in% c("Leaf area", "Leaf area per leaf dry mass (specific leaf area, SLA)", "Leaf dry mass",
                      "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)", "Leaf fresh mass")) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    Trait == "Leaf area" ~ "leaf_area",
    Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Leaf fresh mass" ~ "wet_mass_g",
    Trait == "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)" ~ "LDMC",
    TRUE ~ "Unknown"
  )) |>
  # Filter out irrelevant traits
  filter(!trait %in% c("Unknown")) |>
  # Filter out error flag
  filter(!Comments %in% c("leaf mass could be more than one leaf!")) |>
  # Only non-experimental values
  filter(Treatment == "none") |>
  # Convert units
  mutate(Value = case_when(
    trait == "leaf_area" ~ Value / 100,
    trait == "SLA" ~ Value * 10,
    trait == "LDMC" ~ Value * 1000,
    TRUE ~ Value
  ),
  # Force envelope_ID type
  IndividualID = as.character(IndividualID)) |>
  # Specify dataset
  mutate(dataset = "TTT",
         leaf_age = "database") |>
  # Remove datasets we're extracting directly
  filter(!DataContributor %in% c("Maitane Iturrate Garcia, Gabriela Schaepman-Strub")) |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age, DataContributor, Comments, ValueKindName) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName, source = DataContributor)

table(tundratraits.join$trait[tundratraits.join$species=="Vaccinium vitis-idaea"])
table(tundratraits.join$ValueKindName)

# Look at variance ----
## Empetrum ----
### TundraTrait dataset ----
TTT.EN = tundratraits |>
  # Focal species
  filter(AccSpeciesName == "Empetrum nigrum") |>
  # Only measurements from single plants
  filter(ValueKindName == "Single") |>
  # Traits of interest
  ## To be used in the future but DURIN doesn't have all of these yet
  # filter(Trait %in% c("Leaf area", "Leaf area per leaf dry mass (specific leaf area, SLA)", "Leaf dry mass",
  #                     "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)", "Leaf fresh mass",
  #                     "Plant height, vegetative")) |>
  # Placeholder
  filter(Trait %in% c("Leaf fresh mass", "Plant height, vegetative")) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    # Trait == "Leaf area" ~ "leaf_area",
    # Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Plant height, vegetative" ~ "plant_height",
    Trait == "Leaf fresh mass" ~ "wet_mass_g",
    TRUE ~ "Unknown"
  )) |>
  # Convert plant height to centimeters
  mutate(Value = case_when(
    trait == "plant_height" ~ Value * 100,
    TRUE ~ Value
  )) |>
  # Specify dataset
  mutate(dataset = "TTT",
         leaf_age = "database") |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName)

### DURIN dataset ----
durin.subset.EN = read.csv("output/2023.08.16_cleanDURIN.csv") %>%
  # Filter to relevant data
  filter(species %in% c("Empetrum nigrum")) |>
  filter(project == "Field - Traits") |>
  filter(DroughtTrt %in% c(NA, "Amb (0)")) |>
  filter(habitat %in% c(NA, "Open")) |>
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
### Make single dataset ----
durin.TTT.EN = durin.subset.EN |>
  bind_rows(TTT.EN)

#### Troubleshoot ----
durin.TTT.nas = durin.TTT.EN |>
  filter(is.na(leaf_age))

### Visualize ----
library(ggh4x)

#### Plant height ----
ggplot(durin.TTT.EN |> filter(trait %in% c("plant_height", "wet_mass_g")),
       aes(x = leaf_age, y = value)) +
  geom_boxplot() +
  scale_x_discrete(guide = "axis_nested") +
  facet_grid(~trait, scales = "free_y") +
  # labs(title = "Plant height") +
  theme_bw()

# Export authors to see if I can find pubs ----
ttt.authors = tundratraits |>
  filter(AccSpeciesName == "Empetrum nigrum") |>
  select(Year, DataContributor) |>
  distinct()

# write.csv(ttt.authors, "output/TTT_authors.csv")

# Look at interspecific variance ----

TTT.spp = tundratraits |>
  ## To be used in the future but DURIN doesn't have all of these yet
  filter(Trait %in% c("Leaf area", "Leaf area per leaf dry mass (specific leaf area, SLA)", "Leaf dry mass",
                      "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC", "Leaf fresh mass")) |>
  filter(AccSpeciesName %in% c("Andromeda polifolia", "Arctostaphylos uva-ursi", "Betula nana", "Calluna vulgaris",
                               "Cassiope tetragona", "Erica herbacea", "Rhododendron caucasicum",
                               "Rhododendron ferrugineum", "Rhododendron lapponicum", "Rubus chamaemorus",
                               "Salix herbacea", "Salix lanata", "Vaccinium caespitosum", "Vaccinium myrtillus", "Vaccinium uliginosum")) |>
  # Assign same trait names as DURIN
  mutate(trait = case_when(
    Trait == "Leaf area" ~ "leaf_area",
    Trait == "Leaf area per leaf dry mass (specific leaf area, SLA)" ~ "SLA",
    Trait == "Plant height, vegetative" ~ "plant_height",
    Trait == "Leaf fresh mass" ~ "wet_mass_g",
    TRUE ~ "Unknown"
  )) |>
  # Filter out irrelevant traits
  filter(!trait %in% c("plant_height", "Unknown")) |>
  filter(!Comments %in% c("leaf mass could be more than one leaf!")) |>
  # Only non-experimental values
  filter(Treatment == "none") |>
  # Convert units
  mutate(Value = case_when(
    trait == "leaf_area" ~ Value / 100,
    trait == "SLA" ~ Value * 10,
    TRUE ~ Value
  ),
  # Force envelope_ID type
  IndividualID = as.character(IndividualID)) |>
  # Specify dataset
  mutate(dataset = "TTT",
         leaf_age = "unspecified",
         group = "Other species",
         trait = factor(trait, levels = c("leaf_area", "SLA", "LDMC", "dry_mass_g", "leaf_thickness",
                                          "wet_mass_g"),
                        labels = c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)", "Dry mass (g)",
                                   "Leaf thickness (mm)", "Wet mass (g)"))) |>
  # Remove datasets we're extracting directly
  filter(!DataContributor %in% c("Maitane Iturrate Garcia, Gabriela Schaepman-Strub")) |>
  # Select relevant columns
  select(IndividualID, AccSpeciesName, trait, Value, dataset, leaf_age, DataContributor, group) |>
  # Standardize column names
  rename(envelope_ID = IndividualID, value = Value, species = AccSpeciesName, source = DataContributor)

TTT.spp.DURIN = DURIN.lit |>
  filter(dataset == "DURIN") |>
  mutate(group = case_when(
    species == "Empetrum nigrum" & leaf_age == "current" ~ "EN current",
    species == "Empetrum nigrum" & leaf_age == "previous" ~ "EN previous",
    species == "Vaccinium vitis-idaea" & leaf_age == "current" ~ "VV current",
    species == "Vaccinium vitis-idaea" & leaf_age == "previous" ~ "VV previous",
  )) |>
  bind_rows(TTT.spp) |>
  filter(trait %in% c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)"))

## Visualize ----

library(ggh4x)
library(viridis)
library(ggridges)

ggplot(TTT.spp.DURIN |> filter(trait == "SLA (cm^2/g)"),
       aes(x=value, fill=species, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  # geom_vline(data=durin.traitAvg.grp,
  #            aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted", "solid", "longdash")) +
  scale_y_continuous(position = "left") +
  facet_nested(~ trait, scales = "free", independent = "all",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )
