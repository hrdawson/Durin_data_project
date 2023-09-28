# Import and harmonize datasets ----
# Go back later and check that the protocols match what we've been doing...
leda.height = read.csv("raw_data/LEDA/LEDA_CanopyHeight.csv") |>
  rename(species = SBS.name, spp.nr = SBS.number, value = single.value..m.) |>
  mutate(trait = "plant_height",
         #convert to CM
         value = value/100,
         unit = "cm")

leda.ldmc = read.csv("raw_data/LEDA/LEDA_LDMC.csv") |>
  rename(species = SBS.name, spp.nr = SBS.number, value = single.value..mg.g.) |>
  select(-c(mean.LMDC..mg.g.,maximum.LDMC..mg.g., minimum.LDMC..mg.g.,
            number.of.replicates, standard.deviation, standard.error)) |>
  mutate(trait = "LDMC",
         unit = "mg_g")

leda.leafmass = read.csv("raw_data/LEDA/LEDA_LeafMass.csv") |>
  rename(species = SBS.name, spp.nr = SBS.number, value = single.value..mg.) |>
  select(-c(mean.LM..mg.,maximum.LM..mg., minimum.LM..mg.,
            number.of.replicates, standard.deviation, standard.error)) |>
  mutate(trait = "dry_mass_g",
         # Convert to g
         value = value/1000,
         unit = "g")

leda.leafsize = read.csv("raw_data/LEDA/LEDA_LeafSize.csv") |>
  rename(species = SBS.name, spp.nr = SBS.number, value = single.value..mm.2.) |>
  select(-c(mean.LS..mm.2.,maximum.LS..mm.2., minimum.LS..mm.2.,
            number.of.replicates, standard.deviation, standard.error)) |>
  mutate(trait = "leaf_area",
         # Convert to cm^2
         value = value/100,
         unit = "cm2")

leda.sla = read.csv("raw_data/LEDA/LEDA_SLA.csv") |>
  rename(species = SBS.name, spp.nr = SBS.number, value = single.value..mm.2.mg.) |>
  select(-c(mean.SLA..mm.2.mg.,maximum.SLA..mm.2.mg., minimum.SLA..mm.2.mg.,
            number.of.replicates, standard.deviation, standard.error)) |>
  mutate(trait = "SLA",
         # Convert to cm^2
         value = value * 10,
         unit = "cm2/g")

## Join to one dataframe ----
leda = leda.height |>
  bind_rows(leda.ldmc, leda.leafmass, leda.leafsize, leda.sla) |>
  separate(species, into = c("genus", "epithet", "subspecies"), remove = FALSE)

# Narrow to Empetrum ----
leda.en = leda |>
  filter(genus == "Empetrum") |>
  filter(general.method %in% c("actual measurement", "actual measurement (following LEDA data standards)"))

table(leda.en$trait)

# Narrow to vitis-idaea ----
leda.vv = leda |>
  filter(species == "Vaccinium vitis-idaea") |>
  filter(general.method %in% c("actual measurement", "actual measurement (following LEDA data standards)"))

table(leda.vv$trait)

# Make dataframe that matches other datasets ----
leda.join = leda |>
  # Filter to relevant data
  filter(genus == "Empetrum" | species == "Vaccinium vitis-idaea") |>
  filter(general.method %in% c("actual measurement",
                               "actual measurement (following LEDA data standards)")) |>
  # Update Empetrum name
  mutate(species = case_when(
    genus == "Empetrum" ~ "Empetrum nigrum",
    TRUE ~ species)) |>
  # Select relevant data
  select(species, trait, value) |>
  # Add database info
  mutate(leaf_age = "database",
         dataset = "LEDA") |>
  # Flag datapoints for removal
  mutate(flag = case_when(
    trait == "leaf_area" & value <= 0 ~ "below minimum",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay")
