# Reformat wide data ----
IturrateGarcia = read.csv("raw_data/LitReview Figures/Iturrate-Garcia_2021.csv") |>
  # Assign unique ID
  rowid_to_column(var = "group4") |>
  # Pivot long
  pivot_longer(cols = LDMC1:SLA, values_to = "value", names_to = "trait") |>
  # Rename traits for consistency
  mutate(trait = case_when(
    str_detect(trait, "SLA") ~ "SLA",
    str_detect(trait, "LDMC") ~ "LDMC",
    str_detect(trait, "LA") ~ "leaf_area",
  ),
  # Add units
  units = case_when(
    trait == "SLA" ~ "cm2 g",
    trait == "LDMC" ~ "mg g",
    trait == "leaf_area" ~ "cm2")
  ) |>
  drop_na(value) |>
  # Add in source
  mutate(source = "IturrateGarcia")

Wakui = read.csv("raw_data/LitReview Figures/Wakui_2021.csv") |>
  mutate(trait = replace(trait, trait == "leaf_mass", "dry_mass_g")) |>
  # Add in source
  add_column(source = "Wakui")

# Read in collated data ----

## Check units ----
LitReview.datasets.units = LitReview.datasets |>
  summarise(n = length(group1),.by = c(trait, units))

LitReview.datasets = read.csv("raw_data/LitReview Figures/LitReview extracted data.csv") |>
  # Join in downloaded datasets
  bind_rows(IturrateGarcia,
            read.csv("raw_data/LitReview Figures/Rajewicz_2023.csv"),
            Wakui
            ) |>
  # Calculate errors
  mutate(error.real = case_when(
    error.sign == "plus" ~ error.raw - value,
    error.sign == "minus" ~ (error.raw - value) * -1,
    is.na(error.sign) ~ error.raw
  )) |>
  # Convert units
  rename(value.original = value, units.original = units) |>
  mutate(#
    value.converted = case_when(
    units.original == "cm2 mg" ~ value.original * 1000,
    units.original == "m2 g" ~ value.original*10000,
    units.original == "m2 kg" ~ value.original * 10,
    units.original == "g m2" ~ value.original/10000,
    units.original == "mg cm2" ~ value.original / 1000,
    units.original == "mg mm2" ~ value.original / 10,
    units.original == "mg" ~ value.original / 1000,
    units.original == "mm2" ~ value.original / 100,
    units.original == "um" ~ value.original / 1000,
    units.original == "mm2 mg" ~ value.original * 10,
    units.original == "percent" ~ value.original * 10,
    TRUE ~ value.original
  ),
  error = case_when(
    units.original == "cm2 mg" ~ error.real * 1000,
    units.original == "m2 g" ~ error.real*10000,
    units.original == "m2 kg" ~ error.real * 10,
    units.original == "g m2" ~ error.real/10000,
    units.original == "mg cm2" ~ error.real / 1000,
    units.original == "mg mm2" ~ error.real / 10,
    units.original == "mg" ~ error.real / 1000,
    units.original == "mm2" ~ error.real / 100,
    units.original == "um" ~ error.real / 1000,
    units.original == "percent" ~ error.real * 10,
    TRUE ~ error.real
  ),
  units = case_when(
    units.original %in% c("cm2 mg", "m2 g", "m2 kg", "mm2 mg") ~ "cm2 g",
    units.original %in% c("g m2", "mg cm2","mg mm2") ~ "g cm2",
    units.original == "mg" ~ "g",
    units.original == "mm2" ~ "cm2",
    units.original == "um" ~ "mm",
    units.original == "percent" ~ "mg g",
    TRUE ~ units.original
  ),
  # Convert from LMA to SLA
  value.converted = case_when(
    trait == "LMA" ~ 1/value.converted,
    TRUE ~ value.converted
  ),
  units = case_when(
    trait == "LMA" ~ "cm2 g",
    TRUE ~ units
  ),
  trait = case_when(
    trait == "LMA" ~ "SLA",
    TRUE ~ trait
  )
  ) |>
  relocate(c(trait, scale, value.original, value.converted, units, units.original), .after = leaf_age) |>
  mutate(dataset = "Literature") |>
  # Filter out traits that can't be converted
  mutate(flag = case_when(
    scale == "ln" ~ "logarithmic",
    metric == "median" ~ "median",
    trait == "LDMC" & units == "g" ~ "incompatible units",
    trait == "LDMC" & value.converted < 100 ~ "below minimum",
    source == "Lagerström_2013_Fig1" ~ "All values unreasonably high",
    trait == "leaf_thickness" ~ "No longer included",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay")

# write.csv(LitReview.datasets, "raw_data/LitReview_Datasets.csv")

# Visualize ----
ggplot(LitReview.datasets,
       aes(x = leaf_age, y = value.converted, fill = leaf_age)) +
  geom_boxplot() +
  ggh4x::facet_grid2(species~trait, scales = "free_y", independent = "y") +
  theme_bw() +
  theme(legend.position = "none")

# Summarize available data ----
LitReview.datasets.sum = LitReview.datasets |>
  separate(source, into = c("Author", "Year", "Figure"), sep = "_") |>
  summarize(n = length(value.converted), .by = c(Author, Year, metric, species, trait,leaf_age)) |>
  pivot_wider(names_from = species, values_from = n)
  # write.csv("output/2024.04.18_LitReviewData_summary.csv")
