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
  )
