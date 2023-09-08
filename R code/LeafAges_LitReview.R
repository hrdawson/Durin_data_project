# Read in all lit review files
# Make file list
filesLitReview <- dir(path = "raw_data/LitReview", pattern = ".csv", full.names = TRUE, recursive = TRUE)

# Read in data

tempLitReview <- map_df(set_names(filesLitReview), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read.csv(file = file)) #important! read_csv2 reads in European format
}, .id = "File") |>
  # Make unified tag column
  mutate(tags = paste0(Manual.Tags, ";", Automatic.Tags),
         tags = str_replace(tags, "; ", ";")) |>
  # Drop unnecessary columns
  select(Key, Author, Publication.Year, Publication.Title, Title, tags) |>
  # Split so that each tag is its own column
  tidyr::separate_wider_delim(tags, delim = ";", names_sep = "X", too_few = "align_start") |>
  # Pivot tags
  pivot_longer(cols = tagsX1:tagsX42, names_to = "X", values_to = "tag") |>
  select(-X) |>
  drop_na(tag) |>
  # remove leading spaces
  mutate(tag = str_trim(tag))

tempLitReview.tags = as.data.frame(table(tempLitReview$tag))

# Make summary of which papers belong to which species ----
# First, check that keys are unique
# Make sure you have tidylog running for this one!
keycheck = tempLitReview |>
  select(Key, Title) |>
  distinct() |>
  select(Key) |>
  # This one should say `distinct: no rows removed`
  distinct()

specieslist = tempLitReview |>
  filter(tag == "Empetrum nigrum" | tag == "Vaccinium vitis-idaea") |>
  # Unique identifier
  select(Key, tag) |>
  rename(species = tag) |>
  distinct()

# Make list of articles to include ----
keeplist = tempLitReview |>
  # Filter to the tag with all the studies of interest
  filter(tag == "leaf morphological traits") |>
  # Filter out the duplicate studies
  filter(!Key %in% c("5DJJGLCC", "4EWK5C7D")) |>
  #Unique identifier
  pull(Key)

list = as.data.frame(keeplist) |>
  rename(Key = keeplist) |>
  inner_join(specieslist)

table(list$species)

# Summarize counts of each tag ----
tagcount = specieslist |>
  right_join(tempLitReview) |>
    # Filter out irrelevant articles
    drop_na(species) |>
  # Filter to studies with leaf traits
  filter(Key %in% keeplist) |>
  distinct() |>
  # Group and count
  group_by(species, tag) |>
  summarize(n = length(Key)) |>
  ungroup() |>
  mutate_all(na_if,"") |>
  drop_na(tag) |>
  # Filter out the phantom tags
  filter(!tag %in% c("extractable data: full dataset", "extractable data: no"))

# List all the tags that are relevant to my review
relevant.tags = c("extractable data", "leaf year",
                  "sampling month", "sampling season",
                  "extractable data", "habitat type",
                  # "Lit:",
                  "measurement type",
                  "trait:", "database source:", "database:",
                  "location:", "study")
#
# Filter lit review for visualizations
# This is inelegant but appears to work
# From https://gist.github.com/simmwill/dc34d71c2da8f644576afa20cca3bbef
lit.review = map(relevant.tags, str_subset, string = tagcount$tag) %>%
  reduce(union) |>
  # Make into data frame
  as.data.frame() |>
  rename(tag = "map(relevant.tags, str_subset, string = tagcount$tag) %>% reduce(union)") |>
  # Bring back in counts
  left_join(tagcount) |>
  # Separate out variables
  separate(tag, into = c("variable", "value"), sep = ":") |>
  # Remove the white space
  mutate(variable = str_trim(variable),
         value = str_trim(value)) |>
  # Filter out leaf nitrogen as a trait
  filter(value != "leaf nitrogen") |>
  relocate(species, variable, value, n) |>
  # Filter out the wrong species ones
  mutate(drop = case_when(
    str_starts(variable, "EN") & species == "Vaccinium vitis-idaea" ~ "cut",
    str_starts(variable, "VV") & species == "Empetrum nigrum" ~ "cut",
    TRUE ~ "keep"
  )) |>
  filter(drop == "keep") |>
  select(-drop) |>
  # Modify variable names
  mutate(variable = str_replace(variable, "EN ", ""),
         variable = str_replace(variable, "VV ", ""))

# Check that all fields are filled out for each study ----
## Complete list of relevant tags ----
alltags = map(relevant.tags, str_subset, string = tagcount$tag) %>%
  reduce(union)
## List of studies with actual measurements ----
actuallist = tempLitReview |>
  filter(tag == "measurement type: actual") |>
  # Unique identifier
  select(Key) |>
  distinct() |>
  pull()

## Create object to check ----
studies.actual = specieslist |>
  right_join(tempLitReview) |>
  # Filter out irrelevant articles
  drop_na(species) |>
  # Filter to studies with leaf traits
  filter(Key %in% keeplist) |>
  # Filter to studies with actual measurements
  filter(Key %in% actuallist) |>
  # Filter to relevant tags
  filter(tag %in% alltags) |>
  distinct() |>
  # Separate out variables
  separate(tag, into = c("variable", "value"), sep = ":") |>
  # Filter out the wrong species ones
  mutate(drop = case_when(
    str_starts(variable, "EN") & species == "Vaccinium vitis-idaea" ~ "cut",
    str_starts(variable, "VV") & species == "Empetrum nigrum" ~ "cut",
    TRUE ~ "keep"
  )) |>
  filter(drop == "keep") |>
  select(-drop) |>
  # # Modify variable names
  # mutate(variable = str_replace(variable, "EN ", ""),
  #        variable = str_replace(variable, "VV ", "")) |>
  # Pivot to see completeness
  select(-species) |>
  pivot_wider(names_from = "variable", values_from = "value", values_fill = NA)

# Visualize ----
## Calculate by percentage ----
litreview.percents = lit.review |>
  filter(variable != "Lit") |>
  group_by(species, variable) |>
  summarize(total = sum(n)) |>
  ungroup() |>
  left_join(lit.review) |>
  mutate(percent = round((n/total), 2))

litreview.levels.value = c(
  # measurement
  "actual", "database",
  #sampling month
  "February", "March", "April", "May", "June", "July", "August", "September",
  #sampling season
  "winter", "spring", "early summer", "late summer", "autumn",
  #habitat type
  "both", "open", "forested",
  # leaf year
  "both (alternate)", "both (concurrent)", "current", "previous",
  # data extractable
  "no", "dataset", "means",
  # country
  "Canada", "Eurasia", "Finland", "Japan", "Norway", "Russia", "Sweden",
  # database
  "general literature", "EcoFlora", "Shidakov2007", "LEDA", "TRY",
  # traits
  "leaf area", "leaf mass", "leaf thickness", "LDMC", "LMA", "SLA",
  "unspecified")

litreview.levels.value = c(
  "unspecified", "actual", "February", "winter", "both", "both (alternate)", "no", "Canada", "general literature",
  "leaf area", "database", "March", "spring", "open", "both (concurrent)", "dataset", "Eurasia",
  "EcoFlora", "leaf mass", "April", "early summer", "forested", "current", "means", "Finland",
  "Shidakov2007", "leaf thickness", "May", "late summer", "previous", "Japan", "LEDA", "LDMC",
  "June", "autumn", "Norway", "TRY", "LMA", "July", "Russia", "August", "Sweden", "SLA",
  "September"
)

litreview.levels.variable = c("measurement type", "leaf year", "sampling month", "sampling season",
                              "trait", "habitat type", "location", "database")

# All variable by species in stacked barchart
ggplot(litreview.percents |>
         filter(!variable %in% c("database source", "study", "extractable data")) |>
         mutate(value = factor(value, levels = litreview.levels.value),
                variable = factor(variable, levels = litreview.levels.variable)),
       aes(x = species, y = percent, fill = value)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  facet_grid(~variable) +
  labs(x = "", y = "Percent of studies") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave("visualizations/2023.09.08_LitReview_metaanalysis.png", width = 10, height = 8, units = "in")

# Make table of available database info ----
TRY.sum = trydata |>
  group_by(species, trait) |>
  summarize(n = length(trait)) |>
  pivot_wider(names_from = species, values_from = n) |>
  arrange(trait)

TRY.sum.spp = TRY.sum |>
  group_by(`Empetrum nigrum`, `Vaccinium vitis-idaea`) |>
  summarize(n = sum())

write.csv(TRY.sum, "output/2023.09.08_TRY summary.csv")

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

write.csv(TTT.sum, "output/2023.09.08_TTT summary.csv")

leda.sum = leda |>
  filter(species == "Vaccinium vitis-idaea" | genus == "Empetrum")  |>
  filter(general.method %in% c("actual measurement", "actual measurement (following LEDA data standards)")) |>
  filter(trait != "plant_height") |>
  group_by(genus, trait) |>
  summarize(n = length(trait)) |>
  pivot_wider(names_from = genus, values_from = n) |>
  arrange(trait)

write.csv(leda.sum, "output/2023.09.08_LEDA summary.csv")
