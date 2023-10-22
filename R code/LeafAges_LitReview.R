# Read in all lit review files
library(janitor)
# Make file list
filesLitReview <- dir(path = "raw_data/LitReview", pattern = ".csv", full.names = TRUE, recursive = TRUE)

# Read in data

temp = read_csv(file = "raw_data/LitReview/2023.09.19 Empetrum leaf.csv",
                col_select = c(-Issue), name_repair = ~ janitor::make_clean_names(., case = "upper_camel"))

tempLitReview <- map_df(set_names(filesLitReview), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_csv(file = file, col_select = c(-Issue),
                      name_repair = ~ janitor::make_clean_names(., case = "upper_camel"))) #important! read_csv2 reads in European format
}, .id = "File") |>
  # Make unified tag column
  mutate(tags = paste0(ManualTags, ";", AutomaticTags),
         tags = str_replace(tags, "; ", ";")) |>
  # Drop unnecessary columns
  select(Key, Author, PublicationYear, PublicationTitle, Title, tags) |>
  # Split so that each tag is its own column
  tidyr::separate_wider_delim(tags, delim = ";", names_sep = "X", too_few = "align_start") |>
  # Pivot tags
  pivot_longer(cols = tagsX1:tagsX48, names_to = "X", values_to = "tag") |>
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
  # dplyr::mutate_all(na_if,"") |>
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
                  "location:", "study", "justification:", "graph:")
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
  filter(Key %in% keeplist | Key %in% keeplist.broad) |>
  # Filter to studies with actual measurements
  filter(Key %in% actuallist) |>
  # Filter to relevant tags
  filter(tag %in% alltags | tag %in% relevant.tags.broad) |>
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

# https://stackoverflow.com/questions/48024266/save-a-data-frame-with-list-columns-as-csv-file
studies.actual %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) %>%
  arrange(PublicationYear) |>
  write.csv('output/2023.10.17_CheckLitCompleteness.csv', row.names = FALSE)

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
  "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",
  #sampling season
  "winter", "spring", "early summer", "late summer", "autumn",
  #habitat type
  "both", "open", "forested",
  # leaf year
  "both (alternate)", "both (concurrent)", "both (mixed)", "current", "previous",
  # data extractable
  "no", "dataset", "means",
  # country
  "Canada", "Eurasia", "Finland", "Italy", "Japan", "Norway", "Russia", "Scotland", "Serbia", "Sweden",
  # database
  "general literature", "EcoFlora", "Shidakov2007", "LEDA", "TRY",
  # traits
  "leaf area", "leaf mass", "leaf thickness", "LDMC", "LMA", "SLA",
  # justifications
  "none", "standardization", "appearance", "testing physiology by age",
  # graphs
  "years lumped", "years split",
  "unspecified")

litreview.levels.value = c(
  "unspecified", "actual", "February", "winter", "both", "both (alternate)", "no", "Canada", "general literature",
  "leaf area", "none", "years lumped", "standardization", "database", "March", "spring", "open", "both (concurrent)", "dataset", "Eurasia",
  "EcoFlora", "leaf mass", "appearance", "years split", "April", "both (mixed)", "early summer", "forested", "current", "means", "Finland",
  "Shidakov2007", "leaf thickness", "testing physiology by age", "May", "late summer", "previous", "Italy", "LEDA", "LDMC",
  "June", "autumn", "Japan", "TRY", "LMA", "July", "Norway", "August", "Russia", "September", "Scotland",
  "SLA", "October", "Serbia", "November", "Sweden"
)

litreview.levels.variable = c("measurement type", "leaf year", "justification", "graph", "sampling month", "sampling season",
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

# ggsave("visualizations/2023.09.21_LitReview_metaanalysis.png", width = 10, height = 8, units = "in")

# Broader species and leaf review ----
# Make list of articles to include ----

keeplist.broad = tempLitReview |>
  # Filter to the tag with all the studies of interest
  filter(tag == "fresh leaves measured" | tag == "leaf morphological traits") |>
  #Unique identifier
  pull(Key)

specieslist = tempLitReview |>
  filter(tag == "Empetrum nigrum" | tag == "Vaccinium vitis-idaea") |>
  # filter(tag == "Empetrum nigrum") |>
  # Unique identifier
  dplyr::select(Key, tag) |>
  rename(species = tag) |>
  distinct()

list.broad = as.data.frame(keeplist.broad) |>
  rename(Key = keeplist.broad) |>
  inner_join(specieslist)

# Summarize counts of each tag ----
tagcount.broad = specieslist |>
  right_join(tempLitReview) |>
  # Filter out irrelevant articles
  drop_na(species) |>
  # Filter to studies with leaf traits
  filter(Key %in% keeplist.broad) |>
  distinct() |>
  # Re-key duplicates for accurate counting
  left_join(duplicates.broad) |>
  mutate(key.new = case_when(
    is.na(key.new) ~ Key,
    TRUE ~ key.new
  )) |>
  # Filter duplicate observations now duplicates are rekeyed
  select(-Key) |>
  distinct() |>
  # Filter out the wrong species ones
  mutate(drop = case_when(
    str_detect(tag, "EN ") & species == "Vaccinium vitis-idaea" ~ "cut",
    str_detect(tag, "VV ") & species == "Empetrum nigrum" ~ "cut",
    TRUE ~ "keep"
  )) |>
  filter(drop == "keep") |>
  select(-drop) |>
  # Modify variable names
  mutate(tag = str_replace(tag, "EN only", ""),
         tag = str_replace(tag, "VV only", ""),
         tag = str_trim(tag)) |>
  # Group and count
  group_by(species, tag) |>
  summarize(n = length(key.new)) |>
  ungroup() |>
  # dplyr::mutate_all(na_if,"") |>
  drop_na(tag) |>
  # Filter out the phantom tags
  filter(!tag %in% c("extractable data: full dataset", "extractable data: no")) |>
  # Filter out recategorized tags
  filter(!tag %in% c("trait type: freeze tolerance", "trait type: stable isotope", "trait type: radioactive isotope"))


# List all the tags that are relevant to my review
relevant.tags.broad = c("leaf year",
                  "sampling month", "sampling season",
                  "trait type:",
                  "location:", "justification:")
#
# Filter lit review for visualizations
# This is inelegant but appears to work
# From https://gist.github.com/simmwill/dc34d71c2da8f644576afa20cca3bbef
lit.review.broad = map(relevant.tags.broad, str_subset, string = tagcount.broad$tag) %>%
  reduce(union) |>
  # Make into data frame
  as.data.frame() |>
  rename(tag = "map(relevant.tags.broad, str_subset, string = tagcount.broad$tag) %>% reduce(union)") |>
  # Bring back in counts
  left_join(tagcount.broad) |>
  # Separate out variables
  separate(tag, into = c("variable", "value"), sep = ":") |>
  # Remove the white space
  mutate(variable = str_trim(variable),
         value = str_trim(value)) |>
  relocate(species, variable, value, n) |>
  # Filter out the wrong species ones
  mutate(drop = case_when(
  str_starts(variable, "EN") & species == "Vaccinium vitis-idaea" ~ "cut",
  str_starts(variable, "VV") & species == "Empetrum nigrum" ~ "cut",
  str_detect(value, "EN only") & species == "Vaccinium vitis-idaea" ~ "cut",
  str_detect(value, "VV only") & species == "Empetrum nigrum" ~ "cut",
  TRUE ~ "keep"
  )) |>
  filter(drop == "keep") |>
  select(-drop) |>
  # Modify variable names
  mutate(variable = str_replace(variable, "EN ", ""),
         variable = str_replace(variable, "VV ", ""),
         value = str_replace(value, "EN only", ""),
         value = str_replace(value, "VV only", ""),
         value = str_trim(value),
         variable = str_trim(variable))



## Prep visuals ----
litreview.percents.broad = lit.review.broad |>
  filter(variable != "Lit") |>
  group_by(species, variable) |>
  summarize(total = sum(n)) |>
  ungroup() |>
  left_join(lit.review.broad) |>
  mutate(percent = round((n/total), 2))

litreview.levels.value = c(
  ##
  "unspecified",
  #sampling month
  "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December",
  #sampling season
  "winter", "spring", "early summer", "summer", "late summer", "autumn", "growing",
  # leaf year
  "both (mixed)", "both (concurrent)", "both (alternate)", "current", "previous",
  # justifications
  "none", "appearance", "data quality", "only current leaves available",
  "prior research", "standardization", "testing physiology by age",
  # graphs
  "years lumped", "years split",
  # country
  "greenhouse", "Bosnia", "Bosnia and Herzegovina", "Bulgaria", "Canada", "Estonia", "Eurasia",
  "Finland", "Germany", "Greenland", "Italy", "Japan", "Lithuania","Mongolia", "Norway",
  "Poland", "Romania", "Russia", "Scotland", "Serbia", "South Korea", "Sweden", "Turkey", "Ukraine",
  "USA",
  # trait types
  "BVOC", "chemical compound", "freeze tolerance", "microscopic morphology",
  "morphological", "NDVI", "pH", "photosynthetic (chemical)", "photosynthetic (electrical)",
  "photosynthetic (flux)", "photosynthetic (radioactive labeling)", "physiological", "radioactive isotope",
  "spectroscopy", "stable isotope", "stoichiometric"
  )

litreview.levels.value = c(
  "unspecified", "none", "years lumped", "January", "winter","greenhouse", "both (mixed)", "Bosnia", "Bosnia and Herzegovina",
  "BVOC", "appearance", "years split", "February", "Bulgaria", "spring", "both (concurrent)",
  "Canada", "chemical compound", "data quality", "March", "early summer", "both (alternate)", "Estonia", "isotopic", "only current leaves available",
  "April", "Eurasia", "summer", "current", "Finland", "microscopic morphology", "prior research", "May", "late summer", "previous", "Germany", "morphological", "standardization",
  "June", "autumn", "Greenland", "NDVI", "testing physiology by age", "July", "pH", "growing", "Italy", "photosynthetic (chemical)", "August", "Japan", "photosynthetic (electrical)",
  "September", "Lithuania", "photosynthetic (flux)", "October", "Mongolia", "photosynthetic (radioactive labeling)", "November",
  "Norway", "Poland", "physiological", "December", "Romania", "Russia", "Scotland", "Serbia",
  "spectroscopy", "South Korea", "Sweden", "Turkey", "stoichiometric","Ukraine", "USA"
)

litreview.levels.variable = c("leaf year", "justification", "graph", "sampling month", "sampling season",
                              "trait type",  "location")

ggplot(litreview.percents.broad |>
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

# ggsave("visualizations/2023.09.21_LitReview_metaanalysis_broad.png", width = 10, height = 8, units = "in")
