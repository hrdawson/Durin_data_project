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
  select(Key, Publication.Year, Publication.Title, Title, tags) |>
  # Split so that each tag is its own column
  tidyr::separate_wider_delim(tags, delim = ";", names_sep = "X", too_few = "align_start") |>
  # Pivot tags
  pivot_longer(cols = tagsX1:tagsX42, names_to = "X", values_to = "tag") |>
  select(-X)

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
  filter(tag %in% c("Empetrum nigrum", "Vaccinium vitis-idaea")) |>
  # Unique identifierd
  select(Key, tag) |>
  rename(species = tag) |>
  distinct()
  # Some studies have multiple species
  # So this will have to have its own column
  distinct() |>
  mutate(placeholder = 1) |>
  pivot_wider(names_from = tag, values_from = placeholder)

# Summarize counts of each tag ----
tagcount = tempLitReview |>
  left_join(specieslist) |>
    # Filter out irrelevant articles
    drop_na(species) |>
# remove leading spaces
    mutate(tag = str_trim(tag)) |>
  group_by(species, tag) |>
  summarize(n = length(Key)) |>
  mutate_all(na_if,"") |>
  drop_na(tag)

# List all the tags that are relevant to my review
relevant.tags = c("extractable data", "leaf year",
                  "sampling month", "sampling season",
                  "extractable data", "habitat type",
                  "Lit:", "measurement type")

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
  # Filter out the phantom tags
  filter(!tag %in% c("extractable data: full dataset", "extractable data: no")) |>
  # Separate out variables
  separate(tag, into = c("variable", "value"), sep = ":") |>
  # Remove extraneous variable info
  # mutate(variable = str_replace(variable, "EN ", ""),
  #        variable = str_replace(variable, "VV ", "")) |>
  relocate(species, variable, value, n)

# Something isn't working with the species joining
# Everything is labelled Empetrum
