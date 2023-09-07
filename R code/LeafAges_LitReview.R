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
  select(-X) |>
  # Summarize
  group_by(tag) |>
  summarize(n = length(Key)) |>
  mutate_all(na_if,"") |>
  drop_na(tag) |>
  mutate(tag = str_trim(tag))

# List all the tags that are relevant to my review
relevant.tags = c("extractable data", "leaf year",
                  "sampling month", "sampling season",
                  "extractable data", "habitat type",
                  "Lit:", "measurement type")

# Filter lit review for visualizations
# This is inelegant but appears to work
# From https://gist.github.com/simmwill/dc34d71c2da8f644576afa20cca3bbef
lit.review = map(relevant.tags, str_subset, string = tempLitReview$tag) %>%
  reduce(union) |>
  # Make into data frame
  as.data.frame() |>
  rename(tag = "map(relevant.tags, str_subset, string = tempLitReview$tag) %>% reduce(union)") |>
  # Bring back in counts
  left_join(tempLitReview) |>
  # Filter out the phantom tags
  filter(!tag %in% c("extractable data: full dataset", "extractable data: no")) |>
  # Separate out variables
  separate(tag, into = c("variable", "key"), sep = ":") |>
  # Specify species
  mutate(species = case_when(
    str_detect(variable, "EN") ~ "Empetrum nigrum",
    str_detect(variable, "VV") ~ "Vaccinium vitis-idaea",
    TRUE ~ "Unknown"
  ))

