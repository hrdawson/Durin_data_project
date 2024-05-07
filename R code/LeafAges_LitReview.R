# Redoing the LitReview code for the new round (cleanly downloaded once)
# Search used for the new round:
# ALL=((trait* OR "leaf" OR "leaves") AND (("Vaccinium vitis-idaea" OR “vitis-idaea” OR ("lingon*" OR puolukka OR "mountain cranberr*" OR "partridgeberr*" OR "cowberr*")) OR  (("Empetrum" AND ("hermaphroditum" OR "nigrum")) OR "E hermaphroditum" OR "E nigrum" OR ("crowberr*" OR kragebær OR sortebær OR krekebær OR krøkebær OR kråkebær OR kråkbär OR kråkris OR Variksenmarja OR mustavariksenmarja)) OR “dwarf shrub*”  OR ((ericoid* OR ericac*) AND shrub*)))

# Read in data ----
LitReview.leaves.all = read.csv("raw_data/2024.04.10_Leaves.csv", na.strings = c(" ", "")) |>
  # Make unified tag column
  mutate(tags = paste0(Manual.Tags, ";", Automatic.Tags),
         tags = str_replace(tags, "; ", ";")) |>
  # Drop unnecessary columns
  select(Key, Author, Publication.Year, Publication.Title, Title, DOI, tags) |>
  # Split so that each tag is its own column
  tidyr::separate_wider_delim(tags, delim = ";", names_sep = "X", too_few = "align_start") |>
  # Pivot tags
  pivot_longer(cols = tagsX1:tagsX52, names_to = "X", values_to = "tag")|>
  select(-X) |>
  # For some reason NA strings aren't being read as NA. Filtering manually instead
  filter(tag != "NA") |>
  # remove leading spaces
  mutate(tag = str_trim(tag))

# Make lists to filter by and assign values by ----
## Which studies to keep ----
LitReview.leaves.keepList = LitReview.leaves.all |>
  # Filter to just studies being kept
  filter(tag == "status: keep") |>
  # Isolate their IDs
  pull(Key)

## Which tags to keep ----
LitReview.leaves.tagList = LitReview.leaves.all |>
  # All tags of interest include a colon
  filter(str_detect(tag, ":")) |>
  # Make a list of unique tags
  select(tag) |>
  distinct() |>
  pull(tag)

## Which papers belong to which species ----
LitReview.leaves.speciesList = LitReview.leaves.all |>
  filter(tag == "Empetrum nigrum" | tag == "Vaccinium vitis-idaea") |>
  # Unique identifier
  select(Key, tag) |>
  rename(species = tag) |>
  distinct()

## Which papers have morpho data ----
LitReview.leaves.morphoList = LitReview.leaves.all |>
  filter(str_detect(tag, "trait type: morphological")) |>
  # Isolate their IDs
  pull(Key)

## Which papers have database data ----
LitReview.leaves.databaseList = LitReview.leaves.all |>
  filter(str_detect(tag, "measurement type: database")) |>
  # Isolate their IDs
  pull(Key)

## Which papers need a justification -----
LitReview.leaves.justificationList = LitReview.leaves.all |>
  filter(tag != "VV leaf year: unspecified" & tag != "EN leaf year: unspecified") |>
  filter(str_detect(tag, "leaf year")) |>
  select(Key) |>
  distinct() |>
  pull(Key)

## Which papers need graph data -----
LitReview.leaves.graphList = LitReview.leaves.all |>
  filter(str_detect(tag, "leaf year: both")) |>
  filter(str_detect(tag, "concurrent")) |>
  pull(Key)

# Compile into useful dataset ----
LitReview.leaves.traits = LitReview.leaves.all |>
  # Filter to relevant studies
  filter(Key %in% LitReview.leaves.keepList) |>
  # Filter to relevant tags
  filter(tag %in% LitReview.leaves.tagList) |>
  # Separate out variables
  separate(tag, into = c("variable", "value"), sep = ":") |>
  # Remove the white space
  mutate(variable = str_trim(variable),
         value = str_trim(value)) |>
  # Remove tags we're no longer interested in
  filter(!(variable %in% c("C", "N", "language", "Lit", "study", "status"))) |>
  filter(!(value %in% c("BVOC", "iWUE", "leaf nitrogen"))) |>
  # Add in species data
  left_join(LitReview.leaves.speciesList, by = "Key") |>
  # Filter out the wrong species ones
  mutate(drop = case_when(
    str_detect(variable, "EN ") & species == "Vaccinium vitis-idaea" ~ "cut",
    str_detect(variable, "VV ") & species == "Empetrum nigrum" ~ "cut",
    str_detect(value, "EN ") & species == "Vaccinium vitis-idaea" ~ "cut",
    str_detect(value, "VV ") & species == "Empetrum nigrum" ~ "cut",
    TRUE ~ "keep"
  )) |>
  filter(drop == "keep") |>
  select(-drop) |>
  # Modify variable names
  mutate(value = str_replace(value, "EN only", ""),
         value = str_replace(value, "VV only", ""),
         value = str_trim(value),
         variable = str_replace(variable, "EN ", ""),
         variable = str_replace(variable, "VV ", ""),
         variable = str_trim(variable)) |>
  # Filter out recategorized tags
  filter(!value %in% c("freeze tolerance", "stable isotope", "radioactive isotope"))

## Export the useful dataset -----
write.csv(LitReview.leaves.traits, "clean_data/LitReview_leaves.csv")

# Summarise all the tags ----
LitReview.leaves.tagCount = LitReview.leaves.traits |>
  # Group and count
  group_by(species, variable, value) |>
  summarize(n = length(Key)) |>
  ungroup()

LitReview.leaves.totals = LitReview.leaves.traits |>
  group_by(species, variable) |>
  summarize(total = length(Key)) |>
  ungroup()

LitReview.leaves.tagPercentage = LitReview.leaves.tagCount |>
  left_join(LitReview.leaves.totals) |>
  mutate(percent = round((n/total), 2))

# Summarise tags for morpho studies ----
LitReview.leaves.tagCount.morpho = LitReview.leaves.traits |>
  filter(Key %in% LitReview.leaves.morphoList) |>
  # Group and count
  group_by(species, variable, value) |>
  summarize(n = length(Key)) |>
  ungroup()

LitReview.leaves.totals.morpho = LitReview.leaves.traits |>
  filter(Key %in% LitReview.leaves.morphoList) |>
  group_by(species, variable) |>
  summarize(total = length(Key)) |>
  ungroup()

LitReview.leaves.tagPercentage.morpho = LitReview.leaves.tagCount.morpho |>
  left_join(LitReview.leaves.totals.morpho) |>
  mutate(percent = round((n/total), 2))

# Visualize all the tags (messy!)----
ggplot(LitReview.leaves.tagPercentage,
       aes(x = species, y = percent, fill = value)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  facet_grid(~variable) +
  labs(x = "", y = "Percent of studies") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

# Visualize graphs for the paper ----
library(ochRe)
## Organise tags ----
table(LitReview.leaves.tagPercentage$variable)
table(LitReview.leaves.tagPercentage$value[LitReview.leaves.tagPercentage$variable=="sampling season"])

litreview.levels.variable = c("leaf year", "justification", "trait type", "graph", "sampling month", "sampling season",
                              "habitat type", "trait", "measurement type", "database")

litreview.levels.value = c(
  # database
  "EcoFlora", "LEDA", "TRY", "Shidakov2007", "general literature",
  # database source
  "single", "multiple",
  # extractable data
  "dataset", "points", "means", "median",
  # graph
  "years lumped", "years split",
  # habitat type
  "open", "forested", "both", "greenhouse",
  # justification
  "representation of the plant", "both were present", "testing physiology by age", "appearance", "current leaves too young",
  "only current leaves available", "missing other cohort", "experimental constraints", "standardization", "data quality",
  # leaf year
  "both (concurrent)", "both (mixed)", "both (alternate)", "current", "previous",
  # location
  "Austria", "Bosnia and Herzegovina", "Bulgaria", "Canada", "China", "Estonia", "Eurasia", "Finland", "Germany", "Greenland",
  "Italy", "Japan", "Lithuania", "Mongolia", "Norway", "Poland", "Romania", "Russia", "Scotland", "Serbia", "South Korea",
  "Sweden", "Turkey", "Ukraine", "USA",
  # measurement type
  "actual", "database",
  # sampling month
  "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December",
  # sampling season
  "end of overwintering", "before new growth", "before flowering", "spring", "spring recovery of photosynthesis", "early growing season",
  "rapid growth period", "summer", "mid growing season", "growing season", "peak growing season", "fully expanded", "peak aboveground biomass",
  "leaf maturity", "maximum shoot length", "peak leaf thickness", "late summer", "autumn", "peak ripeness", "late growing season", "before snowfall",
  "winter", "maximum snow depth",
  # trait
  "leaf area", "leaf mass", "LMA", "SLA", "leaf thickness", "LDMC", "leaf shape",
  # trait type
  "chemical compound", "isotopic", "microscopic morphology", "morphological", "NDVI", "pH", "photosynthetic (chemical)",
  "photosynthetic (electrical)", "photosynthetic (flux)", "photosynthetic (radioactive labeling)", "physiological", "spectroscopy",
  "stoichiometric",
  # catch alls
  "no", "none", "unspecified"
)

## Fig. 4: Key variables for all trait studies ----
tagVariables.allTraitKey = c("leaf year", "justification", "trait type")

ggplot(LitReview.leaves.tagPercentage |> filter(variable %in% tagVariables.allTraitKey) |>
         mutate(value = factor(value, levels = litreview.levels.value),
                variable = factor(variable, levels = litreview.levels.variable)),
       aes(x = species, y = percent, fill = value)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  facet_grid(~variable) +
  labs(x = "", y = "Percent of studies") +
  scale_fill_ochre(palette = "olsen_qual", reverse = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave("visualizations/2024.05.01_LitReview_AnalysisAllTraits_KeyVariables.png", width = 10, height = 8, units = "in")

## Fig. S2: Additional variables for all trait studies ----
tagVariables.addTraitKey = c("trait type", "graph", "sampling month", "sampling season")

ggplot(LitReview.leaves.tagPercentage |> filter(variable %in% tagVariables.addTraitKey) |>
         mutate(value = factor(value, levels = litreview.levels.value),
                variable = factor(variable, levels = litreview.levels.variable)),
       aes(x = species, y = percent, fill = value)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  facet_grid(~variable) +
  labs(x = "", y = "Percent of studies") +
  scale_fill_ochre(palette = "olsen_qual", reverse = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave("visualizations/2024.05.02_LitReview_AnalysisAddTraits_KeyVariables.png", width = 10, height = 8, units = "in")

## Fig. S4: Morpho trait studies ----
tagVariables.morphoKey = c("leaf year", "sampling month", "sampling season", "habitat type", "trait", "measurement type", "database")

ggplot(LitReview.leaves.tagPercentage.morpho |> filter(variable %in% tagVariables.morphoKey) |>
         mutate(value = factor(value, levels = litreview.levels.value),
                variable = factor(variable, levels = litreview.levels.variable)),
       aes(x = species, y = percent, fill = value)) +
  geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  facet_grid(~variable) +
  labs(x = "", y = "Percent of studies") +
  scale_fill_ochre(palette = "olsen_qual", reverse = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave("visualizations/2024.05.02_LitReview_AnalysisAddTraits_KeyVariables.png", width = 10, height = 8, units = "in")

# Try bar chart alternatives ----
## Shifted baseline ----
library(ggrepel)

LitReview.leaves.tagPercentage.morpho.neg = LitReview.leaves.tagPercentage.morpho |>
  mutate(percent = case_when(
    value %in% c("unspecified", "none", "no") ~ percent * -1,
    TRUE ~ percent
  ))

ggplot(LitReview.leaves.tagPercentage.morpho.neg |> filter(variable %in% tagVariables.morphoKey) |>
         mutate(value = factor(value, levels = litreview.levels.value),
                variable = factor(variable, levels = litreview.levels.variable)),
       aes(x = species, y = percent, fill = value)) +
  geom_col() +
  # geom_bar(position="fill", stat="identity", color = "black") +
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.7)) +
  geom_text(aes(label = n), size = 3, position = position_stack(vjust = 0.3)) +
  geom_hline(yintercept = 0) +
  facet_grid2(~variable, scales = "free_y", independent = "y") +
  labs(x = "", y = "Percent of studies") +
  scale_fill_ochre(palette = "olsen_qual", reverse = TRUE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

## Piecharts ----
library(plotly)

LitReview.leaves.tagPercentage.morpho %>%
  group_by(species) %>%
do(p=plot_ly(., labels = ~value, values = ~percent, type = 'pie',
             textposition = 'outside',textinfo = 'label+percent')) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)

  layout(title = 'Letters',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Visualize the years of publication ----
LitReview.years = LitReview.leaves.traits |>
  select(Key, Publication.Year, species) |>
  distinct()

ggplot(LitReview.years, aes(x = Publication.Year, fill = species)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  theme_bw()

# Data cleaning -----
## List all the tags that all studies should have
tags.allTraits = data.frame(variable = c("leaf year", "sampling month", "sampling season", "trait type", "location"))

tags.morpho = data.frame(variable = c(
  "measurement type",
  "extractable data",
  "leaf year",
  "sampling month",
  "trait",
  "habitat type",
  "location"
))

## Check that all studies do have these data ----
LitReview.leaves.tagCheck = LitReview.leaves.traits |>
  # Remove database-based papers
  filter(!(Key %in% LitReview.leaves.databaseList)) |>
  # Filter to needed tags
  filter(variable %in% tags.allTraits$variable) |>
  # Select the relevant columns
  select(Key, variable, value) |>
  # make sure each variable is given only once
  slice_sample(by = c(Key, variable)) |>
  # Create NAs for missing variables
  pivot_wider(names_from = variable, values_from = value) %>%
  # Replace NAs with searchable values
  replace(is.na(.), "no value") |>
  # Pivot back
  pivot_longer(cols = location:'trait type', names_to = "variable", values_to = "value") |>
  # Filter to the problems
  filter(value == "no value")

## Check that all morphological studies have their data ----
LitReview.leaves.tagCheck.morpho = LitReview.leaves.traits |>
  # Limit to just morpho papers
  filter(Key %in% LitReview.leaves.morphoList) |>
  # Remove database-based papers
  filter(!(Key %in% LitReview.leaves.databaseList)) |>
  # Filter to needed tags
  filter(variable %in% tags.morpho$variable) |>
  # Select the relevant columns
  select(Key, variable, value) |>
  # make sure each variable is given only once
  slice_sample(by = c(Key, variable)) |>
  # Create NAs for missing variables
  arrange(variable) |>
  pivot_wider(names_from = variable, values_from = value) %>%
  # Replace NAs with searchable values
  replace(is.na(.), "no value") |>
  # Pivot back
  pivot_longer(cols = 'extractable data':trait, names_to = "variable", values_to = "value") |>
  # Filter to the problems
  filter(value == "no value")

## Check that all studies stating leaf year have their justification ----
LitReview.leaves.tagCheck.just = LitReview.leaves.traits |>
  # Limit to papers with justification needed
  filter((Key %in% LitReview.leaves.justificationList)) |>
  # Filter to needed tags
  filter(variable %in% c("leaf year", "justification")) |>
  # Select the relevant columns
  select(Key, variable, value) |>
  # make sure each variable is given only once
  slice_sample(by = c(Key, variable)) |>
  # Create NAs for missing variables
  pivot_wider(names_from = variable, values_from = value) %>%
  # Replace NAs with searchable values
  replace(is.na(.), "no value") |>
  # Pivot back
  pivot_longer(cols = 'leaf year':justification, names_to = "variable", values_to = "value") |>
  # Filter to the problems
  filter(value == "no value")

LitReview.leaves.tagCheck.graph = LitReview.leaves.traits |>
  # Limit to papers with graph justification needed
  filter(Key %in% LitReview.leaves.graphList) |>
  # Filter to needed tags
  filter(variable %in% c("leaf year", "graph")) |>
  # Select the relevant columns
  select(Key, variable, value) |>
  # make sure each variable is given only once
  slice_sample(by = c(Key, variable)) |>
  # Create NAs for missing variables
  pivot_wider(names_from = variable, values_from = value) %>%
  # Replace NAs with searchable values
  replace(is.na(.), "no value") |>
  # Pivot back
  pivot_longer(cols = graph:'leaf year', names_to = "variable", values_to = "value") |>
  # Filter to the problems
  filter(value == "no value")

# Make one list of all studies to reanalyse ----
LitReview.leaves.tagRepair = LitReview.leaves.tagCheck |>
  bind_rows(LitReview.leaves.tagCheck.morpho, LitReview.leaves.tagCheck.just, LitReview.leaves.tagCheck.graph) |>
  distinct()

############################
# ARCHIVED 2024.04.11 -----
############################
# Read in all lit review files ----
# This is for both lit reviews
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
  select(Key, Author, PublicationYear, PublicationTitle, Title, Doi, tags) |>
  # Split so that each tag is its own column
  tidyr::separate_wider_delim(tags, delim = ";", names_sep = "X", too_few = "align_start") |>
  # Pivot tags
  pivot_longer(cols = tagsX1:tagsX48, names_to = "X", values_to = "tag") |>
  select(-X) |>
  drop_na(tag) |>
  # remove leading spaces
  mutate(tag = str_trim(tag))

# Check that keys are unique
# Make sure you have tidylog running for this one!
keycheck = tempLitReview |>
  select(Key, Title) |>
  distinct() |>
  select(Key) |>
  # This one should say `distinct: no rows removed`
  distinct()

# Lit review 2: Morphological traits ----
## Make summary of which papers belong to which species ----
specieslist = tempLitReview |>
  filter(tag == "Empetrum nigrum" | tag == "Vaccinium vitis-idaea") |>
  # Unique identifier
  select(Key, tag) |>
  rename(species = tag) |>
  distinct()

## Make list of articles to include ----
keeplist = tempLitReview |>
  # Filter to the tag with all the studies of interest
  filter(tag == "leaf morphological traits") |>
  # Filter out the duplicate studies
  filter(!Key %in% c("5DJJGLCC", "4EWK5C7D")) |>
  #Unique identifier
  pull(Key)

## Pull DOIs for citationChaser ----
LitReview.DOIs = tempLitReview |>
  filter(Key %in% keeplist) |>
  select(Doi) |>
  distinct() |>
  drop_na() |>
  write.csv("output/2024.04.04_LitReview_DOIs_MorphoKeepList.csv")

## Summarize counts of each tag ----
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

# List all the tags that are relevant to review 2: morpho----
relevant.tags = c("extractable data", "leaf year",
                  "sampling month", "sampling season",
                  "extractable data", "habitat type",
                  # "Lit:",
                  "measurement type",
                  "trait:", "database source:", "database:",
                  "location:", "study", "justification:", "graph:")

## Filter lit review for visualizations----
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

### Calculate percentage ----
litreview.percents = lit.review |>
  filter(variable != "Lit") |>
  group_by(species, variable) |>
  summarize(total = sum(n)) |>
  ungroup() |>
  left_join(lit.review) |>
  mutate(percent = round((n/total), 2))

## Visualize ----

### All value levels in the desired order
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

### All value in the desired order layered for coloring
litreview.levels.value = c(
  "unspecified", "actual", "February", "winter", "both", "both (alternate)", "no", "Canada", "general literature",
  "leaf area", "none", "years lumped", "standardization", "database", "March", "spring", "open", "both (concurrent)", "dataset", "Eurasia",
  "EcoFlora", "leaf mass", "appearance", "years split", "April", "both (mixed)", "early summer", "forested", "current", "means", "Finland",
  "Shidakov2007", "leaf thickness", "testing physiology by age", "May", "late summer", "previous", "Italy", "LEDA", "LDMC",
  "June", "autumn", "Japan", "TRY", "LMA", "July", "Norway", "August", "Russia", "September", "Scotland",
  "SLA", "October", "Serbia", "November", "Sweden"
)

### All levels of the variables
litreview.levels.variable = c("measurement type", "leaf year", "justification", "graph", "sampling month", "sampling season",
                              "trait", "habitat type", "location", "database")

## All variable by species in stacked barchart ----
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

# Lit review 1: Broader species and leaf review ----
## Make list of articles to include ----
keeplist.broad = tempLitReview |>
  # Filter to the tag with all the studies of interest
  filter(tag == "fresh leaves measured" | tag == "leaf morphological traits") |>
  #Unique identifier
  pull(Key)

list.broad = as.data.frame(keeplist.broad) |>
  rename(Key = keeplist.broad) |>
  inner_join(specieslist)

## Make list of duplicate studies ----
duplicates.broad = tempLitReview |>
  # Filter to relevant studies
  filter(Key %in% keeplist.broad) |>
  # Filter to studies that are duplicates |>
  filter(tag == "duplicate study") |>
  # Join back in the rest of the dataset
  select(Key) |>
  left_join(tempLitReview) |>
  # Select relevant columns
  select(Key, Author, PublicationYear, PublicationTitle, Title, tag) |>
  # Filter to just the tags with study:
  filter(str_detect(tag, "study:")) |>
  distinct()  |>
  # Rename to replace keys
  rename(key.new = tag)

## Summarize counts of each tag ----
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


## List all the tags that are relevant to  review 1 ----
relevant.tags.broad = c("leaf year",
                  "sampling month", "sampling season",
                  "trait type:",
                  "location:", "justification:")

# Filter lit review for visualizations ----
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

## Calculate by percentage ----
litreview.percents.broad = lit.review.broad |>
  filter(variable != "Lit") |>
  group_by(species, variable) |>
  summarize(total = sum(n)) |>
  ungroup() |>
  left_join(lit.review.broad) |>
  mutate(percent = round((n/total), 2))


## Prep visuals ----
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
