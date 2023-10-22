# Modify Aud's functions ----
get_started <- function(path = "data_dic", data){

  # create a directory data dic
  dir.create(path)

  # make template for description table
  description_table <- as_tibble(colnames(data)) |>
    rename(Variable_name = value) |>
    mutate(TableID = character(length = length(colnames(data))),
           Description = character(length = length(colnames(data))),
           Unit = character(length = length(colnames(data))),
           "How measured" = character(length = length(colnames(data))))


  # write table
  write_csv(x = description_table, file = paste0(path, "/description_table.csv"))

}

# Aud's function to make data dictionary-----
make_data_dictionary <- function(data, description_table, table_ID, keep_table_ID = FALSE){

  # use function get_range() to get range from characters, numeric variables and dates
  range <- get_range(data)

  # use get_class function to extract class from data
  class <- get_class(data)

  # combine range and class
  range_class <- class |>
    # join with range
    left_join(range, by = "Variable_name")  %>%
    mutate(TableID = {{table_ID}})

  # make dictionary
  dictionary <- bind_rows(
    # join general variables
    range_class %>%
      inner_join(description_table %>%
                   filter(is.na(.data$TableID)) %>%
                   select(-all_of("TableID")), by = "Variable_name"),
    # join special variables with same name but different meaning across datasets
    range_class %>%
      inner_join(description_table %>%
                   filter(.data$TableID == table_ID), by = c("Variable_name", "TableID"))
  ) %>%
    select(all_of(c("TableID", "Variable_name", "Description", "Variable type", "Variable range or levels", "Unit", "How.measured")))

  if(!keep_table_ID){
    dictionary <- dictionary %>%
      select(-all_of("TableID"))

  }

  dictionary

}
## get range from characters, numeric variables and dates
get_range <- function(data){

  # get range from each variable
  range <- data %>%
    as_tibble() %>%
    summarise(
      across(tidyselect::vars_select_helpers$where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.numeric), ~paste(round(min(., na.rm = TRUE), 3),round(max(., na.rm = TRUE), 3), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.Date), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      across(tidyselect::vars_select_helpers$where(is.POSIXct), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
    ) %>%

    # make long table
    pivot_longer(cols = everything(),
                 names_to = "Variable_name",
                 values_to = "Variable range or levels")

  range

}
## get class and make it into a tibble
get_class <- function(data){

  class <- map_dfr(data %>% as_tibble, ~enframe(class(.x)[1], value = "Variable type"),
                   .id = "Variable_name") %>%
    select(-all_of("name")) %>%

    # rename class
    mutate(`Variable type` = case_when(`Variable type` %in% c("character", "logical") ~ "categorical",
                                       `Variable type` %in% c("integer", "numeric") ~ "numeric",
                                       `Variable type` %in% c("Date") ~ "date",
                                       `Variable type` %in% c("POSIXct") ~ "date_time"))

  class

}

# make data dictionary for morpho studies
get_started(data = litreview.percents)
description_table_morpho = read.csv("data_dic/litreview.percents_description_table.csv")
data_dic_morpho <- make_data_dictionary(data = litreview.percents,
                                        description_table = description_table_morpho,
                                        table_ID = "morpho_traits",
                                        keep_table_ID = FALSE)
write.csv(data_dic_morpho, "data_dic/litreview.percents_data_dic.csv")

# make data dictionary for all leaf studies
get_started(data = litreview.percents.broad)
description_table_leaves = read.csv("data_dic/litreview.percents.broad_description_table.csv")
data_dic_leaves <- make_data_dictionary(data = litreview.percents.broad,
                                        description_table = description_table_leaves,
                                        table_ID = "leaves",
                                        keep_table_ID = FALSE)
write.csv(data_dic_leaves, "data_dic/litreview.percents.broad_data_dic.csv")
