# Download data using this script
# Use RStudio's handy script menu to jump to the data you need
# You should only need to download data once, unless you know they have been updated
# If you've been away from the project for awhile, you should download them again
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Clean trait data ----
get_file(
  # Which repository is it in?
  node = "f4v9t",
  # Which file do you want?
  file = "DURIN_clean.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "Vegetation/clean_data")

# Clean database data ----
get_file(
  # Which repository is it in?
  node = "qp62y",
  # Which file do you want?
  file = "DURIN_database.data.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "Database/clean_data")
