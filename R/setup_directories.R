# Setup Directories --------------------------------------------------
source("R/functions.R")

for (year in 2017:2019) {
  data_dir <- set_data_dir(year)
  sensitive_intermediates_dir <- set_intermediates_dir(year)

  create_dirs(data_dir = data_dir, sensitive_intermediates_dir = sensitive_intermediates_dir)
}
