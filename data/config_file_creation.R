
library(fs)
library(tidyverse)


# Rename PDFs -------------------------------------------------------------

# Remove "-Amended" from any file name
rename_files <- str_subset(list.files("data/pdfs", full.names = TRUE), "-Amended")
walk(rename_files, ~ file_move(.x, str_remove(.x, "-Amended")))
    

# Config File Creation ---------------------------------------------------

return_file <- list.files("data/pdfs")
party <- str_remove(str_extract(return_file, "[:graph:]*-Annual"), "-Annual")
year <- str_extract(return_file, "[:digit:]+")

write_csv(tibble(party, year, return_file), "./data/party_return_config.csv")
