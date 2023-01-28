
library(fs)
library(here)
library(pdftools)
library(magick)
library(tesseract)
library(tidyverse)

# Config file
cfg <- read.csv(here("data", "party_return_config.csv"))


# PDF Manipulation --------------------------------------------------------

# Split pdf file to necessary pages
walk2(cfg$return_file, cfg$page_subset, ~ {
    pdf_subset(here("data", "pdfs", .x), eval(parse(text = .y)))
    file_delete(here("data", "pdfs", .x))
    file_move(here("data", "pdfs", str_replace(.x, ".pdf", "_output.pdf")), here("data", "pdfs", .x))
})


# Image Creation and Manipulation -----------------------------------------

# Image creation
walk(cfg$return_file, ~ {
    pdf_convert(here("data", "pdfs", .x), dpi = 1000, filenames = , verbose = FALSE)
    cat(paste0(which(cfg$return_file == .x), "/", length(cfg$return_file), ": ",   .x, "\n"))
    gc()
})

# Manipulate pngs to high contrast grayscale and move to png folder
pngs <- list.files(pattern = ".png", full.names = TRUE)
walk(pngs, ~ {
    image_read(.x) |> 
        image_convert(type = "grayscale") |> 
        image_contrast(sharpen = 1) |> 
        image_write(.x)
    
    file_move(.x, here("data", "pngs"))
    gc()
})


# Format File Names -------------------------------------------------------

# Rename png files with time in name
pngs <- list.files("data/pngs", pattern = " \\d\\.\\d{2}\\.\\d{2} PM.png", full.names = TRUE)
walk(pngs, ~ file_move(.x, str_replace(.x, " \\d\\.\\d{2}\\.\\d{2} PM.png", ".png")))

# Format numbers in file names
pngs <- list.files("data/pngs", full.names = TRUE)
replacements <- str_extract(pngs, "_\\d{1,2}") |> 
    unique() |> 
    str_remove("_") |> 
    str_pad(2, "left", pad = 0) |> 
    str_pad(3, "left", pad = "_") |> 
    paste0(".png") |> 
    setNames(paste0(unique(str_extract(pngs, "_\\d{1,2}")), ".png"))

walk2(pngs, str_replace_all(pngs, replacements), ~ file_move(.x, .y))


# OCR ---------------------------------------------------------------------
 
pngs <- list.files("data/pngs")
image_text <- map(set_names(pngs), ~ ocr(here("data", "pngs", .x)))
image_text_confidence <- map_dfr(set_names(pngs), ~ ocr_data(here("data", "pngs", .x)), .id = "image")
 

# Cleanup -----------------------------------------------------------------

rm(list = c("pngs", "cfg", "replacements"))
