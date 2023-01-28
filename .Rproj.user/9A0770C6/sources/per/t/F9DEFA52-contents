
library(tidyverse)
library(tidytext)
library(lexicon)



# Helpful string regex ----------------------------------------------------

st_suffixes <- "road|rd|street|st|avenue|ave|boulevard|blvd|lane|ln|drive|dr|way|court|ct|plaza|plz|terrace|ter|place|pl|bay|crescent|cres|cr|trail|trl|turnpike|tpke|parkway|pkwy|causeway|cswy|row|beltway|bltway|crossing|xing|alley|aly|point|pt|pike|square|sq|landing|lndg|circle|cir|valley|val"
date_string <- "\\d{1,2}/\\d{1,2}/\\d{4}"
money_string <- "\\$ ?\\d+( |,)\\d+"
benefactor_string <- "\\w+,( \\w+){0,}? \\w+ -"

name_clean <- function(.df, .col){
    filter(.df, length({{ .col }}) >= 4, prop > 0.0005) |> 
        mutate(name = str_to_lower({{ .col }})) |> 
        pull(name)
}

names_string <- c(name_clean(freq_first_names, Name), name_clean(freq_last_names, Surname)) |> 
    paste0(collapse = "|")

# -------------------------------------------------------------------------

# x <- image_text[str_detect(names(image_text), "^Labour")]
df_raw <- tibble(image=names(image_text), text=unlist(image_text)) |> 
    mutate(
        party = str_remove(str_extract(image, "[:graph:]*-Annual"), "-Annual")
        , image = str_remove(str_extract(image, "\\d{2}.png"), ".png") |> as.numeric()
        , .before = everything()
    ) |>  
    unnest_lines(text, text) 


# -------------------------------------------------------------------------


df <- filter(df_raw, image == 2) |> 
    mutate(tags = case_when(
        str_detect(text, "^(.{2,3})?part [a-i]") 
            & lead(image, 5) != image + 1 
                ~ str_extract(text, "part [a-i]")
        , str_detect(str_remove(text, "[:punct:]"), "^((donor|contributor|overseas contributor)s )?name and") 
            | str_detect(text, "(donation|payment) received")
                ~ "table_start"
        , lead(image) == image + 1 
            | is.na(lead(image)) 
            | lead(image) < image
            | lead(party, 2) != party
                ~ "table_end"
    )) |> 
    mutate(filt_ass = tags) |> 
    fill(filt_ass) |>  
    mutate(filt_ass = if_else(filt_ass == "table_start" & is.na(tags), "table", filt_ass))  |> 
    filter(!(is.na(tags) & str_detect(filt_ass, "part"))) |>  
    mutate(
        # Benefactor not right for Green
        benefactor = if_else(
            str_detect(text, names_string)
            # str_detect(text, glue::glue("\\d+( st)? \\w+ {st_suffixes}"))
                # & str_count(text, ",") >= 2 
            , TRUE, FALSE
        )
        , date_pres = str_detect(text, date_string)
        , part = ifelse(str_detect(tags, "part"), tags, NA)
    ) |> 
    fill(part) |> 
    filter(benefactor | date_pres) |> 
    mutate(
        date = as.Date(str_extract(text, date_string), format = "%d/%m/%Y")
        , amount = str_extract_all(text, money_string)
        , amount = str_remove_all(amount, "[:punct:]")
        , amount = str_remove(amount, "\\$")
        , amount = as.numeric(str_remove_all(amount, " "))
        , benefactor = str_extract(text, names_string)
        , benefactor = str_squish(str_remove(benefactor, "-"))
    ) 
    fill(amount, .direction = "up") |> 
    fill(benefactor)
    mutate(benefactor = ifelse(str_detect(part, "c|f"), paste0("unknown_", row_number()), benefactor)) |> 
    group_by(party, part, benefactor) |> 
    summarise(donation_count = n(), donation_total = mean(amount), .groups = "drop") |> 
    select(party, part, benefactor, starts_with("donation"))
