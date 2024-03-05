## START ==============================================================================

## Automated Data Acquisition and Cleaning for Host Plant Identification

# - This R script streamlines the search for native host plants of L. achelous and L. obliqua species
# - It automates the download and cleaning of large occurrence datasets from
#   multiple biodiversity databases (BIEN, GBIF, iDigBio, SpeciesLink).
# - It uses help functions that are in the "utils" >> `3.1_functions_download_hosp_data.R"`

## Packages --------------------------------------------------------------------
library(gsheet)
library(tidyverse)
library(janitor)
library(kableExtra)
library(readxl)

## databases
library(BIEN)
library(rgbif)
library(ridigbio)

## Functions -------------------------------------------------------------------
## Helpfull function
`%!in%` <- Negate(`%in%`)
# This function will return the logical negation of the `%in%` (or 'not in')
# The `%in%` operator checks if a value exists in a vector. If the value exists, it returns TRUE; otherwise, it returns FALSE.
# The `%!in%` function we create here does the exact opposite:
# it returns TRUE if a value does not exist in the vector and FALSE if it does.

## Open the functions we use to download the data:
source("3.1_functions_download_hosp_data.R")

## Hosts names for download data -----------------------------------------------
plan <- nombre_hosts()

# Return:
# scientific names: plan$plan
# just genus: plan$gen

## Download occ. data ----------------------------------------------------------

## BIEN data
biendata_sp <- bien_download(plan, species = TRUE)

## GBIF data
gbif_sp <- gbif_download(plan, species = TRUE)

## RifigBio data
idigbio_sp <- ridigbio_download(plan$plan, species = TRUE)

## SpeciesLink data
spslink_sp <- specieslink_download(plan, species = TRUE)

## Merge the databases ---------------------------------------------------------
## Species
sp <- rbind(biendata_sp, gbif_sp, idigbio_sp, spslink_sp) |>
    dplyr::mutate(Taxon_type = "sp")

## Cleaning species names ------------------------------------------------------
## Standardize scientific names and exclude species that are not known hosts
## of L. achelous or L. obliqua

# First, keep the species names that are already correct
cleaned_names_sp1 <- sp %>%
    dplyr::filter(Species %in% plan$plan) # Selects rows where Species is in plan$plan

# 1. Check if there are any missing species from our 'clean' list by checking against plan$plan
# print(plan$plan[plan$plan %!in% cleaned_names_sp$Species])
# 2. Checking for Didymopanax morototoni and similarities in spelling
# sp |> filter(str_detect(Species, "morototoni"))

# Second part: Cleanup on incorrect names
# Start by identifying unique incorrect names
print(sp[sp$Species %!in% plan$plan, ]$Species |> unique())

# Create a 'provisional' data frame where species names are not in plan$plan
# Then perform multiple mutations to clean up incorrect names using string manipulations (removal of extra phrases and characters)
# Finally remove NA data
sp_prov <- sp %>%
    dplyr::filter(Species %!in% plan$plan) %>%
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" cf. "), paste0(" "))) %>% # remove the " cf. " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" cf "), paste0(""))) %>% # remove the " cf " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" x spruceana"), paste0(" "))) %>% # remove " x spruceana" - whit space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" x benthamin"), paste0(" "))) %>% # remove " x benthamin" - whit space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" spruceana x "), paste0(" "))) %>% # remove " spruceana x " - whit space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" × "), paste0(""))) %>% # remove " × " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" ×"), paste0(" "))) %>% # remove " × " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" x "), paste0(""))) %>% # remove " x " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" X "), paste0(""))) %>% # remove " X " - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" x"), paste0(" "))) %>% # remove " x" - with space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" aff. "), paste0(" "))) %>% # remove " aff. " - whit space
    dplyr::mutate(Species = stringr::str_replace(Species, stringr::fixed(" aff "), paste0(" "))) %>% # remove " aff " - whit space
    dplyr::mutate(Species = stringr::str_replace_all(Species, "[^[:alnum:] ]", paste0(" "))) %>% # remove non-alphanumeric symbols from a string
    dplyr::mutate(Species = iconv(Species, from = "UTF-8", to = "ASCII//TRANSLIT")) %>% # convert accented characters to unaccented
    dplyr::mutate(Species = stringr::word(Species, 1, 2)) %>% # genus and epipetum only
    dplyr::mutate(Species = stringr::str_to_sentence(Species)) %>% # Capitalize the firt word and lowcase the rest
    tidyr::drop_na(Species) # remove NA data

# Check for unique species names not in plan$plan after cleaning
print(sp_prov[sp_prov$Species %!in% plan$plan, ]$Species |> unique())

# Some adjustments previously made by hand
# sp_prov[sp_prov == "Albizia niopioides"] <- "Albizia niopoides"
# sp_prov[sp_prov == "Casearia silvestris"] <- "Casearia sylvestris"
# sp_prov[sp_prov == "Casearia sylvestri"] <- "Casearia sylvestris"
# sp_prov[sp_prov == "Casearia decandrae"] <- "Casearia decandra"
# sp_prov[sp_prov == "Cedrella fissilis"] <- "Cedrela fissilis"
# sp_prov[sp_prov == "Erythrina crista"] <- "Erythrina cristagalli"
# sp_prov[sp_prov == "Erythrina cristagalli"] <- "Erythrina cristagalli"
# sp_prov[sp_prov == "Erythrina crista-galli"] <- "Erythrina cristagalli"
# sp_prov[sp_prov == "Lithrea brasiliensis"] <- "Erythrina cristagalli"
# sp_prov[sp_prov == c("Lithraea moleoides", "Lithrea molleoides")] <- "Lithraea molleoides"
# sp_prov[sp_prov == "Luehea nf"] <- "Luehea divaricata"

# Here, more specific corrections are made on species names based on certain conditions.
sp_prov <- sp_prov |> dplyr::mutate(
    Species = dplyr::case_when(
        stringr::str_detect(Species, "^Sche..+morot..|^Dyd..+mor|^Did..+mor") ~ "Didymopanax morototoni",
        stringr::str_detect(Species, "^Alb...+nio") ~ "Albizia niopoides",
        stringr::str_detect(Species, "^Case..+s(i|y)lvestri(\\b|s)") ~ "Casearia sylvestris",
        stringr::str_detect(Species, "^Case...+dec...") ~ "Casearia decandra",
        stringr::str_detect(Species, "^Ced...+fis...") ~ "Cedrela fissilis",
        stringr::str_detect(Species, "^Ery..+cri..|^Lit..+bras..") ~ "Erythrina cristagalli", ## ! pareiqui
        stringr::str_detect(Species, "^Lith..+mol..") ~ "Lithraea molleoides", ## ! pareiqui
        stringr::str_detect(Species, "^Lu..+div..") ~ "Luehea divaricata", ## ! pareiqui

        TRUE ~ Species
    )
)

# Combine `cleaned_names_sp1` and `sp_prov`, rename the Species column to Taxon_name
# Convert Latitude and Longitude to numeric, remove NA values and convert to tibble
cleaned_names_sp <- sp_prov |>
    dplyr::filter(Species %in% plan$plan) |>
    rbind(cleaned_names_sp1) |>
    dplyr::rename(Taxon_name = Species) |>
    dplyr::mutate_at(dplyr::vars(Latitude, Longitude), as.numeric) |>
    tidyr::drop_na(c(Latitude, Longitude)) |>
    tibble::as_tibble()

print(cleaned_names_sp |> head())

# Save
write.csv2(cleaned_names_sp, here::here("..", "data", "occ.csv"))


## END ==============================================================================
