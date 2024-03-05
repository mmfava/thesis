# START =========================================================================

# ATENTION: ! DONT RUN !

# The objective of this pipeline is to obtain coordinates and clean the survey data of
# Lonomia achelous and Lonomia obliqua occurrences in South America.

# Due to the presence of sensitive data in the original dataset (victims names and adresses),
# the database used for these analyzes will not be shared.

## Negate funcion ---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)

## Session info ------------------------------------------------------------------
sessionInfo()

# Import Lonomia data with coordinates and addresses
# Select Lonomia achelous, obliqua and paraobliqua data
df_lonomia_original <- rio::import("https://docs.google.com/spreadsheets/d/1PzJdUGWxhmMIk8-iGw9YqTbpHNNTPvCMUJOnHefPZD0/edit?usp=sharing") |>
    # Cleaning column names for better readability and format consistency
    janitor::clean_names() |>
    # Recode certain species names in the 'specie_orig' column ('Lonomia diabolus' to 'Lonomia achelous')
    dplyr::mutate(specie_orig = recode(specie_orig, "Lonomia diabolus" = "Lonomia achelous")) |>
    # Filter rows based on the presence of specific substrings in the 'specie_orig' column
    dplyr::filter(str_detect(specie_orig, "achelous|paraobliqua|obliqua")) |>
    # Remove the 'specie' column from the data frame
    dplyr::select(-specie) |>
    # Rename the 'specie_orig' column to 'species'
    dplyr::rename(species = specie_orig) |>
    # Apply the next mutations on a per-row basis
    dplyr::rowwise() |>
    # Create a new 'addrs' column:
    # concatenates different location-related fields if
    # 'use_coordinates' is 'possible', else it's NA
    dplyr::mutate(addrs = if_else(
        use_coordinates %in% "possible",
        gsub(
            "^,+(,+|s|-)*", "",
            paste(location, municipality_department, province_state, country, sep = ", ")
        ),
        NA
    ))

# groupby (use_coordinates)
# * 2690 as no-possible
# * 90 as possible
# * 489 as "yes" for the presence of coordinates

# --------------------------------

# Obtain Coordinates from address
# Perform geocoding on the 'addrs' field using
# OpenStreetMap (osm) method via tidygeocoder
# (in other words, obtain the coordinates from the addresses)
df_lonomia_geocode_adress <- df_lonomia_original |>
    tidygeocoder::geocode(addrs, method = "osm")

# --------------------------------

# Obtain Coordinates from city/state

# Flag the occurences without coordinates
# The "1's" is used in the SDM modelling
df_lonomia_flag_no_coordinates <- df_lonomia_geocode_adress |>
    dplyr::rename(lat = `lat...7`, long = `long...8`) |>
    # Replace missing lat and long values with corresponding values
    # from `lat...18` and `long...19`
    dplyr::mutate(
        lat = if_else(is.na(lat) & !is.na(`lat...18`), `lat...18`, lat),
        long = if_else(is.na(long) & !is.na(`long...19`), `long...19`, long)
    ) |>
    # Add a new column 'coord_model' which is 1 if both lat and long are not NA, otherwise it's 0
    dplyr::mutate(coord_model = if_else(!is.na(lat) & !is.na(long), 1, 0))

# group_by(coord_model):
# * 0 = 2746
# * 1 = 523

# Perform geocoding on city, state, and country columns using
# the OpenStreetMap (osm) method via tidygeocoder
# (used with the host distribution)
df_lonomia_geocode_cities <- df_lonomia_flag_no_coordinates |>
    tidygeocoder::geocode(
        city = municipality_department,
        state = province_state,
        country = country,
        method = "osm"
    )

# Clening the data
df_lonomia_tidy_data <- df_lonomia_geocode_cities |>
    # Rename the resulting `lat...7` and `long...8` back to 'lat' and 'long'
    dplyr::rename(lat = `lat...7`, long = `long...8`) |>
    # Again replaces the missing lat and long values now with the corresponding values from `lat...21` and `long...22`
    dplyr::mutate(
        lat = dplyr::if_else(is.na(lat) & !is.na(`lat...21`), `lat...21`, lat),
        long = dplyr::if_else(is.na(long) & !is.na(`long...22`), `long...22`, long)
    )

# --------------------------------

# Now obtain the adress, municipalities and so on based
# on the coordinates
# (Used to validate some conversions/information manually)
df_lonomia_see_adresses <- df_lonomia_tidy_data |>
    tidygeocoder::reverse_geocode(
        lat = "lat",
        long = "long",
        method = "osm",
        full_results = TRUE
    )
# Perform reverse geocoding on 'lat' and 'long' fields using OpenStreetMap (osm) method via tidygeocoder
# full_results=TRUE gets the complete results from reverse geocoding

# 'species''location''use_coordinates''municipality_department''province_state''country...6''lat...7''long...8''month'
# 'year''taxonomic_identification_type''taxnomer_who_identified''reference''reference_type''more_info''dropdown''addrs''lat...18'
# 'long...19''coord_model''lat...21''long...22''address''place_id''licence''osm_type''osm_id''osm_lat''osm_lon''class''type'
# 'place_rank''importance''addresstype''name''road''hamlet''village''state_district''state''ISO3166-2-lvl4''country...42'
# 'country_code''boundingbox''town''municipality''isolated_dwelling''neighbourhood''city_district''shop''house_number'
# 'postcode''industrial''residential''city''county''man_made''region''suburb''amenity''club''retail''historic''farm''office'
# 'leisure''tourism''ISO3166-2-lvl3''natural''highway''locality''farmyard''quarter''district'

# --------------------------------

# Save the occurrence data of interest with less
# precision than the actual used in the thesis analysis
# (to prevent leakage of sensitive data, such as addresses of victims of lonomsm).
dt_lonomia_occ <- df_lonomia_see_adresses |>
    # Select just the interesting columns
    dplyr::select(
        species,
        coord_model,
        lat = `lat...7`,
        long = `long...8`,
        year,
        country = `country...6`,
    ) |>
    # Round the coordinate to three
    dplyr::mutate(lat = round(lat, 3), long = round(long, 3))

# clean_coordinates function is used to perform various tests and corrections on geographic coordinate datasets
# species: Column name containing species names, in this case, it's "species"
# lon: Column name containing longitude values, in this case, it's "long"
# lat: Column name containing latitude values, in this case, it's "lat"
# tests: The tests to be applied. In this case 'equal', 'seas', and 'zeros' tests are applied
# equal: Checks for records where latitude equals longitude
# seas: Checks for records located in the sea
# zeros: Checks for records with zero as coordinates
# value: Determines if the cleaned or original dataset should be returned. Here we specify 'clean' so that the cleaned dataset will be returned
# report: If TRUE, it generates a report of performed tests, otherwise not. Here it is set to FALSE, so no report will be generated
dt_clean <- df_lonomia_see_adresses |>
    # Select just the interesting columns
    dplyr::select(
        species,
        coord_model,
        lat = `lat...7`,
        long = `long...8`,
        year,
        country = `country...6`
    ) |>
    dplyr::filter(!is.na(lat), !is.na(long)) |>
    CoordinateCleaner::clean_coordinates(
        species = "species",
        lon = "long",
        lat = "lat",
        tests = c("equal", "seas", "zeros"),
        value = "clean",
        report = FALSE
    )

# Save the dataframes
dt_lonomia_occ |> write.csv2(here::here("..", "data", "occ_lon_data.csv"))
dt_clean |> write.csv2(here::here("..", "data", "occ_lon_data_clean.csv"))

# END =========================================================================
