## START =====================================================================

# Base functions for pipeline 3.2.

# Functions for downloading occurrences of Lonomia host plants from databases.

## Packages --------------------------------------------------------------------
# library(gsheet)
# library(tidyverse)
# library(janitor)
# library(kableExtra)
# library(readxl)

# ## databases
# library(BIEN)
# library(rgbif)
# library(ridigbio)

## TODO: IMPROVE FUNCTION DESCRIPTIONS

## Hosts names -----------------------------------------------------------------
nombre_hosts <- function() {
  ## Host vs. Lonomia data
  sps <- readxl::read_xlsx(here::here("..", "data", "survey_host_data.xlsx"), sheet = "survey_info_hosts") |>
    dplyr::mutate(host_species = host_complete_name)

  ## Species of Lonomia for which we are going to search for host data
  lon <- c("Lonomia achelous", "Lonomia obliqua")

  ## List to store the species name
  names_plan <- list()

  ## Species level data
  names_plan[["plan"]] <- sps$host_complete_name

  ## Genus level data
  names_plan[["gen"]] <- sps$genus

  ## List with names
  return(names_plan)
}


## BIEN data -------------------------------------------------------------------

# Function accepts a plan and a species flag (TRUE by default)
bien_download <- function(plan, species = TRUE) {
  ## Creates an empty data frame to store the fetched data
  biendata <- data.frame()

  ## Checks whether the species flag is set to TRUE
  if (species == TRUE) {
    # Iterates over the plan items
    for (i in plan$plan) {
      # Displays a message indicating that a request is being made to bien for the current plan item
      message("Making request to bien for ", i)

      # Fetches occurrences for the current species from the new world
      # and selects only the scrubbed_species_binomial, latitude, and longitude fields
      sp <- BIEN_occurrence_species(
        species = i,
        new.world = TRUE
      ) %>%
        dplyr::select(scrubbed_species_binomial, latitude, longitude)

      # Appends rows of the downloaded info to our data frame 'biendata'
      biendata <- rbind(biendata, sp)

      # Prints a completion message after each iteration
      message("Done!")
    }
    # Renames columns of biendata to Species, Latitude and Longitude
    names(biendata) <- c("Species", "Latitude", "Longitude")

    # Print courtesy message
    message("Dataset ready")

    # Returns the populated dataframe
    return(biendata)
  } else {
    # If species isn't set to TRUE, it downloads genus data instead.
    for (i in plan$gen) {
      # Displaying a message about the ongoing request
      message("Making request to bien for ", i)

      # Downloads genus occurrence and specifically selects certain fields
      gen <- BIEN_occurrence_genus(
        genus = i,
        new.world = TRUE
      ) %>%
        select(scrubbed_genus, latitude, longitude)

      # Appends rows to the biendata
      biendata <- rbind(biendata, gen)

      # Print successful message after each request
      message("Done!")
    }

    # Renames columns of biendata to Genus, Latitude and Longitude
    names(biendata) <- c("Genus", "Latitude", "Longitude")

    # Print courtesy message
    message("Dataset ready")

    # Returns the dataframe
    return(biendata)
  }
}

## GBIF data -------------------------------------------------------------------
# Define a function that downloads data from GBIF (Global Biodiversity Information Facility)
gbif_download <- function(plan, species = TRUE) {
  ## Initialize an empty list to store our dataframes
  gbif_list <- list()

  ## Specify condition for downloading occurrences
  if (species == TRUE) {
    # Iterate through each plan
    for (i in seq_along(plan$plan)) {
      # Message to inform the user of current request
      message("Making request to GBIF for ", plan$plan[i])

      # Download occurrence data using GBIF API
      dt <- occ_data(
        scientificName = plan$plan[i],
        hasCoordinate = TRUE,
        decimalLatitude = c("-60,15"),
        decimalLongitude = c("-90,-30"),
        limit = 50000
      )

      # If data exists for this scientific name...
      if (!is.null(dt$data)) {
        # ...select only the needed columns
        dt2 <- dt$data %>%
          select(scientificName, decimalLatitude, decimalLongitude)

        # Stores each dataframe in a list
        gbif_list[[i]] <- dt2
        # Completion message
        message("Done!")
      }
    }

    # rbind all dataframes in the list into one dataframe
    gbif_data <- do.call(rbind, gbif_list)
    # Rename the column names
    names(gbif_data) <- c("Species", "Latitude", "Longitude")
    # Inform the user that the dataset is ready
    message("Dataset ready")

    # If we're searching based on genus instead of individual species...
  } else {
    # Iterate through each genus
    for (i in seq_along(plan$gen)) {
      # Message to inform the user of current request
      message("Making request to GBIF for ", plan$gen[i])

      # Download occurrence data using GBIF API
      dt <- occ_data(
        search = plan$gen[i],
        hasCoordinate = TRUE,
        continent = "south_america",
        limit = 50000
      )

      # If data exists for this genus...
      if (!is.null(dt$data)) {
        # ...select only the needed columns
        dt2 <- dt$data %>%
          select(scientificName, decimalLatitude, decimalLongitude)

        # Stores each dataframe in a list
        gbif_list[[i]] <- dt2
        message("Done!")
      }
    }

    # rbind all dataframes into one dataframe
    gbif_data <- do.call(rbind, gbif_list)
    # Rename the column names
    names(gbif_data) <- c("Genus", "Latitude", "Longitude")
    # Inform the user that the dataset is ready
    message("Dataset ready")
  }

  # Return the final dataset
  return(gbif_data)
}


## RifigBio data ---------------------------------------------------------------

# Define a function to download data from iDigBio
ridigbio_download <- function(plan, species = TRUE) {
  ## Initialize an empty dataframe to store our data
  idigbio_data <- data.frame()

  ## Specify condition for downloading occurrence records
  if (species == TRUE) {
    # Iterate through each plan
    for (i in plan$plan) {
      # Message to inform the user of current request
      message("Making request to RifigBio for ", i)

      # Search for records using the iDigBio API
      sp <- idig_search_records(rq = list(
        scientificname = i,
        geopoint = list(type = "exists")
      )) %>%
        # Select only the needed columns
        select(scientificname, geopoint.lon, geopoint.lat)

      # Append the new records to our existing dataframe
      idigbio_data <- rbind(idigbio_data, sp)

      # Confirmation message
      message("Done!")
    }

    # Rename column names
    names(idigbio_data) <- c("Species", "Latitude", "Longitude")
    message("Dataset ready")

    # Return the final dataset
    return(idigbio_data)
  } else {
    # If we're not searching based on individual species
    for (i in plan$gen) {
      # Message to inform the user of current request
      message("Making request to RifigBio for ", i)

      # Search for records using the iDigBio API
      gen <- idig_search_records(rq = list(
        genus = i,
        geopoint = list(type = "exists")
      )) %>%
        # Select only the needed columns
        select(genus, geopoint.lon, geopoint.lat)

      # Append the new records to our existing dataframe
      idigbio_data <- rbind(idigbio_data, gen)

      # Confirmation message
      message("Done!")
    }

    # Rename column names
    names(idigbio_data) <- c("Genus", "Latitude", "Longitude")

    message("Dataset ready")
    # Return the final dataset
    return(idigbio_data)
  }
}


## SpeciesLink data ------------------------------------------------------------
# ! Based on the 'rocc' package
# (is no longer available in R-CRAN and does not work with this R version)

# Define function for downloading species data
specieslink_download <- function(plan, species = TRUE) {
  # Inner function to create URL for getting species data
  rspeciesLink <- function(species_sl = NULL,
                           MaxRecords = NULL) {
    # Base URL for API
    my_url <- "https://api.splink.org.br/records/"

    # A helper function to prepare portions of URL query
    url_query <- function(vector, name) {
      paste0(name, "/", paste(paste0(vector, "/"), collapse = ""))
    }

    # If species is provided, include it in URL
    sp <- ifelse(is.character(species_sl),
      url_query(gsub(" ", "%20", species_sl), "scientificName"), my_url
    )

    # Append XY coordinates and plants scope to our URL
    xy <- url_query("Yes", "Coordinates") ## With coordinates
    sc <- url_query("plants", "Scope") ## scope is plants
    my_url <- paste0(my_url, sp, xy, sc)

    # If max records number is provided, include it in URL
    if (!is.null(MaxRecords) && is.numeric(MaxRecords)) {
      mr <- url_query(MaxRecords, "MaxRecords")
      my_url <- paste0(my_url, mr)
    }

    # Return constructed URL
    return(my_url)
  }

  # Loop over every plan in the plans list, download the data and save it into a list of dataframes
  results <- lapply(plan$plan, function(i) {
    link <- rspeciesLink(i)
    my_url <- paste0(link, "Format/JSON/")

    message("Making request to speciesLink for ", i)

    df <- jsonlite::fromJSON(my_url)$result

    if (!is_empty(df)) {
      df <- df %>%
        select(
          scientificName = ifelse(species, "scientificName", "genus"),
          decimalLatitude, decimalLongitude
        )
    }

    message("Done!")

    # Return downloaded data as dataframe
    return(df)
  })

  # Combine all the dataframes from the list into one dataframe
  dt_specieslink <- bind_rows(results)

  # Rename the columns of the final dataframe
  names(dt_specieslink) <- c(ifelse(species, "Species", "Genus"), "Latitude", "Longitude")

  message("Dataset ready")

  # Return final complete dataframe
  return(dt_specieslink)
}

## -----------------------------------------------------------------------------
## Coming to species that may have inconsistent distributions
incon_sps <- function(geo_clean) {
  ## host data
  # (we will use the data in another function)
  native_hst <- read_excel(here::here("..", "data", "survey_host_data.xlsx"),
    sheet = "survey_info_hosts"
  ) |>
    select(host_complete_name, native, lonomia_species) |>
    filter(native == "yes")

  ## Lonomie data
  lonomies <- rio::import("https://docs.google.com/spreadsheets/d/1PzJdUGWxhmMIk8-iGw9YqTbpHNNTPvCMUJOnHefPZD0/edit?usp=sharing") |>
    drop_na(long, lat) %>%
    mutate(specie_orig = case_when(
      specie_orig == "Lonomia diabolus" ~ "Lonomia achelous",
      specie_orig == "Lonomia paraobliqua" ~ "Lonomia obliqua",
      TRUE ~ specie_orig
    )) |>
    filter(str_detect(specie_orig, "achelous|paraobliqua|obliqua")) |>
    select(specie_orig, long, lat) |>
    distinct(.keep_all = TRUE) |>
    st_as_sf(
      coords = c("long", "lat"),
      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    )

  occ_lonomies <- lonomies |>
    group_by(specie_orig) |>
    summarize(geometry = st_union(geometry)) |>
    st_convex_hull()

  ## Host data
  occ_plan <- geo_clean |>
    as_tibble() |>
    st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    ) |>
    group_by(Taxon_name) |>
    summarize(geometry = st_union(geometry)) |>
    st_convex_hull()

  results <- list()


  ## Inconsistent distribution
  inst_sps <- st_join(occ_plan, occ_lonomies, join = st_intersects) |>
    dplyr::filter(!is.na(specie_orig)) |>
    mutate(specie_orig = ifelse(str_detect(specie_orig, "paraobliqua"), "Lonomia obliqua", specie_orig)) |>
    mutate(pres = 1) |>
    pivot_wider(names_from = "specie_orig", values_from = "pres", values_fn = sum) |>
    select(-geometry) |>
    group_by(Taxon_name) |>
    summarize_if(is.numeric, sum, na.rm = TRUE) |>
    mutate_if(is.numeric, as.logical) |>
    left_join(native_hst, by = c("Taxon_name" = "host_complete_name")) |>
    filter(native %in% "yes" & `Lonomia obliqua` == FALSE | native %in% "yes" & `Lonomia achelous` == FALSE)

  results[["inconsistent_distribution"]] <- inst_sps

  message("Done for inconsistent distribution!")

  ## Do all host species contain distribution data?
  occ_host <- unique(occ_plan$Taxon_name) |> str_c(collapse = "|")
  gen_host <- unique(occ_plan$Taxon_name) |>
    word() |>
    str_c(collapse = "|")
  epi_host <- unique(occ_plan$Taxon_name) |>
    word(2) |>
    na.omit() |>
    str_c(collapse = "|")

  teste <- native_hst |>
    filter(str_detect(host_complete_name, occ_host, negate = T)) |>
    filter(str_detect(host_complete_name, gen_host, negate = T))

  results[["without_occ"]] <- teste

  message("Done for NA occ data! 👌")

  message("End of analysis!")

  return(results)
}

## END ==================================================================
