## START ======================================================================

# Pipeline produces map showing the areas with the highest host density of
# Lonomia achelous and Lonomia obliqua in South America

## Packages --------------------------------------------------------------------
library(CoordinateCleaner) # clean coordinartes
library(maps)
library(tidyverse) # data manipulation and visualization
library(janitor) # clear table column name
library(leaflet)
library(readxl)
library(sf)
library(sp)
library(eks)
library(cowplot)

## Negate funcion ---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)

## Session info ------------------------------------------------------------------
sessionInfo()

## -------------------------------------------------------------------------------
# Import data
# 'read.csv2' is used to read the file from the specified path
# 'here::here' function constructs paths relative to the project's root directory

# Lonomia occurences:
dt_clean <- read.csv2(here::here("..", "data", "occ_lon_data_clean.csv"))

# Host occurences:
occ_host <- read.csv2(here::here("..", "data", "occ_host_data.csv"))

# import dataframe with lonomia
# and host "relationship":
native_hst <- read_excel(
    here::here("..", "data", "survey_host_data.xlsx"),
    sheet = "survey_info_hosts"
) |>
    # keep only the desired columns:
    # 'host_complete_name', 'native', and 'lonomia_species'
    select(host_complete_name, native, lonomia_species)

# Loading Spatial Data and Merging Datasets
# 'st_read' is used to read spatial data from the specified path
# 'st_transform' function is applied to transform the spatial data to a common
# coordinate reference system (CRS), here CRS is 4326 which
# represents WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS
south <- sf::st_read(dsn = here::here("..", "data", "South_America")) |>
    sf::st_transform(4326)

# -------------------------------------------------------------

# Transforming and Cleaning Process on 'dt_clean' data frame
# Begin transformation with the cleaned data in 'dt_clean'
lonomie <- dt_clean |>
    # select columns 'species', 'long', and 'lat' from the data
    dplyr::select(species, long, lat) |>
    # 'distinct' is used to remove duplicates from the data
    # '.keep_all = true' means all columns are retained
    dplyr::distinct(.keep_all = true) |>
    # 'st_as_sf' converts simple data frames to simple feature objects
    # 'coords = c("long", "lat")' specifies which columns contain the coordinate data
    # 'crs' - Coordinate Reference System, "+proj=longlat +datum=WGS84 +no_defs" is a commonly used CRS
    sf::st_as_sf(
        coords = c("long", "lat"),
        crs = "+proj=longlat +datum=WGS84 +no_defs"
    )

# -------------------------------------------------------------

# Updating 'dt_clean' by converting it into a simple feature object
# This will be useful for spatial analysis or creating spatial visualizations
dt_clean <- dt_clean |>
    # Use 'st_as_sf' to convert the dataframe into an sf (simple features) object
    # 'coords = c("long", "lat")' specifies which columns contain the coordinate data
    # 'crs' - Coordinate Reference System, "+proj=longlat +datum=WGS84 +no_defs" is a commonly used CRS
    sf::st_as_sf(coords = c("long", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")

# -------------------------------------------------------------

## Merge 'occ_host' and 'native_hst' datasets and classify as native or exotic
# We start with the 'occ_host' dataset and use 'left_join' to add data from 'native_hst'
# The join is performed on matching the 'occ_host' dataset's 'Taxon_name' column with 'native_hst'
# dataset's 'host_complete_name' column
# Use 'mutate' to add a new column 'native'. If the original 'native' value was "yes", it is replaced with "Native",
# otherwise, it is replaced with "Exotic"
dt_maps <- occ_host |>
    dplyr::left_join(
        native_hst,
        by = c("Taxon_name" = "host_complete_name")
    ) |>
    dplyr::mutate(native = ifelse(native == "yes", "Native", "Exotic"))

# -------------------------------------------------------------

# Kernell density map
## Initialize an empty list to store kernel maps
kern_maps <- list()

## A loop is initiated, iterating through "achelous" and "obliqua"
for (i in c("achelous", "obliqua")) {
    ## For each iteration, perform the following operations on dt_maps dataframe:
    ## 1. Filter data where 'native' field contains "Native" and 'lonomia_species' field contains current iteration value (i)
    ## 2. Select 'Longitude' and 'Latitude' columns
    ## 3. Convert the filtered dataframe to tibble
    ## 4. Transform the tibble into spatial feature object with specified coordinates and coordinate reference system
    ## 5. Apply Kernel Density Estimation methodology on the spatial feature
    krnlm <- dt_maps |>
        dplyr::filter(
            stringr::str_detect(native, "Native") &
                stringr::str_detect(lonomia_species, i)
        ) |>
        dplyr::select(Longitude, Latitude) |>
        tibble::as_tibble() |>
        sf::st_as_sf(
            coords = c("Longitude", "Latitude"),
            crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
        ) |>
        eks::st_kde()

    ## Perform intersection operation on south with krnlm$sf
    ## Add a new column 'contlabel' by converting existing 'contlabel' values to numeric
    maps <- krnlm$sf |>
        sf::st_intersection(south) |>
        dplyr::mutate(contlabel = as.numeric(contlabel))

    ## Save the processed map into kern_maps list with key as combination of current iterator value and string "_native"
    kern_maps[[paste0(i, "_native")]] <- maps
}

# -------------------------------------------------------------

# Creating a list of Maps using ggplot
# Create an empty list `ggplants` to store the plots
ggplants <- list()

# Loop through each of the names in `kern_maps`
for (i in names(kern_maps)) {
    # For each name, create a plot using ggplot
    figs_plan <- ggplot() +
        # Plot the south american spatial data with grey color
        geom_sf(data = south, fill = "grey80") +
        # Overlay this with the specific map from kern_maps for the current index name 'i'
        # The fill parameter is set to the contlabel attribute of the dataset
        geom_sf(
            data = kern_maps[[i]],
            aes(fill = contlabel),
            alpha = 0.8, color = NA, show.legend = FALSE
        ) +
        # Apply gradient color scale to fill based on the value of contlabel attribute
        scale_fill_gradient2(
            low = "#4EA699", # Color for low values
            mid = "white", midpoint = 50, # Color for mid-range values
            high = "#ff1b6b", # Color for high values
            name = "%"
        ) + # Name of legend
        # Add the borders of South American countries back on top, colored grey
        geom_sf(data = south, fill = NA, color = "grey50", size = 1.3) +
        # Set the limits for x and y coordinates
        xlim(c(-90, -30)) +
        ylim(c(-55, 12)) +
        # Use a light theme for the plot
        theme_light() +
        # Remove axis title, test and ticks for a clean look
        theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()
        )

    # Add the plot to the list of plots
    ggplants[[i]] <- figs_plan
}

# -------------------------------------------------------------

# Reading and transforming data points
# Read the 'H2' sheet from lonomism_southamerica.xlsx dataset located in Scripts/datasets folder
data_points <- read_excel(here::here("data", "survey_lonomism_data.xlsx"), sheet = "H2") |>
    # Select columns 'long' and 'lat'
    dplyr::select(long, lat) |>
    # Rename these columns to 'x' and 'y' respectively
    dplyr::rename(y = lat, x = long) |>
    # Convert dataframe into spatial points
    SpatialPoints() |>
    # Transform spatial points to sf object (Simple Features - modern standard for spatial vector data)
    st_as_sf() |>
    # Create a buffer around each point with distance of 0.5 degrees
    st_buffer(dist = 0.5) |> # ! 0.5 degrees
    # Combine all these buffers into a single shape
    st_union()

# Assigning a Coordinate Reference System (CRS)
# Set the CRS of the data points to WGS 84 (code: 4326), defining this spatial object to use a global latitude-longitude grid
st_crs(data_points) <- 4326

# -------------------------------------------------------------

## Function to create a map visualization

# The function takes three arguments:
# 1. lonomia_name: the name of the Lonomia species.
# 2. lonomia_kern: Kern from the kern_maps corresponding to the provided species name.
# 3. shape_type: The unique shape identifier for the points representing the occ_hosturrences.

map_function <- function(lonomia_name, lonomia_kern, shape_type) {
    # Retrieve the kernel map for the specific Lonomia species
    lonomia_kern <- kern_maps[[lonomia_kern]]

    # Create the map visualization using ggplot
    mapita <- ggplot() +
        # Draw the base map with grey fill
        ggplot2::geom_sf(data = south, fill = "grey") +
        # Overlay the density kernel of the species distribution,
        # and apply a gradient color scale to visualize the density labels
        ggplot2::geom_sf(
            data = lonomia_kern,
            aes(fill = contlabel),
            alpha = 0.8, color = NA, show.legend = TRUE
        ) +
        scale_fill_gradient2(
            low = "#4EA699",
            mid = "white",
            midpoint = 50,
            high = "#ff1b6b",
            name = "🌳 Kernel density"
        ) +
        # Redraw boundaries of base map for clear demarcation
        ggplot2::geom_sf(data = south, fill = NA, color = "grey50", linewidth = 1) +
        # Set range of x and y axes
        xlim(c(-90, -30)) +
        ylim(c(-55, 12)) +
        theme_light() +
        # Plot observed data points
        ggplot2::geom_sf(
            data = data_points, fill = NA,
            linewidth = 1.2, aes(colour = "Buffer")
        ) +
        # Apply color scheme to highlight the buffer areas
        scale_colour_manual(
            values = c("Buffer" = "#FF0000"),
            name = "🌍 Lonomism notifications"
        ) +
        # Add species occ_hosturrence points
        ggplot2::geom_sf(
            data = dt_clean |> filter(species %in% lonomia_name),
            aes(shape = "Lonomia"),
            size = 6,
            fill = "#000000",
            color = "#000000",
            alpha = 0.7
        ) +
        # Specify shape of these occ_hosturrence points
        scale_shape_manual(
            values = c("Lonomia" = shape_type),
            labels = c(lonomia_name),
            name = c("🐛 occ_hosturrences")
        ) +
        # Theme setting and legend ordering and styling
        theme_bw(base_size = 28) +
        theme(
            # Customized visual settings for plot, including text color, legend placement,
            # absence of axis ticks, and more.
            # [various elements omitted]
        ) +
        guides(
            fill = guide_colourbar(title.position = "top", title.hjust = 0, order = 3),
            # The legend details are specified with the given customizations.
            # Ordering is given to arrange the three legends, fill (color bar), colour and shape.
            # [various elements omitted]
        ) +
        # Adds a north arrow to the plot
        ggspatial::annotation_north_arrow(
            location = "tr",
            which_north = "true",
            pad_x = unit(0.25, "in"),
            pad_y = unit(0.25, "in"),
            height = unit(5, "cm"),
            width = unit(5, "cm"),
            style = ggspatial::north_arrow_nautical(
                fill = c("grey40", "white"),
                line_col = "grey20",
                text_size = 20
            )
        )

    return(mapita) # End of function body by returning the created map
}

# Call the function for two species and assign the resulting maps
map_host_ach = map_function("Lonomia achelous", "achelous_native", 24) # Map for Lonomia achelous
map_host_obl = map_function("Lonomia obliqua", "obliqua_native", 25) # Map for Lonomia obliqua


# -------------------------------------------------------------

# Save results
# 'svg' is a function in R to start a new device driver that produces Scalable Vector Graphics (SVG).
# This outputs graphical representations to a file or a connection, in this case a SVG file named
# "fig_hosts_native_time.svg".

# 'width' and 'height' arguments define the size of the image in inches.
# Here, the width and height of the plot are set to 68 inches and 34 inches respectively.

png(here::here("..", "images", "fig_hosts_native_time.svg"), width = 68, height = 34)

# 'print' is used to print the plot object 'with_legend' on the current plotting device,
# which is an SVG file in this case.
# It will output the 'with_legend' plot into the SVG file we created above.

print(with_legend)

# 'dev.off()' is a function that shuts down the current (or specified) device.
# In this case, it turns off the SVG file device that was turned on earlier.
# This is necessary because it finalizes the file for use by adding necessary closing tags etc.

dev.off()

## ----

# Save the current plot to an image using `agg_tiff` from the ragg package.
# The image will be titled 'especies_tempo.tiff', having dimensions of
# 68x34 inches with a resolution of 400 DPI.
ragg::agg_tiff(
    here::here("..", "images", "fig_hosts_native_time.tiff"),
    width = 68, height = 34, units = "in", res = 400
)

with_legend

# Turns off the device that was turned on with ragg::agg_tiff and finalizes the image file.
dev.off()

## END =====================================================================
