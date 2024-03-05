## START ======================================================================

# Script describes the characteristics and types of use of
# Lonomia achelous and Lonomia obliqua hosts.

## Packages --------------------------------------------------------------------
library(tidyverse) # data manipulation and visualization
library(janitor) # clear table column name
library(magrittr) # pip types
library(gtsummary) # result tables
library(gt) # change table format
library(gtExtras)
library(IRdisplay)

## Negate funcion ---------------------------------------------------------------
`%!in%` <- Negate(`%in%`)

# No warnings ------------------------------------------------------------------
options(warn = -1)

# Session info ------------------------------------------------------------------
sessionInfo()

## host data --------------------------------------------------------------------

# (we will use the data in another function)
status_hst <- readxl::read_excel(here::here("..", "data", "survey_host_data.xlsx"),
    sheet = "survey_info_hosts"
) |>
    clean_names() |>
    select(-latex) |>
    mutate(native = ifelse(native == "yes", 1, 0)) |>
    mutate(one_antro_use = +any(str_detect(c_across(use_popular_medicine:use_agroforestry), "yes"))) |>
    mutate(sps_name = paste0(host_complete_name, " ", canonical_author))

names(status_hst)

# Define the function named `add_stat_pairwise_cs`
add_stat_pairwise_cs <- function(data, variable, by, tbl, ...) {
    # Extract the column specified by 'variable' from the dataframe 'data'
    d <- data[[variable]]

    # Perform a chi-squared test on the non-null values of the selected data column
    e <- chisq.test(table(na.omit(d)), simulate.p.value = TRUE, B = 1000)
    # Get the length of residuals from the chi-squared test
    l <- length(e$residuals)

    # Initialize class_ress with default value "-"
    class_ress <- c("-")

    # Check if the length of residuals is not equal to 1
    if (l != 1) {
        # Define a helper function `fq_class` to assign symbols based on conditions
        fq_class <- function(res, p_value) {
            # If p-value < 0.05 and residual is negative, return "▼"
            # If p-value < 0.05 and residual is positive, return "▲"
            # Else, return "-"
            case_when(
                p_value < 0.05 & res < -1.96 ~ "▼",
                p_value < 0.05 & res > 1.96 ~ "▲",
                TRUE ~ "-"
            )
        }

        # Call `fq_class` with residuals and p-value of the chi-squared test result
        class_res <- fq_class(e$residuals, e$p.value)
    }

    # Return a tibble with the calculated residuals as its content
    return(tibble(residuals = class_res))
}

## ---------------------------------------------------------------------------------

# Define a function named `add_stat_pairwise_csp` with parameters data,
# variable, by, tbl and other potential arguments (...)
add_stat_pairwise_csp <- function(data, variable, by, tbl, ...) {
    # Extract the column specified by 'variable' from the dataframe 'data'
    d <- data[[variable]]

    # The pipe operator ('|>') sequentially passes the result of one operation to the next.
    e <- d[!is.na(d)] |>
        table() |>
        chisq.test(simulate.p.value = TRUE, B = 1000)

    # Create a new tibble (a type of DataFrame in R), with a single row and column.
    # The column name is "p", and the value in this column is the p-value from the chi-squared test.
    tibble("p" = e$p.value)
}

## ---------------------------------------------------------------------------------

# Define how types will be represented in the output summary,
# here we're considering all categorical variables as 'categorical'
type <- list(all_categorical() ~ "categorical")

#  Define labels for variables
label <- list(
    use_popular_medicine ~ "Popular medicine",
    use_consume_fruits ~ "Edible fruits",
    use_fruits_commercialized ~ "Marketable fruits",
    use_energy_generation ~ "Power generation (coal or firewood)",
    use_of_wood ~ "Commercial wood",
    use_for_paper ~ "Paper",
    use_urban_afforestation ~ "Urban afforestation",
    use_agroforestry ~ "Agroforestry"
)

# This function generates a summarized data table for given status history and species
antrop_desc <- function(status_hst, sp) {
    # Filter rows with matching species and select relevant columns
    status_hst |>
        filter(str_detect(lonomia_species, sp)) |>
        select(
            use_popular_medicine, use_consume_fruits,
            use_fruits_commercialized, use_energy_generation,
            use_of_wood, use_for_paper, use_urban_afforestation,
            use_agroforestry
        ) |>
        # Create a summary table with customized type and label settings
        tbl_summary(
            missing = "no",
            type = type,
            label = label
        ) |>
        # Add additional pairwise crosstab statistics to the summary
        add_stat(
            fns = all_categorical() ~ add_stat_pairwise_cs,
            location = everything() ~ "level"
        ) |>
        # Add a second set of pairwise crosstab statistics to the summary
        add_stat(fns = all_categorical() ~ add_stat_pairwise_csp) |>
        # Modify the headers in the report
        modify_header(
            p = "**p-value**", # The header for the p-value column
            label = "**variables**", # The header for the variables column
            residuals = "**freq. class.**" # Header for frequency class column
        ) |>
        # Modify the footnotes in the report
        modify_footnote(
            p ~ "Chi-Square Goodness of Fit Test (α = 0.05)", # Footnote for the Chi-square test
            residuals ~ "Frequency classification" # Footnote for frequency classification
        ) |>
        # Modify how p-values are presented in the table, limit to 3 decimal points
        modify_fmt_fun(
            update = p ~ function(x) style_pvalue(x, digits = 3)
        )
}

## ---------------------------------------------------------------------------------

# Define a function to create a summary table describing distributions for given status history and species
distrib_desc <- function(status_hst, sp) {
    # Filter data by matching species and native state, then select relevant columns
    status_hst |>
        filter(str_detect(lonomia_species, sp) & native == 1) |>
        select(
            endemic_south_america, # native
            endemic_biome_south_america,
            red_list_status,
            deciduos:evergreen,
            light_demand
        ) |>
        # Create summary table with specified type-label pairs and calculated statistics
        tbl_summary(
            missing = "no",
            type = list(all_categorical() ~ "categorical"),
            label = list(
                red_list_status ~ "Red list status",
                endemic_south_america ~ "Endemic in South America",
                endemic_biome_south_america ~ "Endemic in a biome",
                deciduos ~ "Deciduos",
                semideciduos ~ "Semideciduos",
                evergreen ~ "Evergreen",
                light_demand ~ "Light demander/Heliophile"
            )
        ) |>
        # Add additional stats to the summary
        add_stat(
            fns = all_categorical() ~ add_stat_pairwise_cs,
            location = all_categorical() ~ "level"
        ) |>
        # Add a second set of pairwise crosstab statistics to the summary
        add_stat(fns = all_categorical() ~ add_stat_pairwise_csp) |>
        # Modify table headers and footnotes, formatting p-value to 3 decimal points
        modify_header(
            p = "**p-value**",
            label = "**variables**",
            residuals = "**freq. class.**"
        ) |>
        modify_footnote(
            p ~ "Chi-Square Goodness of Fit Test (α = 0.05)",
            residuals ~ "Frequency classification"
        ) |>
        modify_fmt_fun(update = p ~ function(x) style_pvalue(x, digits = 3))
}

## ---------------------------------------------------------------------------------

# Define a function to create a summary table describing families for given status history and species
family_desc <- function(status_hst, sp) {
    # Define an inner function to add genus to the data frame.
    add_genus <- function(data, variable, ...) {
        # Create new dataframe with family, genus and species, filter by specified species
        data.frame(
            family = status_hst[["family"]],
            genus = status_hst[["host_complete_name"]],
            sps = status_hst[["lonomia_species"]]
        ) %>%
            filter(str_detect(sps, {{ sp }})) %>%
            dplyr::group_by(family) %>%
            dplyr::summarise(genus = paste(genus, collapse = ", ")) %>%
            select(genus)
    }

    # Filter data by matching species, then select relevant columns
    status_hst |>
        filter(str_detect(lonomia_species, sp)) |>
        select(family, genus) |>
        # Create summary table with specified type-label pairs and calculated statistics
        tbl_summary(
            missing = "no",
            include = -genus,
            type = list(all_categorical() ~ "categorical"),
            label = list(family ~ "Family")
        ) |>
        # Add additional stats and genus (from add_genus function) to the summary
        add_stat(
            fns = all_categorical() ~ add_stat_pairwise_cs,
            location = all_categorical() ~ "level"
        ) |>
        add_stat(fns = all_categorical() ~ add_stat_pairwise_csp) |>
        add_stat(
            fns = all_categorical() ~ add_genus,
            location = all_categorical() ~ "level"
        ) |>
        # Modify table headers
        modify_header(
            p = "**p-value**",
            label = "**variables**",
            residuals = "**freq. class.**",
            genus = "**Host species**"
        )
}

## ---------------------------------------------------------------------------------

# Here we use the 'import' function from rio package to import an Excel file "hosts.xlsx" from a directory called "data"
# The specific sheet being imported is named "lonomia_host"
initial_data <- rio::import(
    here::here("data", "hosts.xlsx"),
    sheet = "lonomia_host"
)

# The loaded data is manipulated using a sequence of operations performed via the pipe operator (%>%)
# arrange() from dplyr package is used to sort the data frame by host species and minimum year
# pivot_wider() reshapes the data where 'lonomia_species' column values become new columns and their corresponding 'min_year' values are filled
# group_by() groups the data by host species
# summarise() then computes summary statistics for each group. Here, it calculates the minimum years of Lonomia achelous and Lonomia obliqua sightings,
# and concatenates references using paste function. '.group' argument is set to 'keep' to retain the grouping structure
tab_yr <- initial_data |>
    dplyr::arrange(host_especies, min_year) |>
    pivot_wider(
        names_from = "lonomia_species",
        values_from = "min_year", values_fn = ~ min(.x)
    ) |>
    dplyr::group_by(host_especies) |>
    dplyr::summarise(
        year_achelous = min(`Lonomia achelous`),
        year_obliqua = min(`Lonomia obliqua`),
        references = paste(references, collapse = ", "), .groups = "keep"
    )

## ---------------------------------------------------------------------------------

# The result is left joined with status_hst based on host_complete_name column
# Columns are selected using select() and missing values in year_achelous and year_obliqua columns are replaced with "-" symbol
# The final table is created using 'gt' function and stylized with labels, footnotes and themes
tab_status <- status_hst |>
    left_join(tab_yr, by = c("host_complete_name" = "host_especies")) |>
    select(
        family, lonomia_species, host_complete_name,
        native, year_achelous, year_obliqua, references
    ) |>
    mutate(native = ifelse(native == 1, "yes", "no")) |>
    mutate_at(vars(year_achelous, year_obliqua), as.character) |>
    mutate_at(vars(year_achelous, year_obliqua), replace_na, "-") |>
    select(-lonomia_species) |>
    arrange(family, host_complete_name) |>
    select(-references) |>
    gt() |>
    cols_label(
        host_complete_name = "family/host species",
        year_achelous = "Lonomia achelous¹",
        year_obliqua = "Lonomia obliqua¹",
    ) |>
    tab_footnote(
        footnote = "¹Year of first interaction record.
    If the documentation does not include the sampling year, the document's year is used instead."
    ) |>
    gt_theme_espn() |>
    gt::tab_options(table.font.size = "12px")

# Results
print(tab_status$`_data`)

## ---------------------------------------------------------------------------------

# -----------------------
# Antropogenic
# Merge antropogenic information about hosts for L. achelous and L. obliqua
merged_tbl <- tbl_merge(
    list(
        antrop_desc(status_hst, "achelous"),
        antrop_desc(status_hst, "obliqua")
    ),
    tab_spanner = c("*Lonomia achelous*", "*Lonomia obliqua*") # setting labels for the merged tables
)

# Applying additional styling options
styled_tbl <- merged_tbl |>
    bold_labels() |> # making the labels bold
    as_gt() |> # converting to gt object for applying gt based functions
    gt::tab_options(table.font.size = "12px") # setting font size

# 'as_raw_html' and 'display_html' convert gt table to raw HTML
# and display it respectively (when use jupyter notebook)
html_tbl <- styled_tbl |>
    as_raw_html()

display_html(html_tbl)

## ---------------------------------------------------------------------------------

# Distribution
# `tbl_merge` function is used to merge two tables generated using the 'distrib_desc' function, for "achelous" and "obliqua" species respectively.
merged_distributions <- tbl_merge(
    list(
        distrib_desc(status_hst, "achelous"), # generates a table describing the distribution of "achelous" species
        distrib_desc(status_hst, "obliqua") # generates a table describing the distribution of "obliqua" species
    ),
    tab_spanner = c("*Lonomia achelous*", "*Lonomia obliqua*") # providing labels for the merged tables
)

# The final styled and filtered table is created by piping the merged table into a series of functions
styled_filtered_distributions <- merged_distributions |>
    bold_labels() |> # making the labels bold

    # hiding specified columns (add_stat_1_1 and add_stat_2_1) from the table
    modify_column_hide(columns = c(add_stat_1_1, add_stat_2_1)) |>
    as_gt() |> # converting the table into a gt object for applying gt based functionalities

    gt::tab_options(table.font.size = "12px") |> # setting font size of the entire table to 12 pixels

    as_raw_html() # converting the gt table to raw HTML format (Assuming this function exists in your local package)

# ... display it respectively (when use jupyter notebook)
display_html(styled_filtered_distributions)

## END ======================================================================
