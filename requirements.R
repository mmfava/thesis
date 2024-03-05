# -------------------------------------------------------------------------
# Set the default CRAN repository to download the packages
options(repos = c(CRAN = "https://cran.r-project.org"))

# Package for handling and manipulating unit-aware data
install.packages("units")

# Package for support of simple features, a standardized way to encode spatial vector data
install.packages("sf")

# Package for world vector map data from Natural Earth used in creating maps
install.packages("rnaturalearth")

# Package for spatial data processing with an emphasis on speed and large datasets
install.packages("terra")

# Package for automated cleaning of geographic coordinate data
install.packages("CoordinateCleaner")

# Package for drawing geographical maps
install.packages("maps")

# Package for data manipulation and visualization
install.packages("tidyverse")

# Package for cleaning and examining dirty data (for e.g., messy column names)
install.packages("janitor")

# Package for creating interactive web maps using the JavaScript 'Leaflet' library
install.packages("leaflet")

# Package for reading Excel files (.xls and .xlsx formats)
install.packages("readxl")

# Package for classes and methods for spatial data
install.packages("sp")

# Package for arranging multiple ggplot2 plots into a grid on same page
install.packages("cowplot")

# Package for accessing Google Sheets through Sheets API
install.packages("gsheet")

# Package for creating attractive and flexible tables using 'kable' from 'knitr'
install.packages("kableExtra")

# Package to convert JSON objects into R objects and vice-versa
install.packages("jsonlite")

# Package for functional programming tools for working with data
install.packages("purrr")

# Package to access data from BIEN Database (Botanical Information and Ecology Network)
install.packages("BIEN")

# Package for accessing the iDigBio specimen records
install.packages("ridigbio")

# Package simplifying file path specification
install.packages("here")

# Package for geocoding and reverse geocoding with tidy results
install.packages("tidygeocoder")

# Package for a swiss-army knife for data I/O
install.packages("rio")

# Package for spatial visualization with ggplot2
install.packages("ggspatial")

# Package for creating descriptive statistics tables 'gtsummary'
install.packages("gtsummary")

# Package for adding supplementary functionality to 'gt' package
install.packages("gtExtras")

# Package for rendering complex text using 'ggplot2'
install.packages("ggtext")

# -------------------------------------------------------------------------
# Load packages
library("units")
library("sf")
library("rnaturalearth")
library("terra")
library("CoordinateCleaner")
library("maps")
library("tidyverse")
library("janitor")
library("leaflet")
library("readxl")
library("sp")
library("cowplot")
library("eks")
library("gsheet")
library("tidyverse")
library("janitor")
library("kableExtra")
library("readxl")
library("jsonlite")
library("BIEN")
library("purrr")
library("BIEN")
library("ridigbio")
library("here")
library("ggspatial")
library("gtsummary")
library("gtExtras")
library("ggtext")

# -------------------------------------------------------------------------
# Details
sessionInfo()

# R version 4.3.2 (2023-10-31)
# Platform: x86_64-conda-linux-gnu (64-bit)
# Running under: Ubuntu 22.04.3 LTS

# Matrix products: default
# BLAS/LAPACK: /opt/conda/lib/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

# locale:
#  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
#  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
#  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
#  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
#  [9] LC_ADDRESS=C               LC_TELEPHONE=C
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

# time zone: Etc/UTC
# tzcode source: system (glibc)

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base

# other attached packages:
#  [1] ggtext_0.1.2            gtExtras_0.5.0          gt_0.10.1
#  [4] gtsummary_1.7.2         ggspatial_1.1.9         here_1.0.1
#  [7] ridigbio_0.3.8          BIEN_1.2.6              RPostgreSQL_0.7-6
# [10] DBI_1.2.0               jsonlite_1.8.8          kableExtra_1.4.0
# [13] gsheet_0.4.5            eks_1.0.4               cowplot_1.1.3
# [16] sp_2.1-3                readxl_1.4.3            leaflet_2.2.1
# [19] janitor_2.2.0           lubridate_1.9.3         forcats_1.0.0
# [22] stringr_1.5.1           dplyr_1.1.4             purrr_1.0.2
# [25] readr_2.1.4             tidyr_1.3.0             tibble_3.2.1
# [28] ggplot2_3.4.4           tidyverse_2.0.0         maps_3.4.2
# [31] CoordinateCleaner_3.0.1 terra_1.7-71            rnaturalearth_1.0.1
# [34] sf_1.0-15               units_0.8-5

# loaded via a namespace (and not attached):
#  [1] s2_1.1.6             rematch2_2.1.2       rlang_1.1.2
#  [4] magrittr_2.0.3       snakecase_0.11.1     e1071_1.7-14
#  [7] compiler_4.3.2       systemfonts_1.0.5    vctrs_0.6.5
# [10] rgbif_3.7.9          pkgconfig_2.0.3      wk_0.9.1
# [13] fastmap_1.1.1        fontawesome_0.5.2    utf8_1.2.4
# [16] rmarkdown_2.25       tzdb_0.4.0           pracma_2.4.4
# [19] xfun_0.42            parallel_4.3.2       R6_2.5.1
# [22] stringi_1.8.3        cellranger_1.1.0     Rcpp_1.0.11
# [25] iterators_1.0.14     knitr_1.45           mapsf_0.9.0
# [28] Matrix_1.6-4         timechange_0.2.0     tidyselect_1.2.0
# [31] rstudioapi_0.15.0    doParallel_1.0.17    codetools_0.2-19
# [34] lattice_0.22-5       plyr_1.8.9           ks_1.14.2
# [37] withr_2.5.2          geosphere_1.5-18     evaluate_0.23
# [40] proxy_0.4-27         isoband_0.2.7        xml2_1.3.6
# [43] mclust_6.0.1         pillar_1.9.0         whisker_0.4.1
# [46] KernSmooth_2.23-22   foreach_1.5.2        generics_0.1.3
# [49] rprojroot_2.0.4      paletteer_1.6.0      hms_1.1.3
# [52] munsell_0.5.0        scales_1.3.0         class_7.3-22
# [55] glue_1.6.2           lazyeval_0.2.2       tools_4.3.2
# [58] data.table_1.14.10   mvtnorm_1.2-4        grid_4.3.2
# [61] ape_5.7-1            crosstalk_1.2.1      colorspace_2.1-0
# [64] nlme_3.1-164         raster_3.6-26        cli_3.6.2
# [67] fasterize_1.0.5      fansi_1.0.6          broom.helpers_1.14.0
# [70] viridisLite_0.4.2    svglite_2.1.3        gtable_0.3.4
# [73] oai_0.4.0            digest_0.6.33        classInt_0.4-10
# [76] htmlwidgets_1.6.4    htmltools_0.5.7      lifecycle_1.0.4
# [79] httr_1.4.7           maplegend_0.1.0      gridtext_0.1.5
