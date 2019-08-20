Stroke Mortality Rates by County
================
Christopher Prener, Ph.D.
(August 20, 2019)

## Introduction

This notebook creates the stroke mortality maps used in Lecture-01 for
my sections of SOC 1120.

## Packages

This notebook requires the following packages:

``` r
# tidyverse packages
library(dplyr)          # data wrangling
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)        # create plots
library(magrittr)       # piping
library(purrr)          # iteration
```

    ## 
    ## Attaching package: 'purrr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     set_names

``` r
library(readxl)         # read excel files
library(stringr)        # tools for strings
library(tidyr)          # data wrangling
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     extract

``` r
# spatial packages
library(sf)             # spatial data tools
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
library(albersusa)      # composite projection for US
library(RColorBrewer)   # color palettes

# other packages
library(cowplot)        # legend plotting
```

    ## 
    ## ********************************************************

    ## Note: As of version 1.0.0, cowplot does not change the

    ##   default ggplot2 theme anymore. To recover the previous

    ##   behavior, execute:
    ##   theme_set(theme_cowplot())

    ## ********************************************************

``` r
library(here)           # manage file paths
```

    ## here() starts at /Users/chris/GitHub/slu-soc1120/US_OUTCOMES_Stroke

``` r
library(janitor)        # clean names
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

Two functions from the `source/` directory are also needed:

``` r
# calculate breaks
source(here("source", "cp_breaks.R"))

# plot theme
source(here("source", "cp_sequoiaTheme.R"))

# save plots
source(here("source", "cp_plotSave.R"))
```

The `ggthemes` package is a dependency for `cp_sequoiaTheme()` but it
does not need to be loaded - as long as it is installed, the function
will work as designed.

## Load Data

All of the raw data for these plots were obtained from the [Centers for
Disease Control’s stroke data
page](https://www.cdc.gov/stroke/maps_data.htm).

They are in separate `.csv` files that we’ll load individually:

``` r
# total stroke mortality rates
all <- read_excel(path = here("data", "stroke_all.xlsx")) 

# black stroke mortality rates
black <- read_excel(path = here("data", "stroke_blacks.xlsx")) 

# white stroke mortality rates
white <- read_excel(path = here("data", "stroke_whites.xlsx")) 
```

## Clean Data

Each of the data sets needs nearly identical cleaning, so we’ll do that
all at once:

``` r
# total stroke mortality rates
all <- all %>%
  clean_names() %>%
  rename(
    "fips" = "fips_code",
    "stroke_mrate" = "adults_35"
  ) %>%
  select(-"stroke_death_rates_per_100_000_2014_2016_ages_35_by_county", 
       -state_name, -county_name) %>%
  mutate(stroke_mrate = ifelse(stroke_mrate == -1, NA, stroke_mrate)) %>%
  filter(is.na(stroke_mrate) == FALSE) %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) %>%
  cp_breaks(var = stroke_mrate, newvar = stroke_cat, classes = 4, 
            style = "fisher", dig_lab = 4)
```

    ## Warning in classInt::classIntervals(.data[[refQ]], n = classes, style =
    ## style): N is large, and some styles will run very slowly; sampling imposed

``` r
# black stroke mortality rates
black <- black %>%
  clean_names() %>%
  rename(
    "fips" = "fips_code",
    "stroke_mrate" = "black_35"
  ) %>%
  select(-"stroke_death_rates_per_100_000_2014_2016_black_35_by_county", 
         -state_name, -county_name) %>%
  mutate(stroke_mrate = ifelse(stroke_mrate == -1, NA, stroke_mrate)) %>%
  filter(is.na(stroke_mrate) == FALSE) %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) %>%
  cp_breaks(var = stroke_mrate, newvar = stroke_cat, classes = 4, 
            style = "fisher", dig_lab = 4)

# white stroke mortality rates
white <- white %>%
  clean_names() %>%
  rename(
    "fips" = "fips_code",
    "stroke_mrate" = "white_35"
  ) %>%
  select(-"stroke_death_rates_per_100_000_2014_2016_white_35_by_county", 
         -state_name, -county_name) %>%
  mutate(stroke_mrate = ifelse(stroke_mrate == -1, NA, stroke_mrate)) %>%
  filter(is.na(stroke_mrate) == FALSE) %>%
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) %>%
  cp_breaks(var = stroke_mrate, newvar = stroke_cat, classes = 4, 
            style = "fisher", dig_lab = 4)
```

    ## Warning in classInt::classIntervals(.data[[refQ]], n = classes, style =
    ## style): N is large, and some styles will run very slowly; sampling imposed

## Create sf Objects

We’ll make use of both the state and county objects from the `albersusa`
package to create a state layer, a stroke belt layer (using the most
expansive definition), and a county layer:

``` r
# states
states <- usa_sf(proj = "laea") %>%
  select(name) %>%
  mutate(name = as.character(name))

# stroke belt states
sb_nhlbi <- c("AL", "AR", "GA", "IN", "KY", "LA", "MS", "NC", "SC", "TN", "VA")

strokeBelt <- usa_sf(proj = "laea") %>%
  select(name, iso_3166_2) %>%
  mutate(
    name = as.character(name),
    iso_3166_2 = as.character(iso_3166_2)
  ) %>%
  filter(iso_3166_2 %in% sb_nhlbi)

# counties
counties <- counties_sf(proj = "laea") %>%
  mutate(
    fips = as.character(fips),
    name = as.character(name),
    state = as.character(state)
  ) %>%
  select(fips, state, name)
```

Next, we need to add our mortality data to the `sf` object we’ve
created, removing counties that do not have any stroke mortality data.

``` r
# total stroke mortality rates
all <- left_join(counties, all, by = "fips") %>%
  filter(is.na(stroke_mrate) == FALSE)

# black stroke mortality rates
black <- left_join(counties, black, by = "fips") %>%
  filter(is.na(stroke_mrate) == FALSE)

# white stroke mortality rates
white <- left_join(counties, white, by = "fips") %>%
  filter(is.na(stroke_mrate) == FALSE)
```

## Create Maps

We’ll create separate maps for total stroke mortality, white stroke
mortality, and black stroke mortality.

### Total Stroke Mortality Map

First up, we’ll create a simplified version of the map that we can use
as the basis for our legend. The legend gets pulled out using `cowplot`
and then saved separately. This approach makes slide layout easier and
reduces the overall size of the slide deck.

``` r
# create legend
plot <- ggplot() +
  geom_sf(data = all, mapping = aes(fill = stroke_cat), color = NA) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# extract legend
legend <- ggdraw(get_legend(plot))
legend <- legend + 
  theme(plot.background = element_rect(fill = '#EBEBEB'))

# save legend
ggsave(filename = here("results", "stroke_all_legend.png"), legend, dpi = 500)
```

    ## Saving 7 x 5 in image

Next, we’ll draw a full version of the map that includes a number of
additional layers, and save it:

``` r
# create map
plot <- ggplot() +
  geom_sf(data = counties, fill = "#d7d7d7", color = NA) +
  geom_sf(data = all, mapping = aes(fill = stroke_cat), color = NA, show.legend = FALSE) +
  geom_sf(data = counties, fill = NA, color = "#474747", size = .1) +
  geom_sf(data = states, fill = NA, color = "#1f1f1f", size = .3) +
  geom_sf(data = strokeBelt, fill = NA, color = "#670000", size = .75) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  labs(
    title = "Stroke Mortality Rates (2014-2016)",
    subtitle = "All Races, Adults 35+",
    caption = "Data via the Centers for Disease Control"
  ) +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# save map
cp_plotSave(here("results", "stroke_all.png"), plot, preset = "lg", dpi = 500)
```

### African American Stroke Mortality Map

Next, we’ll map the African American mortality rates. First, the legend:

``` r
# create legend
plot <- ggplot() +
  geom_sf(data = black, mapping = aes(fill = stroke_cat), color = NA) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# extract legend
legend <- ggdraw(get_legend(plot))
legend <- legend + 
  theme(plot.background = element_rect(fill = '#EBEBEB'))

# save legend
ggsave(filename = here("results", "stroke_black_legend.png"), legend, dpi = 500)
```

    ## Saving 7 x 5 in image

Next, we’ll draw a full version of the map that includes a number of
additional layers, and save it:

``` r
# create map
plot <- ggplot() +
  geom_sf(data = counties, fill = "#d7d7d7", color = NA) +
  geom_sf(data = black, mapping = aes(fill = stroke_cat), color = NA, show.legend = FALSE) +
  geom_sf(data = counties, fill = NA, color = "#474747", size = .1) +
  geom_sf(data = states, fill = NA, color = "#1f1f1f", size = .3) +
  geom_sf(data = strokeBelt, fill = NA, color = "#670000", size = .75) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  labs(
    title = "Stroke Mortality Rates (2014-2016)",
    subtitle = "African Americans, Adults 35+",
    caption = "Data via the Centers for Disease Control"
  ) +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# save map
cp_plotSave(here("results", "stroke_black.png"), plot, preset = "lg", dpi = 500)
```

### White Stroke Mortality Map

Finall, we’ll map the white mortality rates. First, the legend:

``` r
# create legend
plot <- ggplot() +
  geom_sf(data = white, mapping = aes(fill = stroke_cat), color = NA) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# extract legend
legend <- ggdraw(get_legend(plot))
legend <- legend + 
  theme(plot.background = element_rect(fill = '#EBEBEB'))

# save legend
ggsave(filename = here("results", "stroke_white_legend.png"), legend, dpi = 500)
```

    ## Saving 7 x 5 in image

Next, we’ll draw a full version of the map that includes a number of
additional layers, and save it:

``` r
# create map
plot <- ggplot() +
  geom_sf(data = counties, fill = "#d7d7d7", color = NA) +
  geom_sf(data = white, mapping = aes(fill = stroke_cat), color = NA, show.legend = FALSE) +
  geom_sf(data = counties, fill = NA, color = "#474747", size = .1) +
  geom_sf(data = states, fill = NA, color = "#1f1f1f", size = .3) +
  geom_sf(data = strokeBelt, fill = NA, color = "#670000", size = .75) +
  scale_fill_brewer(palette = "Purples", name = "Rate per 100,000") +
  labs(
    title = "Stroke Mortality Rates (2014-2016)",
    subtitle = "White (Non-Hispanic), Adults 35+",
    caption = "Data via the Centers for Disease Control"
  ) +
  cp_sequoiaTheme(background = "transparent", map = TRUE, legend_size = 1)

# save map
cp_plotSave(here("results", "stroke_white.png"), plot, preset = "lg", dpi = 500)
```
