Driftless BNT streams
================
Bryan Maitland
7 July 2021

## Overview

What is the percentage of Driftless ecoregion streams (by WBIC for
streams for which we have survey data) in which brown trout are present?

``` r
library(tidyverse)
library(here)
library(rgdal)  
library(sf)
```

## Data

``` r
trout_sites_VA <- read_rds(here("trout-sites-24K-VA.rds"))

trout_data <- read_rds(here("fmdb_fish_trout.rds"))

hydro_lines <- 
  readOGR(
    dsn = here("24K_Hydro.gdb"), 
    layer = "WD_HYDRO_FLOWLINE_LN_24K") %>% 
  st_as_sf()
```

    ## OGR data source with driver: OpenFileGDB 
    ## Source: "C:\Users\maitlb\Documents\projects\driftless-trout-presence\24K_Hydro.gdb", layer: "WD_HYDRO_FLOWLINE_LN_24K"
    ## with 302930 features
    ## It has 39 fields

``` r
# clean hydro lines
hydro_lines <- 
  hydro_lines %>% 
  mutate(hydroid = as.character(HYDROID))

# link trout data with 24k hydro for ecoregion
data <- left_join(trout_data, trout_sites_VA, by = "site.seq.no")

# filter data frame
data <- 
  data %>% 
  select(county, waterbody.name, wbic, survey.year, survey.seq.no, visit.fish.seq.no, 
         species, number.of.fish, c_eco, hydroid) %>% 
  filter(c_eco == "52") %>% 
  filter(species == "brown_trout")
```

### get number of wbics

``` r
# number of distinct wbics
num.of.wbics.24k <- 
  hydro_lines %>% 
  semi_join(data, by = "hydroid") %>% 
  distinct(RIVER_SYS_WBIC) %>% 
  pull() %>% 
  length()
```

``` r
# number of distinct surveys wbics
num.of.wbics <- 
  data %>% 
  distinct(wbic) %>% 
  pull() %>% 
  length()
```

### get number of wbics with BNT

``` r
# number of wbics with BNT
num.of.bnt.wbics <-
  data %>% 
  group_by(wbic) %>% 
  summarise(n = sum((number.of.fish))) %>% 
  mutate(n = if_else(n == 0, 0, 1)) %>% 
  filter(n == 1) %>% 
  distinct(wbic) %>% 
  pull() %>% 
  length()
```

### calculate % streams with BNT

``` r
percent.wbics.w.bnt <- round((num.of.bnt.wbics/num.of.wbics) * 100, digits = 2)
percent.wbics24k.w.bnt <- round((num.of.bnt.wbics/num.of.wbics.24k) * 100, digits = 2)
```

## Results

In the Driftless Ecoregion of Wisconsin, 84.99% of surveyed wbics have
Brown Trout present, while 32.14% of *all* wbics present in the 24k
Hydro Geodatabase have Brown Trout present. **NOTE:** Result is based on
survey data collected from 1994-2020 by Fisheries Management.
