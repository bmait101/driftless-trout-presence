How common are trout in Driftless streams
================
Bryan Maitland
29 July 2021

## TO DO

  - purrr the calculations and streamline
  - fix infographic plots

## Overview

What is the percentage of Driftless area streams in which Brook, Brown,
or Tiger Trout are present?

``` r
library(tidyverse)
library(here)
library(wdnr.fmdb)
set_fmdb_credentials()
```

    ## Please enter password in TK window (Alt+Tab)
    ## Please enter password in TK window (Alt+Tab)

``` r
library(rgdal)  
library(sf)

theme_set(theme_void(base_family = "sans"))
```

## 1\. Data

### FMDB surveys and efforts

Download all surveys from 1994-2020 on streams using backpack and stream
shockers, retaining only surveys that are complete and proofed:

### clip survey data to driftless region:

``` r
# clip the classified trout streams to driftless region
lines_drift <- 
  lines %>% 
  st_intersection(driftless)

# make surveys spatial and clip to driftless area
df_surveys_drift <- 
  df_surveys %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 3071) %>% 
  st_intersection(driftless)
```

### join surevys sites to stream lines

We only want surveys on classified trout streams, so we need to link
survey sites to streams to see if they are on classified water or not:

    ##   TROUT_CLAS    n
    ## 1    CLASS I  983
    ## 2   CLASS II 1575
    ## 3  CLASS III  282
    ## 4       <NA> 1396

### filter efforts for surveys on classified streams

``` r
# filter efforts for driftless surveys on classified streams
df_efforts_drift_class <- semi_join(df_efforts, df_surveys_drift_class, by = "survey.seq.no")
  
# clean up 
rm(df_surveys); rm(df_efforts); rm(df_surveys_drift_sites_lines)
```

### download fishraw data for classified streams in driftless region:

### clean fish data

``` r
df_fish_drift <- 
  df_fish_drift %>% 
  filter(!species == "no_fish_captured") %>%
  # bind_rows(df_trout_0s) %>%  
  mutate(species = if_else(species == "tiger_trout_(i21_x_i22)", "tiger_trout", species)) %>% 
  filter(species %in% c("brook_trout","brown_trout","tiger_trout"))
```

## 2\. Count of surveys and streams (wbics)

#### how many surveys and surevyed wbics?

``` r
# unique surveys and wbics in driftles surveys data
df_surveys_drift_class %>% 
  summarise(across(c(survey.seq.no, wbic), ~ length(unique(.x))))
```

    ## # A tibble: 1 x 2
    ##   survey.seq.no  wbic
    ##           <int> <int>
    ## 1          6851   693

``` r
# set to objects
n.surveys <- length(unique(df_surveys_drift_class$survey.seq.no))
n.surveyed.wbics <- length(unique(df_surveys_drift_class$wbic))

# unique surveys and wbics in driftles surveys data class 1-2
n.surveyed.wbics.12 <- 
  df_surveys_drift_class %>% 
  filter(TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  summarise(across(c(wbic), ~ length(unique(.x)))) %>% 
  pull()
```

#### how many wbics in classified stream layer?

    ##   TROUT_CLAS   n
    ## 1    CLASS I 368
    ## 2   CLASS II 518
    ## 3  CLASS III 125

    ##   WBIC
    ## 1  761

    ## [1] 679

``` r
relig_income
```

    ## # A tibble: 18 x 11
    ##    religion `<$10k` `$10-20k` `$20-30k` `$30-40k` `$40-50k` `$50-75k` `$75-100k`
    ##    <chr>      <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 Agnostic      27        34        60        81        76       137        122
    ##  2 Atheist       12        27        37        52        35        70         73
    ##  3 Buddhist      27        21        30        34        33        58         62
    ##  4 Catholic     418       617       732       670       638      1116        949
    ##  5 Don’t k~      15        14        15        11        10        35         21
    ##  6 Evangel~     575       869      1064       982       881      1486        949
    ##  7 Hindu          1         9         7         9        11        34         47
    ##  8 Histori~     228       244       236       238       197       223        131
    ##  9 Jehovah~      20        27        24        24        21        30         15
    ## 10 Jewish        19        19        25        25        30        95         69
    ## 11 Mainlin~     289       495       619       655       651      1107        939
    ## 12 Mormon        29        40        48        51        56       112         85
    ## 13 Muslim         6         7         9        10         9        23         16
    ## 14 Orthodox      13        17        23        32        32        47         38
    ## 15 Other C~       9         7        11        13        13        14         18
    ## 16 Other F~      20        33        40        46        49        63         46
    ## 17 Other W~       5         2         3         4         2         7          3
    ## 18 Unaffil~     217       299       374       365       341       528        407
    ## # ... with 3 more variables: `$100-150k` <dbl>, `>150k` <dbl>, `Don't
    ## #   know/refused` <dbl>

``` r
relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")
```

    ## # A tibble: 180 x 3
    ##    religion income             count
    ##    <chr>    <chr>              <dbl>
    ##  1 Agnostic <$10k                 27
    ##  2 Agnostic $10-20k               34
    ##  3 Agnostic $20-30k               60
    ##  4 Agnostic $30-40k               81
    ##  5 Agnostic $40-50k               76
    ##  6 Agnostic $50-75k              137
    ##  7 Agnostic $75-100k             122
    ##  8 Agnostic $100-150k            109
    ##  9 Agnostic >150k                 84
    ## 10 Agnostic Don't know/refused    96
    ## # ... with 170 more rows

### count of surveyed wbics in which trout are present:

#### convert catch data to presence data:

``` r
df_fish_drift_pa <-
  df_fish_drift %>% 
  group_by(wbic, survey.seq.no, species) %>% 
  summarise(n = sum((number.of.fish)), .groups = "drop") %>% 
  mutate(n = if_else(n > 1, 1, 0)) %>% 
  pivot_wider(names_from = species, values_from = n, values_fill = 0) %>%
  mutate(symp = brook_trout + brown_trout) %>%
  mutate(symp = if_else(symp == 2, 1, 0)) %>% 
  pivot_longer(cols = 3:6, names_to = "species", values_to = "present") %>% 
  left_join(df_surveys_drift_class %>%
              select(survey.seq.no, TROUT_CLAS, geometry), 
            by = "survey.seq.no") %>% 
  st_as_sf()

# df_fish_drift_pa_wide <- 
#   df_fish_drift_pa %>%
#   st_drop_geometry() %>%
#   pivot_wider(names_from = species, values_from = n, values_fill = 0) %>%
#   mutate(symp = brook_trout + brown_trout) %>%
#   mutate(symp = if_else(symp == 2, 1, 0))
```

#### number wbics with trout present

``` r
n.wbics.bnt <- 
  df_fish_drift_pa %>% 
  filter(species == "brown_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bnt.12 <- 
  df_fish_drift_pa %>% 
  filter(species == "brown_trout" & present == 1 & TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bkt <- 
  df_fish_drift_pa %>% 
  filter(species == "brook_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bkt.12 <- 
  df_fish_drift_pa %>% 
  filter(species == "brook_trout" & present == 1 & TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.tiger <- 
  df_fish_drift_pa %>% 
  filter(species == "tiger_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.tiger.12 <- 
  df_fish_drift_pa %>% 
  filter(species == "tiger_trout" & present == 1 & TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.symp <- 
  df_fish_drift_pa %>% 
  filter(species == "symp" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.symp.12 <- 
  df_fish_drift_pa %>% 
  filter(species == "symp" & present == 1 & TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  pull(wbic) %>% unique() %>% length()
```

## 4\. calculate percentages

#### brown trout

``` r
(percent.wbics.bnt <- round((n.wbics.bnt/n.surveyed.wbics) * 100, digits = 2))
```

    ## [1] 61.18

``` r
(percent.wbics.bnt.12 <- round((n.wbics.bnt.12/n.surveyed.wbics.12) * 100, digits = 2))
```

    ## [1] 61.94

#### brook trout

``` r
(percent.wbics.bkt <- round((n.wbics.bkt/n.surveyed.wbics) * 100, digits = 2))
```

    ## [1] 70.42

``` r
(percent.wbics.bkt.12 <- round((n.wbics.bkt.12/n.surveyed.wbics.12) * 100, digits = 2))
```

    ## [1] 70.81

#### tiger trout

``` r
(percent.wbics.tgt <- round((n.wbics.tiger/n.surveyed.wbics) * 100, digits = 2))
```

    ## [1] 2.16

``` r
(percent.wbics.tgt.12 <- round((n.wbics.tiger.12/n.surveyed.wbics.12) * 100, digits = 2))
```

    ## [1] 2.42

#### brook/brown sympatry

``` r
(percent.wbics.symp <- round((n.wbics.symp/n.surveyed.wbics) * 100, digits = 2))
```

    ## [1] 39.83

``` r
(percent.wbics.symp.12 <- round((n.wbics.symp.12/n.surveyed.wbics.12) * 100, digits = 2))
```

    ## [1] 40.65

## Plot

``` r
panel_labels <- c(
  `brook_trout` = glue::glue("Brook Trout ({percent.wbics.bkt}%)"),
  `brown_trout` = glue::glue("Brown Trout ({percent.wbics.bnt}%)"),
  `symp` = glue::glue("BKT/BNT Sympatry ({percent.wbics.symp}%)"),
  `tiger_trout` = glue::glue("Tiger Trout ({percent.wbics.tgt}%)")
  )

ggplot() + 
  geom_sf(data = driftless, fill = "white") +
  geom_sf(data = lines_drift, aes(color = TROUT_CLAS), show.legend = "line") +
  geom_sf(data = df_fish_drift_pa %>% filter(present == 1), aes(fill = TROUT_CLAS), 
          show.legend = "point", size = 1, alpha = 0.5, shape = 21) + 
  scale_colour_manual(
    values = c("#53DC4D", "#00C5ff", "#004DA8"), 
    guide = guide_legend(
      override.aes = list(linetype = c("solid", "solid","solid"), 
                          shape = c(NA, NA, NA),
                          size = c(1,1,1)))
    ) +
  scale_fill_manual(
    values = c("#53DC4D", "#00C5ff", "#004DA8", "grey"), 
    guide = guide_legend(
      override.aes = list(linetype = "blank", shape = 21, size = 2, alpha = 1))
    ) +
  facet_wrap(vars(species), labeller = as_labeller(panel_labels), nrow = 1) +
  labs(
    title = "How common are trout in Driftless Area streams?",
    # title ="Percent of all classified and surveyed streams with trout present:",
    subtitle ="Percent of surveyed trout streams with trout present:",
    caption = "Visualization: Bryan Maitland • Data: WI DNR Fisheries Managment (https://dnr.wisconsin.gov/)", 
    color = "Stream Class", 
    fill = "Trout Presence") + 
  theme(
    plot.title = element_text(hjust=0.5, size=18, face = "bold", margin=margin(0,0,5,0)), 
    plot.subtitle = element_text(hjust=0.5, size=12, margin=margin(0,0,15,0)), 
    plot.caption = element_text(hjust = .5,size = 10, margin = margin(35, 0, 0, 0)), 
    strip.text = element_text(face = "bold")
    )
```

![](bnt-dirftless-streams_files/figure-gfm/plot-1.png)<!-- -->

``` r
path <- here::here("plots", "driftless_trout_presence")

ggsave(glue::glue("{path}.pdf"), width = 12, height = 5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
            filenames = glue::glue("{path}.png"),
            format = "png", dpi = 300)
```

    ## Converting page 1 to C:/Users/maitlb/Documents/projects/driftless-trout-presence/plots/driftless_trout_presence.png... done!

    ## [1] "C:/Users/maitlb/Documents/projects/driftless-trout-presence/plots/driftless_trout_presence.png"

``` r
Sys.time()
```

    ## [1] "2021-08-03 15:11:28 CDT"

``` r
git2r::repository()
```

    ## Local:    main C:/Users/maitlb/Documents/projects/driftless-trout-presence
    ## Remote:   main @ origin (https://github.com/bmait101/driftless-trout-presence.git)
    ## Head:     [293cd75] 2021-08-03: update readme

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17763)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] sf_0.9-7        rgdal_1.5-23    sp_1.4-5        wdnr.fmdb_0.3.2
    ##  [5] here_1.0.1      forcats_0.5.1   stringr_1.4.0   dplyr_1.0.2    
    ##  [9] purrr_0.3.4     readr_1.3.1     tidyr_1.1.2     tibble_3.0.4   
    ## [13] ggplot2_3.3.3   tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         jsonlite_1.7.1     modelr_0.1.8       assertthat_0.2.1  
    ##  [5] getPass_0.2-2      pdftools_3.0.1     askpass_1.1        highr_0.8         
    ##  [9] blob_1.2.1         cellranger_1.1.0   yaml_2.2.1         qpdf_1.1          
    ## [13] pillar_1.4.7       backports_1.1.10   lattice_0.20-41    glue_1.4.2        
    ## [17] digest_0.6.27      rvest_0.3.6        colorspace_1.4-1   htmltools_0.5.0   
    ## [21] pkgconfig_2.0.3    broom_0.7.4        haven_2.3.1        scales_1.1.1      
    ## [25] git2r_0.28.0       generics_0.1.0     farver_2.0.3       ellipsis_0.3.2    
    ## [29] withr_2.4.1        cli_2.5.0          magrittr_2.0.1     crayon_1.4.0      
    ## [33] readxl_1.3.1       evaluate_0.14      fs_1.5.0           fansi_0.4.1       
    ## [37] xml2_1.3.2         class_7.3-17       textshaping_0.3.4  tools_4.0.2       
    ## [41] hms_1.0.0          lifecycle_0.2.0    munsell_0.5.0      reprex_1.0.0      
    ## [45] compiler_4.0.2     e1071_1.7-4        systemfonts_1.0.1  rlang_0.4.10      
    ## [49] classInt_0.4-3     units_0.6-7        grid_4.0.2         rstudioapi_0.13   
    ## [53] tcltk_4.0.2        rmarkdown_2.6      gtable_0.3.0       DBI_1.1.1         
    ## [57] R6_2.5.0           lubridate_1.7.9    knitr_1.31         keyring_1.1.0     
    ## [61] utf8_1.1.4         rprojroot_2.0.2    ragg_1.1.2         KernSmooth_2.23-17
    ## [65] stringi_1.5.3      Rcpp_1.0.5         vctrs_0.3.5        dbplyr_1.4.4      
    ## [69] tidyselect_1.1.0   xfun_0.22
