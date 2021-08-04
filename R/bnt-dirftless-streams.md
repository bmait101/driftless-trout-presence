How common are trout in Driftless streams
================
Bryan Maitland
29 July 2021

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

``` r
# # params for pulling surveys and efforts
# yrs <- list(1994:1999,2000:2005,2006:2011,2012:2016,2017:2020)
# waterbody_types <- c( "wadable_stream", "non-wadable_stream", "stream")
# gear_types <- c("stream_shocker","backpack_shocker")
# 
# # pull surveys
# df_surveys_raw <- 
#   yrs %>% 
#   map_df(~get_fmdb_surveys(year = ., waterbody_type = waterbody_types))
# 
# # pulls efforts
# df_efforts_raw <- 
#   yrs %>% 
#   map_df(~get_fmdb_efforts(year = ., waterbody_type = waterbody_types, gear = gear_types))
# 
# # clean up
# rm(yrs); rm(gear_types); rm(waterbody_types)
# 
# # wrtie to file
# write_rds(df_surveys_raw, here("data", "surveys_raw_20210803.rds"))
# write_rds(df_efforts_raw, here("data", "efforts_raw_20210803.rds"))

# load
df_surveys_raw <- read_rds(here("data", "surveys_raw_20210803.rds"))
df_efforts_raw <- read_rds(here("data", "efforts_raw_20210803.rds"))
```

``` r
# # check surveys with no match in the efforts (which are for electofishing only)
# df_surveys_raw %>% 
#   anti_join(df_efforts_raw, by = "survey.seq.no") 
# # these are IBI surveys or those with efforts that are "multiple_gear_types"

# filter for unique surveys in the efforts data
df_surveys <- semi_join(df_surveys_raw, df_efforts_raw, by = "survey.seq.no")

# filter for proofed survey data
targ.survs <- c(
  "data_entry_complete_and_proofed",
  "historical_data_complete_and_proofed",
  "historical_data_entry_complete",
  "historical_data_load_status_unknown")
df_surveys <- filter(df_surveys, survey.status %in% targ.survs)


# filter efforts
targ.spp <- c(
  "all_species","gamefish_species","gamefish_panfish",
  "trout_spp","brown_trout","brook_trout","rainbow_trout")

df_efforts <- 
  df_efforts_raw %>% 
  semi_join(df_surveys, by = "survey.seq.no") %>%
  filter(target.species %in% targ.spp | secondary.target.species %in% targ.spp) %>% 
  filter(site.seq.no != 315) 

# surveys again by the cleaned efforts
df_surveys <- semi_join(df_surveys, df_efforts, by = "survey.seq.no")

# clean up 
rm(df_surveys_raw);rm(df_efforts_raw)
rm(targ.survs); rm(targ.spp)
```

### clip data to driftless region:

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

# make a simple xwalk for wbic and trout class
xwalk_wbic_troutclass <- 
  lines_drift %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  distinct(WBIC, .keep_all = TRUE) %>% 
  select(WBIC, TROUT_CLAS)

# filter efforts for driftless surveys
df_efforts_drift <- semi_join(df_efforts, df_surveys_drift, by = "survey.seq.no")
  
# clean up 
rm(df_surveys); rm(df_efforts)
```

### join surevys sites to stream lines and isolate sites

``` r
# isolate unique survey site locations in driftless
df_surveys_site_locs <-
  df_surveys_drift %>% 
  left_join(xwalk_wbic_troutclass, by = c("wbic" = "WBIC")) %>% 
  mutate(TROUT_CLAS = if_else(is.na(TROUT_CLAS), "Unclassified", TROUT_CLAS))  %>%
  distinct(site.seq.no, .keep_all = TRUE)
```

### download fishraw data

``` r
df_fish_drift_trout <- 
  df_fish_drift %>% 
  filter(!species == "no_fish_captured") %>% bind_rows(df_trout_0s) %>%
  mutate(species = if_else(species == "tiger_trout_(i21_x_i22)", "tiger_trout", species)) %>% 
  filter(species %in% c("brook_trout","brown_trout","tiger_trout")) %>% 
  filter(!str_detect(wbic, "^999"))

rm(df_trout_0s)
```

``` r
# 3 surveys do not have any fish data
# the other 27 missing wbics get deleted when fioltering for trout
# missing_fish_wbics <- 
#   anti_join(
#     df_surveys_drift %>% 
#       left_join(xwalk_wbic_troutclass , by = c("wbic"="WBIC")) %>% 
#       filter(TROUT_CLAS %in% c("CLASS I", "CLASS II", "CLASS III")) %>% 
#       distinct(wbic), 
#     df_fish_drift_trout %>% 
#       left_join(xwalk_wbic_troutclass , by = c("wbic"="WBIC")) %>% 
#       filter(TROUT_CLAS %in% c("CLASS I", "CLASS II", "CLASS III")) %>% 
#       distinct(wbic), 
#     by = "wbic") %>% 
#   st_drop_geometry() %>% 
#   pull()
# 
# 
# missing_fish_survs <-
#   df_surveys_drift %>% 
#   filter(wbic %in% missing_fish_wbics)
# 
# missing_efforts <- 
#   df_efforts_drift %>% 
#   filter(wbic %in% missing_fish_wbics)
# 
# missing_efforts$visit.fish.seq.no %in% vs
# 
# missing_data <- 
#   get_fmdb_fishraw(visit_seq = missing_efforts$visit.fish.seq.no)
#   
# missing_data %>% 
#   filter(wbic %in% missing_fish_wbics)
```

## 2\. Count of surveys and streams (wbics)

### count of surveyed wbics in which trout are present:

``` r
# test <-
#   df_fish_drift_trout %>% 
#   group_by(wbic, site.seq.no, species) %>% 
#   summarise(n = sum((number.of.fish)), .groups = "drop") %>% 
#   mutate(n = if_else(n >= 1, 1, 0)) %>% 
#   pivot_wider(names_from = species, values_from = n, values_fill = 0) %>%  
#   mutate(
#     no_trout = if_else(brook_trout==0 & brown_trout==0, 1, 0),
#     symp = if_else(brook_trout==1 & brown_trout==1, 1, 0),
#     bkt_only = if_else(brook_trout==1 & brown_trout==0, 1, 0),
#     bnt_only = if_else(brook_trout==0 & brown_trout==1, 1, 0)
#     ) %>% 
#   pivot_longer(cols = 3:9, names_to = "species", values_to = "present") 
# 
# test
# 
# df_fish_drift_trout_pa <-
#   test %>% 
#   group_by(wbic, species) %>% 
#   summarise(present = sum((present)), .groups = "drop") %>% 
#   mutate(present = if_else(present >= 1, 1, 0)) %>%
#   left_join(xwalk_wbic_troutclass, by = c("wbic" = "WBIC")) 


df_fish_drift_trout_pa <-
  df_fish_drift_trout %>% 
  group_by(wbic, species) %>% 
  summarise(n = sum((number.of.fish)), .groups = "drop") %>% 
  mutate(n = if_else(n >= 1, 1, 0)) %>% 
  pivot_wider(names_from = species, values_from = n, values_fill = 0) %>%  
  mutate(
    no_trout = if_else(brook_trout==0 & brown_trout==0, 1, 0),
    symp = if_else(brook_trout==1 & brown_trout==1, 1, 0),
    bkt_only = if_else(brook_trout==1 & brown_trout==0, 1, 0),
    bnt_only = if_else(brook_trout==0 & brown_trout==1, 1, 0)
    ) %>%
  pivot_longer(cols = 2:8, names_to = "species", values_to = "present") %>% 
  left_join(xwalk_wbic_troutclass, by = c("wbic" = "WBIC")) 
df_fish_drift_trout_pa
```

    ## # A tibble: 6,489 x 4
    ##      wbic species     present TROUT_CLAS
    ##     <int> <chr>         <dbl> <chr>     
    ##  1 875300 brook_trout       1 CLASS II  
    ##  2 875300 brown_trout       1 CLASS II  
    ##  3 875300 tiger_trout       0 CLASS II  
    ##  4 875300 no_trout          0 CLASS II  
    ##  5 875300 symp              1 CLASS II  
    ##  6 875300 bkt_only          0 CLASS II  
    ##  7 875300 bnt_only          0 CLASS II  
    ##  8 880100 brook_trout       0 CLASS II  
    ##  9 880100 brown_trout       1 CLASS II  
    ## 10 880100 tiger_trout       0 CLASS II  
    ## # ... with 6,479 more rows

``` r
n.wbics.bnt <- 
  df_fish_drift_trout_pa %>% 
  filter(species == "brown_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bnt.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "brown_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bnt.12 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II")) %>% 
  filter(species == "brown_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bkt <- 
  df_fish_drift_trout_pa %>% 
  filter(species == "brook_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bkt.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "brook_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bkt.12 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II")) %>% 
  filter(species == "brook_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()


n.wbics.tgt <- 
  df_fish_drift_trout_pa %>% 
  filter(species == "tiger_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.tgt.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "tiger_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.tgt.12 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II")) %>% 
  filter(species == "tiger_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()


n.wbics.smp <- 
  df_fish_drift_trout_pa %>% 
  filter(species == "symp" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.smp.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "symp" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.smp.12 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II")) %>% 
  filter(species == "symp" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()


n.wbics.smp.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "symp" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()


n.wbics.bkt.only.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "bkt_only" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.bnt.only.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "bnt_only" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()

n.wbics.notrout.123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I","CLASS II","CLASS III")) %>% 
  filter(species == "no_trout" & present == 1) %>% 
  pull(wbic) %>% unique() %>% length()
```

#### number of surveyed wbics

``` r
# unique surveys and wbics in driftles surveys data class 1-3
n.fish.wbics <- 
  df_fish_drift_trout_pa %>% 
  summarise(across(c(wbic), ~ length(unique(.x)))) %>% 
  pull()

# unique surveys and wbics in driftles surveys data class 1-3
n.fish.wbics.class123 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I", "CLASS II", "CLASS III")) %>% 
  summarise(across(c(wbic), ~ length(unique(.x)))) %>% 
  pull()

# unique surveys and wbics in driftles surveys data class 1-2
n.fish.wbics.class12 <- 
  df_fish_drift_trout_pa %>% 
  filter(TROUT_CLAS %in% c("CLASS I", "CLASS II")) %>% 
  summarise(across(c(wbic), ~ length(unique(.x)))) %>% 
  pull()
```

## 4\. calculate percentages

``` r
(percent.wbics.bnt <- round((n.wbics.bnt/n.fish.wbics) * 100, digits = 2))
```

    ## [1] 61.6

``` r
(percent.wbics.bnt.123 <- round((n.wbics.bnt.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 70.35

``` r
# (percent.wbics.bnt.12 <- round((n.wbics.bnt.12/n.fish.wbics.class12) * 100, digits = 2))

(percent.wbics.bkt <- round((n.wbics.bkt/n.fish.wbics) * 100, digits = 2))
```

    ## [1] 70.66

``` r
(percent.wbics.bkt.123 <- round((n.wbics.bkt.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 83.17

``` r
# (percent.wbics.bkt.12 <- round((n.wbics.bkt.12/n.fish.wbics.class12) * 100, digits = 2))

(percent.wbics.tgt <- round((n.wbics.tgt/n.fish.wbics) * 100, digits = 2))
```

    ## [1] 6.9

``` r
(percent.wbics.tgt.123 <- round((n.wbics.tgt.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 9.62

``` r
# (percent.wbics.tgt.12 <- round((n.wbics.tgt.12/n.fish.wbics.class12) * 100, digits = 2))

(percent.wbics.smp <- round((n.wbics.smp/n.fish.wbics) * 100, digits = 2))
```

    ## [1] 42.39

``` r
(percent.wbics.smp.123 <- round((n.wbics.smp.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 54.81

``` r
# (percent.wbics.smp.12 <- round((n.wbics.smp.12/n.fish.wbics) * 100, digits = 2))

(percent.wbics.bkt.only <- round((n.wbics.bkt.only.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 28.37

``` r
(percent.wbics.bnt.only <- round((n.wbics.bnt.only.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 15.54

``` r
(percent.wbics.notrout <- round((n.wbics.notrout.123/n.fish.wbics.class123) * 100, digits = 2))
```

    ## [1] 1.28

``` r
percent.wbics.bkt.only + percent.wbics.bnt.only + percent.wbics.smp.123 + percent.wbics.notrout
```

    ## [1] 100

## Plot

``` r
panel_labels <- c(
  `bkt_only` = glue::glue("BKT Only ({percent.wbics.bkt.only}%)"),
  `bnt_only` = glue::glue("BNT ONLY ({percent.wbics.bnt.only}%)"),
  `no_trout` = glue::glue("No Trout ({percent.wbics.notrout}%)"),

  `brook_trout` = glue::glue("Brook Trout ({percent.wbics.bkt.123}%)"),
  `brown_trout` = glue::glue("Brown Trout ({percent.wbics.bnt.123}%)"),
  `symp` = glue::glue("BKT/BNT Sympatry ({percent.wbics.smp.123}%)"),
  `tiger_trout` = glue::glue("Tiger Trout ({percent.wbics.tgt.123}%)")
  )

plot_lines <-
  lines_drift %>% 
  select(WBIC,geometry) %>% 
  left_join(df_fish_drift_trout_pa, by = c("WBIC"="wbic")) %>% 
  filter(present == 1)
```

``` r
p <- 
  ggplot() + 
  geom_sf(data = driftless, fill = "white") +
  geom_sf(data = lines_drift, color = "grey", show.legend = "line") +
  geom_sf(data = plot_lines, aes(color = TROUT_CLAS), 
          show.legend = "line", size = 0.75, shape = 21) + 
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

p
```

![](bnt-dirftless-streams_files/figure-gfm/plot-1.png)<!-- -->

``` r
path <- here::here("plots", "driftless_trout_presence")

ggsave(glue::glue("{path}.pdf"), width = 15, height = 5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
            filenames = glue::glue("{path}.png"),
            format = "png", dpi = 300)
```

    ## Converting page 1 to C:/Users/maitlb/Documents/projects/driftless-trout-presence/plots/driftless_trout_presence.png... done!

    ## [1] "C:/Users/maitlb/Documents/projects/driftless-trout-presence/plots/driftless_trout_presence.png"

``` r
Sys.time()
```

    ## [1] "2021-08-04 16:53:07 CDT"

``` r
git2r::repository()
```

    ## Local:    main C:/Users/maitlb/Documents/projects/driftless-trout-presence
    ## Remote:   main @ origin (https://github.com/bmait101/driftless-trout-presence.git)
    ## Head:     [2131a9d] 2021-08-04: finally fixed the calculations!

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
