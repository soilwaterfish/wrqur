
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wrqur

Water Rights Quantification/Uses for R provides methods to retrieval
water data and compare to allocation use.

## Installation

You can install the development version of wrqur from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soilwaterfish/wrqur")
```

## Example

``` r
# Load packages required to define the pipeline:
library(targets)
library(future)
library(future.callr)
plan(callr)

# Set target options:
tar_option_set(
  tidy_eval = TRUE, packages = c("sf", "tidyverse", "wrqur", "nhdplusTools", "furrr")
)

list(
tar_target(basin_entry, "data/basin.shp", format = 'file'),

tar_target(basin, read_sf(basin_entry)),

tar_target(admin_int, suppressMessages(get_adminboundaries(filter_geom = sf::st_bbox(basin),
                                                           where = "OWNERCLASSIFICATION='USDA FOREST SERVICE' OR OWNERCLASSIFICATION='UNPARTITIONED RIPARIAN INTEREST'")%>%
                                         sf::st_transform(sf::st_crs(basin)) %>%
                                         sf::st_make_valid() %>%
                                         sf::st_intersection(basin) %>%
                                         sf::st_union() %>%
                                         sf::st_as_sf())),

tar_target(flowmet_intersect, suppressMessages(get_flowmet(filter_geom = sf::st_bbox(basin),
                                                fields = c("MAUG_HIST", "COMID")) %>%
                                      sf::st_transform(sf::st_crs(basin)) %>%
                                      sf::st_intersection(basin))),

tar_target(nhdplus, nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(flowmet_intersect)))),

tar_target(flowmet_join_nhdplus, flowmet_intersect %>% dplyr::select(MAUG_HIST, COMID) %>%
    dplyr::left_join(nhdplus %>%
                       sf::st_drop_geometry() %>%
                       dplyr::mutate(comid = as.character(comid)), by = c('COMID' = 'comid')) %>%
      dplyr::filter(ftype %in% c('StreamRiver'))
),

tar_target(pou, get_mtwr(basin, layer = 'WR1POU', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup()),

tar_target(pod, get_mtwr(basin, layer = 'WR1DIV', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup() %>%
             dplyr::filter(WRKEY %in% pou$WRKEY)),

tar_target(pou_pod_together, suppressMessages(pod %>%
                                                  dplyr::left_join(pou %>%
                                                                     sf::st_drop_geometry() %>%
                                                                     dplyr::select(c("WRKEY", "PURPOSE", "IRRTYPE", "MAXACRES", "FLWRTGPM", "FLWRTCFS", "VOL", "ACREAGE"))))
),

tar_target(pou_pod_together_sf, date_cleaning(pou_pod_together)),

tar_target(flowmet_grt_strahler_1_order, flowmet_join_nhdplus %>% filter(streamorde > 1)),

tar_target(basins, get_pod_basins(flowmet_grt_strahler_1_order, sf::st_crs(pou_pod_together_sf))),

tar_target(pou_pod_together_fs_intersection, fs_logic(pou_pod_together_sf, admin_int)),

tar_target(adding_intersecting_flows, basins %>% split(.$COMID) %>%
             furrr::future_map(
               ~capture_sites_within(.x, pou_pod_together_fs_intersection)) %>%
             dplyr::bind_rows() %>%
             sf::st_as_sf()),
tar_target(pou_pod_together_sf_final_joined, adding_intersecting_flows %>%
             st_drop_geometry() %>%
             left_join(flowmet_grt_strahler_1_order %>% select(COMID,MAUG_HIST, gnis_name, qe_08)) %>%
             st_as_sf() %>%
             mutate(
               intersecting_flow_all_together_percent = (intersecting_flow_all_together/MAUG_HIST)*100,
               intersecting_flow_fs_percent = (intersecting_flow_fs/MAUG_HIST)*100,
               intersecting_flow_private_percent = (intersecting_flow_private/MAUG_HIST)*100
             ))
)
```
