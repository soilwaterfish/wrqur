library(tidyverse)
library(sf)
devtools::document()
devtools::install()
library(wrqur)



koot <- gwavr::get_nldi_interactively()

basin <- koot[[1]]$basin_boundary

write_sf(basin, 'data/little_missouri.shp')

basin <- read_sf('data/little_missouri.shp')


admin_int <- suppressMessages(get_adminboundaries(filter_geom = sf::st_bbox(basin),
                                                           where = "OWNERCLASSIFICATION='USDA FOREST SERVICE' OR OWNERCLASSIFICATION='UNPARTITIONED RIPARIAN INTEREST'")%>%
                                         sf::st_transform(sf::st_crs(basin)) %>%
                                         sf::st_make_valid() %>%
                                         sf::st_intersection(basin) %>%
                                         sf::st_union() %>%
                                         sf::st_as_sf())

flowmet_intersect <- suppressMessages(get_flowmet(filter_geom = sf::st_bbox(basin),
                                                           fields = c("MAUG_HIST", "COMID")) %>%
                                                 sf::st_transform(crs = basin_crs_data.little_missouri.shp) %>%
                                                 sf::st_intersection(basin))

read_sf(r"{Z:\Downloads\S_USA.Hydro_FlowMet_1990s\S_USA.Hydro_FlowMet_1990s.shp}")

get_flowmet(filter_geom = basin, local_path = r"{Z:\Downloads\S_USA.Hydro_FlowMet_1990s.gdb\S_USA.Hydro_FlowMet_1990s.gdb}")  %>%
  sf::read_sf()%>%
  dplyr::select(c("MAUG_HIST", "COMID"))


nhdplus <- nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(flowmet_intersect)))

flowmet_join_nhdplus <- flowmet_intersect %>% dplyr::select(MAUG_HIST, COMID) %>%
             dplyr::left_join(nhdplus %>%
                                sf::st_drop_geometry() %>%
                                dplyr::mutate(comid = as.character(comid)), by = c('COMID' = 'comid')) %>%
             dplyr::filter(ftype %in% c('StreamRiver'))

plot(flowmet_join_nhdplus[,c('MAUG_HIST', 'geometry')])

pou <- get_mtwr(basin, layer = 'WR1POU', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup()

pod <- get_mtwr(basin, layer = 'WR1DIV', local_path =  r'{Z:\Downloads\MTWaterRights.gdb\MTWaterRights.gdb}') %>%
             sf::read_sf() %>%
             dplyr::group_by(WRKEY) %>%
             dplyr::slice(1) %>%
             dplyr::ungroup() %>%
             dplyr::filter(WRKEY %in% pou$WRKEY)

pou_pod_together <- suppressMessages(pod %>%
                                      dplyr::left_join(pou %>%
                                                       sf::st_drop_geometry() %>%
                                                       dplyr::select(c("WRKEY", "PURPOSE", "IRRTYPE",
                                                                       "MAXACRES", "FLWRTGPM", "FLWRTCFS",
                                                                       "VOL", "ACREAGE"))))

plot(pou_pod_together[,c('geometry')])

pou_pod_together %>% st_drop_geometry() %>%
dplyr::filter(!is.na(PER_DIV))

pou_pod_together_sf <- date_cleaning(pou_pod_together)

flowmet_grt_strahler_1_order <-  flowmet_join_nhdplus %>% filter(streamorde > 1)

basins <- get_pod_basins(flowmet_grt_strahler_1_order, sf::st_crs(pou_pod_together_sf))

pou_pod_together_fs_intersection <- fs_logic(pou_pod_together_sf, admin_int)

adding_intersecting_flows <- basins %>% split(.$COMID) %>%
             furrr::future_map(
               ~capture_sites_within(.x, pou_pod_together_fs_intersection)) %>%
             dplyr::bind_rows() %>%
             sf::st_as_sf()
pou_pod_together_sf_final_joined <- adding_intersecting_flows %>%
             st_drop_geometry() %>%
             left_join(flowmet_grt_strahler_1_order %>% select(COMID,MAUG_HIST, gnis_name, qe_08)) %>%
             st_as_sf() %>%
             mutate(
               intersecting_flow_all_together_percent = (intersecting_flow_all_together/MAUG_HIST)*100,
               intersecting_flow_fs_percent = (intersecting_flow_fs/MAUG_HIST)*100,
               intersecting_flow_private_percent = (intersecting_flow_private/MAUG_HIST)*100
             )

plot(pou_pod_together_sf_final_joined[,c('intersecting_flow_fs_percent', 'geometry')])
plot(pou_pod_together$geometry, add = TRUE)

mapview::mapview(pou_pod_together_sf_final_joined)

library(ggtext)

ggplot() +
  geom_sf(data = pou_pod_together_sf_final_joined, aes(color = intersecting_flow_fs_percent)) +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1')) +
  geom_sf(data = pou_pod_together_sf, shape = 21) +
  resourceviz::custom_theme(map_void = 4)
