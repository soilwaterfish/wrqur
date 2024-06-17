#' Get Consumptive Estimate
#'
#' @param data A sf POLYGON or MULTIPOLYGON object.
#' @param return_everything A logical for return spatial objects and results (TRUE) or just tabular results (FALSE).
#' @param local_path A file path character string to the MT gdb for water rights POU and POD layers. 'Z:/some_path/wr_rights.gdb'
#' @return
#' @export
#' @importFrom dplyr "%>%"
#'
#' @examples
get_consumptive_estimate <- function(data, local_path = NULL, return_everything = TRUE) {

    basin <- sf::st_as_sf(gwavr:::get_NLDI(data)$basin_boundary) %>% dplyr::mutate(basin = 'user_selected_basin')

    print('Downloading Admin Boundaries')
    admin_int <- suppressMessages(get_adminboundaries(filter_geom = sf::st_bbox(basin),
                               where = "OWNERCLASSIFICATION='USDA FOREST SERVICE' OR OWNERCLASSIFICATION='UNPARTITIONED RIPARIAN INTEREST'")%>%
                        sf::st_transform(sf::st_crs(basin)) %>%
                        sf::st_make_valid() %>%
                        sf::st_intersection(basin) %>%
                        sf::st_union() %>%
                        sf::st_as_sf())

    print('Downloading FlowMet')
    fm_int <- suppressMessages(get_flowmet(filter_geom = sf::st_bbox(basin),
                        fields = c("MAUG_HIST", "COMID")) %>%
                        sf::st_transform(sf::st_crs(basin)) %>%
                        sf::st_intersection(basin))
#
#     fm_int <- fm_int %>% dplyr::mutate(comid = as.integer(COMID)) %>% dplyr::select(-COMID)
#
#     fm_int <- suppressMessages(nhdplusTools::get_nhdplus(sf::st_as_sfc(sf::st_bbox(fm_int))) %>%
#                         sf::st_drop_geometry() %>%
#                         dplyr::right_join(fm_int, by = 'comid'))
#

    print('Downloading MT POU')

    pou <- get_mtwr(admin_int, layer = 'WR1POU', local_path = local_path) %>%
                        sf::read_sf()

    print('Downloading MT POD')

    pod <- get_mtwr(admin_int, layer = 'WR1DIV', local_path = local_path) %>%
                        sf::read_sf()


    pou <- pou %>%
          dplyr::group_by(WRKEY) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup()

    pod <- pod %>%
           dplyr::group_by(WRKEY) %>%
           dplyr::slice(1) %>%
           dplyr::ungroup() %>%
           dplyr::filter(WRKEY %in% pou$WRKEY)

    print('Tidying Data')

    pou_pod_together <- suppressMessages(pod %>%
                              dplyr::left_join(pou %>%
                                       sf::st_drop_geometry() %>%
                                         dplyr::select(c("WRKEY", "PURPOSE", "IRRTYPE", "MAXACRES", "FLWRTGPM", "FLWRTCFS", "VOL", "ACREAGE"))))

    pou_pod_together <- pou_pod_together %>%
                      tidyr::separate_wider_delim(PER_DIV, delim = ' to ', names = c('date1', 'date2')) %>%
                      dplyr::mutate(date1 = as.Date(paste0('2000-', stringr::str_replace_all(date1, '/', '-'))),
                             date2 = as.Date(paste0('2000-', stringr::str_replace_all(date2, '/', '-'))),
                             date_logic = as.Date('2000-08-15') > date1 & as.Date('2000-08-15') < date2) %>%
                      dplyr::filter(date_logic) %>%
                      dplyr::select(-date1, -date2, -date_logic) %>%
                      sf::st_as_sf()

  fm_int_tog <- suppressMessages(fm_int %>% sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(sf::st_crs(pou_pod_together)) %>%
    sf::st_line_merge())

  closest_points <- suppressMessages(data %>%
                                       sf::st_transform(sf::st_crs(fm_int_tog)) %>%
    dplyr::mutate(
      my_linestring = sf::st_nearest_points(geometry, fm_int_tog),
      closest_point = sf::st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
    ))

  closest_points_threshold <- closest_points %>%
    dplyr::mutate(
      distance = sf::st_distance(geometry, fm_int_tog)[,1],
      distance2 = sf::st_length(my_linestring), # not used, demo purposes only
      snapped_point_cond = sf::st_as_sf(sf::st_sfc(ifelse(as.numeric(distance) <= 400, sf::st_geometry(closest_point), geometry), crs = sf::st_crs(fm_int_tog)))
    )

  # new_pou_pod_together <- suppressMessages(closest_points_threshold$snapped_point_cond %>%
  #                         dplyr::rename(geometry = 'x') %>%
  #                         dplyr::mutate(WRKEY = pou_pod_together$WRKEY) %>%
  #                         sf::st_as_sf())

  fm_int_flow <- sf::st_intersects(fm_int %>%
                                     sf::st_transform(sf::st_crs(fm_int_tog)) %>%
                                     sf::st_buffer(1), closest_points_threshold$snapped_point_cond)

  fm_int_flow <- fm_int[lengths(fm_int_flow) > 0, ] %>% sf::st_drop_geometry()

  print('Wrapping up')

  # adding_intersecting_flows <- suppressMessages(purrr::map2(basin_wrkeys, basin_wrkeys_names,
  #      ~capture_sites_within(.x, .y, pou_pod_together)) %>%
  #       dplyr::bind_rows() %>%
  #       sf::st_as_sf())

#
#   adding_intersecting_flows_df <- suppressMessages(pou_pod_together %>% sf::st_drop_geometry() %>%
#                                                     dplyr::mutate())

  if(return_everything){
  list(comparison = sum(pou_pod_together$FLWRTCFS, na.rm = TRUE)/fm_int_flow$MAUG_HIST,
       pod_pou_joined = pou_pod_together,
       admin_boundary = admin_int,
       flow_met = fm_int,
       basin = basin
  )

  } else {
    adding_intersecting_flows_df
  }

}


#' Get Montana Water Rights Data
#' @param filter_geom an object of class bbox be used for clipping.
#' @param layer Layer name within the `https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip` gdb.
#' @param local_path A file path character string to the MT gdb for water rights POU and POD layers. 'Z:/some_path/wr_rights.gdb'
#' @return
#' @export
#'
get_mtwr <- function(filter_geom, layer, local_path = NULL) {

  url <- 'https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip'

  tmp <- tempfile(fileext = '.shp')

  filter_geom <- sf::st_transform(filter_geom, 32100)

  bb <- sf::st_bbox(filter_geom)

  tmpclp <- tempfile(fileext = '.shp')

  sf::write_sf(filter_geom, tmpclp)

  if(!is.null(local_path)){


    system(paste('ogr2ogr -spat ',
                 paste(bb[[1]],
                       bb[[2]],
                       bb[[3]],
                       bb[[4]]),
                 '-clipsrc ', tmpclp,
                 ifelse(layer == 'WR1POU', '-where "WRSTATUS = \'ACTIVE\' AND FLWRTCFS IS NOT NULL"',
                        '-where "WR_STATUS = \'ACTIVE\' AND SRCTYPE = \'SURFACE\'"'),
                 ' -f "ESRI Shapefile"',
                 tmp,
                 local_path,
                 paste(layer, sep = " ", collapse = " ")), intern = TRUE)
  } else {

  system(paste('ogr2ogr -spat ',
               paste(bb[[1]],
                     bb[[2]],
                     bb[[3]],
                     bb[[4]]),
               '-clipsrc ', tmpclp,ifelse(layer == 'WR1POU', '-where "WRSTATUS = \'ACTIVE\' AND FLWRTCFS IS NOT NULL"',
                                          '-where "WR_STATUS = \'ACTIVE\' AND SRCTYPE = \'SURFACE\'"'),

               ' -f "ESRI Shapefile"',
               tmp,
               paste0('/vsizip//vsicurl/', url),
               paste(layer, sep = " ", collapse = " ")), intern = TRUE)
  }

  tmp
}


#' Get FlowMet Data
#'
#' @description
#' A map service depicting modeled streamflow metrics from the historical time period (1977-2006) in the United States.
#' In addition to standard NHD attributes, the streamflow datasets include metrics on mean daily flow (annual and seasonal),
#' flood levels associated with 1.5-year, 10-year, and 25-year floods; annual and decadal minimum weekly flows and date of
#'  minimum weekly flow, center of flow mass date; baseflow index, and average number of winter floods. These files and
#'  additional information are available on the project website, https://www.fs.usda.gov/rm/boise/AWAE/projects/modeled_stream_flow_metrics.shtml. Streams without flow metrics (null values) were removed from this dataset to improve display speed; to see all stream lines, use an NHD flowline dataset.
#'
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @return
#' @export
#'
get_flowmet <- function(filter_geom, ...){


  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_HydroFlowMetricsHistorical_01/MapServer/15')

  flowmet <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

}


#' Get USDA-Forest Service Administration Boundaries
#'
#' @description
#' An area depicted as surface ownership parcels dissolved on the same ownership classification. Go to this URL for full metadata description: https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.BasicOwnership.xml
#'
#'
#' @param filter_geom an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @return
#' @export
#'
get_adminboundaries <- function(filter_geom, ...) {

url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_BasicOwnership_01/MapServer/0')

admin <- arcgislayers::arc_select(url, filter_geom = filter_geom, ...)

}


