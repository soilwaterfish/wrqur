#' Plot Consumptive Outputs
#'
#' @param data A previously created `get_consumptive_estimate()` list.
#'
#' @return
#' @export
#'

plot_consumptive <- function(data) {

  basin <- data$basin %>% dplyr::mutate(percent_of_flow = round(as.numeric(data$comparison)*100,3))

  mapview::mapview(list(data$pod_pou_joined, data$admin_boundary, data$flow_met),
                   alpha.regions = list(0.9, 0.2, 0.6),
                   #col.regions = list(NULL, NULL, 'forestgreen'),
                   zcol = list('FLWRTCFS', NULL, NULL)) +
    mapview::mapview(basin, alpha.regions = 0)

}
