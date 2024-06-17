
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
library(targets)
tar_visnetwork()
#> Google Chrome was not found. Try setting the `CHROMOTE_CHROME` environment variable to the executable of a Chromium-based browser, such as Google Chrome, Chromium or Brave.
#> PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

<div class="visNetwork html-widget html-fill-item" id="htmlwidget-46d47c5e9137d0f3e1c4" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-46d47c5e9137d0f3e1c4">{"x":{"nodes":{"name":["adding_intersecting_flows","admin_int","basin","basin_entry","basins","flowmet_grt_strahler_1_order","flowmet_intersect","flowmet_join_nhdplus","nhdplus","pod","pou","pou_pod_together","pou_pod_together_fs_intersection","pou_pod_together_sf","pou_pod_together_sf_final_joined"],"type":["stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem"],"description":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"status":["uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate","uptodate"],"seconds":[9.140000000000001,10.89,0.03,1.41,8.5,0.02,48.08,0.03,3.46,6.91,19.86,0.01,0.03,0.24,0.03],"bytes":[106603,41770,6827,7340,99677,240469,245341,529609,1121784,28722,127801,33802,33410,33255,109175],"branches":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"label":["adding_intersecting_flows","admin_int","basin","basin_entry","basins","flowmet_grt_strahler_1_order","flowmet_intersect","flowmet_join_nhdplus","nhdplus","pod","pou","pou_pod_together","pou_pod_together_fs_intersection","pou_pod_together_sf","pou_pod_together_sf_final_joined"],"color":["#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823","#354823"],"id":["adding_intersecting_flows","admin_int","basin","basin_entry","basins","flowmet_grt_strahler_1_order","flowmet_intersect","flowmet_join_nhdplus","nhdplus","pod","pou","pou_pod_together","pou_pod_together_fs_intersection","pou_pod_together_sf","pou_pod_together_sf_final_joined"],"level":[8,3,2,1,7,6,3,5,4,4,3,5,7,6,9],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"]},"edges":{"from":["admin_int","pou_pod_together_sf","flowmet_join_nhdplus","adding_intersecting_flows","flowmet_grt_strahler_1_order","basin","basin","pou","basins","pou_pod_together_fs_intersection","basin","flowmet_grt_strahler_1_order","pou_pod_together_sf","pod","pou","flowmet_intersect","basin_entry","pou_pod_together","flowmet_intersect","nhdplus","basin"],"to":["pou_pod_together_fs_intersection","pou_pod_together_fs_intersection","flowmet_grt_strahler_1_order","pou_pod_together_sf_final_joined","pou_pod_together_sf_final_joined","admin_int","pod","pod","adding_intersecting_flows","adding_intersecting_flows","flowmet_intersect","basins","basins","pou_pod_together","pou_pod_together","nhdplus","basin","pou_pod_together_sf","flowmet_join_nhdplus","flowmet_join_nhdplus","pou"],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":{"type":"cubicBezier","forceDirection":"horizontal"}},"physics":{"stabilization":false},"interaction":{"zoomSpeed":1},"layout":{"hierarchical":{"enabled":true,"direction":"LR"}}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":{"text":"","style":"font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;"},"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":{"from":1,"to":1},"algorithm":"hierarchical","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":true,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"},"legend":{"width":0.2,"useGroups":false,"position":"right","ncol":1,"stepX":100,"stepY":100,"zoom":true,"nodes":{"label":["Up to date","Stem"],"color":["#354823","#899DA4"],"shape":["dot","dot"]},"nodesToDataframe":true},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);"},"evals":[],"jsHooks":[]}</script>
