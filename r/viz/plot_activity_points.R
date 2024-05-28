# script for viewing point data

library(RColorBrewer)


source("r/help_fun/loadPointData.R")

# plot home points

home_points_sf <- HOME_points %>% sample_n(25000) %>%  st_as_sf(coords = c("X", "Y"), crs = 27700)
home_points_html <- mapview::mapview(home_points_sf, cex = 0.5, alpha = 0, alpha.regions = 0.5, col.regions = brewer.pal(8, "Greys")[7] )
mapview::mapshot(home_points_html, url = "plot/home_points.html")
rm(home_points_html)


# plot work points
work_points_sf <- WORK_points %>% st_as_sf(coords = c("X", "Y"), crs = 27700)
work_points_html <- mapview::mapview(work_points_sf, cex = 0.5, alpha = 0, alpha.regions = 0.5, zcol = "sector", col.regions = brewer.pal(5, "Spectral") )
mapview::mapshot(work_points_html, url = "plot/work_points.html")
rm(work_points_html)












