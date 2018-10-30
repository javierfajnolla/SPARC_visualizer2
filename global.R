library(tidyverse)
library(raster)
library(rgdal)

# Load data to display

# Zonation solution
# solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed.tif") %>% 
#   aggregate(fact = 10, fun = mean)
# proj4string(solution) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(solution, "data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")  
solution <- raster("data/solutions/NG_Birds_CAZ_hfp_pa.CAZ_MDE.wrscr.compressed_agg10.tif")

mask <- solution / solution

# Carbon storage
# carbon_stor <- raster("data/solutions/total_carbon.tif") %>%
#   aggregate(fact = 10, fun = sum) %>%
#   crop(mask) %>%
#   `*` (mask)   # There is data in pixels that are not in the zonation solution
# proj4string(carbon_stor) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# writeRaster(carbon_stor, "data/solutions/total_carbon_agg10.tif")
carbon_stor <- raster("data/solutions/total_carbon_agg10.tif")

## Max
max_carbon <- carbon_stor %>% values %>% sum(na.rm = T)

################## Table of carbon offset for each percentage
# tbl <- tibble(perc_pixels = seq(0.01, 1, by = 0.01))
# tbl$cutoff_sol <- NA
# 
# # Sorted priority values (to select x% with higher priority)
# sol_values_sort <- solution %>% values %>% sort %>% rev
# 
# # Cutoff in solution to retain each percentage of the study area
# for (j in 1:nrow(tbl)){
#   tbl$cutoff_sol[j] <- sol_values_sort[(length(sol_values_sort) * tbl$perc_pixels[j]) %>% round(0)]
# }
# 
# # Carbon storage saved by protecting each percentage of the study area
# tbl$carbon_strg <- NA
# for(i in tbl$perc_pixels){#[is.na(tbl$carbon_strg)]){
#   sel_thr <- tbl %>%
#     filter(perc_pixels == i) %>%
#     pull(cutoff_sol)
# 
#   sol_rcl <- solution %>%
#     reclassify(tibble(from = c(0, sel_thr),
#                       to = c(sel_thr, max(values(solution), na.rm = T)),
#                       becomes = c(NA, 1)))
# 
#   tbl$carbon_strg[tbl$perc_pixels == i] <- (sol_rcl * carbon_stor) %>% values %>% sum(na.rm = T)
# }
# 
# tbl <- tbl %>%
#   mutate(prop_carbon_strg = carbon_strg / max_carbon)
# 
# saveRDS(tbl, "data/tables/tbl.rds")
  
### Load precalculated table to speed up
tbl <- readRDS("data/tables/tbl.rds")

##################

