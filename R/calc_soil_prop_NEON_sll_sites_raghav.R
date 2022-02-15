#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Calculate Soil Properties at NEON sites
# Written by Pushpendra Raghav
# Email: ppushpendra@crimson.ua.edu
# Feb 14, 2022
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
library(dplyr)
source('/Users/raghav/Library/CloudStorage/Box-Box/FVS_NEON/Scripts/R/funs_to_estimate_soil_properties/pedotransfer.R')
df_silt_sand_clay <- read.csv('/Users/raghav/Library/CloudStorage/Box-Box/FVS_NEON/NEON_Soil_data/NEON_soil_data_sand_silt_clay_fraction_all_sites.csv')
sites <- unique(df_silt_sand_clay$siteID)
soil_dens_files <- list.files('/Users/raghav/Library/CloudStorage/Box-Box/FVS_NEON/NEON_Soil_data/NEON_soil-megapit/',
                              pattern = "_perbulksample", recursive = TRUE, full.names = TRUE)
df_final <- NULL
for (site in sites){
  print(site)
  # Sand-Sil-Clay content
  temp <- subset(df_silt_sand_clay, siteID==site)
  temp <- temp %>%
    arrange(horizonBottomDepth)
  idx <- which.min(abs(temp$horizonBottomDepth - 100))  # 100 cm
  temp <- temp[1:idx,]
  siteID <- temp['siteID']
  horizonTopDepth <- temp$horizonTopDepth
  horizonBottomDepth <- temp$horizonBottomDepth # Depth below the soil surface of the bottom of a soil horizon
  sand <-temp$sandTotal   # sand content [%] (Up to near 100 cm)
  silt <- temp$siltTotal  # silt content [%] (Up to near 100 cm)
  clay <- temp$clayTotal  # clay content [%] (Up to near 100 cm)
  # Bulk density of soil samples
  file <- grep(site, soil_dens_files, value = TRUE)
  temp <- read.csv(file)
  temp <- temp %>%
    arrange(temp$bulkDensBottomDepth)
  #idx <- which.min(abs(temp$bulkDensBottomDepth - 100))  # 100 cm
  temp <- temp[1:idx,]
  bulkDens <- temp$bulkDensExclCoarseFrag # Bulk density of soil excluding coarse fragments (>2 mm) [gm cm-3 or Mg m3] (Up to near 100 cm)
  soilpro <- cbind(horizonBottomDepth, bulkDens) %>%
    cbind(., clay) %>%
    cbind(., silt) %>%
    cbind (., sand) # Matrix of n x 5 matrix of soil composition with the following columns 1. depth (cm), 2. bulk density (Mg/m3), 3. clay (\%), 4. silt (\%), 5. sand (\%)
  soil_prop_out <- pedotransfer(soilpro = soilpro, model = 1)
  soil_prop_out <- as.data.frame(soil_prop_out)
  
  # Save the output
  soilpro <- as.data.frame(soilpro)
  temp <- cbind(siteID, horizonTopDepth) %>%
    cbind(., horizonBottomDepth) %>% cbind(., soilpro) %>% cbind(., soil_prop_out)
  df_final <- rbind(df_final, temp)
}
write.csv(df_final, '/Users/raghav/Library/CloudStorage/Box-Box/FVS_NEON/Scripts/R/funs_to_estimate_soil_properties/NEON_soil_properties_all_sites_raghav.csv', row.names = FALSE)
write.csv(df_final, '/Users/raghav/Library/CloudStorage/Box-Box/FVS_NEON/NEON_Soil_data/NEON_soil_properties_all_sites_raghav.csv', row.names = FALSE)