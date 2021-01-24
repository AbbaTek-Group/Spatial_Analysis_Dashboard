library(rgdal)
library(sf)
library(tidyverse)
library(bcmaps)
library(tmap)
library(magick)

# The input file geodatabase
fgdb0 <- "D:/BC_Spatial/CPCAD-BDCAPC_2019.gdb/CPCAD-BDCAPC_2019.gdb"
fgdb1 <- "D:/BC_Spatial/CPCAD-BDCAPC_Dec2019.gdb/CPCAD-BDCAPC_Dec2019.gdb"
fgdb2 <- "D:/BC_Spatial/CPCAD-BDCAPC-2018.gdb/CPCAD_2018.gdb"

fgdb <- list(fgdb0,fgdb1,fgdb2)

  list.features <- function(filename){       # List all feature classes in a file geodatabase

subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(filename)
print(fc_list)

  }
  
all.featureclasses <- map(fgdb, list.features)  
  

# Read and save to file the feature class
setwd('D:/BC_Spatial/')
readOGR(dsn=fgdb0, layer = 'CPCAD_2019v1') %>% saveRDS('feateureclass_June2019')           
readOGR(dsn=fgdb1, layer = 'CPCAD_Dec2019') %>% saveRDS('feateureclass_Dec2019')
readOGR(dsn=fgdb2, layer = 'CPCAD_2018v1') %>% saveRDS('feateureclass_Dec2018')

# Determine the FC extent, projection, and attribute information
fc_0 <-readRDS('D:/BC_Spatial/feateureclass_June2019')
fc_1 <-readRDS('D:/BC_Spatial/feateureclass_Dec2019')
fc_2 <-readRDS('D:/BC_Spatial/feateureclass_Dec2018')

# view summary features
summary(fc_0)
summary(fc_1)
summary(fc_2)


# convert feature class to sf

fc_0 <- st_as_sf(fc_0)
fc_1 <- st_as_sf(fc_1)
fc_2 <- st_as_sf(fc_2)

fc_0 %>% filter(LOC_E == 'British Columbia') %>% saveRDS('CPCAD_BCJune2019')      # subset spatial and non-spatial data for BC
fc_1 %>% filter(LOC_E == 'British Columbia') %>% saveRDS('CPCAD_BCDec2019')
fc_2 %>% filter(LOC_E == 'British Columbia') %>% saveRDS('CPCAD_BCDec2018')

# Eco-regions feature class

fc_0 <- readRDS('D:/BC_Spatial/Code/CPCAD_BCJune2019')
fc_1 <- readRDS('D:/BC_Spatial/Code/CPCAD_BCDec2019')
fc_2 <- readRDS("D:/BC_Spatial/Code/CPCAD_BCDec2018")

BC_ecoreg <- st_read("D:/BC_Spatial/ECOregions_BC/ERC_ECOREGIONS_SP/ERC_ECOREG_polygon.shp") 


# View the feature class
plot1 <- plot(fc_0)
plot2 <- plot(BC_ecoreg)


# combining all feature classes to plot animated map
fc_0$YEAR <- as.Date(paste0("2019-", "06","-01"),"%Y-%m-%d")
fc_1$YEAR <- as.Date(paste0("2019-", "12","-01"),"%Y-%m-%d")
fc_2$YEAR <- as.Date(paste0("2018-", "12","-01"),"%Y-%m-%d")

fc_3_combined <- cbind(fc_0[c(1:1287),],fc_1[c(1:1287),], fc_2)
biome_changes <- fc_3_combined %>% filter(BIOME == 'T' & BIOME.1 == 'T' & BIOME.2 == 'T')

# faceted plot of biome changes over time

  tm_shape(fc_3_combined) + tm_polygons() + tm_shape(biome_changes) +
    tm_symbols(col = 'black', border.col = 'white', size = 'O_AREA')+ 
      tm_facets(along = "YEAR", free.coords = FALSE)
    
    

# plots of percentage change in protected and conserved terrestrial areas  
  
  BiomeT_June2018 <- BC_June2018 %>% filter(BIOME == 'T')%>% group_by(TYPE_E)%>% summarise(area_des_ha = as.numeric(sum(O_AREA)) * 1e-4) %>%
    mutate(percent_des = (area_des_ha * 1e4) / as.numeric(sum(BC_Dec2019$O_AREA)) * 100) %>%
    mutate_if(is.numeric, round, digits = 2)%>% mutate(Date = as.Date(paste0("2018-", 06, "-01"), "%Y-%m-%d"))
  
  BiomeT_June2019 <- BC_June2019 %>% filter(BIOME == 'T')%>% group_by(TYPE_E)%>% summarise(area_des_ha = as.numeric(sum(O_AREA)) * 1e-4) %>%
    mutate(percent_des = (area_des_ha * 1e4) / as.numeric(sum(BC_Dec2019$O_AREA)) * 100) %>% mutate_if(is.numeric, round, digits = 2)%>%
     mutate(Date = as.Date(paste0("2019-", 06, "-01"), "%Y-%m-%d"))
  
  BiomeT_Dec2019 <- BC_Dec2019 %>% filter(BIOME == 'T')%>% group_by(TYPE_E)%>% summarise(area_des_ha = as.numeric(sum(O_AREA)) * 1e-4) %>%
    mutate(percent_des = (area_des_ha * 1e4) / as.numeric(sum(BC_Dec2019$O_AREA)) * 100) %>% mutate_if(is.numeric, round, digits = 2)%>% 
    mutate(Date = as.Date(paste0("2019-", 12, "-01"), "%Y-%m-%d"))
  
#  Transform each spatial object to B.C projection crs
  
  BiomeT_June2018 <- transform_bc_albers(BiomeT_June2018)
  BiomeT_June2019 <- transform_bc_albers(BiomeT_June2019)
  BiomeT_Dec2019 <- transform_bc_albers(BiomeT_Dec2019)
  
# combine spatial objects for ploting
  BiomeT_combined <- rbind(BiomeT_June2018, BiomeT_June2019, BiomeT_Dec2019)
  
  # plot spatial objects
  
  tm_shape(BiomeT_combined) +
    tm_polygons() + tm_polygons(col = "Protected Area") + tm_polygons(col = "Conservation Area")
    tm_symbols(col = "black", border.col = "white", size = "area_des_ha") +
    tm_facets(by = "Date", nrow = 2, free.coords = FALSE)
