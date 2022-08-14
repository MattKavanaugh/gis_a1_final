# Setup Environment --------------------------------------------------------------------

# load packages

# for rpubs
library(rmarkdown)
library(knitr)

# for input / outputs
library(here)

# for data manipulation
library(tidyverse)
library(tidyselect)
library(magrittr)
library(janitor)

# for spatial data manipulation
library(sf)

# for OpenTripPlanner (OTP)
library(osmextract)
library(opentripplanner)
#library(usethis)

# for plotting
library(ggsn)
library(ggspatial)
library(tmap)
library(tmaptools)
library(cowplot)

# set optional preferences

# set working directory for our inputs/outputs
#setwd()

# set tmap view mode to static or interactive ("plot" or "view")
tmap_mode("plot")

# free memory (fm) not being used to lessen the load on our computer
fm <- gc()

# OTP requires coordinates to be in CRS 4326 (WGS 84) so we will set a common crs
common_crs = 4326

# set functions

# OTP also requires latitude and longitude coordinates as numeric values in two separate columns
# we will use the function below split the geometry column accordingly 
# source: https://rpubs.com/Hussein-Mahfouz/Accessibility-Analysis-Cairo
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# PART 1a - Load and Clean Municipal Data -------------------------------------

# Import municipal facility shapefiles into R.
# shapefiles downloaded from Open Hamilton https://open.hamilton.ca/
lib <- st_read(here::here("data/raw/Libraries.shp"),
               stringsAsFactors = FALSE)
rcc <- st_read(here::here("data/raw/Recreation_and_Community_Centres.shp"),
               stringsAsFactors = FALSE)
sc <- st_read(here::here("data/raw/Municipal_Service_Centres.shp"),
              stringsAsFactors = FALSE)

# have a peek
str(lib)
str(rcc)
str(sc)

# select desired columns (facility name and geometry)
lib <- lib %>% 
  select(2, 12)
rcc <- rcc %>% 
  select(2, 5)
sc <- sc %>% 
  select(2, 7)

# change CRS from NAD83 / UTM zone 17N to CRS 4326 (WGS 84) to facilitate merge
lib <- lib %>% st_transform(common_crs)
rcc <- rcc %>% st_transform(common_crs)
sc <- sc %>% st_transform(common_crs)

# plot together
ggplot() + 
  geom_sf(data = rcc, color = 'orange') +
  geom_sf(data = lib, color = 'blue') +
  geom_sf(data = sc, color = 'red') +
  blank()

# merge all into one community spaces (cspaces) shape
cspaces = bind_rows(lib, rcc)
cspaces = bind_rows(cspaces, sc)

# plot to confirm merge
ggplot() + 
  geom_sf(data = cspaces, color = 'green') +
  blank()

# final data check
summary(cspaces)
str(cspaces)

# save community spaces data
st_write(obj = cspaces,
         here::here("data/clean/cspaces.gpkg"),
         delete_layer = TRUE)

# free memory
fm

# PART 1b - Load and Clean Census Data -----------------------------------------------

# NOTE 1 - census divisions (CDs)
# Statistics Canada definition: 
# "intermediate geographic areas between the province/territory level and the municipality
# https://www150.statcan.gc.ca/n1/pub/92-195-x/2021001/geo/cd-dr/cd-dr-eng.htm
# As a single-tier municipality (amalgamated in 1998), the City of Hamilton has a single CD

# NOTE 2 - census dissemination areas (DAs)
# Statistics Canada definition: 
# "the smallest standard geographic area for which all census data are disseminated with an average population of 400 to 700 persons"
# https://www150.statcan.gc.ca/n1/pub/92-195-x/2021001/geo/da-ad/da-ad-eng.htm 

# NOTE 3 - census data file size
# original CD and DA shapefiles downloaded from Statistics Canada:
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# the files were too large (whole country) to upload to Github
# filters below were run locally and the files on Github are therefore already reduced
# to repeat for full file, subset data for the Province of Ontario using PRUID = 35
    #cd21 <- cd21 %>%
    #  filter(str_detect(PRUID, "^35"))
    #da21 <- da21 %>%
    #  filter(str_detect(PRUID, "^35"))

# NOTE 4 - census population data at the DA level downloaded from Statistics Canada:
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810002301

# - - - - - - - - - - - - 

# Hamilton CD

# load CD boundaries
cd21 <- st_read(here::here("data/raw/CD_Boundary_ON.shp"),
                stringsAsFactors = FALSE)

# change CRS from NAD83 / UTM zone 17N to CRS 4326 (WGS 84)
cd21 <- cd21 %>% st_transform(common_crs)

# subset for Hamilton's CD boundary
cd21_ham <- cd21 %>%
  filter(str_detect(CDNAME, "^Hamilton"))

# final data check
summary(cd21_ham)
str(cd21_ham)

# check plot
qtm(cd21_ham)

# looks good!

# save hamilton (cd) boundary
st_write(obj = cd21_ham,
         here::here("data/clean/cd21_ham.gpkg"),
         delete_layer = TRUE)

# - - - - - - - - - - - - 

# Hamilton DAs

# load DA boundaries
da21 <- st_read(here::here("data/raw/DA_Boundary_ON.shp"),
                stringsAsFactors = FALSE)

# change CRS from NAD83 / UTM zone 17N to CRS 4326 (WGS 84)
da21 <- da21 %>% st_transform(common_crs)

# NOTE 4 - Hamilton DA boundaries 
# To subset for Hamilton's DAs, several id's need to be matched within the 
# Dissemination Geographies Relationship File (DGRF) Reference Guide
# for more information see Statistics Canada's website:
# https://www150.statcan.gc.ca/n1/pub/98-26-0003/982600032021001-eng.htm

# step 1 - find the DGUID for the CD for Hamilton
DGUID = (cd21_ham[cd21_ham$CDNAME=="Hamilton",]$DGUID)

# step 2 - load the DGRF
DGRF <- read_csv(here::here("data/raw/DGRF_2021.zip"), col_types = cols(.default = "c"))

# step 3 - get the CDDGUID_DRIDUGD values that match Hamilton's DGUID and 
# isolate the unique DADGUID_ADIDUGD values from the results
DGRF <- DGRF %>%
  filter(DGRF$CDDGUID_DRIDUGD==DGUID) %>% 
  distinct(DADGUID_ADIDUGD)

# step 4 - match the unique DADGUID_ADIDUGD values with Hamilton DAs
da21_ham <- da21 %>%
  filter(DGUID %in% c(DGRF$DADGUID_ADIDUGD))

# have a look
head(da21_ham, 1)

# select only needed columns (IDs and geometry)
da21_ham <- da21_ham %>%
  dplyr::select(c(1,2,5))

# final data check
summary(da21_ham)
str(da21_ham)

# check plot
qtm(da21_ham)

# looks good! 

# save hamilton (da) boundary
st_write(obj = da21_ham,
         here::here("data/clean/da21_ham.gpkg"),
         delete_layer = TRUE)

# - - - - - - - - - - - - 

# Hamilton DA population data

# Load DA population data into R.
da21_data <- read_csv(here::here("data/raw/DA_census.zip"),
                      locale = locale(encoding = "latin1"),
                      na = "n/a")

# reduce data to Hamilton (DAs) using the DA boundary file 
da21_data_ham <- da21_data %>%
  filter(DGUID %in% da21_ham$DGUID)

# Have a peek
Datatypelist <- da21_data_ham %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

# select only needed columns
da21_data_ham <- da21_data_ham %>%
  dplyr::select(c(3,4,6))
head(da21_data_ham, 1)

# rename columns
da21_data_ham <- da21_data_ham %>%
  dplyr::rename("age"=2, "pop"=3)
head(da21_data_ham, 1)

# subset data to senior population for each (DA) by time period:
# Next 10-years (55 to 59), Next 5-years (60 to 64), and Current (65+)
# looking at the census documentation reveals that these categories already 
# exist in the age column
# we will filter only these rows
da21_ham_srpop <- da21_data_ham %>%
  filter(age == "55 to 59 years" | age == "60 to 64 years" | age == "65 years and over")

# Have a peek
head(da21_ham_srpop, 5)

# flatten data frame
da21_ham_srpop <- da21_ham_srpop %>%
  pivot_wider(names_from = "age", values_from = c("pop"),
              names_sep = "."
  )  
da21_ham_srpop  

# clean column names
da21_ham_srpop <- da21_ham_srpop %>%
  clean_names()
da21_ham_srpop

# rename column for 2021 senior pop
da21_ham_srpop <- da21_ham_srpop %>%
  dplyr::rename("srpop21"=4)

# create new column with 2026 senior pop
da21_ham_srpop <- da21_ham_srpop %>%
  mutate(srpop26 = (srpop21 + x60_to_64_years))

# create new column with 2031 senior pop
da21_ham_srpop <- da21_ham_srpop %>%
  mutate(srpop31 = (srpop26 + x55_to_59_years))

# confirm
head(da21_ham_srpop, 1)

# recapitalize DGUID to facilitate merge later on
da21_ham_srpop <- da21_ham_srpop %>%
  dplyr::rename("DGUID"=1)

# select desired columns
da21_ham_srpop <- da21_ham_srpop %>%
  dplyr::select(c(1,4,5,6))

# check for NA's
summary(da21_ham_srpop)

# replace NA's with 0
da21_ham_srpop[is.na(da21_ham_srpop)] <- 0

# final data check
summary(da21_ham_srpop)
head(da21_ham_srpop, 10)

# save data
write.csv(da21_ham_srpop, 
          here::here("data/clean/da21_ham_srpop.csv"),
          row.names = FALSE)

# - - - - - - - - - - - - 

# free unused memory
fm

# PART 2 - Load and Prepare Data  --------------------------------------------------------

# NOTE 5 - rpubs file starts roughly around here

# load clean data
cd21_ham <- sf::st_read(here::here("data/clean/cd21_ham.gpkg"),
                        stringsAsFactors = FALSE)

da21_ham <- sf::st_read(here::here("data/clean/da21_ham.gpkg"),
                        stringsAsFactors = FALSE)

da21_data <- read_csv(here::here("data/clean/da21_ham_srpop.csv"),
                      locale = locale(encoding = "latin1"),
                      na = "n/a")

cspaces <- sf::st_read(here::here("data/clean/cspaces.gpkg"),
                       stringsAsFactors = FALSE)

# - - - - - - - - - - - - 

# prep DA data

# merge DA data with DA shape file on DGUID
da21_ham <- merge(x = da21_ham,
                  y = da21_ham_srpop,
                  by.x = "DGUID",
                  by.y = "DGUID")

# final data check to confirm merge
summary(da21_ham)
str(da21_ham)

# let's make some statistical variables for later
srpop21total = sum(da21_ham_srpop$srpop21)
srpop26total = sum(da21_ham_srpop$srpop26)
srpop31total = sum(da21_ham_srpop$srpop31)

# - - - - - - - - - - - - 

# create DA centroid data

# create centroids of Hamilton's DA boundaries
# another name for centroid is 'geometric center' so we will use (gc) here
da21_ham_gc <- st_centroid(da21_ham)

# this analysis uses centroids to query OPT 
# recall OTP requires latitude and longitude coordinates as numeric values in two separate columns
# we will use our function to split geometry and add results as new columns
da21_ham_gc <- sfc_as_cols(da21_ham_gc)

# confirm split with a final data check
summary(da21_ham_gc)
str(da21_ham_gc)

# check plot
qtm(da21_ham_gc)

# save hamilton (da) centroids
st_write(obj = da21_ham_gc,
         here::here("data/clean/da21_ham_gc.gpkg"),
         delete_layer = TRUE)

# - - - - - - - - - - - - 

# prep cspaces data

# final data check
summary(cspaces)
str(cspaces)

# there are 97 cspaces, let's make a variable to store this
cspacestotal <- 97

# - - - - - - - - - - - - 

# check plot together
qtm(da21_ham, fill = NULL) +
  tm_shape(da21_ham_gc) +
  tm_bubbles(col = "blue", scale = .3) +
  tm_shape(cspaces) +
  tm_bubbles(col = "red", scale = .5)

# free unused memory
fm

# PART 3a - Setup OpenTripPlanner ------------------------------------------------

# NOTE 6 - opentripplanner package setup
# sections 3a, 3B, 3C, and 3d are based on the following otp package vignettes
# see prerequisites vignette -> https://docs.ropensci.org/opentripplanner/articles/prerequisites.html
# see getting started vignette -> https://docs.ropensci.org/opentripplanner/articles/opentripplanner.html
# see advanced features vignette -> https://docs.ropensci.org/opentripplanner/articles/advanced_features.html

# NOTE 7 - Java 8 is required to run OTP
# to download visit: https://www.java.com/en/download/

# confirm correct version of Java
otp_check_java()

# NOTE 8 - folder structure
# OTP requires a certain folder structure in accordance with the documentation
# example structure:

# Top folder for storing all OTP data    / otp                         
# Subfolder containing all graphs           /graphs                     
# 1 Subfolder that acts as OTP router            /default                 
# 2 Required OSM road map                           osm.pbf              
# 3 Optional config file                            router-config.json   
# 4 Optional config file                            build-config.json    
# 5 Optional GTFS data                              gtfs.zip             
# 6 Optional Elevation data                         dem.tif     

# create otp folder
path_data <- file.path(here::here(),
                       "otp")
dir.create(path_data)

# download otp jar file into otp folder
path_otp <- otp_dl_jar(path_data, cache = FALSE)

# for some reason I could not sort out, otp requires the "graphs" folder to be 
# created through the demo function first prior to creating your own graph
# the default folder is also downloaded automatically
otp_dl_demo(path_data)

# 1 - create router subfolder
dir.create(file.path(here::here("otp/graphs/hamilton")))

# 2 - download the osm data

# NOTE 9 - this section uses the osmextract package
# website -> https://cran.r-project.org/web/packages/osmextract/index.html
# introducing osmextract vignette -> https://cran.r-project.org/web/packages/osmextract/vignettes/osmextract.html

# geocode hamilton's location
hamilton = tmaptools::geocode_OSM("Hamilton, Canada")$coords

# Check osm data for hamilton's location
oe_match(hamilton, provider = "geofabrik")
oe_match(hamilton, provider = "openstreetmap_fr")

# geofrabrik shows data is available for the province, whereas openstreetmap_fr shows data for the region
# we will go with the regional option as it is a smaller area

# NOTE 10 - osm download
# downloading OSM takes a few minutes as the file is 126.5 MB

# Download osm data for Hamilton
oe_get("Hamilton", 
       provider = "openstreetmap_fr",
       download_directory = here::here("otp",
                                       "graphs",
                                       "hamilton"),
       boundary = cd21_ham,
       boundary_type = c("spat", "clipsrc"),
       download_only = TRUE, 
       skip_vectortranslate = TRUE)

# Remove .pbf and .gpkg files in tempdir since they may interact with other queries
file.remove(list.files(path = tempdir(), pattern = "(pbf|gpkg)", full.names = TRUE))

# 3 - configure the router to reflect our study parameters (ie. population ages 65+)

# Make a config object
router_config <- otp_make_config("router")     

# Check default walking speed
router_config$routingDefaults$walkSpeed 

# NOTE 11 - walking speed
# since we can assume that average walking speed decreases with age, let's check average walking speed by age
# we will calculate average senior walk speed using data from McMaster University
# see -> https://www.mcmasteroptimalaging.org/e-learning/walking-speed-is-it-a-vital-sign

# create dataframe of McMaster University's findings
walk_speed <- data.frame("age" = c("60-69", "70-79", "80-89"),
                         "srmale" = c(1.34, 1.26, 0.97),
                         "srfemale" = c(1.24, 1.13, 0.94))
walk_speed

# calculate average walk speed
srmale_avg <- (sum(walk_speed$srmale)/3)
srfemale_avg <- (sum(walk_speed$srfemale)/3)
sravg <- ((srmale_avg + srfemale_avg)/2)
sravg

# calibrate the walking speed of otp router to the senior average
router_config$routingDefaults$walkSpeed <- 1.15 

# confirm change
router_config$routingDefaults$walkSpeed

# confirm that the new config file is valid
otp_validate_config(router_config)            

# Save the config file
otp_write_config(router_config,
                 dir = path_data,
                 router = "hamilton")

# 4 - we do not need to create a build-config for this analysis

# 5 -  download the gtfs data
# Search for gtfs --> https://open.hamilton.ca/documents/6eeccf172c824c2db0484aea54ed7fe4/about
# download gtfs feed to appropriate folder
url <- "https://googlehsrdocs.hamilton.ca/"  
destfile = here::here("otp",
                      "graphs",
                      "hamilton",
                      "ham_gtfs.zip")
download.file(url, destfile, mode="wb")

# 6 - we do not need elevation data for this analysis

# We are now ready to test our OTP router!

# free unused memory
fm

# PART 3b - Test OpenTripPlanner ------------------------------------------------------

# set tmap mode to view
tmap_mode("view")

# NOTE 12 - creating otp graphs
# to increase speed, memory settings were increased to 10240 in this section
# however, memory settings have been reset to default value in the script below
# as a result, this script will take 30-45 minutes to run

# NOTE 13 - querying otp for isochrones:
# to increase speed, ncore settings were increased to 4 cores in this section
# however, ncores settings have been reset to default value (1) in the script below
# as a result, this script will take 30-45 minutes to run

# NOTE 14 - ncroes
# never set ncores to more ncores than avaialble on your computer
# to check your ncores, hold ctrl + shift + esc to bring up task manager
# click performance tab and number of cores should be indicated in the bottom right 

# create the default OTP graph file
# optional memory bump up - see NOTE 1
log1 <- otp_build_graph(otp = path_otp, 
                        dir = path_data, 
                        memory = 2048)        # 2048 is default, I ran at 10240

# launch otp and load the default graph (the server)
log2 <- otp_setup(otp = path_otp, 
                  dir = path_data)

# Connect to OTP from R
otpcon <- otp_connect(timezone = "Europe/London")

# web interface will auto-load when ready at http://localhost:8080

# test by getting a route
def_test <- otp_plan(otpcon, 
                     fromPlace = c(-1.17502, 50.64590), 
                     toPlace = c(-1.15339, 50.72266))

# view the route
qtm(def_test)

# stop connection to default otp router
otp_stop(warn=FALSE)

# create a hamilton OTP graph file
# optional memory bump up - see NOTE 1
log1 <- otp_build_graph(otp = path_otp, 
                        dir = path_data, 
                        memory = 2048,        # 2048 is default, I ran at 10240
                        router = "hamilton")

# launch otp and load the hamilton graph (the server)
log2 <- otp_setup(otp = path_otp, 
                  dir = path_data,
                  router = "hamilton")

# Connect to OTP from R
otpcon <- otp_connect(router = "hamilton",
                      timezone = Sys.timezone())

# web interface will auto-load when ready at http://localhost:8080

# test by getting a route -> McMaster University to Tim Hortons Field
ham_test1 <- otp_plan(otpcon, 
                      fromPlace = c(-79.917054, 43.258032), 
                      toPlace = c(-79.830855, 43.251014))

# view the route
qtm(ham_test1)

# test by getting an isochrone for McMaster University
# optional ncores bump up - see NOTE 2
ham_test2  <- otp_isochrone(otpcon = otpcon,
                            fromPlace = c(-79.917151, 43.258050),
                            mode = c("WALK", "TRANSIT"),
                            date_time = as.POSIXct(strptime("2022-08-03 09:00", "%Y-%m-%d %H:%M")),
                            maxWalkDistance = 1000,
                            cutoffSec = (c(5, 10, 15) * 60),
                            ncores = 1)    # 1 is default, I ran at 4

ham_test2$minutes = (ham_test2$time / 60)

testmap <- tm_shape(ham_test2) +
  tm_fill("minutes",
          breaks = c(0, 5.01, 10.01, 15.01),
          style = "fixed",
          palette = "-RdYlBu") +
  tm_borders()
testmap

# it works! 
# now that the graph is built, we will stop connection to hamilton otp router here
# this way, this section of the script can be skipped next time so we don't re-build otp every time
otp_stop(warn=FALSE)

# free unused memory
fm

# PART 3c - Connect to OpenTripPlanner  --------------------------------------------------------------

# launch otp and load the hamilton graph
log2 <- otp_setup(otp = path_otp, 
                  dir = path_data,
                  router = "hamilton")

# Connect to OTP from R
otpcon <- otp_connect(router = "hamilton")

# web interface will auto-load when ready at http://localhost:8080

# free unused memory
fm

# PART 3d - Query OpenTripPlanner for Isochrones -------------------------------------------------------------

# load clean data
da21_ham_gc <- sf::st_read(here::here("data/clean/da21_ham_gc.gpkg"),
                           stringsAsFactors = FALSE)

# take a peek
head(da21_ham_gc)

# get isochrone polygons for each DA centroid via loop

# variable with number of DAs
nrows <- nrow(da21_ham_gc)

# empty list to store output
da21_ham_gc_iso <- list()

# get isochrone polygon for each DA centroid and store in da21_ham_reach list
# cutoffsec is the max time in seconds for the isochrone (15 minutes for this analysis)
# with 891 DAs, this function takes about 20 minutes to run using ncores = 4
for (i in 1:nrows){
  da21_ham_gc_iso[[i]] <- otp_isochrone(otpcon = otpcon,
                                        fromPlace = c(da21_ham_gc$lon[i], da21_ham_gc$lat[i]),
                                        fromID = c(da21_ham_gc$DAUID[i]),
                                        mode = c("WALK", "TRANSIT"),
                                        date_time = as.POSIXct(strptime("2022-08-03 09:00", "%Y-%m-%d %H:%M")),
                                        maxWalkDistance = 1000,
                                        cutoffSec = (15 * 60),
                                        ncores = 1)}    # 1 is default, I ran at 4

# check
head(da21_ham_gc_iso, 1)

# test plot
tm_shape(da21_ham_gc_iso[[333]]) +
  tm_fill(col = "#FFC20A") +
  tm_borders()

# OTP was not able to query some of our centroids
# these would show as NA values - lets check
sum(is.na(da21_ham_gc_iso))

# 32 centroids of our 891 have zero values. We will remove these for now.
da21_ham_gc_iso <- da21_ham_gc_iso[da21_ham_gc_iso != "NA"]

# convert result to a single simple feature
da21_ham_gc_iso = do.call(rbind, da21_ham_gc_iso)

# select desired columns 
da21_ham_gc_iso <- da21_ham_gc_iso %>%
  dplyr::select(c(3, 4))

# rename columns to facilitate merge later on
da21_ham_gc_iso <- da21_ham_gc_iso %>%
  dplyr::rename("DAUID"= 1)

# check
head(da21_ham_gc_iso, 3)

# test plot all isochrones
qtm(da21_ham_gc_iso)

# test plot a single isochrone
qtm(da21_ham_gc_iso[da21_ham_gc_iso$DAUID == "35250468", ])

# save isochrones
st_write(obj = da21_ham_gc_iso,
         here::here("data/clean/da21_ham_gc_iso.gpkg"),
         delete_layer = TRUE)

# otp no longer needed - stop connection to otp router
otp_stop(warn=FALSE)

# free unused memory
fm

# PART 4 - Calculate Accessibility Scores -------------------------------------------------------------------

# We will calculate accessibility scores with the following equations 
# 1 - DA accessibility scores = number cspaces reached by DA / total number of cspaces in Hamilton
# 2 - Weighted DA accessibility scores = DA accessibility scores * DA senior population
# 2 - Hamilton accessibility score = Weighted DA accessibility scores / total senior population in Hamilton

# STEP 1 - Determine how many community spaces each isochrone intersects with and add it as a column

# perform intersection of 15 minute isochrones and cspaces, then add point count as numeric to each polygon
da21_ham_gc_iso_int <- da21_ham_gc_iso %>%
  mutate(cspaces_15 = (as.numeric(lengths(st_intersects(da21_ham_gc_iso, cspaces)))))

# merge back into DA shape file to plot results, remove geometry first to avoid conflict
st_geometry(da21_ham_gc_iso_int) <- NULL

# Merge with (DA) shape file on DAUID
da21_ham <- left_join(x = da21_ham,
                      y = da21_ham_gc_iso_int,
                      by.x = "DGUID",
                      by.y = "DGUID")

# confirm
da21_ham

# STEP 2 - Calculate DA Accessibility Scores

# calcualte score (% of the total community spaces in Hamilton that are reachable from each DA) as new column
da21_ham <- da21_ham %>%
  mutate(DA_SSOS = ((da21_ham$cspaces_15 / cspacestotal) * 100))

# confirm
da21_ham

# good, however, recall that we dropped all cases where OTP could not route from a DA centroid
# let's have a look
sum(is.na(da21_ham))

# 64 NA values
# let's check how many distinct values are in the DAUID column - this is the only column that can't be NA or 0 
# and therefore must have 891 unique values
n_distinct(da21_ham$DAUID)

# 891 values. Perfect, we have the correct number of DA's
# now let's perform a master replace "NA" with "0" for all other columns
# key assumption: 
# if OTP can not route from the location, then there are no cspaces reachable from that location
# see paper for discussion on assumptions with centroid analyses
da21_ham[is.na(da21_ham)] <- 0

# confirm
sum(is.na(da21_ham))

# no more NA values

# lets plot where NA values are for awareness
# first find the zeros
zeros <- da21_ham %>%
  filter(cspaces_15 == 0)

# now plot them
ggplot() +
  geom_sf(data = cd21_ham, color = 'black', fill = '#ABEBC6', size = 0.05) +
  geom_sf(data = zeros, fill = '#EAECEE') +
  blank()

# that is a lot of emptiness

# Check statistics

# group number of DA's by score
stats1 <- da21_ham %>%
  group_by(DA_SSOS) %>%
  summarise(DAUID = n()) %>%
  mutate(DApct = (DAUID / sum(DAUID))*100) %>%
  st_drop_geometry()

# group population by 2021 score
stats2 <- da21_ham %>%
  group_by(DA_SSOS) %>%
  summarise(sum_srpop21 = sum(srpop21)) %>%
  mutate(srpop21pct = ((sum_srpop21 / srpop21total)*100)) %>%
  st_drop_geometry()

# group population by 2026 score
stats3 <- da21_ham %>%
  group_by(DA_SSOS) %>%
  summarise(sum_srpop26 = sum(srpop26)) %>%
  mutate(srpop26pct = ((sum_srpop26 / srpop26total)*100)) %>%
  st_drop_geometry()

# group population by 2031 score  
stats4 <- da21_ham %>%
  group_by(DA_SSOS) %>%
  summarise(sum_srpop31 = sum(srpop31)) %>%
  mutate(srpop31pct = ((sum_srpop31 / srpop31total)*100)) %>%
  st_drop_geometry()

# merge
DA_SSoS <- merge(x = stats1,
                 y = stats2,
                 by.x = "DA_SSOS",
                 by.y = "DA_SSOS")
DA_SSoS <- merge(x = DA_SSoS,
                 y = stats3,
                 by.x = "DA_SSOS",
                 by.y = "DA_SSOS")
DA_SSoS <- merge(x = DA_SSoS,
                 y = stats4,
                 by.x = "DA_SSOS",
                 by.y = "DA_SSOS")

# have a look
DA_SSoS

# STEP 3 - Calculate Weighted Accessibility Scores for Hamilton

# use mutate to calculate for all three time periods
# by multiplying (weighing) each DA SSOS by its senior population
# then dividing the result by total senior population in Hamilton
da21_ham <- da21_ham %>%
  mutate(DA_SSOSw = (((DA_SSOS * srpop21)/srpop21total)*100),
         DA_SSOSw_5y = (((DA_SSOS * srpop26)/srpop26total)*100),
         DA_SSOSw_10y = (((DA_SSOS * srpop31)/srpop31total)*100))

# use mutate to calculate difference between Weighted SSOS now and 5y /10y
da21_ham <- da21_ham %>%
  mutate(DA_SSOSw_5y_chg = (DA_SSOSw_5y - DA_SSOSw),
         DA_SSOSw_10y_chg = (DA_SSOSw_10y - DA_SSOSw))

# check dataframe
da21_ham

# save results
st_write(obj = da21_ham,
         here::here("data/results/da21_ham.gpkg"),
         delete_layer = TRUE)

# STEP 4 - Calculate Average Scores
# use summarize
ham_scores_final <- da21_ham %>%
  st_drop_geometry() %>%
  summarise(DA_SSOS_avg = mean(DA_SSOS),
            DA_SSOSw_avg = mean(DA_SSOSw),
            DA_SSOSw_5y_avg = mean(DA_SSOSw_5y),
            DA_SSOSw_10y_avg = mean(DA_SSOSw_10y)) 

# Use mutate to calculate difference between Weighted SSOS now and 5y /10y
ham_scores_final <- ham_scores_final %>%
  mutate(DA_SSOSw_5y_chg = (DA_SSOSw_5y_avg - DA_SSOSw_avg),
         DA_SSOSw_10y_chg = (DA_SSOSw_10y_avg - DA_SSOSw_avg))

# check dataframe
ham_scores_final

# save summary results
write.csv(ham_scores_final, 
          here::here("data/results/ham_scores_final.csv"),
          row.names = FALSE)

# free unused memory
fm

# PART 5 - Plot Results -----------------------------------------------------------

# load data
cd21_ham <- sf::st_read(here::here("data/clean/cd21_ham.gpkg"),
                        stringsAsFactors = FALSE)

da21_ham <- sf::st_read(here::here("data/results/da21_ham.gpkg"),
                        stringsAsFactors = FALSE)

da21_ham_gc <- sf::st_read(here::here("data/clean/da21_ham_gc.gpkg"),
                           stringsAsFactors = FALSE)

da21_ham_gc_iso <- sf::st_read(here::here("data/clean/da21_ham_gc_iso.gpkg"),
                               stringsAsFactors = FALSE)

cspaces <- sf::st_read(here::here("data/clean/cspaces.gpkg"),
                       stringsAsFactors = FALSE)

# subset census divisions for greater golden horseshoe
cd21_ggh <- cd21 %>%
  filter(str_detect(CDNAME, "Barrie|Brant|Dufferin|Durham|Haldimand|Halton|Hamilton|Kawartha|Niagara|Northumberland|Orillia|Peel|Peterborough|Simcoe|Toronto|Waterloo|Wellington|York"))

# load additional data for study area plot, convert CRS, and clip to hamilton CD
parks <- sf::st_read(here::here("data/raw/Parks.shp"),
                     stringsAsFactors = FALSE)
parks <- parks %>% st_transform(common_crs)
parks <- sf::st_intersection(parks, cd21_ham)

water <- sf::st_read(here::here("data/raw/Waterbodies.shp"),
                     stringsAsFactors = FALSE)
water <- water %>% st_transform(common_crs)
water <- sf::st_intersection(water, cd21_ham)

esa <- sf::st_read(here::here("data/raw/Environmentally_Sensitive_Areas_Boundaries.shp"),
                   stringsAsFactors = FALSE)
esa <- esa %>% st_transform(common_crs)
esa <- sf::st_intersection(esa, cd21_ham)

urban_area <- sf::st_read(here::here("data/raw/Urban_Boundary.shp"),
                          stringsAsFactors = FALSE)
urban_area <- urban_area %>% st_transform(common_crs)
urban_area <- sf::st_intersection(urban_area, cd21_ham)

rural_settlement <- sf::st_read(here::here("data/raw/Rural_Settlement_Areas.shp"),
                                stringsAsFactors = FALSE)
rural_settlement <- rural_settlement %>% st_transform(common_crs)
rural_settlement <- sf::st_intersection(rural_settlement, cd21_ham)

rural_area <- sf::st_read(here::here("data/raw/Rural_Boundary.shp"),
                          stringsAsFactors = FALSE)
rural_area <- rural_area %>% st_transform(common_crs)
rural_area <- sf::st_intersection(rural_area, cd21_ham)

# set tmap mode to plot
tmap_mode("plot")

# - - - - - - - - - - - - 

# Plot greater golden horseshoe
ggh <- ggplot()+
  geom_sf(data=cd21_ggh, fill='#EAECEE',size=0.1) +
  geom_sf(data=cd21_ham, col='red', fill='#998781',lwd = 0.5) +
  blank()

# Plot study area
study_area <- ggplot()+
  geom_sf(data=rural_area, col=NA, fill='#d6e7cd') +
  geom_sf(data=rural_settlement, col=NA, fill='#d6c362') +
  geom_sf(data=urban_area, col=NA, fill='#e7d1a7') +
  geom_sf(data=esa, col=NA, fill='#afd2ae') +
  geom_sf(data=water, col=NA, fill='#a8cfec') +
  geom_sf(data=parks, col=NA, fill='#729b6f') +
  geom_sf(data=cd21_ham, col='black', fill=NA, lwd = 0.1) +
  blank() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl",height = unit(0.1, "cm"))

# plot together
studyarea_ggh <- ggdraw() +
  draw_plot(study_area) +
  draw_plot(ggh, x = 0.62, y = 0.62, width = 0.35, height = 0.35)
studyarea_ggh

# save plot
ggsave(filename = "studyarea_ggh.png",
       path = here::here("data/results/"),
       width = 5,
       height = 5,
       dpi = 300,
       units = "in",
       device = "png")

# - - - - - - - - - - - - 

# Plot study area and methodology

# A - DAs - Plot Zones
DAplot <- ggplot() + 
  ggtitle("1) Load Dissemination Areas") +
  theme(plot.title = element_text(size = 10, hjust = 0, vjust = -1)) +
  geom_sf(data = da21_ham, color = 'grey', fill = NA, lwd = 0.3) +
  geom_sf(data = cd21_ham, color = 'black', fill = NA, lwd = 0.2) +
  blank()

# B - DA Centroids - Calculate Centroids
DAGCplot <- ggplot() + 
  ggtitle("2) Calculate Centroids") +
  theme(plot.title = element_text(size = 10, hjust = 0, vjust = -1)) +
  geom_sf(data = da21_ham_gc, color = '#F8C471') +
  geom_sf(data = da21_ham, color = 'grey', fill = NA, lwd = 0.3) +
  geom_sf(data = cd21_ham, color = 'black', fill = NA, lwd = 0.2) +
  blank()

# C - GC Isochrones - Calculate 15min Isochrones from Centroids
DAGCISOplot <- ggplot() + 
  ggtitle("3) Query OTP for 15min Isochrones") +
  theme(plot.title = element_text(size = 10, hjust = 0, vjust = -1)) +
  geom_sf(data = da21_ham_gc_iso, fill = '#F8C471', color = '#F8C471') +
  #geom_sf(data = da21_ham, color = 'grey', fill = NA, lwd = 0.3) +
  geom_sf(data = cd21_ham, color = 'black', fill = NA, lwd = 0.2) +
  blank()+
  annotation_scale(location = "bl",height = unit(0.1, "cm")) 

# D - Community spaces - Intersect Isochrones by Community Spaces
CSPACESplot <- ggplot() + 
  ggtitle("4) Intersect Community Spaces") +
  theme(plot.title = element_text(size = 10, hjust = 0, vjust = -1)) +
  geom_sf(data = da21_ham_gc_iso, fill = '#F8C471', color = '#F8C471') +
  #geom_sf(data = da21_ham, color = 'grey', fill = NA, lwd = 0.3) +
  geom_sf(data = cd21_ham, color = 'black', fill = NA, lwd = 0.2) +
  geom_sf(data = cspaces, color = '#A04000', shape=18, size=1.2) +
  blank()+
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(0.8, "cm"),
                         width = unit(0.8, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)

# plot together
studyarea_method <- plot_grid(ncol = 2,
                              align = "hv",
                              DAplot, DAGCplot, DAGCISOplot, CSPACESplot,
                              label_size = 9) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1))
studyarea_method

# save plot
ggsave(filename = "studyarea_method.png",
       device = "png",
       path = here::here("data/results/"),
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

# - - - - - - - - - - - - 

# Plot DA SSOS >1

DA_SSOS_1plus <- ggplot() +
  geom_sf(data = cd21_ham, color = NA, fill = '#F8C471', size = 0.05) +
  geom_sf(data = zeros, col = 'black', fill = '#CDD1D4', lwd = 0.001) +
  blank() +
  annotation_north_arrow(location = "br", which_north = "true",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = "bl",height = unit(0.1, "cm"))
DA_SSOS_1plus

# save plot
ggsave(filename = "DA_SSOS_1plus.png",
       device = "png",
       path = here::here("data/results/"),
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

# - - - - - - - - - - - -  

# Plot Cspaces reach
cspaces_reach <- tm_shape(da21_ham) +
  tm_fill("cspaces_15",
          style = "jenks",
          legend.hist = TRUE,
          title = "Community Spaces within 15 min.\n(Transit, max. 1000m walk, AM Peak)") +
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.outside.position = 'bottom',
            legend.stack = 'horizontal',
            legend.title.fontface = 'bold',
            legend.hist.width = 1,
            legend.hist.height = 0.6) +
  tm_borders(col = "grey40", lwd = 0.1) +
  tm_legend(title.size=0.9,
            text.size = 0.6,
            position = c("left", "bottom")) +
  tm_shape(cd21_ham, color = 'black', fill = NA) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_compass(north = 0, type = 'arrow', show.labels =0, position = c('right','bottom')) + 
  tm_scale_bar(color.dark = "black",
               position = c("left", "bottom"))
cspaces_reach

# save plot
tmap_save(tm = cspaces_reach,
          filename = here::here("data/results/cspaces_reach.png"),
          width = 5,
          height = 6,
          units = "in",
          dpi = 300)

# - - - - - - - - - - - -  

# Plot SSOS by Dissemination Area, 2021

# plot
DA_SSOS_plot <- tm_shape(da21_ham) +
  tm_polygons("DA_SSOS", 
              style="jenks", 
              title="SSOS",
              border.alpha = 0) +
  tm_shape(cd21_ham, color = 'black', fill = NA) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(frame = FALSE) +
  tm_legend(position = c("right", "top")) +
  tm_compass(position = c('right','bottom')) + 
  tm_scale_bar(position = c("left", "bottom"), width = 0.15)
DA_SSOS_plot

# save plot
tmap_save(tm = DA_SSOS_plot,
          filename = here::here("data/results/DA_SSOS.png"),
          height = 3.5,
          width = 3.5,
          units = "in",
          dpi = 300)

# - - - - - - - - - - - -  

# Plot Weighted SSOS by Dissemination Area, Current (2021)
DA_SSOSw_plot <- tm_shape(da21_ham) +
  tm_polygons("DA_SSOSw", 
              style="jenks", 
              title="Weighted SSOS",
              border.alpha = 0) +
  tm_shape(cd21_ham, color = 'black', fill = NA) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(frame = FALSE) +
  tm_legend(position = c("right", "top")) +
  tm_compass(position = c('right','bottom')) + 
  tm_scale_bar(position = c("left", "bottom"), width = 0.15)
DA_SSOSw_plot

# save plot
tmap_save(tm = DA_SSOSw_plot,
          filename = here::here("data/results/DA_SSOSw.png"),
          height = 3.5,
          width = 3.5,
          units = "in",
          dpi = 300)

# - - - - - - - - - - - -   

# Plot 5-Year Change in Weighted DA SSOS
DA_SSOSw_5y_chg_plot <- tm_shape(da21_ham) +
  tm_polygons("DA_SSOSw_5y_chg", 
              style="jenks", 
              title="Five-Year\nChange (2026)",
              border.alpha = 0) +
  tm_shape(cd21_ham, color = 'black', fill = NA) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(frame = FALSE) +
  tm_legend(position = c("right", "top")) +
  tm_compass(position = c('right','bottom')) + 
  tm_scale_bar(position = c("left", "bottom"), width = 0.15)
DA_SSOSw_5y_chg_plot

# save plot
tmap_save(tm = DA_SSOSw_5y_chg_plot,
          filename = here::here("data/results/DA_SSOSw_5y_chg.png"),
          height = 3.5,
          width = 3.5,
          units = "in",
          dpi = 300)

# - - - - - - - - - - - - 

# Plot 10-Year change in Weighted DA SSOS
DA_SSOSw_10y_chg_plot <- tm_shape(da21_ham) +
  tm_polygons("DA_SSOSw_10y_chg", 
              style="jenks", 
              title="Ten-Year\nChange (2031)",
              border.alpha = 0) +
  tm_shape(cd21_ham, color = 'black', fill = NA) +
  tm_borders(col = "black", lwd = 0.1) +
  tm_layout(frame = FALSE) +
  tm_legend(position = c("right", "top")) +
  tm_compass(position = c('right','bottom')) + 
  tm_scale_bar(position = c("left", "bottom"), width = 0.15)
DA_SSOSw_10y_chg_plot

# save plot
tmap_save(tm = DA_SSOSw_10y_chg_plot,
          filename = here::here("data/results/DA_SSOSw_10y_chg.png"),
          height = 3.5,
          width = 3.5,
          units = "in",
          dpi = 300)

# free unused memory
fm