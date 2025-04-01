### yaps ###
install.packages("devtools") 
install.packages("TMB") 
TMB::runExample(all=TRUE)
devtools::install_github("baktoft/yaps")
devtools::install_github("spkaluzny/splusTimeSeries")
library(dplyr)
library(yaps)
library(lubridate)
library(leaflet)
library(data.table)
library(Matrix)
library(TMB)
library(splusTimeSeries)
library(sp)

`801231`$ts <- as.POSIXct(`801231`$ts, format = "%Y-%m-%d %H:%M:%OS")
`801233`$ts <- as.POSIXct(`801233`$ts, format = "%Y-%m-%d %H:%M:%OS")
`801236`$ts <- as.POSIXct(`801236`$ts, format = "%Y-%m-%d %H:%M:%OS")

# yyyy-mm-dd hh:mm:ss.000

`801231`$serial <- 801231
`801233`$serial <- 801233
`801236`$serial <- 801236

detections <- full_join(`801231`, `801233`, by = c("ts", "tag", "serial")) %>%
  full_join(`801236`, by = c("ts", "tag", "serial"))

detections$ts <- as.POSIXct(detections$ts, format = "%Y-%m-%d %H:%M:%OS")

# Extract the sub-second part as a fraction of a second
detections$frac <- as.numeric(format(detections$ts, "%OS3")) %% 1  # %OS3 for milliseconds
detections$epo <- as.integer(detections$ts)
detections$serial <- as.integer(detections$serial)

hydros$x <- as.integer(hydros$x)
hydros$y <- as.integer(hydros$y)
hydros$z <- as.numeric(hydros$z)
hydros$sync_tag <- as.numeric(hydros$sync_tag)

hydros <- as.data.table(hydros)
detections <- as.data.table(detections)

data_list <- list(
  hydros = hydros,
  detections = detections
)

plot(y~x, data = data_list$hydros, asp=1)
points(y~x, data=data_list$hydros[!is.na(sync_tag)], col="red", pch=20)

head(data_list$detections)

hydros <- data_list$hydros
coordinates(hydros) <- ~x+y
proj4string(hydros) <- CRS("+init=epsg:32615")
hydros_latlon <- spTransform(hydros, CRS("+init=epsg:4326"))
m <- leaflet(data=hydros_latlon, options = leafletOptions(minZoom = 0, maxZoom = 18), width="100%", height=700)
m <- addTiles(m, group="OSM")
m <- addCircles(m, radius=5, label=as.character(hydros_latlon$idx), labelOptions = labelOptions(noHide = T, textOnly = TRUE))
m <- addMeasure(m, primaryLengthUnit="meters")
m <- addProviderTiles(m, providers$Esri.WorldImagery, group="Esri.WorldImagery")
m <- addLayersControl(m, baseGroups = c("OSM (default)", "Esri.WorldImagery"),    options = layersControlOptions(collapsed = FALSE)  )
m

# Step 2: Set synchronization parameters

# set sync parameters

max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 1
fixed_hydros_idx <- c(1:3)
n_offset_day <- 2
n_ss_day <- 2

inp_sync <- getInpSync(sync_dat=data_list, max_epo_diff, min_hydros, time_keeper_idx, 
                       fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=0.25)

###Possible issues: 
    # WARNING: getSyncModel() will estimate speed of sound. It is strongly advised to use data instead!
    #WARNING: At least one hydro x offset_idx combination has less than 5 observations. This/these hydro(s) might not be synced in that/these period(s)!
    #Run getSyncCoverage(inp_sync, plot=TRUE) for more info
    #WARNING: At least one hydro has less than 10 pings in an offset_idx - try getSyncCoverage(inp_sync, plot=TRUE) for visual
    #and rerun getInpSync() with increased keep_rate

# Step 4: Run synchronization model
lower_bounds <- rep(-100, length(inp_sync$inits))
upper_bounds <- rep(100, length(inp_sync$inits))

sync_model <- getSyncModel(inp_sync, silent = FALSE, max_iter = 1000)

sync_model_finetune <- fineTuneSyncModel(sync_model, eps_threshold= 100, silent = FALSE)

# Step 5: Quality check synchronization model
plotSyncModelResids(sync_model_finetune, by='overall')    
plotSyncModelResids(sync_model_finetune, by='quantiles')
plotSyncModelResids(sync_model_finetune, by='sync_tag')      
plotSyncModelResids(sync_model_finetune, by='hydro')

# Step 6: Apply synchronization to full dataset
detections_synced <- applySync(toa=data_list$detections, hydros=data_list$hydros, sync_model)
detections_synced_finetune <- applySync(toa=data_list$detections, hydros=data_list$hydros, sync_model_finetune)
# run yaps
hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')

focal_tag <- 24612
rbi_min <- 80
rbi_max <- 160

synced_dat_tag_finetune <- detections_synced_finetune[tag == focal_tag]

toa_data <- getToaYaps(synced_dat_tag_finetune, hydros_yaps, rbi_min, rbi_max)

inp_data <- getInp(hydros_yaps, toa_data, E_dist="Mixture", n_ss=2, pingType="rbi", 
                   sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est")

### doesnt work yet, probably not enough receivers
### classic error when YAPS isn't really able to calculate positions
  ### Error in newton(par = c(X = 59.0002737320408, X = -187.377606427637, X = -9.92795537897286,  : Newton failed to find minimum.

yaps_out_data <- runYaps(inp_data, silent=FALSE)

plotYaps(yaps_out=yaps_out_data, type="map")

par(mfrow=c(2,1))
plotYaps(yaps_out=yaps_out_data, type="coord_X")
plotYaps(yaps_out=yaps_out_data, type="coord_Y")

### practice dataset

load("C:/Users/jcmoore/Downloads/ssu1.rda")

install.packages('devtools')
install.packages("TMB", type = "source")
devtools::install_github('baktoft/yaps')


#Load libaries needed for this tutorial.

library(data.table)
library(dplyr)
library(sp)
library(leaflet)


#Load `yaps`and test that everything is working as expected. This should display a short simulated known track (in black) and the track estimated by `yaps` in red.

library(yaps)
testYaps(silent=FALSE)


## Getting started - the `ssu1` example data set

##This is a tiny data set collected as part of a feasibility study using YAPS on Vemco PPM style data to track fish in shallow parts of Florida Bay, USA. Data collected by J.S. Rehage, J.R. Rodemann, R.S. Corujo and N. Viadero. Included in `yaps` with permission from [J.S. Rehage](https://case.fiu.edu/about/directory/profiles/rehage-jennifer-schopf.html), FIU Florida International University.

##Have a look at the data - details can be found in `?ssu1`

names(ssu1)
head(ssu1$hydros)

# Pretty self explanatory. Coordinates are in UTM - YAPS will (most probably) not work well with lat/lon data. Column `sync_tag` indicate serial number of special transmitters co-located with the hydrophones; data from these are used in the synchronization process. Column `idx` is an index running from `1:nrow(hydros)`.


plot(y~x, data = ssu1$hydros, asp=1)
points(y~x, data=ssu1$hydros[!is.na(sync_tag)], col="red", pch=20)

head(ssu1$detections)

# Almost self explanatory. Each row is a detection of a transmitter (`tag`) on a hydrophone identified by `serial`. Column `ts` is the (non-synced) timestamp of the detection in timezone UTC. Column `epo` is `ts` converted to UNIX epoch using `as.numeric(ts)` and `frac` is fractions of second for the detection, i.e. the complete time of detection is given by `epofrac = epo + frac`.


head(ssu1$gps)


#Quick map to see where we are in the world (check out [`leaflet`](https://cran.r-project.org/web/packages/leaflet/index.html) - awesome for making quick slippy-maps)

hydros <- ssu1$hydros
coordinates(hydros) <- ~x+y
proj4string(hydros) <- CRS("+init=epsg:32617")
hydros_latlon <- spTransform(hydros, CRS("+init=epsg:4326"))
m <- leaflet(data=hydros_latlon, options = leafletOptions(minZoom = 0, maxZoom = 18), width="100%", height=700)
m <- addTiles(m, group="OSM")
m <- addCircles(m, radius=5, label=as.character(hydros_latlon$idx), labelOptions = labelOptions(noHide = T, textOnly = TRUE))
m <- addMeasure(m, primaryLengthUnit="meters")
m <- addProviderTiles(m, providers$Esri.WorldImagery, group="Esri.WorldImagery")
m <- addLayersControl(m, baseGroups = c("OSM (default)", "Esri.WorldImagery"),    options = layersControlOptions(collapsed = FALSE)  )
m


## Synchronizing the array
# The code below is identical to that presented in our pre-print [Opening the black box of high resolution fish tracking using yaps](https://www.researchgate.net/publication/338010182_Opening_the_black_box_of_high_resolution_fish_tracking_using_yaps), which also include detailed description of the parameters in `getInpSync()`.

#First, set the parameters to be used in the sync model and get input data prepared for use with `getSyncModel()`.

# set sync parameters 
max_epo_diff <- 120
min_hydros <- 2
time_keeper_idx <- 5
fixed_hydros_idx <- c(2:3, 6, 8, 11, 13:17)
n_offset_day <- 2
n_ss_day <- 2

inp_sync <- getInpSync(sync_dat=ssu1, max_epo_diff, min_hydros, time_keeper_idx, 
                       fixed_hydros_idx, n_offset_day, n_ss_day, keep_rate=0.5)

#Then, obtain a synchronization model using `getSyncModel()`

sync_model <- getSyncModel(inp_sync, silent=FALSE)

# Use the diagnostic plots to ensure the obtained synchronization model is good. Basically, we want all number to be as close to zero as possible. Note, that a few outliers far away from zero is not serious, as the synchronization model follows a scaled t-distribution allowing long tails in the residuals.

plotSyncModelResids(sync_model, by='overall')    
plotSyncModelResids(sync_model, by='quantiles')
plotSyncModelResids(sync_model, by='sync_tag')      
plotSyncModelResids(sync_model, by='hydro')         



#Next we apply the obtained synchronization model to the tracking data using `applySync()`

detections_synced <- applySync(toa=ssu1$detections, hydros=ssu1$hydros, sync_model)


## Running `yaps` to estimate the track
## Now that we have the receivers synchronized, we can estimate the track using `yaps`. First, we need a data.table containing positions of the receivers in three dimension with colnum names 'hx', 'hy' and 'hz'.

hydros_yaps <- data.table::data.table(sync_model$pl$TRUE_H)
colnames(hydros_yaps) <- c('hx','hy','hz')

## We specify tag specific minimum and maximum burst intervals and extract the detetections pertaining to the focal tag from the synchronized data

focal_tag <- 15266
rbi_min <- 20
rbi_max <- 40

synced_dat_ssu1 <- detections_synced[tag == focal_tag]


## The main input data to `yaps` is a Time-of-Arrival matrix of the detections. This can be obtained using `getToaYaps()`
toa_ssu1 <- getToaYaps(synced_dat_ssu1, hydros_yaps, rbi_min, rbi_max)


## Next, we use the function `getInp()` to compile input data for `runYaps()`.

inp_ssu1 <- getInp(hydros_yaps, toa_ssu1, E_dist="Mixture", n_ss=2, pingType="rbi", 
                   sdInits=1, rbi_min=rbi_min, rbi_max=rbi_max, ss_data_what="est", ss_data=0)


## Finally, we are ready to run `yaps` to obtain the track

yaps_out_ssu1 <- runYaps(inp_ssu1, silent=FALSE)


## Basic plotting of estimated track
## To do some very basic visualization of the obtained track, we can use `plotYaps()` as below.

plotYaps(yaps_out=yaps_out_ssu1, type="map")
lines(utm_y~utm_x, data=ssu1$gps, lty=2)

par(mfrow=c(2,1))
plotYaps(yaps_out=yaps_out_ssu1, type="coord_X")
lines(utm_x~ts, data=ssu1$gps, lty=2)
plotYaps(yaps_out=yaps_out_ssu1, type="coord_Y")
lines(utm_y~ts, data=ssu1$gps, lty=2)

