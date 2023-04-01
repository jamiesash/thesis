
source("functions.R")
source("libraries.R")
source("searchcomcat.R")
library(imputeTS)

quak <- read.csv("../data/quakes_1997_2023.csv")
quak$time <- substr(quak$time, start = 1, stop = 10)
quak$time <- as.Date(quak$time)

gry <- colvect("grey22", alpha = 0.3)

# STL Filter Example -----------------------------------------------------------
# Loading packages and all pre-writen functions
# setwd("C:\\Users\\james\\Desktop\\jamieslife\\analysis")
rasterOptions(maxmemory = 120e+10, memfrac = 0.9)

# using a two degree box around aloha
lons = c(-155, -144)
lats = c(22, 26)
e = extent(lons, lats)

# ------------------------------------------------------------------------------
# Set variables
lat_varid = "lat"
lon_varid = "lon"
sdate = as.Date("1997-01-01")
edate = as.Date("2022-12-31")
var = "CHL"
url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M?"
origin = "1900-01-01"

data = nc_open(url, verbose = FALSE, write = FALSE)
lat  = ncvar_get(data, varid = lat_varid)
lon  = ncvar_get(data, varid = lon_varid)
time = ncvar_get(data, varid = "time")
time = as.Date(time, origin = origin)

idx_lat <- which(lat > lats[1] & lat < lats[2])
idx_lon <- which(lon > lons[1] & lon < lons[2])
idx_time <- which(time >= sdate & time <= edate)


idx_ras <- paste("CHL",
                 paste("[", range(idx_time)[1], ":1:", range(idx_time)[2], "]", sep = ""),
                 paste("[", range(idx_lat)[1],  ":1:", range(idx_lat)[2],  "]", sep = ""),
                 paste("[", range(idx_lon)[1],  ":1:", range(idx_lon)[2],  "]", sep = ""),
                 sep = "")

idx_time <- paste("time", paste("[", range(idx_time)[1], ":1:",range(idx_time)[2], "]", sep = ""), sep = "")
idx_lat  <- paste(lat_varid, paste("[", range(idx_lat)[1], ":1:",range(idx_lat)[2],  "]", sep = ""), sep = "")
idx_lon  <- paste(lon_varid, paste("[", range(idx_lon)[1], ":1:",range(idx_lon)[2],  "]", sep = ""), sep = "")
idx <- paste(idx_lat, idx_lon, idx_time, idx_ras, sep = ",")

url <- paste(url, idx, sep = "")

nc_close(data)
rm(data)

data = nc_open(url, verbose = FALSE, write = FALSE)

lat  = ncvar_get(data, varid = lat_varid)
lon  = ncvar_get(data, varid = lon_varid)
time = ncvar_get(data, varid = "time")
time = as.Date(time, origin = origin)
ras  = ncvar_get(data)
nc_close(data)

s = dim(ras)
ras = raster::brick(ras)
ras = t(ras)
ras = setZ(ras, z = as.Date(time, origin = org), name = "time")
extent(ras) = extent(min(lons), max(lons), min(lats), max(lats))

chl = ras
rm(ras)
gc()

# ------------------------------------------------------------------------------

chl = bufcoast(chl, 
               region = "Hawaiian Islands", 
               path = "../data/USMaritimeLimitsAndBoundariesSHP")

chla = anomalize(chl, detrend = TRUE)
gc()

tc   <- cellStats(chl, stat="mean", na.rm = TRUE)
tc   <- as.vector(tc)
# tc[is.nan(tc)] <- NA
time <- getZ(chl)

tca   <- cellStats(chla, stat="mean", na.rm = TRUE)
tca   <- as.vector(tca)

u = mad(tca, na.rm = TRUE)
o = median(tca, na.rm = TRUE)

par(mar = c(5, 5, 5, 1))
plot(time, tca, 
     pch = 19, 
     cex = 0.75, 
     xlab = "",
     ylab = "",
     col = "grey22", 
     axes = FALSE,
     xlim = c(as.Date("1997-10-01"), as.Date("2023-01-01"))
     )
abline(v = as.Date("2018-04-25"), 
       col = "grey49", lwd = 1, lty = 2)
abline(h = u, 
       col="red", lwd = 1, lty = 1)
axis(side = 2,
     las = 2, 
     lwd = 2, 
     mgp = c(1, 0.75, 0), 
     cex.axis = 1.5)
axis(side = 1,
     labels = as.character(year(seq(min(time), max(time), length = 10))),
     at =  as.numeric(seq(min(time), max(time), length = 10)), 
     las = 1, 
     lwd = 2, 
     mgp = c(2, 1, 0), 
     cex.axis = 1.5)
title(main = list("satellite chlorophyll anomaly at Stn. ALOHA", cex = 1.5),
      line = -1.5,
      adj  = 0.05)
title(ylab    = "CHL [mg/m^3]", 
      cex.lab = 1.5,
      line    = 3.5)
title(xlab    = "Year", 
      cex.lab = 1.5,
      line    = 2.5)
box(which = "plot", lty = "solid", lwd = 3, col = "grey12")


par(mar = c(5, 5, 5, 1))
plot(quak$time, quak$mag, 
     pch = 19, 
     cex = 0.75, 
     xlab = "",
     ylab = "",
     col = gry, 
     axes = FALSE,
     xlim = c(as.Date("1997-10-01"), as.Date("2023-01-01"))
     )
abline(v = as.Date("2018-04-25"), 
       col = "grey49", lwd = 1, lty = 2)
axis(side = 1,
     labels = as.character(year(seq(min(time), max(time), length = 10))),
     at =  as.numeric(seq(min(time), max(time), length = 10)), 
     las = 1, 
     lwd = 2, 
     mgp = c(2, 1, 0), 
     cex.axis = 1.5)
axis(side = 2,
     las = 2, 
     lwd = 2, 
     mgp = c(1, 0.75, 0), 
     cex.axis = 1.5)
title(main = list("Earthquakes along the Hawaiian Islands", cex = 1.5),
      line = -1.5,
      adj  = 0.05)
title(ylab    = "Magnitude", 
      cex.lab = 1.5,
      line    = 2)
title(xlab    = "Year", 
      cex.lab = 1.5,
      line    = 2.5)
box(which = "plot", lty = "solid", lwd = 3, col = "grey12")



# ------------------------------------------------------------------------------
par(mar = c(5, 5, 5, 1))
plot(time, tca, 
     pch = 19, 
     cex = 0.75, 
     xlab = "",
     ylab = "",
     col = "grey22", 
     axes = FALSE,
     xlim = c(as.Date("1997-10-01"), as.Date("2023-01-01"))
)
abline(h = median(tca) + mad(tca), 
       col="red", lwd = 1, lty = 1)
axis(side = 2,
     las = 2, 
     lwd = 2, 
     mgp = c(1, 0.75, 0), 
     cex.axis = 1.5)
axis(side = 1,
     labels = as.character(year(seq(min(time), max(time), length = 10))),
     at =  as.numeric(seq(min(time), max(time), length = 10)), 
     las = 1, 
     lwd = 2, 
     mgp = c(2, 1, 0), 
     cex.axis = 1.5)
title(main = list("Satellite chlorophyll anomaly at Stn. ALOHA", cex = 1.5),
      line = -1.5,
      adj  = 0.05)
title(ylab    = "CHL [mg/m^3]", 
      cex.lab = 1.5,
      line    = 3.5)
title(xlab    = "Year", 
      cex.lab = 1.5,
      line    = 2.5)
box(which = "plot", lty = "solid", lwd = 3, col = "grey12")

par(mar = c(5, 5, 5, 1))
plot(time, tc, 
     pch = 19, 
     cex = 0.75, 
     xlab = "",
     ylab = "",
     col = "grey22", 
     axes = FALSE,
     xlim = c(as.Date("1997-10-01"), as.Date("2023-01-01"))
)
abline(h = median(tc) + mad(tc), 
       col="red", lwd = 1, lty = 1)
axis(side = 2,
     las = 2, 
     lwd = 2, 
     mgp = c(1, 0.75, 0), 
     cex.axis = 1.5)
axis(side = 1,
     labels = as.character(year(seq(min(time), max(time), length = 10))),
     at =  as.numeric(seq(min(time), max(time), length = 10)), 
     las = 1, 
     lwd = 2, 
     mgp = c(2, 1, 0), 
     cex.axis = 1.5)
title(main = list("Satellite chlorophyll at Stn. ALOHA", cex = 1.5),
      line = -1.5,
      adj  = 0.05)
title(ylab    = "CHL [mg/m^3]", 
      cex.lab = 1.5,
      line    = 3.5)
title(xlab    = "Year", 
      cex.lab = 1.5,
      line    = 2.5)
box(which = "plot", lty = "solid", lwd = 3, col = "grey12")









