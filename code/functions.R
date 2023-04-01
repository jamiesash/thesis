# savenc -----------------------------------------------------------------------
# Function to save raster's (both stack and layers) to netcdf files. Input is a 
# raster stack or layer and the output is a set of separate netcdf, one file for 
# every layer in the raster stack. Currently sets a working directory in the 
# function, which is not good and needs to be changed. If this stops working, 
# set uni back to "deg_C". 
# Arguments: 
#    X: Rasterstack object or layer to be saved. Must have a Z time value. 
#    sdir: The directory to save the nc files to. 
#    ncname: Name of the new netcdf file being written 
#    dlname: name of the primary variable. Default is "VALUE", change to "value"
#    uni: units of the primary variable being saved. Default is for chl
savenc    = function(X, 
                  sdir   = getwd(), 
                  ncname = "CHL_day", 
                  dlname = "VALUE",
                  uni    = "mg/m3") {
  
  #have extent as an input
  #setwd(sdir)
  
  s     <- dim(X) ## dimensions
  ext   <- extent(X)
  lons  <- c(ext[1], ext[2])
  lats  <- c(ext[3], ext[4])
  lons  <- seq(min(lons), max(lons), by = ((max(lons) - min(lons))/(s[2] - 1)))
  lats  <- seq(min(lats), max(lats), by = ((max(lats) - min(lats))/(s[1] - 1)))
  sdate <- getZ(X) ## fake datetime vector for stl input
  
  # Creating variables to save: Gridded Arrays
  value <- array(raster::values(X), c(s[1], s[2],s[3])) ## the empty array
  
  # Creating variables to save: Global atributes
  history <- paste("P.J. Bartlein", date(), sep=", ")
  
  # path and file name, set dname
  ncpath  <- sdir
  ncfname <- paste(ncpath, "/", ncname, ".nc", sep="")
  
  # create and write the netCDF file -- ncdf4 version define dimensions
  # will need a lon lat and time variable
  londim  <- ncdim_def("lons", "degrees_east",  as.double(lons)) 
  latdim  <- ncdim_def("lats", "degrees_north", as.double(lats)) 
  timedim <- ncdim_def("time", "sec",           as.double(sdate))
  
  # define variables
  fillvalue <- 1e32
  #value.def <- ncvar_def("value", uni, list(londim, latdim, timedim), 
  #                       fillvalue, dlname, prec = "single")
  value.def <- ncvar_def(name     = "value", 
                         units    = uni, 
                         dim      = list(londim, latdim, timedim), 
                         missval  = fillvalue, 
                         longname = dlname, 
                         prec     = "single")
  
  # Creat the nc file to fill
  ncout <- nc_create(ncfname, list(value.def), force_v4 = FALSE)
  
  # put variables
  #ncvar_put(ncout, value.def, value)
  ncvar_put(ncout, vals = raster::values(X))
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout, "lons",  "axis", "X") #,verbose=FALSE) #, definemode=FALSE)
  ncatt_put(ncout, "lats",  "axis", "Y")
  ncatt_put(ncout, "time",  "axis", "T")
  
  # add global attributes
  ncatt_put(ncout, 0, "history", history)
  
  # Get a summary of the created file:
  #print(ncout)
  
  # close the file, writing data to disk
  nc_close(ncout)
}

# load_nc ----------------------------------------------------------------------
load_nc = function(path = "C:\\Users\\james\\Desktop\\jamieslife\\data\\infiles\\fsle\\",
                   patt = "fsle_po_2018.nc", 
                   vars = c("lat", "lon", "fsle_max", "time"),
                   origin = "1950-01-01"){
  infile = paste(path, patt, sep = "")
  dat   = nc_open(infile)
  lat   = ncvar_get(dat, varid = vars[[1]])
  lon   = ncvar_get(dat, varid = vars[[2]])
  ras   = ncvar_get(dat, varid = vars[[3]])
  time  = ncvar_get(dat, varid = vars[[4]])
  attr  = ncatt_get(dat, varid = 0)
  
  nc_close(dat)
  time  = as.Date(time, origin = as.Date(origin))
  ras   = brick(ras)
  
  extent(ras) = extent(min(lon), max(lon), min(lat), max(lat))
  ras  = setZ(ras, z = time, name = "time")
  ras
}

# loadnc -----------------------------------------------------------------------
# Loads all mat files from sdir and outputs them as a single raster-stack with a 
# time value var1 and lats/lons. Currently sets the working directory in the 
# functoin which is bad grammer. Then lists the files, and uses lapply to read, 
# get variables from, and "unlists" the time variable so it can be rbined. 
# Matfiles have listed variables so this needs to match with the order of the 
# netcdf. This is for netcdf that I save, and I try to keep the variables order
# consistent.
# Arguments...
#    patt: the pattern in the file name to recognize and grab
#    var1: a variable to grab. Default is time. 
#    var2: A variable in netcdf. Default is the matrix values ie. sla, chl. I 
#          save all my netcdf with the relivent variable always as value. 
#    var3: A variable in a netcdf. Default is latitude
#    var4: A variable in a netcdf. Default is longitude. 
#    alot: Logical. Lots of daily files? alot = TRUE. One brick? alot = FALSE
#    flip: Should the map be fliped? If so, direction is given by rect
#    rect: The direction to flip the map. "y" or "x"

loadnc = function(sdir = "C:\\Users\\james", 
                  patt = "\\.nc",
                  var1 = "time", 
                  var2 = "value",
                  var3 = "lats", 
                  var4 = "lons",
                  alot = TRUE,
                  t    = FALSE,
                  flip = FALSE,
                  rect = "y",
                  origin = 1970) {
  file2nc <- function(x) {
    #Get relevent info from each NetCDF file
    file <- nc_open(x)
    lats <- ncvar_get(file, varid = var3)
    lons <- ncvar_get(file, varid = var4)
    ras  <- ncvar_get(file, varid = var2)
    attr <- ncatt_get(file, varid = 0)
    #if (is.null(attr$period_start_day) == TRUE) {
    s    <- dim(ras)
    if(alot == TRUE) {
      time <- attr$period_start_day
      ras  <- raster(ras) 
      } else {
        time <- ncvar_get(file, varid = var1)
        if(length(time)==1) ras  <- raster(ras) 
        else ras <- brick(ras)
        # time <- as.Date(time, origin = as.Date("1900-01-01"))
        if(origin == 1950) time <- as.Date(time, origin = as.Date("1950-01-01"))
        if(origin == 1970) time <- as.Date(time, origin = as.Date("1970-01-01"))
        if(origin == 1990) time <- as.Date(time, origin = as.Date("1990-01-01"))
        }
    nc_close(file)
    time <- anydate(time)
    ras  <- setZ(ras, time, name="time")
    extent(ras) <- c(min(lons), max(lons), min(lats), max(lats))
    ras
  }
  
  files <- list.files(path = sdir, pattern = patt, recursive = TRUE) 
  files <- paste(rep(sdir, length(files)), files, sep="")
  ras   <- lapply(files, file2nc)
  time  <- lapply(ras,   getZ) # Z gets lost along the way so grabbing it now
  time  <- unlist(time,  recursive=FALSE)
  # does not like stackin rasters with slightly diff extents so forcing it
  for(i in 1:length(ras)) extent(ras[[i]]) <- extent(ras[[1]])
  ras   <- stack(ras)
  e     <- extent(ras)
  if(t    == TRUE) ras <- t(ras)
  if(flip == TRUE) ras <- raster::flip(ras,  direction = rect)
  raster::crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  # extent(ras) <- e
  extent(ras) <- extent(-170, -130, 20, 35)
  ras   <- setZ(ras, anydate(time), name = "t")
  ras
}

# loadnc2 ----------------------------------------------------------------------
# Not sure if theres a difference between this and the other loadnc. Maybe it's
# for nc files that I get from other sources ie. not saved by me? Or it's just 
# old. I typicaly add 2 tot he end of a function while I'm updatting it, then 
# delete the 2 function when I'm satisfied with whatever changes I've made. 
loadnc2    = function(sdir = "C:\\Users\\james", 
                     patt = "\\.nc",
                     var1 = "time", 
                     var2 = "value",
                     var3 = "lats", 
                     var4 = "lons",
                     t    = FALSE, 
                     flip = FALSE,
                     rect = "y"
                     ) {
  file2nc <- function(X) {
    #Get relevent info from each NetCDF file
    file <- nc_open(X)
    lats <- ncvar_get(file, varid = var3)
    lons <- ncvar_get(file, varid = var4)
    ras  <- ncvar_get(file, varid = var2)
    attr <- ncatt_get(file, varid = 0)
    #if (is.null(attr$period_start_day) == TRUE) {
    if (dim(ras)[3] > 1) {
      time <- ncvar_get(file, varid = var1)
      #time <- as.Date(time, origin = as.Date("1970-01-01"))
      time <- as.Date(time, origin = as.Date("1900-01-01"))
      ras  <- brick(ras)
    } else {
      time <- attr$period_start_day
      ras  <- raster(ras)
    }
    nc_close(file)
    time <- anydate(time)
    ras  <- setZ(ras, time, name="time")
    extent(ras) <- c(min(lons), max(lons), min(lats), max(lats))
    ras
  }
  files <- list.files(path = sdir, pattern = patt, recursive = TRUE) 
  files <- paste(rep(sdir, length(files)), files, sep="")
  ras   <- lapply(files, file2nc)
  time  <- lapply(ras,   getZ) # Z gets lost along the way so grabbing it now
  time  <- unlist(time,  recursive=FALSE)
  # does not like stackin rasters with slightly diff extents so forcing it
  for (i in 1:length(ras)) extent(ras[[i]]) <- extent(ras[[1]])
  ras   <- stack(ras)
  
  if (t == TRUE)    ras <- t(ras)
  if (flip == TRUE) ras <- flip(ras,  direction = rect)
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  extent(ras) <- extent(-170, -130, 20, 35)
  
  ras   <- setZ(ras, anydate(time), name = "time")
  ras
}
# loadmats ---------------------------------------------------------------------
# Loads all mat files from diric and outputs them as a single raster-stack.
# Currently sets the working directory in the functoin which is bad grammer. 
# Then lists the files, and uses lapply to read, get variables from. This is for
# matfiel that I save, and I try to keep the variables order consistent. Order 
# of variables in the matfile matters a lot. Lon in the 200's because +360
# Arguments:
#    diric:   directory to be given where the matfiles to be loaded are
#    patt:    pattern of files to be recognized. Default is .mat
#    datenum: If TRUE the time value is given as a numeric, if FALSE it is 
#             given as a a Datetime class
loadmats  =  function(diric, patt = "\\.mat", datenum = TRUE) {
  setwd(diric)
  #read all files in the working directory that end in .mat
  f    <- list.files(pattern = patt) 
  ras  <- lapply(f,   read.mat)    #Reads all layers and saves as an object 
  vec  <- lapply(ras, function(l) l[[4]]) #Grabs the fourth field from each file
  lons <- lapply(ras, function(l) l[[2]]) #Grabs the first field from each file
  lats <- lapply(ras, function(l) l[[3]]) #Grabs the first field from each file
  ras  <- lapply(ras, function(l) l[[1]]) #Grabs the first field from each file
  ras  <- lapply(ras, raster) #converst matrixes into single layer rasters
  # stacks the rasters into a single rasterstack object
  ras  <- stack(ras)
  #rbinds the timestamps to one vector
  vec  <- do.call(rbind, vec) 
  # output the Z value as a numeric of Datetime variable. STL likes Datetime?
  if (datenum==FALSE) vec <- as.Date(vec, origin = as.Date("0000-01-01"))
  ras <- setZ(ras, vec, name = "time")
  # Set the Lat/lon extent of the rasterstack. Should be same as lat/lon values.
  extent(ras) <- extent(c(min(lons[[1]]), #extent(ras) <- c(190, 230, 20, 35) 
                          max(lons[[1]]), 
                          min(lats[[1]])+360, 
                          max(lats[[1]])+360))
  ras
} #end

# stlfilter ---------------------------------------------------------------------
# stlfilter() Performs an STL filter on raster object by looping through each 
# grid cell. Filters each girded cell across time and either outputs the 
# seasonal or the residual component as a raster stack. Has a confusing error 
# catch in the for loop. Raster object must have time z value. Output will have
# a Z value as well. Inputs one raster stack, outputs one raster stack. 
# Arguments...
#    ras:   The raster stack object to be filtered. Must have a Z parameter set
#    np:    The length of the period to be seasonal filtered. Default is 91 for  
#           4 day temporal resolution as 91*4 = 364 days in a year. 
#    subst: Short for sub.start argument of prewriten stlplus function. "which 
#           element of sub.labels does the series begin with" - Defoult is 1, 
#           meaning our cycle starts on the first day of a year (for yearly 
#           trends), and our data must start on the 1st day of the year as well. 
#    ot:    Short for the outer argument of the stlplus() function. integer; 
#           "the number of outer robustness iterations. Default is 0, but 
#           recommended if outliers are present." Higher outer value generates 
#           residuals that look more like the input data I think. 
#    wind:  Short for the s.window window argument of the stlplus function. 
#           "either the character string "periodic" or the span (in lags) of the 
#           loess window for seasonal extraction, which should be odd. This has 
#           no default." Length of the loess running window
#    data:  Logical TRUE/FASLE to denote if the resudals or the seasonal data
#           should be saved and output. Should use a string probably
#    rem:   Should the remainder be saved? If false the seasonal cycle is saved.
stlfilter = function(ras, np = 91, subst = 1, ot = 4, wind = 7, rem = TRUE) {
  ## set the memory to handle larger matrices
  rasterOptions(maxmemory = 100e+10)
  ## initialize an empty vector of the same size.
  s     <- dim(ras) ## dimensions
  sdate <- getZ(ras) ## fake datetime vector for stl input
  y     <- array(rep(NA, s[1]*s[2]*s[3]), c(s[1], s[2],s[3])) ## the empty array
  na_id <- is.na(ras)
  ras   <- approxNA(ras)
  for (i in 1:s[1]) {
    if (i == round((s[1]/5)))   print("So much more to go")
    if (i == round(s[1]/2))     print("You're half way there")
    if (i == round((s[1]/5)*4)) print("Last leg")
    for (j in 1:s[2]) {
      tryCatch({
        stlDaily <- stlplus(x         = ras[i,j,], # One time series of the raster
                            t         = sdate, # datetime vector
                            n.p       = np, # give the period of seasonality
                            s.window  = wind, # length of the running window
                            sub.start = subst,
                            outer     = ot) 
        if (rem == FALSE) y[i,j,]  <- stlDaily$data$seasonal  #keep the seasonal
        if (rem == TRUE)  y[i,j,]  <- stlDaily$data$remainder #keep the residual
      }, error=function(e){
        #skips to next iteration if there's an error  
      }) 
    } 
    if (i == max(s[1])) print("Finish!")
    }
  y <- brick(y)
  y[na_id]  <- NA #X and Y are not same dim()
  y <- setZ(y, sdate, name="time")
  extent(y) <- extent(ras)
  y  
} 

# bloomfreq --------------------------------------------------------------------
# Input is a Rasterstack object with a set Z value as a Datetime variable. 
# Output is a vector of 12 values one for each month. Mean value of chl in each 
# month, not frequency. Should be done as a data frame with a separate row of 
# months for each value. This does take the mean of means (grand mean) so should 
# be careful as it may not be accurate. Finds months equal to the month number 
# in a loop then averages them. i in the loop is a string which I love.
# Arguments:
#    x: Raster stack with a Datetime value as the Z value. 
#       Should have at least one value for each month, otherwise an error given
bloomfreq = function(x, func = "mean", valname = "chl") {
  themonths <- c("January",
                 "February", 
                 "March", 
                 "April", 
                 "May",
                 "June", 
                 "July",
                 "August", 
                 "September", 
                 "October", 
                 "November",
                 "December")
  sdate <- getZ(x)
  m     <- months(sdate) 
  clim  <- rep(0, 12)
  j     <- 0
  # looping through each the months and finding the average in the data set
  for (i in themonths) {
    j         <- j + 1
    idx_m     <- which(m==i)
    Z         <- subset(x, idx_m)
    clim[j]   <- mean(cellStats(Z, stat = func, na.rm = TRUE), na.rm = TRUE)
  }
  clim <- data.frame(clim ,c("January", "February", "March", "April", "May",
                             "June", "July", "August", "September", "October", 
                             "November", "December"))
  colnames(clim) <- c(valname, "month")
  clim
  }

# bloomclim --------------------------------------------------------------------
# Input is a Rasterstack object with a set Z value as a Datetime variable. 
# Output is a rasterstack with 12 raster layers  one for each month. To be 
# ploted using levelplot or mylevelplot.
# Arguments:
#    x: Raster stack with a Datetime value as the Z value. 
#       Should have at least one value for each month, otherwise an error given
bloomclim = function(x) {
  themonths <- c("January","February", "March", "April", "May","June",  "July",
                 "August", "September", "October", "November","December")
  sdate <- getZ(x) # pulling out the Z value to be worked on
  sdate <- anydate(sdate)
  m     <- months(sdate) # getting the months from the Z value
  j     <- 0 # start j at 0 to count loops. i is a string
  clim  <- stack() #initialize an empty raster stack
  # looping through each month and finding a spacial average using calc()
  for (i in themonths) {
    j      <- j + 1
    idx_m  <- which(m == i)
    z      <- subset(x, idx_m)
    #Z      <- calc(Z, mean, na.rm = TRUE)
    z      <- calc(z, median, na.rm = TRUE)
    clim   <- addLayer(clim, stack(z))
  }
  # Should add a time stamp and extent to the raster
  clim
  }

# subsum -----------------------------------------------------------------------
# Subsets the late summer months from a rasterstack and outputs a rasterstack. 
# Input is a rasterstack with Z values as a Datetime variable. Output is a 
# rasterstack comprised of only the late summer months of the input data set
# one value for each month must be included in the input rasterstack for
# function to work.  
# Arguments...
#    x: Rasterstack with Z values as a Datetime variable. 
subsum    = function(x, mnths = 7:10) {
  sdate     <- getZ(x)
  themonths <- c("January","February",
                 "March", "April",
                 "May","June", 
                 "July","August",
                 "September", "October",
                 "November","December")
  m     <- months(sdate) 
  idx_t <- which(is.element(m, themonths[mnths]))
  x     <- subset(x, idx_t)
  sdate <- sdate[idx_t] 
  x     <- setZ(x, sdate, name = "time")
  x
}

# mycrop -----------------------------------------------------------------------
# Crops a raster stack or layer across a given extent, and timespan. Input is a 
# rasterstack or raster layer with Z value as a Datetime. Output is a 
# rasterstack with an updated extent. For now the region is given as a string,
# but should probably be given as an extent. ALOHA and 30N extent is included in
# the function, but should probably be outside of the function. I honestly don't
# know if this function works properly. It is messy. Looks like it was ported 
# over from one of my matlab functions. 
# Arguments...
#    X:      rasterstack object with Z value as a Daatetime
#    lons:   longitude values as separate variable. This is some matlab shit
#    lats:   latitude values as separate variables, this is some matlab shit
#    sdate:  datetime variable as a separate object. This is some matlab shit
#    start:  time-stamp to begin the cut
#    stop:   times-tamp to end the cut
#    region: either "ALOHA" or anything not "ALOHA" recognized as FALSE
mycrop    = function(X, lons, lats, sdate, start, stop, region) {
  if (string(region) == "ALOHA") {
    i = 1 
  } else {
    i = 2
  }
  
  lat1 = c(c(22,   27),  
           c(27.5, 35))
  lon1 = c(c(-159, -152), 
           c(-159, -132))
  lat  = lat1[i, ]
  lon  = lon1[i, ]
  
  test <- crop(X, extent(lat, lon))
  
  lon = lon + 360 #lons not in correct format
  index_lon = which(lons > lon(1) & lons < lon(2))
  index_lat = which(lats > lat(1) & lats < lat(2))
  
  #index by time of bloom
  start = double(datenum(start)) 
  stop  = double(datenum(stop)) 
  time2 = double(sdate)
  ind_t = find(time2 >= start  & time2 <= stop)
  
  field = X[index_lat, index_lon, ind_t]; #index climotology
  
  p = field;
}

# clean ------------------------------------------------------------------------
# This is a heavy function that can take up a lot of RAM if it's allowed. 
# Removes repeating layers. Keep first iteration of each unique time-stamp. 
# Fills in missing dates. Used to keep all raster's uniform so that indexing and 
# filters work as they should. Still converts a raster to an array which is a 
# bulky computation. 
# Arguments...
#    ras: Rasterstack object with a time Z value as a Datetime variable 
clean     = function(ras) {
  # save the extent
  e     <- extent(ras)
  
  # Remove repeating layers. Keep first iteration of each unique time-stamp.
  ts    <- getZ(ras)
  ts    <- as.Date(substr(ts, 1, 10)) # remove timestamp but keep day year month
  ind_t <- which(!duplicated(ts))
  ras   <- subset(ras, ind_t)
  ts    <- ts[ind_t]
  
  # filling in missing dates
  odate <- seq(from = min(ts), to = max(ts), by=1)
  found <-  is.element(odate, ts)
  miss  <- !is.element(odate, ts)
  fdate <- odate[found]
  mdate <- odate[miss]
  
  if (length(mdate) > 1) {
    s     <- as.numeric(dim(ras))
    y     <- array(rep(NA, s[1]*s[2]*length(mdate)), 
                   c(s[1], s[2], length(mdate)))
    y     <- brick(y)
    extent(y) <- extent(ras)
    ras   <- stack(y, ras)
    temp  <- c(fdate, mdate)
    
    idx   <- sort.int(temp, index.return = TRUE)$ix
    dates <- sort.int(temp, index.return = TRUE)$x
    ras   <- ras[[idx]]
    #inputting the dates in Z field and printing raster
    ras   <- setZ(ras, dates)
    } else {
      ras <- setZ(ras, ts)
    }
  
  extent(ras) <- e
  ras
  }

# vect -------------------------------------------------------------------------
# Takes a rasterstack and converts it to a long format data frame using 
# rastertoPoints and melt functions. This is to prep for a cluster analysis. 
# Arguments...
#    x: rasterstack with the Z values as Datetime variables
vect      = function(x) {
  sdate <- getZ(x)
  x     <- rasterToPoints(x)
  x     <- data.frame(x)
  colnames(x) <- c("lat", "lon", as.character(sdate))
  # may need to use data.table::melt()
  x     <- reshape2::melt(x, id.vars = c("lat", "lon"))
}

# mylevelplot ------------------------------------------------------------------
# Levelplot function that plots a with my default settings that I like. 
# Coastlines are all messed up. I made a better map function in a seperate 
# script that I need to write into a function. This one is useless and I will 
# not use it. 
mylevelplot = function(X, 
                       title = "CHL (mg m^-3) bloom", 
                       lay   = 1, 
                       cuts  = 8) {
  
  library(rgeos)
  library(maptools)
  library(rnaturalearthdata)
  library(rnaturalearth)
  ## Loading the world coasts, and convert from Atlantic to Pacific view
  xrange <- c(0,  360)
  yrange <- c(-90, 90)
  
  world <- ne_countries(scale = "medium", returnclass = "sp")
  
  ext <- extent(xrange[1], xrange[2], yrange[1], yrange[2])
  data(wrld_simpl, package = "maptools")
  
  eastrange <- c(xrange[1], 180.0)
  westrange <- c(-180.0, xrange[2] - 360)
  
  ew <- extent(westrange[1], westrange[2], yrange[1], yrange[2])
  ee <- extent(eastrange[1], eastrange[2], yrange[1], yrange[2])
  
  geome <- as(ee, "SpatialPolygons")
  geomw <- as(ew, "SpatialPolygons")
  
  proj4string(geome) <- CRS(proj4string(wrld_simpl))
  proj4string(geomw) <- CRS(proj4string(wrld_simpl))
  
  worlde <- gIntersection(wrld_simpl, geome)
  worldw <- gIntersection(wrld_simpl, geomw)
  worldw <- elide(worldw, shift = c(360.0, 0.0))
  
  proj4string(worldw) <- CRS(proj4string(wrld_simpl))
  
  dfe <- data.frame(x = seq_len(length(worlde@polygons)))
  dfw <- data.frame(x = seq_len(length(worldw@polygons)))
  rownames(dfw) <- as.character(as.numeric(rownames(dfe)) + nrow(dfe))
  
  worlde <- SpatialPolygonsDataFrame(worlde, dfe, match.ID = FALSE)
  worldw <- SpatialPolygonsDataFrame(worldw, dfw, match.ID = FALSE)
  worldw <- spChFIDs(worldw, rownames(dfw))
  
  ## I had to add this spTransform() call to stop the spRbind proj error
  worldw_proj <- spTransform(worldw, CRSobj = worlde@proj4string)
  world <- spRbind(worlde, worldw_proj)
  
  ##############################################################################
  
  e6       <- extent(210, 220, 22, 27)
  e6pol    <- as(e6, 'SpatialPolygons')
  centroid <- coordinates(e6pol)
  
  extent(X) <- extent(190, 230, 20, 35)
  
  ##############################################################################
  
  levelplot(X, 
            layers  = lay, 
            contour = TRUE, 
            pretty  = TRUE, 
            cut     = cuts,
            margin  = FALSE, 
            par.settings = rasterTheme(viridis_pal(option = "D")(255)), 
            main    = list(title, side=1,line=0.3), 
            xlab    = "Longitude", 
            ylab    = "Latitude") +
    latticeExtra::layer(sp.lines(world))
}

# map --------------------------------------------------------------------------
# Maps a single raster layer using imagerp function and worldmap. Plots land as 
# black images. Should be able to plot anywhere in the ocean using the same lat
# lon box. 
# Arguments...
#     val:   Raster layer that has a true lat lon extent, not 0-1
#     caxis: color axis cut offs of the plot. 
#     title: title of the main plot
#     cuts:  How many cuts to use in the continous scale?
#     name:  Plot title
#     col:   CMOCEAN color pallet - algae, tempo, topo, haline, delta
#     clab:  Label for the colormap
map   = function(val, 
                  zlim = c(min(raster::values(val), na.rm=TRUE), 
                           max(raster::values(val), na.rm=TRUE)), 
                  cuts = 25, 
                  col  = 'haline',
                  main = "The Title",
                  mai = c(0.01, 0.01, 0.01, 0.1), 
                  line = 1,
                  adj  = 0,
                  xlab = "Longitude",
                  ylab = "Latitude",
                  zlab = "CHL",
                  xaxes = TRUE,
                  yaxes = TRUE,
                  cex.axis = 1.5,
                  cex.lab  = 1.25,
                  cex.main = 1.5,
                  cex.zlab = 1,
                  plot = TRUE,
                  colorbar = TRUE,
                  add  = FALSE,
                  mar = c(5, 5, 5, 2)) {

  
  wdmap <- getMap(resolution = "high")
  s     <- dim(val)
  e     <- extent(val)
  lons  <- seq(from = e[1], to = e[2], length = s[1])
  lats  <- seq(from = e[3], to = e[4], length = s[2])
  val   <- raster::flip(val, direction = "x")
  val   <- raster::clamp(val, zlim[1], zlim[2])
  
  par(mar = mar)
  if(length(col) == 1) {col <- cmocean(col)(cuts)} 
  if(colorbar == TRUE) {
    drawPalette(zlim = zlim, 
                #mai = mai,
                col  = col, 
                plot = plot,
                pos  = 4,
                zlab = zlab,
                #drawContours = TRUE,
                cex  = cex.zlab) 
  }
  image(lons, lats, as.matrix(val),
        col  = col, # algae, tempo, topo, haline, delta, 
        xlim = e[1:2],
        ylim = e[3:4],
        zlim = zlim,
        main = "",
        xlab = "",
        ylab = "",
        asp = NA,
        axes = FALSE,
        cex.lab  = cex.lab,
        cex.axis = cex.axis,
        add = add)
  title(main = list(main), line = line,  adj = adj, cex.main = cex.main)
  title(xlab = xlab, line = 2.6, cex.lab = 1.75)
  title(ylab = ylab, line = 2.6, cex.lab = 1.75)
  if(yaxes == TRUE) axis(side = 2, 
                         las = 2, 
                         lwd = 2, 
                         at = c(22, 26, 30, 34),
                         mgp = c(1, 0.75, 0), 
                         cex.axis = cex.axis)
  if(xaxes == TRUE) axis(side = 1, 
                         las = 1, 
                         lwd = 2,
                         mgp = c(2, 1, 0),    
                         at = floor(seq(from = e[1]+2, to = e[2]-2, length = 4)),
                         cex.axis = cex.axis)
  box(which = "plot", lty = "solid", lwd = 3, col = "grey25")
  plot(wdmap, 
       xlim = e[1:2], 
       ylim = e[3:4], 
       asp = 1, 
       bg = "white", 
       border = "black", 
       col = "ivory", 
       #wrap = c(-180, 180), 
       add = TRUE)
}
# Line equations  --------------------------------------------------------------
# Equations that take a model summary output and convert the equation to text. 
# Alot of different ones are given because they are not smart functions. 
# Functions...
#    lm0_eqn: 0 intercept linear regression
#    lm_eqn: linear regression
#    nls_eqn: non least squares linear regression.
# Arguments...
#    x:   vector of x values
#    y:   vector of y values
#    dat: data frame x and y come from
#    srt: start values for nls only. m = slope, b = intercept

lm0_eqn <- function(x, y){
  m <- lm(y ~ 0 + x);
  eq <- substitute(italic(y) == a %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a  = format(unname(coef(m)[1]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

#lm_eqn <- function(x, y){
#  m <- lm(y ~ x);
#  
#  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                   list(a  = format(unname(coef(m)[1]), digits = 2),
#                        b  = format(unname(coef(m)[2]), digits = 2),
#                        r2 = format(summary(m)$r.squared, digits = 3)))
#  as.character(as.expression(eq));
#}

lm_eqn <- function(slope, intercept, r){
  eq <- substitute(italic(y) == b + m %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b  = intercept,
                        m  = slope,
                        r2 = r))
  as.character(as.expression(eq));
}

nls_eqn <- function(x, y, dat, srt = list(m = 1, b = 0.05)){
  m <- nls(y ~ x * m + b, data = dat, start = srt)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a  = format(unname(coef(m)[1]), digits = 2),
                        b  = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(sqrt(summary(m)$sigma), digits = 3)))
  as.character(as.expression(eq));
}

# triplot ----------------------------------------------------------------------
# Generates a triangle plot using ggplot scatter plot function. Not actually 
# how it's done in the Guo paper. 
triplot = function(tbl, main, small = FALSE) {  
  # Removing values below FSLE detection limit
  #xyz <- subset(xyz, !xyz$fsle == 0) 
  tbl <- subset(tbl, !is.na(xyz$chl)) 
  
  # Downsizing data frame to test plotting
  if (is.numeric(small)) {
    ind <- sample(1:nrow(tbl), small)
    tbl <- tbl[ind,]
  }
  
  # Creating breaks and color scale manually
  clow  <- round(median(tbl$chl, na.rm = TRUE) - sd(tbl$chl) * 2, 2)
  chigh <- round(median(tbl$chl) + sd(tbl$chl) * 2, 2)
  tbl$chls <- oob_squish(tbl$chl, range = c(clow, chigh), only.finite = TRUE)
  steps <- round(seq(from = clow, to = chigh, length = 20), 2)
  # make mid values more transparent
  trans <- abs(scales::rescale(tbl$chls, to = c(-1, 1))) 
  
  ggplot(data = tbl, aes(sla, fsle*-1, col = chls)) +
    geom_point(stroke = 0, alpha = trans, size = 0.1) +
    geom_vline(xintercept = mean(tbl$sla, na.rm=TRUE), size = 1, alpha = 0.5) +
    geom_vline(xintercept = mean(tbl$sla, na.rm=TRUE) + sd(tbl$sla, na.rm=TRUE),
               alpha = 0.5,
               linetype = "dashed",
               size = 1) +
    geom_vline(xintercept = mean(tbl$sla, na.rm=TRUE) - sd(tbl$sla, na.rm=TRUE), 
               linetype = "dashed",
               alpha = 0.5,
               size = 1) +
    geom_hline(yintercept = mean(tbl$fsle*-1, na.rm=TRUE), 
               size = 1, 
               alpha = 0.5) +
    scale_colour_steps2(low = "darkslategrey", 
                        mid = "white", 
                        high = muted("coral"),  
                        breaks = steps,
                        midpoint = mean(tbl$chl, na.rm = TRUE)) +
    scale_y_continuous(expand = c(0,0)) + 
    guides(colour = guide_coloursteps(show.limits = TRUE)) +
    xlab("SLA [m]") +
    ylab("FSLE [day^-1]") +
    labs(title = main, colour = "CHL [mg/m^3]") +
    #geom_text(x=-0.05, y= 0.27,  label= "(-) Sub") + 
    #geom_text(x= 0.2,  y= 0.27,  label= "(+) Sub") + 
    #geom_text(x=-0.05, y= 0.03,  label= "(-) Meso") + 
    #geom_text(x= 0.2,  y= 0.03,  label= "(+) Meso") + 
    theme_bw() +  
    theme(legend.key.size = unit(2, "cm"), legend.key.width = unit(1,"cm"))
}

# thresher ---------------------------------------------------------------------
# Input is a table with columns sla, and fsle. Thresholds are calculated. 
# Output is a row with percent of points above that threshold
thresher <- function(tbl, mix = TRUE, anom = TRUE) {
  if (mix == TRUE) {
    # remove clouds from chl
    tbl   <- subset(tbl, !is.na(tbl$chl))
    
    eddy  <-   sd(tbl$sla,  na.rm = TRUE)
    usla  <- mean(tbl$sla,  na.rm = TRUE)
    ufsle <- mean(tbl$fsle, na.rm = TRUE)
    uchl  <- median(tbl$chl, na.rm = TRUE)
    
    if (anom == TRUE) tbl   <- subset(tbl, chl > uchl)
    
    mesop <- tbl[tbl$sla > (usla + eddy) & tbl$fsle < ufsle,]
    meson <- tbl[tbl$sla < (usla - eddy) & tbl$fsle < ufsle,]
    subm  <- tbl[tbl$sla < (usla + eddy) & tbl$sla < (usla - eddy) & tbl$fsle > ufsle,]
    mixn  <- tbl[tbl$sla > (usla - eddy) & tbl$sla < (usla + eddy) & tbl$fsle < ufsle,]
    mixp  <- tbl[tbl$sla > (usla - eddy) & tbl$sla < (usla + eddy) & tbl$fsle > ufsle,]
    
    mixp  <-  (nrow(mixp) / nrow(tbl)) * 100 
    mixn  <-  (nrow(mixn) / nrow(tbl)) * 100 
    subm  <-  (nrow(subm) / nrow(tbl)) * 100 
    mesop <- (nrow(mesop) / nrow(tbl)) * 100
    meson <- (nrow(meson) / nrow(tbl)) * 100
    
    df_mix    <- data.frame(mesop, meson, subm, mixn+mixp)
    colnames(df_mix) <- c("mesop", "meson", "subm", "mix")
    df_mix
  }
  
  if (mix == FALSE) {
    
    # remove clouds from chl
    tbl   <- subset(tbl, !is.na(tbl$chl))
    
    usla  <- mean(tbl$sla,  na.rm = TRUE)
    ufsle <- mean(tbl$fsle, na.rm = TRUE)
    uchl  <- median(tbl$chl, na.rm = TRUE)
    
    if (anom == TRUE) tbl <- subset(tbl, chl > uchl)
    
    mesop <- tbl[tbl$sla  > usla  & tbl$fsle < ufsle,]
    meson <- tbl[tbl$sla  < usla  & tbl$fsle < ufsle,]
    subp  <- tbl[tbl$sla  > usla  & tbl$fsle > ufsle,]
    subn  <- tbl[tbl$sla  < usla  & tbl$fsle > ufsle,]
    
    subp  <-  (nrow(subp) / nrow(tbl)) * 100 
    subn  <-  (nrow(subn) / nrow(tbl)) * 100
    mesop <- (nrow(mesop) / nrow(tbl)) * 100
    meson <- (nrow(meson) / nrow(tbl)) * 100
    
    df_no <- data.frame(mesop, meson, subp, subn)
    colnames(df_no) <- c("mesop", "meson", "subp", "subn")
    df_no
  }
}

# tableit ----------------------------------------------------------------------
# Uses the previously defined vect function to generate a long format table from
# raster stack objects
# x: a raster or list of rasters as c(x, y, z) where x, y, and z are rasters 

tableit <- function(x) {
  
  vectorize  <- function(x) {
    sdate <- getZ(x)
    x     <- rasterToPoints(x, na.rm = FALSE)
    x     <- data.frame(x)
    
    colnames(x) <- c("lat", "lon", as.character(sdate))
    x     <- reshape2::melt(x, id.vars = c("lat", "lon"))
    
    colnames(x) <- c("lats", "lons", "time", "val")
    x
  }
  
  x     <- lapply(x, vectorize)
  vals  <- lapply(x, subset, select = "val")
  quord <- lapply(x, subset, select = c("lats", "lons", "time"))
  
  xyz   <- data.frame(quord[1], vals)
  #colnames(xyz) <- c("lats", "lons", "time", "sla", "fsle", "chl")
  
  # removing chl clouds
  #xyz   <- xyz[!is.na(xyz$chl), ]
  xyz
}

# crop3d -----------------------------------------------------------------------
crop3d  <- function(x, 
                    sdate = "2003-07-01",
                    edate = "2003-10-01", 
                    ext = extent(x)) {
  # subset time
  #if(!is.null(sdate)){
    t <- getZ(x)
    ind_t <- which(t > sdate & t < edate)
    indx_bool <- (t > sdate & t < edate)
    x <- subset(x, ind_t)
    t <- subset(t, indx_bool)
  #}
  
  # subset space
  #if(!is.null(ext)){
    ext <- extent(ext)
    x   <- raster::crop(x, ext)
    x   <- setZ(x, z = t, name= "time")
    x
   # }
}

# ------------------------------------------------------------------------------
timesnip = function(x, 
                     sdate = "2003-07-01",
                     edate = "2003-10-01") {
  # subset time
  t = getZ(x)
  ind_t = which(t >= sdate & t <= edate)
  indx_bool = (t >= sdate & t <= edate)
  x = subset(x, ind_t)
  t = subset(t, indx_bool)
  x = setZ(x, z = t, name = "time")
  x
  }

# findtime ---------------------------------------------------------------------
findtime <- function(x, thresh = 0.15) {
  y <- x
  raster::values(y)[is.na(raster::values(y))]   <- 0 
  if (is.numeric(thresh)) {
    raster::values(y)[raster::values(y) > thresh] <- 1 
    raster::values(y)[raster::values(y) < thresh] <- 0 
  } else {
    raster::values(y)[raster::values(y) > raster::values(thresh)] <- 1 
    raster::values(y)[raster::values(y) < raster::values(thresh)] <- 0 
  }
  blooms <- calc(y, sum, na.rm = TRUE)
  
  y <- x
  raster::values(y)[!is.na(raster::values(y))] <- 0 
  raster::values(y)[is.na(raster::values(y))]  <- 1 
  clouds   <- calc(y, sum) #na.rm should deal with clouds
  days     <- calc(y, length) #na.rm should deal with clouds
  sunydays <- days - clouds
  
  (blooms/sunydays)*100
}

# boot -------------------------------------------------------------------------
boot <- function(tbl, iter = 10, sigma = FALSE, mu = FALSE){
  # remove clouds from chl
  df   <- subset(tbl, !is.na(tbl$chl))
  sz   <- nrow(subset(df, chl > 0)) # size of sub-samples = total anomalies
  
  # creating an empty data frame for each sub-sample iteration
  df_no <- data.frame(rep(NA, iter), rep(NA, iter), rep(NA, iter), rep(NA, iter))
  colnames(df_no) <- c("mesp", "mesn", "sub", "mix")
  for (i in 1:iter) {
    ind   <- sample(1:nrow(df), sz, replace = FALSE)
    anom   <- df[ind, ] # treate these like the anomolies? or find anomolies again  
    #if (anom == TRUE) tbl <- subset(tbl, chl > 0) 
    sub  <- subset(anom, k == 1)
    mesp <- subset(anom, k == 4)
    mesn <- subset(anom, k == 2)
    mix  <- subset(anom, k == 3)
    sub  <- (nrow(sub)  / nrow(anom)) * 100 
    mesp <- (nrow(mesp) / nrow(anom)) * 100 
    mesn <- (nrow(mesn) / nrow(anom)) * 100 
    mix  <- (nrow(mix)  / nrow(anom)) * 100 
    df_no[i, ] <- data.frame(mesp, mesn, sub, mix)[1, ] 
  } 
  # sd and mean of each column. 
  if (sigma == TRUE) test <- apply(df_no, 2, FUN = sd,   na.rm = TRUE)
  if (mu    == TRUE) test <- apply(df_no, 2, FUN = mean, na.rm = TRUE) 
  test
}

# kperc ------------------------------------------------------------------------ 
kperc <- function(df, anom = TRUE){
  tbl   <- subset(df, !is.na(df$chl))
  if (anom == TRUE) tbl <- subset(tbl, chl > 0) 
  sub  <- subset(tbl, k == 1)
  mesp <- subset(tbl, k == 4)
  mesn <- subset(tbl, k == 2)
  mix  <- subset(tbl, k == 3)
  sub  <- (nrow(sub)  / nrow(tbl)) * 100 
  mesp <- (nrow(mesp) / nrow(tbl)) * 100 
  mesn <- (nrow(mesn) / nrow(tbl)) * 100 
  mix  <- (nrow(mix)  / nrow(tbl)) * 100 
  perc <- data.frame(mesp, mesn, sub, mix)
  colnames(perc) <- c("mesp", "mesn", "sub", "mix")
  perc
}

# timeseries -------------------------------------------------------------------
# Time series of year of interest: bloom threshold
timeseries <- function(x, 
                       y, 
                       xlim,
                       ylim = c(range(y, na.rm = TRUE)[1] 
                                - sd(y, na.rm = TRUE),
                                range(y, na.rm = TRUE)[2] 
                                + sd(y, na.rm = TRUE)),
                       ylab = "CHLsat Anomaly [mg/m^3]",
                       xlab = "Date",
                       thresh = median(y, na.rm = TRUE) + mad(y, na.rm=TRUE)*2,
                       main   = "Timeseries of CHLsat anomaly",
                       cex.main = 1.5,
                       line = 0.7,
                       adj = 0,
                       xaxis = TRUE,
                       yaxis = TRUE,
                       pretty = TRUE,
                       mar = c(3,3,3,3)) {
  # smooth timeseries
  if (pretty == TRUE) {
    y <- na_ma(y, k = 10, weighting = "exponential")
    y <- smooth(y)
  }
  # bloom threshold
  #thresh <- median(y) + mad(y)
  # all values above threshold are threshold
  y2 <- y
  y2[y2 < thresh] <- NA
  y2[y2 > thresh] <- thresh
  
  ylarge <- max(y, na.rm = TRUE)
  ysmall <- min(y, na.rm = TRUE)
  xlarge <- subset(x, y == ylarge)
  xsmall <- subset(x, y == ysmall)
  
  par(mar=mar)
  plot(x, y, 
       type = 'n', 
       ylim = ylim,
       xlim = xlim,
       xlab = "",
       ylab = "",
       axes = FALSE,
       main = "")
  box(which = "plot", lty = "solid", lwd = 2, col = "grey25")
  title(main = list(main, cex = cex.main),
        line= line,
        adj = adj)
  title(ylab = ylab, 
        cex.lab = 1.5,
        line = 3.5)
  title(xlab = xlab, 
        cex.lab = 1.5,
        line = 2.5)
  if(yaxis) axis(side = 2,
                 las = 2, 
                 lwd = 2, 
                 mgp = c(1, 0.75, 0), 
                 cex.axis = 1.5)
  if(xaxis) axis(side = 1, 
                 #at = as.numeric(x)[seq(from = 10, to = length(x), by = 5)],
                 #labels = labels[seq(from = 10, to = length(x), by = 5)],
                 las = 1, 
                 lwd = 2, 
                 mgp = c(2, 1, 0), 
                 cex.axis = 1.5)
  #text(xlarge - 8, ylarge, as.character(round(ylarge, 2)))
  #text(xsmall - 8, ysmall, as.character(round(ysmall, 2)))
  #points(c(xlarge, xsmall), c(ylarge, ysmall), pch = 21, bg = "grey49")
  #title(main = list(title), line = line, adj = adj)
  clip(x1 = min(x),
       x2 = max(x), 
       y1 = thresh, 
       y2 = max(y))
  polygon(c(min(x), x, max(x)), c(min(y), y, min(y)), col = "darkseagreen4")
  clip(par("usr")[1], par("usr")[2], par("usr")[3], par("usr")[4]) # reset clipping region
  lines(x, y,  type='l')
  lines(x, y2, lwd = 1)
  abline(h = thresh, col = "coral4")
}
# ------------------------------------------------------------------------------
map2   = function(val, 
                 zlim = c(min(raster::values(val), na.rm=TRUE), 
                          max(raster::values(val), na.rm=TRUE)), 
                 cuts = 25, 
                 col  = 'haline',
                 main = "The Title",
                 mai = c(0.01, 0.01, 0.01, 0.1), 
                 line = -2,
                 adj  = 0.05,
                 xlab = "Longitude",
                 ylab = "Latitude",
                 zlab = "CHL",
                 xaxes = TRUE,
                 yaxes = TRUE,
                 cex.axis = 1.5,
                 cex.lab  = 1.75,
                 cex.main = 1.5,
                 cex.zlab = 1,
                 plot = TRUE,
                 colorbar = TRUE,
                 t = TRUE,
                 add  = FALSE,
                 mar = c(5, 5, 5, 2)) {
  
  wdmap <- getMap(resolution = "high")
  s     <- dim(val)
  e     <- extent(val)
  lons  <- seq(from = e[1], to = e[2], length = s[2])
  lats  <- seq(from = e[3], to = e[4], length = s[1])
  if(t == TRUE) val   <- t(val)
  val   <- raster::flip(val, direction="x")
  val   <- raster::clamp(val, zlim[1], zlim[2])
  
  if(length(col) == 1) {col <- cmocean(col)(cuts)} 
  
  d     <- 0.14
  
  par(mar = mar)
  #layout(matrix(1:2, nrow=1), widths=c(1-d,d))
  # if(colorbar == TRUE) {
  #   drawPalette(zlim = zlim, 
  #               #mai = mai,
  #               col  = col, 
  #               plot = plot,
  #               pos  = 4,
  #               zlab = zlab,
  #               #drawContours = TRUE,
  #               cex  = cex.zlab) 
  # }
  image(lons, lats, as.matrix(val),
        col  = col, # algae, tempo, topo, haline, delta, 
        xlim = e[1:2],
        ylim = e[3:4],
        zlim = zlim,
        main = "",
        xlab = "",
        ylab = "",
        asp = NA,
        axes = FALSE,
        cex.lab  = cex.lab,
        cex.axis = cex.axis,
        add = add)
  title(main = list(main), line = line,  adj = adj, cex.main = cex.main)
  title(xlab = xlab, line = 2.6, cex.lab = cex.lab)
  title(ylab = ylab, line = 2.6, cex.lab = cex.lab)
  if(yaxes == TRUE) axis(side = 2, 
                         las = 2, 
                         lwd = 2, 
                         at = c(18, 22, 26, 30, 34),
                         mgp = c(1, 0.75, 0), 
                         cex.axis = cex.axis)
  if(xaxes == TRUE) axis(side = 1, 
                         las = 1, 
                         lwd = 2,
                         mgp = c(2, 1, 0),    
                         #labels = as.character(seq(-165, -130, by = 10)),
                         at = seq(-163, -133, by = 10),
                         cex.axis = cex.axis)
  box(which = "plot", lty = "solid", lwd = 3, col = "grey25")
  plot(wdmap, 
       xlim = e[1:2], 
       ylim = e[3:4], 
       asp = 1, 
       bg = "black", 
       border = "black", 
       col = "black", 
       #wrap = c(-180, 180), 
       add = TRUE)
  #drawPalette(zlim = zlim, col  = pal, plot = plot, fullpage = TRUE)
}
# hotdata ----------------------------------------------------------------------
# loads hotdata from the web
hotdata = function(path = "..//data//infiles//hotdogs//", 
                   file = "HPLC_HOT.txt",
                   cutdate = "2002-01-01",
                   varname = "chl",
                   id = c("press", "hplc"),
                   depth = NULL){
  
  dtfile <- paste(path, file, sep = "")
  # Load the HPLC Data text file from HOT_DOGS webserver
  dt <- data.frame(fread(dtfile, 
                         header = TRUE),
                   stringsAsFactors = FALSE)
  
  dt <- data.frame(fread(dtfile, 
                         colClasses = c(rep("character", ncol(dt))), 
                         header = TRUE),
                   stringsAsFactors = FALSE)
  date <- as.POSIXct(dt$date, format = "%m%d%y")
  
  dt   <- data.frame(apply(dt[, id], 
                           MARGIN = 2, 
                           FUN = as.numeric))
  dt      <- cbind(dt, date) # create a dataframe
  colnames(dt) <- c(id, "time")
  dt$time <- as.Date(dt$time) # that Postix is now a date class
  if(is.numeric(depth)) dt <- subset(dt, press < depth)# subset upper 5m
  ind     <- which(dt$time > as.Date(cutdate))
  dt      <- dt[ind,] 
  dt
}

# ------------------------------------------------------------------------------
# creates a color vector for ploting that has an alpha value
colvect <- function(x = c("white", "lightblue", "coral3", "purple"), 
                    alpha = 0.5) {
  cols <- vector()
  temp <- col2rgb(x)
  for (i in 1:ncol(temp)) {
    if (length(alpha) > 1) {
      cols[i] <- rgb(temp[1,i], temp[2,i], temp[3,i], 
                     alpha = alpha[i] * 255, 
                     max = 255)
    } else {
      cols[i] <- rgb(temp[1,i], temp[2,i], temp[3,i], 
                     alpha = alpha * 255, 
                     max = 255)
    }
  }
  cols
}

# ------------------------------------------------------------------------------
shadedline = function(x, y1, y2, y0 = NULL,
                      ylim = c(min(y1, na.rm = TRUE), max(y2, na.rm = TRUE)),
                      xlim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)),
                      ylab = "",
                      xlab = "",
                      line = -1.5,
                      adj = 0.05,
                      cex.main = 1.25,
                      main = "CHL Climatology",
                      col = "darkseagreen",
                      labels = format(x, "%b"),
                      xaxis = TRUE,
                      yaxis = TRUE,
                      mar = c(3, 3, 3, 3),
                      title = "") {
  
  mycolor <- colvect(x = col, alpha = 0.5)
  gridcol <- colvect(x = "gray", alpha = 0.6)
  m <- as.Date(x)
  
  par(mar=mar)
  plot(x, y1, 
       type = "l",
       ylim = ylim, 
       xlim = xlim,
       ylab = "",
       xlab = "",
       axes = FALSE)
  lines(x, y2, type = "l", col = 2)
  if(yaxis) axis(side = 2,
                 las = 2, 
                 lwd = 2, 
                 mgp = c(1, 0.75, 0), 
                 cex.axis = 1.15)
  if(xaxis) axis(side = 1, 
                 at = as.numeric(m)[seq(from = 1, to = length(m), by = 2)],
                 labels = format(m, "%b")[seq(from = 1, to = length(m), by = 2)],
                 las = 1, 
                 lwd = 2, 
                 mgp = c(2, 1, 0), 
                 cex.axis = 1.15)
  grid(nx = NULL, 
       ny = NULL,
       lty = 1,      # Grid line type
       col = gridcol, # Grid line color
       lwd = 1)      # Grid line width
  box(which = "plot", lty = "solid", lwd = 3, col = "grey22")
  title(main = list(main, cex = cex.main),
        line= line,
        adj = adj)
  title(main = list(title, cex = cex.main),
        line= 0.7,
        adj = 0)
  title(ylab = ylab, 
        cex.lab = 1.25,
        line = 3.5)
  title(xlab = xlab, 
        cex.lab = 1.25,
        line = 2.5)
  # Fill area between lines
  polygon(c(x, rev(x)), c(y2, rev(y1)),
          col = mycolor, lty = 0)
  
  # Redraw the lines
  lines(x, y0, col = "grey22", lty = 1, lwd = 2)
  lines(x, y1, col = col, lwd = 1)
  lines(x, y2, col = col, lwd = 1)
}

# load_edds --------------------------------------------------------------------
# 
# load_edds = function(
#   path = "..\\data\\eddies\\",
#   file = "Eddy_trajectory_nrt_3.2exp_cyclonic_20180101_20220118.nc",
#   sdate = as.Date("2018-10-02"),
#   edate = as.Date("2018-10-02") + 10,
#   domain = extent(-158, -130, 23, 35)){
#   
#   nc_data <- nc_open(paste(path, file, sep = ""))
#   # variables <- names(nc_data$var)
#   slat = ncvar_get(nc_data, "effective_contour_latitude")
#   
#   slon = ncvar_get(nc_data, "effective_contour_longitude")
#   
#   time = ncvar_get(nc_data, "time")
#   
#   age  = ncvar_get(nc_data, "observation_number")
#   
#   speed = ncvar_get(nc_data, "speed_average")
#   
#   track = ncvar_get(nc_data, "track")
#   
#   # names(nc_data$var)
#   
#   nc_close(nc_data)
#   
#   time  = unlist(time)
#   time  = as.Date(time, origin = as.Date("1950-01-01"))
#   track = unlist(track)
#   age   = unlist(age)
#   
#   #ind  <- which((time >= sdate) & (time <= edate))
#   eddies = list()
#   uniq_track = unique(track)
# 
#   j = 0
#   #for(i in seq(sdate, edate, by = 1)) {
#   for(i in uniq_track) {
#     j = j + 1
#     
#     ind = which(track == i)
#     
#     ed_age = age[ind] # index for all the eddies that day
#     ed_id  = track[ind]
#     ed_date = time[ind]
#     # We do this because lats are not in a standard vector
#     lat = slat[, ind]
#     lon = slon[, ind]
#     
#     #id  = sort(rep(1:dim(lat)[2], dim(lat)[1]))
#     lat = c(lat)
#     lon = c(lon)
#     lon = lon - 360
#     
#     # edds <- data.frame(time = as.Date(rep(i, length(lat))), lat, lon, id, cy, flag)
#     edds <- data.frame(time = ed_date, lat, lon, ed_id, ed_age)
#     
#     #subset by domain
#     domain <- extent(domain)
#     edds <- subset(edds, lon > domain[1] & lon < domain[2])
#     edds <- subset(edds, lat > domain[3] & lat < domain[4])
#     eddies[[j]] = edds 
#     }
#   do.call(rbind, eddies)
#   }

# load_edds PARELLEL -----------------------------------------------------------
load_edds = function(
    path = "..\\data\\eddies\\",
    file = "Eddy_trajectory_nrt_3.2exp_cyclonic_20180101_20220118.nc",
    sdate = as.Date("2018-10-02"),
    edate = as.Date("2018-10-02") + 10,
    domain = extent(-158, -130, 23, 35)){
  
  nc_data <- nc_open(paste(path, file, sep = ""))
  # variables <- names(nc_data$var)
  lat = ncvar_get(nc_data, "latitude")
  lon = ncvar_get(nc_data, "longitude")
  time = ncvar_get(nc_data, "time")
  age  = ncvar_get(nc_data, "observation_number")
  track = ncvar_get(nc_data, "track")
  
  clon = ncvar_get(nc_data, "effective_contour_longitude")
  clat = ncvar_get(nc_data, "effective_contour_latitude")
  
  names(nc_data$var)
  
  nc_close(nc_data)
  
  time  = unlist(time)
  track = unlist(track)
  age   = unlist(age)
  lat   = unlist(lat)
  lon   = unlist(lon)
  
  clat = c(clat)
  clon = c(clon)
  
  lon = rep(lon, each = 20)
  lat = rep(lat, each = 20)
  time = rep(time, each = 20)
  age = rep(age, each = 20)
  track = rep(track, each = 20)
  
  time  = as.Date(time, origin = as.Date("1950-01-01"))
  age   = as.numeric(age)
  track = as.numeric(track)
  lon   = lon - 360
  clon  = clon - 360
  
  edds = data.frame(time, lat, lon,  clat, clon, track, age)
  domain = extent(domain)
  edds = subset(edds, lon > domain[1] & lon < domain[2])
  edds = subset(edds, lat > domain[3] & lat < domain[4])
  edds
  }

# boxit ------------------------------------------------------------------------
boxit <- function(x1, x2, y, data,
                  xaxes = TRUE, 
                  yaxes = TRUE,
                  col=c("slateblue1" , "tomato"),
                  ylim = c(min(y, na.rm = TRUE), max(y, na.rm = TRUE)),
                  sub = "Car Milage Data",
                  xlab = "", 
                  main = "",
                  ylab = "Miles Per Gallon",
                  legend = c("ALOHA", "30N"),
                  add = FALSE,
                  at = 1:24,
                  labels = as.character(1:12),
                  cex.main = 1.5,
                  cex.lab = 1.5,
                  line = -1.5,
                  adj = 0.05,
                  mar = c(5, 5, 5, 3)) {
  par(mar = mar)
  boxplot(y ~ x1*x2, 
          data = box_df,
          ylim = ylim,
          main = main, 
          col=col,
          xlab = "", 
          ylab = "", 
          add = add,
          outline = FALSE, 
          axes = FALSE)
  grid(nx = 6, # X-axis divided in two sections
       ny = 3, # Y-axis divided in three sections
       lty = 2, col = colvect(c("gray69"), alpha = 0.5), lwd = 1)
  legend(x = 20, y = 0.2, legend = legend, pch = 22,
         pt.bg = col)
  if(yaxes) axis(side = 2,
                 las = 2, 
                 lwd = 2, 
                 mgp = c(1, 0.75, 0), 
                 cex.axis = 1.5)
  if(xaxes) axis(side = 1, 
                 las = 1, 
                 lwd = 2, 
                 at = at,
                 labels = labels, 
                 mgp = c(2, 1, 0), 
                 cex.axis = 1.5)
  box(which = "plot", lty = "solid", lwd = 3, col = "grey25")
  title(main = list(sub, cex = cex.main),
        line= line,
        adj = adj)
  title(ylab = ylab, 
        cex.lab = cex.lab,
        line = 3.5)
  title(xlab = xlab, 
        cex.lab = cex.lab,
        line = 2.5)
}



# ------------------------------------------------------------------------------
plotit <- function(x, y, 
                   asp = 1,
                   abline = c(m, b, r),
                   tex_x = 0.15,
                   tex_y = 0.05,
                   xaxes = TRUE, 
                   yaxes = TRUE,
                   ylim = c(min(y, na.rm = TRUE), 
                            max(y, na.rm = TRUE)),
                   xlim = c(min(x, na.rm = TRUE), 
                            max(x, na.rm = TRUE)),
                   sub = "Car Milage Data",
                   xlab = "", 
                   main = "",
                   pch = 19,
                   ylab = "Miles Per Gallon",
                   cex.main = 1.5,
                   cex.lab = 1.5,
                   line = -1.5,
                   adj = 0.05,
                   mar = c(5, 5, 5, 3)) {
  
  # This is for a linear regression
  xm <- seq(xlim[1]-xlim[1]*2, xlim[2]+xlim[2], length = 100)
  ym <- xm * abline[1] + abline[2]
  
  par(mar = mar)
  #plot.new()
  plot(1,
       asp = asp,
       ylim = ylim,
       xlim = xlim,
       main = main,
       xlab = "",
       ylab = "",
       axes = FALSE)
  #text(0.19, 0.02, pos = 1,
  #     labels = paste('r =', as.character(abline[3]), sep = " "))
  text(tex_x, tex_y, pos = 1,
       labels = paste('y =', 
                      as.character(abline[1]), "* x",
                      "+",
                      as.character(abline[2]), 
                      sep = " "))
  grid(nx = NULL, # X-axis divided in two sections
       ny = NULL, # Y-axis divided in three sections
       lty = 2, col = colvect(c("gray69"), alpha = 0.5), lwd = 1)
  points(x, y, pch = pch)
  lines(xm, ym, lwd = 2,  col = "grey44")
  if(yaxes) axis(side = 2,
                 las = 2, 
                 lwd = 2, 
                 mgp = c(1, 0.75, 0), 
                 cex.axis = 1.5)
  if(xaxes) axis(side = 1, 
                 las = 1, 
                 lwd = 2, 
                 mgp = c(2, 1, 0), 
                 cex.axis = 1.5)
  box(which = "plot", lty = "solid", lwd = 3, col = "grey25")
  title(main = list(sub, cex = cex.main),
        line= line,
        adj = adj)
  title(ylab = ylab, 
        cex.lab = cex.lab,
        line = 3.5)
  title(xlab = xlab, 
        cex.lab = cex.lab,
        line = 2.5)
}

# loadchl ----------------------------------------------------------------------
# load data from an erddap server
loadchl <- function(url   = "https://upwell.pfeg.noaa.gov/erddap",
                    id = "erdMH1chla8day",
                    parameter = c("chlorophyll", "time", "latitude", "longitude"),
                    sdate = as.Date("2018-05-01"),
                    edate = as.Date("2022-04-11"),
                    lat   = c(20, 35),
                    lon   = c(-170, -135),
                    large = FALSE,
                    by    = 365){
  # where all the large cache files go
  # C:\Users\james\AppData\Local\cache\R\rerddap
  # C:/Users/james/AppData/Local/Temp/RtmpUzoLlB/raster
  data_info <- rerddap::info(id, url = url)
  
  down_chl <- function(x,
                       data_info = data_info,
                       longitude = lon,
                       latitude  = lat,
                       fields    = parameter,
                       url       = url_base){
    griddap(data_info,
            longitude = lon,
            latitude  = lat,
            time   = x,
            fields = parameter,
            url    = url_base)
  }
  
  if(large == TRUE){
    # create a list of one year sequences 
    y <- year(sdate):year(edate)
    starts <- seq(from = sdate, to = edate, by = by)
    starts[length(starts)+1] <- edate 
    starts <- paste(as.character(starts), 
                    "T00:00:00Z", 
                    sep = "")
    
    times <- list()
    for(i in 2:length(starts)) times[[i-1]] <- c(starts[i-1], starts[i])
    
    # Windows can paralleling this but could do it with a for loop
    data <- lapply(times, down_chl)
    data <- lapply(data, function(x) nc_open(x$summary$filename))
    ras  <- lapply(data, ncvar_get)
    lat  <- lapply(data, ncvar_get, varid = "latitude")
    lon  <- lapply(data, ncvar_get, varid = "longitude")
    time <- lapply(data, ncvar_get, varid = "time")
    #chl <- lapply(data, ncvar_get, varid = "chlorophyll")
    lapply(data, nc_close)
    time <- lapply(time, function(x) as.Date(as.POSIXct(x, origin = "1970-01-01")))
    
    ras <- lapply(ras, 
                  function(x) {
                    if(is.na(dim(x)[3])) dim(x) <- c(dim(x)[1], dim(x)[2], 1)
                    brick(x)
                  })
    
    time <- as.Date(unlist(time))
    idx <- !duplicated(time)
    ras <- brick(ras)
    ras <- subset(ras, which(idx))
    ras <- setZ(ras, z = time[idx], name = "time")
    extent(ras) <- extent(min(lon[[1]]), max(lon[[1]]), min(lat[[1]]), max(lat[[1]]))
  } else {
    t <- paste(as.character(c(sdate, edate)), 
               c("T00:00:00Z", "T00:00:00Z"), 
               sep = "")
    
    data_info <- rerddap::info(id, url = url)
    data <- griddap(data_info, 
                    longitude = lon, 
                    latitude = lat, 
                    time = t, 
                    fields = parameter, 
                    url = url_base
                    )
    
    data <- nc_open(data$summary$filename)
    ras  <- ncvar_get(data)
    lats <- ncvar_get(data, varid = "latitude")
    lons <- ncvar_get(data, varid = "longitude")
    time <- ncvar_get(data, varid = "time")
    attr <- ncatt_get(data, varid = 0)
    nc_close(data)
    rm(data)
    
    time <- as.Date(as.POSIXct(time, origin = "1970-01-01"))
    
    ras <- brick(ras) 
    ras <- setZ(ras, z = time, name = "time")
    extent(ras) <- extent(min(lons), max(lons), min(lats), max(lats))
    }
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #print("remember to clear cache Users james AppData Local cache R rerddap")
  ras
}

# loadsat ----------------------------------------------------------------------
# load data from an erddap server
loadsat = function(url   = "https://upwell.pfeg.noaa.gov/erddap",
                    id = "erdMH1chla8day",
                    parameter = c("chlorophyll", "time", "latitude", "longitude"),
                    sdate = as.Date("2018-05-01"),
                    edate = as.Date("2022-04-11"),
                    lat   = c(20, 35),
                    lon   = c(-170, -135),
                    large = FALSE,
                    by    = 10){
  # where all the large cache files go
  # C:\Users\james\AppData\Local\cache\R\rerddap
  # C:/Users/james/AppData/Local/Temp/RtmpUzoLlB/raster
  
  # https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3ASCIDINEOFDaily.nc?chlor_a%5B(2018-05-29T12:00:00Z):1:(2018-12-21T12:00:00Z)%5D%5B(0):1:(0)%5D%5B(34.041664):1:(16.041664)%5D%5B(-179.95833):1:(-129.95833)%5D
  data_info = rerddap::info(id, url = url)
  down_chl = function(window,
                       data_info = data_info,
                       longitude = lon, 
                       latitude  = lat, 
                       fields    = parameter, 
                       url       = url_base){
    griddap(x = data_info, 
            longitude = lon, 
            latitude  = lat, 
            time   = window, 
            fields = parameter, 
            url    = url_base)
  }
  
  if(large == TRUE){
    # create a list of one year sequences 
    y <- year(sdate):year(edate)
    starts <- seq(from = sdate, to = edate, by = by)
    starts[length(starts)+1] <- edate 
    starts <- paste(as.character(starts), 
                    "T00:00:00Z", 
                    sep = "")
    
    times <- list()
    for(i in 2:length(starts)) times[[i-1]] <- c(starts[i-1], starts[i])
    
    blocks = list()
    for(i in 1:length(times)){
      blocks[[i]] = down_chl(window = times[[i]],
                             data_info = data_info,
                             longitude = lon, 
                             latitude  = lat, 
                             fields    = parameter, 
                             url       = url_base)
    }
    
    # Windows can paralleling this but could do it with a for loop
    #data <- lapply(times, down_chl)
    
    blocks <- lapply(blocks, function(x) nc_open(x$summary$filename))
    ras  <- lapply(blocks, ncvar_get)
    lat  <- lapply(blocks, ncvar_get, varid = "latitude")
    lon  <- lapply(blocks, ncvar_get, varid = "longitude")
    time <- lapply(blocks, ncvar_get, varid = "time")
    #chl <- lapply(data, ncvar_get, varid = "chlorophyll")
    lapply(blocks, nc_close)
    rm(blocks)
    time <- lapply(time, function(x) as.Date(as.POSIXct(x, origin = "1970-01-01")))
    
    ras <- lapply(ras, 
                  function(x) {
                    if(is.na(dim(x)[3])) dim(x) <- c(dim(x)[1], dim(x)[2], 1)
                    brick(x)
                  })
    
    time <- as.Date(unlist(time))
    idx <- !duplicated(time)
    ras <- brick(ras)
    ras <- subset(ras, which(idx))
    ras <- setZ(ras, z = time[idx], name = "time")
    extent(ras) <- extent(min(lon[[1]]), max(lon[[1]]), min(lat[[1]]), max(lat[[1]]))
  } else {
    window <- paste(as.character(c(sdate, edate)), 
               c("T00:00:00Z", "T00:00:00Z"), 
               sep = "")
    
    data_info <- rerddap::info(id, url = url)
    data <- griddap(data_info, 
                    longitude = lon, 
                    latitude = lat, 
                    time = window, 
                    fields = parameter, 
                    url = url_base
    )
    
    data <- nc_open(data$summary$filename)
    ras  <- ncvar_get(data)
    lats <- ncvar_get(data, varid = "latitude")
    lons <- ncvar_get(data, varid = "longitude")
    time <- ncvar_get(data, varid = "time")
    attr <- ncatt_get(data, varid = 0)
    nc_close(data)
    rm(data)
    
    time <- as.Date(as.POSIXct(time, origin = "1970-01-01"))
    
    ras <- brick(ras) 
    ras <- setZ(ras, z = time, name = "time")
    extent(ras) <- extent(min(lons), max(lons), min(lats), max(lats))
  }
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  #print("remember to clear cache Users james AppData Local cache R rerddap")
  ras
}

# ------------------------------------------------------------------------------
# This function will requir three data inputs (x, y, z) and one units input 
# (grid spacing). Possibly a padding input.
# Dates should be a numeric type
# x, y, and z should be individual nemeric vectors
# use the range of x, and y to determine the x-yunits
# units = 0.005

loadfloat <- function(fields =  c("temp",
                                  "temp_qc",
                                  "temp_adjusted", 
                                  "chla",
                                  "doxy", 
                                  "psal", 
                                  "psal_adjusted", 
                                  "pres", 
                                  "pres_adjusted", 
                                  "time", 
                                  "float_serial_no",
                                  "cycle_number", 
                                  "latitude", 
                                  "longitude",
                                  "platform_number"), 
                      sdate = "2018-07-01", 
                      edate = "2018-10-01", 
                      lons = c(-160, -145), 
                      lats = c(28, 32),
                      base_url = "https://erddap.ifremer.fr/erddap",
                      id   = "ArgoFloats-synthetic-BGC") {
  
  sdate <- paste("time>=", as.character(sdate), "T00:00:00Z", sep = "")
  edate <- paste("time<=", as.character(edate), "T00:00:00Z", sep = "")
  slon <- paste("longitude>=", lons[1], sep = "")
  elon <- paste("longitude<=", lons[2], sep = "")
  slat <- paste("latitude>=", lats[1], sep = "")
  elat <- paste("latitude<=", lats[2], sep = "")
  
  data_info <- rerddap::info(id, url = base_url)
  
  floats <- tabledap(x = data_info, 
                     fields = fields, 
                     url = base_url,
                     sdate, 
                     edate,
                     slon, 
                     elon,
                     slat, 
                     elat)
  floats      <- data.frame(floats)
  floats 
}

# ------------------------------------------------------------------------------
tsect <- function(x, y, z, xreach = 1, yreach = 1, xlen = 120, ylen = 40){
  #make a max min vector of sla and fsle
  coord = c(max(x, na.rm = TRUE), min(x, na.rm = TRUE), 
            max(y, na.rm = TRUE), min(y, na.rm = TRUE))
  
  # use maxmin vector to create fake x, y vectors (downsized) as meshgrid input
  y_vec <- seq(from = coord[4], to = coord[3], length.out = ylen) 
  # may need to invert to be same length 
  x_vec <- seq(from = coord[2], to = coord[1], length.out = xlen)
  xy_grid <- meshgrid(x_vec, y_vec)
  
  #inisilize a matrix of dim fs_grid[1] filled with NA values
  dims <- dim(xy_grid[[1]])
  z_grid <- matrix(data = NA, 
                   nrow = dims[1], 
                   ncol = dims[2], 
                   dimnames = NULL)
  
  for(iy in 1:dims[1]) {
    for(ix in 1:dims[2]){
      # where in the df is the difference greater than the units
      box_x <- which(abs(x - xy_grid[[1]][iy, ix]) <= xreach)
      box_y <- which(abs(y - xy_grid[[2]][iy, ix]) <= yreach)
      # I think the grids are dif sizes and should be subet differently
      #index vector of both cox_sla and box_fsle as one
      #box <- sort(match(box_y, box_x))
      
      # I think this is the correct way to do this
      box <- box_x[box_x %in% box_y]
      
      z_grid[iy, ix] <- mean(z[box], na.rm = TRUE)
    }
  }
  
  list(z_grid, y_vec, x_vec)
}
# mldchu -----------------------------------------------------------------------
# Method for determining the mixed layer by Chu and Fan (2010)
# ctd is a data frame with at least a "pressure/pres" column and variable column
# n is the length of the running window
# variable is the dependent variable that determines the mixed layer
# output is the row of the ctd data set that the mixed layer occurs at 
mldchu = function(ctd, n = 5, y = "pres", x = "temperature"){
  pressure = ctd[[y]]
  x = ctd[[x]]
  ndata = length(pressure)
  E1 = rep(NA, ndata)
  E2 = E1
  E2overE1 = E2
  kstart = min(n,3)
  for (k in seq(kstart, ndata-n,1)){
    above = seq.int(1,k)
    below = seq.int(k+1, k+n)
    fit = lm(x~pressure, subset = above)
    E1[k] = sd(predict(fit) - x[above])
    pBelow = data.frame(pressure = pressure[below])
    E2[k] = abs(mean(predict(fit, newdata = pBelow) -x[below]))
    E2overE1[k] = E2[k] / E1[k]
  }
  MLDindex = which.max(E2overE1)
  return(ctd[MLDindex,])
  # MLDindex = MLDindex, #E1 = E1, #E2 = E2
}

# ------------------------------------------------------------------------------
scale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

# anom <- function(x) x - mean(x, na.rm = TRUE)

# ------------------------------------------------------------------------------
# This is all to calculate the mixed layer depth. Not sure if it's needed
# The mixed layer depth (MLD) is detected following de Boyer Montegut et al 
# (2004): a reference value for density is taken near the surface and the water 
# column is considered to be mixed until the depth at which density deviates 
# from this reference by more than 0.03 kg/m^3. Optionally, when no depth 
# satisfies this criterion, a second criterion of 0.01 kg/m^3 can be considered 
# (as is by default). 
# In addition, here, when a range of depths is provided as reference, the 
# reference density is the average of the densities recorded in this depth range
# x:             vector of the variable of interest, usually potential or in situ density.
# depth:         vector of depths at which x is measured.
# ref.depths:    depth(s) of reference, near the surface; when ref.depths is a vector, the value of x is averaged between those depths.
# criteria:      value(s) considered as thresholds for the computation of the depth of the mixed layer. The thresholds are tried successively.
# default.depth: when no threshold is crossed, return this value instead; a usual value is the full depth of the water column (or max(depth)).
# n.smooth:      integer, number of times to smooth the data before applying the mixed layer criteria.
# k:             order of the moving average used for smoothing; the window size is 2k+1. NB: when data is smoothed, it should have been recorded at approximately regular intervals for the moving average to make sense

slide <- function(x, k, fun, n=1, ...) {
  # make sure to get a function as the `fun` argument (i.e. avoid name masking)
  if (!is.function(fun)) {
    fun <- get(as.character(substitute(fun)), mode="function")
  }
  
  if (n>=1) {
    # repeat n times
    for (t in 1:n) {
      # pad the extremities of data to be able to compute over the whole vector
      x <- c(rep(NA, times=k), x, rep(NA, times=k))
      
      # apply the rolling function (and remove padding at the extremities)
      x <- sapply((k+1):(length(x)-k), function(i) {
        fun(x[(i-k):(i+k)], ...)
      })
    }
  }
  
  return(x)
}

get_depth <- function(i, depth) {
  if (length(i) > 0) {
    if (!is.null(depth)) {
      i <- depth[i]
    }
  } else {
    i <- NA
  }
  return(i)
}

smooth <- function(x, k=1, n=1) {
  # compute centered weights
  w <- c(1:k,k+1,k:1)
  w <- w / sum(w)
  # compute the (running) weighted moving average
  slide(x, k=k, stats::weighted.mean, na.rm=TRUE, w=w, n=n)
}

check_input <- function(x, depth=NULL) {
  ok <- TRUE
  # check the input
  if (all(is.na(x))) {
    ok <- FALSE
  }
  if (!is.null(depth)) {
    if (length(depth) != length(x)) {
      ok <- FALSE
      stop("The vector of data (n=", length(x), ") should be as long as the vector of depths (n=", length(depth), ")")
    }
  }
  return(ok)
}

mld <- function(x, depth, 
                ref.depths = 5:10, 
                criteria = c(0.03, 0.01), 
                default.depth = NA, 
                n.smooth = 0, 
                k = 2) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }
  
  # smooth the profile (if requested)
  x <- smooth(x, k = k, n = n.smooth)
  
  # compute the reference value
  iref <- which(depth >= min(ref.depths) & depth <= max(ref.depths))
  xref <- mean(x[iref], na.rm = TRUE)
  if (is.na(xref)) {
    warning("No data at reference depth(s).")
    m <- NA
  } else {
    for (crit in criteria) {
      i <- which(x > (xref + crit) & depth > max(ref.depths)) - 1
      # NB: we want the element previous to meeting this criterion
      if (length(i) > 0) {
        i <- min(i)
        break
      }
    }
    
    # extract the corresponding depth
    m <- get_depth(i, depth)
    
    # replace by the default value when no criterion is met
    if (is.na(m)) {
      m <- default.depth
    }
  }
  return(m)
}

# ------------------------------------------------------------------------------
jamie_theme <- function(x,
                        y, 
                        #asp = 1,
                        adj = 0,
                        ylim = range(y, na.rm = TRUE),
                        xlim = range(x, na.rm = TRUE),
                        line = 0.75,
                        main = "",
                        # labels = seq(from = min(x)+5, to   = max(x)-5, by   = 15),
                        labels = round(seq(range(x)[1]+2, range(x)[2]-2, length = 5), 1),
                        at =  round(seq(range(x)[1]+2, range(x)[2]-2, length = 5), 1),
                        dt = TRUE,
                        yaxes = FALSE,
                        xaxes = FALSE,
                        ylab = "",
                        xlab = "",
                        gridd = TRUE,
                        mar = c(1,1,1,1)
){
  
  if(dt == TRUE) labels <- format(as.Date(labels), "%b-%d")
  if(is.numeric(x)) x <- round(x, 1)
  if(is.numeric(y)) y <- round(y, 1)
  par(mar = mar)
  plot(0,
       ylim = ylim,
       xlim = xlim,
       main = "",
       xlab = "",
       ylab = "",
       axes = FALSE,
       xaxs="i",
       yaxs="i"
       #asp = asp,
       )
  if(gridd == TRUE) {
    grid(nx = NULL, # X-axis divided in two sections
         ny = NULL, # Y-axis divided in three sections
         lty = 2, 
         col = colvect(c("gray69"), alpha = 0.5), lwd = 1)
    }
  box(which = "plot", 
      lty = "solid", 
      lwd = 3, 
      col = colvect("grey22", alpha = 0.9))
  windowsFonts(script=windowsFont("Script MT Bold"))
  if(xaxes) {
    axis(side = 1,
    at = at,
    labels = labels,
    las = 1, 
    lwd = 2, 
    mgp = c(2, 1, 0), 
    cex.axis = 1.25,
    col = colvect("grey22", alpha = 0.9))}
  if(yaxes){
    axis(side = 2,
    las  = 2, 
    lwd  = 2, 
    mgp  = c(1, 0.75, 0), 
    cex.axis = 1.25,
    col = colvect("grey22", alpha = 0.9))}
  title(main = main,
        cex.main = 1.75,
        line = line,
        adj = adj)
  title(ylab = ylab, cex.lab = 1.5, line = 3)
  title(xlab = xlab, cex.lab = 1.5, line = 2.5)
}

# anomalize --------------------------------------------------------------------
anomalize = function(ras, detrend = FALSE, f = 0.6){
  # find the monthly climotology of the data set 
  ras_clim = bloomclim(ras)
  
  # subtract each month from corresponding daily data set
  ogt      = getZ(ras)
  ogt      = anydate(ogt)
  # I should be able to do this with week of year or day of year onj a larger
  # dataset
  mon_raw  = month(ogt)
  #mon_clim = month(getZ(ras_clim))
  
  # subtract each day from corresponding daily data set
  #day_raw  = day(getZ(ras))
  #day_clim = day(getZ(ras_clim))
  
  s     = dim(ras_clim)
  chla  = stack() #initialize an empty raster stack
  t     = vector()
  
  for (mon in 1:s[3]) {
    ind  = which(mon_raw == mon)
    z    = ogt[ind]
    temp = ras[[ind]] - ras_clim[[mon]]
    chla = addLayer(chla, temp)
    t    = c(t, z)
  }
  
  rm(temp, z)
  
  chla = setZ(chla, t, name = "time")
  chla = chla[[order(t)]]
  t    = getZ(chla)
  chla = setZ(chla, anydate(getZ(chla)), name = "time")
  
  if(detrend == TRUE) {
    clim = smooth.time.series(chla, f = f, smooth.data = TRUE)
    chla = chla - clim
    chla = setZ(chla, z = anydate(t), name = "time")
    }
  chla
}
# anom -------------------------------------------------------------------------
# x is a raster with a time atribute
# output is  araster missing leap days
# will likely take 365/12 times longer than the other function 
# x is a raster with a time atribute
# output is  araster missing leap days
# will likely take 365/12 times longer than the other function 
anom = function(x, detrend = FALSE){
  t = getZ(x)
  y = year(t)
  m = month(t) 
  d = yday(t)
  
  lyears = y %% 4 == 0
  
  idx = which(!(lyears & d == 60))
  
  # removing teh leap days
  x = x[[idx]]
  t = t[idx]
  d = d[idx]
  lyears = lyears[idx]
  
  # creating a day of year to do climitology
  idx = which(lyears & (d > 60))
  d[idx] = d[idx] - 1
  
  # just make a seq of 1:365 as the doy from then on
  anomaly = stack()
  time = vector()
  for (i in 1:365) {
    idx = which(d == i)
    z = subset(x, idx)
    t = getZ(z)
    z = calc(z, median, na.rm = TRUE)
    z = x[[idx]] - z
    
    anomaly = addLayer(anomaly, stack(z))
    time = c(time, t)
  }
  rm(z, t, idx, x)
  
  # re-order amonlay by time
  idx = order(time)
  time = as.Date(time[idx])
  anomaly = anomaly[[idx]]
  
  # set the time atribute
  anomaly = setZ(anomaly, z = time, name = "time") 
  
  if(detrend == TRUE) {
    clim = smooth.time.series(anomaly, f = 0.6, smooth.data = TRUE)
    anomaly = anomaly - clim
    anomaly = setZ(anomaly, z = time, name = "time")
  }
  anomaly
}

# polymask ---------------------------------------------------------------------
polymask <- function(ras, 
                     lon_coord = c(-170, -152, -157, -162, -170, -170), 
                     lat_coord = c(  20,   20,   22,  24.5, 28, 20)){
  t   <- getZ(ras)
  ras <- raster::flip(ras, direction = "x")
  ras <- raster::flip(ras, direction = "y")
  
  Sr1 <- Polygon(cbind(lon_coord, lat_coord))
  
  spp <- SpatialPolygons(list(Polygons(list(Sr1), "s1")))
  spp_ras <- raster::rasterize(spp, ras, getCover = TRUE)
  spp_ras[spp_ras == 1] <- NA
  
  ras <- raster::mask(ras, spp_ras)
  
  # put it back in reverse order
  ras <- raster::flip(ras, direction = "y")
  ras <- raster::flip(ras, direction = "x")
  
  ras <- setZ(ras, z = t, name = "time")
  ras
}



# laodsat for opendap ----------------------------------------------------------
# DEPRECIATED
# loadsat <- function(var = "sla",
#                     url,
#                     sdate = as.Date("2018-05-01"), 
#                     edate = as.Date("2018-11-01"), 
#                     lons = c(190-360, 230-360), 
#                     lats = c(25, 32),
#                     org = "1900-01-01",
#                     lat_varid = "lat",
#                     lon_varid = "lon",
#                     by = 365
#                     ){
#   
#   data = nc_open(url, verbose = FALSE, write = FALSE)
#   lat  = ncvar_get(data, varid = lat_varid)
#   lon  = ncvar_get(data, varid = lon_varid)
#   time = ncvar_get(data, varid = "time")
#   # time is in a crazy format. Maybe seperat loadchl function? Geting harry
#   # seconds since 1970-01-01 00:00:00
#   
#   if(var == "CHL") {
#     #time = as.POSIXct(time, origin = origin)
#     time = as.Date(time, origin = org)
#     #time = anydate(time)
#   }
#   
#   #else time <- as.Date(time)
#   # create a list of one year sequences 
#   y <- year(sdate):year(edate)
#   starts <- seq(from = sdate, to = edate, by = by)
#   starts[length(starts)+1] <- edate #
#   times <- list()
#   for(i in 2:length(starts)) times[[i-1]] <- c(starts[i-1], starts[i])
#   times = lapply(times, function(x) data.frame(sdate = x[[1]], edate = x[[2]]))
#   
#   # writing this function to apply it to each one year date span for download
#   down_sat = function(x, data, lat, lon, time){
#     sdate = x$sdate
#     edate = x$edate
#     
#     #lon[lon < 0] = lon[lon < 0] + 360
#     #lons[lons < 0] = lons[lons < 0] + 360
#     
#     # I need to convert lons to 0-360 for both
#     idx_lat <- which(lat > lats[1] & lat < lats[2])
#     idx_lon <- which(lon > lons[1] & lon < lons[2])
#     idx_time <- which(time >= sdate & time <= edate)
#     
#     idx_sla <- paste(var,
#                      paste("[", range(idx_time)[1], ":1:", range(idx_time)[2], "]", sep = ""), 
#                      paste("[", range(idx_lat)[1],  ":1:", range(idx_lat)[2],  "]", sep = ""),
#                      paste("[", range(idx_lon)[1],  ":1:", range(idx_lon)[2],  "]", sep = ""),
#                      sep = "")
#     
#     idx_time <- paste("time",      paste("[", range(idx_time)[1], ":1:",range(idx_time)[2], "]", sep = ""), sep = "")
#     idx_lat  <- paste(lat_varid,  paste("[", range(idx_lat)[1],  ":1:",range(idx_lat)[2],  "]", sep = ""), sep = "")
#     idx_lon  <- paste(lon_varid, paste("[", range(idx_lon)[1],  ":1:",range(idx_lon)[2],  "]", sep = ""), sep = "")
#     idx <- paste(idx_lat, idx_lon, idx_time, idx_sla, sep = ",")
#     
#     url <- paste(url, idx, sep = "")
#     
#     data = nc_open(url, verbose = FALSE, write = FALSE)
#     ras  = ncvar_get(data)
#     lats = ncvar_get(data, varid = lat_varid)
#     lons = ncvar_get(data, varid = lon_varid)
#     time = ncvar_get(data, varid = "time")
#     attr = ncatt_get(data, varid = 0)
#     nc_close(data)
#     rm(data)
#     
#     s = dim(ras)
#     if(length(s) == 2) ras = raster(ras)
#     else ras = brick(ras) #may want raster() here 
#     ras = setZ(ras, z = as.Date(time, origin = org), name = "time")
#     #ras = setZ(ras, z = as.Date(getZ(ras), origin = "1900-01-01"), name = "time")
#     extent(ras) = extent(min(lons), max(lons), min(lats), max(lats))
#     #crs(ras) <- idk
#     ras
#   }
#   
#   ras = lapply(times, FUN = down_sat, data = data, lat= lat, lon=lon, time=time)
#   
#   # idk why thisgoes so 
#   time = lapply(ras, FUN = getZ)
#   time = unlist(time)
#   idx  = duplicated(time)
#   time = time[!idx]
#   
#   ras = do.call(stack, ras)
#   ras = subset(ras, which(!idx))
#   ras = setZ(ras, z = as.Date(time), name = "time")
#   }
# dap ----------------------------------------------------------------------
# load errdapp data function. 
# set up for ssta.
# only good for daily data now, but probs not necissary for monthy data

dap = function(sdate, edate, e, 
               id = 'jplMURSST41anom1day', 
               url = "https://coastwatch.pfeg.noaa.gov/erddap/",
               var = c("sstAnom", "latitude", "longitude", "time")) {
  # input is a datetime and an extent. It grabs that raster
  dap_that_ass = function(x, data_info, e){
    data = griddap(data_info, 
                   latitude = e[3:4], 
                   longitude = e[1:2], 
                   time = c(as.character(x),as.character(x)), 
                   fields = 'all')
    data = nc_open(data$summary$filename)
    ras  = ncvar_get(data, varid = var[1])
    lats = ncvar_get(data, varid = var[2])
    lons = ncvar_get(data, varid = var[3])
    time = ncvar_get(data, varid = var[4])
    time = as.Date(as.POSIXct(time, origin = "1970-01-01"))
    nc_close(data)
    rm(data)
    
    ras = raster(ras)
    extent(ras) = extent(min(lons), max(lons), min(lats), max(lats))
    ras = setZ(ras, z = time, name = "time")
    crs(ras) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ras
  }
  
  time = seq(sdate, edate, by = 1)
  time = as.list(time)
  
  data_info = info(id, 
                   url = url)
  
  ras  = lapply(time, FUN = dap_that_ass, data_info = data_info, e = e)
  time = lapply(ras, getZ)
  time = unlist(time)
  time = as.Date(time)
  ext    = lapply(ras, extent)
  ras  = stack(ras)
  extent(ras) = ext[[1]]
  ras = setZ(ras, z= time, name = "time")
  ras
}

# ------------------------------------------------------------------------------

opendap = function(url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D?",
                   lons = c(-165, -135),
                   lats = c( 17,   35),
                   sdate = as.Date("2018-07-01"),
                   edate = as.Date("2018-11-30"),
                   lat_varid = "lat",
                   lon_varid = "lon",
                   var = "CHL",
                   origin = "1900-01-01"){
  # delete all built up files in cache
  # cache_list()
  e = extent(lons, lats)
  
  cache_delete_all(force = FALSE)
  data = nc_open(url, verbose = FALSE, write = FALSE)
  lat  = ncvar_get(data, varid = lat_varid)
  lon  = ncvar_get(data, varid = lon_varid)
  time = ncvar_get(data, varid = "time")
  time = as.Date(time, origin = origin)
  
  idx_lat  = which(lat > lats[1] & lat < lats[2])
  idx_lon  = which(lon > lons[1] & lon < lons[2])
  idx_time = which(time >= sdate & time <= edate)
  
  idx_ras = paste("CHL",
                  paste("[", range(idx_time)[1], ":1:", range(idx_time)[2], "]", sep = ""),
                  paste("[", range(idx_lat)[1],  ":1:", range(idx_lat)[2],  "]", sep = ""),
                  paste("[", range(idx_lon)[1],  ":1:", range(idx_lon)[2],  "]", sep = ""),
                  sep = "")
  
  idx_time = paste("time", paste("[", range(idx_time)[1], ":1:",range(idx_time)[2], "]", sep = ""), sep = "")
  idx_lat  = paste(lat_varid, paste("[", range(idx_lat)[1], ":1:",range(idx_lat)[2],  "]", sep = ""), sep = "")
  idx_lon  = paste(lon_varid, paste("[", range(idx_lon)[1], ":1:",range(idx_lon)[2],  "]", sep = ""), sep = "")
  idx = paste(idx_lat, idx_lon, idx_time, idx_ras, sep = ",")
  
  url = paste(url, idx, sep = "")
  
  nc_close(data)
  rm(data)
  
  data = nc_open(url, verbose = FALSE, write = FALSE)
  
  lat  = ncvar_get(data, varid = lat_varid)
  lon  = ncvar_get(data, varid = lon_varid)
  time = ncvar_get(data, varid = "time")
  time = as.Date(time, origin = origin)
  ras  = ncvar_get(data)
  nc_close(data)
  
  s   = dim(ras)
  ras = raster::brick(ras)
  ras = t(ras)
  ras = setZ(ras, z = as.Date(time, origin = org), name = "time")
  extent(ras) = extent(min(lons), max(lons), min(lats), max(lats))
  ras
}

# ------------------------------------------------------------------------------
bufcoast = function(ras, 
                    region = "Hawaiian Islands", 
                    path = "../data/USMaritimeLimitsAndBoundariesSHP"){
  path.eez.usa = (path)
  fnam.eez.usa = "USMaritimeLimitsNBoundaries.shp"
  eez.usa = readOGR(dsn = path.eez.usa, layer = file_path_sans_ext(fnam.eez.usa))
  idx = eez.usa$REGION == "Hawaiian Islands"
  hawaii = eez.usa[idx,]
  #idx = hawaii$CZ == 1
  idx = hawaii$TS == 1
  hawaii = hawaii[idx,]
  hawaii = st_as_sf(hawaii)
  hawaii = st_polygonize(hawaii)
  hawaii = as(hawaii, "Spatial")
  
  # t = getZ(ras)
  # e = extent(ras)
  # ras = t(ras)
  # extent(ras) = e
  
  ras = raster::mask(ras, hawaii, inverse = TRUE)
  
  # ras = t(ras)
  # extent(ras) = e
  # ras = setZ(ras, z = t, name = "time")
  # ras
}

# ------------------------------------------------------------------------------
# input is a raster
fronts = function(ras,
                  downsize = 10,
                  I = 2){
  ras  = raster::flip(ras, direction = "x")
  thresh = median(ras, na.rm = TRUE) + mad(ras, na.rm = TRUE)*I
  idx  = which(values(ras) > thresh)
  e    = extent(ras)
  s    = dim(ras)
  lons = seq(from = e[1], to = e[2], length = s[1]) 
  lats = seq(from = e[3], to = e[4], length = s[2]) 
  grid = pracma::meshgrid(lons, lats)
  fronts = data.frame(lons = grid$X[idx], lats = grid$Y[idx], value = ras[idx])
  idx = seq(1, nrow(fronts), downsize)
  fronts = fronts[idx, ]
  fronts
}

# ------------------------------------------------------------------------------
# input is a raster output is a boolian raster of bloom/not bloom
bool = function(x){
  u <- calc(x, fun = median, na.rm = TRUE)
  o <- calc(x, fun = mad, na.rm = TRUE)
  boo <- x > (u + o)
  extent(boo) <- extent(x)
  boo <- setZ(boo, z = getZ(x), name = "time")
  boo
}

# oreant -----------------------------------------------------------------------

oreant = function(ras, flip = NULL, t1 = FALSE, t2 = FALSE){
  e    = extent(ras)
  time = getZ(ras)
  if(t1) ras = raster::t(ras)
  if(!is.null(flip)) ras = raster::flip(ras,  direction = flip)
  if(t2) ras = raster::t(ras)
  extent(ras) = e
  ras = setZ(ras, z = time, name = "time")
  ras
}

# scale ------------------------------------------------------------------------
# plotting the data
scale <- function(x, to, from){   
  (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)) * (to - from) + from
}





