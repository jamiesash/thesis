# USING PREDOWNOADED DATA

# load xyz table ---------------------------------------------------------------
source("functions.R")
source("libraries.R")
rasterOptions(maxmemory = 123e+10)

xyz = read.csv("..\\data\\xyz_2010_2021_20230218.csv")

head(xyz)
gc()

# ------------------------------------------------------------------------------
# plotting the data
scale <- function(x, to, from){   
  (x - min(x))/(max(x)-min(x)) * (to - from) + from
}


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
      # this could be changed to nearest euclidean distance
      box_x <- which(abs(x - xy_grid[[1]][iy, ix]) <= xreach)
      box_y <- which(abs(y - xy_grid[[2]][iy, ix]) <= yreach)
      # I think the grids are dif sizes and should be subet differently
      #index vector of both cox_sla and box_fsle as one
      #box <- sort(match(box_y, box_x))
      
      # I think this is the correct way to do this
      box <- box_x[box_x %in% box_y]
      
      # z_grid[iy, ix] <- mean(z[box], na.rm = TRUE)
      z_grid[iy, ix] <- median(z[box], na.rm = TRUE)
    }
  }
  
  list(z_grid, y_vec, x_vec)
}

# ------------------------------------------------------------------------------
library(Thermimage)

temp = subset(xyz, !is.na(chla))

idx = sample(1:nrow(temp), 100000)
lil = temp[idx,]

lil$fsle = lil$fsle * -1

lil$fsle =  scale(lil$fsle, from = 0, to = 1)
lil$slaa =  scale(lil$slaa, from = 0, to = 1)

# lil$chla = lil$chla - median(lil$chla)

tmat <- tsect(x = lil[,'slaa'], 
              y = lil[,'fsle'], 
              z = lil[,'chla'], 
              xreach = 0.01, 
              yreach = 0.01,
              xlen = 100, 
              ylen = 100)

yvect <- tmat[[2]]
xvect <- tmat[[3]]
# I need to do this better using base R
zmat <- apply(tmat[[1]], 2, rev)

# zmat <- flip.matrix(t(zmat))
zmat = t(zmat)

# ------------------------------------------------------------------------------

col <- colorRampPalette(c("purple3",  "blue", "cyan", "white", "yellow", "orangered", "red3"))(21)

zlim = c(median(lil$chla)-mad(lil$chla)*2, median(lil$chla) + mad(lil$chla)*2)


drawPalette(zlim = zlim,
            col  = col, 
            plot = TRUE,
            pos  = 4) 

par(mar = c(5, 5, 3, 4))
image(xvect, yvect, zmat,
      #ylim = c(max(yvect)-0.01, 0.3),
      #xlim = c(-0.08, 1.08),
      zlim = zlim,
      main = "",
      xlab = "SLA",
      ylab = "FSLE",
      col = col)
box(which = "plot", lty = "solid", lwd = 3, col = "grey25")











