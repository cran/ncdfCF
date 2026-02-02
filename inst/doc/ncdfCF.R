## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic_example------------------------------------------------------------
library(ncdfCF)
  
# Get a netCDF file, here hourly data for 2016-01-01 over Rwanda
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
  
# Open the file, all metadata is read
(ds <- open_ncdf(fn))
  
# Variables can be accessed through standard list-type extraction syntax
(t2m <- ds[["t2m"]])
  
# Same with axes, but now without first assigning the object to a symbol
ds[["longitude"]]
  
# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A data variable: list of axis names
  
dimnames(ds[["longitude"]]) # An axis: vector of axis coordinate values
  
# Access attributes
ds[["pev"]]$attribute("long_name")

## ----extract------------------------------------------------------------------
# Extract a time series for a specific location
# As can be seen above, the `t2m` data variable has 3 axes
ts <- t2m[5, 4, ]
str(ts)
  
# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)

## ----subset-------------------------------------------------------------------
# Extract a specific region, full time dimension
(ts <- t2m$subset(X = 29:30, Y = -1:-2))

# Extract specific time slices for a specific region
# Note that the axes are specified out of order and using alternative
# specifications: only the extreme values are used.
(ts <- t2m$subset(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                  X = c(29.6, 28.8),
                  Y = seq(-2, -1, by = 0.05)))

## ----profile------------------------------------------------------------------
rwa <- t2m$profile(longitude = c(30.07, 30.07, 29.74), latitude = c(-1.94, -1.58, -2.60), 
                   .names = c("Kigali", "Byumba", "Butare"), .as_table = TRUE)
head(rwa)
attr(rwa, "value")

## ----transect-----------------------------------------------------------------
(trans29_74 <- t2m$profile(longitude = 29.74, .names = "lon_29_74"))

## ----summarise----------------------------------------------------------------
# Summarising hourly temperature data to calculate the daily maximum temperature
t2m$summarise("tmax", max, "day")

## ----summarise_multi----------------------------------------------------------
# Function to calculate multiple daily stats
# It is good practice to include a `na.rm` argument in all your functions
daily_stats <- function(x, na.rm = TRUE) {
  # x is the vector of values for one day
  minmax <- range(x, na.rm = na.rm)
  diurnal <- minmax[2L] - minmax[1L]
  c(minmax, diurnal)
}

# Call summarise() with your own function
# The `name` argument should have as many names as the function returns results
(stats <- t2m$summarise(c("tmin", "tmax", "diurnal_range"), daily_stats, "day"))

## ----KtoC---------------------------------------------------------------------
tsC <- ts - 273.15
tsC$set_attribute("units", "NC_CHAR", "degrees_Celsius")
tsC

## ----hot----------------------------------------------------------------------
# This produces a "logical" CFVariable: all values are 0 (FALSE) or 1 (TRUE)
tsHot <- tsC > 20
tsHot$set_attribute("units", "NC_CHAR", "1")
tsHot

## ----orienting----------------------------------------------------------------
# Open a file and read the data from a variable into a CFVariable instance
fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
ds <- open_ncdf(fn)
tx <- ds[["tasmax"]]

# Get the raw data as an R array
tx_raw <- tx$raw()
str(tx_raw)

## ----array--------------------------------------------------------------------
tx_array <- tx$array()
str(tx_array)

## ----ggplot-------------------------------------------------------------------
library(ggplot2)
ggplot() + geom_ncdf(data = tx) + coord_equal() + scale_fill_viridis_c()

## ----save, eval=FALSE---------------------------------------------------------
# # Save a CFVariable instance to a netCDF file on disk
# stats[["diurnal_range"]]$save("~/path/file.nc")

## ----export-------------------------------------------------------------------
# install.packages("data.table")
library(data.table)
head(dt <- ts$data.table())

#install.packages("terra")
suppressMessages(library(terra))
(r <- stats[["diurnal_range"]]$terra())

## ----stars--------------------------------------------------------------------
suppressMessages(library(stars))
(st <- st_as_stars(ts))

