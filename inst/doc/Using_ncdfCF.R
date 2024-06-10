## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----basic_example------------------------------------------------------------
suppressPackageStartupMessages(library(ncdfCF))

# Get a NetCDF file, here hourly data for 2016-01-01 over Rwanda
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")

# Open the file, all metadata is read
ds <- open_ncdf(fn)

# Easy access in understandable format to all the details
ds

# Variables can be accessed through standard list-type extraction syntax
t2m <- ds[["t2m"]]
t2m

# Same with dimensions, but now without first putting the object in a variable
ds[["longitude"]]

# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A variable: list of dimension names

dimnames(ds[["longitude"]]) # A dimension: vector of dimension element values

# Access attributes
attribute(ds[["pev"]], "long_name")

## ----extract------------------------------------------------------------------
# Extract a timeseries for a specific location
ts <- t2m[5, 4, ]
str(ts)

# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)

## ----subset-------------------------------------------------------------------
# Extract a specific region, full time dimension
ts <- subset(t2m, list(X = 29:30, Y = -1:-2))
str(ts)

# Extract specific time slices for a specific region
# Note that the dimensions are specified out of order and using alternative
# specifications: only the extreme values are used.
ts <- subset(t2m, list(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                       X = c(29.6, 28.8),
                       Y = seq(-2, -1, by = 0.05)))
dimnames(ts)

# Same extraction but now with the upper bound included.
ts <- subset(t2m, list(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                       X = c(29.6, 28.8),
                       Y = seq(-2, -1, by = 0.05)),
             rightmost.closed = TRUE)
dimnames(ts)

