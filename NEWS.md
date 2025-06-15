# ncdfCF 0.6.1

- ncdfCF is now hosted on Github through the R-CF organization: all things related to the CF Metadata Conventions in R.
- `CFArray$new()` writes `coordinates` attribute for scalar axes.
- Set boundary values on lat-long grids warped from auxiliary grids.
- Fixed profiling on a "time" axis and writing boundary values to file, with regular or "climatology" bounds.
- Fixed subsetting on multiple character labels from a label set.
- Testing added. This is rather bare-bones because the netCDF files that are bundled with the package are necessarily small. Much more testing is done locally on a large set of production netCDF files.

# ncdfCF 0.6.0

- New `profile()` method for `CFVariable` and `CFArray`. With this method profiles can be extracted from the data, having a user-selectable dimensionality and location. Examples are temporal profiles at a given location or a zonal (lat or long) vertical profile of the atmosphere, but there are many other options. Multiple profiles can be extracted in a single call and they can be generated as a list of `CFArray` instances or as a single `data.table`.
- New `append()` method for `CFArray`. With this method you can append a `CFArray` instance to another `CFArray` instance, along a single, selectable axis with all other axes being identical. This is especially useful to append time series data spread over multiple files, as is often the case with CMIP data. Given the large size of many such files, it is often necessary to `subset()` the data variable first before appending.
- `subset()` method signature changed to have similar signature as the new `profile()` method.
- `indexOf()` method on axes is using boundary values consistently.
- CFObject and descendants take group field from NCVariable instance.
- Fixed creating a scalar time axis from a longer time axis when doing e.g. `CFVariable$subset(time = "2025-04-17")`.
- Scalar axes are now implemented as an axis of a specific type with length 1.
- Fix label set references when multiple variables share an axis with label sets associated with it.
- Minor code fixes.

# ncdfCF 0.5.0

- Cell measure variables fully supported. External variables can be linked and are then automatically available to referring data variables.
- Writing a `CFArray` instance to file automatically orients the data into the canonical axis order of X - Y - Z - T - others.
- `CFAxis` has several methods added to work with multiple sets of auxiliary coordinates associated with the axis.
- `CFLabel` has `print()` and `write()` methods.
- `CFVariable::subset()` can subset over a discrete axis with auxiliary coordinates.
- `summarise()` can now summarise over eras. This yields a climatological statistic which is now supported with the appropriate "time" axis description.
- `actual_range` attribute is set on data arrays, axes and bounds.
- Added `CFDataset$var_names` and `CFDataset$axis_names` fields.
- Check for duplicate object names at group level added.
- Axis `values` fields made private. Read-only access provided through`CFAxis$values` and `CFAxis$coordinates` (preferred) for consistent access patterns throughout the axis class hierarchy.
- Root group can `print()` without enclosing data set.
- Fixed `summarise()` when temporal result yields scalar time axis.
- Fixed writing bounds for a "time" axis.
- Fixed saving a packed `CFArray` when the original netCDF file was packed as well.
- Minor code fixes.
- Documentation updates.

# ncdfCF 0.4.0

- `CFData` has been renamed `CFArray` to more accurately describe its contents.
- `CFArray` objects can now be written to a netCDF file.
- Methods `CFVariable$summarise()` and `CFArray$summarise()` summarise the temporal dimension of a data object to a lower resolution using a user-supplied function, using the specific calendar of the temporal dimension and returning a new `CFArray` object with the summarised data for every return value of a call to the function, i.e. the function may have multiple return values. The `CFArray` version is much faster (because all data has been read already), but the `CFVariable` version can also summarise data variables that are too big to fit into the available memory entirely. In either case, code is optimized compared to the R base version so an operation over the "time" dimension of a data array is about twice as fast as using the base R `apply(X, MARGIN, tapply, INDEX, FUN, ...)` call.
- Method `CFArray$data.table()` exports a data object to a `data.table`.
- `CFVariable` and `CFArray` classes now have `time()` method to retrieve the "time" axis or its `CFTime` instance, if present.
- `CFAxis` has new `coordinates` field with which to retrieve the coordinates along the axis.
- New attributes can be defined on any object that supports attributes, or deleted.
- Fixed error on reading bounds for auxiliary coordinate variables. Various other minor code fixes.
- `CFResource` fixed to conform to new `R6` version.
- Documentation updated, including vignettes.

# ncdfCF 0.3.0

- Function `peek_ncdf()` returns quick-view information on a netCDF resource.
- String-valued labels for discrete and generic numeric axes are now supported, including multiple label sets per axis. The labels are associated with an axis rather than a data variable (as the CF documents imply) and the axis must be explicitly defined (as the CF documents imply but not explicitly state, and it is missing from the example given).
- Functions `makeMemoryGroup()`, `makeLongitudeAxis()` and `makeLatitudeAxis()` added to create scaffolding for new CF objects.
- `NCGroup::unused()` method identifies unused `NCVariable`s to aid in finding issues with netCDF resources.
- `print()` method for `NCVariable` and `NCDimension`.
- Method `CFObject$fullname` added, giving fully-qualified CF object name.
- "Axis" associated with bounds variable is no longer created.
- NASA level-3 binned data (L3b) is now supported.
- Reference to a containing `NCGroup` moved down to `CFObject` for CF objects.
- Minor code fixes.
- Documentation extended and formatting fixed, new vignette.

# ncdfCF 0.2.1

- NetCDF groups are now fully supported, including traversing group hierarchies with absolute or relative paths.
- All axis types are identified correctly. This includes:
  - Regular coordinate axes (a.k.a. NUG coordinate variables).
  - Discrete axes, even when no coordinate variable is present - an extension to the CF conventions.
  - Parametric vertical axes, but values are not yet computed.
  - Scalar axes, linked to variables through the "coordinates" attribute of the latter.
  - Auxiliary longitude-latitude grids, the horizontal component of the grid of a variable that was not defined as a Cartesian product of latitude and longitude, using the "coordinates" attribute of the variable. When subsetting a data variable, resampling is automatically performed.
- The four axes that *"receive special treatment"* by the Conventions each have a separate class to deal with their specific nature: CFAxisLongitude, CFAxisLatitude, CFAxisVertical, and CFAxisTime.
- Bounds are read and interpreted on all axes except the vertical axis, including any auxiliary long-lat grids.
- Information on UDTs is captured in a separate class. This is effectively only supported for the "compound" sub-type, for scalar values only.
- Data is read into the most compact form possible. This saves a significant amount of memory when large integer variables are read as they would remain integers rather than the default numeric type.
- Data is returned from `CFVariable$subset()` as an instance of the `CFData` class, with associated objects such as axes and the attributes from the variable. Data can be read out in a variety of forms, currently as a raw array, an oriented array or as a `terra::SpatRaster` or `terra::SpatRasterDataset`.
- Full support for grid mapping variables. As a significant extension over CF Metadata requirements, CRS strings are produced in the OGC WKT2 format, using the latest EPSG database of geodetic objects.
- Improvements in printing object details.
- Code refactored to R6.
- GHA enabled

# ncdfCF 0.1.1

- `objects_by_standard_name()` will list objects in the netCDF resource that have a "standard_name" attribute.

# ncdfCF 0.1.0

- Initial CRAN submission. This is a WORK IN PROGRESS and the package is not yet fit for a production environment.
- This version supports reading from netCDF resources. CF Metadata Conventions are used to set properties on axis orientation, time dimensions and bounds.
- Standard R commands can be used to inspect properties of the netCDF resource, such as `dimnames()` and `length()`.
- Access to data uses the R standard `[` selection operator for use with dimension indices. Use real-world coordinates with `subset()`.
