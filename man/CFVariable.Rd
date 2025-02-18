% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFVariable.R
\docType{class}
\name{CFVariable}
\alias{CFVariable}
\title{CF data variable}
\description{
This class represents the basic structure of a CF data variable, the object that
provides access to an array of data.

The CF data variable instance provides access to the data array from
the netCDF resource, as well as all the details that have been associated
with the data variable, such as axis information, grid mapping parameters,
etc.

Do not use this class directly. Instead, use \link{CFVariableGeneric} of any of
the more specific \verb{CFVariable*} classes.
}
\section{Super class}{
\code{\link[ncdfCF:CFObject]{ncdfCF::CFObject}} -> \code{CFVariable}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{axes}}{List of instances of classes descending from \link{CFAxis} that
are the axes of the variable.}

\item{\code{grid_mapping}}{The coordinate reference system of this variable, as
an instance of \link{CFGridMapping}. If this field is \code{NULL}, the horizontal
component of the axes are in decimal degrees of longitude and latitude.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{friendlyClassName}}{(read-only) A nice description of the class.}

\item{\code{gridLongLat}}{The grid of longitude and latitude values of every
grid cell when the main variable grid has a different coordinate system.}

\item{\code{crs}}{(read-only) Retrieve the coordinate reference system
description of the variable as a WKT2 string.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CFVariable-new}{\code{CFVariable$new()}}
\item \href{#method-CFVariable-print}{\code{CFVariable$print()}}
\item \href{#method-CFVariable-brief}{\code{CFVariable$brief()}}
\item \href{#method-CFVariable-shard}{\code{CFVariable$shard()}}
\item \href{#method-CFVariable-peek}{\code{CFVariable$peek()}}
\item \href{#method-CFVariable-data}{\code{CFVariable$data()}}
\item \href{#method-CFVariable-clone}{\code{CFVariable$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="attribute"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-attribute'><code>ncdfCF::CFObject$attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="print_attributes"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-print_attributes'><code>ncdfCF::CFObject$print_attributes()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-new"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-new}{}}}
\subsection{Method \code{new()}}{
Create an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$new(grp, nc_var, axes)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{grp}}{The group that this CF variable lives in.}

\item{\code{nc_var}}{The netCDF variable that defines this CF variable.}

\item{\code{axes}}{List of \link{CFAxis} instances that describe the dimensions.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An instance of this class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-print"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-print}{}}}
\subsection{Method \code{print()}}{
Print a summary of the data variable to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-brief"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-brief}{}}}
\subsection{Method \code{brief()}}{
Some details of the data variable.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$brief()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A 1-row \code{data.frame} with some details of the data variable.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-shard"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-shard}{}}}
\subsection{Method \code{shard()}}{
The information returned by this method is very concise
and most useful when combined with similar information from other
variables.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$shard()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Character string with very basic variable information.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-peek"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-peek}{}}}
\subsection{Method \code{peek()}}{
Retrieve interesting details of the data variable.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$peek(with_groups = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{with_groups}}{Should group information be included? The save option
is \code{TRUE} (default) when the netCDF resource has groups because names may
be duplicated among objects in different groups.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A 1-row \code{data.frame} with details of the data variable.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-data"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-data}{}}}
\subsection{Method \code{data()}}{
Retrieve all data of the variable. Scalar variables are not
present in the result.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{CFData} instance with all data from this variable.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFVariable-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CFVariable-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFVariable$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
