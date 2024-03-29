% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_summary.R
\name{plot_summary}
\alias{plot_summary}
\alias{plot_summary.swmpr}
\title{Plot graphical summaries of SWMP data}
\usage{
plot_summary(swmpr_in, ...)

\method{plot_summary}{swmpr}(
  swmpr_in,
  param,
  colsleft = c("lightblue", "lightgreen"),
  colsmid = "lightblue",
  colsright = c("lightblue", "lightgreen", "tomato1"),
  base_size = 11,
  years = NULL,
  plt_sep = FALSE,
  sum_out = FALSE,
  fill = c("none", "monoclim", "interp"),
  ...
)
}
\arguments{
\item{swmpr_in}{input swmpr object}

\item{...}{additional arguments passed to other methods}

\item{param}{chr string of variable to plot}

\item{colsleft}{chr string vector of length two indicating colors for left plots}

\item{colsmid}{chr string vector of length one indicating colors for middle plots}

\item{colsright}{chr string vector of length three indicating colors for right plots}

\item{base_size}{numeric for text size}

\item{years}{numeric vector of starting and ending years to plot, default all}

\item{plt_sep}{logical if a list is returned with separate plot elements}

\item{sum_out}{logical if summary data for the plots is returned}

\item{fill}{chr string indicating if missing monthly values are left as is (\code{'none'}, default), replaced by long term monthly averages (\code{'monoclim'}), or linearly interpolated using \code{\link[zoo]{na.approx}}}
}
\value{
A graphics object (Grob) of multiple \code{\link[ggplot2]{ggplot}} objects, otherwise a list of  individual \code{\link[ggplot2]{ggplot}} objects if \code{plt_sep = TRUE} or a list with data frames of the summarized data if \code{sum_out = TRUE}.
}
\description{
Plot graphical summaries of SWMP data for individual parameters, including seasonal/annual trends and anomalies
}
\details{
This function creates several graphics showing seasonal and annual trends for a given swmp parameter.  Plots include monthly distributions, monthly anomalies, and annual anomalies in multiple formats.  Anomalies are defined as the difference between the monthly or annual average from the grand mean.  Monthly anomalies are in relation to the grand mean for the same month across all years.  All data are aggregated for quicker plotting.  Nutrient data are based on monthly averages, wheras weather and water quality data are based on daily averages.  Cumulative precipitation data are based on the daily maximum.

Individual plots can be obtained if \code{plt_sep = TRUE}.  Individual plots for elements one through six in the list correspond to those from top left to bottom right in the combined plot.

Summary data for the plots can be obtained if \code{sum_out = TRUE}.  This returns a list with three data frames with names \code{sum_mo}, \code{sum_moyr}, and \code{sum_mo}.  The data frames match the plots as follows: \code{sum_mo} for the top left, bottom left, and center plots, \code{sum_moyr} for the top right and middle right plots, and \code{sum_yr} for the bottom right plot. 

Missing values can be filled using the long-term average across years for each month (\code{fill = 'monoclim'}) or as a linear interpolation between missing values using \code{\link[zoo]{na.approx}} (\code{fill = 'interp'}).  The monthly average works well for long gaps, but may not be an accurate representation of long-term trends, i.e., real averages may differ early vs late in the time series if a trend exists. The linear interpolation option is preferred for small gaps.
}
\examples{
## import data
data(apacpnut)
dat <- qaqc(apacpnut)

## plot
plot_summary(dat, param = 'chla_n', years = c(2007, 2013))

## get individaul plots
plots <- plot_summary(dat, param = 'chla_n', years = c(2007, 2013), plt_sep = TRUE)

plots[[1]] # top left
plots[[3]] # middle
plots[[6]] # bottom right

## get summary data
plot_summary(dat, param = 'chla_n', year = c(2007, 2013), sum_out = TRUE)

}
\seealso{
\code{\link[ggplot2]{ggplot}}
}
\concept{analyze}
