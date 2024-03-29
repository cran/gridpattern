% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clippingPathGrob.R
\name{clippingPathGrob}
\alias{clippingPathGrob}
\title{Clip grob using another grob to specify the clipping path}
\usage{
clippingPathGrob(
  clippee,
  clipper,
  use_R4.1_clipping = getOption("ggpattern_use_R4.1_clipping",
    getOption("ggpattern_use_R4.1_features")),
  png_device = NULL,
  res = getOption("ggpattern_res", 72),
  name = NULL,
  gp = gpar(),
  vp = NULL
)
}
\arguments{
\item{clippee}{Grob to be clipped}

\item{clipper}{Grob that defines clipping region}

\item{use_R4.1_clipping}{If \code{TRUE} use the grid clipping path feature introduced in R v4.1.0.
If \code{FALSE} do a \code{rasterGrob} approximation.
If \code{NULL} try to guess an appropriate choice.
Note not all graphic devices support the grid clipping path feature
and the grid clipping path feature does not nest.}

\item{png_device}{\dQuote{png} graphics device to use if \code{use_R4.1_clipping} is \code{FALSE}.
If \code{NULL} (default) will use \code{ragg::agg_png()} if the
suggested package \code{ragg} is available else \code{grDevices::png()}.}

\item{res}{Resolution of desired \code{rasterGrob} in pixels per inch if \code{use_R4.1_clipping} is \code{FALSE}.}

\item{name}{ A character identifier. }

\item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link[grid]{gpar}}.  This is basically
    a list of graphical parameter settings.}

\item{vp}{A Grid viewport object (or NULL).}
}
\value{
A \code{grid} grob
}
\description{
\code{clippingPathGrob()} clips a grob using another grob to specify the clipping path
}
\examples{
  if (capabilities("png") && require("grid")) {
    clippee <- patternGrob("circle", gp = gpar(col = "black", fill = "yellow"),
                           spacing = 0.1, density = 0.5)
    angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 7)
    x_hex_outer <- 0.5 + 0.5 * cos(angle)
    y_hex_outer <- 0.5 + 0.5 * sin(angle)
    x_hex_inner <- 0.5 + 0.25 * cos(rev(angle))
    y_hex_inner <- 0.5 + 0.25 * sin(rev(angle))
    clipper <- grid::pathGrob(x = c(x_hex_outer, x_hex_inner),
                              y = c(y_hex_outer, y_hex_inner),
                              id = rep(1:2, each = 7),
                              rule = "evenodd")
    clipped <- clippingPathGrob(clippee, clipper, use_R4.1_clipping = FALSE)
    grid.newpage()
    grid.draw(clipped)
  }
}
