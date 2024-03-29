% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reset_image_cache.R
\name{reset_image_cache}
\alias{reset_image_cache}
\title{Reset 'gridpattern' image cache}
\usage{
reset_image_cache()
}
\description{
\code{\link[=grid.pattern_image]{grid.pattern_image()}} and \code{\link[=grid.pattern_placeholder]{grid.pattern_placeholder()}} store images in a cache
(so we won't download image URLs over and over).
\code{reset_image_cache()} resets this cache.
}
