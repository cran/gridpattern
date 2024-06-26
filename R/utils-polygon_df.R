#' Create a \code{polygon_df} object from the given coordinates
#'
#' code using \code{polygon_df} should not assume that the first and last point
#' within each id are the same.  i.e. they may have to manually set a final
#' point equal to the initial point if that is what their graphics system
#' desires
#'
#' @param x,y coordinates of polygon. not necessarily closed.
#' @param id a numeric vector used to separate locations in x,y into multiple polygons
#'
#' @return data.frame with x, y, id columns.
#'
#' @noRd
create_polygon_df <- function(x, y, id = 1L) {
  data_frame(
    x     = x,
    y     = y,
    id    = id
  )
}

# Convert units from 'npc' to another {grid} unit
convert_polygon_df_units <- function(df, units = "bigpts") {
    df$x <- convertX(unit(df$x, "npc"), units, valueOnly = TRUE)
    df$y <- convertY(unit(df$y, "npc"), units, valueOnly = TRUE)
    df
}

#' Convert a \code{polygon_df} to \code{grid::polygonGrob} object
#'
#' @param polygon_df polygon_df data.frame
#' @param default.units 'npc
#' @param gp default: gpar()
#'
#' @return sf polygon object
#'
#' @noRd
convert_polygon_df_to_polygon_grob <- function(polygon_df, default.units = 'npc', gp = gpar()) {


  if (is.null(polygon_df) || nrow(polygon_df) < 3) {
    return(grid::nullGrob())
  }

  grid::polygonGrob(
    x             = polygon_df$x,
    y             = polygon_df$y,
    id            = polygon_df$id,
    default.units = default.units,
    gp            = gp
  )
}

#' Convert a \code{polygon_df} to an \code{sf} POLYGON/MULTIPOLYGON
#'
#' @param polygon_df Polygon data.frame
#' @param buffer_dist buffer the polygon by the given distance
#'
#' @return sf polygon object
#' @noRd
convert_polygon_df_to_polygon_sf <- function(polygon_df, buffer_dist = 0) {

  if (is.null(polygon_df) || nrow(polygon_df) < 3 ||
      anyNA(polygon_df$x) || anyNA(polygon_df$y)) {
    return(sf::st_polygon())
  }

  polys <- split(polygon_df, polygon_df$id)

  create_coords <- function(poly) {
    xs <- poly$x
    ys <- poly$y

    # {sf} wants explicitly closed polygons, so set the last point
    # to be the same as the first
    if (xs[1] != tail(xs, 1) || ys[1] != tail(ys, 1)) {
      xs <- c(xs, xs[1])
      ys <- c(ys, ys[1])
    }

    list(cbind(xs, ys))
  }

  all_coords <- lapply(polys, create_coords)

  res <- sf::st_multipolygon(all_coords)

  # perform a zero-buffer operation to remove self-intersection
  # As suggested here:
  #  - https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec#163480
  # https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
  res <- sf::st_buffer(res, buffer_dist)
  res
}

#' Convert a sf GEOMETRYCOLLECTION/POLYGON/MULTIPOLYGON into a polygon_df
#'
#' @param mp sf GEOMETRYCOLLECTION, POLYGON, or MULTIPOLYGON object
#'
#' @return polygon_df data.frame object
#'
#' @noRd
convert_polygon_sf_to_polygon_df <- function(mp) {
  mat <- as.matrix(mp)
  if (sf::st_is_empty(mp))
    return(mat)
  if (!inherits(mp, c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION',
                      'MULTILINESTRING', 'MULTIPOINT', 'LINESTRING', 'POINT'))) {
    warn(paste("convert_polygon_sf_to_polygon_df():",
               "Not GEOMETRYCOLLECTION, POLYGON, or MULTIPOLYGON:",
               deparse(class(mp))))
  }
  if (!inherits(mp, c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION'))) {
    return(NULL)
  }

  poly_lengths <- get_poly_lengths(mp)
  id  <- rep.int(seq_along(poly_lengths), times = poly_lengths)

  create_polygon_df(x=mat[,1], y=mat[,2], id=id)
}

get_poly_lengths <- function(sf_object) {
    if (inherits(sf_object, c('POLYGON', 'LINESTRING', 'POINT'))) {
        nrow(as.matrix(sf_object))
    } else if (inherits(sf_object, c('MULTIPOLYGON', 'MULTILINESTRING', 'MULTIPOINT'))) {
        if (max(lengths(sf_object)) > 1L)
            abort("There is a MULTIPOLYGON/MULTILINESTRING/MULTIPOINT with length greater than 1")
        vapply(sf_object, function(x) nrow(as.matrix(x[[1]])), integer(1))
    } else if (inherits(sf_object, 'GEOMETRYCOLLECTION')) {
        vapply(sf_object, get_poly_lengths, integer(1))
    } else {
        abort(paste("convert_polygon_sf_to_polygon_df():",
                    "Not GEOMETRYCOLLECTION, POLYGON, or MULTIPOLYGON:",
                    deparse(class(sf_object))))
    }
}

#' Convert a polygon to an alpha mask
#'
#' Alpha values of 1 correspond to pixels within the polygon, all other
#' values will be zero.
#'
#' @param polygon_df polygon_df object
#' @param width,height dimensions of the alpha channel matrix to return
#'
#' @return numeric matrix with all values with 0 or 1
#'
#' @noRd
convert_polygon_df_to_alpha_channel <- function(polygon_df, width, height) {
  current_dev <- grDevices::dev.cur()
  if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the polygon to an actual grob, coloured 'black'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp <- gpar(fill = 'black')
  boundary_grob <- convert_polygon_df_to_polygon_grob(polygon_df, gp=gp)

  # Note `ragg::agg_capture()`'s non-"native" format is a matrix of color strings
  # while `png::readPNG()`'s non-"native" format is an array of numeric values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the grob as an image of the given size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  png_file <- tempfile(fileext = ".png")
  png(png_file, width=width, height=height)
  grid.draw(boundary_grob)
  dev.off()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the file and convert o a numeric matrix with values 0/1 depending
  # on whether the pixel is white or black.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  alpha_channel <- png::readPNG(png_file, native = FALSE)
  alpha_channel <- alpha_channel[,,1] < 0.5
  storage.mode(alpha_channel) <- 'numeric'

  # t(alpha_channel)
  alpha_channel
}
