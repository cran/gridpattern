fill_types <- c('fit', 'expand', 'squish', 'none', 'tile')

#' Resize image, preserving aspect, such that one of target width or height is achieved without distoring or cropping.
#'
#' The canvas of the image will be expanded to cover the full width/height but
#' will be transparent.
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' try({
#'   filename <- system.file("img", "Rlogo.png", package="png")
#'   img <- magick::image_read(filename)
#'   fill_area_with_img_fit(img, 100, 400)
#' })
#' @noRd
fill_area_with_img_fit <- function(img, width, height, gravity = 'Center', filter = 'lanczos') {

  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = TRUE)
  img      <- magick::image_resize(img, geometry = geometry, filter=filter)
  img      <- magick::image_extent(img, geometry, gravity = gravity)

  img
}

#' Resize image, ignoring aspect, such that both the target width and height is achieved.
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' try({
#'   filename <- system.file("img", "Rlogo.png", package="png")
#'   img <- magick::image_read(filename)
#'   fill_area_with_img_squish(img, 100, 400)
#' })
#' @noRd
fill_area_with_img_squish <- function(img, width, height, filter='lanczos') {
  geometry <- magick::geometry_size_pixels(width = width, height = height,
                                           preserve_aspect = FALSE)
  img <- magick::image_resize(img, geometry = geometry, filter=filter)
  img
}


#' Resize image, preserving aspect, such that when cropped, the target width and height is achieved.
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' try({
#'   filename <- system.file("img", "Rlogo.png", package="png")
#'   img <- magick::image_read(filename)
#'   fill_area_with_img_expand(img, 100, 400)
#' })
#' @noRd
fill_area_with_img_expand <- function(img, width, height, gravity = 'Center', filter='lanczos') {
  img_info   <- magick::image_info(img)
  img_aspect <- img_info$width/img_info$height
  area_aspect <- width/height
  if (area_aspect > img_aspect) {
    scale_width  <- width
    scale_height <- width / img_aspect
  } else if (area_aspect < img_aspect) {
    scale_height <- height
    scale_width  <- height * img_aspect
  } else {
    scale_height <- height
    scale_width  <- width
  }

  # expand
  geometry <- magick::geometry_size_pixels(width = scale_width, height = scale_height, preserve_aspect = FALSE)
  img      <- magick::image_resize(img, geometry = geometry, filter=filter)

  # crop
  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = FALSE)
  img      <- magick::image_crop(img, geometry = geometry, gravity = gravity)

  img
}


#' Resize image canvas to the target width/height, and use gravity to place the undistorted image
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' try({
#'   filename <- system.file("img", "Rlogo.png", package="png")
#'   img <- magick::image_read(filename)
#'   fill_area_with_img_none(img, 100, 400)
#' })
#' @noRd
fill_area_with_img_none <- function(img, width, height, gravity = 'Center',
                                    filter = 'lanczos', scale = 1) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale if requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scale <- update_scale(scale, img, width, height)
  if (scale != 1) {
    geometry <- magick::geometry_size_percent(width = scale * 100)
    img      <- magick::image_resize(img, geometry, filter = filter)
  }


  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = TRUE)
  img      <- magick::image_extent(img, geometry, gravity = gravity)

  img
}

# if `scale == -1` scale to img width, if `scale == -2` scale to img height
update_scale <- function(scale, img, width, height) {
  if (scale == -1) {
    info   <- magick::image_info(img)
    width/info$width
  } else if (scale == -2) {
    info   <- magick::image_info(img)
    height/info$height
  } else {
    scale
  }
}

#' Tile image to fill the specified area
#'
#' @return magick image of the required dimensions
#' @noRd
fill_area_with_img_tile <- function(img, width, height, gravity = "SouthWest", filter = filter, scale = 1) {

  scale <- update_scale(scale, img, width, height)
  if (scale != 1) {
    geometry <- magick::geometry_size_percent(width = scale * 100)
    img      <- magick::image_resize(img, geometry, filter = filter)
  }

  img_info <- magick::image_info(img)
  n_width <- width %/% img_info$width + as.integer(width %% img_info$width > 0)
  n_height <- height %/% img_info$height + as.integer(height %% img_info$height > 0)
  rows <- magick::image_append(rep(img, n_width))
  tiled <- magick::image_append(rep(rows, n_height), stack=TRUE)

  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = FALSE)

  cropped <- magick::image_crop(tiled, geometry = geometry, gravity = gravity)

  cropped
}

#' Fill an area with a magick image
#'
#' Support fill types
#'
#' \itemize{
#' \item{\code{fit   }}{ - Fill image canvas, preserving aspect, such that one of target width or height is achieved without distoring or cropping.}
#' \item{\code{squish}}{ - Fill image canvas, ignoring aspect, such that both the target width and height is achieved.}
#' \item{\code{expand}}{ - Fill image canvas, by scaling image and preserving aspect, such that when cropped, the target width and height is achieved.}
#' \item{\code{none  }}{ - Fill image canvas canvas to the target width/height, and use gravity to place the undistorted image}
#' \item{\code{tile  }}{ - Tile the image canvas with image.}
#' }
#'
#' @param img magick image
#' @param width,height target dimensions for final image
#' @param type how to resize the image. 'none', 'fit', 'squish', 'expand' or 'tile'. default: squish
#' @param gravity how to anchor the image during the operation. See \code{magick::gravity_types()}
#' @param filter magick filter type. default: lanczos. See \code{magick::filter_types()}
#'        for more information.  This option has no effect for \code{type = 'none'}.
#' @param scale scale the source image before tiling. default: 1
#' @param filter default: lanczos
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' try({
#'   filename <- system.file("img", "Rlogo.png", package="png")
#'   img <- magick::image_read(filename)
#'   fill_area_with_img(img, 100, 400, type = 'squish')
#' })
#' @noRd
fill_area_with_img <- function(img, width, height, type='squish',
                               gravity = 'Center', filter='lanczos',
                               scale = 1) {

  if (length(width) != 1 || length(height) != 1 ||
      is.na(width) || is.na(height) ||
      width <= 0 || height <= 0) {
    return(img)
  }

  type <- check_default(type, fill_types)

  switch(
    type,
    fit    = fill_area_with_img_fit   (img, width, height, gravity = gravity, filter = filter),
    expand = fill_area_with_img_expand(img, width, height, gravity = gravity, filter = filter),
    squish = fill_area_with_img_squish(img, width, height                   , filter = filter),
    none   = fill_area_with_img_none  (img, width, height, gravity = gravity, filter = filter, scale = scale),
    tile   = fill_area_with_img_tile  (img, width, height, gravity = gravity, filter = filter, scale = scale),
    {
      warn("fill_area_with_img(): resize not understood: '", type,
           "', using 'squish'")
      fill_area_with_img_squish(img, width, height)
    }
  )
}
