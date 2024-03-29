#' Magick patterned grobs
#'
#' `grid.pattern_magick()` draws a `imagemagick` pattern onto the graphic device.
#' `names_magick`, `names_magick_intensity`, and
#' `names_magick_stripe` are character vectors of supported `type` values
#' plus subsets for shaded intensity and stripes.
#'
#' @inheritParams grid.pattern_image
#' @param type Magick pattern types.  `names_magick`, `names_magick_intensity`, and
#'             `names_magick_stripe` are character vectors of supported `type` values
#'             plus subsets for shaded intensity and stripes.
#' @param fill Fill colour
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (requireNamespace("magick")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.pattern_magick(x_hex, y_hex, type="octagons", fill="blue", scale=2)
#'   }
#'
#'   # supported magick pattern names
#'   print(names_magick)
#' @seealso The `imagemagick` documentation <http://www.imagemagick.org/script/formats.php> for more information.
#' @export
grid.pattern_magick <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                type = "hexagons", fill = "grey20", scale = 1, filter = "box",
                                alpha = gp$alpha %||% NA_real_, aspect_ratio = 1, key_scale_factor = 1,
                                res = getOption("ggpattern_res", 72),
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("magick", x, y, id,
                 type = type, fill = fill, scale = scale, scale = scale, filter = filter,
                 alpha = alpha, aspect_ratio = aspect_ratio,
                 key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

## Names of patterns available in image magick, plus subsets for shaded intensity and stripes
## See \url{http://www.imagemagick.org/script/formats.php} for more information.

#' @rdname grid.pattern_magick
#' @export
names_magick <- c(
  "bricks", "checkerboard", "circles", "crosshatch", "crosshatch30",
  "crosshatch45", "fishscales", "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100", "hexagons", "horizontal", "horizontal2",
  "horizontal3", "horizontalsaw", "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "leftshingle", "octagons", "right30", "right45", "rightshingle",
  "smallfishscales", "vertical", "vertical2", "vertical3", "verticalbricks",
  "verticalleftshingle", "verticalrightshingle", "verticalsaw"
)

#' @rdname grid.pattern_magick
#' @export
names_magick_intensity <- c(
  "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100"
)


#' @rdname grid.pattern_magick
#' @export
names_magick_stripe <- c(
  "crosshatch", "crosshatch30", "crosshatch45",
  "horizontal", "horizontal2", "horizontal3",
  "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "right30", "right45",
  "vertical", "vertical2", "vertical3"
)

#' Read a user specified filename as an image
#'
#' @inheritParams create_gradient_as_array
#'
#' @return array
#'
#' @noRd
create_magick_pattern_as_array <- function(width, height, params, legend) {

  assert_suggested("magick", "magick")

  if (legend) {
      params$pattern_scale <- params$pattern_scale * params$pattern_key_scale_factor
  }
  type   <- check_default(as.character(params$pattern_type),
                          options = names_magick,
                          default = 'hexagons')

  scale  <- check_default(params$pattern_scale, default = 1, type = 'numeric')

  filter <- tolower(as.character(params$pattern_filter))
  filter <- check_default(filter, options = tolower(magick::filter_types()), default = 'box')

  colour <- as.character(params$pattern_fill)

  img <- create_magick_pattern_img_scaled(
    width  = width,
    height = height,
    type   = type,
    colour = colour,
    scale  = scale,
    filter = filter
  )

  convert_img_to_array(img)
}
