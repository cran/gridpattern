# We use `<<-` below to modify the package's namespace
# in order to prevent a build time dependency on `memoise`
# as recommended in <http://memoise.r-lib.org/reference/memoise.html#details>.
# We don't modify the global environment.
# See <https://github.com/r-lib/memoise/issues/76> for further details.

# Define function at build time
img_read_memoised <- img_read

# Modify function at load time
.onLoad <- function(libname, pkgname) {
    img_read_memoised <<- memoise::memoise(img_read)
}

#' @import grid
#' @importFrom glue glue
#' @importFrom grDevices col2rgb dev.capture dev.capabilities dev.off png rgb
#' @importFrom rlang %||% abort inform warn
#' @importFrom utils hasName head packageVersion tail
NULL
