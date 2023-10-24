#' Create a weights matrix for use as a circular or sector focal neighbourhood
#'
#' Creates a binary weights matrix representing a circular neighbourhood to use
#' with the \code{\link[terra]{focal}} function. Note, if you are specifying any
#' function other than \code{"mean"} with the focal function you should adjust
#' the weight values accordingly. Optionally, a sector neighbourhood can be
#' constructed rather than a full circular neighbourhood. The sector is defined
#' via the \code{start_bearing} and \code{end_bearing} arguments. The arc of
#' the sector will go clockwise from the start to the end bearing.
#'
#' @param radius (integer > 0) Radius of the neighbourhood (pixels).
#'
#' @param start_bearing Optional compass bearing (degrees) to define a sector.
#'   Default value @code{NULL} means create a full circular neighbourhood.
#'
#' @param end_bearing Optional compass bearing (degrees) to define a sector.
#'   Ignored if \code{start_bearing} is NULL.
#'
#' @param include_centre (logical, default TRUE) Whether to include the centre
#'   cell in the neighbourhood.
#'
#' @param inside_value (default 1) Value to assign to cells inside the
#'   neighbourhood.
#'
#' @param outside_value (default NA) Value to assign to cells outside the
#'   neighbourhood.
#'
#'
#' @examples
#' # Create a neighbourood with a radius of 10 pixels. Returns a 21 x 21 matrix.
#' w <- make_focal_matrix(10)
#'
#' # Create a sector neighbourhood facing West with angular width of 90 degrees
#' # (i.e. SW to NW directions).
#' #
#' w <- make_focal_matrix(10, 225, 315)
#'
#' # Create a sector neighbourhood that excludes pixels between SW and NW
#' # directions.
#' #
#' w <- make_focal_matrix(10, 315, 225)
#'
#' @export
#'
make_focal_matrix <- function(radius,
                              start_bearing = NULL,
                              end_bearing = NULL,
                              include_centre = TRUE,
                              inside_value = 1,
                              outside_value = NA) {

  checkmate::assert_int(radius, lower = 1)
  checkmate::assert_logical(include_centre, any.missing = FALSE, len = 1)

  pos <- (-radius):radius
  width <- length(pos)
  r2 <- radius^2

  # Check if a sector was requested
  is_sector <- FALSE
  if (!is.null(start_bearing)) {
    checkmate::assert_number(start_bearing, lower = 0, upper = 359)
    start_bearing_rad <- normalize_radians( compass_to_rad(start_bearing) )

    checkmate::assert_number(end_bearing, lower = 0, upper = 359)
    end_bearing_rad <- normalize_radians( compass_to_rad(end_bearing) )

    is_sector <- TRUE

    # Check if the rotation between the start and end bearings is clockwise
    is_clockwise <- compass_is_clockwise(start_bearing, end_bearing)$clockwise
  }

  # Create the neighbourhood
  m <- sapply(pos, function(x) {
    y <- -pos  # to have upper row of pixels with max y coord
    inside <- (x)^2 + y^2 <= r2

    if (is_sector) {
      a <- normalize_radians( atan2(y, x) )
      between <- angle_is_between(a, start_bearing_rad, end_bearing_rad)

      if (!is_clockwise) between <- !between

      inside <- inside & between
    }

    ifelse(inside, inside_value, outside_value)
  })

  # Centre cell
  m[radius+1, radius+1] <- as.integer(include_centre)

  m
}


#' Quick plot of a focal matrix
#'
#' Uses the default \code{\link[graphics]{image}} function to visualize a
#' matrix. The centre cell is indicated by an orange point.
#'
#' @param m A numeric matrix representing a focal neighbourhood.
#'
#' @examples
#' # Pacman
#' m <- make_focal_matrix(100, start_bearing = 315, end_bearing = 225)
#' plot_focal_matrix(m)
#'
#' @export
#'
plot_focal_matrix <- function(m) {
  clrs <- palette.colors(4)[-1]

  image(t(m[nrow(m):1,] ), axes=FALSE, asp = c(1,1), col = clrs[1:2])

  points(0.5, 0.5, pch = 16, cex = 2, col = clrs[3])
}

