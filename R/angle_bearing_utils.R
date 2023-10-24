#' Convert Cartesian angles in radians to compass bearings in degrees
#'
#' Cartesian angles, as used by R's trigonometric functions, treat 0 as the
#' positive X-axis direction, with positive angles being counter-clockwise
#' rotation. In contrast, compass angles treat 0 as North with positive angles
#' being clockwise rotation.
#'
#' @param x Vector of one or more Cartesian angles in radians.
#'
#' @return Vector of corresponding compass angles in the range [0,360).
#'
rad_to_compass <- function(x) {
  (180 * (pi/2 - x) / pi) %% 360
}


#' Convert compass bearings in degrees to Cartesian angles in radians
#'
#' Cartesian angles, as used by R's trigonometric functions, treat 0 as the
#' positive X-axis direction, with positive angles being counter-clockwise
#' rotation. In contrast, compass angles treat 0 as North with positive angles
#' being clockwise rotation.
#'
#' @param x Vector of one or more compass bearings in degrees.
#'
#' @return Vector of corresponding Cartesian angles in radians in the range
#'   [0, 2*pi).
#'
compass_to_rad <- function(x) {
  x <- x %% 360
  (pi/2 - (x * pi / 180)) %% (2*pi)
}


#' Check if the shortest rotation two bearings is clockwise
#'
#' Given two compass bearings, this function checks if the shortest rotation
#' from the first to the second is clockwise. For cases where the two bearings
#' are exactly opposite, e.g. 90 (E) and 270 (W), the function returns TRUE.
#' Adapted from a post by Allan Cameron on StackOverflow:
#' https://stackoverflow.com/a/63429855/40246.
#'
#' @param b1 First compass bearing (degrees).
#' @param b2 Second compass bearing (degrees).
#'
#' @return A list with two named elements: clockwise (logical) and angle (shortest
#'   angular rotation between the two bearings).
#'
#' @examples
#' # NW to NE
#' compass_is_clockwise(315, 45)  # returns list(TRUE, 45)
#'
#' # NE to NW
#' compass_is_clockwise(45, 315)  # returns list(FALSE, 45)
#'
#' # Exactly opposite bearings are treated as clockwise
#' compass_is_clockwise(270, 90)  # returns list(TRUE, 180)
#'
compass_is_clockwise <- function(b1, b2) {
  angle <- b2 - b1
  clockwise <- angle %% 360
  counter_clockwise <- 360 - clockwise

  if (abs(clockwise) <= abs(counter_clockwise)) {
    list(clockwise = TRUE, angle = abs(clockwise))
  } else {
    list(clockwise = FALSE, angle = abs(counter_clockwise))
  }
}


#' Put Cartesian angles into the range [0, 2*pi)
#'
#' @param x Vector of one or more Catesian angles (radians).
#'
#' @return Corresponding values in the range [0, 2*pi).
#'
#' @examples
#' normalize_radians(-pi/4) # returns 7*pi/4 (approx 5.4978)
#'
normalize_radians <- function(x) {
  under <- x < 0
  over <- x >= 2*pi

  while (any(under | over)) {
    x[under] <- x[under] + 2*pi
    under <- x < 0

    x[over] <- x[over] + 2*pi
    over <- x >= 2*pi
  }

  x
}


#' Test if one or more angles are between two reference angles
#'
#' Given references angles a and b, this function checks if the angle(s) x are
#' between a and b. This will be true when a given angle lies in the clockwise
#' arc starting at a and ending at b. Adapted from an example posted by Jonathan
#' Mee on Stack Overflow: https://stackoverflow.com/a/42424631/40246.
#'
#' @param x A vector of one or more angles to test.
#' @param a The first reference angle (start of arc).
#' @param b The second reference angle (end of arc).
#'
#' @return A logical vector the same length as x.
#'
angle_is_between <- function(x, a, b) {
  checkmate::assert_number(a)
  checkmate::assert_number(b)
  checkmate::assert_numeric(x, finite = TRUE)

  a <- normalize_radians(a)
  b <- normalize_radians(b)
  x <- normalize_radians(x)

  sapply(x, function(xi) {
    if(a <= b) {
      if(b - a <= pi) {
        a <= xi && xi <= b
      } else {
        b <= xi || xi <= a
      }
    } else {
      if(a - b <= pi) {
        b <= xi && xi <= a
      } else {
        a <= xi || xi <= b
      }
    }
  })
}


