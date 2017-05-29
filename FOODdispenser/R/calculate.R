##################################################################################################################
######################### Function for calculating the perfect measurements for food dispensers ##################
##################################################################################################################
#' function for calculating one missing measurement of a food dispenser
#'
#' @param rho Bulk density. By default 0.6 kg per liter because it will provide enough place for almost every cereal.
#' @param kg How much kilos shall the dispenser contain? By default 25 kg
#' @param angle Angle of the big slanted board inside the dispenser; unit is degree; by default 45
#' @param depth Dispenser's depth. Because of 'euro-norm' 40cm by default.
#' @param width Dispenser's widht. Either width or height is required! Measurements in cm.
#' @param height Dispenser's height.
#' @return If width is given, height will be calculated. If height is given, width will be calculated. You can play with the angle input as well as the depth input. Output is a vector containing width, height, depth and angle.
#'
#'
#' @export
calculate <- function(rho = 0.6, kg = 25, angle = 45, depth = 40, width = NA, height = NA) {
  if (is.na(width) && is.na(height)) stop("Either width or height is required")
  depth <- depth / 10 # cm to dm
  # necessary volume
  volume <- kg / rho

  # triangle's height
  h_tri <- tan(deg2rad(angle)) * depth

  # now it depends on the input...
    # for a given height calculate the width.
  if (is.na(width) && is.numeric(height)) {
    height <- height / 10 # cm to dm
    # height of rectangle
    h_rec <- height - h_tri
    width <- volume / (depth*h_rec + 0.5*depth*h_tri)
  }
    # otherwise calculate the height
  if (is.numeric(width) && is.na(height)) {
    width <- width / 10 # cm to dm
    vol_tri <- 0.5*width*depth*h_tri
    vol_rec <- volume - vol_tri
    h_rec <- vol_rec / (width*depth)
    height <- h_rec + h_tri
  }
  # don't forget 're-converting' the units from dm to cm
  return(c(width = round(width*10, 2),
           depth = round(depth*10, 2),
           height = round(height*10, 2),
           alpha = alpha))
}
