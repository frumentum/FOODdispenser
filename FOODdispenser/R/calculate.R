##################################################################################################################
######################### Function for calculating the perfect measurements for food dispensers ##################
##################################################################################################################

# function for converting degree to radians
deg2rad <- function(deg) {(deg * pi) / (180)}

calculate <- function(rho = 0.6, kg = 25, alpha = 45, depth = 40, width = NA, height = NA) {
  if (is.na(width) && is.na(height)) stop("Either width or height is required")
  depth <- depth / 10 # cm to dm
  # necessary volume
  volume <- kg / rho

  # triangle's height
  h_tri <- tan(deg2rad(alpha)) * depth

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
