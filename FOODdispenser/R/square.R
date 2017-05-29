square <- function(width, height, ylab = "Höhe", xlab = "Breite") {
  ## create an empty plot
  plot.new() 
  
  ## set up measurements
  plot.window(c(0,width),
              c(0,height))
  
  if (height >= width) {
    plot(x = 1:height, 
         y = 1:height, 
         xlim = c(1, width), 
         type = "n", 
         xlab = paste(xlab, width, "cm"), 
         ylab = paste(ylab, height, "cm"), 
         xaxt = "n",
         yaxt = "n", 
         bty = "n")
  } else {
    plot(x = 1:width, 
         y = 1:width, 
         ylim = c(1, height), 
         type = "n",
         bty = "n",
         xlab = paste(xlab, width, "cm"), 
         ylab = paste(ylab, height, "cm"), 
         xaxt = "n",
         yaxt = "n")
  }
  
  ## draw x-axis
  axis(1, at = seq(from = 0.1, to = width+0.1, by = 5), labels = seq(from = 0.1, to = width+0.1, by = 5) - 0.1)
  
  ## draw y-axis
  axis(2, at = seq(from = 0.1, to = height+0.1, by = 5), labels = seq(from = 0.1, to = height+0.1, by = 5) - 0.1, las = 1)
  
  ## vertical lines
  lines(x = rep(0.1,height+1), y = 0.1:(0.1+height))
  lines(x = rep(width+0.1, height+1), y = 0.1:(0.1+height))
  
  ## horizontal lines
  lines(x = 0.1:(0.1+width), y = rep(0.1, width+1))
  lines(x = 0.1:(0.1+width), y = rep(height+0.1, width+1))
}