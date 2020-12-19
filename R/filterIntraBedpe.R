## Define function to filter out interactions that are too close to the diagonal
filterIntraBedpe <- function(bedpe, res, buffer) {

  ## Only valid for intrachromosomal bedpe files
  stopifnot(bedpe[[1]] == bedpe[[4]])

  ## Distance from diagonal to center bedpe pixel
  d <- (sqrt((abs(bedpe[[5]] - bedpe[[2]])^2 * 2)) / 2)

  ## Distance from center bedpe pixel to corner of apa
  y <- sqrt((res*(buffer*2 + 1))^2 * 2) / 2

  ## Return loops that don't intersect the diagonal
  return(bedpe[d > y])

}
