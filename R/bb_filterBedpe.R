## Define function to filter out interactions that are too close to the diagonal
bb_filterBedpe <- function(bedpe, res, buffer) {

  ## TODO Generalize this function for inter & intra pairs

  ## Only valid for intrachromosomal bedpe files
  if(bedpe[[1]] != bedpe[[4]]) {
    stop("bb_filterBedpe is only valid for intrachromosomal pairs")
  }

  ## Distance from diagonal to center bedpe pixel
  d <- (sqrt((abs(bedpe[[5]] - bedpe[[2]])^2 * 2)) / 2)

  ## Distance from center bedpe pixel to corner of apa
  y <- sqrt((res*(buffer*2 + 1))^2 * 2) / 2

  ## Return loops that don't intersect the diagonal
  return(bedpe[d > y])

}
