## Newest version of plotApa


## Load APA data
# load("data/output/inter_intrachromAPA_10kb_500peaks_10buffer_noNorm.rda")

## Required libraries (add to imports)
library(colourvalues)


## Input variables -----------------------------------------------------------------------
apa = inter_wt
# apa = Reduce("+", inter_wt)
# apa = reshape2::melt(apa)

palette = colorRampPalette(c("white", "dark red"))
range = c(0, 1252)#c(639, 1252)


## Tests ---------------------------------------------------------------------------------

# ## Different classes 1
# apa = inter_wt
# class(apa[[1]]) <- "data.frame"
#
# ## Different classes 2
# apa = inter_wt
# apa[[1]] <- as.data.frame(apa[[1]])
#
# ## Same dimesion 1
# apa = inter_wt
# apa <- apa[1:10]
# apa[[1]] <- apa[[1]][-c(1:2),]
#
# ## Same dimesion 2
# apa = inter_wt
# apa <- apa[1:10]
# apa <- lapply(1:length(apa), function(i) apa[[i]] <- apa[[i]][-c(1:2),])
#
# ## All numeric 1
# apa <- list(
#   matrix(data = "a", nrow = 21, ncol = 21),
#   matrix(data = "b", nrow = 21, ncol = 21)
# )
#
# ## All numeric 2
# apa <- matrix(data = "a", nrow = 21, ncol = 21)
#
# ## Long format 1
# apa = Reduce("+", inter_wt)
# apa = reshape2::melt(apa)
# apa$something <- "a"
#
# ## long format 2
# apa = Reduce("+", inter_wt)
# apa = reshape2::melt(apa)
# apa$value <- as.character(apa$value)

## Unsupported class

## Define functions ----------------------------------------------------------------------

## Define function that checks apa for list format
check_apa_list <- function(apa) {

  ## Check that the class of each element is a matrix
  if (!identical(unique(unlist(lapply(apa, class))), "matrix")) {
    stop("apa list must all be class 'matrix'.")
  }

  ## Check that all elements are of the same, square dimensions
  if (length(unique(unlist(lapply(apa, dim)))) != 1) {
    stop("apa list must contain matricies of the same dimensions and nrow == ncol.")
  }

  ## Check that all elements are numeric
  if (!identical(class(unlist(apa)), "numeric")) {
    stop("apa list must be a list of numeric matricies.")
  }

}

## Define function that checks apa for data.frame format
check_apa_dataFrame <- function(apa) {

  ## Check that format is long (expects 3 column data.frame)
  if (ncol(apa) != 3) {
    stop("apa data.frame expects long format (i.e. bin1, bin2, value)")
  }

  ## Check that third column is numeric
  if (!identical(class(apa[[3]]), "numeric")) {
    stop("apa data.frame values must be numeric.")
  }

}

## Define function that checks apa for matrix format
check_apa_matrix <- function(apa) {

  ## Check that format is wide
  if (nrow(apa) != ncol(apa)) {
    stop("apa matrix must have equal numbers of rows and columns (i.e. wide format).")
  }

  ## Check that all elements are numeric
  if (!identical(class(as.vector(apa)), "numeric")) {
    stop("apa matrix must be a numeric.")
  }

}

## Define color mapping function (from package:BentoBox)
maptocolors <- function(vec, col, num = 100, range = NULL){

  if (is.null(range) == TRUE){
    breaks <- seq(min(vec), max(vec), length.out = num)
  } else {
    vec[which(vec < range[1])] = range[1]
    vec[which(vec > range[2])] = range[2]
    breaks <- seq(range[1], range[2], length.out = num)
  }

  cols <- col(length(breaks) + 1)
  colvec <- as.character(cut(vec, c(-Inf, breaks, Inf), labels = cols))
  return(colvec)


}






## Begin function ------------------------------------------------------------------------

## Accept multiple types of input and convert to matrix
if (class(apa) == "list") {

  ## Check for errors with list format
  check_apa_list(apa)

  ## Convert apa matrix list to a single apa matrix
  apa <- Reduce("+", apa)


} else if ("data.frame" %in% class(apa)) {

  ## Check for errors with data.frame format
  check_apa_dataFrame(apa)

  ## Rename columns & cast into wide format matrix
  colnames(apa) <- c("var1", "var2", "value")
  apa <- reshape2::acast(apa, var1 ~ var2)

} else if (class(apa) != "matrix") {

  ## Stop for anything that is not a matrix
  stop(sprintf("class %s is not supported.", class(apa)))

}

## Check for matrix errors
check_apa_matrix(apa)


## Map apa matrix values to color palette
colv <- maptocolors(vec = as.vector(apa), col = palette, range = range)

## Format color vector back to apa matrix
m <- matrix(data = colv, nrow = nrow(apa), ncol = ncol(apa))

## Create viewport
grid::grid.newpage()

## Visualize apa
grid::grid.raster(image = m, interpolate = F)



# remotes::install_github(repo = "EricSDavis/Nori", ref = "Fork-Nori", force = T)



