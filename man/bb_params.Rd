% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_params.R
\name{bb_params}
\alias{bb_params}
\title{bb_params: BentoBox parameters object}
\usage{
bb_params(
  alpha = NULL,
  altchrom = NULL,
  altchromend = NULL,
  altchromstart = NULL,
  althalf = NULL,
  arch = NULL,
  archHeight = NULL,
  assembly = "hg19",
  at = NULL,
  bed = NULL,
  bedpe = NULL,
  bg = NULL,
  binCap = NULL,
  binSize = NULL,
  border = NULL,
  boxHeight = NULL,
  boxWidth = NULL,
  BSgenome = NULL,
  cex = NULL,
  check.overlap = NULL,
  chrom = NULL,
  chromend = NULL,
  chromstart = NULL,
  col = NULL,
  colorby = NULL,
  colorbyrange = NULL,
  colorbyStrand = NULL,
  commas = NULL,
  cutoffArches = NULL,
  dataType = NULL,
  default.units = NULL,
  display.column = NULL,
  draw = NULL,
  end = NULL,
  filename = NULL,
  fill = NULL,
  fillcolor = NULL,
  fontcolor = NULL,
  fontcolors = NULL,
  fontsize = NULL,
  gene = NULL,
  gene.id.column = NULL,
  geneBackground = NULL,
  geneBuffer = NULL,
  geneHighlights = NULL,
  geneOrder = NULL,
  genome = NULL,
  gp = NULL,
  half = NULL,
  height = NULL,
  hic = NULL,
  highlightCol = NULL,
  just = NULL,
  label = NULL,
  labels = NULL,
  legend = NULL,
  linecolor = NULL,
  loops = NULL,
  lty = NULL,
  lwd = NULL,
  main = NULL,
  norm = NULL,
  OrgDb = NULL,
  orientation = NULL,
  palette = NULL,
  pch = NULL,
  plot = NULL,
  position = NULL,
  pVals = NULL,
  r = NULL,
  range = NULL,
  res_scale = NULL,
  resolution = NULL,
  rot = NULL,
  scale = NULL,
  sequence = NULL,
  shift = NULL,
  showOutline = NULL,
  showRuler = NULL,
  showUnit = NULL,
  sigCol = NULL,
  sigLine = NULL,
  signal = NULL,
  sigVal = NULL,
  space = NULL,
  spaceHeight = NULL,
  spaceWidth = NULL,
  start = NULL,
  strand = NULL,
  strandcolors = NULL,
  strandLabels = NULL,
  strandSplit = NULL,
  stroke = NULL,
  style = NULL,
  tcl = NULL,
  ticks = NULL,
  title = NULL,
  TxDb = NULL,
  type = NULL,
  width = NULL,
  x = NULL,
  xgrid = NULL,
  y = NULL,
  ygrid = NULL,
  ymax = NULL,
  zrange = NULL
)
}
\arguments{
\item{assembly}{string defining the genome build. Default value is \code{assembly = "hg19"}.}

\item{gene}{(optional) string naming a gene used to set the chromosome, chromstart, and
chromend arguments.}

\item{geneBuffer}{(optional) integer base-pairs to extend the start and end of a gene
defined by argument \code{gene}.}

\item{...}{This function will take any BentoBox function parameters and their values.}
}
\value{
Returns an object of class "bb_params" containing BentoBox function arguments.
}
\description{
Creates an object of class "bb_params" that can be used by BentoBox functions.
bb_params can be used to set a set of parameters to be shared across multiple
functions.
}
\details{
bb_params generates arguments from exported BentoBox functions at loading time of the
package. Arguments defined in a bb_params object can be passed into the \code{params}
argument of BentoBox functions. bb_params arguments can be overridden from within
BentoBox functions.

bb_params also provides an alternative region definition mechanism. Given a gene name
and genome assembly, bb_params returns the appropriate "chrom", "chromstart", and
"chromend" with a default buffer of \code{(gene length) / 2} added to the ends of the
gene coordinates. The buffer amount can be set manually with the \code{geneBuffer}
parameter. Buffer extending beyond the length of the chromosome will be trimmed.
}
\examples{
## Define parameters
p1 <- bb_params(gene = "IL1B", assembly = "hg19")

## Optionally add more parameters
p2 <- bb_params(geneBuffer = 10000)

## Combine parameters and pass them to a BentoBox function
bb_plotGenes(params = c(p1, p2))


}
