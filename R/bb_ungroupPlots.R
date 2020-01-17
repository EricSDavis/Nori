#' ungroups a "bb_group" object
#'
#' @param group a "bb_group" object, as outputted from bb_groupPlots
#'
#' @return Function will return a list of plot objects that were in the "bb_group"
#'
#' @export
#'
bb_ungroupPlots <- function(group){

  # ======================================================================================================================================================================================
  # FUNCTIONS
  # ======================================================================================================================================================================================
  ## Error function

  ## Define a function that extracts plot coordinates within container to the center and converts them to page units
  convert_fromContainer <- function(plot){

    center_coords <- adjust_vpCoords(viewport = plot$vp)

    ## Convert dimensions/coordinates of placed version of plot into page_units
    new_x <- convertX(center_coords[[1]], unitTo = get("page_units", envir = bbEnv))
    new_y <- convertY(center_coords[[2]], unitTo = get("page_units", envir = bbEnv))
    new_width <- convertWidth(plot$vp$width, unitTo = get("page_units", envir = bbEnv))
    new_height <- convertHeight(plot$vp$height, unitTo = get("page_units", envir = bbEnv))

    ## Reassign dimensions/coordinates to plot
    plot$vp$x <- new_x
    plot$vp$y <- new_y
    plot$vp$width <- new_width
    plot$vp$height <- new_height
    plot$vp$justification <- "center"

    return(plot)

  }

  ## Define a function that adjusts plot coordinates from container basis to page basis
  adjust_toPage <- function(plot, container_coords){

    new_x <-  plot$vp$x + container_coords[[1]]
    new_y <- plot$vp$y + container_coords[[2]]

    ## Reassign dimensions to plot
    plot$vp$x <- convertX(new_x, unitTo = get("page_units", envir = bbEnv))
    plot$vp$y <- convertY(new_y, unitTo = get("page_units", envir = bbEnv))

    return(plot)

  }

  ## Define a function that updates plot viewports with new coordinates and dimensions
  update_viewport <- function(plot){

    new_vp <- viewport(x = plot$vp$x, y = plot$vp$y, width = plot$vp$width, height = plot$vp$height, just = "center",
                           xscale = plot$vp$xscale, yscale = plot$vp$yscale, name = plot$vp$name)
    plot$vp <- new_vp

    return(plot)
  }

  ## Define a function that updated sub_plot objects
  update_plot <- function(plot, plot_information){

    plot$grobs <- plot_information
    plot$x <- plot_information$vp$x
    plot$y <- convertY(unit(get("page_height", envir = bbEnv), get("page_units", envir = bbEnv)) - plot_information$vp$y,
                       unitTo = get("page_units", envir = bbEnv))
    plot$width <- plot_information$vp$width
    plot$height <- plot_information$vp$height
    plot$justification <- plot_information$vp$justification

    return(plot)

  }

  # ======================================================================================================================================================================================
  # GO INTO GROUP CONTAINER GTREE AND ADJUST COORDINATES/DIMENSIONS
  # ======================================================================================================================================================================================

  downViewport(name = group$grobs$vp$name)

  group$grobs$children <- lapply(group$grobs$children, convert_fromContainer)

  ## Go back up to page viewport
  upViewport()

  # ======================================================================================================================================================================================
  # GET BOTTOM LEFT OF CONTAINER VIEWPORT COORDINATES
  # ======================================================================================================================================================================================

  container_coords <- vp_bottomLeft(viewport = group$grobs$vp)

  # ======================================================================================================================================================================================
  # ADJUST PLOT COORDINATES TO BE BASED ON PAGE
  # ======================================================================================================================================================================================

  group$grobs$children <- lapply(group$grobs$children, adjust_toPage, container_coords = container_coords)

  # ======================================================================================================================================================================================
  # CREATE UPDATED VIEWPORTS FOR EACH PLOT
  # ======================================================================================================================================================================================

  group$grobs$children <- lapply(group$grobs$children, update_viewport)

  ## Change list back to gList
  class(group$grobs$children) <- "gList"

  # ======================================================================================================================================================================================
  # REMOVE PREVIOUS VIEWPORTS AND PLOTS
  # ======================================================================================================================================================================================

  bb_removeGroup(group = group)

  # ======================================================================================================================================================================================
  # ASSIGN UPDATED PLOTS TO EACH SUBPLOT OBJECT
  # ======================================================================================================================================================================================

  sub_plots <- group$plots

  sub_plots <- mapply(update_plot, sub_plots, plot_information = group$grobs$children)

  # ======================================================================================================================================================================================
  # DRAW NEW PLOTS OUTSIDE OF CONTAINER
  # ======================================================================================================================================================================================

  grid.draw(group$grobs$children)

  # ======================================================================================================================================================================================
  # RETURN A LIST OF THE SEPARATED PLOT OBJECTS WITH THEIR NEW LOCATIONS AND DIMENSIONS
  # ======================================================================================================================================================================================

  return(sub_plots)

}
