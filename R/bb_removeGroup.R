#' removes a "bb_group" of BentoBox plots
#'
#' @param group a "bb_group" object, as outputted from bb_groupPlots
#'
#'
#' @export
#'
bb_removeGroup <- function(group){

  # ======================================================================================================================================================================================
  # FUNCTIONS
  # ======================================================================================================================================================================================

  ## Define a function that calls errors for bb_removeGroup
    # the group doesn't exist
    # the group isn't plotted

  ## Define a function to remove a plot gtree and its corresponding viewport
  # remove_plots <- function(gtree){
  #
  #   ## get the grobs of the plot and convert to gpath
  #   #grob_list <- lapply(gtree$children, convert_gpath)
  #
  #   ## get the last grob
  #   #last_grob <- grob_list[length(grob_list)]
  #
  #   ## remove the last grob from the list of grobs
  #   #grob_list <- grob_list[- length(grob_list)]
  #
  #   ## remove all grobs except last one, not redrawing each time (to save time)
  #   #invisible(lapply(grob_list, grid.remove, redraw = FALSE))
  #
  #   ## remove last grob with redrawing, now removing all the grobs
  #   #invisible(lapply(last_grob, grid.remove))
  #   grid.remove(gPath(gtree$name))
  #
  #   ## remove the viewport
  #   #seekViewport(name = gtree$vp$name)
  #   #popViewport()
  #
  # }

  # ======================================================================================================================================================================================
  # REMOVE ALL THE PLOTS IN THE GROUP AND THEIR CORRESPONDING VIEWPORTS
  # ======================================================================================================================================================================================

  #sub_plots <- group$grobs$children
  #invisible(lapply(sub_plots, remove_plots))

  grid.remove(gPath(group$grobs$name))

  # ======================================================================================================================================================================================
  # REMOVE THE GROUP VIEWPORT
  # ======================================================================================================================================================================================
#
#   group_vp <- group$grobs$vp
#   seekViewport(name = group_vp$name)
#   popViewport()

}
