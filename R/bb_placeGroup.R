#' moves and/or rescales a group of grobs
#'
#' @param group a "bb_group" object, as outputted from bb_groupPlots
#' @param x A unit object specifying x-location.
#' @param y A unit object specifying y-location.
#' @param width A unit object specifying width.
#' @param height A unit object specifying height.
#' @param just A string or numeric vector specifying the justification of the plot relative to its (x, y) location
#'
#' @return Function will return an updated "bb_group" object
#'
#' @export
#'
bb_placeGroup <- function(group, x = NULL, y = NULL, width = NULL, height = NULL, just = c("left", "top"), draw = T){

  # ======================================================================================================================================================================================
  # FUNCTIONS
  # ======================================================================================================================================================================================

  ## Error function

  remove_plots <- function(gtree){

    ## get the grobs of the plot and convert to gpath
    grob_list <- lapply(gtree$children, convert_gpath)

    ## get the last grob
    last_grob <- grob_list[length(grob_list)]

    ## remove the last grob from the list of grobs
    grob_list <- grob_list[- length(grob_list)]

    ## remove all grobs except last one, not redrawing each time (to save time)
    invisible(lapply(grob_list, grid.remove, redraw = FALSE))

    ## remove last grob with redrawing, now removing all the grobs
    invisible(lapply(last_grob, grid.remove))

    ## remove the viewport
    seekViewport(name = gtree$vp$name)
    popViewport()

  }

  set_x <- function(object, val){

    if (is.null(val)){

      object['x'] <- list(NULL)

    } else {

      object$x <- val

    }

    return(object)

  }

  set_y <- function(object, val){

    if (is.null(val)){

      object['y'] <- list(NULL)

    } else {

      object$y <- val

    }

    return(object)

  }

  set_width <- function(object, val){

    if (is.null(val)){

      object['width'] <- list(NULL)

    } else {

      object$width <- val

    }

    return(object)

  }

  set_height <- function(object, val){

    if (is.null(val)){

      object['height'] <- list(NULL)

    } else {

      object$height <- val

    }

    return(object)

  }

  set_values <- function(object, x, y, width, height){

    object <- set_x(object = object, val = x)
    object <- set_y(object = object, val = y)
    object <- set_width(object = object, val = width)
    object <- set_height(object = object, val = height)

    return(object)
  }

  replace_value <- function(val, new){

    return(new[val])

  }

  inherit_coordinates <- function(input_plot, output_plot){

    ## Make sublists of the dimensions and coordinates of the input and output plots
    inputCoords <- list(x = input_plot$x, y = input_plot$y, width = input_plot$width, height = input_plot$height, justification = input_plot$jusification)
    outputCoords <- list(x = output_plot$x, y = output_plot$y, width = output_plot$width, height = output_plot$height, justification = output_plot$justification)

    ## Determine which values in the output plot are NULL
    to_replace <- names(outputCoords[sapply(outputCoords, is.null)])
    not_replace <- outputCoords[!sapply(outputCoords, is.null)]

    ## Get corresponding values for those that are NULL from the input plot
    replaced <- unlist(lapply(to_replace, replace_value, new = inputCoords), recursive = F)

    ## Recombine values that weren't replaced and those that were
    new_coords <- c(not_replace, replaced)

    ## Assign new values to object
    output_plot <- set_values(object = output_plot, x = new_coords$x, y = new_coords$y, width = new_coords$width, height = new_coords$height)
    output_plot$justification <- new_coords$justification

    ## Return object
    return(output_plot)

  }

  # ======================================================================================================================================================================================
  # INITIALIZE PLOT OBJECT AND UPDATE DIMENSIONS/COORDINATES BASED ON INPUTS
  # ======================================================================================================================================================================================

  object <- group
  object <- set_values(object = object, x = x, y = y, width = width, height = height)
  object$justification <- just
  attr(x = object, which = "plotted") <- draw

  # ======================================================================================================================================================================================
  # CATCH ERRORS
  # ======================================================================================================================================================================================



  # ======================================================================================================================================================================================
  # UPDATE DIMENSIONS AND COORDINATES OF PLOT OBJECT
  # ======================================================================================================================================================================================
  # if (is.null(width) & is.null(height)){
  #
  #   object$width <- group$grobs$vp$width
  #   object$height <- group$grobs$vp$height
  #
  # } else {
  #
  #   object$width <- width
  #   object$height <- height
  #
  # }
  #
  # object$x <- x
  # object$y <- y
  # object$justification <- just

  object <- inherit_coordinates(input_plot = group, output_plot = object)

  if (is.null(object$width) & is.null(object$height)){

    object$width <- group$grobs$vp$width
    object$height <- group$grobs$vp$height

  }

  # ======================================================================================================================================================================================
  # CHECK PLACEMENT
  # ======================================================================================================================================================================================

  check_group_placement(object = object)


  # ======================================================================================================================================================================================
  # ADJUST GROUP VIEWPORT DIMENSIONS AND COORDINATES BASED ON INPUT
  # ======================================================================================================================================================================================

  ## x or y coordinates can only be NULL if draw = F
  if (!is.null(object$x) & !is.null(object$y)){

    ## Convert coordinates into same units as page
    page_coords <- convert_page(object = object)

    object$grobs$vp$x <- page_coords$x
    object$grobs$vp$y <- page_coords$y
    object$grobs$vp$width <- page_coords$width
    object$grobs$vp$height <- page_coords$height
    object$grobs$vp$justification <- just

  }


  # ======================================================================================================================================================================================
  # DRAW
  # ======================================================================================================================================================================================

  if (draw == T){

    ## Remove all previous drawn plots in group if they still exist
    current_grobs <- grid.ls(print = F)$name
    if (group$grobs$name %in% current_grobs){

      bb_removeGroup(group = group)

    }

    grid.draw(object$grobs)

  }

  # ======================================================================================================================================================================================
  # RETURN UPDATED OBJECT
  # ======================================================================================================================================================================================

  return(object)

}
