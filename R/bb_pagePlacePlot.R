#' places a plot that has been previously created but not drawn
#'
#' @param plot plot to be placed
#' @param params an optional "bb_params" object space containing relevant function parameters
#' @param x A numeric or unit object specifying x-location.
#' @param y A numeric or unit object specifying y-location.
#' @param width A numeric or unit object specifying width.
#' @param height A numeric or unit object specifying height.
#' @param just A string or numeric vector specifying the justification of the plot relative to its (x, y) location
#' @param default.units A string indicating the default units to use if x, y, width, or height are only given as numeric vectors
#' @param draw A logical value indicating whether graphics output should be produced
#'
#' @return Function will update dimensions of an input plot and return an updated BentoBox plot object
#'
#'
#' @export
bb_pagePlacePlot <- function(plot, params = NULL, x = NULL, y = NULL, width = NULL, height = NULL, just = c("left", "top"),
                         default.units = "inches", draw = T){

  # ======================================================================================================================================================================================
  # FUNCTIONS
  # ======================================================================================================================================================================================

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

  ## Define a function to parse coordinates
  parse_coordinates <- function(input_plot, output_plot){

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
    # output_plot$x <- new_coords$x
    # output_plot$y <- new_coords$y
    # output_plot$width <- new_coords$width
    # output_plot$height <- new_coords$height

    output_plot <- set_values(object = output_plot, x = new_coords$x, y = new_coords$y, width = new_coords$width, height = new_coords$height)
    output_plot$justification <- new_coords$justification

    ## Return object
    return(output_plot)

  }

  ## Define a function that renames grobs and copies to a new gtree
  copy_grobs <- function(grob){

    new_name <- grobName(grob)

    grob$name <- new_name
    assign("new_gtree", addGrob(gTree = get("new_gtree", envir = bbEnv), child = grob), envir = bbEnv)

  }

  # ======================================================================================================================================================================================
  # PARSE PARAMETERS
  # ======================================================================================================================================================================================

  ## Check which defaults are not overwritten and set to NULL
  if(missing(just)) just <- NULL
  if(missing(default.units)) default.units <- NULL
  if(missing(draw)) draw <- NULL

  ## Check if plot argument is missing (could be in object)
  if(!hasArg(plot)) plot <- NULL

  ## Compile all parameters into an internal object
  bb_place <- structure(list(plot = plot, x = x, y = y, width = width, height = height, draw = draw,
                                     just = just, default.units = default.units), class = "bb_place")

  bb_place <- parseParams(bb_params = params, object_params = bb_place)

  ## For any defaults that are still NULL, set back to default
  if(is.null(bb_place$just)) bb_place$just <- c("left", "top")
  if(is.null(bb_place$default.units)) bb_place$default.units <- "inches"
  if(is.null(bb_place$draw)) bb_place$draw <- TRUE

  # ======================================================================================================================================================================================
  # ERRORS
  # ======================================================================================================================================================================================

  if(is.null(bb_place$plot)) stop("argument \"plot\" is missing, with no default.", call. = FALSE)

  # ======================================================================================================================================================================================
  # INITIALIZE PLOT OBJECT COPY
  # ======================================================================================================================================================================================

  object <- bb_place$plot

  # ======================================================================================================================================================================================
  # PARSE UNITS FOR INPUTS
  # ======================================================================================================================================================================================

  if (!is.null(bb_place$x)){

    if (!"unit" %in% class(bb_place$x)){

      if (!is.numeric(bb_place$x)){

        warning("x-coordinate is neither a unit object or a numeric value. Cannot parse x-coordinate.", call. = FALSE)
        bb_place$x <- NULL

      }

      if (is.null(bb_place$default.units)){

        warning("x-coordinate detected as numeric.\'default.units\' must be specified.", call. = FALSE)
        bb_place$x <- NULL

      }

      bb_place$x <- unit(bb_place$x, bb_place$default.units)

    }
  }

  if (!is.null(bb_place$y)){

    if (!"unit" %in% class(bb_place$y)){

      if (!is.numeric(bb_place$y)){

        warning("y-coordinate is neither a unit object or a numeric value. Cannot parse y-coordinate.", call. = FALSE)
        bb_place$y <- NULL

      }

      if (is.null(bb_place$default.units)){

        warning("y-coordinate detected as numeric.\'default.units\' must be specified.", call. = FALSE)
        bb_place$y <- NULL

      }

      bb_place$y <- unit(bb_place$y, bb_place$default.units)

    }
  }

  if (!is.null(bb_place$width)){

    if (!"unit" %in% class(bb_place$width)){

      if (!is.numeric(bb_place$width)){

        warning("width is neither a unit object or a numeric value. Cannot parse width.", call. = FALSE)
        bb_place$width <- NULL

      }

      if (is.null(bb_place$default.units)){

        warning("width detected as numeric.\'default.units\' must be specified.", call. = FALSE)
        bb_place$width <- NULL

      }

      bb_place$width <- unit(bb_place$width, bb_place$default.units)

    }
  }

  if (!is.null(bb_place$height)){

    if (!"unit" %in% class(bb_place$height)){

      if (!is.numeric(bb_place$height)){

        warning("height is neither a unit object or a numeric value. Cannot parse height.", call. = FALSE)
        bb_place$height <- NULL


      }

      if (is.null(bb_place$default.units)){

        warning("height detected as numeric.\'default.units\' must be specified.", call. = FALSE)
        bb_place$height <- NULL

      }

      bb_place$height <- unit(bb_place$height, bb_place$default.units)

    }
  }

  # ======================================================================================================================================================================================
  # UPDATE DIMENSIONS AND COORDINATES OF PLOT OBJECT BASED ON INPUTS
  # ======================================================================================================================================================================================

  object <- set_values(object = object, x = bb_place$x, y = bb_place$y, width = bb_place$width, height = bb_place$height)
  object$justification <- bb_place$just
  attr(x = object, which = "plotted") <- bb_place$draw

  # ======================================================================================================================================================================================
  # INHERIT DIMENSIONS/COOORDINATES WHERE NULL
  # ======================================================================================================================================================================================

  object <- parse_coordinates(input_plot = bb_place$plot, output_plot = object)

  # ======================================================================================================================================================================================
  # CALL ERRORS
  # ======================================================================================================================================================================================

  check_placement(object = object)

  # ======================================================================================================================================================================================
  # DEFINE A NEW VIEWPORT
  # ======================================================================================================================================================================================

  ## Get viewport name
  currentViewports <- current_viewports()
  vp_name <- paste0(gsub(pattern = "[0-9]", replacement = "", x = object$grobs$vp$name), length(grep(pattern = gsub(pattern = "[0-9]", replacement = "", x = object$grobs$vp$name), x = currentViewports)) + 1)


  ## If full placing information isn't provided but plot == TRUE, set up it's own viewport separate from bb_makepage
  ## Not translating into page_coordinates
  if (is.null(object$x) | is.null(object$y) | is.null(object$width) | is.null(object$height)){

    new_vp <- viewport(height = unit(1, "snpc"), width = unit(1, "snpc"),
                       x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       clip = "on",
                       xscale = object$grobs$vp$xscale, yscale = object$grobs$vp$yscale,
                       just = "center",
                       name = vp_name)

    if (bb_place$draw == TRUE){

      grid.newpage()
      warning("Plot placement will only fill up the graphical device.", call. = FALSE)

    }

  } else {

    ## Convert coordinates into same units as page
    page_coords <- convert_page(object = object)

    ## Make viewport
    new_vp <- viewport(height = page_coords$height, width = page_coords$width,
                       x = page_coords$x, y = page_coords$y,
                       clip = "on",
                       xscale = object$grobs$vp$xscale, yscale = object$grobs$vp$yscale,
                       just = bb_place$just,
                       name = vp_name)
  }

  # ======================================================================================================================================================================================
  # RENAME GROBS
  # ======================================================================================================================================================================================

  assign("new_gtree", gTree(vp = new_vp), envir = bbEnv)
  invisible(lapply(object$grobs$children, copy_grobs))

  # ======================================================================================================================================================================================
  # ASSIGN NEW GROBS TO OBJECT
  # ======================================================================================================================================================================================

  object$grobs <- get("new_gtree", envir = bbEnv)

  # ======================================================================================================================================================================================
  # IF DRAW == TRUE, DRAW GROBS
  # ======================================================================================================================================================================================

  if (bb_place$draw == TRUE){

    grid.draw(object$grobs)

  }

  # ======================================================================================================================================================================================
  # RETURN UPDATED OBJECT
  # ======================================================================================================================================================================================

  return(object)


}
