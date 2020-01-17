#' copies group into another group object so same group can be placed and/or rescaled
#'
#' @param group a "bb_group" object, as outputted from bb_groupPlots
#'
#' @return Function will return an updated "bb_group" object
#'
#' @export
#'
bb_copyGroup <- function(group){

  # ======================================================================================================================================================================================
  # FUNCTIONS
  # ======================================================================================================================================================================================

  ## Error function

  ## Define a function that renames and copies grobs
  copy_grobs <- function(grob){

    new_name <- grobName(grob)
    grob$name <- new_name
    return(grob)

  }

  ## Define a function that copies and renames the grobs in each gtree into a new gtree
  rename_gtree_grobs <- function(gtree){

    new_gtree <- gTree(vp = gtree$vp)

    renamed_grobs <- lapply(gtree$children, copy_grobs)
    class(renamed_grobs) <- "gList"

    new_gtree <- setChildren(x = new_gtree, children = renamed_grobs)

    return(new_gtree)

  }

  # ======================================================================================================================================================================================
  # COPY GTREE GROBS WITH NEW NAMES INTO NEW GTREES
  # ======================================================================================================================================================================================

  sub_gtrees <- group$grobs$children

  new_gtrees <- lapply(sub_gtrees, rename_gtree_grobs)
  class(new_gtrees) <- "gList"

  # ======================================================================================================================================================================================
  # ADD NEW GTREES TO LARGER CONTAINER GTREE
  # ======================================================================================================================================================================================

  new_container_gtree <- gTree(vp = group$grobs$vp)
  new_container_gtree <- setChildren(x = new_container_gtree, children = new_gtrees)

  # ======================================================================================================================================================================================
  # ADD NEW GTREE TO OBJECT
  # ======================================================================================================================================================================================

  group$grobs <- new_container_gtree

  # ======================================================================================================================================================================================
  # RETURN UPDATED OBJECT
  # ======================================================================================================================================================================================

  return(group)

}
