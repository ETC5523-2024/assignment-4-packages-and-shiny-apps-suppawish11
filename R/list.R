#' Entity list in the space_objects dataset
#'
#' @description
#' Print list of entities that is in space_objects the dataset
#'
#'
#' @return Entity list
#' @export
#'
#' @examples
#' entity_list()
entity_list <- function() {
  unique(spaceobs::space_objects$entity)
}



#' Year range of selected entity
#'
#' @param entity
#'
#' @return Return range of the year of the selected entity in the space_objects dataset
#' @export
#'
#' @examples
#' year_entity()
year_entity <- function(entity) {

  if(!(entity %in% spaceobs::entity_list())) {
    stop("Plese insert the entity name from spaceobs::entity_list()")
  }

  else {
    entity <- spaceobs::space_objects |>
    dplyr::filter(.data$entity == {{entity}})

  range(entity$year)
  }
}
