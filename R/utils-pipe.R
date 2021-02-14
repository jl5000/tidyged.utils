#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

xrefs_individuals <- utils::getFromNamespace("xrefs_individuals", "tidyged")
xrefs_families <- utils::getFromNamespace("xrefs_families", "tidyged")
xrefs_multimedia <- utils::getFromNamespace("xrefs_multimedia", "tidyged")
xrefs_sources <- utils::getFromNamespace("xrefs_sources", "tidyged")
xrefs_repositories <- utils::getFromNamespace("xrefs_repositories", "tidyged")
xrefs_notes <- utils::getFromNamespace("xrefs_notes", "tidyged")
remove_section <- utils::getFromNamespace("remove_section", "tidyged")
get_valid_xref <- utils::getFromNamespace("get_valid_xref", "tidyged")