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

remove_section <- utils::getFromNamespace("remove_section", "tidyged")
get_valid_xref <- utils::getFromNamespace("get_valid_xref", "tidyged")


##' Construct a regular expression for an xref
##' 
##' See \code{tidyged.internals::\link[tidyged.internals:xref_pattern]{xref_pattern}} for details.
##' 
##' @importFrom tidyged.internals xref_pattern
##' @name xref_pattern
NULL