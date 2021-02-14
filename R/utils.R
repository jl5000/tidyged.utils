

#' Identify unreferenced records
#' 
#' This function identifies records that are not referenced in any other records.
#' 
#' @details You would expect every record to be referenced by another in some way. For example, Individual
#' records should reference Family Group records (and vice-versa), Repository records should be referenced
#' by Source records, and Source records should be cited by other records.
#' 
#' You can use the output of this function with tidyged::remove_records() to remove them, or 
#' tidyged::describe_records() to find out more about them.
#'
#' @param gedcom A tidyged object.
#'
#' @return A vector of xrefs that are not referenced anywhere else in the tidyged object.
#' @export
identify_unused_records <- function(gedcom) {
  
  xrefs_indi <- xrefs_individuals(gedcom)
  xrefs_fam <- xrefs_families(gedcom)
  xrefs_media <- xrefs_multimedia(gedcom)
  xrefs_sour <- xrefs_sources(gedcom)
  xrefs_repo <- xrefs_repositories(gedcom)
  xrefs_note <- xrefs_notes(gedcom)
  
  # get unattached individuals
  attached <- unique(dplyr::filter(gedcom, record %in% xrefs_fam, tag %in% c("HUSB","WIFE","CHIL"))$value)
  unattached <- dplyr::setdiff(xrefs_indi, attached)
  
  #also look at family links perspective to check consistency
  attached2 <- unique(dplyr::filter(gedcom, record %in% xrefs_indi, tag %in% c("FAMC","FAMS"))$record)
  unattached2 <- dplyr::setdiff(xrefs_indi, attached2)
  
  if (!identical(sort(attached), sort(attached2)))
    warning("Family group membership and individual family links are inconsistent")
  
  # get empty families
  nonempty <- unique(dplyr::filter(gedcom, record %in% xrefs_fam, tag %in% c("HUSB","WIFE","CHIL"))$record)
  empty <- dplyr::setdiff(xrefs_fam, nonempty) 
  
  #get unused media
  used_media <- unique(dplyr::filter(gedcom, !record %in% xrefs_repo, tag == "OBJE")$value)
  unused_media <- dplyr::setdiff(xrefs_media, used_media) 
  
  #get unused sources
  used_sour <- unique(dplyr::filter(gedcom, !record %in% xrefs_sour, tag == "SOUR")$value)
  unused_sour <- dplyr::setdiff(xrefs_sour, used_sour)
  
  #get unused repos
  used_repo <- unique(dplyr::filter(gedcom, !record %in% xrefs_repo, tag == "REPO")$value)
  unused_repo <- dplyr::setdiff(xrefs_repo, used_repo)
  
  #get unused notes
  used_notes <- unique(dplyr::filter(gedcom, !record %in% xrefs_note, tag == "NOTE")$value)
  unused_notes <- dplyr::setdiff(xrefs_note, used_notes)
  
  c(unattached, empty, unused_media, unused_sour, unused_repo, unused_notes)
  
}


#' Remove all CHANge dates from a tidyged object
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with all CHAN structures removed.
#' @export
remove_change_dates <- function(gedcom) {
  
  gedcom %>% 
    remove_section(1, "CHAN", "")
  
}



#' Consolidate duplicated notes
#'
#' @param gedcom A tidyged object.
#' @param min_occurences How many duplicates to prompt creating a new Note record.
#'
#' @return A tidyged object with all notes consolidated.
#' @export
consolidate_notes <- function(gedcom, min_occurences = 2) {
  
  note_dupes <- gedcom %>% 
    #get all note structures
    dplyr::filter(level > 0, tag == "NOTE") %>% 
    dplyr::group_by(value) %>% 
    dplyr::filter(dplyr::n() >= min_occurences) %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(value) %>% 
    unique()
  
  for(note in note_dupes) {
    
    existing_notes <- xrefs_notes(gedcom)
    
    # get xrefs of existing note record
    xref <- gedcom %>%
      dplyr::filter(level == 0, tag == "NOTE", value == note) %>% 
      dplyr::pull(record)
    
    # if it doesn't exist create it
    if(length(xref) == 0) {
      gedcom <- gedcom %>% 
        tidyged::add_note(note)
      
      new_notes <- xrefs_notes(gedcom)
      
      xref <- dplyr::setdiff(new_notes, existing_notes)
    } 
    
    # change notes to references
    gedcom <- gedcom %>% 
      dplyr::mutate(value = dplyr::if_else(level > 0 & tag == "NOTE" & value == note,
                                           xref,
                                           value))
  }
  
  gedcom
}


#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref or name of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_individual Whether to also include the individual themselves.
#' @param include_spouses Whether to also include all spouses of this individual (and their descendants).
#' @param include_families Whether to also include all Family Group records where this individual is a spouse.
#'
#' @return A vector of xrefs of descendants.
#' @export
identify_descendants <- function(gedcom,
                                 individual = character(),
                                 include_individual = FALSE,
                                 include_spouses = FALSE,
                                 include_families = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, is_indi)
  
  return_xrefs <- NULL
  
  spou_xref <- tidyged::get_spouses(gedcom, xref)
  chil_xref <- tidyged::get_children(gedcom, xref)
  fams_xref <- tidyged::get_families_as_spouse(gedcom, xref)
  
  # if spouse is to be included, add their children to be included
  if (include_spouses) {
    # we don't use purrr::map here because the return values could vary in length
    spou_chil <- NULL
    for(i in seq_along(spou_xref)) {
      spou_chil <- c(spou_chil, tidyged::get_children(gedcom, spou_xref[i]))
    }
    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (include_families) return_xrefs <- c(return_xrefs, fams_xref)
  if (include_spouses) return_xrefs <- c(return_xrefs, spou_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      identify_descendants(gedcom, chil_xref[i], TRUE, TRUE,TRUE))
  }
  
  return_xrefs
}


identify_ancestors <- function(gedcom,
                               individual = character(),
                               include_individual = TRUE,
                               include_siblings = FALSE,
                               include_families = FALSE) {
  
  
  
}


#' Remove all creation dates from a tidyged object
#' 
#' @details This is a function used in tests so that the objects created do not
#' change every time.
#'
#' @param gedcom A tidyged object.
#'
#' @return The tidyged object with creation dates removed.
#' @tests
#' expect_snapshot_value(remove_dates_for_tests(tidyged::sample555), "json2")
remove_dates_for_tests <- function(gedcom) {
  
  gedcom %>% 
    remove_change_dates() %>% 
    dplyr::filter(!(level == 1 & record == "HD" & tag == "DATE"))
  
}