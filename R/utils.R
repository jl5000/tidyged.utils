

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
  
  xrefs_indi <- tidyged::xrefs_indi(gedcom)
  xrefs_fam <- tidyged::xrefs_famg(gedcom)
  xrefs_media <- tidyged::xrefs_media(gedcom)
  xrefs_sour <- tidyged::xrefs_sour(gedcom)
  xrefs_repo <- tidyged::xrefs_repo(gedcom)
  xrefs_note <- tidyged::xrefs_note(gedcom)
  
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
    
    existing_notes <- tidyged::xrefs_note(gedcom)
    
    # get xrefs of existing note record
    xref <- gedcom %>%
      dplyr::filter(level == 0, tag == "NOTE", value == note) %>% 
      dplyr::pull(record)
    
    # if it doesn't exist create it
    if(length(xref) == 0) {
      gedcom <- gedcom %>% 
        tidyged::add_note(note)
      
      new_notes <- tidyged::xrefs_note(gedcom)
      
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
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
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


#' Split a tidygedcom object into two
#'
#' @param gedcom A tidygedcom object to split.
#' @param xrefs A vector of xrefs to put into the new tidyged object.
#' @param remove_dead_refs Whether to remove references to records not in the new tidyged object.
#'
#' @return A new tidyged object containing the xrefs specified. It will also have the same
#' header and submitter information as the input tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(split_gedcom(sample555, c("@I1@","@S1@")), "json2")
#' expect_snapshot_value(split_gedcom(sample555, c("@I1@","@S1@"), FALSE), "json2")
split_gedcom <- function(gedcom,
                         xrefs,
                         remove_dead_refs = TRUE) {
  
  xrefs <- c(xrefs, tidyged::xrefs_subm(gedcom))
  
  new <- gedcom %>% 
    dplyr::filter(record %in% c("HD", "TR", xrefs))
  
  links <- dplyr::filter(new, grepl(xref_pattern(), value)) %>% 
    dplyr::pull(value) %>% 
    unique()
  
  #links to records not retained
  absent <- dplyr::setdiff(c(xrefs, links), xrefs)
  
  if(length(absent) > 0) {
    if(remove_dead_refs) {
      absent_rows <- dplyr::filter(new, value %in% absent)
      
      for (i in 1:nrow(absent_rows)) {
        new <- remove_section(new, absent_rows$level[i], absent_rows$tag[i], absent_rows$value[i])
      }
    } else {
      message("Some record references are dead: ", paste(absent, collapse = ", "))
    }
  }  
  
  new
}

#' Merge two tidygedcom objects
#'
#' @param gedcom1 The first tidygedcom object to merge.
#' @param gedcom2 The second tidygedcom object to merge.
#'
#' @return A new tidyged object containing the records of both input objects. 
#' It will also have the same header and submitter information as the first input tidyged object.
#' @export
merge_gedcoms <- function(gedcom1, gedcom2) {
  
  #find duplicate records
  
  #use header/subm info from first gedcom
  
  # make all xrefs unique
  
  
}