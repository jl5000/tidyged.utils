

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
#' @tests
#' expect_equal(tidyged::gedcom() %>% tidyged::add_indi() %>% tidyged::add_famg() %>% identify_unused_records(),
#'              c("@I1@","@F1@"))
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
#' @tests
#' expect_snapshot_value(tidyged::sample555 %>% tidyged::add_indi() %>% remove_change_dates(), "json2")
remove_change_dates <- function(gedcom) {
  
  tidyged.internals::remove_section(gedcom, 1, "CHAN", "")
  
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
    xref <- dplyr::filter(gedcom, level == 0, tag == "NOTE", value == note)$record
    
    # if it doesn't exist create it
    if(length(xref) == 0) {
      gedcom <- tidyged::add_note(gedcom, note)
      
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




#' Split a tidyged object into two
#'
#' @param gedcom A tidyged object to split.
#' @param xrefs A vector of xrefs to put into the new tidyged object.
#'
#' @return A new tidyged object containing the xrefs specified. It will also have the same
#' header and submitter information as the input tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(split_gedcom(tidyged::sample555, c("@I1@","@S1@")), "json2")
split_gedcom <- function(gedcom,
                         xrefs) {
  
  xrefs <- c(xrefs, tidyged::xrefs_subm(gedcom))
  
  new <- gedcom %>% 
    dplyr::filter(record %in% c("HD", "TR", xrefs))
  
  links <- dplyr::filter(new, grepl(tidyged.internals::xref_pattern(), value)) %>% 
    dplyr::pull(value) %>% 
    unique()
  
  #links to records not retained
  absent <- dplyr::setdiff(c(xrefs, links), xrefs)
  
  if(length(absent) > 0) {

      absent_rows <- dplyr::filter(new, value %in% absent)
      
      for (i in seq_len(nrow(absent_rows))) {
        new <- tidyged.internals::remove_section(new, absent_rows$level[i], absent_rows$tag[i], absent_rows$value[i])
      }

      message("Some dead record references have been removed: ", paste(absent, collapse = ", "))

  }  
  
  new
}



#' Arrange all records in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param order A character string indicating the desired order of records. The letters
#' indicate (I)ndividual, (F)amily Group, (M)ultimedia, (S)ource, (R)epository, (N)ote.
#'
#' @return An arranged tidyged object.
#' @export
arrange_records <- function(gedcom, order = "IFMSRN") {
  
  if(nchar(order) != 6) stop("The order argument should have 6 characters")
  
  order <- stringr::str_replace(order, "M", "O")
  order <- strsplit(order, character())[[1]]
  subm_xref <- tidyged::xrefs_subm(gedcom)
  
  record_order <- dplyr::filter(gedcom, level == 0, !tag %in% c("HEAD","TRLR","SUBM")) %>% 
    dplyr::mutate(tag = stringr::str_sub(tag, 1, 1),
                  tag = factor(tag, levels = order, ordered = TRUE)) %>% 
    dplyr::arrange(tag) %>% 
    dplyr::pull(record) %>% 
    c("HD", subm_xref, ., "TR")
  
  gedcom %>% 
    dplyr::mutate(record = factor(record, levels = record_order, ordered = TRUE)) %>% 
    dplyr::arrange(record) %>%
    dplyr::mutate(record = as.character(record))
  
}


#' Determine whether a person is still alive from their date of birth
#'
#' @param date_of_birth A date string from the tidyged object.
#' @param max_age The maximum age to assume for a living person.
#'
#' @return A logical value indicating whether the person is still alive.
is_alive <- function(date_of_birth,
                     max_age) {
  
  if(date_of_birth == "" | stringr::str_detect(date_of_birth, "AFT")) return(TRUE)
  if(!stringr::str_detect(date_of_birth, "\\d{3,4}")) return(TRUE)
  
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  
  max_year <- stringr::str_extract_all(date_of_birth, "\\d{3,4}") %>% 
    unlist() %>% 
    as.integer() %>% 
    max()
  
  ifelse((this_year - max_year) > max_age, FALSE, TRUE)
}


#' Insert explicit death subrecords
#' 
#' This function inserts explicit death subrecords for individuals who have a date of birth that
#' makes them older than a maximum age.
#'
#' @param gedcom A tidyged object.
#' @param max_age The maximum age to assume for a living person.
#'
#' @return An updated tidyged object with additional death subrecords.
#' @export
insert_explicit_death_subrecords <- function(gedcom, max_age = 120) {
  
  indi_xrefs <- tidyged::xrefs_indi(gedcom)
  
  indi_with_birth <- unique(dplyr::filter(gedcom, record %in% indi_xrefs, level == 1, tag == "BIRT")$record)
  
  indi_with_death <- unique(dplyr::filter(gedcom, record %in% indi_with_birth, level == 1, tag == "DEAT")$record)
  
  indi_no_death <- dplyr::setdiff(indi_with_birth, indi_with_death)
    
  for(xref in indi_no_death) {
    
    dob <- tidyged.internals::gedcom_value(gedcom, xref, "DATE", 2, "BIRT")
    
    if(!is_alive(dob, max_age)) {
      next_row <- tidyged.internals::find_insertion_point(gedcom, xref, 0, "INDI")
      gedcom <- tibble::add_row(gedcom,
                                tibble::tibble(record = xref, level = 1, tag = "DEAT", value = "Y"),
                                .before = next_row)
    }
    
  }
  
  gedcom
}


#' Order children in all Family Group records by birth date
#'
#' @param gedcom A tidyged object.
#'
#' @return The same tidyged object with rearranged children rows in the Family Group records.
#' @export
order_famg_children_all <- function(gedcom) {
  
  fam_xrefs <- tidyged::xrefs_famg(gedcom)
  
  for(xref in fam_xrefs) {
    gedcom <- tidyged::order_famg_children(gedcom, xref)
  }
  gedcom
}
