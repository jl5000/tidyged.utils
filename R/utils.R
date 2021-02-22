

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
#' expect_snapshot_value(tidyged::gedcom() %>% tidyged::add_indi() %>% remove_change_dates(), "json2")
remove_change_dates <- function(gedcom) {
  
  gedcom %>% 
    tidyged.internals::remove_section(1, "CHAN", "")
  
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
#' @param remove_dead_refs Whether to remove references to records not in the new tidyged object.
#'
#' @return A new tidyged object containing the xrefs specified. It will also have the same
#' header and submitter information as the input tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(split_gedcom(tidyged::sample555, c("@I1@","@S1@")), "json2")
#' expect_snapshot_value(split_gedcom(tidyged::sample555, c("@I1@","@S1@"), FALSE), "json2")
split_gedcom <- function(gedcom,
                         xrefs,
                         remove_dead_refs = TRUE) {
  
  xrefs <- c(xrefs, tidyged::xrefs_subm(gedcom))
  
  new <- gedcom %>% 
    dplyr::filter(record %in% c("HD", "TR", xrefs))
  
  links <- dplyr::filter(new, grepl(tidyged.internals::xref_pattern(), value)) %>% 
    dplyr::pull(value) %>% 
    unique()
  
  #links to records not retained
  absent <- dplyr::setdiff(c(xrefs, links), xrefs)
  
  if(length(absent) > 0) {
    if(remove_dead_refs) {
      absent_rows <- dplyr::filter(new, value %in% absent)
      
      for (i in seq_len(nrow(absent_rows))) {
        new <- tidyged.internals::remove_section(new, absent_rows$level[i], absent_rows$tag[i], absent_rows$value[i])
      }
    } else {
      message("Some record references are dead: ", paste(absent, collapse = ", "))
    }
  }  
  
  new
}

#' Merge two tidyged objects
#'
#' @param gedcom1 The first tidyged object to merge.
#' @param gedcom2 The second tidyged object to merge.
#'
#' @return A new tidyged object containing the records of both input objects. 
#' It will also have the same header and submitter information as the first input tidyged object.
#' @export
merge_gedcoms <- function(gedcom1, gedcom2) {
  
   migrate_records(gedcom1, gedcom2) %>% 
    potential_duplicates_indi() %>% 
    potential_duplicates_famg() %>% 
    potential_duplicates_sour() %>% 
    potential_duplicates_repo() %>% 
    potential_duplicates_media()
}


potential_duplicates_indi <- function(gedcom) {
  # given + surname identical, yob, yod (2 year error)
  ind_xrefs <- tidyged::xrefs_indi(gedcom)
  
  given <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "GIVN", level = 2)
  surname <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "SURN", level = 2)
  dob <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "BIRT")
  dod <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "DEAT")
  
  yob <- stringr::str_extract(dob, "\\d{3,4}")
  yod <- stringr::str_extract(dod, "\\d{3,4}")
  
  comb <- tibble::tibble(xref = ind_xrefs,
                         given = given,
                         surname = surname,
                         yob = yob,
                         yod = yod) %>% 
    dplyr::filter(!is.na(given), !is.na(surname)) %>% 
    dplyr::mutate(full = paste(given, surname))
  
  # use a join to identify similar rows
  for (i in seq_along(nrow(comb))) {
    this_row <- comb[i,]
    dupes <- dplyr::left_join(this_row, comb, by = "full")$xref
    
  }
  # give user option of merging them
  comb
}


merge_records <- function(gedcom, xrefs) {
  
  # check records are of the same type
  
  
  # merge and then remove duplicate subrecords
  gedcom <- merge_subrecords(gedcom, xrefs)
  
  for (xref in xrefs) {
    gedcom <- remove_duplicate_subrecords(gedcom, xref)
  }
  
}

merge_subrecords <- function(gedcom, xrefs) {
  
  
}

remove_duplicate_subrecords <- function(gedcom, xref) {
  
  record <- dplyr::filter(gedcom, record == xref)
    
  new_record <- dplyr::mutate(record, new_sub = level == 1,
                              subrecord_no = cumsum(new_sub)) %>% 
    dplyr::select(-new_sub) %>% 
    dplyr::group_nest(subrecord_no) %>% 
    dplyr::distinct() %>% 
    tidyr::unnest(data) %>% 
    dplyr::select(-subrecord_no)
  
  dplyr::filter(gedcom, record != xref) %>% 
    tibble::add_row(new_record, .before = nrow(.))
  
}

migrate_records <- function(gedcom1, gedcom2) {
  
 # update xrefs in gedcom2
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_indi, tidyged.internals::assign_xref_indi)
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_famg, tidyged.internals::assign_xref_famg)
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_sour, tidyged.internals::assign_xref_sour)
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_repo, tidyged.internals::assign_xref_repo)
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_media, tidyged.internals::assign_xref_media)
  gedcom2 <- update_xrefs_of_type(gedcom1, gedcom2, tidyged::xrefs_note, tidyged.internals::assign_xref_note)
  
  new_xrefs <- c(tidyged::xrefs_indi(gedcom2), tidyged::xrefs_famg(gedcom2),
                 tidyged::xrefs_sour(gedcom2), tidyged::xrefs_repo(gedcom2),
                 tidyged::xrefs_media(gedcom2), tidyged::xrefs_note(gedcom2))
  
# move xrefs to gedcom1
  records_to_move <- dplyr::filter(gedcom2, record %in% new_xrefs)
  tibble::add_row(gedcom1, records_to_move, .before = nrow(gedcom1))
}

update_xrefs_of_type <- function(gedcom1, gedcom2, all_xrefs_fn, assign_xref_fn) {

  old_xrefs <- all_xrefs_fn(gedcom2)
  if (length(old_xrefs) == 0) return(gedcom2)
  
  new_xrefs <- assign_xref_fn(gedcom1, quantity = length(old_xrefs))
  for (i in seq_along(old_xrefs)) {
    gedcom2 <- dplyr::mutate(gedcom2,
                             record = ifelse(record == old_xrefs[i], new_xrefs[i], record),
                             value = ifelse(value == old_xrefs[i], new_xrefs[i], value))
  }

  gedcom2
  
}

