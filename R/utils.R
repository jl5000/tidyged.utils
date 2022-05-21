

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
#' @param tg A tidyged object.
#'
#' @return A vector of xrefs that are not referenced anywhere else in the tidyged object.
#' @export
#' @tests
#' expect_equal(tidyged::gedcom() |> tidyged::add_indi() |> tidyged::add_famg() |> identify_unused_records(),
#'              c("@I1@","@F1@"))
identify_unused_records <- function(tg) {
  
  xrefs_indi <- tidyged::xrefs_indi(tg)
  xrefs_fam <- tidyged::xrefs_famg(tg)
  xrefs_media <- tidyged::xrefs_media(tg)
  xrefs_sour <- tidyged::xrefs_sour(tg)
  xrefs_repo <- tidyged::xrefs_repo(tg)
  xrefs_note <- tidyged::xrefs_note(tg)
  
  # get unattached individuals
  attached <- unique(dplyr::filter(tg, record %in% xrefs_fam, level == 1, 
                                   tag %in% c("HUSB","WIFE","CHIL"))$value)
  unattached <- dplyr::setdiff(xrefs_indi, attached)
  
  #also look at family links perspective to check consistency
  attached2 <- unique(dplyr::filter(tg, record %in% xrefs_indi, tag %in% c("FAMC","FAMS"))$record)
  unattached2 <- dplyr::setdiff(xrefs_indi, attached2)
  
  if (!identical(sort(attached), sort(attached2)))
    warning("Family group membership and individual family links are inconsistent")
  
  # get empty families
  nonempty <- unique(dplyr::filter(tg, record %in% xrefs_fam, level == 1,
                                   tag %in% c("HUSB","WIFE","CHIL"))$record)
  empty <- dplyr::setdiff(xrefs_fam, nonempty) 
  
  #get unused media
  used_media <- unique(dplyr::filter(tg, !record %in% xrefs_repo, tag == "OBJE")$value)
  unused_media <- dplyr::setdiff(xrefs_media, used_media) 
  
  #get unused sources
  used_sour <- unique(dplyr::filter(tg, !record %in% xrefs_sour, tag == "SOUR")$value)
  unused_sour <- dplyr::setdiff(xrefs_sour, used_sour)
  
  #get unused repos
  used_repo <- unique(dplyr::filter(tg, !record %in% xrefs_repo, tag == "REPO")$value)
  unused_repo <- dplyr::setdiff(xrefs_repo, used_repo)
  
  #get unused notes
  used_notes <- unique(dplyr::filter(tg, !record %in% xrefs_note, tag == "NOTE")$value)
  unused_notes <- dplyr::setdiff(xrefs_note, used_notes)
  
  c(unattached, empty, unused_media, unused_sour, unused_repo, unused_notes)
  
}


#' Remove all CHANge dates from a tidyged object
#'
#' @param tg A tidyged object.
#'
#' @return A tidyged object with all CHAN structures removed.
#' @export
#' @tests
#' expect_snapshot_value(tidyged::sample555 |> tidyged::add_indi() |> remove_change_dates(), "json2")
remove_change_dates <- function(tg) {
  
  tidyged.internals::remove_section(tg, 1, "CHAN", "")
  
}



#' Consolidate duplicated notes
#'
#' @param tg A tidyged object.
#' @param min_occurences How many duplicates to prompt creating a new Note record.
#'
#' @return A tidyged object with all notes consolidated.
#' @export
consolidate_notes <- function(tg, min_occurences = 2) {
  
  note_dupes <- tg |> 
    #get all note structures
    dplyr::filter(level > 0, tag == "NOTE") |> 
    dplyr::group_by(value) |> 
    dplyr::filter(dplyr::n() >= min_occurences) |> 
    dplyr::ungroup() |> 
    dplyr::pull(value) |> 
    unique()
  
  for(note in note_dupes) {
    
    existing_notes <- tidyged::xrefs_note(tg)
    
    # get xrefs of existing note record
    xref <- dplyr::filter(tg, level == 0, tag == "NOTE", value == note)$record
    
    # if it doesn't exist create it
    if(length(xref) == 0) {
      tg <- tidyged::add_note(tg, note)
      
      new_notes <- tidyged::xrefs_note(tg)
      
      xref <- dplyr::setdiff(new_notes, existing_notes)
    } 
    
    # change notes to references
    tg <- tg |> 
      dplyr::mutate(value = dplyr::if_else(level > 0 & tag == "NOTE" & value == note,
                                           xref,
                                           value))
  }
  
  tg
}




#' Split a tidyged object into two
#'
#' @param tg A tidyged object to split.
#' @param xrefs A vector of xrefs to put into the new tidyged object.
#'
#' @return A new tidyged object containing the xrefs specified. It will also have the same
#' header and submitter information as the input tidyged object.
#' @export
#' @tests
#' expect_snapshot_value(split_gedcom(tidyged::sample555, c("@I1@","@S1@")), "json2")
split_gedcom <- function(tg,
                         xrefs) {
  
  xrefs <- c(xrefs, tidyged::xrefs_subm(tg))
  
  new <- dplyr::filter(tg, record %in% c("HD", "TR", xrefs))
  
  links <- dplyr::filter(new, grepl(tidyged.internals::reg_xref(TRUE), value)) |> 
    dplyr::pull(value) |> 
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
#' This function groups together all records of a particular type and puts them in a specific order.
#' This rearrangement makes no functional difference to the file, it just makes it more organised.
#'
#' @param tg A tidyged object.
#' @param order A character string indicating the desired order of records. The letters
#' indicate (I)ndividual, (F)amily Group, (M)ultimedia, (S)ource, (R)epository, (N)ote.
#'
#' @return An arranged tidyged object.
#' @export
arrange_records <- function(tg, order = "IFMSRN") {
  
  if(nchar(order) != 6) stop("The order argument should have 6 characters")
  
  order <- stringr::str_replace(order, "M", "O")
  order <- strsplit(order, character())[[1]]
  subm_xref <- tidyged::xrefs_subm(tg)
  
  record_order <- dplyr::filter(tg, level == 0, !tag %in% c("HEAD","TRLR","SUBM")) |> 
    dplyr::mutate(tag = stringr::str_sub(tag, 1, 1),
                  tag = factor(tag, levels = order, ordered = TRUE)) |> 
    dplyr::arrange(tag) |> 
    dplyr::pull(record)
  
  record_order <- c("HD", subm_xref, record_order, "TR")
  
  tg |> 
    dplyr::mutate(record = factor(record, levels = record_order, ordered = TRUE)) |> 
    dplyr::arrange(record) |>
    dplyr::mutate(record = as.character(record))
  
}





#' Insert explicit death subrecords
#' 
#' This function inserts explicit death subrecords for individuals who have a date of birth that
#' makes them older than a maximum age.
#'
#' @param tg A tidyged object.
#' @param max_age The maximum age to assume for a living person.
#' @param guess If a date of birth cannot be found, whether to guess it from other information.
#' @param explan_note Text to include in an explanatory note for any added death subrecords. An empty string will not add a note.
#'
#' @return An updated tidyged object with additional death subrecords.
#' @export
insert_explicit_death_subrecords <- function(tg, 
                                             max_age = 120,
                                             guess = FALSE,
                                             explan_note = "This death event has been inferred automatically from other facts") {
  
  indi_xrefs <- tidyged::xrefs_indi(tg)
  
  for(xref in indi_xrefs) {
    death_events <- dplyr::filter(tg, record == xref, tag == "DEAT")
    
    if(nrow(death_events) == 0) {
      
      dob <- tidyged.internals::gedcom_value(tg, xref, "DATE", 2, "BIRT")
      
      if(dob == "") {
        if(!guess) next
        age <- guess_age(tg, xref)
      } else {
        age <- date_diff(dob, minimise = TRUE)
      }
      
      if(age < 0) next
      if(age > max_age) {
        next_row <- tidyged.internals::find_insertion_point(tg, xref, 0, "INDI")
        tg <- tibble::add_row(tg,
                              tibble::tibble(record = xref, level = 1, tag = "DEAT", value = ""),
                              .before = next_row)
        
        if(nchar(explan_note) > 0)
          tg <- tibble::add_row(tg,
                                tibble::tibble(record = xref, level = 2, tag = "NOTE", value = explan_note),
                                .before = next_row + 1)
        
        tg <- tidyged::update_change_date(tg, xref)
      }
      
      
    }
    
  }
  
  tg
}


#' Order children in all Family Group records by birth date
#'
#' @param tg A tidyged object.
#'
#' @return The same tidyged object with rearranged children rows in the Family Group records.
#' @export
order_famg_children_all <- function(tg) {
  
  fam_xrefs <- tidyged::xrefs_famg(tg)
  
  for(xref in fam_xrefs) {
    tg <- tidyged::order_famg_children(tg, xref)
  }
  tg
}

#' Insert explicit marriage subrecords
#' 
#' This function inserts explicit marriage subrecords for Family Group records that do not have one.
#' 
#' @details The GEDCOM specification recommends that all marriage events have an explicit TYPE subrecord
#' subordinate to the MARR relationship event. If one is not given, marriage is assumed.
#'
#' @param tg A tidyged object.
#'
#' @return An updated tidyged object with additional marriage subrecords.
#' @export
insert_explicit_marr_types_all <- function(tg){
  
  fam_xrefs <- tidyged::xrefs_famg(tg)
  
  for(xref in fam_xrefs) {
    tg <- tidyged::insert_explicit_marr_types(tg, xref)
  }
  tg
  
}


#' Remove data for living individuals in a tidyged object
#'
#' @param tg A tidyged object.
#' @param max_age The maximum age to assume for a living person (if a date of birth is given).
#' @param guess Whether to guess the age of individuals if no death event or date of birth is given and possibly retain them, or be cautious and remove them anyway (the default).
#' @param remove_record Whether to remove the Individual records, or retain them as placeholders.
#' @param explan_note Text to include in an explanatory note for any redacted records. An empty string will not add a note.
#' @param remove_supp_records Whether to also remove supporting records (sources, repositories, notes, multimedia) associated with the living individuals. These may contain names and dates so it is probably best to remove them.
#'
#' @return A tidyged object cleansed of information on living individuals.
#' @export
remove_living <- function(tg,
                          max_age = 120,
                          guess = FALSE,
                          remove_record = FALSE,
                          explan_note = "Information on this individual has been redacted",
                          remove_supp_records = TRUE) {
  
  indi_xrefs <- tidyged::xrefs_indi(tg)
  
  for(xref in indi_xrefs) {
    death_events <- dplyr::filter(tg, record == xref, tag == "DEAT")
    
    # death events exist - go to next individual
    if(nrow(death_events) > 0) next
    
    dob <- tidyged.internals::gedcom_value(tg, xref, "DATE", 2, "BIRT")
    
    # dob exists and age is bigger than max age - go to next individual
    if(dob != "" && date_diff(dob, minimise = TRUE) > max_age) next
    
    # dob doesn't exist, but guessed age is bigger than max age - go to next individual
    if(dob == "" && guess && guess_age(tg, xref) > max_age) next
      
    if(remove_supp_records) {
      supp_recs <- tidyged::get_supporting_records(tg, xref)
      tg <- tidyged::remove_records(tg, supp_recs)
    }
    
    if(remove_record) {
      tg <- tidyged::remove_records(tg, xref)
    } else {
      message(tidyged::describe_records(tg, xref, short_desc = TRUE), " cleansed")
      # only keep family links in individual record (but no pedigree linkage or notes)
      tg <- dplyr::filter(tg, record != xref | 
                            (record == xref & level == 0) |
                            (record == xref & level == 1 & tag %in% c("FAMS","FAMC")))
      
      if(nchar(explan_note) > 0) {
        next_row <- tidyged.internals::find_insertion_point(tg, xref, 0, "INDI")
        tg <- tibble::add_row(tg,
                              tibble::tibble(record = xref, level = 1, tag = "NOTE", value = explan_note),
                              .before = next_row)
        
      }
      
      tg <- tidyged::update_change_date(tg, xref)
    }
    
  }
  
  tg
}


#' Add ancestor records for an individual
#' 
#' This function adds placeholder Individual records for ancestors going back a specific
#' number of generations.
#' 
#' @details This function may also create Family Group records and will 
#' not modify existing ancestors.
#'
#' @param tg A tidyged object.
#' @param xref The xref of an Individual record to add ancestors for.
#' @param num_gen The number of generations to create ancestors for.
#' @param inc_sex Whether to populate the sex of the ancestors. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#'
#' @return A tidyged object with additional ancestor records.
#' @export
add_ancestors <- function(tg, xref, num_gen, inc_sex = TRUE){
  
  xrefs_par <- xref
  for(gen in seq_len(num_gen)){
    
    for(xref_par in xrefs_par){
      tg <- add_parents(tg, xref_par, inc_sex)
    }
    
    xrefs_par <- purrr::map(xrefs_par, tidyged::get_indi_parents,
                            gedcom = tg,
                            birth_only = TRUE) |>
      unlist()
  }
  
  tg
}


#' Add parent records for an individual
#' 
#' This function adds placeholder records for an individual's parents.
#' 
#' @details This function may also create a Family Group record and will 
#' not modify existing parents.
#'
#' @param tg A tidyged object.
#' @param xref The xref of an Individual record to add parents for.
#' @param inc_sex Whether to populate the sex of the parents. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#'
#' @return A tidyged object with additional parent records.
#' @export
add_parents <- function(tg, xref, inc_sex = TRUE){
  
  xref_par <- tidyged::get_indi_parents(tg, xref, birth_only = TRUE)
  
  if(length(xref_par) >= 2) return(tg)
  
  # check if family exists
  xref_famc <- tidyged::get_families_as_child(tg, xref, birth_only = TRUE)
  
  if(length(xref_famc) == 0){
    xref_famc <- tidyged.internals::assign_xref_famg(tg)
    tg <- tidyged::add_famg(tg) |>
      tidyged::activate_indi(xref) |>
      tidyged::add_indi_links_to_families(famg_xref_chil = xref_famc)
  }
  
  if(length(xref_par) == 1){
    
    par_sex_new <- "U"
    
    if(inc_sex){
      par_sex_cur <- tg$value[tg$record == xref_par & tg$tag == "SEX"]
      
      if(length(par_sex_cur) == 1){
        par_sex_new <- dplyr::case_when(par_sex_cur == "M" ~ "F",
                                        par_sex_cur == "F" ~ "M",
                                        TRUE ~ "U")
      }
    }
    
    tg <- tidyged::add_indi(tg, sex = dplyr::if_else(inc_sex, par_sex_new, "U")) |>
      tidyged::add_indi_links_to_families(famg_xref_spou = xref_famc) 
    
  } else {
    # No parents - add them both
    tg <- tidyged::add_indi(tg, sex = dplyr::if_else(inc_sex, "M", "U")) |>
      tidyged::add_indi_links_to_families(famg_xref_spou = xref_famc) |>
      tidyged::add_indi(sex = dplyr::if_else(inc_sex, "F", "U")) |>
      tidyged::add_indi_links_to_families(famg_xref_spou = xref_famc)
  }
  
  tg
}