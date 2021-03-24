

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
    potential_duplicates()
  
}


#' Copy all records from one tidyged object to another
#' 
#' This function takes two tidyged objects and copies all of the records in the second to the first,
#' while making their identifiers unique (even if they may be duplicates).
#'
#' @param gedcom1 The first tidyged object and the one you want to use for header and submitter information.
#' @param gedcom2 The second tidyged object.
#'
#' @return A tidyged object which contains the same header and submitter information as the first object
#' and all records contained within both input objects.
migrate_records <- function(gedcom1, gedcom2) {
  
  # update xrefs in gedcom2
  gedcom2 <- make_xrefs_unique(gedcom1, gedcom2)
  
  new_xrefs <- c(tidyged::xrefs_indi(gedcom2), tidyged::xrefs_famg(gedcom2),
                 tidyged::xrefs_sour(gedcom2), tidyged::xrefs_repo(gedcom2),
                 tidyged::xrefs_media(gedcom2), tidyged::xrefs_note(gedcom2))
  
  # move xrefs to gedcom1
  records_to_move <- dplyr::filter(gedcom2, record %in% new_xrefs)
  tibble::add_row(gedcom1, records_to_move, .before = nrow(gedcom1))
}



#' Update xrefs in a tidyged object to make them unique from another tidyged object
#' 
#' This function ensures that all record xrefs across two tidyged objects are unique.
#'
#' @param gedcom1 The first tidyged object.
#' @param gedcom2 The second tidyged object. This will be the one that will be updated.
#'
#' @return The second tidyged object, where all xrefs have been made unique from the first object.
make_xrefs_unique <- function(gedcom1, gedcom2) {
  
  for(i in letters[1:6]) {
    
    all_xrefs_fn <- switch(i, a = tidyged::xrefs_indi,
                           b = tidyged::xrefs_famg,
                           c = tidyged::xrefs_sour,
                           d = tidyged::xrefs_repo,
                           e = tidyged::xrefs_media,
                           f = tidyged::xrefs_note)
    assign_xref_fn <- switch(i, a = tidyged.internals::assign_xref_indi,
                             b = tidyged.internals::assign_xref_famg,
                             c = tidyged.internals::assign_xref_sour,
                             d = tidyged.internals::assign_xref_repo,
                             e = tidyged.internals::assign_xref_media,
                             f = tidyged.internals::assign_xref_note)
    
    old_xrefs <- all_xrefs_fn(gedcom2)
    if (length(old_xrefs) == 0) next
    
    new_xrefs <- assign_xref_fn(gedcom1, quantity = length(old_xrefs))
    for (i in seq_along(old_xrefs)) {
      gedcom2 <- dplyr::mutate(gedcom2,
                               record = ifelse(record == old_xrefs[i], new_xrefs[i], record),
                               value = ifelse(value == old_xrefs[i], new_xrefs[i], value))
    }
    
  }
  
  gedcom2
  
}


#' Identify potentially duplicate records
#'
#' @param gedcom A tidyged object.
#'
#' @return The same tidyged object, potentially with some records merged.
#' @export
potential_duplicates <- function(gedcom) {
  
  gedcom <- gedcom %>% 
    potential_duplicates_indi() %>% 
    potential_duplicates_famg() %>%
    potential_duplicates_sour() %>%
    potential_duplicates_repo() %>%
    potential_duplicates_media()
  
  records <- c(tidyged::xrefs_indi(gedcom),
               tidyged::xrefs_famg(gedcom),
               tidyged::xrefs_sour(gedcom),
               tidyged::xrefs_repo(gedcom),
               tidyged::xrefs_media(gedcom))
  
  for (record in records) {
    gedcom <- remove_duplicate_subrecords(gedcom, record)
  }
  
  gedcom
  
}


potential_duplicates_indi <- function(gedcom,
                                      year_margin = 2) {
  
  ind_xrefs <- tidyged::xrefs_indi(gedcom)
  
  given <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "GIVN", level = 2)
  surname <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "SURN", level = 2)
  dob <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "BIRT")
  dod <- purrr::map_chr(ind_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "DATE", level = 2, after_tag = "DEAT")
  
  yob <- as.numeric(stringr::str_extract(dob, "\\d{3,4}"))
  yod <- as.numeric(stringr::str_extract(dod, "\\d{3,4}"))
  
  comb <- tibble::tibble(xref = ind_xrefs,
                         given = given,
                         surname = surname,
                         yob = yob,
                         yod = yod) %>% 
    dplyr::filter(given != "", surname != "") %>% 
    dplyr::mutate(full = paste(given, surname))
  
  while(nrow(comb) > 0) {
    this_row <- comb[1,]
    dupes <- dplyr::left_join(this_row, comb, by = "full") %>% 
      dplyr::filter(is.na(yob.y) | dplyr::between(yob.y, comb$yob[1]-year_margin, comb$yob[1]+year_margin)) %>% 
      dplyr::filter(is.na(yod.y) | dplyr::between(yod.y, comb$yod[1]-year_margin, comb$yod[1]+year_margin)) %>%
      dplyr::pull(xref.y)
    
    if (length(dupes) > 1) {
      # give user option of merging them
      xrefs_to_merge <- utils::select.list(title = "Which of these individuals would you like to merge?",
                                           choices = tidyged::describe_records(gedcom, dupes),
                                           multiple = TRUE)
      
      xrefs_to_merge <- stringr::str_extract(xrefs_to_merge, "@[a-zA-Z0-9]{1,20}@") 
      
      if(length(xrefs_to_merge) > 1) gedcom <- merge_records(gedcom, xrefs_to_merge)
      
    }
    
    comb <- dplyr::filter(comb, !xref %in% dupes)
  }
  
  gedcom
}


potential_duplicates_famg <- function(gedcom) {
  
  famg_xrefs <- tidyged::xrefs_famg(gedcom)
  
  husb <- purrr::map_chr(famg_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "HUSB", level = 1)
  wife <- purrr::map_chr(famg_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "WIFE", level = 1)
  
  comb <- tibble::tibble(xref = famg_xrefs,
                         husb = husb,
                         wife = wife) %>% 
    dplyr::filter(husb != "", wife != "")
  
  while(nrow(comb) > 0) {
    this_row <- comb[1,]
    dupes <- dplyr::left_join(this_row, comb, by = c("husb","wife")) %>% 
      dplyr::pull(xref.y)
    
    if (length(dupes) > 1) {
      # give user option of merging them
      xrefs_to_merge <- utils::select.list(title = "Which of these families would you like to merge?",
                                           choices = tidyged::describe_records(gedcom, dupes),
                                           multiple = TRUE)
      
      xrefs_to_merge <- stringr::str_extract(xrefs_to_merge, "@[a-zA-Z0-9]{1,20}@") 
      
      if(length(xrefs_to_merge) > 1) gedcom <- merge_records(gedcom, xrefs_to_merge)
      
    }
    
    comb <- dplyr::filter(comb, !xref %in% dupes)
  }

  gedcom
  
}

potential_duplicates_sour <- function(gedcom) {
  
  sour_xrefs <- tidyged::xrefs_sour(gedcom)
  
  title <- purrr::map_chr(sour_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "TITL", level = 1)
  
  comb <- tibble::tibble(xref = sour_xrefs,
                         title = title) %>% 
    dplyr::filter(title != "")
  
  while(nrow(comb) > 0) {
    this_row <- comb[1,]
    dupes <- dplyr::left_join(this_row, comb, by = "title") %>% 
      dplyr::pull(xref.y)
    
    if (length(dupes) > 1) {
      # give user option of merging them
      xrefs_to_merge <- utils::select.list(title = "Which of these sources would you like to merge?",
                                           choices = tidyged::describe_records(gedcom, dupes),
                                           multiple = TRUE)
      
      xrefs_to_merge <- stringr::str_extract(xrefs_to_merge, "@[a-zA-Z0-9]{1,20}@") 
      
      if(length(xrefs_to_merge) > 1) gedcom <- merge_records(gedcom, xrefs_to_merge)
      
    }
    
    comb <- dplyr::filter(comb, !xref %in% dupes)
  }
  
  gedcom
  
}

potential_duplicates_repo <- function(gedcom) {
  
  repo_xrefs <- tidyged::xrefs_repo(gedcom)
  
  name <- purrr::map_chr(repo_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "NAME", level = 1)
  
  comb <- tibble::tibble(xref = repo_xrefs,
                         name = name) %>% 
    dplyr::filter(name != "")
  
  while(nrow(comb) > 0) {
    this_row <- comb[1,]
    dupes <- dplyr::left_join(this_row, comb, by = "name") %>% 
      dplyr::pull(xref.y)
    
    if (length(dupes) > 1) {
      # give user option of merging them
      xrefs_to_merge <- utils::select.list(title = "Which of these repositories would you like to merge?",
                                           choices = tidyged::describe_records(gedcom, dupes),
                                           multiple = TRUE)
      
      xrefs_to_merge <- stringr::str_extract(xrefs_to_merge, "@[a-zA-Z0-9]{1,20}@") 
      
      if(length(xrefs_to_merge) > 1) gedcom <- merge_records(gedcom, xrefs_to_merge)
      
    }
    
    comb <- dplyr::filter(comb, !xref %in% dupes)
  }
  
  gedcom
  
}

potential_duplicates_media <- function(gedcom) {
  
  media_xrefs <- tidyged::xrefs_media(gedcom)
  
  file_ref <- purrr::map_chr(media_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FILE", level = 1)
  format <- purrr::map_chr(media_xrefs, tidyged.internals::gedcom_value, gedcom = gedcom, tag = "FORM", level = 2)
  
  comb <- tibble::tibble(xref = media_xrefs,
                         file_ref = file_ref,
                         format = format) %>% 
    dplyr::filter(file_ref != "", format != "")
  
  while(nrow(comb) > 0) {
    this_row <- comb[1,]
    dupes <- dplyr::left_join(this_row, comb, by = c("file_ref", "format")) %>% 
      dplyr::pull(xref.y)
    
    if (length(dupes) > 1) {
      # give user option of merging them
      xrefs_to_merge <- utils::select.list(title = "Which of these multimedia would you like to merge?",
                                           choices = tidyged::describe_records(gedcom, dupes),
                                           multiple = TRUE)
      
      xrefs_to_merge <- stringr::str_extract(xrefs_to_merge, "@[a-zA-Z0-9]{1,20}@") 
      
      if(length(xrefs_to_merge) > 1) gedcom <- merge_records(gedcom, xrefs_to_merge)
      
    }
    
    comb <- dplyr::filter(comb, !xref %in% dupes)
  }
  
  gedcom
  
}


#' Combine multiple records into a single record
#' 
#' This function takes multiple records and replaces them with a single record containing
#' all of their subrecords. It does not remove duplicate subrecords.
#'
#' @param gedcom A tidyged object.
#' @param xrefs The xrefs of the records to merge.
#'
#' @return A new tidyged object where all specified records have been merged.
#' @export
#' @tests
#' expect_error(merge_records(tidyged::sample555, c("@I1@","@F1@")))
merge_records <- function(gedcom, xrefs) {
  
  if(length(xrefs) == 1) return(gedcom)
    
  rec_types <- unique(dplyr::filter(gedcom, level == 0, record %in% xrefs)$tag)
  
  if(length(rec_types) != 1) stop("Records of different types cannot be merged")
  
  merged <- dplyr::bind_rows(
    dplyr::filter(gedcom, record == xrefs[1], level == 0),
    dplyr::filter(gedcom, record %in% xrefs, level != 0)
  ) %>% 
    remove_change_dates() %>% 
    dplyr::bind_rows(tidyged.internals::CHANGE_DATE() %>% tidyged.internals::add_levels(1)) %>% 
    tidyged.internals::finalise()
  
  gedcom %>% 
    dplyr::filter(!record %in% xrefs) %>% 
    tibble::add_row(merged, .before = nrow(.)) %>% 
    dplyr::mutate(value = ifelse(value %in% xrefs, xrefs[1], value),
                  record = ifelse(record %in% xrefs, xrefs[1], record))
  
  
}


#' Remove duplicate subrecords from a tidyged record
#' 
#' This function removes duplicate level 1 subrecords within a single record.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record to act on.
#'
#' @return The same tidyged object with duplicate subrecords removed from the specified record.
#' @export
remove_duplicate_subrecords <- function(gedcom, xref) {
  
  record <- dplyr::filter(gedcom, record == xref)
    
  new_record <- record %>% 
    dplyr::mutate(new_sub = level == 1,
                  subrecord_no = cumsum(new_sub)) %>% 
    dplyr::select(-new_sub) %>% 
    dplyr::group_nest(subrecord_no) %>% 
    dplyr::select(-subrecord_no) %>%
    dplyr::mutate(tmp = data) %>% 
    dplyr::distinct(
      tmp = purrr::map(data, dplyr::arrange, tag, value),
      .keep_all = TRUE
    ) %>%
    dplyr::select(-tmp) %>% 
    tidyr::unnest(data)
  
  dplyr::filter(gedcom, record != xref) %>% 
    tibble::add_row(new_record, .before = nrow(.))
  
}



