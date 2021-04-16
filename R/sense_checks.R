
sense_checks <- function(tg,
                         max_age = 120,
                         min_marriage_age = 18,
                         min_age_parent = 16) {
  
  xrefs <- tidyged::xrefs_indi(tg)
  
  for(xref in xrefs) {
    # separation between birth and death
  # separation between birth and marriage
  # separation between birth and child's birth
  # separation between death and child's birth
    
    
    
  }
  
}


guess_age <- function(tg, xref) {
  # no dob available
  
  age <- guess_age_from_indi_events(tg, xref)
  if(age >= 0) return(age)
  
  age <- guess_age_from_famg_events(tg, xref)
  if(age >= 0) return(age)
  
  # guess age from relatives
  
}


guess_age_from_indi_events <- function(tg, xref) {
  
  indi_rec <- dplyr::filter(tg, record == xref)
  
  # give each subrecord an identifier
  indi_rec_flags <- indi_rec %>% 
    dplyr::mutate(new_sr = level == 1,
                  sr_no = cumsum(new_sr))
  
  no_sr <- max(indi_rec_flags$sr_no)
  
  # take average age
  ages <- NULL
  for(sr in seq_len(no_sr)) {
    sr <- dplyr::filter(indi_rec_flags, sr_no == sr)
    tags <- sr$tag
    
    if("AGE" %in% tags & "DATE" %in% tags){
      age <- dplyr::filter(sr, tag == "AGE")$value
      event_date <- dplyr::filter(sr, tag == "DATE")$value
      #take mean age
      
      age <- age_now(event_date, age)
      if(age < 0) next
      
      ages <- c(ages, age)
    }
    
  }
  
  if (length(ages) == 0) return(-1)
  mean(ages)
}

guess_age_from_famg_events <- function(tg, xref) {
  
  
}

#' Determine the number of years between two dates
#' 
#' @details Doesn't yet handle dual years or BCE dates.
#'
#' @param date1 A date string from the tidyged object.
#' @param date2 A date string from the tidyged object. If no date is given, today's date is used.
#' @param minimise If date ranges or periods are used in the dates, whether to choose the bounds which
#' assume the minimum date difference. If this is FALSE, the maximum date difference is assumed.
#'
#' @return A numeric value giving the number of years. A numeric value less than zero means no
#' determination could be made.
date_diff <- function(date1,
                      date2 = tidyged::date_current(),
                      minimise = TRUE) {
  
  # put both dates into a vector
  dates <- c(date1, date2)
  
  # dates must have years
  if(!all(stringr::str_detect(dates, "\\d{3,4}"))) return(-1)
  
  regex_ic <- purrr::partial(stringr::regex, ignore_case = TRUE)
  
  # if date is a range/period, get the appropriate bound
  if(any(stringr::str_detect(dates, regex_ic("AND|FROM.+TO")))) {
    
    dates <- stringr::str_replace_all(dates, regex_ic("FROM |BET "), "")
    
    if(minimise) {
      dates[1] <- stringr::str_extract(dates[1], "(\\d{1,2} )?([A-Za-z]{3} )?(\\d{3,4})?$")
      dates[2] <- stringr::str_extract(dates[2], "^(\\d{1,2} )?([A-Za-z]{3} )?(\\d{3,4})?")
    } else {
      dates[1] <- stringr::str_extract(dates[1], "^(\\d{1,2} )?([A-Za-z]{3} )?(\\d{3,4})?")
      dates[2] <- stringr::str_extract(dates[2], "(\\d{1,2} )?([A-Za-z]{3} )?(\\d{3,4})?$")
    }
    
  } else if(any(stringr::str_detect(dates, regex_ic("BEF|AFT")))) {
    # bomb out if difference is indeterminate
    if(minimise) {
      if(stringr::str_detect(dates[1], regex_ic("AFT")) |
         stringr::str_detect(dates[2], regex_ic("BEF")))
        return(-1)
    } else {
      if(stringr::str_detect(dates[1], regex_ic("BEF")) |
         stringr::str_detect(dates[2], regex_ic("AFT")))
        return(-1)
    }
    
  }
  
  dates1 <- tidyged.internals::parse_gedcom_date(dates[1], minimise)
  dates2 <- tidyged.internals::parse_gedcom_date(dates[2], minimise)
  
  lubridate::interval(dates1, dates2) %>% 
    lubridate::time_length("years")
}

#' Determine the age of an individual now given their age on a previous date
#' 
#' @param date_of_fact A date string from the tidyged object.
#' @param age_at_fact An age at event string from the tidyged object.
#' @param minimise If date ranges or periods are used in the date, whether to choose the bounds which
#' assume the minimum age. If this is FALSE, the maximum age is assumed.
#'
#' @return A numeric value giving the current age in years. A numeric value less than zero means no
#' determination could be made.
age_now <- function(date_of_fact,
                    age_at_fact,
                    minimise = TRUE) {
  
  age_of_fact <- date_diff(date_of_fact, minimise = minimise)
  if(age_of_fact < 0) return(-1)
  
  age <- tidyged.internals::parse_gedcom_age(age_at_fact)
  
  age_of_fact + age
}