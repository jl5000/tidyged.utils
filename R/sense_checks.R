
sense_checks <- function(gedcom,
                         max_age = 120,
                         min_marriage_age = 18,
                         min_age_parent = 16) {
  
  xrefs <- tidyged::xrefs_indi(gedcom)
  
  for(xref in xrefs) {
    # separation between birth and death
  # separation between birth and marriage
  # separation between birth and child's birth
  # separation between death and child's birth
    
    
    
  }
  
}


guess_age <- function(gedcom, xref) {
  # no dob available
  
  # check age at indi events
  
  # check age at famg events
  
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
    # bomb out if difference in indeterminate
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
  
  years <- as.numeric(stringr::str_extract(dates, "\\d{3,4}"))
  
  years[2] - years[1]
}
