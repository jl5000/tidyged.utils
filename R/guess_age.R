
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


#' Guess an individual's age
#' 
#' This function calculates an age for an individual based on their individual or family facts. It calculates age based on the date of the fact and their age when the fact applied.
#'
#' @param tg A tidyged object.
#' @param xref The xref of an individual.
#' @param agg_fn If multiple ages are calculated, the function to use to aggregate them.
#'
#' @return A numeric value giving the estimated age. A numeric value less than zero means no
#' determination could be made.
guess_age <- function(tg, xref, agg_fn = mean) {
  
  age <- guess_age_from_indi_events(tg, xref)
  if(age >= 0) return(age)
  
  age <- guess_age_from_famg_events(tg, xref)
  
  # guess age from relatives?
  age
}


#' Guess an individual's age from their facts
#' 
#' This function takes an individual's attributes and events and calculates an estimated age based on the date of the fact and their age when the fact applied.
#'
#' @param tg A tidyged object.
#' @param xref The xref of an individual.
#' @param agg_fn If multiple ages are calculated, the function to use to aggregate them.
#'
#' @return A numeric value giving the estimated age. A numeric value less than zero means no
#' determination could be made.
guess_age_from_indi_events <- function(tg, xref, agg_fn = mean) {
  
  indi_rec <- dplyr::filter(tg, record == xref)
  
  # give each subrecord an identifier
  indi_rec_flags <- indi_rec |> 
    dplyr::mutate(new_sr = level == 1,
                  sr_no = cumsum(new_sr))
  
  no_sr <- max(indi_rec_flags$sr_no)
  
  # take average age
  ages <- NULL
  for(sr in seq_len(no_sr)) {
    
    sub_rec <- dplyr::filter(indi_rec_flags, sr_no == sr)
    tags <- sub_rec$tag
    
    if("AGE" %in% tags & "DATE" %in% tags){
      age <- dplyr::filter(sub_rec, tag == "AGE")$value
      event_date <- dplyr::filter(sub_rec, tag == "DATE")$value
      #take mean age
      
      age <- age_now(event_date, age)
      if(age < 0) next
      
      ages <- c(ages, age)
    }
    
  }
  
  if (length(ages) == 0) return(-1)
  if (max(ages) - min(ages) > 10)
    warning("The range of possible ages for the following individual exceeds 10 years - you may want to check dates/ages:\n", tidyged::describe_indi(tg, xref))
  
  agg_fn(ages)
}


#' Guess an individual's age from their family group events
#' 
#' This function takes an individual's family group events and calculates an estimated age based on the date of the event and their age when the event occurred.
#'
#' @param tg A tidyged object.
#' @param xref The xref of an individual.
#' @param agg_fn If multiple ages are calculated, the function to use to aggregate them.
#'
#' @return A numeric value giving the estimated age. A numeric value less than zero means no
#' determination could be made.
guess_age_from_famg_events <- function(tg, xref, agg_fn = mean) {
  
  fams <- tidyged::get_families_as_partner(tg, xref)
  
  # take aggregated age
  ages <- NULL
  
  # Loop through every family as a partner
  for (fam in fams) {
    
    fam_rec <- dplyr::filter(tg, record == fam)
    
    husb_or_wife <- dplyr::filter(fam_rec, value == xref)$tag
      
    # give each subrecord an identifier
    fam_rec_flags <- fam_rec |> 
      dplyr::mutate(new_sr = level == 1,
                    sr_no = cumsum(new_sr))
    
    no_sr <- max(fam_rec_flags$sr_no)
    
    for(sr in seq_len(no_sr)) {
      
      #get rid of other age if it exists
      sub_rec <- dplyr::filter(fam_rec_flags, sr_no == sr, 
                          tag != dplyr::if_else(husb_or_wife == "HUSB", "WIFE_AGE", "HUSB_AGE"))
      
      tags <- sub_rec$tag
      
      if(("HUSB_AGE" %in% tags | "WIFE_AGE" %in% tags) & "DATE" %in% tags){
        age <- dplyr::filter(sub_rec, stringr::str_detect(tag, "_AGE"))$value
        event_date <- dplyr::filter(sub_rec, tag == "DATE")$value
        
        age <- age_now(event_date, age)
        if(age < 0) next
        
        ages <- c(ages, age)
      }
      
    }
  }
  
  if (length(ages) == 0) return(-1)
  if (max(ages) - min(ages) > 10)
    warning("The range of possible ages for the following individual exceeds 10 years - you may want to check dates/ages:\n", tidyged::describe_indi(tg, xref))
  
  agg_fn(ages)
  
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
#' @tests
#' expect_equal(date_diff("1900", "2000"), 99, tolerance = 0.01)
#' expect_equal(date_diff("1900", "2000", minimise = FALSE), 101, tolerance = 0.01)
#' expect_equal(date_diff("25 JAN", "8 MAR"), -1)
#' expect_equal(date_diff("800", "2020"), 1219, tolerance = 0.01)
#' expect_equal(date_diff("AFT 1900", "2000"), -1)
#' expect_equal(date_diff("1900", "BEF 2000"), -1)
#' expect_equal(date_diff("28 JAN 2006", "14 DEC 2008"), 2.877, tolerance = 0.01)
#' expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016"), 4, tolerance = 0.01)
#' expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016", minimise = FALSE), 16.35, tolerance = 0.01)
#' expect_equal(date_diff("ABT 1932", "CAL 2000"), 67, tolerance = 0.01)
date_diff <- function(date1,
                      date2 = tidyged::date_current(),
                      minimise = TRUE) {
  
  # put both dates into a vector
  dates <- c(date1, date2)
  
  # dates must have years
  if(!all(stringr::str_detect(dates, "\\d{3,4}"))) return(-1)
  
  dates <- stringr::str_remove_all(dates, "ABT |CAL |EST ")
  
  regex_ic <- purrr::partial(stringr::regex, ignore_case = TRUE)
  
  # if date is a range/period, get the appropriate bound
  if(any(stringr::str_detect(dates, regex_ic("AND|FROM.+TO")))) {
    
    dates <- stringr::str_replace_all(dates, regex_ic("FROM |BET "), "")
    date_reg <- tidyged.internals::reg_date(flatten = FALSE, only = FALSE)
    first_date <- paste(paste0("^", date_reg), collapse = "|")
    sec_date <- paste(paste0(date_reg, "$"), collapse = "|")
    
    if(minimise) {
      dates[1] <- stringr::str_extract(dates[1], sec_date)
      dates[2] <- stringr::str_extract(dates[2], first_date)
    } else {
      dates[1] <- stringr::str_extract(dates[1], first_date)
      dates[2] <- stringr::str_extract(dates[2], sec_date)
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
  
  if(minimise){
    dates1 <- tidyged.internals::parse_gedcom_date(dates[1], FALSE)
    dates2 <- tidyged.internals::parse_gedcom_date(dates[2], TRUE)
  } else {
    dates1 <- tidyged.internals::parse_gedcom_date(dates[1], TRUE)
    dates2 <- tidyged.internals::parse_gedcom_date(dates[2], FALSE)
  }
  
  lubridate::interval(dates1, dates2) |> 
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
