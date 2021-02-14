# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value"))

.pkgenv <- new.env(parent=emptyenv())

.pkgenv$record_string_indi <- "Individual"
.pkgenv$record_string_famg <- "Family group"
.pkgenv$record_string_subm <- "Submitter"
.pkgenv$record_string_repo <- "Repository"
.pkgenv$record_string_obje <- "Multimedia"
.pkgenv$record_string_note <- "Note"
.pkgenv$record_string_sour <- "Source"


