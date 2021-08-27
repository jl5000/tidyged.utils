# Stop the NOTES from R CMD CHECK when creating columns with mutate()
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("level", "record", "tag", "value","new_sr","sr_no",".","xref.y",
                           "xref","yob.y","yod.y","new_sub","subrecord_no","data","tmp"))




