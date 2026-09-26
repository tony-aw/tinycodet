
# Programmatically Dynamic Code ====

ls <- import_ls("stringi", "infix")
import_from("stringi", ls)

if("bit64" %installed in% .libPaths()) {
  ls <- import_ls("bit64", "nonfun")
  import_from("bit64", ls)
}


# Syntactically Readable Code ====

import_ls("stringi", "rp") # copy-pasted the printed literal code
import_from(
  "stringi",
  ls = c("stri_datetime_add", "stri_datetime_add<-", "stri_sub", "stri_sub_all", 
         "stri_sub_all<-", "stri_sub<-", "stri_subset", "stri_subset_charclass", 
         "stri_subset_charclass<-", "stri_subset_coll", "stri_subset_coll<-", 
         "stri_subset_fixed", "stri_subset_fixed<-", "stri_subset_regex", 
         "stri_subset_regex<-", "stri_subset<-")
)


