if(!require(tinyoperators)){install.packages("tinyoperators")}
library(tinyoperators)

import_as(
  dr., "dplyr",, extensions = "powerjoin"
)
dr.$mutate



import_as(dt., "data.table", foreign_exports=FALSE)
inops <- names(dt.)
inops <- grep("%|:=", inops, value=TRUE)


import_inops("data.table", overwrite_action = "warn")
import_inops("data.table", overwrite_action = "abort")
import_inops("data.table", overwrite_action = "avoid")
import_inops("data.table", overwrite_action = "continue")
rm(list=ls())

import_inops("data.table", include.only = ":=")
import_inops("data.table", overwrite_action = "abort")
import_inops("data.table", overwrite_action = "avoid")
import_inops("data.table", overwrite_action = "continue")
rm(list=ls())

import_inops("data.table", exclude = ":=")
import_inops("data.table", overwrite_action = "abort")
import_inops("data.table", overwrite_action = "avoid")
import_inops("data.table", overwrite_action = "continue")
rm(list=ls())
