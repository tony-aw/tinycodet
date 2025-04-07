
library(tinycodet)
library(tinytest)

# help.import() as a replacement for help() ====
help.import()
help()

help.import(apply)
help(apply)

help.import("apply")
help("apply")

help.import(base)
help(base)

help.import(topic = apply)
help(topic = apply)

help.import(topic = "apply")
help(topic = "apply")

help.import(package = base)
help(package = base)


# help.import ====
import_as(~ stri., "stringi")

help.import(i = "about_search_regex", alias = stri.)

help.import(i = stri.$stri_cmp)

import_as(~ mr., "magrittr")
import_inops(expose = "magrittr")
help.import(i = `%>%`)
help.import(i = mr.$freduce)
help.import(i = "%>%", alias=mr.)
help.import(i = "add", alias=mr.)

import_as(~ rg., "Rgraphviz")
help.import(i = rg.$plot)
help.import(i = "plot.graph", alias = rg.)

safer_partialmatch()
expect_warning(
  iris$Sepal.Len
)

# end manual tests
