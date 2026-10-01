
search()
searchenv_add("my_ops")
search()

exports <- import_ls("stringi", c("infix", "rp"))
import_from("stringi", exports, env = "my_ops")

foo <- searchenv_get("my_ops")
all(exports %in% names(foo))

search()
searchenv_rm("my_ops")
search()
