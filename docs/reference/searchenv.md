# Add, Remove, or Access Search Path Environments

Functions for safely attaching, removing, or accessing environments from
[search](https://rdrr.io/r/base/search.html) path.  
  
`searchenv_add()` adds a new, empty environment to the
[search](https://rdrr.io/r/base/search.html) path.  
`searchenv_rm()` removes a user-defined environment from the
[search](https://rdrr.io/r/base/search.html) path.  
`searchenv_get()` returns a user-defined environment from the
[search](https://rdrr.io/r/base/search.html) path.  
  

## Usage

``` r
searchenv_add(name, pos = 2L)

searchenv_rm(name, pos)

searchenv_get(name, pos)
```

## Arguments

- name:

  a single string giving the name for the environment in the
  [search](https://rdrr.io/r/base/search.html) path.

- pos:

  a single positive integer giving the position for the environment in
  the [search](https://rdrr.io/r/base/search.html) path.

## Details

These functions were designed with safety in mind.  
They do not allow adding, removing, or accessing search environments
like the following:

- a package path (their names start with 'package:')

- a tools path (their names start with 'tools:')

- the Global environment ('.GlobalEnv')

- autoloads search path ('Autoloads')

- a path at position 1 or `length(search())`  
    

Attempting to add a new search path environment whose name already
exists gives an error.  
Attempting to remove or access a search path environmeent whose name
does not exists gives an error.  
  

## Examples

``` r
search()
#>  [1] ".GlobalEnv"        "package:magrittr"  "package:tinycodet"
#>  [4] "package:stats"     "package:graphics"  "package:grDevices"
#>  [7] "package:utils"     "package:datasets"  "package:methods"  
#> [10] "Autoloads"         "tools:callr"       "package:base"     
searchenv_add("my_ops")
search()
#>  [1] ".GlobalEnv"        "my_ops"            "package:magrittr" 
#>  [4] "package:tinycodet" "package:stats"     "package:graphics" 
#>  [7] "package:grDevices" "package:utils"     "package:datasets" 
#> [10] "package:methods"   "Autoloads"         "tools:callr"      
#> [13] "package:base"     

exports <- import_ls("stringi", c("infix", "rp"))
#> c("%s!=%", "%s!==%", "%s$%", "%s*%", "%s+%", "%s<%", "%s<=%", 
#> "%s==%", "%s===%", "%s>%", "%s>=%", "%stri!=%", "%stri!==%", 
#> "%stri$%", "%stri*%", "%stri+%", "%stri<%", "%stri<=%", "%stri==%", 
#> "%stri===%", "%stri>%", "%stri>=%", "stri_datetime_add", "stri_datetime_add<-", 
#> "stri_sub", "stri_sub<-", "stri_sub_all", "stri_sub_all<-", "stri_subset", 
#> "stri_subset<-", "stri_subset_charclass", "stri_subset_charclass<-", 
#> "stri_subset_coll", "stri_subset_coll<-", "stri_subset_fixed", 
#> "stri_subset_fixed<-", "stri_subset_regex", "stri_subset_regex<-"
#> )
import_from("stringi", exports, env = "my_ops")
#> Import & method registration complete

foo <- searchenv_get("my_ops")
all(exports %in% names(foo))
#> [1] TRUE

search()
#>  [1] ".GlobalEnv"        "my_ops"            "package:magrittr" 
#>  [4] "package:tinycodet" "package:stats"     "package:graphics" 
#>  [7] "package:grDevices" "package:utils"     "package:datasets" 
#> [10] "package:methods"   "Autoloads"         "tools:callr"      
#> [13] "package:base"     
searchenv_rm("my_ops", which(search() == "my_ops"))
search()
#>  [1] ".GlobalEnv"        "package:magrittr"  "package:tinycodet"
#>  [4] "package:stats"     "package:graphics"  "package:grDevices"
#>  [7] "package:utils"     "package:datasets"  "package:methods"  
#> [10] "Autoloads"         "tools:callr"       "package:base"     
```
