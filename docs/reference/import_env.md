# Environment Specification in the import\_ Functions

The `env` argument in the `import_` functions specify where the
functions, exported objects, or alias object will be placed.  
The following can be specified for `env`:

- `NULL`: If `env = NULL`, the objects will be placed in the caller
  environment.

- an environment.

- a string, giving the name of the search path, as given by
  [search](https://rdrr.io/r/base/search.html), to place the objects
  in.  
  If multiple search paths have the specified name, an error is
  returned.

- a number larger than 1 and smaller than `length(search())`, giving the
  position of the search path to place the objects in.  
    

If `env` is a string or number, and thus points to a place in the search
path, the search path environment is not allowed to be any of the
following:

- a package path (their names start with 'package:')

- a tools path (their names start with 'tools:')

- the Global environment ('.GlobalEnv')

- autoloads search path ('Autoloads')

- a path at position 1 or `length(search())`  
    

Attempting to use such a path results in an error.  
The user can use the
[searchenv\_](https://tony-aw.github.io/tinycodet/reference/searchenv.md)
functions, provided by 'tinycodet', to safely add or remove custom
search paths.  
  
The default value for `env` is `NULL`.  
  

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

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
searchenv_rm("my_ops")
search()
#>  [1] ".GlobalEnv"        "package:magrittr"  "package:tinycodet"
#>  [4] "package:stats"     "package:graphics"  "package:grDevices"
#>  [7] "package:utils"     "package:datasets"  "package:methods"  
#> [10] "Autoloads"         "tools:callr"       "package:base"     
```
