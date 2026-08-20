# Miscellaneous Package Related Functions

The `pkgs %installed in% lib.loc` operator checks if one or more given
packages (`pkgs`) exist in the given library paths (`lib.loc`), without
loading the packages.  
The syntax of this operator forces the user to make it syntactically
explicit where to look for installed R-packages.  
As `pkgs %installed in% lib.loc` does not even load a package, the user
can safely use it without fearing any unwanted side-effects.  
  
The `pkg_get_deps()` function gets the **direct** dependencies of a
package from the Description file. It works on non-CRAN packages also.  
  
The `pkg_get_deps_minimal()` function is the same as `pkg_get_deps()`,
except with `base, recom, rstudioapi, shared_tidy` all set to `FALSE`,
and the default value for `deps_type` is c("Depends", "Imports").  
  
The `pkg_lsf()` function gets a list of exported functions/operators
from a package.  
One handy use for this function is to, for example, globally attach all
infix operators from a package using `library`, like so:

    y <- pkg_lsf("packagename", type = "inops")
    library(packagename, include.only = y)

## Usage

``` r
pkgs %installed in% lib.loc

pkg_get_deps(
  package,
  lib.loc = .libPaths(),
  deps_type = c("LinkingTo", "Depends", "Imports"),
  base = FALSE,
  recom = TRUE,
  rstudioapi = TRUE,
  shared_tidy = TRUE
)

pkg_get_deps_minimal(
  package,
  lib.loc = .libPaths(),
  deps_type = c("Depends", "Imports")
)

pkg_lsf(package, type, lib.loc = .libPaths())
```

## Arguments

- pkgs:

  a character vector with the package name(s).

- lib.loc:

  character vector specifying library search path (the location of R
  library trees to search through).  
  The `lib.loc` argument would usually be
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html).  
  See also [loadNamespace](https://rdrr.io/r/base/ns-load.html).

- package:

  a single string giving the package name.

- deps_type:

  a character vector, giving the dependency types to be used.  
  The order of the character vector given in `deps_type` affects the
  order of the returned character vector; see Details sections.

- base:

  `TRUE` or `FALSE`, indicating whether base/core R should be included
  (`TRUE`), or not included (`FALSE`).

- recom:

  `TRUE` or `FALSE`, indicating whether the pre-installed 'recommended'
  R-packages should be included (`TRUE`), or not included (`FALSE`).

- rstudioapi:

  `TRUE` or `FALSE`, indicating whether the 'rstudioapi' R-package
  should be included (`TRUE`), or not included (`FALSE`).

- shared_tidy:

  `TRUE` or `FALSE`, indicating whether the following packages should be
  included (`TRUE`) or not included (`FALSE`):  
  'rlang', 'lifecycle', 'cli', 'glue', and 'withr'.

- type:

  The type of functions to list. Possibilities:

  - `"inops"` or `"operators"`: Only infix operators.

  - `"regfuns"`: Only regular functions (thus excluding infix
    operators).

  - `"all"`: All functions, both regular functions and infix
    operators.  
      

## Value

For `pkgs %installed in% lib.loc`:  
Returns a named logical vector.  
The names give the package names.  
The value `TRUE` indicates a package is installed in `lib.loc`.  
The value `FALSE` indicates a package is not installed in `lib.loc`.  
The value `NA` indicates a package is not actually a separate package,
but base/core 'R' (i.e. 'base', 'stats', etc.).  
  
For `pkg_get_deps()` and `pkg_get_deps_minimal()`:  
A character vector of direct dependencies, without duplicates.  
  
For `pkg_lsf()`:  
Returns a character vector of exported function names in the specified
package.  
  

## Details

For `pkg_get_deps()`:  
For each string in argument `deps_type`, the package names in the
corresponding field of the Description file are extracted, in the order
as they appear in that field.  
The order given in argument `deps_type` also affects the order of the
returned character vector:  
For example, `c("LinkingTo", "Depends", "Imports")`,  
means the package names are extracted from the fields in the following
order:

1.  "LinkingTo";

2.  "Depends";

3.  "Imports".

The unique (thus non-repeating) package names are then returned to the
user.  
  

## References

O'Brien J., elegantly extract R-package dependencies of a package not
listed on CRAN. *Stack Overflow*. (1 September 2023).
<https://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran>

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
"dplyr" %installed in% .libPaths()
#> dplyr 
#>  TRUE 

pkg_get_deps_minimal("dplyr")
#> [1] "generics"   "magrittr"   "pillar"     "R6"         "tibble"    
#> [6] "tidyselect" "vctrs"     
pkgs <- pkg_get_deps("dplyr")
pkgs %installed in% .libPaths()
#>        cli   generics       glue  lifecycle   magrittr     pillar         R6 
#>       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE 
#>      rlang     tibble tidyselect      vctrs 
#>       TRUE       TRUE       TRUE       TRUE 
pkg_lsf("dplyr", "all")
#>   [1] "db_drop_table"         "group_split"           "mutate_at"            
#>   [4] "db_commit"             "tally_"                "dense_rank"           
#>   [7] "summarise_all"         "any_vars"              "as.tbl"               
#>  [10] "db_insert_into"        "collect"               "full_join"            
#>  [13] "cur_group"             "dplyr_col_modify"      "all_equal"            
#>  [16] "sql_subquery"          "first"                 "mutate_"              
#>  [19] "nest_join"             "src_sqlite"            "db_query_rows"        
#>  [22] "group_by_drop_default" "validate_grouped_df"   "filter"               
#>  [25] "recode"                "db_create_index"       "group_walk"           
#>  [28] "when_all"              "cummean"               "db_query_fields"      
#>  [31] "db_create_indexes"     "last_dplyr_warnings"   "order_by"             
#>  [34] "sql_translate_env"     "tally"                 "summarize_each"       
#>  [37] "mutate_all"            "summarize_each_"       "ntile"                
#>  [40] "filter_if"             "db_desc"               "with_order"           
#>  [43] "summarize_"            "group_by_prepare"      "rows_insert"          
#>  [46] "group_indices"         "filter_all"            "sql_set_op"           
#>  [49] "summarize_at"          "sql_join"              "copy_to"              
#>  [52] "between"               "summarize"             "db_list_tables"       
#>  [55] "desc"                  "group_trim"            "replace_values"       
#>  [58] "db_rollback"           "semi_join"             "cur_column"           
#>  [61] "add_tally"             "dim_desc"              "bind_cols"            
#>  [64] "sql_select"            "vars"                  "slice"                
#>  [67] "nth"                   "symdiff"               "src_local"            
#>  [70] "rows_append"           "ungroup"               "is.grouped_df"        
#>  [73] "dplyr_row_slice"       "db_has_table"          "src_tbls"             
#>  [76] "db_data_type"          "compute"               "pull"                 
#>  [79] "count_"                "if_all"                "wrap_dbplyr_obj"      
#>  [82] "combine"               "summarise_if"          "add_rownames"         
#>  [85] "do"                    "sample_frac"           "group_by_if"          
#>  [88] "arrange_"              "all_vars"              "ident"                
#>  [91] "group_by_all"          "db_save_query"         "transmute_at"         
#>  [94] "mutate"                "sample_n"              "group_keys"           
#>  [97] "slice_tail"            "src_df"                "show_query"           
#> [100] "left_join"             "cur_group_id"          "group_by_at"          
#> [103] "db_begin"              "slice_sample"          "src_mysql"            
#> [106] "distinct_at"           "db_create_table"       "percent_rank"         
#> [109] "inner_join"            "cross_join"            "arrange_if"           
#> [112] "mutate_each_"          "group_by"              "groups"               
#> [115] "mutate_each"           "cur_data_all"          "top_frac"             
#> [118] "cur_group_rows"        "n"                     "new_rowwise_df"       
#> [121] "rows_patch"            "db_write_table"        "group_indices_"       
#> [124] "transmute_all"         "new_grouped_df"        "distinct_if"          
#> [127] "filter_out"            "dplyr_reconstruct"     "consecutive_id"       
#> [130] "case_when"             "pick"                  "c_across"             
#> [133] "group_map"             "if_any"                "distinct"             
#> [136] "summarize_all"         "validate_rowwise_df"   "progress_estimated"   
#> [139] "case_match"            "funs_"                 "cumall"               
#> [142] "slice_max"             "same_src"              "last"                 
#> [145] "arrange_all"           "cume_dist"             "summarize_if"         
#> [148] "distinct_prepare"      "reframe"               "sql"                  
#> [151] "transmute_if"          "tbl_df"                "rows_upsert"          
#> [154] "is.src"                "src"                   "replace_when"         
#> [157] "make_tbl"              "with_groups"           "transmute_"           
#> [160] "sql_escape_ident"      "distinct_all"          "rename_"              
#> [163] "lead"                  "summarise"             "is.tbl"               
#> [166] "group_size"            "add_count_"            "group_data"           
#> [169] "when_any"              "tbl"                   "db_analyze"           
#> [172] "db_explain"            "arrange"               "select"               
#> [175] "slice_"                "group_cols"            "transmute"            
#> [178] "filter_at"             "group_rows"            "sql_escape_string"    
#> [181] "count"                 "funs"                  "rename_all"           
#> [184] "summarise_each"        "check_dbplyr"          "min_rank"             
#> [187] "row_number"            "tbl_nongroup_vars"     "distinct_"            
#> [190] "rename"                "top_n"                 "group_modify"         
#> [193] "cumany"                "rows_update"           "rename_with"          
#> [196] "summarise_each_"       "right_join"            "lag"                  
#> [199] "cur_data"              "grouped_df"            "filter_"              
#> [202] "slice_min"             "nest_by"               "anti_join"            
#> [205] "collapse"              "group_nest"            "rowwise"              
#> [208] "across"                "select_"               "relocate"             
#> [211] "add_tally_"            "group_by_"             "common_by"            
#> [214] "auto_copy"             "select_if"             "if_else"              
#> [217] "add_count"             "near"                  "sql_semi_join"        
#> [220] "tbl_ptype"             "recode_values"         "slice_head"           
#> [223] "n_groups"              "group_vars"            "rows_delete"          
#> [226] "tbl_vars"              "select_at"             "do_"                  
#> [229] "summarise_"            "bind_rows"             "coalesce"             
#> [232] "is_grouped_df"         "union_all"             "n_distinct"           
#> [235] "select_all"            "rename_if"             "explain"              
#> [238] "mutate_if"             "src_postgres"          "arrange_at"           
#> [241] "na_if"                 "summarise_at"          "rename_at"            
#> [244] "join_by"               "recode_factor"        


```
