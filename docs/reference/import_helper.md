# Helper Functions for the 'tinycodet' Package Import System

The `help.import()` function finds the help file for functions or
topics, including exposed functions/operators as well as functions in a
package alias object.  
  

## Usage

``` r
help.import(..., i, alias)
```

## Arguments

- ...:

  further arguments to be passed to
  [help](https://rdrr.io/r/utils/help.html).

- i:

  either one of the following:

  - a function (use back-ticks when the function is an infix/replacement
    operator).  
    Examples:  
    `myfun` , `` `%operator%` `` , `` `fun<-` ``,
    `myalias.$some_function`.  
    If a function, the `alias` argument is ignored.

  - a string giving the function name or topic (i.e. `"myfun"`,
    `"thistopic"`).  
    If a string, argument `alias` must be specified also.

- alias:

  an object of class
  [tinyimport_alias](https://tony-aw.github.io/tinycodet/reference/tinyimport_alias.md)
  as returned by
  [import_as](https://tony-aw.github.io/tinycodet/reference/import_as.md).

## Value

For `help.import()`:  
Opens the appropriate help page.  
  

## Details

For `help.import(...)`:  
Do not use the `topic` / `package` and `i` / `alias` argument sets
together. It's either one set or the other.  
For example:

    import_as(.str ~ stringi)
    import_from("magrittr", import_ls("magrittr", "infix"))
    help.import(i = .str$stri_sub)
    help.import(i = `%>%`)
    help.import(i = "stri_sub", alias = .str)
    help.import(topic = "%>%", package = "magrittr")
    help.import("%>%", package = "magrittr") # same as previous line

## See also

[tinycodet_import](https://tony-aw.github.io/tinycodet/reference/aaa2_tinycodet_import.md)

## Examples

``` r
import_as(.stri ~ stringi)
#> Import & method registration complete
.stri$stri_join("a", "b")
#> [1] "ab"

import_from("stringi", import_ls("stringi", "infix"))
#> c("%s!=%", "%s!==%", "%s$%", "%s*%", "%s+%", "%s<%", "%s<=%", 
#> "%s==%", "%s===%", "%s>%", "%s>=%", "%stri!=%", "%stri!==%", 
#> "%stri$%", "%stri*%", "%stri+%", "%stri<%", "%stri<=%", "%stri==%", 
#> "%stri===%", "%stri>%", "%stri>=%")
#> Import & method registration complete
"a" %s+% "b"
#> [1] "ab"

attr.import(.stri)
#> $pkgs
#> $pkgs$packages_order
#> [1] "stringi"
#> 
#> $pkgs$main_package
#> [1] "stringi"
#> 
#> $pkgs$re_exports.pkgs
#> NULL
#> 
#> 
#> $conflicts
#>                package winning_conflicts
#> 1 stringi + re-exports                  
#> 
#> $ordered_object_names
#>   [1] "stri_startswith"               "stri_locate_first"            
#>   [3] "%s==%"                         "stri_subset_regex"            
#>   [5] "stri_locate_all_boundaries"    "stri_width"                   
#>   [7] "stri_datetime_add<-"           "stri_datetime_parse"          
#>   [9] "stri_join_list"                "stri_extract_all_regex"       
#>  [11] "stri_extract_first_fixed"      "stri_detect_regex"            
#>  [13] "stri_trim_right"               "stri_order"                   
#>  [15] "stri_locale_info"              "stri_extract_last_charclass"  
#>  [17] "stri_datetime_add"             "%s<%"                         
#>  [19] "stri_trans_isnfkc"             "stri_subset_fixed"            
#>  [21] "stri_trans_isnfkd"             "stri_extract_last_coll"       
#>  [23] "stri_replace_first_regex"      "stri_reverse"                 
#>  [25] "stri_enc_fromutf32"            "stri_opts_fixed"              
#>  [27] "stri_datetime_fields"          "%s>=%"                        
#>  [29] "stri_locate_first_boundaries"  "stri_enc_toascii"             
#>  [31] "stri_locate_all_fixed"         "stri_trans_toupper"           
#>  [33] "stri_sub_all<-"                "stri_sort_key"                
#>  [35] "stri_locate_first_words"       "%stri!=%"                     
#>  [37] "stri_info"                     "stri_replace_last_charclass"  
#>  [39] "stri_enc_isutf16le"            "stri_length"                  
#>  [41] "stri_replace_first_coll"       "stri_extract_last_boundaries" 
#>  [43] "stri_split_lines1"             "stri_trans_nfkc_casefold"     
#>  [45] "stri_trans_tolower"            "stri_na2empty"                
#>  [47] "stri_sub<-"                    "stri_read_lines"              
#>  [49] "stri_detect"                   "stri_locate_last_coll"        
#>  [51] "stri_trans_casefold"           "stri_split_fixed"             
#>  [53] "%s>%"                          "stri_extract_all_words"       
#>  [55] "stri_rand_strings"             "stri_trans_isnfc"             
#>  [57] "stri_endswith_fixed"           "stri_trans_isnfd"             
#>  [59] "stri_split_coll"               "stri_locate_all_charclass"    
#>  [61] "%s!=%"                         "stri_c"                       
#>  [63] "stri_subset<-"                 "stri_locate_first_coll"       
#>  [65] "stri_locate_first_charclass"   "stri_conv"                    
#>  [67] "stri_sub_replace_all"          "stri_enc_list"                
#>  [69] "stri_string_format"            "stri_sub_replace"             
#>  [71] "stri_pad_left"                 "stri_locate_all_coll"         
#>  [73] "stri_subset_regex<-"           "stri_detect_fixed"            
#>  [75] "stri_unique"                   "stri_omit_na"                 
#>  [77] "stri_locale_list"              "stri_trans_isnfkc_casefold"   
#>  [79] "stri_locate_last_regex"        "stri_escape_unicode"          
#>  [81] "stri_duplicated"               "stri_sort"                    
#>  [83] "stri_split_lines"              "stri_flatten"                 
#>  [85] "stri_extract_last_regex"       "stri_subset_coll"             
#>  [87] "stri_subset"                   "stri_datetime_format"         
#>  [89] "stri_replace_last_regex"       "stri_split_boundaries"        
#>  [91] "stri_extract_all_coll"         "stri_trans_nfc"               
#>  [93] "stri_trans_nfd"                "stri_cmp_equiv"               
#>  [95] "stri_endswith"                 "stri_replace_first"           
#>  [97] "stri_split_regex"              "stri_locate_last_charclass"   
#>  [99] "stri_count_boundaries"         "stri_endswith_coll"           
#> [101] "stri_endswith_charclass"       "stri_sprintf"                 
#> [103] "stri_enc_isutf32le"            "stri_detect_coll"             
#> [105] "stri_remove_na"                "stri_locate_all_words"        
#> [107] "stri_locale_set"               "stri_printf"                  
#> [109] "%stri<=%"                      "stri_cmp"                     
#> [111] "stri_rank"                     "stri_replace_na"              
#> [113] "%s$%"                          "stri_count_coll"              
#> [115] "stri_count_words"              "stri_sub_all"                 
#> [117] "stri_extract_all"              "stri_numbytes"                
#> [119] "stri_enc_detect"               "stri_datetime_symbols"        
#> [121] "stri_isempty"                  "stri_replace_first_fixed"     
#> [123] "stri_count_fixed"              "stri_rand_lipsum"             
#> [125] "stri_trans_nfkc"               "stri_trans_nfkd"              
#> [127] "stri_replace_all_coll"         "stri_encode"                  
#> [129] "stri_replace_rstr"             "stri_match_last"              
#> [131] "%stri<%"                       "stri_duplicated_any"          
#> [133] "stri_timezone_info"            "stri_match_first_regex"       
#> [135] "stri_match_all"                "stri_extract_first_charclass" 
#> [137] "stri_extract_first_boundaries" "stri_enc_tonative"            
#> [139] "stri_startswith_fixed"         "stri_pad"                     
#> [141] "%stri*%"                       "stri_dup"                     
#> [143] "stri_opts_brkiter"             "stri_omit_empty_na"           
#> [145] "stri_write_lines"              "stri_match_last_regex"        
#> [147] "stri_c_list"                   "stri_replace_all"             
#> [149] "stri_opts_regex"               "%stri!==%"                    
#> [151] "stri_subset_charclass<-"       "stri_replace_all_regex"       
#> [153] "stri_locate_last_boundaries"   "stri_subset_fixed<-"          
#> [155] "stri_locate_first_fixed"       "stri_extract_last_words"      
#> [157] "stri_replace_last"             "stri_enc_isutf16be"           
#> [159] "stri_extract_first"            "stri_rand_shuffle"            
#> [161] "%stri+%"                       "stri_extract_last"            
#> [163] "stri_locate_last"              "stri_datetime_now"            
#> [165] "stri_startswith_coll"          "stri_trim_both"               
#> [167] "%s!==%"                        "stri_replace"                 
#> [169] "stri_extract_last_fixed"       "stri_replace_all_charclass"   
#> [171] "stri_pad_right"                "stri_match"                   
#> [173] "stri_replace_last_coll"        "stri_opts_collator"           
#> [175] "stri_cmp_lt"                   "stri_subset_coll<-"           
#> [177] "stri_enc_info"                 "stri_trans_char"              
#> [179] "stri_stats_latex"              "stri_trim_left"               
#> [181] "stri_replace_all_fixed"        "stri_replace_last_fixed"      
#> [183] "stri_join"                     "stri_enc_toutf32"             
#> [185] "stri_cmp_neq"                  "stri_locate_all_regex"        
#> [187] "stri_replace_first_charclass"  "stri_enc_toutf8"              
#> [189] "stri_locale_get"               "stri_trim"                    
#> [191] "stri_count_regex"              "stri_cmp_le"                  
#> [193] "stri_timezone_set"             "stri_count_charclass"         
#> [195] "stri_pad_both"                 "stri_paste_list"              
#> [197] "stri_sub_all_replace"          "stri_datetime_create"         
#> [199] "stri_extract_first_coll"       "stri_read_raw"                
#> [201] "stri_enc_mark"                 "stri_timezone_get"            
#> [203] "stri_datetime_fstr"            "stri_locate_last_words"       
#> [205] "stri_match_first"              "stri_cmp_nequiv"              
#> [207] "stri_extract_all_charclass"    "stri_list2matrix"             
#> [209] "stri_startswith_charclass"     "stri_remove_empty_na"         
#> [211] "stri_subset_charclass"         "stri_locate_first_regex"      
#> [213] "stri_remove_empty"             "%stri===%"                    
#> [215] "stri_trans_general"            "stri_stats_general"           
#> [217] "stri_locate_all"               "%s*%"                         
#> [219] "stri_enc_set"                  "stri_cmp_gt"                  
#> [221] "stri_detect_charclass"         "stri_split"                   
#> [223] "stri_compare"                  "stri_extract_all_boundaries"  
#> [225] "stri_unescape_unicode"         "stri_locate"                  
#> [227] "stri_enc_get"                  "stri_omit_empty"              
#> [229] "stri_enc_isutf32be"            "stri_timezone_list"           
#> [231] "%s===%"                        "stri_extract"                 
#> [233] "%stri$%"                       "stri_wrap"                    
#> [235] "stri_split_charclass"          "stri_enc_detect2"             
#> [237] "%stri==%"                      "stri_locate_last_fixed"       
#> [239] "%s+%"                          "%s<=%"                        
#> [241] "stri_cmp_ge"                   "stri_sub"                     
#> [243] "stri_enc_isutf8"               "stri_trans_list"              
#> [245] "stri_match_all_regex"          "stri_extract_first_regex"     
#> [247] "stri_paste"                    "stri_count"                   
#> [249] "stri_extract_all_fixed"        "stri_coll"                    
#> [251] "%stri>%"                       "stri_cmp_eq"                  
#> [253] "stri_extract_first_words"      "stri_trans_totitle"           
#> [255] "stri_enc_isascii"              "%stri>=%"                     
#> 
#> $tinyimport
#> [1] "tinyimport"
#> 
```
