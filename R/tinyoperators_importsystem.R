#' Some background information on tinyoperator's import system
#'
#'@description
#' 
#' Attaching a package necessarily loads a package,
#' but loading a package does not entail attaching a package. \cr
#' \cr
#' Whenever a package is used in any way,
#' even internally by a function,
#' a package is **loaded**.
#' A loaded package is simply a package that R is prepared to use.
#' A package that is loaded is not necessarily attached.
#' But **attaching** package is what happens when one uses
#' the \code{library}() or \code{require}() function: \cr
#' the functions from the package are exposed to the namespace. \cr
#' Exposing functions to the namespace is
#' **not a necessity**,
#' it is merely a **convenience**. \cr
#' \cr
#' Without attaching a package,
#' there are basically 2 ways to use functions from a package: \cr
#'  - Using the \code{::} operator, like so: \code{package_name::function_name()}. \cr
#'  - If a package name is very long, the \code{::} approach may eventually become annoying.
#' So alternatively, one can use an abbreviated \code{alias}, like so: \code{alias <- loadNamespace("packagename")};
#' then one can use \code{alias$function_name()}, instead of the longer \code{package_name::function_name()}. \cr
#' 
#' The advantages of attaching a package instead are as follows:
#'   
#' 1) \strong{Less typing}:
#' You have to type less to use functions. This is especially handy for **infix operators** - which `tinyoperators` obviously focuses on - as operators use special characters, which require them to be surrounded by back-ticks when using `::` or `alias$`. \cr
#' 2) \strong{More collective usage}:
#' If multiple packages are meant to work together, constantly switching between the different package name/alias prefixes may eventually become annoying and even syntactically chaotic-looking. By attaching the packages, you no longer have to deal with this annoyance. \cr
#' 
#' 
#' But, attaching many packages willy-nilly has a few potentially serious drawbacks:
#'   
#' 1) **masking namespaces**: functions from different packages that have the same function names will mask each other.
#' 2) **overloading core R functions**: some R packages overload core R functions. This is not always wanted (or even expected) from the user. Using `::` or a package alias will allow one to explicitly choose whether to use the original R function, or the package function.
#' 3) **lack of code clarity**: The absence of a package name or alias prefix at function calls makes it less clear from which package which function came. This will be compounded when an R package overloads core R functions. If something goes wrong in a script, one has to figure out from which package a function came from, in order to figure out the cause of the issue. The lack of a package name or alias prefix makes this more cumbersome. 
#' 4) **global assignment**: Unlike aliases, which exist **locally**, when packages are attached, they are attached **globally**. Using `library()` inside a function will thus not merely attach the package inside the function, but attach it globally, which may worsen the previously named problems when a function silently attaches a package.
#' 5) **Polluting your namespace**: The more packages one attaches, the greater the chance for bugs caused by masked namespaces, and the greater the difficulty in de-bugging your code due to the aforementioned lack of syntactical clarity.
#' 
#' Some programming languages don't even allow attaching packages, because of these issues.
#' 
#' I do see the advantages of attaching a package - especially when it comes to using `infix operators`. But the disadvantages of attaching a package cannot just be ignored. So what `tinyoperators` attempts to do with the functions described in this section, is to somewhat find the best of both worlds. Basically, `tinyoperators` has functions that allow the following functionality lacking in base R:
#' 
#'  - Allow **multiple related** packages to be loaded under **one alias**. This essentially combines the advantages of "collective usage" (see attaching advantage number 2), whilst keeping most advantages of only loading a package under an alias.
#'  - Allow **exposing** of infix operators to the **current environment**. This gains advantages "less typing" whilst simultaneously avoiding the disadvantage of "global assignment".
#' 
#' Moreover, `tinyoperators` extends this functionality to also work on **sourced modules**.

#' @rdname tinyoperators_help
#' @export
tinyoperators_importsystem <- function() {
  utils::`?`(tinyoperators_importsystem)
}
