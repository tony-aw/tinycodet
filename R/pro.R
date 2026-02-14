#' Standard Evaluated Versions of Some Common Expression-Evaluation Functions
#'
#' @description
#' The \code{with_pro()} and \code{aes_pro()} functions
#' are standard-evaluated versions of the expression-evaluation functions
#' \link[base]{with} and \code{ggplot2::}\link[ggplot2]{aes},
#' respectively. \cr
#' \cr
#' These alternative functions are more programmatically friendly: \cr
#' They use proper standard evaluation,
#' through the usage of one-sided formulas,
#' instead of non-standard evaluation,
#' tidy evaluation,
#' or similar programmatically unfriendly evaluations. \cr \cr
#' 
#'
#'
#' @param ... arguments to be passed to \code{ggplot2::}\link[ggplot2]{aes},
#' but given as one-sided formulas.
#' @param data a list, environment, or data.frame.
#' @param form a one-sided formula giving the expression to evaluate in \code{with_pro}. \cr
#' If the formula has an environment,
#' that environment is used to find any variables or objects not present in `data`.
#' 
#'
#'
#' 
#' @details
#' The \code{aes_pro()} function is the standard evaluated alternative to
#' \code{ggplot2::}\link[ggplot2]{aes}. \cr
#' Due to the way \code{aes_pro()} is programmed,
#' it should work even if the tidy evaluation technique
#' changes in 'ggplot2'. \cr
#' To support functions in combinations with references of the variables,
#' the input used here are formula inputs, rather than string inputs. \cr
#' See the Examples section below. \cr \cr
#'
#'
#' @note
#' The `with_pro()` function, like the original \link[base]{with} function,
#' is made for primarily for convenience. \cr
#' When using modelling or graphics functions with an explicit \code{data} argument
#' (and typically using \link[stats]{formula}s),
#' it is typically preferred to use the \code{data} argument of that function,
#' rather than to use either \cr
#' \code{with(data, ...)} or \code{with_pro(data, ...)}. \cr \cr
#'
#'
#'
#' @section Non-Standard Evaluation:
#' 
#' Non-Standard Evaluation (sometimes abbreviated as "NSE"),
#' is somewhat controversial. \cr
#' Consider the following example:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' aplot <- "ggplot2"
#' library(aplot)
#' ```
#' What package will be attached? It will not be 'ggplot2',
#' nor will an error occur.
#' Instead, the package 'aplot' will be attached. \cr
#' This is due to evaluating the expression 'aplot' as a quoted expression,
#' instead of evaluating the contents (i.e. string or formula) of the variable.
#' In other words: Non-Standard Evaluation. \cr
#' \cr
#' Regular Standard Evaluation does not have the above problem. \cr
#' 
#' 
#'
#' @seealso \link{tinycodet_safer}
#'
#' @return
#' For `with_pro()`: see \link[base]{with}. \cr
#' For `aes_pro()`: see \code{ggplot2::}\link[ggplot2]{aes}. \cr \cr
#'
#'
#' @examplesIf requireNamespace("ggplot2")
#' requireNamespace("ggplot2")
#' 
#' 
#' d <- import_data("ggplot2", "mpg")
#'
#' # mutate data:
#' myform <- ~ displ + cyl + cty + hwy
#' d$mysum <- with_pro(d, myform)
#' summary(d)
#'
#' # plotting data:
#' x <- ~ cty
#' y <- ~ sqrt(hwy)
#' color <- ~ drv
#'
#' ggplot2::ggplot(d, aes_pro(x, y, color = color)) +
#'   ggplot2::geom_point()
#' 
#' 
#' 
#'

#' @rdname pro
#' @export
with_pro <- function(data, form) {
  is_formula <- .internal_is_formula(form)
  if(!is_formula) stop("`form` must be a formula")
  if(length(form) != 2) stop("improper formula given")
  if(!is.recursive(data)) stop("`data` must be a recursive object")
  
  vars <- all.vars(form)
  env <- environment(form)
  search_names <- c(names(data), names(env))
  if(any(!vars %in% search_names)) stop("unknown variable(s) given")
  txt <- as.character(form)[2]
  out <- eval(parse(text = txt), data, enclos = env)
  environment(form) <- NULL
  return(out)
}


#' @rdname pro
#' @export
aes_pro <- function(...) {
  
  # collect args:
  lst <- list(...)
  
  # error checks:
  check <- vapply(lst, .internal_is_formula, logical(1))
  if(any(!check)) stop("formula inputs must be given")
  check <- lengths(lst)
  if(any(check != 2)) stop("improper formula given")
  
  # conversion:
  args.names <- ifelse(names(lst) == "", "", paste0(names(lst), " = "))
  args.values <- vapply(lst, \(x)deparse(x[[2]], backtick = TRUE), character(1))
  args <- paste0(args.names, args.values, collapse = ", ")
  
  # evaluating:
  txt <- paste0("ggplot2::aes(", args, ")")
  eval(parse(text=txt), envir = parent.frame(n = 1))
}



