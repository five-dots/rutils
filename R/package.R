
#' List all function signatures in a package
#'
#' @param pkg package name in a character
#'
#' @return list
#' @export
list_all_signature <- function(pkg) {
  assertthat::assert_that(assertthat::is.string(pkg))

  library(pkg, character.only = TRUE)
  all_funs <- pacman::p_funs(pkg, character.only = TRUE)

  purrr::map(all_funs, function(fun) {
    args <- tryCatch({
      formals(fun)
    }, error = function(e) "not found")

    if (is.character(args) && args == "not found")
      return(NULL)

    ## NA means fhe function has no args
    if (all(is.na(args)))
      return(glue::glue("{fun}()"))

    args_str <- purrr::imap_chr(args, ~ {
      .x <- deparse(.x)
      if (.x == "")
        .y
      else
        glue::glue("{.y} = {.x}")
    })

    args_flatten <- stringr::str_flatten(args_str, collapse = ", ")
    glue::glue("{fun}({args_flatten})")
  }) %>%
    ## Remove NULL element
    purrr::compact()
}
