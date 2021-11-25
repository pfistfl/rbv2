#' @import checkmate
#' @import data.table
#' @import lgr
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  suppressWarnings(RNGversion("3.3"))

  configureMlr(
    on.learner.error = "quiet",
    on.learner.warning = "quiet",
    show.learner.output = FALSE,
    show.info = FALSE,
    on.error.dump = FALSE
  )

  options(warn = 1)
}
