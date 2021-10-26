#' Sets a maximum number of allowed factor levels, collapses all others to "collapsed"
#' @export
cpoMaxFact <- makeCPO("max.fact",
  pSS(max.fact.no = .Machine$integer.max: integer[1, ]),
  fix.factors = TRUE,
  dataformat = "factor",
  cpo.train = {
    sapply(data, function(d) {
      if (length(levels(d)) < max.fact.no - 1) {
        return(levels(d))
      }
      c(names(sort(table(d), decreasing = TRUE))[seq_len(max.fact.no - 1)],
        rep("collapsed", length(levels(d)) - max.fact.no + 1))
    }, simplify = FALSE)
  },
  cpo.retrafo = {
    for (n in names(data)) {
      levels(data[[n]]) = control[[n]]
    }
    data
  })

# Make the preprocessing + learner pipeline
#' @export
make_preproc_pipeline = function(algo) {
  pipe = cpoFixFactors() %>>%
    cpoSample() %>>%
    cpoCbind(
        cpoImputeConstant("__MISSING__", affect.type = c("factor", "ordered")) %>>%
        cpoMultiplex(id = "num.impute",
          list(
              cpoImputeMean(affect.type = "numeric"),
              cpoImputeMedian(affect.type = "numeric"),
              cpoImputeHist(use.mids = FALSE, affect.type = "numeric")),
          selected.cpo = "impute.hist"),
        MISSING = cpoSelect(type = "numeric") %>>% cpoMissingIndicators()) %>>%
    cpoMaxFact(32) %>>%
    cpoDropConstants(abs.tol = 0)

    if (algo %in% c("classif.RcppHNSW", "classif.xgboost"))
      pipe = pipe %>>% cpoDummyEncode(reference.cat = TRUE, infixdot = TRUE)

    lrn = makeLearner(algo, predict.type = "prob")
    pipe %>>% lrn
}
