# Iterate over a list converting character "TRUE", "FALSE" to logical.
parse_lgl = function(lst) {
  lst = lapply(lst, function(x) {
    if (!is.na(x)) {
      if (x == "FALSE" || x == "TRUE")
        x = as.logical(x)
    }
    return(x)
  })
  Filter(Negate(is.na), lst)
}

repairPoints2 = function(ps, hpars) {
  library(mlr3misc)

  hpars_num = keep(hpars, is.numeric)
  # Current invalid strategy: Replace with 10^-6
  invalids = map_lgl(hpars_num, is.infinite) | map_lgl(hpars_num, is.nan) | map_lgl(hpars_num, function(x) if (is.numeric(x)) abs(x) >= 1.2676506e+30 else FALSE)
  if (any(invalids)) {
    hpars_num[invalids] = 10^-6
  }
  too_big_int = map_lgl(hpars_num, function(x) if (is.numeric(x)) abs(x) >= .Machine$integer.max else FALSE)
  if (any(too_big_int)) {
    hpars_num[too_big_int] = .Machine$integer.max - 1
  }
  hpars = BBmisc::insert(hpars, hpars_num)
  hpars = repairPoint(ps, hpars)
  setNames(mlr3misc::pmap(list(map(ps$pars, "type")[names(hpars)], hpars), function(type, par) {
    if (type == "integer" && !is.null(par)) par = round(par)
    # if (type == "integer" && is.null(par)) par = 10^6
    return(par)
  }), names(hpars))
  keep(hpars, Negate(is.null))
}

# Fix task ids that have changed on OpenML since  the results were obtained.
fix_task = Vectorize(function(task) {
  if (task == 168759) task = 167211 # Satellite
  if (task == 168761) task = 168912 # sylvine
  if (task == 168770) task = 168909 # Dilbert
  if (task == 168767) task = 190411 # Ada
  return(task)
})

get_n_cores = function(parallel, learner, task_id) {
  if (is.null(parallel)) {
    parallel = 1
  }
  if (learner == "classif.xgboost") {
    parallel = 0
  }
  return(parallel)
}

get_fix_omltask = function(tsk) {
  omltsk = OpenML::getOMLTask(tsk)
  omltsk$input$estimation.procedure$parameters$stratified_sampling = "false"
  omltsk$input$evaluation.measures = ""
  return(omltsk)
}

set_task_hyperpars = function(hpars, task, learner_id) {
  if (is.null(hpars[["sample.rate"]])) {
    hpars[["sample.rate"]] = 1
  }
  if (learner_id == "classif.xgboost") {
    hpars[["nthread"]] = 1L
    if (length(task$mlr.task$task.desc$class.levels) == 2) {
      hpars[["eval_metric"]] = "error"
    } else {
      hpars[["eval_metric"]] = "merror"
    }
  }

  if (learner_id == "classif.ranger") {
    nfeats = sum(task$mlr.task$task.desc$n.feat)
    if (task %in% c(3, 219, 15)) nfeats = round(0.8*nfeats)
    hpars[["mtry"]] = max(min(round(hpars[["mtry"]]), nfeats), 1)
  }

  if (learner_id == "classif.RcppHNSW") {
    hpars[["M"]] = min(64, hpars[["M"]])
    hpars[["ef_construction"]] = min(4096, hpars[["ef_construction"]])
  }

  return(hpars)
}

config_to_string = function(configuration) {
  xs = paste0(names(configuration), ":", unlist(configuration))
  paste0(xs, collapse=" ")
}