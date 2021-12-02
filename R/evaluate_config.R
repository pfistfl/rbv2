#' Run an algorithm
#'
#'  @param learner `character`\cr
#'    Learner name e.g. "classif.svm".
#'  @param task  `character`\cr
#'    OpenML Task ID of the corresponding task.
#'  @param configuration `list`\cr
#'    Named list of hyperparameters.
#'  @param parallel `integer`\cr
#'    Number of cores to use. Set automatically to 1 if not provided by the user.
#'    Always set to 1 for xgboost to prevent paralellization errors.
#'  @param logfile `character`\cr
#'    Logfile to write to.
#'  @param seed `character`\cr
#'    Seed to set. Defaults to `NULL` (no seed).
#'  @param extra_metrics `list`\cr
#'    Additional metrics to compute. List of mlr metrics. Metrics that are always returned:
#'    `mlr::acc, mlr::bac, mlr::auc, mlr::multiclass.aunp, mlr::logloss, mlr::multiclass.brier, mlr::f1, mlr::mcc, mlr::timetrain, mlr::timepredict`.
#' @export
#' @examples
#'   learner_id = "classif.svm"
#'   task_id = 3
#'   configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .1)
#'   eval_config(learner_id, task_id, configuration)
eval_config = function(learner, task_id, configuration, parallel = NULL, logfile = NULL, seed = NULL,
  extra_metrics = list()) {
  base_metrics = list(mlr::acc, mlr::bac, mlr::auc, mlr::multiclass.aunp, mlr::logloss, mlr::multiclass.brier, mlr::f1, mlr::mcc, mlr::timetrain, mlr::timepredict)
  extra_metrics = c(base_metrics, extra_metrics)
  assert_list(configuration)
  task_id = fix_task(task_id)
  learner_id = assert_choice(learner, paste0("classif.", c("rpart","glmnet","svm","RcppHNSW","ranger.pow","xgboost")))

  ncores = get_n_cores(parallel, learner, task_id)
  if (ncores > 1) {
    parallelMap::parallelStartMulticore(ncores, level = "mlr.resample")
    on.exit(parallelMap::parallelStop())
  }

  lgr = get_logger("eval_logger")$set_threshold("info")
  if (!is.null(logfile)) {
    lgr$add_appender(lgr::AppenderFile$new(logfile))
  }
  lgr$info(sprintf("Evaluating %s on %s.", learner, task_id))

  # Instantiate the preprocessing pipeline
  lrn = make_preproc_pipeline(learner_id)
  # Repair parameters
  # print(setdiff(names(getParamSet(lrn)$pars), names(configuration)))
  # print(setdiff(names(configuration), names(getParamSet(lrn)$pars)))
  ps = getParamSet(lrn)
  ps = filterParams(ps, intersect(names(configuration), names(ps$pars)))
  configuration = parse_lgl(configuration)
  configuration = repairPoints2(ps, configuration[names(ps$pars)])

  omltsk = get_fix_omltask(task_id)
  z = OpenML::convertOMLTaskToMlr(omltsk, measures = mmce)
  configuration = set_task_hyperpars(configuration, z, learner_id)
  lrn = setHyperPars(lrn, par.vals = configuration)

  lgr$info(sprintf("Hyperparameters: %s", config_to_string(configuration)))
  if (!is.null(seed)) {
    assert_int(seed)
    set.seed(seed)
  }

  # Throw out non-multiclass measures for multiclass tasks
  if (length(z$mlr.task$task.desc$class.levels) > 2) {
    extra_metrics = extra_metrics[map_lgl(extra_metrics, function(x) "classif.multi" %in% x$properties)]
  }
  measures = unique_measures(c(z$mlr.measures, extra_metrics))
  bmr = benchmark(lrn, z$mlr.task, z$mlr.rin, measures = measures)
  aggr = bmr$results[[1]][[1]]$aggr
  lgr$info(sprintf("Result: %s: %s", names(aggr), aggr))
  return(bmr)
}


#' Run an algorithm from rbv2
#'
#'  @param learner `character`\cr
#'    Learner name e.g. "rbv2-super".
#'  @param task  `character`\cr
#'    OpenML Task ID of the corresponding task.
#'  @param configuration `list`\cr
#'    Named list of hyperparameters.
#'  @param ... `any`\cr
#'    Arguments passed on to `eval_config`.
#' @export
#' @examples
#'   learner_id = "rbv2_svm"
#'   task_id = 3
#'   configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .1)
#'   eval_rbv2(learner_id, task_id, configuration)
eval_rbv2 = function(learner, task_id, configuration, ...) {
  assert_true(grepl("rbv2_", learner))

  if (learner == "rbv2_super") {
    assert_true(!is.null(configuration$learner))
    learner = paste0("rbv2_", configuration$learner)
    configuration$learner = NULL
  }
  rbv2_name = gsub("rbv2_", "", learner)

  if (learner == "rbv2_aknn") {
    learner = "classif.RcppHNSW"
  } else if (learner == "rbv2_ranger") {
    learner = "classif.ranger.pow"
  } else {
    learner = gsub("rbv2_", "classif.", learner)
  }

  # Filter missing args
  configuration = Filter(Negate(is.na), configuration)
  configuration$repl = NULL
  # Fix up configuration names
  nms = names(configuration)
  nms[nms == "trainsize"] = "sample.rate"
  nms = gsub(paste0(rbv2_name, "."), "", nms)
  names(configuration) = nms

  task_id = data_to_task_id(task_id)
  eval_config(learner, task_id, configuration, ...)
}

#' Run a yahpo gym config
#' Returns a data.table
#'  @param scenario `character`\cr
#'    Scenario (e.g. `rbv2_super`) to evaluate.
#'  @param configuration `list`\cr
#'    Named list of hyperparameters including task_id and learner.
#'  @param ... `any`\cr
#'    Arguments passed on to `eval_config`.
#' @export
#' @examples
#'   configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .1, task_id = 3, learner_id = "rbv2_svm")
#'   eval_rbv2(learner_id, task_id, configuration)
eval_yahpo = function(scenario, configuration, ...) {
  task_id = configuration$task_id
  learner = scenario
  repl = configuration$repl
  configuration = configuration[-which(names(configuration) %in% c("task_id", "repl"))]
  out = eval_rbv2(learner, task_id, configuration, ...)
  out = getBMRPerformances(out)[[1]][[1]]
  as.list(out[out$iter == repl,])
}


data_to_task_id = function(task_id) {
  oml_task_info[oml_task_info$data.id %in%  as.integer(task_id), "task.id_cv10"]
}
