#' Run an algorithm
#'  @param learner `character`\cr
#'    Learner name e.g. "classif.svm".
#'  @param task  `character`\cr
#'    OpenML Task ID of the corresponding task.
#'  @param configuration `list`\cr
#'    Named list of hyperparameters.
#'  @param parallel `integer`\cr
#'    Number of cores to use. Set automatically if not provided by the user.
#'  @param logfile `character`\cr
#'    Logfile to write to.
#'  @param parallel `list`\cr
#'    Metrics to compute.
#' @export
#' @examples
#'   learner_id = "classif.svm"
#'   task_id = 3
#'   configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .1)
#'   eval_config(learner_id, task_id, configuration)
eval_config = function(learner, task_id, configuration, parallel = NULL, logfile = NULL,
  extra_metrics = list(mlr::logloss, mlr::mmce, mlr::auc, mlr::f1, mlr::timetrain, mlr::timepredict)) {
  assert_list(configuration)
  task_id = fix_task(task_id)
  learner_id = assert_choice(learner, paste0("classif.", c("rpart","glmnet","svm","knn","ranger","xgboost")))
  parallelMap::parallelStartMulticore(get_n_cores(parallel, learner, task_id), level = "mlr.resample")
  on.exit(parallelMap::parallelStop())

  lgr = get_logger("eval_logger")$set_threshold("info")
  if (!is.null(logfile)) {
    lgr$add_appender(lgr::AppenderFile$new(logfile))
  }
  lgr$info(sprintf("Evaluating %s on %s.", learner, task_id))

  # Instantiate the preprocessing pipeline
  lrn = make_preproc_pipeline(learner_id)

  # Repair parameters
  ps = filterParams(getParamSet(lrn), names(configuration))
  configuration = parse_lgl(configuration)
  configuration = repairPoints2(ps, configuration[names(ps$pars)])

  omltsk = get_fix_omltask(task_id)
  z = OpenML::convertOMLTaskToMlr(omltsk, measures = mmce)
  configuration = set_task_hyperpars(configuration, z, learner_id)
  lrn = setHyperPars(lrn, par.vals = configuration)
  lgr$info(sprintf("Hyperparameters: %s", config_to_string(configuration)))

  bmr = benchmark(lrn, z$mlr.task, z$mlr.rin, measures = c(z$mlr.measures, extra_metrics))
  aggr = bmr$results[[1]][[1]]$aggr
  lgr$info(sprintf("Result: %s: %s", names(aggr), aggr))
}





