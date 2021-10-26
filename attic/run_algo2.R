

# Run an algorithm
# @param algo :: algorithm name, e.g. classif.svm
# @param task :: task_id, e.g. 3
# @param str  :: tuple string, e.g. "make_tuple(1,1)"
# @example
# run_algo("mlr_svm", 3, "make_tuple(1,1)")
run_algo_2 = function(problem, task, cfg, parallel = 10L) {

   if (set_parallel_by_task(parallel, task) && problem != "mlr_xgboost")
          parallelMap::parallelStartMulticore(parallel, level = "mlr.resample")
    on.exit(parallelMap::parallelStop())

    lgr = get_logger("eval_logger")$set_threshold("info")
    lgr$add_appender(lgr::AppenderFile$new("runs/mlr_evaluation_log.log"))

    lgr$info(sprintf("Evaluating %s|%s|%s", problem, task, paste0(as.character(cfg), collapse=",")))

    # Get learner and hyperpars
    lrn = make_preproc_pipeline(problem)
    hpars = cfg

    # Repair hyperparams according to paramset before predicting
    ps = filterParams(getParamSet(lrn), names(hpars))
    hpars = parse_lgl(hpars)
    hpars = repairPoints2(ps, hpars[names(ps$pars)])
    lrn = setHyperPars(lrn, par.vals = hpars)
    if (problem == "mlr_xgboost")
      lrn = setHyperPars(lrn, nthread = 1L)
    bmr = try({
        # Some task have gotten different ids
        task = fix_task(task)
        omltsk = getOMLTask(task)
        # Hack away bugs / missing stuff in OpenML, stratified does not matter as splits are fixed anyway
        if (task %in% c(2073, 41, 145681)) omltsk$input$estimation.procedure$parameters$stratified_sampling = "false"
        if (task %in% c(146212, 168329, 168330, 168331, 168332, 168339, 145681, 168331)) omltsk$input$evaluation.measures = ""
        z = convertOMLTaskToMlr(omltsk, measures = mmce)
        if (problem == "mlr_random forest") {
          nfeats = sum(z$mlr.task$task.desc$n.feat)
          if (task %in% c(3, 219, 15)) nfeats = 0.8*nfeats
          lrn = setHyperPars(lrn, mtry = max(min(hpars[["mtry"]], nfeats), 1))
        }
        if (problem == "mlr_knn") {
          hpars[["M"]] = min(64, hpars[["M"]])
          hpars[["ef_construction"]] = min(4096, hpars[["ef_construction"]])
        }
      lrn = setHyperPars(lrn)
        benchmark(lrn, z$mlr.task, z$mlr.rin, measures = c(z$mlr.measures, list(mlr::logloss, mlr::mmce)))
    })
    aggr = bmr$results[[1]][[1]]$aggr
    measure = "logloss.test.mean"
    lgr$info(sprintf("Result: %s: %s", measure, aggr[[measure]]))
    bmr$results[[1]][[1]]
}

# Write results from registry to a CSV
# @param pname: problem_name
# @param out_suffix: suffix for output file
problem_results_to_csv = function(pname, out_suffix) {
  jt = getJobTable()[, success := !is.na(done) & is.na(error)]
  jt = cbind(jt, setnames(map_dtr(jt$algo.pars, identity), "problem", "problem_name"))
  jt = jt[problem_name == pname, c("job.id", "problem_name", "task", "str", "success")]
  jt = merge(jt, imap_dtr(get_problem_json(pname)$benchmark, function(x, y) data.table("default" = y, "str" = x)), by = "str")
  if (nrow(jt[(!success)]))
    message("Unfinished jobs: ", paste0(jt[(!success)]$job.id, collapse = ","))
  jt = jt[(success)]
  jt = cbind(jt,
    map_dtr(reduceResultsList(jt$job.id, function(x)
      t(x$aggr[c("timetrain.test.sum", "timepredict.test.sum", "mmce.test.mean")])), data.table)
  )
  browser()
  # Update column order
  jt = jt[, c(3, 4, 1, 6, 9, 7, 8, 2)]
  fwrite(jt, file = paste0("data/results_gecco/", pname, "_", out_suffix, ".csv"))
}

symbolic_results_to_csv = function(pname, out_suffix, default_name = "symbolic_default", jobs = NULL) {
  browser()
  jt = getJobTable(jobs)[, success := !is.na(done) & is.na(error)]
  jt = cbind(jt, setnames(map_dtr(jt$algo.pars, identity), "problem", "problem_name"))
  jt = jt[problem_name == pname, c("job.id", "problem_name", "task", "str", "success")]
  if (nrow(jt[(!success)]))
    message("Unfinished jobs: ", paste0(jt[(!success)]$job.id, collapse = ","))
  jt = jt[(success)]
  jt = cbind(jt,
    map_dtr(reduceResultsList(jt$job.id, function(x)
      t(x$aggr[c("timetrain.test.sum", "timepredict.test.sum", "logloss.test.mean", "mmce.test.mean")])), data.table)
  )
  files = c(
    "constant" = paste0("data/generated_defaults/constants/",substr(pname, 5, 100),"_cst_found_by_mean_rank_less.csv"),
    "short" = paste0("data/generated_defaults/symbolic/",substr(pname, 5, 100),"_found_by_hp_based_max_length.csv"),
    "long" = paste0("data/generated_defaults/symbolic/",substr(pname, 5, 100),"_found_by_mean_rank.csv"),
    "package" = paste0("data/",pname,"_real_data_baselines_results.csv")
  )
  imap(files, function(x, n) {
    if (file.exists(x)) {
      df = fread(x)
      jt[jt$str %in% df$expression, default := n]
    }
  })
  # Update column order
  jt = jt[, c("problem_name","task","str","default", "logloss.test.mean", "mmce.test.mean","timetrain.test.sum","timepredict.test.sum","job.id")]
  fwrite(jt, file = paste0("data/results_gecco/", pname, "_", out_suffix, ".csv"))
}

problem_nn_results_to_csv = function(pname, out_suffix) {
  jt = getJobTable()[, success := !is.na(done) & is.na(error)]
  jt = cbind(jt, setnames(map_dtr(jt$algo.pars, function(x) {
    y = as.data.table(x[c("task", "problem")])
    y$str = stringify_list(x["cfg"])
    return(y)
    }), "problem", "problem_name"))
  jt = jt[problem_name == pname, c("job.id", "problem_name", "task", "str", "success")]
  # jt = merge(jt, imap_dtr(get_problem_json(pname)$benchmark, function(x, y) data.table("default" = y, "str" = x)), by = "str")
  if (nrow(jt[(!success)]))
    message("Unfinished jobs: ", paste0(jt[(!success)]$job.id, collapse = ","))
  jt = jt[(success)]
  jt = cbind(jt,
    map_dtr(reduceResultsList(jt$job.id, function(x)
      t(x$aggr[c("timetrain.test.sum", "timepredict.test.sum", "mmce.test.mean", "logloss.test.mean")])), data.table)
  )
  jt$default = "nearest_neighbour"
  # Update column order
  jt = jt[, c("problem_name","task","str", "default", "logloss.test.mean", "mmce.test.mean","timetrain.test.sum","timepredict.test.sum","job.id")]
  fwrite(jt, file = paste0("data/results_gecco/", pname, "_", out_suffix, ".csv"))
}


stringify_list = function(x) {
  paste0(imap_chr(x$cfg, function(x,n) {paste0(x, ":", n)}), collapse=",")
}

filter_run_files = function(jobs, run_files) {
  dd = rbindlist(map(run_files, fread))
  names(dd) = c("problem", "task", "str")
  dd$problem = paste0("mlr_", dd$problem)
  jt = getJobTable(jobs)
  d2 = map_dtr(transpose_list(jt), function(x) {
      if ("str" %in% names(x$algo.pars)) {
        data.table(c(x$algo.pars, x$job.id))[, str := str]
      } else {
        as.data.table(x$algo.pars[c(1,3)])[, str := paste0(x$algo.pars[[2]], collapse=",")][, job.id := x$job.id]
      }
    }
  )
  unique(c(d2[dd, on = names(dd), nomatch = 0]$job.id, d2[!startsWith(str, "make_tuple"),]$job.id))
}