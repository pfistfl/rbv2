
REG_DIR = "cluster/registry_symbolics"
source_files = c("cluster/R/CPO_maxfact.R", "cluster/R/RLearner_classif_rcpphnsw.R", "cluster/R/helpers.R", "cluster/R/config.R")
sapply(source_files, source)
source_packages = c("mlr", "mlrCPO", "OpenML", "jsonlite", "data.table", "parallelMap", "lgr", "mlr3misc")


####################################################################################################
### Results: unitmp

run_files = list.files("data/generated_defaults/symbolic", full.names=TRUE)

# Create Job Registry
if (!file.exists(REG_DIR)) {
  reg = makeExperimentRegistry(
    file.dir = REG_DIR,
    seed = 1,
    packages = source_packages,
    source = source_files
  )
  addProblem("symbolic_best_best")
  addAlgorithm("run_algo", fun = function(data, job, instance, ...) {run_algo(..., parallel = 1)})

  # Each line in grd is a configuration
  for (file in run_files) {
    grd = fread(file)
    grd = grd[, c("algorithm", "task", "expression")]
    grd[algorithm == "random forest", ]$problem = "rf"
    grd[, str := expression][, expression := NULL][, problem := paste0("mlr_", algorithm)][, algorithm := NULL]
    grd = unique(grd)
    addExperiments(algo.designs = list(run_algo = grd))
  }

} else {
  reg = loadRegistry(REG_DIR, writeable = TRUE)
  # unlink(REG_DIR, TRUE)
}

# reg$cluster.functions = makeClusterFunctionsSocket(2)

# Submit jobs
jobs = setdiff(findNotDone()$job.id, findRunning()$job.id)
jobs = filter_run_files(jobs, run_files)
while (length(jobs)) {
  if (length(jobs)) {
    jt = getJobTable(jobs)
    jt = cbind(jt, setnames(map_dtr(jt$algo.pars, identity), "problem", "problem_name"))
    try({submitJobs(sample(jt$job.id))})
  }
  Sys.sleep(500)
}

# dir.create("data/results_gecco")
reg = loadRegistry(REG_DIR, writeable = FALSE)
jobs = getJobTable()$job.id
jobs = filter_run_files(jobs, run_files)
symbolic_results_to_csv(pname = "mlr_rpart", out_suffix = "real_data_symbolic_results_2", jobs = jobs)           # done
symbolic_results_to_csv(pname = "mlr_svm", out_suffix = "real_data_symbolic_results_2", jobs = jobs)             # running: ssh:christoph
symbolic_results_to_csv(pname = "mlr_rf", out_suffix = "real_data_symbolic_results_2")              # done
symbolic_results_to_csv(pname = "mlr_xgboost", out_suffix = "real_data_symbolic_results_2")         # missing: run on: ssh:compstat
symbolic_results_to_csv(pname = "mlr_knn", out_suffix = "real_data_symbolic_results_2", jobs = jobs)             # done
symbolic_results_to_csv(pname = "mlr_glmnet", out_suffix = "real_data_symbolic_results_2", jobs = jobs)             # done






####################################################################################################
### Baselines: uni2
run_files = c(
  "data/svm_nearest_neighbors.csv",
  "data/rpart_nearest_neighbors.csv",
  "data/knn_nearest_neighbors.csv",
  "data/glmnet_nearest_neighbors.csv",
  "data/rf_nearest_neighbors.csv",
  "data/xgboost_nearest_neighbors.csv"
)

# Create Job Registry
if (!file.exists(REG_DIR)) {
  reg = makeExperimentRegistry(
    file.dir = REG_DIR,
    seed = 1,
    packages = source_packages,
    source = source_files
  )
  addProblem("symbolic_best_best")
  addAlgorithm("run_algo_2", fun = function(data, job, instance, ...) {run_algo_2(..., parallel = RESAMPLE_PARALLEL_CPUS)})

  # Each line in grd is a configuration
  for (file in run_files) {
    grd = fread(file)
    grd = data.table(task = grd$V1, cfg = pmap(grd[, 2:ncol(grd)], list), algorithm = gsub("_nearest_neighbors.csv", "", gsub("data/", "", file)))
    grd = grd[, c("algorithm", "task", "cfg")]
    grd[, problem := paste0("mlr_", algorithm)][, algorithm := NULL]
    grd = unique.data.frame(grd)
    addExperiments(algo.designs = list(run_algo_2 = grd))
  }
} else {
  reg = loadRegistry(REG_DIR, writeable = TRUE)
  # unlink(REG_DIR, TRUE)
}
reg$cluster.functions = makeClusterFunctionsSocket(2)


jobs = findNotDone()$job.id
while (length(jobs)) {
  jobs = setdiff(findNotDone()$job.id, findRunning()$job.id)
  if (length(jobs)) {
    jt = getJobTable(jobs)
    jt$problem_name = map_chr(jt$algo.pars, function(x) x$problem)
    try({submitJobs(sample(jt$job.id))})
  }
  Sys.sleep(500)
}

reg = loadRegistry(REG_DIR, writeable = FALSE)                                                    # STATUS / PLANNING
problem_nn_results_to_csv(pname = "mlr_rpart", out_suffix = "real_data_nn_results")
problem_nn_results_to_csv(pname = "mlr_svm", out_suffix = "real_data_nn_results")
problem_nn_results_to_csv(pname = "mlr_glmnet", out_suffix = "real_data_nn_results")
problem_nn_results_to_csv(pname = "mlr_rf", out_suffix = "real_data_nn_results")
problem_nn_results_to_csv(pname = "mlr_xgboost", out_suffix = "real_data_nn_results")
problem_nn_results_to_csv(pname = "mlr_knn", out_suffix = "real_data_nn_results")

####################################################################################################
### Implementation defaults uni3

run_files = list.files("data/generated_defaults/constants", full.names=TRUE)
# Create Job Registry
if (!file.exists(REG_DIR)) {
  reg = makeExperimentRegistry(
    file.dir = REG_DIR,
    seed = 1,
    packages = source_packages,
    source = source_files
  )
  addProblem("symbolic_best_best")
  addAlgorithm("run_algo", fun = function(data, job, instance, ...) {run_algo(..., parallel = 1)})

  # Each line in grd is a configuration
  for (file in run_files) {
    grd = fread(file)
    grd = grd[, c("algorithm", "task", "expression")]
    grd[algorithm == "random forest", ]$problem = "rf"
    grd[, str := expression][, expression := NULL][, problem := paste0("mlr_", algorithm)][, algorithm := NULL]
    grd = unique(grd)
    addExperiments(algo.designs = list(run_algo = grd))
  }

} else {
  reg = loadRegistry(REG_DIR, writeable = TRUE)
  # unlink(REG_DIR, TRUE)
}

reg$cluster.functions = makeClusterFunctionsSocket(12)

# Submit jobs
jobs = setdiff(findNotDone()$job.id, findRunning()$job.id)
jobs = filter_run_files(jobs, run_files)
while (length(jobs)) {
  if (length(jobs)) {
    jt = getJobTable(jobs)
    jt = cbind(jt, setnames(map_dtr(jt$algo.pars, identity), "problem", "problem_name"))
    try({submitJobs(sample(jt$job.id))})
  }
  Sys.sleep(500)
}



reg = loadRegistry(REG_DIR, writeable = FALSE)                                                    # STATUS / PLANNING
symbolic_results_to_csv(pname = "mlr_rpart", out_suffix = "real_data_constant_results_2")           # done
symbolic_results_to_csv(pname = "mlr_svm", out_suffix = "real_data_constant_results_2")             # running: ssh:christoph
symbolic_results_to_csv(pname = "mlr_glmnet", out_suffix = "real_data_constant_results_2")          # done
symbolic_results_to_csv(pname = "mlr_rf", out_suffix = "real_data_constant_results_2")              # done
symbolic_results_to_csv(pname = "mlr_xgboost", out_suffix = "real_data_constant_results_2")         # missing: run on: ssh:compstat
symbolic_results_to_csv(pname = "mlr_knn", out_suffix = "real_data_constant_results_2")             # done


library(mlr3misc)
library(data.table)
source("cluster/R/helpers.R")
dts = map(list.files('data/results_gecco', full.names = TRUE), fread)
dt = rbindlist(dts)[, default := as.character(default)]


# Get the name of the implementation default
x = unlist(lapply(unique(dt$problem_name), function(x) get_problem_json(x)$benchmark))
xx = data.table(def = x, name = names(x))
get_default = function(str) {
  xx[def == str,]$name
}
dt[is.na(default), default := Vectorize(get_default)(str)]
dt = dt[default != "symbolic_best",] # We do not incldue this in the table
# Choose best implementation default
package_dt = dt[default %in% xx$name, .(logloss.test.mean = min(logloss.test.mean, na.rm=TRUE)), by = c("task", "problem_name")]
package_dt[,default := "package"]
dt = rbind(dt[,names(package_dt), with=FALSE], package_dt)
dt = dt[!(default %in% c("mlr_default", "sklearn_default", "short")),]
dt = dt[default == "long", default := "symbolic"]
dt[, learner := substr(problem_name, 5, 100)][, optimizer := "None"][, constants := FALSE][, expression := default][, score := logloss.test.mean]

fwrite(dt[, c("task","learner","optimizer","constants","expression","score")], "Figures/real_data_performance.csv")


# Aggregate
dt = dt[, .(m = mean(logloss.test.mean, na.rm = TRUE), sd = sd(logloss.test.mean, na.rm=TRUE), n = .N) , by = .(problem_name, default)]
dt[, sd := round(sd, 2)][, m := round(m, 2)]
strip_null = function(x) {
  x = as.character(x)
  if (substr(x, 1, 2) == "0.")
    x = substr(x, 2, 1000)
  return(x)
}
strip0 = Vectorize(strip_null)
dt[, val := paste0(strip0(m), "(", strip0(sd), ")")]
tab = dcast(dt, problem_name ~ default, value.var = "val")
tab