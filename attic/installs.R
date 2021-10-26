install_requirements = function() {
  install.packages("mlr")
  install.packages("OpenML")
  install.packages("farff")
  install.packages("data.table")
  install.packages("OpenML")
  install.packages("testthat")
  install.packages("mlrCPO")
  install.packages("jsonlite")
  install.packages("parallelMap")
  install.packages("reticulate")
  install.packages("BBmisc")
  install.packages("batchtools")
  install.packages("mlr3misc")
  install.packages("lgr")
  install.packages("e1071")
  install.packages("glmnet")
  install.packages("rpart")
  install.packages("xgboost")
  install.packages("ranger")
  install.packages("RcppHNSW")
  install.packages("snow")
}




# The following tasks should not be computed in parallel;
# set_parallel_by_task (below) is used to check this.
NO_PARALLEL_TASKS = c(168329, 168338, 168332,9977, 3903, 146821)

# sets parallel to 0 for 'NO_PARALLEL_TASKS'
set_parallel_by_task = function(parallel, task) {
  if (task %in% c(NO_PARALLEL_TASKS, sapply(NO_PARALLEL_TASKS, fix_task))) parallel = 2
  return(parallel)
}


MTRY_FEATS=2