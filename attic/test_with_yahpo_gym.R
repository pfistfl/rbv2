reticulate::use_condaenv("yahpo_gym", required=TRUE)
library("yahpogym")
library("reticulate")
# init_local_config(data_path = "~/Documents/repos/yahpo_data")
devtools::load_all()


eval_sample = function(scenario = "rbv2_glmnet", instance = "1040", ...) {
  rbv2 = BenchmarkSet$new(scenario)
  os = rbv2$py_instance$get_opt_space(instance, drop_fidelity_params = TRUE)
  fs = rbv2$py_instance$get_fidelity_space()
  samp = c(
    os$sample_configuration(1L)$get_dictionary(),
    fs$sample_configuration(1L)$get_dictionary()
  )
  eval_yahpo(scenario, samp, ...)
}


scs = c("rbv2_svm", "rbv2_ranger", "rbv2_rpart", "rbv2_glmnet", "rbv2_aknn", "rbv2_super", "rbv2_xgboost")
instances = BenchmarkSet$new(scs[1])$instances


for (i in 1:10) {
eval_sample(sample(scs, 1), sample(instances,1), parallel = 4) -> res
}