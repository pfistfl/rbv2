reticulate::use_condaenv("yahpo_gym", required=TRUE)
library("yahpogym")
library("reticulate")
# init_local_config(data_path = "~/Documents/repos/yahpo_data")
devtools::load_all()

rbv2 = BenchmarkSet$new("rbv2_super")

os = rbv2$py_instance$get_opt_space("1040", drop_fidelity_params = TRUE)
fs = rbv2$py_instance$get_fidelity_space()

samp = c(
  os$sample_configuration(1L)$get_dictionary(),
  fs$sample_configuration(1L)$get_dictionary()
)

eval_yahpo("rbv2_super", samp)




eval_sample = function(instance = "rbv2_glmnet") {
  rbv2 = BenchmarkSet$new(instance)
  os = rbv2$py_instance$get_opt_space("1040", drop_fidelity_params = TRUE)
  fs = rbv2$py_instance$get_fidelity_space()
  samp = c(
    os$sample_configuration(1L)$get_dictionary(),
    fs$sample_configuration(1L)$get_dictionary()
  )
  res = eval_yahpo(instance, samp)
  getBMRPerformances(res)[[1]][[1]]
}

eval_sample("rbv2_rpart") -> res