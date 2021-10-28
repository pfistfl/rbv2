test_that("eval_rbv2 svm works", {
  learner_id = "rbv2_svm"
  task_id = 3
  configuration = list(gamma = 0.1, cost  = 10, sample.rate = .01, foo = NA)
  bmr = eval_rbv2(learner_id, task_id, configuration)
  expect_class(bmr, "BenchmarkResult")
  # Wrong inputs error
  expect_error(eval_rbv2("classif.svm", task_id, configuration))
  configuration = list(gamma = 0.1, cost  = 10, sample.rate = .01, foo = "bar")
  expect_error(eval_rbv2(learner_id, task_id, configuration))

})

test_that("eval_rbv2 aknn works", {
  learner_id = "rbv2_aknn"
  task_id = 3
  configuration = list(ef = 10, ef_construction = 20, sample.rate = .01, foo = NA)
  bmr = eval_rbv2(learner_id, task_id, configuration)
  expect_class(bmr, "BenchmarkResult")
  # Wrong inputs error
  expect_error(eval_rbv2("classif.svm", task_id, configuration))
  configuration = list(gamma = 0.1, cost  = 10, sample.rate = .01, foo = "bar")
  expect_error(eval_rbv2(learner_id, task_id, configuration))

})

test_that("eval_rbv2 super works", {
  learner_id = "rbv2_super"
  task_id = 3
  configuration = list(gamma = 0.1, cost  = 10, sample.rate = .01, learner = "svm", foo = NA)
  bmr = eval_rbv2(learner_id, task_id, configuration)
  expect_class(bmr, "BenchmarkResult")
})

test_that("eval_rbv2 super works", {
  learner_id = "rbv2_super"
  task_id = 3
  configuration = list(svm.gamma = 0.1, svm.cost  = 10, trainsize = .05, learner = "svm", foo = NA)
  eval_rbv2(learner_id, task_id, configuration)

  for (i in 1:5) {
    learner_id = "rbv2_super"
    configuration = sample_pars[[i]]
    task_id = as.integer(sample(c(3, 50, 46, 60), 1))
    configuration$trainsize = 200 / oml_task_info[oml_task_info$data.id == task_id, "number.of.instances"]
    configuration$trainsize = ifelse(length(configuration$trainsize), configuration$trainsize, .1)
    bmr = eval_rbv2(learner_id, task_id, configuration)
    expect_class(bmr, "BenchmarkResult")
  }
})