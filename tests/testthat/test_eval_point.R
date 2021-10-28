test_that("eval_point works", {
  learner_id = "classif.svm"
  task_id = 3
  configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .01)
  bmr = eval_config(learner_id, task_id, configuration)
  expect_class(bmr, "BenchmarkResult")
})