test_that("eval_point works", {
  eval_rbv2_super_dataframe(rbv2_super[2,])
  eval_rbv2_super_dataframe(rbv2_super[1,])
  eval_rbv2_super_dataframe(rbv2_super[3,])
  eval_rbv2_super_dataframe(rbv2_super[sample(100,1),])
})

test_that("eval_point works", {
  skip("Long")
  for (i in seq_len(100)) {
    eval_rbv2_super_dataframe(rbv2_super[2,])
  }
})

test_that("eval_point works", {
  skip("long")
  for (i in 1:10) {
    x = super_evals[[i]]$METADATA
    ll = eval(parse(text = x$point))
    task = strsplit(x$task, ".", fixed = TRUE)[[1]]
    task = task[length(task)]
    y = super_evals[[i]]$performances
    if (sum(y[1:10, "timetrain"]) < 20) {
      bmr = eval_rbv2("rbv2_glmnet", task, ll, seed=x$seed)
      out = bmr[[1]][[1]][[1]]$measures.test
      expect_true(all(y[1:10, 'logloss'] - out$logloss < 1e-8))
      expect_true(all(y[1:10, 'mmce'] - out$mmce < 1e-8))
      if (!anyNA(y[1:10, 'auc'])) {
        expect_true(all(y[1:10, 'f1'] - out$f1 < 1e-8))
        expect_true(all(y[1:10, 'auc'] - out$auc < 1e-8))
      } 
    }
  }
})
