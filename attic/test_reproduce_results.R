r1 = readRDS("~/Downloads/superevals_classif.xgboost.gblinear.rds")
r2 = readRDS("~/Downloads/superevals_classif.ranger.pow.rds")

r1 = r1[sample(length(r1), 30)]
r2 = r2[sample(length(r2), 30)]

test_reproduce = function(r, learner) {
  for (i in seq_len(length(r))) {
    x = r[[i]]$METADATA
    ll = eval(parse(text = x$point))
    task = strsplit(x$task, ".", fixed = TRUE)[[1]]
    task = task[length(task)]
    if (task != 307) next
    y = r[[i]]$performances
    if (sum(y[1:10, "timetrain"]) < 20) {
      bmr = eval_rbv2(learner, task, ll, seed=x$seed)
      out = bmr[[1]][[1]][[1]]$measures.test
      if (task == 307) browser()
      expect_true(all(abs(y[1:10, 'logloss'] - out$logloss) < 1e-6))
      expect_true(all(abs(y[1:10, 'mmce'] - out$mmce) < 1e-6))
      if (!anyNA(y[1:10, 'auc'])) {
        expect_true(all(abs(y[1:10, 'f1'] - out$f1) < 1e-6))
        expect_true(all(abs(y[1:10, 'auc'] - out$auc) < 1e-6))
      } 
    }
  }
}

test_reproduce(r1, "rbv2_xgboost")





