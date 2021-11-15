expect_circa = function(x, y, eps = .2) {
  diff = abs(x - y) < (eps * abs(y))
  expect_true(diff, info = paste0("x:", x, "y:", y))
}

eval_rbv2_super_dataframe = function(rw) {
  tgt = rw[,1:8]
  task_id = rw$task_id
  xs = rw[,setdiff(colnames(rw), c(colnames(tgt), "task_id")), with = FALSE]
  res = eval_rbv2("rbv2_super", task_id, as.list(xs))
  res = res$results[[1]][[1]]$measures.test
  res = res[res$iter == tgt$repl,]

  # We check for less than 20% deviation (without seed)
  expect_circa(res$mmce, tgt$mmce)
  expect_circa(res$logloss, tgt$logloss)
  if (!is.na(tgt$auc) | !is.na(tgt$f1)) {
    expect_circa(res$auc, tgt$auc)
    expect_circa(res$f1, tgt$f1)
  }
  expect_circa(res$timetrain, tgt$timetrain, eps = 3)
  expect_circa(res$timepredict, tgt$timepredict, eps = 3)
}