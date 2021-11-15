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