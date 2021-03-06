# library(paradox)
# library(mlr3misc)
# domain = ps(
#     # svm
#     svm.kernel = p_fct(levels = c("linear", "polynomial", "radial")),
#     svm.cost = p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x)),
#     svm.gamma =  p_dbl(lower = -10, upper = 10, tags = "log", trafo = function(x) exp(x), depends = svm.kernel == "radial"),
#     svm.tolerance = p_dbl(lower = -10, upper = log(2), tags = "log", trafo = function(x) exp(x)),
#     svm.degree = p_int(lower = 2, upper = 5, depends = svm.kernel == "polynomial"),
#     # glmnet
#     glmnet.alpha = p_dbl(lower = 0, upper = 1, default = 1),
#     glmnet.s = p_dbl(lower = -7, upper = 7, default = 0, tags = "log", trafo = function(x) exp(x)),
#     # rpart
#     rpart.cp = p_dbl(lower = -7, upper = 0, default = log(0.01), tags = "log", trafo = function(x) exp(x)),
#     rpart.maxdepth = p_int(lower = 1, upper = 30, default = 30),
#     rpart.minbucket = p_int(lower = 1, upper = 100, default = 1),
#     rpart.minsplit = p_int(lower = 1, upper = 100, default = 20),
#     # ranger
#     ranger.num.trees = p_int(lower = 1, upper = 2000),
#     ranger.sample.fraction = p_dbl(lower = 0.1, upper = 1),
#     ranger.mtry.power = p_int(lower = 0, upper = 1),
#     ranger.respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
#     ranger.min.node.size = p_int(lower = 1, upper = 100),
#     ranger.splitrule = p_fct(levels = c("gini", "extratrees")),
#     ranger.num.random.splits = p_int(lower = 1, upper = 100, default = 1L, depends = ranger.splitrule == "extratrees"),
#     # aknn
#     aknn.k = p_int(lower = 1L, upper = 50L),
#     aknn.distance = p_fct(levels = c("l2", "cosine", "ip"), default = "l2"),
#     aknn.M = p_int(lower = 18L, upper = 50L),
#     aknn.ef = p_dbl(lower = 2, upper = 6, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
#     aknn.ef_construction = p_dbl(lower = 2, upper = 7, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
#     # xgboost
#     xgboost.booster = p_fct(levels = c("gblinear", "gbtree", "dart")),
#     xgboost.nrounds = p_dbl(lower = 2, upper = 8, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
#     xgboost.eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.gamma =  p_dbl(lower = -10, upper = 2, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
#     xgboost.alpha = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
#     xgboost.subsample = p_dbl(lower = 0.1, upper = 1),
#     xgboost.max_depth = p_int(lower = 1, upper = 15, depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.min_child_weight = p_dbl(lower = 1, upper = 5, tags = "log", trafo = function(x) exp(x), depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.colsample_bytree = p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.colsample_bylevel = p_dbl(lower = 0.01, upper = 1, depends = xgboost.booster %in% c("dart", "gbtree")),
#     xgboost.rate_drop = p_dbl(lower = 0, upper = 1, depends = xgboost.booster == "dart"),
#     xgboost.skip_drop = p_dbl(lower =  0, upper = 1, depends = xgboost.booster == "dart"),
#     # learner
#     trainsize = p_dbl(lower = 0.05, upper = 1, tag = "budget"),
#     repl = p_int(lower = 1, upper = 10, tag = "budget"),
#     num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
#     learner = p_fct(levels = c("aknn", "glmnet", "ranger", "rpart", "svm", "xgboost"))
# )
# # Add dependencies
# mm = map(domain$params$learner$levels, function(x) {
#     nms = names(domain$params)[startsWith(names(domain$params), x)]
#     map(nms, function(nm) domain$add_dep(nm, "learner", CondEqual$new(x)))
# })
# zz = generate_design_random(domain, 5L)
# dput(zz$transpose())

task_ids = c("1040", "1049", "1050", "1053", "1056", "1063", "1067", "1068",
  "11", "1111", "12", "14", "1461", "1462", "1464", "1468", "1475",
  "1476", "1478", "1479", "1480", "1485", "1486", "1487", "1489",
  "1494", "1497", "15", "1501", "1510", "1515", "16", "18", "181",
  "182", "188", "22", "23", "23381", "24", "28", "29", "3", "307",
  "31", "312", "32", "334", "37", "375", "377", "38", "40496",
  "40498", "40499", "40536", "40670", "40701", "40900", "40966",
  "40975", "40978", "40979", "40981", "40982", "40983", "40984",
  "40994", "41138", "41142", "41143", "41146", "41156", "41157",
  "41212", "4134", "4154", "42", "44", "4534", "4538", "458", "46",
  "469", "470", "50", "54", "60", "6332")

sample_pars = list(list(glmnet.alpha = 0.242915591457859, glmnet.s = 72.296707467667,
    trainsize = 0.399154885241296, repl = 7L, num.impute.selected.cpo = "impute.mean",
    learner = "glmnet"), list(xgboost.booster = "gblinear", xgboost.nrounds = 7L,
    xgboost.lambda = 187.686254100223, xgboost.alpha = 0.228921673644128,
    xgboost.subsample = 0.552106090006419, trainsize = 0.721934067364782,
    repl = 10L, num.impute.selected.cpo = "impute.hist", learner = "xgboost"),
    list(rpart.cp = 0.413522433060648, rpart.maxdepth = 25L,
        rpart.minbucket = 17L, rpart.minsplit = 77L, trainsize = 0.0602473553270102,
        repl = 9L, num.impute.selected.cpo = "impute.mean", learner = "rpart"),
    list(ranger.num.trees = 425L, ranger.sample.fraction = 0.532143895467743,
        ranger.mtry.power = 1L, ranger.respect.unordered.factors = "ignore",
        ranger.min.node.size = 88L, ranger.splitrule = "gini",
        trainsize = 0.115718818735331, repl = 8L, num.impute.selected.cpo = "impute.hist",
        learner = "ranger"), list(svm.kernel = "polynomial",
        svm.cost = 1.06229922645661, svm.tolerance = 0.000240170286741634,
        svm.degree = 4L, trainsize = 0.751200899295509, repl = 2L,
        num.impute.selected.cpo = "impute.hist", learner = "svm"))