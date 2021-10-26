
# rbv2

Reproduce results from randombot_v2. Work in progress!

## Installation

You can install the released version from github using:

``` r
remotes::install_github("pfistfl/rbv2")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rbv2)
learner_id = "classif.svm"
task_id = 3
configuration = list("gamma" = 0.1, cost  = 10, sample.rate = .1)
eval_config(learner_id, task_id, configuration)
```

