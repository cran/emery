## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(emery)
set.seed(65123)

## ----simulate binary data-----------------------------------------------------
ex_bin_data <- 
  generate_multimethod_data(
    type = "binary",
    n_method = 3,
    n_obs = 200,
    se = c(0.85, 0.90, 0.95),
    sp = c(0.95, 0.90, 0.85),
    method_names = c("alpha", "beta", "gamma")
  )
ex_bin_data$generated_data[98:103, ]

## -----------------------------------------------------------------------------
ex_bin <- 
  estimate_ML(
    type = "binary",
    data = ex_bin_data$generated_data,
    init = list(prev_1 = 0.8, se_1 = c(0.7, 0.8, 0.75), sp_1 = c(0.85, 0.95, 0.75))
  )
ex_bin

## ----fig.width=7, fig.height=4------------------------------------------------
plot(ex_bin)

## ----fig.width=7, fig.height=4------------------------------------------------
plot(ex_bin, params = ex_bin_data$params)

## -----------------------------------------------------------------------------
pmf_pos_ex <- 
  matrix(
    c(
      c(0.05, 0.10, 0.15, 0.30, 0.40),
      c(0.00, 0.05, 0.20, 0.25, 0.50),
      c(0.10, 0.15, 0.20, 0.25, 0.30)
    ),
    nrow = 3, 
    byrow = TRUE
  )

pmf_pos_ex


## -----------------------------------------------------------------------------
pmf_neg_ex <- pmf_pos_ex[, 5:1]

## -----------------------------------------------------------------------------
ex_ord_data <- 
  generate_multimethod_data(
    type = "ordinal",
    n_method = 3,
    n_obs = 200,
    pmf_pos = pmf_pos_ex,
    pmf_neg = pmf_neg_ex,
    method_names = c("alice", "bob", "carrie"),
    level_names = c("strongly dislike", "dislike", "neutral", "like", "strongly like")
  )
ex_ord_data$generated_data[98:103, ]

## -----------------------------------------------------------------------------
ex_ord <- 
  estimate_ML(
    type = "ordinal",
    data = ex_ord_data$generated_data,
    level_names = ex_ord_data$params$level_names
  )
ex_ord

## ----fig.width=7, fig.height=4------------------------------------------------
plot(ex_ord, params = ex_ord_data$params)

## -----------------------------------------------------------------------------
ex_con_data <- 
  generate_multimethod_data(
    type = "continuous",
    n_method = 3,
    n_obs = 200,
    method_names = c("phi", "kappa", "sigma")
  )
ex_con_data$generated_data[98:103, ]

## -----------------------------------------------------------------------------
ex_con <- 
  estimate_ML(
    type = "continuous",
    data = ex_con_data$generated_data
  )
ex_con

## ----fig.width=7, fig.height=4------------------------------------------------
plot(ex_con, params = ex_con_data$params)

## -----------------------------------------------------------------------------
ex_boot_bin <- boot_ML(
  type = "binary",
  data = ex_bin_data$generated_data,
  n_boot = 20
)

# print the estimates of sensitivity from the complete data set
ex_boot_bin$v_0@results$se_est

# print the first 3 bootstrap estimates of sensitivity
ex_boot_bin$v_star[[1]]$se_est
ex_boot_bin$v_star[[2]]$se_est
ex_boot_bin$v_star[[3]]$se_est

