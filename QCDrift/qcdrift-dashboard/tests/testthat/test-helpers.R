# Run from the project root with:  testthat::test_dir("tests/testthat")

root <- if (file.exists("R/config.R")) "." else file.path("..", "..")
source(file.path(root, "R", "config.R"))
source(file.path(root, "R", "fct_data.R"))
source(file.path(root, "R", "fct_metrics.R"))

# ---- helpers ------------------------------------------------------------------

test_that("pick_col returns the first match or NA", {
  df <- data.frame(io = 1, metabolites = "a")
  expect_equal(pick_col(df, c("injection_order", "io")), "io")
  expect_true(is.na(pick_col(df, c("zzz"))))
})

test_that("as_qc_flag handles logical, numeric and text", {
  expect_equal(as_qc_flag(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_equal(as_qc_flag(c(1, 0)), c(TRUE, FALSE))
  expect_equal(as_qc_flag(c("QC_1", "S1")), c(TRUE, FALSE))
})

test_that("get_plot honours aliases", {
  res <- list(plots = list(pca_before = "x"))
  expect_equal(get_plot(res, "pca_raw"), "x")
  expect_null(get_plot(res, "cv_raw"))
  expect_null(get_plot(NULL, "pca_raw"))
})

# ---- upload checks ----------------------------------------------------------------

test_that("check_upload rejects errors and empty tables", {
  expect_false(check_upload(simpleError("boom"))$ok)
  expect_false(check_upload(data.frame())$ok)
})

test_that("check_upload warns when QC markers are missing", {
  df <- data.frame(a = 1:3, b = 1:3, c = 1:3)
  ck <- check_upload(df, "QC")
  expect_true(ck$ok)
  expect_length(ck$warnings, 1)

  df2 <- data.frame(feature = 1:3, QC1 = 1, QC2 = 1, QC3 = 1, S1 = 1)
  expect_length(check_upload(df2, "QC")$warnings, 0)
})

# ---- CV and verdict ------------------------------------------------------------------

make_long <- function(corrected_sd) {
  qc_vals <- function(sd) 100 + c(-1, 1, -1, 1) * sd
  rbind(
    data.frame(io = 1:4, metabolite = "m1", value = qc_vals(30), qc = TRUE,
               sample = paste0("QC", 1:4), stage = "Raw"),
    data.frame(io = 1:4, metabolite = "m1", value = qc_vals(corrected_sd), qc = TRUE,
               sample = paste0("QC", 1:4), stage = "Corrected")
  ) |> transform(stage = factor(stage, levels = c("Raw", "Corrected")))
}

test_that("qc_cv_table gives one row per feature with both stages", {
  cv <- qc_cv_table(make_long(5))
  expect_equal(nrow(cv), 1)
  expect_true(all(c("Raw", "Corrected") %in% names(cv)))
  expect_lt(cv$Corrected, cv$Raw)
})

test_that("compute_verdict grades success, warning and danger", {
  good <- data.frame(metabolite = letters[1:10], Raw = 40, Corrected = 8)
  expect_equal(compute_verdict(good, 20)$level, "success")

  partial <- data.frame(metabolite = letters[1:10], Raw = 40,
                        Corrected = c(rep(8, 5), rep(30, 5)))
  expect_equal(compute_verdict(partial, 20)$level, "warning")

  bad <- data.frame(metabolite = letters[1:10], Raw = 40, Corrected = 45)
  expect_equal(compute_verdict(bad, 20)$level, "danger")
})

test_that("compute_verdict degrades gracefully without CV data", {
  v <- compute_verdict(NULL, 20)
  expect_false(v$available)
})

test_that("review_table flags features above the limit, worst first", {
  cv <- data.frame(metabolite = c("a", "b", "c"), Raw = c(30, 30, 30),
                   Corrected = c(5, 25, 15))
  rv <- review_table(cv, 20)
  expect_equal(rv$Feature[1], "b")
  expect_equal(rv$Review, c("Yes", "No", "No"))
})
