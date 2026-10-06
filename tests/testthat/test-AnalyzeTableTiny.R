test_that("tinytable class", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  t <- AnalyzeTableTiny(carAhp)
  expect_true(inherits(t, "tinytable"))
})

test_that("tinytable values match Analyze", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  df <- Analyze(carAhp)
  t <- AnalyzeTableTiny(carAhp)
  tdf <- t@data
  expect_equal(nrow(tdf), nrow(df))
  expect_equal(names(tdf), names(df))
  numcols <- setdiff(names(tdf), " ")
  fmt <- function(x) ifelse(is.na(x), "", sprintf("%.1f%%", 100*x))
  expect_true(all(mapply(function(a, b) all(a == b),
                         tdf[, numcols],
                         lapply(df[, numcols], fmt))))
})

test_that("tinytable renders to html and typst", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "laptop.ahp", package="ahp")
  lp <- Load(ahpFile)
  Calculate(lp)
  t <- AnalyzeTableTiny(lp, fontsize = 0.8)
  h <- tempfile(fileext = ".html")
  y <- tempfile(fileext = ".typ")
  expect_no_error(tinytable::save_tt(t, h, overwrite = TRUE))
  expect_no_error(tinytable::save_tt(t, y, overwrite = TRUE))
  expect_true(file.size(h) > 0)
  expect_true(file.size(y) > 0)
})

test_that("tinytable priority and score variables", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "car.ahp", package="ahp")
  carAhp <- Load(ahpFile)
  Calculate(carAhp)
  for (v in c("weightContribution", "priority", "score")) {
    t <- AnalyzeTableTiny(carAhp, variable = v)
    expect_true(inherits(t, "tinytable"))
    expect_true(nrow(t@data) > 0)
  }
})

test_that("tinytable score variable on vacation", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "vacation.ahp", package="ahp")
  vacationAhp <- Load(ahpFile)
  Calculate(vacationAhp)
  t <- AnalyzeTableTiny(vacationAhp, decisionMaker = "Kid", variable = "score")
  expect_true(inherits(t, "tinytable"))
})

test_that("tinytable na_replace", {
  testthat::skip_if_not_installed("tinytable")
  ahpFile <- system.file("extdata", "laptop.ahp", package="ahp")
  lp <- Load(ahpFile)
  Calculate(lp)
  t_empty <- AnalyzeTableTiny(lp)
  t_dash <- AnalyzeTableTiny(lp, na_replace = "-")
  t_null <- AnalyzeTableTiny(lp, na_replace = NULL)
  has_na_cell <- function(t) any(vapply(as.list(t@data), function(col) any(col == "NA"), logical(1)))
  has_dash_cell <- function(t) any(vapply(as.list(t@data), function(col) any(col == "-"), logical(1)))
  has_empty_cell <- function(t) any(vapply(as.list(t@data), function(col) any(col == ""), logical(1)))
  expect_false(has_na_cell(t_empty))
  expect_false(has_na_cell(t_null))
  expect_true(has_empty_cell(t_empty))
  expect_true(has_empty_cell(t_null))
  expect_true(has_dash_cell(t_dash))
})
