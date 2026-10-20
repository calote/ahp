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
  # names column in the table carries leading spaces; trim before compare
  expect_equal(trimws(names(tdf)), trimws(names(df)))
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

test_that("tinytable width and names_fontsize", {
  testthat::skip_if_not_installed("tinytable")
  m2 <- Load(system.file("extdata", "ejmultinivel.ahp", package="ahp"))
  Calculate(m2)
  t_auto <- AnalyzeTableTiny(m2)
  t_wide <- AnalyzeTableTiny(m2, width = 1)
  t_small <- AnalyzeTableTiny(m2, names_fontsize = 0.7)
  expect_true(all(vapply(list(t_auto, t_wide, t_small),
                         function(t) inherits(t, "tinytable"), logical(1))))
  y <- tempfile(fileext = ".typ")
  expect_no_error(tinytable::save_tt(t_auto, y, overwrite = TRUE))
  expect_true(file.size(y) > 0)
})

test_that("tinytable NA cells have white background", {
  testthat::skip_if_not_installed("tinytable")
  lp <- Load(system.file("extdata", "laptop.ahp", package="ahp"))
  Calculate(lp)
  df <- Analyze(lp)
  na_row <- which(is.na(df$Inconsistency))
  expect_true(length(na_row) > 0)  # the fixture really has a missing value
  t <- AnalyzeTableTiny(lp, style = "fill")
  j_incon <- match("Inconsistency", names(t@data))
  hit <- FALSE
  for (k in seq_along(t@lazy_style)) {
    cl <- t@lazy_style[[k]]
    if ("background" %in% names(cl) && !is.null(cl$background) &&
        isTRUE(eval(cl$i) == na_row) &&
        isTRUE(eval(cl$j) == j_incon) &&
        identical(unname(eval(cl$background)), "white")) hit <- TRUE
  }
  expect_true(hit)
})

test_that("tinytable pill style wraps values", {
  testthat::skip_if_not_installed("tinytable")
  m2 <- Load(system.file("extdata", "ejmultinivel.ahp", package="ahp"))
  Calculate(m2)
  t_pill <- AnalyzeTableTiny(m2)
  t_fill <- AnalyzeTableTiny(m2, style = "fill")
  h_pill <- tempfile(fileext = ".html")
  y_pill <- tempfile(fileext = ".typ")
  h_fill <- tempfile(fileext = ".html")
  tinytable::save_tt(t_pill, h_pill, overwrite = TRUE)
  tinytable::save_tt(t_pill, y_pill, overwrite = TRUE)
  tinytable::save_tt(t_fill, h_fill, overwrite = TRUE)
  html_pill <- paste(readLines(h_pill, warn = FALSE), collapse = "\n")
  typ_pill <- paste(readLines(y_pill, warn = FALSE), collapse = "\n")
  html_fill <- paste(readLines(h_fill, warn = FALSE), collapse = "\n")
  expect_true(grepl("<span", html_pill, fixed = TRUE))
  expect_true(grepl("#box(fill", typ_pill, fixed = TRUE))
  expect_false(grepl("<span", html_fill, fixed = TRUE))
})

test_that("tinytable pill mode stores no cell backgrounds", {
  testthat::skip_if_not_installed("tinytable")
  m2 <- Load(system.file("extdata", "ejmultinivel.ahp", package="ahp"))
  Calculate(m2)
  has_bg_calls <- function(t) {
    any(vapply(t@lazy_style,
               function(cl) "background" %in% names(cl) && !is.null(cl$background),
               logical(1)))
  }
  expect_false(has_bg_calls(AnalyzeTableTiny(m2)))
  expect_true(has_bg_calls(AnalyzeTableTiny(m2, style = "fill")))
})

test_that("tinytable col_gap adds space between columns", {
  testthat::skip_if_not_installed("tinytable")
  m2 <- Load(system.file("extdata", "ejmultinivel.ahp", package="ahp"))
  Calculate(m2)
  t0 <- AnalyzeTableTiny(m2)
  t1 <- AnalyzeTableTiny(m2, col_gap = 10)
  # HTML: extra cell padding, applied to headers AND body cells so that
  # both stay aligned. The rule is scoped to a unique class, so that it
  # does not leak into other tinytable tables of the same document.
  h <- tempfile(fileext = ".html")
  tinytable::save_tt(t1, h, overwrite = TRUE)
  html <- paste(readLines(h, warn = FALSE), collapse = "\n")
  expect_true(grepl("padding-right: 18.7px", html, fixed = TRUE))
  expect_true(grepl("white-space: nowrap", html, fixed = TRUE))
  expect_false(grepl("table.tinytable th, table.tinytable td { padding-right",
                     html, fixed = TRUE))
  # Typst: column gutter, which separates whole columns (headers included)
  y <- tempfile(fileext = ".typ")
  tinytable::save_tt(t1, y, overwrite = TRUE)
  typ <- paste(readLines(y, warn = FALSE), collapse = "\n")
  expect_true(grepl("gutter: 10pt", typ, fixed = TRUE))
  # no gap by default
  y0 <- tempfile(fileext = ".typ")
  tinytable::save_tt(t0, y0, overwrite = TRUE)
  typ0 <- paste(readLines(y0, warn = FALSE), collapse = "\n")
  expect_false(grepl("gutter: 10pt", typ0, fixed = TRUE))
  # the HTML rule of a table with gap must not affect a table without it
  h0 <- tempfile(fileext = ".html")
  tinytable::save_tt(t0, h0, overwrite = TRUE)
  html0 <- paste(readLines(h0, warn = FALSE), collapse = "\n")
  expect_false(grepl("padding-right: 18.7px", html0, fixed = TRUE))
})

test_that("AnalyzeTable_to_png insert returns markdown", {
  testthat::skip_if_not_installed("webshot2")
  testthat::skip_if_not_installed("htmltools")
  m2 <- Load(system.file("extdata", "laptop.ahp", package="ahp"))
  Calculate(m2)
  f <- tempfile(fileext = ".png")
  # outside a knitted document the markdown is printed and the path returned
  out <- capture.output(r <- AnalyzeTable_to_png(m2, f, insert = TRUE, caption = "Cap"))
  expect_true(file.exists(f))
  expect_true(any(grepl("![Cap]", out, fixed = TRUE)))
  expect_true(any(grepl(basename(f), out, fixed = TRUE)))
  expect_equal(r, f)
})

test_that("AnalyzeTable_to_png writes a PNG", {
  testthat::skip_if_not_installed("webshot2")
  testthat::skip_if_not_installed("htmltools")
  m2 <- Load(system.file("extdata", "laptop.ahp", package="ahp"))
  Calculate(m2)
  f <- tempfile(fileext = ".png")
  r <- AnalyzeTable_to_png(m2, file = f)
  expect_equal(r, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})
