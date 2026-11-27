# Tests (F): Plot smoke tests
#
# A plot's visual correctness can't easily be asserted, but we can verify that
# each plot type RUNS without error. (Output is discarded to a null PDF device.)

test_that("all main plot types run without error", {

  s <- riskyr(prev = .1, sens = .8, spec = .9, N = 1000)

  pdf(NULL); on.exit(dev.off())

  for (ty in c("prism", "tree", "tab", "area", "bar", "icons", "curve", "plane")) {

    expect_no_error(plot(s, type = ty))
  }

})


test_that("default plot() and type aliases run without error", {

  s <- riskyr(prev = .10, sens = .80, spec = .90, N = 1000)

  pdf(NULL); on.exit(dev.off())

  expect_no_error(plot(s))                   # default (= "prism")
  expect_no_error(plot(s, type = "mosaic"))  # alias of "area"
  expect_no_error(plot(s, type = "net"))     # alias of "prism"
  expect_no_error(plot(s, type = "fbar"))    # alias of "bar"

})


test_that("every exported plot_*() function runs with default arguments", {

  pdf(NULL); on.exit(dev.off())

  # The exported plot_*() functions that work from package defaults:
  plot_fns <- c("plot_area", "plot_bar", "plot_curve", "plot_fnet",
                "plot_icons", "plot_plane", "plot_prism", "plot_tab")

  for (fn in plot_fns) {

    expect_no_error(do.call(fn, list()))
  }

})


test_that("deprecated plot functions still run (plot_mosaic, plot_tree)", {

  pdf(NULL); on.exit(dev.off())

  expect_no_error(plot_mosaic())  # deprecated -> delegates to plot_area()
  expect_no_error(plot_tree())    # deprecated -> delegates to plot_prism()

})


test_that("plot_crisk() runs for cumulative-risk data", {

  x <- seq(0, 100, by = 10)
  y <- c(0, 0, 0, 8, 24, 50, 70, 80, 83, 85, 85)

  pdf(NULL); on.exit(dev.off())

  expect_no_error(plot_crisk(x, y))
  expect_no_error(plot_crisk(x, y, fit_curve = TRUE))
  expect_no_error(plot_crisk(x, y, x_from = 30,
                             show_pas = TRUE, show_rem = TRUE, show_pop = TRUE))

})


test_that("key plot arguments run without error", {

  pdf(NULL); on.exit(dev.off())

  # Perspectives / layouts:
  expect_no_error(plot_prism(by = "cddc", area = "hr"))
  expect_no_error(plot_prism(by = "cd"))

  expect_no_error(plot_area(by = "cddc", scale = "f"))
  expect_no_error(plot_area(scale = "p"))

  expect_no_error(plot_fnet(by = "cddc"))
  expect_no_error(plot_tab(by = "cddc"))

  # Directions / selections:
  expect_no_error(plot_bar(dir = 1))
  expect_no_error(plot_bar(dir = 2))
  expect_no_error(plot_bar(dir = 3))

  expect_no_error(plot_curve(what = "all"))
  expect_no_error(plot_curve(what = c("PPV", "NPV")))

  expect_no_error(plot_plane(what = "PPV"))

})


# eof.
