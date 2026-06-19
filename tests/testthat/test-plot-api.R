test_that("autoplot returns landscape ggplot output", {
  gg <- ggplot2::ggplot(
    data.frame(x = 1, y = 1),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()
  landscape_2d <- structure(
    list(plot = gg),
    class = c("2d_static_landscape", "2d_landscape", "landscape")
  )
  landscape_3d <- structure(
    list(plot = plotly::plot_ly(x = 1, y = 1, z = 1), plot_2 = gg),
    class = c("3d_static_landscape", "3d_landscape", "landscape")
  )

  expect_s3_class(ggplot2::autoplot(landscape_2d), "ggplot")
  expect_s3_class(ggplot2::autoplot(landscape_3d), "ggplot")
})

test_that("plotly_ld returns interactive landscape output", {
  landscape <- structure(
    list(plot = plotly::plot_ly(x = 1, y = 1, z = 1)),
    class = c("3d_static_landscape", "3d_landscape", "landscape")
  )

  expect_s3_class(plotly_ld(landscape), "plotly")
})

test_that("legacy landscape plot method warns and delegates", {
  gg <- ggplot2::ggplot(
    data.frame(x = 1, y = 1),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()
  landscape_2d <- structure(
    list(plot = gg),
    class = c("2d_static_landscape", "2d_landscape", "landscape")
  )
  landscape_3d <- structure(
    list(plot = plotly::plot_ly(x = 1, y = 1, z = 1), plot_2 = gg),
    class = c("3d_static_landscape", "3d_landscape", "landscape")
  )
  landscape_animation <- structure(
    list(plot = plotly::plot_ly(x = 1, y = 1, z = 1),
         plot_2 = gg,
         mat_3d = landscape_3d),
    class = c("3d_animation_landscape", "3d_landscape_batch", "landscape")
  )

  expect_warning(expect_s3_class(plot(landscape_2d), "ggplot"), "deprecated")
  expect_warning(expect_s3_class(plot(landscape_3d), "plotly"), "deprecated")
  expect_warning(expect_s3_class(plot(landscape_3d, 2), "ggplot"), "deprecated")
  expect_warning(
    expect_s3_class(plot(landscape_animation, 3), "ggplot"),
    "deprecated"
  )
})
