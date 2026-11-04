test_that("migrated visualization functions return plots", {
  set.seed(1)
  pca <- plot_omics_pca(matrix(rnorm(40), nrow = 8), group = rep(c("A", "B"), each = 4))
  expect_s3_class(pca, "ggplot")
  expect_s3_class(attr(pca, "pca"), "prcomp")

  expect_s3_class(
    plot_lcms_features(data.frame(rt = 1:4, mz = 101:104, mode = c("pos", "neg", "pos", "neg"))),
    "ggplot"
  )
})

test_that("the compact heatmap and its example render without warnings", {
  example <- load_omics_data(omics_example_file("examples/heatmap.csv"))
  plot <- plot_omics_heatmap(example)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$caption, "Created by Yaoxiang Li")
  expect_warning(ggplot2::ggplot_build(plot), NA)
  shiny::testServer(omicsTools:::mod_vis_heatmap_server, args = list(id = "heatmap"), {
    session$setInputs(example = 1)
    session$flushReact()
    session$setInputs(id = "sample_id", features = names(example)[-1], scale = TRUE)
    expect_s3_class(plot(), "ggplot")
  })
})
