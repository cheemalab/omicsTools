#' Perform Principal Variance Component Analysis for Batch Effect Assessment
#'
#' This function performs Principal Variance Component Analysis to assess batch effects in the dataset.
#'
#' @param data_matrix A data frame or matrix where rows represent features and columns represent samples.
#' @param sample_info A data frame containing sample information with rows matching the columns of `data_matrix`.
#' @param batch_effects A character vector of column names in `sample_info` that represent batch effects.
#' @param threshold A numeric value between 0 and 1 to specify the PVCA threshold.
#'
#' @return A PVCA object containing the results of the batch effect assessment.
#' @importFrom Biobase ExpressionSet
#' @importFrom pvca pvcaBatchAssess
#' @export
#'
#' @examples
#' # Example data
#' set.seed(123)
#' data_matrix <- data.frame(
#'   sample1 = rnorm(100),
#'   sample2 = rnorm(100),
#'   sample3 = rnorm(100),
#'   sample4 = rnorm(100),
#'   sample5 = rnorm(100),
#'   sample6 = rnorm(100),
#'   sample7 = rnorm(100),
#'   sample8 = rnorm(100),
#'   sample9 = rnorm(100),
#'   sample10 = rnorm(100)
#' )
#' rownames(data_matrix) <- paste0("feature", 1:100)
#'
#' sample_info <- data.frame(
#'   dose = c(0, 1, 2, 1, 0, 2, 1, 0, 2, 1),
#'   time = c(-1, 1, 25, -1, 1, 25, -1, 1, 25, -1),
#'   batch = rep(c("A", "B", "C"), length.out = 10)
#' )
#' rownames(sample_info) <- colnames(data_matrix)
#'
#' # Perform Batch Effect Assessment
#' pvca_results <- perform_batch_assessment(data_matrix, sample_info, c("batch", "dose", "time"), 0.6)
perform_batch_assessment <- function(data_matrix, sample_info, batch_effects, threshold) {
  cli::cli_h1("Starting Batch Effect Assessment")

  # Ensure the input data frame is properly formatted
  if (!is.data.frame(data_matrix) && !is.matrix(data_matrix)) {
    cli::cli_alert_danger("Input `data_matrix` must be a data frame or matrix.")
    stop("Input `data_matrix` must be a data frame or matrix.")
  }

  if (!is.data.frame(sample_info)) {
    cli::cli_alert_danger("Input `sample_info` must be a data frame.")
    stop("Input `sample_info` must be a data frame.")
  }

  if (!all(batch_effects %in% colnames(sample_info))) {
    cli::cli_alert_danger("All elements in `batch_effects` must be column names in `sample_info`.")
    stop("All elements in `batch_effects` must be column names in `sample_info`.")
  }

  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    cli::cli_alert_danger("Input `threshold` must be a numeric value between 0 and 1.")
    stop("Input `threshold` must be a numeric value between 0 and 1.")
  }

  if (all(colnames(data_matrix) == rownames(sample_info))) {
    cli::cli_alert_info("Sample names match between columns of `data_matrix` and rows of `sample_info`. No transposition needed.")
    data_matrix <- data.matrix(data_matrix)
  } else if (all(rownames(data_matrix) == rownames(sample_info))) {
    cli::cli_alert_info("Sample names match between rows of `data_matrix` and rows of `sample_info`. Transposing `data_matrix`.")
    data_matrix <- data.matrix(t(data_matrix))
  } else {
    cli::cli_alert_danger("Sample names do not match between `data_matrix` and `sample_info`.")
    stop("Sample names do not match between `data_matrix` and `sample_info`.")
  }

  data_matrix[is.na(data_matrix)] <- 0

  # Align sample names
  common_samples <- intersect(colnames(data_matrix), rownames(sample_info))
  if (length(common_samples) != ncol(data_matrix) || length(common_samples) != nrow(sample_info)) {
    cli::cli_alert_danger("Sample names in `data_matrix` do not match `sample_info`.")
    stop("Sample names in `data_matrix` do not match `sample_info`.")
  }
  data_matrix <- data_matrix[, common_samples]
  sample_info <- sample_info[common_samples, ]

  cli::cli_alert_info("Creating AnnotatedDataFrame.")
  pheno_data <- new("AnnotatedDataFrame", data = sample_info)

  cli::cli_alert_info("Creating ExpressionSet.")
  expression_set <- Biobase::ExpressionSet(
    assayData = data_matrix,
    phenoData = pheno_data
  )

  cli::cli_alert_info("Performing Batch Assessment.")
  pvca_results <- pvca::pvcaBatchAssess(expression_set, batch_effects, threshold)

  cli::cli_h1("Batch Effect Assessment Completed")
  return(pvca_results)
}





# pvcaBF <- function(df, sampleInfo, batch_effect, threshold) {
#   print("pvcaBF start")
#   df <- data.matrix(t(df))
#   #df[is.na(df)]<-0
#   #df<-df[,rownames(sampleInfo)]
#   sampleInfo <- sampleInfo[colnames(df),]
#   phenoData <- new("AnnotatedDataFrame", data = sampleInfo)
#   myExpressionSet <- Biobase::ExpressionSet(assayData = df,
#                                             phenoData = phenoData)
#   pvcaObj <-
#     pcva::pvcaBatchAssess(myExpressionSet, batch_effect, threshold)
#   print("pvcaBF end")
#   return(pvcaObj)
# }

#' import ggplot2
#' @export
pvcaDraw <- function(pvcaobj) {
  dat <- as.vector(unlist(pvcaobj$dat))
  label <- pvcaobj$label
  data <- data.frame(dat, label)
  #data$dat<-as.numeric(data$dat)
  p <-
    ggplot(data, aes(x = label, y = dat, fill = "LightSeaGreen")) +
    geom_bar(stat = "identity") + theme(legend.position = "none")
  p <-
    p + theme(axis.text.x = element_text(vjust = 0.5, angle = 45)) + ylim(0, 1) +
    labs(x = 'Source of variance', y = 'Weighted average proportion variance')
  print(p)
}

#' import ggplot2
#' @export
pieDraw <- function(pvcaobj) {
  dat <- as.vector(unlist(pvcaobj$dat))
  Source <- pvcaobj$label
  df <- data.frame(dat, Source)
  #df$dat<-as.numeric(df$dat)
  df$percent <- round(df$dat / sum(df$dat) * 100, 3)
  df$Source <- paste0(Source, " (", df$percent, "%)")
  df <- df[order(df$percent),]
  df$Source <- ordered(df$Source, levels = df$Source)
  df$Source <- ordered(df$Source, levels = df$Source)
  p <- ggplot(df, aes(x = "", weight = percent , fill = Source)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") + xlab('') + ylab('') +
    #geom_text(aes(x = 1.3, y = Data, label = Sample))+
    theme_minimal()
  print(p)
}
