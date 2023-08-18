#' import Biobase pcva
#' @export
pvcaBF <- function(df, sampleInfo, batch_effect, threshold) {
  print("pvcaBF start")
  df <- data.matrix(t(df))
  #df[is.na(df)]<-0
  #df<-df[,rownames(sampleInfo)]
  sampleInfo <- sampleInfo[colnames(df),]
  phenoData <- new("AnnotatedDataFrame", data = sampleInfo)
  myExpressionSet <- Biobase::ExpressionSet(assayData = df,
                                            phenoData = phenoData)
  pvcaObj <-
    pcva::pvcaBatchAssess(myExpressionSet, batch_effect, threshold)
  print("pvcaBF end")
  return(pvcaObj)
}

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
