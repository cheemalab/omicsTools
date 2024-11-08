print("Souring batch correction tools.")


metanr_packages <- function() {
  metr_pkgs <- c("sva", "Biobase", "pvca", "preprocessCore", "fitdistrplus", "fitdistrplus")
  
  list_installed <- installed.packages()
  
  new_pkgs <- subset(metr_pkgs, !(metr_pkgs %in% list_installed[, "Package"]))
  
  if (length(new_pkgs) != 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(new_pkgs)
    print(c(new_pkgs, " packages added..."))
  }
  
  if ((length(new_pkgs) < 1)) {
    print("No new packages added...")
  }
}

# load library
library(sva) # BiocManager::install("sva")
library(Biobase) # BiocManager::install("Biobase")
library(pvca) # BiocManager::install("pvca")
library(preprocessCore)

library(fitdistrplus)
library(extraDistr)
library(umap)
library(ggplot2)
library(plotly)
library(openxlsx)

library(ggsci)



pvcaDraw <- function(pvcaobj) {
  dat <- as.vector(unlist(pvcaobj$dat))
  label <- pvcaobj$label
  data <- data.frame(dat, label)
  # data$dat<-as.numeric(data$dat)
  p <-
    ggplot(data, aes(x = label, y = dat, fill = "LightSeaGreen")) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none")
  p <-
    p + theme(axis.text.x = element_text(vjust = 0.5, angle = 45)) + ylim(0, 1) +
    labs(x = "Source of variance", y = "Weighted average proportion variance") +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black")
    )
  
  p <- p + scale_fill_aaas()
  print(p)
}

pieDraw <- function(pvcaobj) {
  dat <- as.vector(unlist(pvcaobj$dat))
  Source <- pvcaobj$label
  df <- data.frame(dat, Source)
  # df$dat<-as.numeric(df$dat)
  df$percent <- round(df$dat / sum(df$dat) * 100, 3)
  df$Source <- paste0(Source, " (", df$percent, "%)")
  df <- df[order(df$percent), ]
  df$Source <- ordered(df$Source, levels = df$Source)
  df$Source <- ordered(df$Source, levels = df$Source)
  p <- ggplot(df, aes(x = "", weight = percent, fill = Source)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    xlab("") +
    ylab("") +
    # geom_text(aes(x = 1.3, y = Data, label = Sample))+
    theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black")
    )
  p <- p + scale_fill_aaas()
  
  print(p)
}



my_aprior <- function(gamma.hat) {
  m <- mean(gamma.hat, na.rm = T)
  s2 <- var(gamma.hat, na.rm = T)
  (2 * s2 + m^2) / s2
}
my_bprior <- function(gamma.hat) {
  m <- mean(gamma.hat, na.rm = T)
  s2 <- var(gamma.hat, na.rm = T)
  (m * s2 + m^3) / s2
}
my_it.sol <-
  function(sdat, g.hat, d.hat, g.bar, t2, a, b, conv = 1e-04) {
    n <- rowSums(!is.na(sdat))
    g.old <- g.hat
    d.old <- d.hat
    change <- 1
    count <- 0
    while (change > conv) {
      g.new <- postmean(g.hat, g.bar, n, d.old, t2)
      sum2 <- rowSums(
        (sdat - g.new %*% t(rep(1, ncol(
          sdat
        ))))^2,
        na.rm = TRUE
      )
      d.new <- postvar(sum2, n, a, b)
      change <-
        max(abs(g.new - g.old) / g.old, abs(d.new - d.old) / d.old, na.rm = T)
      g.old <- g.new
      d.old <- d.new
      count <- count + 1
    }
    adjust <- rbind(g.new, d.new)
    rownames(adjust) <- c("g.star", "d.star")
    adjust
  }
combat <-
  function(dat,
           batch,
           mod = NULL,
           par.prior = "auto",
           fit.method = "mle",
           mean.only = FALSE,
           ref.batch = NULL,
           BPPARAM = bpparam("SerialParam")) {
    if (mean.only == TRUE) {
      message("Using the 'mean only' version of ComBat")
    }
    if (length(dim(batch)) > 1) {
      return("This version of ComBat only allows one batch variable")
      stop("This version of ComBat only allows one batch variable")
    }
    batch <- as.factor(batch)
    batchmod <- model.matrix(~ -1 + batch)
    if (!is.null(ref.batch)) {
      if (!(ref.batch %in% levels(batch))) {
        return("reference level ref.batch is not one of the levels of the batch variable")
        stop("reference level ref.batch is not one of the levels of the batch variable")
      }
      cat(
        "Using batch =",
        ref.batch,
        "as a reference batch (this batch won't change)\n"
      )
      ref <- which(levels(as.factor(batch)) == ref.batch)
      batchmod[, ref] <- 1
    } else {
      ref <- NULL
    }
    message("Found", nlevels(batch), "batches")
    n.batch <- nlevels(batch)
    batches <- list()
    for (i in 1:n.batch) {
      batches[[i]] <- which(batch == levels(batch)[i])
    }
    n.batches <- sapply(batches, length)
    if (any(n.batches == 1)) {
      mean.only <- TRUE
      message("Note: one batch has only one sample, setting mean.only=TRUE")
    }
    n.array <- sum(n.batches)
    design <- cbind(batchmod, mod)
    check <- apply(design, 2, function(x) {
      all(x == 1)
    })
    if (!is.null(ref)) {
      check[ref] <- FALSE
    }
    design <- as.matrix(design[, !check])
    message(
      "Adjusting for",
      ncol(design) - ncol(batchmod),
      "covariate(s) or covariate level(s)"
    )
    if (qr(design)$rank < ncol(design)) {
      if (ncol(design) == (n.batch + 1)) {
        return("The covariate is confounded with batch! Remove the covariate and rerun ComBat")
        stop("The covariate is confounded with batch! Remove the covariate and rerun ComBat")
      }
      if (ncol(design) > (n.batch + 1)) {
        if ((qr(design[, -c(1:n.batch)])$rank < ncol(design[, -c(1:n.batch)]))) {
          return(
            "The covariates are confounded! Please remove one or more of the covariates so the design is not confounded"
          )
          stop(
            "The covariates are confounded! Please remove one or more of the covariates so the design is not confounded"
          )
        } else {
          return(
            "At least one covariate is confounded with batch! Please remove confounded covariates and rerun ComBat"
          )
          stop(
            "At least one covariate is confounded with batch! Please remove confounded covariates and rerun ComBat"
          )
        }
      }
    }
    NAs <- any(is.na(dat))
    if (NAs) {
      message(c("Found", sum(is.na(dat)), "Missing Data Values"),
              sep = " "
      )
    }
    cat("Standardizing Data across genes\n")
    if (!NAs) {
      B.hat <- solve(crossprod(design), tcrossprod(
        t(design),
        as.matrix(dat)
      ))
    } else {
      B.hat <- apply(dat, 1, Beta.NA, design)
    }
    if (!is.null(ref.batch)) {
      grand.mean <- t(B.hat[ref, ])
    } else {
      grand.mean <- crossprod(n.batches / n.array, B.hat[1:n.batch, ])
    }
    if (!NAs) {
      if (!is.null(ref.batch)) {
        ref.dat <- dat[, batches[[ref]]]
        var.pooled <- ((ref.dat - t(design[batches[[ref]], ] %*% B.hat))^
                         2) %*% rep(1 / n.batches[ref], n.batches[ref])
      } else {
        var.pooled <- ((dat - t(design %*% B.hat))^2) %*%
          rep(1 / n.array, n.array)
      }
    } else {
      if (!is.null(ref.batch)) {
        ref.dat <- dat[, batches[[ref]]]
        var.pooled <- rowVars(ref.dat - t(design[batches[[ref]], ] %*% B.hat), na.rm = TRUE)
      } else {
        var.pooled <- rowVars(dat - t(design %*% B.hat),
                              na.rm = TRUE
        )
      }
    }
    stand.mean <- t(grand.mean) %*% t(rep(1, n.array))
    if (!is.null(design)) {
      tmp <- design
      tmp[, c(1:n.batch)] <- 0
      stand.mean <- stand.mean + t(tmp %*% B.hat)
    }
    s.data <- (dat - stand.mean) / (sqrt(var.pooled) %*% t(rep(
      1,
      n.array
    )))
    message("Fitting L/S model and finding priors")
    batch.design <- design[, 1:n.batch]
    if (!NAs) {
      gamma.hat <-
        solve(crossprod(batch.design), tcrossprod(
          t(batch.design),
          as.matrix(s.data)
        ))
    } else {
      gamma.hat <- apply(s.data, 1, Beta.NA, batch.design)
    }
    delta.hat <- NULL
    for (i in batches) {
      if (mean.only == TRUE) {
        delta.hat <- rbind(delta.hat, rep(1, nrow(s.data)))
      } else {
        delta.hat <- rbind(delta.hat, rowVars(s.data[, i],
                                              na.rm = TRUE
        ))
      }
    }
    gamma.bar <- rowMeans(gamma.hat)
    t2 <- rowVars(gamma.hat)
    a.prior <- apply(delta.hat, 1, my_aprior)
    b.prior <- apply(delta.hat, 1, my_bprior)
    
    #### test norm distribution
    isNorm <- function(d, bar, t2) {
      tryCatch({
        f1 <- fitdist(d, "norm")
        g1 <- try(gofstat(f1))
      })
      if ("try-error" %in% class(g1)) {
        return(FALSE)
      }
      if (g1$kstest != "not rejected") {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    #### test inverse gamma distribution
    isInverseGamma <- function(d, a, b) {
      f2 <-
        try(fitdist(d, "invgamma", start = list(alpha = a, beta = b)), silent = T)
      if ("try-error" %in% class(f2)) {
        return(FALSE)
      }
      g2 <- try(gofstat(f2), silent = T)
      if ("try-error" %in% class(g2)) {
        return(FALSE)
      }
      if (g2$kstest != "not rejected") {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    switch(par.prior,
           "noparameter" = passTest <-
             lapply(1:n.batch, function(x) {
               return(FALSE)
             }),
           "parameter" = passTest <-
             lapply(1:n.batch, function(x) {
               return(TRUE)
             }),
           "auto" = passTest <- bplapply(1:n.batch, function(i) {
             norm.test <- isNorm(gamma.hat[i, ], gamma.bar[i], t2[i])
             ig.test <- isInverseGamma(delta.hat[i, ], a.prior[i], b.prior[i])
             return(norm.test & ig.test)
           }, BPPARAM = BPPARAM)
    )
    names(passTest) <- levels(batch)
    addition_data <-
      list(
        gamma.hat = gamma.hat,
        delta.hat = delta.hat,
        passTest = passTest
      )
    
    gamma.star <-
      delta.star <- matrix(NA, nrow = n.batch, ncol = nrow(s.data))
    if (sum(passTest == TRUE) > 0) {
      message("Finding parametric adjustments")
      batchNum.par <- which(passTest == TRUE)
      results <- bplapply(batchNum.par, function(i) {
        if (mean.only) {
          gamma.star <- postmean(
            gamma.hat[i, ], gamma.bar[i],
            1, 1, t2[i]
          )
          delta.star <- rep(1, nrow(s.data))
        } else {
          temp <- my_it.sol(
            s.data[, batches[[i]]],
            gamma.hat[i, ],
            delta.hat[i, ],
            gamma.bar[i],
            t2[i],
            a.prior[i],
            b.prior[i]
          )
          gamma.star <- temp[1, ]
          delta.star <- temp[2, ]
        }
        list(gamma.star = gamma.star, delta.star = delta.star)
      }, BPPARAM = BPPARAM)
      for (i in batchNum.par) {
        gamma.star[i, ] <- results[[which(batchNum.par == i)]]$gamma.star
        delta.star[i, ] <- results[[which(batchNum.par == i)]]$delta.star
      }
    }
    if (sum(passTest == FALSE) > 0) {
      message("Finding nonparametric adjustments")
      batchNum.no <- which(passTest == FALSE)
      results <- bplapply(batchNum.no, function(i) {
        if (mean.only) {
          delta.hat[i, ] <- 1
        }
        temp <- int.eprior(
          as.matrix(s.data[, batches[[i]]]),
          gamma.hat[i, ], delta.hat[i, ]
        )
        list(gamma.star = temp[1, ], delta.star = temp[2, ])
      }, BPPARAM = BPPARAM)
      for (i in batchNum.no) {
        gamma.star[i, ] <- results[[which(batchNum.no == i)]]$gamma.star
        delta.star[i, ] <- results[[which(batchNum.no == i)]]$delta.star
      }
    }
    if (!is.null(ref.batch)) {
      gamma.star[ref, ] <- 0
      delta.star[ref, ] <- 1
    }
    message("Adjusting the Data\n")
    bayesdata <- s.data
    j <- 1
    for (i in batches) {
      bayesdata[, i] <- (bayesdata[, i] - t(batch.design[i, ] %*% gamma.star)) /
        (sqrt(delta.star[j, ]) %*% t(rep(
          1,
          n.batches[j]
        )))
      j <- j + 1
    }
    bayesdata <- (bayesdata * (sqrt(var.pooled) %*% t(rep(
      1,
      n.array
    )))) + stand.mean
    if (!is.null(ref.batch)) {
      bayesdata[, batches[[ref]]] <- dat[, batches[[ref]]]
    }
    # cat(unlist(passTest))
    return(list(bayesdata = bayesdata, additiondata = addition_data))
  }

environment(my_it.sol) <- asNamespace("sva")
environment(combat) <- asNamespace("sva")


drawPrior <- function(priorData, batchName = "2") {
  gamma.hat <- priorData$gamma.hat
  delta.hat <- priorData$delta.hat
  gamma.bar <- rowMeans(gamma.hat)
  t2 <- rowVars(gamma.hat)
  a.prior <- apply(delta.hat, 1, aprior)
  b.prior <- apply(delta.hat, 1, bprior)
  batchNum <- which(names(priorData$passTest) == batchName)
  
  par(mfrow = c(2, 2))
  lwd <- 2
  myColor <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00"
  )
  tmp <- density(gamma.hat[batchNum, ])
  plot(
    tmp,
    type = "l",
    bty = "l",
    las = 1,
    lwd = lwd,
    col = myColor[5],
    main = substitute(
      paste(
        "Density Plot of Batch '", batchName, "' for ",
        hat(gamma)
      ),
      list(batchName = batchName)
    )
  )
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  lines(xx,
        dnorm(xx, gamma.bar[batchNum], sqrt(t2[batchNum])),
        lwd = lwd,
        col = myColor[6]
  )
  qqnorm(
    gamma.hat[batchNum, ],
    bty = "l",
    las = 1,
    lwd = lwd,
    col = myColor[2],
    main = substitute(
      paste(
        "Normal Q-Q Plot of Batch '", batchName, "' for ",
        hat(gamma)
      ),
      list(batchName = batchName)
    )
  )
  qqline(gamma.hat[batchNum, ], lwd = lwd, col = myColor[6])
  tmp <- density(delta.hat[batchNum, ])
  xx <- seq(min(tmp$x), max(tmp$x), length = 100)
  tmp1 <-
    list(x = xx, y = dinvgamma(xx, a.prior[batchNum], b.prior[batchNum]))
  plot(
    tmp,
    type = "l",
    bty = "l",
    las = 1,
    ylim = c(0, max(tmp$y, tmp1$y)),
    lwd = lwd,
    col = myColor[5],
    main = substitute(
      paste(
        "Density Plot of Batch '", batchName, "' for ",
        hat(delta)
      ),
      list(batchName = batchName)
    )
  )
  lines(tmp1, lwd = lwd, col = myColor[6])
  invgam <-
    1 / qgamma(
      1 - ppoints(ncol(delta.hat)), a.prior[batchNum],
      b.prior[batchNum]
    )
  qqplot(
    invgam,
    delta.hat[batchNum, ],
    bty = "l",
    las = 1,
    lwd = lwd,
    col = myColor[2],
    main = substitute(
      paste(
        "Inverse Gamma Q-Q Plot of Batch '",
        batchName,
        "' for ",
        hat(delta)
      ),
      list(batchName = batchName)
    ),
    ylab = "Sample Quantiles",
    xlab = "Theoretical Quantiles"
  )
  lines(c(0, max(invgam)), c(0, max(invgam)), lwd = lwd, col = myColor[6])
}

environment(drawPrior) <- asNamespace("sva")




myRF <- function(d, ntree, nodesize) {
  label_index <- grep("^label$", colnames(d))
  header <- colnames(d[-label_index])
  header_name <- paste0("index_", 1:length(header))
  names(header) <- header_name
  colnames(d)[-label_index] <- header_name
  RandomForest <-
    randomForest(
      label ~ .,
      data = d,
      importance = T,
      ntree = ntree,
      nodesize = nodesize,
      na.action = na.exclude
    )
  imps <- data.frame(importance(RandomForest))
  
  impScore <- imps$MeanDecreaseAccuracy
  imps <- imps[order(impScore, decreasing = T), ]
  orderedFeatures <- rownames(imps)
  orderedFeatures <- header[orderedFeatures]
  return(list(features = orderedFeatures, mod = RandomForest))
}



dataCheck <- function(d, v = "none") {
  error <- NULL
  if (ncol(d) < 2) {
    error <- "Error: You may  set wrong separator!"
  } else if (sum(duplicated(d[, 1])) > 0) {
    error <- "Error: The first column should be unique sample id/name!"
  } else if (v == "none") {
    cna <- apply(d[, -1], 1, function(x) {
      sum(is.na(x))
    })
    if (max(cna) == ncol(d[, -1])) {
      error <- "Error: There exists feature (eg. prottein or gene) with all values are NA in the data matrix"
    }
  }
  return(error)
}

replace_missing_values <- function(d, v) {
  d <- d %>%
    mutate_if(is.character, ~ na_if(., "NA"))
  
  if (v != "none") {
    replacement_value <- switch(v,
                                "1" = 1,
                                "0" = 0,
                                minimum = min(d, na.rm = TRUE),
                                "0.1" = 0.1 * min(d, na.rm = TRUE)
    )
    
    # Apply replacement value to numeric columns
    d <- d %>%
      mutate_if(is.numeric, ~ ifelse(is.na(.), replacement_value, .))
  }
  
  return(d)
}

if (!requireNamespace("preprocessCore", quietly = TRUE)) {
  install.packages("preprocessCore")
}

# Load the preprocessCore package
library(preprocessCore)

# Define the function for quantile normalization
quantile_normalization <- function(data_matrix) {
  # Normalize the quantiles
  normalized_matrix <- normalize.quantiles(data_matrix)
  
  # Reassign the row names and column names
  rownames(normalized_matrix) <- rownames(data_matrix)
  colnames(normalized_matrix) <- colnames(data_matrix)
  
  # Return the normalized matrix
  return(normalized_matrix)
}


log2_transformation <- function(data_matrix) {
  # Perform log2 transformation
  transformed_matrix <- log2(data_matrix)
  
  # Reassign the row names and column names
  rownames(transformed_matrix) <- rownames(data_matrix)
  colnames(transformed_matrix) <- colnames(data_matrix)
  
  # Return the transformed matrix
  return(transformed_matrix)
}
