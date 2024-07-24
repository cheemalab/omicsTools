#' Transpose DataFrame
#'
#' This function transposes a data frame by converting rows to columns and columns to rows.
#' The first column is assumed to contain the variable names, and the remaining columns contain the values.
#'
#' @param df A data frame to be transposed.
#'
#' @return A transposed data frame.
#' @examples
#' # Example usage:
#' df <- data.frame(
#'   ID = c("A", "B", "C"),
#'   Var1 = c(1, 2, 3),
#'   Var2 = c(4, 5, 6)
#' )
#' transpose_df(df)
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
transpose_df <- function(df) {
  df %>%
    tidyr::pivot_longer(-1) %>%
    tidyr::pivot_wider(names_from = 1, values_from = 3)
}

#' MS1 Annotation
#' @export
ms1_annotation <- function(AnnotaData, masstole = 0.05, toleUnit = 1, annotaDB = "metlin", ionMode = "pos", adducts = NULL) {
  param <- list()

  # inputId="masstole",label="Tolerance",value=10,step=0.0001,min=0
  param$masstole <- masstole

  if (toleUnit == 1) {
    param$toleUnit <- "Da"
  } else if (toleUnit == 2) {
    param$toleUnit <- "ppm"
  } else {
  }

  # inputId="annotaDB", label="Annotation MS_database:", choices=c("METLIN"="metlin", "HMDB"="hmdb")

  param$annotaDB <- annotaDB

  # inputId="ionMode",  label="Mode:", choices=c("Positive"="pos", "Negative"="neg", "Neutral"="neu")

  param$ionMode <- ionMode

  # param$charge <- charge

  mzlist <- as.numeric(AnnotaData)
  param$mzlist <- mzlist[!is.na(mzlist)]

  AnnotaParam <- param

  ####
  MPchargeName <- c(
    "M+H-2H2O", "M+H-H2O", "M-H", "M-H2O+NH4", "M+H", "M+Li",
    "M+NH4", "M+Na", "M+CH3OH+H", "M+K", "M+ACN+H", "M+2Na-H",
    "M+ACN+Na", "M+2H", "M+H+Na", "M+2Na", "M+3H", "M+2H+Na", "M+H+2Na"
  )
  MNchargeName <- c("M-H2O-H", "M-H", "M+F", "M+Na-2H", "M+Cl", "M+K-2H", "M+FA-H", "M+CH3COO", "M-2H", "M-3H")

  HPchargeName <- c(
    "M+3H", "M+2H+Na", "M+H+2Na", "M+2H", "M+H+NH4", "M+H+Na", "M+H+K", "M+ACN+2H",
    "M+2Na", "M+2ACN+2H", "M+3ACN+2H", "M+H", "M+NH4", "M+Na", "M+CH3OH+H", "M+ACN+H",
    "M+IsoProp+H", "M+ACN+Na", "M+DMSO+H", "M+2Na-H", "M+IsoProp+Na+H", "2M+H", "2M+NH4",
    "2M+Na", "2M+3ACN+2H", "2M+K", "2M+3H2O+H", "2M+ACN+Na"
  )

  HNchargeName <- c(
    "M-2H", "M-3H", "M-H2O-H", "M-H", "M+Na-2H", "M+Cl", "M+K-2H", "M+FA-H",
    "M+Hac-H", "M+Br", "M+TFA-H", "2M-H", "2M+FA-H", "2M+Hac-H", "3M-H"
  )


  if (annotaDB == "metlin" && ionMode == "pos") {
    charge <- which(MPchargeName %in% adducts)
    param$charge <- charge
    AnnotaParam$charge <- charge
    chargeName <- MPchargeName[charge]
  } else if (annotaDB == "metlin" && ionMode == "neg") {
    charge <- which(MNchargeName %in% adducts)
    param$charge <- charge
    AnnotaParam$charge <- charge
    chargeName <- MNchargeName[AnnotaParam$charge]
  } else if (annotaDB == "hmdb" && ionMode == "pos") {
    charge <- which(HPchargeName %in% adducts)
    param$charge <- charge
    AnnotaParam$charge <- charge
    chargeName <- HPchargeName[AnnotaParam$charge]
  } else if (annotaDB == "hmdb" && ionMode == "neg") {
    charge <- which(HNchargeName %in% adducts)
    param$charge <- charge
    AnnotaParam$charge <- charge
    chargeName <- HNchargeName[AnnotaParam$charge]
  } else if (ionMode == "neu") {
    chargeName <- "M"
  } else {
    chargeName <- NULL
  }

  ####
  githubURL <- "https://github.com/YaoxiangLi/tidyview-data/raw/main/ms1_data.RData"
  load(url(githubURL))

  Annotation <- function(param) {
    adduct_list <- annotation_data$adduct_list

    ionList <- adduct_list[intersect(grep(param$annotaDB, adduct_list$compound_db), grep(param$ionMode, adduct_list$ion_mode)), ]

    calcMZLowerBound <- NULL
    calcMZUpperBound <- NULL
    if (param$ionMode == "pos" || param$ionMode == "neg") {
      calcMZLowerBound <- function(mz, unit, db, tole, charge, aum, mcof) {
        eps <- tole * ifelse(unit == "ppm", mz * 1e-6, 1)
        return(charge * (mz - eps - aum) / ifelse(db == "hmdb", mcof, 1))
      }
      calcMZUpperBound <- function(mz, unit, db, tole, charge, aum, mcof) {
        eps <- tole * ifelse(unit == "ppm", mz * 1e-6, 1)
        return(charge * (mz + eps - aum) / ifelse(db == "hmdb", mcof, 1))
      }
    } else if (param$ionMode == "neu") {
      calcMZLowerBound <- function(mz, unit, db, tole, charge, aum, mcof) {
        if (unit == "ppm") {
          return(mz / (1 + tole * 1e-6))
        } else {
          return(mz - tole)
        }
      }
      calcMZUpperBound <- function(mz, unit, db, tole, charge, aum, mcof) {
        if (unit == "ppm") {
          return(mz / (1 - tole * 1e-6))
        } else {
          return(mz + tole)
        }
      }
    }

    unit <- param$toleUnit
    db <- param$annotaDB
    tole <- param$masstole

    AnnotationTables <- list()

    for (addIdx in 1:length(param$charge)) {
      adduct <- param$charge[addIdx]
      queryResult <- NULL

      for (mz in param$mzlist) {
        minMz <- calcMZLowerBound(mz, unit, db, tole, ionList$charge[adduct], ionList$aum[adduct], ionList$mcof[adduct])
        maxMz <- calcMZUpperBound(mz, unit, db, tole, ionList$charge[adduct], ionList$aum[adduct], ionList$mcof[adduct])
        if (db == "metlin") {
          MS_database <- annotation_data$MS_database
          queryLines <- MS_database[intersect(which(as.numeric(MS_database$MetlinMass) >= minMz), which(as.numeric(MS_database$MetlinMass) <= maxMz)), c("LargeMetabo_ID", "MetlinMass", "MID", "Common_name", "Annotation")]
          queryLines <- unique(queryLines)
        } else {
          queryLines <- MS_database[which(MS_database$MonoisotopicMass >= minMz & MS_database$MonoisotopicMass <= maxMz & MS_database$Source == param$annotaDB), c("LargeMetabo_ID", "MonoisotopicMass", "MID", "Common_name", "Annotation")]
          queryLines <- unique(queryLines)
        }

        queryLinesWithQMZ <- cbind(QueryMZ = rep(mz, nrow(queryLines)), queryLines)
        queryResult <- rbind(queryResult, queryLinesWithQMZ)
        queryResult <- unique(queryResult)
      }

      AnnotationTables[addIdx] <- list(queryResult)
    }

    if (param$ionMode == "neu") {
      names(AnnotationTables) <- c("M")
    } else {
      names(AnnotationTables) <- ionList$name[param$charge]
    }

    return(AnnotationTables)
  }


  ## ---------------------------------------------------------------------------------------------------------

  parameter <- AnnotaParam
  resultTable <- NULL
  annotationResult <- Annotation(parameter)

  search_result <- tibble::tibble()
  for (name in names(annotationResult)) {
    search_result <-
      dplyr::bind_rows(
        search_result,
        annotationResult[[name]] |> dplyr::mutate(Adducts = name, .before = "MID")
      )
  }
  search_result <- search_result |> dplyr::select(QueryMZ, MetlinMass, Adducts, MID, Common_name, Annotation)
  search_result <- search_result |> dplyr::arrange(QueryMZ)
  search_result <- search_result |>
    dplyr::filter(grepl("Endogenous", Annotation)) |>
    dplyr::mutate(mz = as.character(QueryMZ))


  return(search_result)
}



# Define adducts to search ------------------------------------------------
# adduct_pos <- c("M+H", "M+Li", "M+NH4", "M+Na", "M+K", "M+H-H20")
# adduct_neg <- c("M-H", "M+Cl", "M-H2O-H")
# mz <- c(100, 201, 301, 401)
# search_result <- ms1_annotation(mz, 0.05, 1, "metlin", "neg", adduct_neg)






