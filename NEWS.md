# omicsTools 2.0.0

* Added a unified table workflow with sample/feature metadata validation,
  batch-aware blank filtering, missing-value handling, QC-RLSC/PQN normalization,
  and processing records.
* Added two-group comparisons using Welch or Wilcoxon tests with
  Benjamini-Hochberg adjusted p-values.
* Added self-contained HTML QC reports and complete result bundles with original
  inputs, processed tables, feature/sample metadata, QC summaries, and settings.
* MS1 annotation now searches CEU Mass Mediator through the optional cmmr package.
  The legacy annotation database and external data-release downloader were removed.
* Retained SCIEX/Qtrap processing, targeted calibration, robust sample QC, PCA,
  heatmaps, LC-MS feature plots, spectrum plotting, and MSP writing.
* Removed the legacy OmicsData S4/synthetic-data system, generic predictive
  modeling, feature selection, graph/enrichment displays, raw-file MS/MS
  inspection/conversion, and incomplete or duplicate app screens.
  These are intentional breaking API changes from CRAN version 1.1.7.
* Reduced required external dependencies and replaced the app wrapper with the
  standard Shiny launcher. Excel import and specialized exports are optional.
* Updated package authorship. Amrita Cheema is the designated maintainer at
  metabolomics@georgetown.edu.
* Added the hex logo and horizontal workflow figure. Standalone QC reports omit
  personal credits while preserving author metadata in exported settings.
