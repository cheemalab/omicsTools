# CRAN submission comments for omicsTools 2.0.0

## Release

This is an update from CRAN version 1.1.7. Version 2.0.0 is a deliberately
simplified major release with intentional public API removals documented in NEWS.
It adds a validated table workflow, blank filtering, group comparisons with FDR
control, and self-contained QC reports while reducing required dependencies.

## Maintainer change

Amrita Cheema is the designated maintainer at metabolomics@georgetown.edu,
replacing Yaoxiang Li. The CRAN incoming feasibility NOTE identifies this
intentional maintainer change. Written consent from the previous maintainer
must accompany the transfer.

## Test environment and results

Windows 11 x64, R 4.6.1 (ucrt).
R CMD build followed by R CMD check --as-cran --no-manual.
The source archive was checked with all suggested packages available.
0 errors, 0 warnings, and 1 NOTE for the maintainer change.
Examples, tests, and vignette rebuilding pass.

Checks on Linux/macOS and R-devel have not been performed locally.

## Reverse dependencies

CRAN's package dependency database was queried on 2026-11-04.
No reverse Depends, Imports, LinkingTo, Suggests, or Enhances dependencies
were listed for omicsTools.

## External resources

MS1 annotation queries CEU Mass Mediator through the optional cmmr package.
Package checks make no live API requests. Tests use mocked search responses.
There are no downloads during installation or package loading.
