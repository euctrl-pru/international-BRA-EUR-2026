#' combine rendered report with cover (and back cover)
#'
#' this script adds the cover page to the pdf version of the final report

library(pdftools)

pdf_combine(c(
    "./figures/Brazil-Europe-2025-Cover.pdf"
  , "./_book-local/Operational-Comparison-of-ANS-Performance.pdf"), 
            output = "./_book-local/Operational-Comparison-of-ANS-Performance-cover.pdf")
