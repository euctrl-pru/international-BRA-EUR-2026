#' Check the contents of a ZIP archive
#'
#' @param .pth  Path to the directory containing the ZIP file
#' @param .archive  Name of the ZIP file (e.g. "apdf-2024.zip")
#'
#' @return A data frame with columns Name, Length, Date (as returned by unzip())
check_zip_content <- function(.pth, .archive) {
  this_zip <- fs::path(.pth, .archive)
  unzip(this_zip, list = TRUE)
}


#' Read a subset of files from a ZIP archive
#'
#' Unzips selected files into a temporary directory, reads them into a named
#' list, then cleans up the temporary directory automatically.
#'
#' @param .pth      Path to the directory containing the ZIP file
#' @param .archive  Name of the ZIP file (e.g. "apdf-2024.zip")
#' @param files     Character vector of filenames to extract (as returned in
#'                  the Name column of check_zip_content())
#' @param .type     File type of the archived files. One of "parquet" or "csv".
#'                  Defaults to "parquet".
#'
#' @return A named list of data frames, one per extracted file. List names are
#'         the filenames stripped of their extension.
read_zip_content_DEPRECATED <- function(.pth, .archive, .files, .type = c("parquet", "csv"), ...) {
  
  .type     <- base::match.arg(.type)
  this_zip  <- fs::path(.pth, .archive)
  tmp_dir   <- fs::path(base::tempdir(), fs::path_ext_remove(.archive))
  
  # ensure temp dir exists and is cleaned up when function exits
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)
  
  # unzip only the requested files into the temp dir
  utils::unzip(this_zip, files = .files, exdir = tmp_dir)
  
  # select the reader based on .type
  reader <- switch(
    .type,
    parquet = arrow::read_parquet,
   # csv     = readr::read_csv
  )
  
  # build named paths and read in
  file_paths <- fs::path(tmp_dir, files)
  
  ds <- file_paths |>
    purrr::set_names(fs::path_ext_remove(base::basename(.files))) |>
    purrr::map(reader, ...)
  
  if(length(ds) == 1) ds <- ds[[1]] |> tibble::as_tibble()  # single element - return tibble
  
  return(ds)
}
# ============ old function (above) DEPRECATED


read_zip_content <- function(.pth, .archive, files = NULL, .type = c("parquet", "csv", "csv_auto")) {
  
  .type    <- base::match.arg(.type)
  this_zip <- fs::path(.pth, .archive)
  tmp_dir  <- fs::path(base::tempdir(), fs::path_ext_remove(.archive))
  
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)
  
  # if files = NULL, unzip everything and capture what was actually extracted
  if (base::is.null(files)) {
    files <- utils::unzip(this_zip, exdir = tmp_dir)
    # unzip() returns full paths when extracting all — strip to basenames
    # so the pipe below stays consistent
    files <- base::basename(files)
  } else {
    utils::unzip(this_zip, files = files, exdir = tmp_dir)
  }
  
  reader <- switch(
    .type,
    parquet  = arrow::read_parquet,
    csv      = readr::read_csv,
    csv_auto = read_csv12
  )
  
  # tmp_dir / files is now always well-defined, regardless of the NULL branch
  ds <- fs::path(tmp_dir, files) |>
    purrr::set_names(fs::path_ext_remove(base::basename(files))) |>
    purrr::map(reader) |>
    purrr::map(tibble::as_tibble)

  if (base::length(ds) == 1) {
    ds <- ds |> purrr::pluck(1)
  }
  
  return(ds)
}




#' Read csv files switching csv and csv2
#'
#' Utility function wrapper for readr::read_csv and readr::read_csv2.
#' Function tests the csv file and then picks the respective parser csv or csv2.
#'
#' @param .fn filename (including file path)
#' @param .colspec optional reading of selective columns
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_csv12 <- function(.fn, .colspec = NULL, .show_col_types = FALSE, ...){
  # test for csv or csv2
  tst <- readr::read_csv(.fn, n_max = 3, show_col_types = .show_col_types)
  siz <- dim(tst)[2]   # dim[2] == 1 for semicolon as read_csv expects comma
  
  # read data files
  if(siz > 1){
    df <- readr::read_csv(.fn, col_types = .colspec, show_col_types = .show_col_types)
  }else{
    df <- readr::read_csv2(.fn, col_types = .colspec, show_col_types = .show_col_types)
  }
  return(df)
}
