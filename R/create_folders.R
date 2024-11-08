#' Create folders
#'
#' @return Creates folders
#' @export
#'
#' @examples
create_folders <- function() {
  dir.create(paste0(getwd(), "/01_sequences"), showWarnings = F)
  dir.create(paste0(getwd(), "/02_raw-data"), showWarnings = F)
  dir.create(paste0(getwd(), "/03_processed-data"), showWarnings = F)
  dir.create(paste0(getwd(), "/04_scripts"), showWarnings = F)
  dir.create(paste0(getwd(), "/05_reports"), showWarnings = F)
  dir.create(paste0(getwd(), "/06_manuscript"), showWarnings = F)
  dir.create(paste0(getwd(), "/07_presentation"), showWarnings = F)
  dir.create(paste0(getwd(), "/08_images"), showWarnings = F)
  dir.create(paste0(getwd(), "/09_protocols"), showWarnings = F)

  cat("\nWelcome to your R-Project:", basename(getwd()), "\n")
}
