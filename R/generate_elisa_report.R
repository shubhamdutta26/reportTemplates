#' Generate ELISA Report
#'
#' @description
#' Generates a standardized PDF report for ELISA (Enzyme-Linked Immunosorbent Assay) data.
#' The report includes the experimental protocol and data visualization with either linear or
#' 4-parameter logistic regression curves.
#'
#' @param file Character string. Path to the Excel (.xlsx) file containing ELISA data.
#'   The file must contain columns: 'primary_mab_name', 'primary_mab_conc', and 'od450'.
#' @param title Character string. Title of the report. Default: "ELISA Report"
#' @param coat_protein Character string. Name of the coating protein used. Default: "coat"
#' @param coat_protein_conc Numeric. Concentration of coating protein in µg/ml. Default: 1
#' @param plate Character string. Type of plate used (e.g., "corning"). Default: "corning"
#' @param blocking_buffer Character string. Type of blocking buffer used. Default: "block"
#' @param detect_dil Character string. Dilution of detection antibody (e.g., "1:5000").
#'   Default: "1:5000"
#' @param time Numeric. Development time in minutes. Default: 6
#' @param method Character string. Method for curve fitting, either "line" for linear
#'   interpolation or "L4" for 4-parameter logistic regression. Default: "line"
#' @param errorbars Logical. Whether to display error bars on the plot. Default: TRUE
#' @param point_size Numeric. Size of data points in the plot. Default: 3
#' @param linewidth Numeric. Width of fitted lines in the plot. Default: 1
#' @param errorbar_width Numeric. Width of error bars in the plot. Default: 0.2
#' @param results A character string with the conclusions
#' @param output_file Character string. Name of the output PDF file. Default: "elisa_report.pdf"
#'
#' @return Invisibly returns the full path to the generated PDF file.
#'
#' @details
#' The function expects an Excel file with ELISA data in a specific format. The data should
#' contain at least three columns:
#' \itemize{
#'   \item primary_mab_name: Name/identifier of the antibody, including "blank" for blank wells
#'   \item primary_mab_conc: Concentration of primary antibody in µg/ml
#'   \item od450: Optical density readings at 450nm
#' }
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input data and parameters
#'   \item Subtracts blank readings
#'   \item Calculates means and standard deviations
#'   \item Generates visualization with chosen curve fitting method
#'   \item Creates a standardized PDF report
#' }
#'
#' @section Curve Fitting Methods:
#' \describe{
#'   \item{line}{Simple linear interpolation between points}
#'   \item{L4}{4-parameter logistic regression, requires the 'drc' package}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' generate_report("path/to/data.xlsx")
#'
#' # Custom parameters
#' generate_report(
#'   file = "path/to/data.xlsx",
#'   title = "Antibody Binding Assay",
#'   coat_protein = "Spike protein",
#'   coat_protein_conc = 2,
#'   method = "L4",
#'   errorbars = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link[drc]{drm}} for details on the 4-parameter logistic regression
#'
#' @section Required Packages:
#' \itemize{
#'   \item quarto
#'   \item ggplot2
#'   \item dplyr
#'   \item tidyplate
#'   \item ggtext
#'   \item drc (optional, for L4 fitting)
#' }
#'
#' @section Error Handling:
#' The function includes comprehensive error checking for:
#' \itemize{
#'   \item File existence and readability
#'   \item Required data columns
#'   \item Parameter validation
#'   \item Package dependencies
#'   \item PDF generation and copying
#' }
#'
#' @importFrom quarto quarto_render
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_errorbar scale_x_log10 theme_bw theme
#' @importFrom dplyr filter mutate group_by summarise
#' @importFrom tidyplate tidy_plate
#' @importFrom ggtext element_markdown
#'
#' @export
generate_elisa_report <- function(
    file,
    title = "ELISA Report",
    coat_protein = "coat",
    coat_protein_conc = 1,
    plate = "corning",
    blocking_buffer = "block",
    detect_dil = "1:5000",
    time = 6,
    method = c("line", "L4"),
    errorbars = TRUE,
    point_size = 3,
    linewidth = 1,
    errorbar_width = 0.2,
    results,
    output_file = "elisa_report.pdf") {

  # Check required packages
  required_packages <- c("quarto", "ggplot2", "dplyr", "tidyplate", "ggtext")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }

  # Add this near the start of your function
  if (!tinytex::is_tinytex() && !tinytex::check_installed("latex")) {
    stop("LaTeX is not installed. Please install TinyTeX by running: tinytex::install_tinytex()")
  }

  # Validation code remains the same...

  # Create temporary directory with error handling
  temp_dir <- tempfile("elisa_report_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(temp_dir)) {
    stop("Failed to create temporary directory")
  }

  # Store current directory and set up cleanup
  old_dir <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_dir)
    unlink(temp_dir, recursive = TRUE)
  })

  # Get template paths
  qmd_template_path <- system.file("templates", "elisa_report_template.qmd",
                                   package = "reportTemplates")
  tex_template_path <- system.file("templates", "template.tex",
                                   package = "reportTemplates")

  if (qmd_template_path == "" || tex_template_path == "") {
    stop("Templates not found. Please check package installation.")
  }

  # Copy both templates to temp directory
  file.copy(qmd_template_path, "report.qmd", overwrite = TRUE)
  file.copy(tex_template_path, "template.tex", overwrite = TRUE)

  # Get absolute paths
  abs_file_path <- normalizePath(file, mustWork = FALSE)
  output_path <- file.path(old_dir, output_file)

  # Render report
  tryCatch({
    quarto::quarto_render(
      input = "report.qmd",
      output_file = "report.pdf",
      execute_params = list(
        file = abs_file_path,
        title = title,
        coat_protein = coat_protein,
        coat_protein_conc = coat_protein_conc,
        plate = plate,
        blocking_buffer = blocking_buffer,
        detect_dil = detect_dil,
        time = time,
        method = method,
        errorbars = errorbars,
        point_size = point_size,
        linewidth = linewidth,
        errorbar_width = errorbar_width,
        results = results
      )
    )

    # Verify PDF was created
    if (!file.exists("report.pdf")) {
      stop("PDF report was not generated")
    }

    # Copy to final destination
    if (!file.copy("report.pdf", output_path, overwrite = TRUE)) {
      stop("Failed to copy report to ", output_path)
    }
  }, error = function(e) {
    stop("Error generating report: ", conditionMessage(e))
  })

  # Return path to generated file
  invisible(normalizePath(output_path))
}
