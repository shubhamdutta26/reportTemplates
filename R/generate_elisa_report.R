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
    results = "",
    output_file = "elisa_report.pdf") {

  # Input validation
  if (!file.exists(file)) {
    stop("Input file does not exist: ", file)
  }

  validate_parameters(method, errorbars, point_size, linewidth, errorbar_width)

  # Check dependencies
  check_dependencies()

  # Create temporary working directory
  temp_dir <- create_temp_directory()
  old_dir <- setwd(temp_dir)
  on.exit({
    setwd(old_dir)
    unlink(temp_dir, recursive = TRUE)
  })

  # Copy and prepare templates
  copy_templates()

  # Generate report
  generate_pdf(
    file = normalizePath(file, mustWork = TRUE),
    params = list(
      title = title,
      coat_protein = coat_protein,
      coat_protein_conc = coat_protein_conc,
      plate = plate,
      blocking_buffer = blocking_buffer,
      detect_dil = detect_dil,
      time = time,
      method = match.arg(method),
      errorbars = errorbars,
      point_size = point_size,
      linewidth = linewidth,
      errorbar_width = errorbar_width,
      results = results
    ),
    output_file = file.path(old_dir, output_file)
  )
}

#' Check Required Dependencies
#' @keywords internal
check_dependencies <- function() {
  required_packages <- c(
    "quarto", "ggplot2", "dplyr", "tidyplate",
    "ggtext", "tinytex"
  )

  # Check packages efficiently using vapply
  missing_packages <- vapply(required_packages, function(pkg) {
    !requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  if (any(missing_packages)) {
    stop(
      "Missing required packages: ",
      paste(names(missing_packages)[missing_packages], collapse = ", ")
    )
  }

  # Check LaTeX installation
  if (!tinytex::is_tinytex() && !tinytex::check_installed("latex")) {
    stop(
      "LaTeX is not installed. Please install TinyTeX:\n",
      "tinytex::install_tinytex()"
    )
  }
}

#' Create Temporary Working Directory
#' @return Path to temporary directory
#' @keywords internal
create_temp_directory <- function() {
  temp_dir <- tempfile("elisa_report_")
  if (!dir.create(temp_dir, recursive = TRUE)) {
    stop("Failed to create temporary directory")
  }
  temp_dir
}

#' Copy Template Files
#' @keywords internal
copy_templates <- function() {
  templates <- c(
    qmd = system.file("templates", "elisa_report_template.qmd",
                      package = "reportTemplates"),
    tex = system.file("templates", "template.tex",
                      package = "reportTemplates")
  )

  if (any(templates == "")) {
    stop("Template files not found. Check package installation.")
  }

  # Copy templates to working directory
  file.copy(templates["qmd"], "report.qmd", overwrite = TRUE)
  file.copy(templates["tex"], "template.tex", overwrite = TRUE)
}

#' Generate PDF Report
#' @param file Normalized path to input file
#' @param params List of report parameters
#' @param output_file Final output file path
#' @return Invisible path to generated PDF
#' @keywords internal
generate_pdf <- function(file, params, output_file) {
  tryCatch({
    quarto::quarto_render(
      input = "report.qmd",
      output_file = "report.pdf",
      execute_params = c(list(file = file), params)
    )

    if (!file.exists("report.pdf")) {
      stop("PDF generation failed")
    }

    if (!file.copy("report.pdf", output_file, overwrite = TRUE)) {
      stop("Failed to copy report to destination")
    }

    invisible(normalizePath(output_file))
  }, error = function(e) {
    stop("Error generating report: ", conditionMessage(e))
  })
}

#' Validate Input Parameters
#' @keywords internal
validate_parameters <- function(method, errorbars, point_size,
                                linewidth, errorbar_width) {
  if (!is.logical(errorbars)) {
    stop("errorbars must be logical (TRUE/FALSE)")
  }

  numeric_params <- list(
    point_size = point_size,
    linewidth = linewidth,
    errorbar_width = errorbar_width
  )

  lapply(names(numeric_params), function(param) {
    value <- numeric_params[[param]]
    if (!is.numeric(value) || value <= 0) {
      stop(param, " must be a positive number")
    }
  })

  if (!method[1] %in% c("line", "L4")) {
    stop('method must be either "line" or "L4"')
  }
}
