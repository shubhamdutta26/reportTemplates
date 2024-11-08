.onLoad <- function(libname, pkgname) {
  # Check for quarto installation
  if (Sys.which("quarto") == "") {
    warning("Quarto installation not found. Please install Quarto from https://quarto.org")
  }
}
