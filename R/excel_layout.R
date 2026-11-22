# Shared presentation rule for generated Excel interfaces; cell data are untouched.
excel_compact_file <- function(path, max_width = 40) {
  stopifnot(file.exists(path), length(max_width) == 1L, is.finite(max_width), max_width > 0)
  scratch <- tempfile('excel_layout_'); dir.create(scratch)
  stopifnot(startsWith(normalizePath(scratch, winslash = '/'),
    paste0(normalizePath(tempdir(), winslash = '/'), '/')))
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = scratch)
  paths <- c(file.path(scratch, 'xl/styles.xml'),
    list.files(file.path(scratch, 'xl/worksheets'), pattern = '^sheet[0-9]+\\.xml$', full.names = TRUE))
  changed <- FALSE
  for (part in paths[file.exists(paths)]) {
    doc <- xml2::read_xml(part); dirty <- FALSE
    for (node in xml2::xml_find_all(doc, "//*[local-name()='alignment'][@wrapText]")) {
      if (xml2::xml_attr(node, 'wrapText') != '0') {
        xml2::xml_set_attr(node, 'wrapText', '0'); dirty <- TRUE
      }
    }
    for (node in xml2::xml_find_all(doc, "//*[local-name()='col'][@width]")) {
      width <- suppressWarnings(as.numeric(xml2::xml_attr(node, 'width')))
      if (is.finite(width) && width > max_width) {
        xml2::xml_set_attr(node, 'width', as.character(max_width))
        xml2::xml_set_attr(node, 'bestFit', '0'); dirty <- TRUE
      }
    }
    if (dirty) { xml2::write_xml(doc, part); changed <- TRUE }
  }
  if (changed) {
    staged <- tempfile(fileext = '.xlsx'); on.exit(unlink(staged), add = TRUE)
    zip::zipr(staged, list.files(scratch, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = scratch, mode = "mirror")
    if (!file.copy(staged, path, overwrite = TRUE))
      stop('Cannot save Excel layout; close the workbook: ', path, call. = FALSE)
  }
  invisible(path)
}