# Local JSON Schema validation for the vocabulary used by the full-text schemas.
# No network resolution or coercion. Unknown assertions fail closed, so a schema
# extension cannot silently weaken validation. JSON must be parsed with
# simplifyVector=FALSE to distinguish objects, arrays, booleans and null.
ft_schema_errors <- function(value, schema, root = schema, path = "$", check_schema = TRUE) {
  supported <- c("$schema", "$id", "title", "description", "$defs", "$ref",
                 "type", "required", "additionalProperties", "properties", "enum",
                 "const", "pattern", "format", "minLength", "minimum", "items",
                 "uniqueItems", "minItems", "maxItems", "allOf", "if", "then", "else")
  if (check_schema) {
    check <- function(s) {
      unknown <- setdiff(names(s), supported)
      if (length(unknown)) stop("Unsupported JSON Schema keyword: ", paste(unknown, collapse = ", "), call. = FALSE)
      for (key in c("properties", "$defs")) for (child in s[[key]]) check(child)
      for (key in c("items", "if", "then", "else")) if (!is.null(s[[key]])) check(s[[key]])
      for (child in s$allOf) check(child)
    }
    check(schema)
  }
  errors <- character()
  fail <- function(message) errors <<- c(errors, paste0(path, ": ", message))
  recurse <- function(v, s, p = path) ft_schema_errors(v, s, root, p, FALSE)
  if (!is.null(schema$`$ref`)) {
    ref <- schema$`$ref`
    if (!startsWith(ref, "#/$defs/")) stop("Only local $defs references are supported.", call. = FALSE)
    target <- root$`$defs`[[substring(ref, 9L)]]
    if (is.null(target)) stop("Unknown JSON Schema reference: ", ref, call. = FALSE)
    errors <- c(errors, recurse(value, target))
  }
  object <- is.list(value) && !is.null(names(value))
  array <- is.list(value) && is.null(names(value))
  scalar <- length(value) == 1L && !is.list(value) && !is.na(value)
  is_type <- function(type) switch(type,
    null = is.null(value), object = object, array = array,
    string = scalar && is.character(value), boolean = scalar && is.logical(value),
    number = scalar && is.numeric(value) && is.finite(value),
    integer = scalar && is.numeric(value) && is.finite(value) && value == floor(value),
    stop("Unsupported schema type: ", type, call. = FALSE))
  if (!is.null(schema$type) && !any(vapply(unlist(schema$type), is_type, logical(1)))) fail("wrong type")
  equal <- function(a, b) identical(a, b) || (is.numeric(a) && is.numeric(b) && isTRUE(all.equal(a, b)))
  if ("const" %in% names(schema) && !equal(value, schema$const)) fail("does not match const")
  if (!is.null(schema$enum) && !any(vapply(schema$enum, function(x) equal(value, x), logical(1)))) fail("value outside enum")
  if (object) {
    missing <- setdiff(unlist(schema$required), names(value))
    if (length(missing)) fail(paste("missing required fields", paste(missing, collapse = ", ")))
    if (identical(schema$additionalProperties, FALSE)) {
      extra <- setdiff(names(value), names(schema$properties))
      if (length(extra)) fail(paste("unexpected fields", paste(extra, collapse = ", ")))
    }
    for (key in intersect(names(schema$properties), names(value))) {
      errors <- c(errors, recurse(value[[key]], schema$properties[[key]], paste0(path, ".", key)))
    }
  }
  if (array) {
    if (!is.null(schema$minItems) && length(value) < schema$minItems) fail("too few items")
    if (!is.null(schema$maxItems) && length(value) > schema$maxItems) fail("too many items")
    if (isTRUE(schema$uniqueItems) && anyDuplicated(value)) fail("duplicate array items")
    if (!is.null(schema$items)) for (i in seq_along(value)) {
      errors <- c(errors, recurse(value[[i]], schema$items, paste0(path, "[", i, "]")))
    }
  }
  if (scalar && is.character(value)) {
    if (!is.null(schema$minLength) && nchar(trimws(value)) < schema$minLength) fail("string too short")
    if (!is.null(schema$pattern) && !grepl(schema$pattern, value, perl = TRUE)) fail("pattern mismatch")
    if (!is.null(schema$format)) {
      if (schema$format != "date") stop("Unsupported schema format.", call. = FALSE)
      parsed <- suppressWarnings(tryCatch(as.Date(value, format = "%Y-%m-%d"), error = function(e) NA))
      if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value) || is.na(parsed) || format(parsed, "%Y-%m-%d") != value) fail("invalid date")
    }
  }
  if (scalar && is.numeric(value) && !is.null(schema$minimum) && value < schema$minimum) fail("below minimum")
  for (s in schema$allOf) errors <- c(errors, recurse(value, s))
  if (!is.null(schema$`if`)) {
    branch <- if (!length(recurse(value, schema$`if`))) schema$then else schema$`else`
    if (!is.null(branch)) errors <- c(errors, recurse(value, branch))
  }
  unique(errors)
}

ft_validate_decision_object <- function(obj, schema_path = file.path("schemas", "full_text_eligibility_decision.schema.json")) {
  schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  errors <- ft_schema_errors(obj, schema)
  if (length(errors)) stop("Invalid decision JSON: ", paste(errors, collapse = "; "), call. = FALSE)
  invisible(obj)
}
