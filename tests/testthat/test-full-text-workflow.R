withr::local_dir(here::here())
source(file.path("R", "full_text_workflow.R"))

pilot_path <- file.path("data", "2.full_abstracting", "2026-10_full_text_decisions.xlsx")
pilot <- ft_read_decision_workbook(pilot_path)
pilot$decisions$workflow_version <- ft_workflow_version
for (key in c("issue_summary", "ai_recommendation", "decision_options", "provenance_note")) {
  if (!key %in% names(pilot$decisions)) pilot$decisions[[key]] <- ""
}
objects <- lapply(seq_len(nrow(pilot$decisions)), function(i) ft_row_to_object(pilot$decisions[i, , drop = FALSE]))
manifest <- pilot$decisions[, c("screening_record_id", "paper_id", "doi", "title", "pdf_path")]

write_objects <- function(objects, path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (obj in objects) jsonlite::write_json(obj, file.path(path, paste0(obj$paper_id, ".json")), auto_unbox = TRUE, pretty = TRUE, null = "null")
  ft_read_ai_decisions(path)
}

test_that("typed schema rejects incomplete, mistyped and contradictory artifacts", {
  obj <- objects[[1]]
  expect_silent(ft_validate_decision_object(obj))
  invalid <- obj; invalid$review_rationale <- NULL
  expect_error(ft_validate_decision_object(invalid), "required")
  invalid <- obj; invalid$review_eligible <- "true"
  expect_error(ft_validate_decision_object(invalid), "wrong type")
  invalid <- obj; invalid$extra_field <- "ignored?"
  expect_error(ft_validate_decision_object(invalid), "unexpected")
  invalid <- obj; invalid$review_evidence <- list(list(evidence = "test", page = 0))
  expect_error(ft_validate_decision_object(invalid), "minimum")
  invalid <- obj; invalid$review_evidence <- list()
  expect_error(ft_validate_decision_object(invalid), "few items")
  invalid <- obj; invalid$screening_date <- "2026-02-30"
  expect_error(ft_validate_decision_object(invalid), "invalid date")
  invalid <- obj; invalid$meta_exclusion_reason <- "LANGUAGE"
  expect_error(ft_validate_decision_object(invalid), "enum")
  invalid <- obj; invalid$final_route <- "both"
  expect_error(ft_validate_decision_object(invalid), "const")
  schema <- list(type = "string", unsupportedAssertion = TRUE)
  expect_error(ft_schema_errors("test", schema), "Unsupported JSON Schema keyword")
  invalid <- obj; invalid$framework_relation_tags <- list("TESTS_SEB_FRAMEWORK", "TESTS_SEB_FRAMEWORK")
  expect_error(ft_validate_decision_object(invalid), "duplicate")
})

test_that("every artifact must match a unique manifest identity and filename", {
  path <- tempfile(); ai <- write_objects(objects, path)
  expect_silent(ft_validate_ai_decisions(ai, manifest))
  expect_error(ft_validate_ai_decisions(ai[-1, ], manifest), "cover every")
  expect_error(ft_validate_ai_decisions(rbind(ai[1, ], ai[-2, ]), manifest), "duplicate")
  bad <- ai; bad$screening_record_id[[1]] <- "seb_aaaaaaaaaaaaaaaa"
  expect_error(ft_validate_ai_decisions(bad, manifest), "does not match")
  file.rename(file.path(path, "0144.json"), file.path(path, "9999.json"))
  expect_error(ft_read_ai_decisions(path), "filename")
})

test_that("pending and uncertain remain unresolved with distinct operational states", {
  obj <- objects[[2]]
  obj$meta_status <- "pending"; obj$meta_pending_reason <- "AUTHOR_CONTACT_NEEDED"
  obj["final_route"] <- list(NULL); obj$current_route <- "review_meta_pending"
  obj$eligible_quantitative_components <- list()
  expect_silent(ft_validate_decision_object(obj))
  ai <- write_objects(list(obj), tempfile())
  decisions <- ft_merge_manifest_decisions(manifest[2, ], ai, "test")
  expect_identical(decisions$final_route, "")
  expect_identical(decisions$workflow_status, "PENDING_DATA")
  expect_error(ft_legacy_routing(decisions), "resolved")
  obj$meta_status <- "uncertain"; obj["meta_pending_reason"] <- list(NULL)
  obj$current_route <- "review_meta_uncertain"; obj$requires_human_review <- TRUE
  obj$human_review_reasons <- list("AMBIGUOUS_BESSI_MEASURE")
  obj["human_review_summary"] <- list(NULL)
  obj["ai_recommendation"] <- list(NULL)
  obj$decision_options <- list()
  expect_error(ft_validate_decision_object(obj), "wrong type|few")
  obj$human_review_summary <- "Is the shortened form sufficiently valid?"
  obj$ai_recommendation <- "Review the validation evidence before including."
  obj$decision_options <- list("include", "exclude")
  expect_silent(ft_validate_decision_object(obj))
  ai <- write_objects(list(obj), tempfile())
  decisions <- ft_merge_manifest_decisions(manifest[2, ], ai, "test")
  queue <- ft_build_human_review_queue(decisions, ai)
  expect_equal(nrow(queue), 1L)
  expect_identical(decisions$workflow_status, "HUMAN_REVIEW_REQUIRED")
  expect_error(ft_legacy_routing(decisions), "resolved")
  obj$final_route <- "both"
  expect_error(ft_validate_decision_object(obj), "const")
})

test_that("upstream IDs, DOI placeholders and padded IDs remain safe", {
  expect_identical(ft_normalise_doi(c("NA", "N/A", "none", "null", "-", "doi:NA", "https://doi.org/10.1000/ABC")), c(rep("", 6), "10.1000/abc"))
  expect_identical(ft_format_paper_id(c("83", "10000")), c("0083", "10000"))
  expect_error(ft_format_paper_id("1.5"), "numeric")
  expect_error(ft_format_paper_id("-1"), "numeric")
  expect_error(ft_bool(NA), "TRUE/FALSE")
  path <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(screening_record_id = "seb_aaaaaaaaaaaaaaaa", title = "Updated title", doi = "10.1000/updated"), path)
  current <- ft_prepare_current_records(path, "2026-11")
  expect_identical(current$screening_record_id, "seb_aaaaaaaaaaaaaaaa")
})

test_that("review uncertainty blocks routing independently of quantitative ineligibility", {
  obj <- objects[[5]]
  obj$requires_human_review <- TRUE
  obj$human_review_reasons <- list("FRAMEWORK_ADOPTION_UNCLEAR")
  obj["final_route"] <- list(NULL)
  obj$current_route <- "human_review_required"
  obj$human_review_summary <- "Is SEB a substantive object of this conceptual paper?"
  obj$ai_recommendation <- "Retain provisionally; adjudicate the framework mapping."
  obj$decision_options <- list("review eligible", "review ineligible")
  expect_silent(ft_validate_decision_object(obj))
  ai <- write_objects(list(obj), tempfile())
  decisions <- ft_merge_manifest_decisions(manifest[5, ], ai, "test")
  expect_identical(decisions$meta_status, "ineligible")
  expect_identical(decisions$final_route, "")
  expect_identical(decisions$workflow_status, "HUMAN_REVIEW_REQUIRED")
  expect_equal(nrow(ft_build_human_review_queue(decisions, ai)), 1L)
  expect_error(ft_legacy_routing(decisions), "resolved")
})

test_that("legacy bootstrap coerces types and preserves aliases and source files", {
  path <- tempfile(); dir.create(path)
  first <- data.frame(paper_id = "0083", title = "Legacy paper", doi = "10.1007/s12310-025-09811-9", gender = 0.5, age = 20, clinical = FALSE)
  second <- first; second$paper_id <- "0086"; second$gender <- "mixed"; second$age <- "adults"; second$clinical <- "no"
  openxlsx::write.xlsx(first, file.path(path, "old_full_decisions.xlsx"))
  openxlsx::write.xlsx(second, file.path(path, "new_full_decisions.xlsx"))
  # A monthly v2 summary sheet must never be mistaken for a legacy table.
  openxlsx::write.xlsx(data.frame(metric = "test"), file.path(path, "2026-10_full_text_decisions.xlsx"))
  files <- list.files(path, full.names = TRUE)
  before <- tools::md5sum(files)
  legacy <- ft_bootstrap_legacy_registry(path)
  expect_identical(legacy$paper_id, "0083")
  expect_true("0086" %in% attr(legacy, "reserved_paper_ids"))
  aliases <- ft_read_table(file.path(path, "paper_id_aliases.csv"))
  expect_identical(aliases$canonical_paper_id, "0083")
  expect_equal(tools::md5sum(files), before)
  empty <- aliases[0, ]
  expect_equal(nrow(ft_write_legacy_alias_report(empty, path)), 1L)
  # Persisted aliases reserve IDs even if source workbooks are later absent.
  attr(legacy, "reserved_paper_ids") <- c("0083", "9999")
  current_path <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(title = "New", doi = "10.1000/new"), current_path)
  current <- ft_prepare_current_records(current_path, "2026-11")
  assigned <- ft_assign_paper_ids(current, legacy, "2026-11", current_path)
  expect_identical(assigned$current$paper_id, "10000")
  rerun <- ft_assign_paper_ids(current, assigned$registry, "2026-11", current_path)
  expect_identical(rerun$current$paper_id, "10000")
  expect_equal(rerun$newly_assigned, 0L)
  bad <- first; bad$doi <- "10.1000/other"
  openxlsx::write.xlsx(bad, file.path(path, "conflict_full_decisions.xlsx"))
  expect_error(ft_bootstrap_legacy_registry(path), "Unsafe legacy")
  expect_true(file.exists(file.path(path, "legacy_registry_conflicts.csv")))
})

test_that("October operational decisions match frozen independent expectations", {
  expected <- ft_read_table(file.path("tests", "full_text", "2026-10_expected_full_text_decisions.csv"))
  actual <- pilot$decisions[match(expected$paper_id, pilot$decisions$paper_id), ]
  for (key in names(expected)) {
    lhs <- tolower(ft_blank_to_empty(actual[[key]])); rhs <- tolower(ft_blank_to_empty(expected[[key]]))
    expect_identical(lhs, rhs, info = key)
  }
  expect_identical(actual$eligible_quantitative_components[actual$paper_id == "0145"], "Study 2")
  expect_false(any(c("sample_id", "matrix_id") %in% names(actual)))
  expect_identical(as.integer(table(actual$final_route)[c("review", "both", "exclude")]), c(2L, 1L, 3L))
  expect_equal(nrow(pilot$human_review), 3L)
  expect_true(all(pilot$human_review$adjudication_status == "RESOLVED"))
})

test_that("rebuilds preserve adjudications and reject unfinalized workbook edits", {
  path <- tempfile(fileext = "_full_text_decisions.xlsx")
  ai <- write_objects(objects, tempfile())
  decisions <- ft_merge_manifest_decisions(manifest, ai, "2026-10")
  result <- ft_apply_adjudications(decisions, pilot$human_review)
  ft_write_monthly_outputs(result$decisions, result$human_review, path, "2026-10")
  rebuilt <- ft_rebuild_decisions(manifest, ai, "2026-10", path)
  expect_equal(rebuilt$decisions, result$decisions)
  expect_equal(rebuilt$human_review, result$human_review)
  invalid <- pilot$human_review; invalid$screening_record_id[[1]] <- "seb_aaaaaaaaaaaaaaaa"
  expect_error(ft_apply_adjudications(decisions, invalid), "identity mismatch")
  invalid <- rbind(pilot$human_review, pilot$human_review[1, ])
  expect_error(ft_apply_adjudications(decisions, invalid), "Duplicate")
  invalid <- pilot$human_review; invalid$adjudication_note[[1]] <- ""
  expect_error(ft_apply_adjudications(decisions, invalid), "note")
  invalid <- pilot$human_review; invalid$human_meta_exclusion_reason[[1]] <- "made_up"
  expect_error(ft_apply_adjudications(decisions, invalid), "enum|non-skill")
  invalid <- pilot$human_review; invalid$adjudication_note[[1]] <- "Unsaved change"
  ft_write_decision_workbook(result$decisions, invalid, path, "2026-10")
  expect_error(ft_rebuild_decisions(manifest, ai, "2026-10", path), "Unfinalized")
  before <- tools::md5sum(path)
  expect_error(ft_rebuild_decisions(manifest, ai[-1, ], "2026-10", path), "cover every")
  expect_identical(tools::md5sum(path), before)
  expect_error(ft_validate_queue_edits(pilot$human_review, pilot$human_review[-1, ]), "removed/edited")
  invalid <- pilot$human_review; invalid$meta_evidence[[1]] <- "Altered evidence"
  expect_error(ft_validate_queue_edits(pilot$human_review, invalid), "removed/edited")
  edited <- pilot$human_review; edited$adjudication_note[[1]] <- "Clarified note"
  expect_silent(ft_validate_queue_edits(pilot$human_review, edited))
})

test_that("configuration and typed schema share controlled values and versions", {
  config <- yaml::read_yaml(file.path("config", "screening", "full_text_eligibility.yml"))
  schema <- jsonlite::fromJSON(file.path("schemas", "full_text_eligibility_decision.schema.json"), simplifyVector = FALSE)
  expect_identical(config$workflow_version, ft_workflow_version)
  expect_identical(config$policy_version, schema$properties$eligibility_policy_version$const)
  for (entry in list(c("meta_status", "meta", "statuses"),
                     c("meta_exclusion_reason", "meta", "exclusion_reasons"),
                     c("meta_pending_reason", "meta", "pending_reasons"),
                     c("review_exclusion_reason", "review", "exclusion_reasons"))) {
    expect_setequal(unlist(schema$properties[[entry[[1]]]]$enum), config[[entry[[2]]]][[entry[[3]]]])
  }
  expect_setequal(unlist(schema$properties$framework_relation_tags$items$enum), config$review$framework_relation_tags)
})

test_that("exported XLSX packages contain all internal relationship targets", {
  path <- tempfile(fileext = ".xlsx")
  ai <- write_objects(objects, tempfile())
  decisions <- ft_merge_manifest_decisions(manifest, ai, "2026-10")
  ft_write_decision_workbook(decisions, pilot$human_review, path, "2026-10")
  contents <- utils::unzip(path, list = TRUE)$Name
  expect_true("xl/sharedStrings.xml" %in% contents)
  for (rel in contents[grepl("\\.rels$", contents)]) {
    con <- unz(path, rel); xml <- paste(readLines(con, warn = FALSE), collapse = ""); close(con)
    doc <- xml2::read_xml(xml)
    for (node in xml2::xml_find_all(doc, "//*[local-name()='Relationship']")) {
      if (identical(xml2::xml_attr(node, "TargetMode"), "External")) next
      target <- xml2::xml_attr(node, "Target")
      directory <- if (rel == "_rels/.rels") "" else dirname(dirname(rel))
      pieces <- strsplit(paste(directory, target, sep = "/"), "/", fixed = TRUE)[[1]]
      resolved <- character()
      for (piece in pieces) {
        if (piece == "..") resolved <- head(resolved, -1L)
        else if (nzchar(piece) && piece != ".") resolved <- c(resolved, piece)
      }
      expect_true(paste(resolved, collapse = "/") %in% contents, info = paste(rel, target))
    }
  }
  roundtrip <- ft_read_decision_workbook(path)
  expect_identical(roundtrip$decisions$paper_id, decisions$paper_id)
})

test_that("legacy routing preserves established identity without assigning samples", {
  ai <- write_objects(objects, tempfile())
  decisions <- ft_merge_manifest_decisions(manifest, ai, "2026-10")
  routing <- ft_legacy_routing(decisions)
  expect_identical(routing$paper_id, manifest$paper_id)
  expect_identical(routing$final_decision, decisions$final_route)
  expect_false(any(c("sample_id", "matrix_id") %in% names(routing)))
})

test_that("existing-workbook import preserves evidence and cannot overwrite inputs", {
  path <- tempfile(fileext = "_full_text_decisions.xlsx")
  expect_true(file.copy(pilot_path, path))
  folder <- tempfile()
  ft_import_existing_workbook(path, folder, "2026-10")
  fresh <- ft_read_decision_workbook(path)
  for (key in c("screening_record_id", "paper_id", "review_evidence", "review_evidence_pages",
                "meta_evidence", "meta_evidence_pages", "screening_date", "decision_source")) {
    expect_identical(fresh$decisions[[key]], pilot$decisions[[key]], info = key)
  }
  expect_equal(fresh$human_review, pilot$human_review)
  expect_true(all(grepl("not re-screened", fresh$decisions$provenance_note, fixed = TRUE)))
  expect_true(all(nchar(fresh$decisions$decision_sha256) == 64L))
  expect_error(ft_import_existing_workbook(path, folder, "2026-10"), "refusing overwrite")
})

test_that("frozen October input identities match the operational manifest", {
  expected <- ft_read_table(file.path("tests", "full_text", "2026-10_expected_records.csv"))
  actual <- ft_read_table(file.path("data", "2.full_abstracting", "2026-10_full_text_manifest.csv"))
  expect_setequal(actual$screening_record_id, expected$screening_record_id)
  actual <- actual[match(expected$screening_record_id, actual$screening_record_id), ]
  expect_identical(ft_normalise_doi(actual$doi), ft_normalise_doi(expected$doi))
  expect_identical(actual$title, expected$title)
  registry <- ft_read_table(file.path("data", "2.full_abstracting", "paper_id_registry.csv"))
  expect_identical(actual$paper_id, registry$paper_id[match(actual$screening_record_id, registry$screening_record_id)])
  aliases <- ft_read_table(file.path("data", "2.full_abstracting", "paper_id_aliases.csv"))
  expect_identical(aliases$canonical_paper_id[aliases$alias_paper_id == "0086"], "0083")
})
