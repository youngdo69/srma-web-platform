# WEB-014 Download Center / release packaging. Packaging never changes scientific results.
web_c4_ruleset <- function() "web-download-center-v1"

web_c4_safe_component <- function(x) {
  x <- gsub("[^A-Za-z0-9._ -]+", "_", basename(x))
  x <- gsub("[[:space:]]+", "_", trimws(x))
  x <- gsub("_+", "_", x)
  x <- sub("^[.]+", "", x)
  web_assert(nzchar(x) && !x %in% c(".", ".."), "A release filename is unsafe.")
  substr(x, 1L, 180L)
}

web_c4_item_id <- function(source_kind, source_id, checksum, package_path) {
  substr(digest::digest(paste(source_kind, source_id, checksum, package_path, sep = "|"), algo = "sha256", serialize = FALSE), 1L, 32L)
}

web_c4_package_path <- function(category, logical_name, checksum, used) {
  name <- web_c4_safe_component(logical_name)
  candidate <- paste0(category, "/", name)
  if (!candidate %in% used) return(candidate)
  ext <- tools::file_ext(name)
  stem <- if (nzchar(ext)) substr(name, 1L, nchar(name) - nchar(ext) - 1L) else name
  suffix <- substr(checksum, 1L, 8L)
  i <- 1L
  repeat {
    tail <- if (i == 1L) suffix else paste0(suffix, "-", i)
    alt <- paste0(stem, "-", tail, if (nzchar(ext)) paste0(".", ext) else "")
    candidate <- paste0(category, "/", alt)
    if (!candidate %in% used) return(candidate)
    i <- i + 1L
    web_assert(i < 1000L, "Too many release filenames collide after normalization.")
  }
}

web_c4_supplemental_paths <- function(manifest) {
  paths <- names(manifest)
  ext <- tolower(tools::file_ext(paths))
  safe_ext <- ext %in% c("csv", "tsv", "xlsx", "json", "txt", "md", "ris")
  selected <- startsWith(paths, "search_strategies/") |
    grepl("(prisma|screening|included|exclusion|extraction|risk[_ -]?of[_ -]?bias|rob[_ -]|analysis[_ -]?ready|crosswalk|study[_ -]?character|database.*preflight)", paths, ignore.case = TRUE)
  paths[safe_ext & selected & !grepl("(^|/)full[_ -]?text|[.]pdf$", paths, ignore.case = TRUE)]
}

web_c4_qc_source <- function(con, settings, actor, project_id, qc_id) {
  web_authorize(con, actor, project_id)
  row <- web_query(con, "SELECT * FROM qc_versions WHERE id=?q AND project_id=?p AND status='PASSED'", q = qc_id, p = project_id)
  web_assert(nrow(row) == 1L, "Choose a Human-approved Final QC decision.")
  x <- web_parse(row$packet_json)
  web_assert(web_c2_hash(x) == row$packet_sha256 && identical(x$id, qc_id), "Final QC evidence failed integrity verification.")
  web_assert(!is.null(x$result) && isTRUE(x$result$machine_pass) && !is.null(x$proof), "Final QC evidence is incomplete.")
  state <- web_c3_view(con, settings, actor, project_id, qc_id)
  list(row = row, packet = x, state = state)
}

web_c4_artifact_entry <- function(con, settings, actor, project_id, id, expected_sha, category, package_name = NULL, analysis_version = NULL) {
  a <- web_query(con, "SELECT * FROM artifacts WHERE id=?i AND project_id=?p AND status='AVAILABLE'", i = id, p = project_id)
  web_assert(nrow(a) == 1L && identical(a$checksum, expected_sha), "A release artifact is missing or changed.", "ARTIFACT_INTEGRITY_FAILED")
  path <- web_store(settings)$get(a$storage_locator, a$checksum)
  logical <- package_name %||b2% a$logical_name
  list(source_kind = "ARTIFACT", source_id = a$id, source_version = a$version_id,
       source_job = a$job_id, engine_id = a$engine_id, logical_name = logical,
       artifact_type = a$artifact_type, category = category, sha256 = a$checksum,
       size = unname(file.info(path)$size), analysis_version = analysis_version, storage_path = path)
}

web_c4_catalog_internal <- function(con, settings, actor, project_id, qc_id) {
  q <- web_c4_qc_source(con, settings, actor, project_id, qc_id)
  x <- q$packet; proof <- x$proof
  used <- character(); entries <- list()
  add <- function(item) {
    item$package_path <- web_c4_package_path(item$category, item$logical_name, item$sha256, used)
    used <<- c(used, item$package_path)
    item$item_id <- web_c4_item_id(item$source_kind, item$source_id, item$sha256, item$package_path)
    entries[[length(entries) + 1L]] <<- item
  }

  # Exact approved manuscript and C1 artifacts carried into the manuscript/QC proof.
  for (a in proof$artifacts) {
    category <- if (identical(a$type, "docx")) "manuscript" else if (identical(a$type, "figure")) "figures" else if (identical(a$type, "table")) "tables" else "supplementary"
    ar <- web_query(con, "SELECT logical_name FROM artifacts WHERE id=?i AND project_id=?p", i = a$id, p = project_id)
    web_assert(nrow(ar) == 1L, "A release artifact registry entry is unavailable.")
    add(web_c4_artifact_entry(con, settings, actor, project_id, a$id, a$sha256, category,
      package_name = if (category == "manuscript") "manuscript.docx" else ar$logical_name,
      analysis_version = a$analysis_version %||b2% NULL))
  }

  # Exact reviewed manuscript source/citation outputs, omitting duplicate rendered figure PNGs.
  for (name in names(proof$outputs)) if (name %in% c("manuscript.md", "citation_map.json")) {
    a <- proof$outputs[[name]]
    add(web_c4_artifact_entry(con, settings, actor, project_id, a$id, a$sha256, "supplementary", package_name = name))
  }

  # Final QC evidence itself is a registered immutable artifact.
  add(web_c4_artifact_entry(con, settings, actor, project_id, x$result_artifact, x$result_sha256, "qc", package_name = "final_qc.json"))

  # Immutable evidence snapshot supplements are selected only by a fixed whitelist.
  ev <- web_version_root(con, settings, actor, project_id, proof$evidence_version)
  for (p in sort(web_c4_supplemental_paths(ev$manifest))) {
    source <- web_inside(file.path(ev$root, p), ev$root)
    sha <- ev$manifest[[p]]
    web_assert(identical(web_sha(source), sha), "A supplementary evidence file changed.", "ARTIFACT_INTEGRITY_FAILED")
    logical <- paste0(gsub("/", "__", p, fixed = TRUE))
    item <- list(source_kind = "PROJECT_VERSION", source_id = paste0(proof$evidence_version, ":", p),
      source_version = proof$evidence_version, source_job = NA_character_, engine_id = x$engine,
      logical_name = logical, artifact_type = tools::file_ext(p), category = "supplementary",
      sha256 = sha, size = unname(file.info(source)$size), analysis_version = NULL, storage_path = source,
      source_relative_path = p)
    add(item)
  }

  entries <- entries[order(vapply(entries, `[[`, character(1), "package_path"))]
  # Public catalog never exposes object-store or filesystem locations.
  public <- lapply(entries, function(e) e[setdiff(names(e), "storage_path")])
  project <- web_authorize(con, actor, project_id)
  list(project = list(id = project$id, canonical_id = project$canonical_id, title = project$title),
       qc_id = qc_id, manuscript_version = proof$manuscript_version, evidence_version = proof$evidence_version,
       analysis_versions = names(proof$analyses), ruleset = web_c4_ruleset(),
       effective_qc_status = q$state$effective_status, current = isTRUE(q$state$eligibility$CURRENT),
       release_eligible = isTRUE(q$state$eligibility$RELEASE_ELIGIBLE), external = q$state$external,
       entries = public, .private_entries = entries)
}

web_c4_catalog <- function(con, settings, actor, project_id, qc_id) {
  x <- web_c4_catalog_internal(con, settings, actor, project_id, qc_id)
  x$.private_entries <- NULL
  x
}

web_c4_qc_list <- function(con, actor, project_id) {
  web_authorize(con, actor, project_id)
  web_query(con, "SELECT id,version_id,status,created_at FROM qc_versions WHERE project_id=?p AND status='PASSED' ORDER BY created_at DESC", p = project_id)
}

web_c4_manifest <- function(catalog) {
  entries <- lapply(catalog$entries, function(e) e[setdiff(names(e), c("item_id"))])
  list(schema = "srma-release-manifest-v1", ruleset = web_c4_ruleset(), project = catalog$project,
       lineage = list(qc_id = catalog$qc_id, manuscript_version = catalog$manuscript_version,
                      evidence_version = catalog$evidence_version, analysis_versions = as.list(catalog$analysis_versions)),
       eligibility = list(current = catalog$current, release_eligible = catalog$release_eligible, external = catalog$external),
       files = entries)
}

web_c4_download_item <- function(con, settings, actor, project_id, qc_id, item_id, destination) {
  catalog <- web_c4_catalog_internal(con, settings, actor, project_id, qc_id)
  hits <- Filter(function(e) identical(e$item_id, item_id), catalog$.private_entries)
  web_assert(length(hits) == 1L, "This Download Center item is unavailable.")
  item <- hits[[1L]]
  if (identical(item$source_kind, "ARTIFACT")) {
    web_download(con, settings, actor, project_id, item$source_id, destination)
  } else {
    web_assert(identical(web_sha(item$storage_path), item$sha256), "This supplementary file failed integrity verification.", "ARTIFACT_INTEGRITY_FAILED")
    web_assert(file.copy(item$storage_path, destination, overwrite = TRUE), "Could not prepare the download.", "SYSTEM_FAILED")
  }
  project <- web_authorize(con, actor, project_id)
  web_audit(con, project, "DOWNLOAD_CENTER_ITEM_DOWNLOADED", item_id, actor, list(qc_id = qc_id, sha256 = item$sha256, package_path = item$package_path))
  invisible(destination)
}

web_c4_write_archive <- function(entries, manifest, output, engine_root = getwd()) {
  old_root <- getOption("srma.web.engine_root")
  options(srma.web.engine_root = engine_root)
  on.exit(options(srma.web.engine_root = old_root), add = TRUE)
  web_assert(requireNamespace("zip", quietly = TRUE), "The release packaging dependency is unavailable.", "DEPENDENCY_FAILED")
  stage <- tempfile("release-stage-")
  dir.create(stage, recursive = TRUE, mode = "0700")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  for (e in entries) {
    web_no_file_secrets(e$storage_path, getOption("srma.web.engine_root", getwd()))
    target <- web_inside(file.path(stage, e$package_path), stage, exists = FALSE)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    web_assert(file.copy(e$storage_path, target, overwrite = TRUE), "Could not stage a release file.", "SYSTEM_FAILED")
    web_assert(identical(web_sha(target), e$sha256), "A staged release file failed checksum verification.", "ARTIFACT_INTEGRITY_FAILED")
  }
  dir.create(file.path(stage, "manifest"), recursive = TRUE, showWarnings = FALSE)
  manifest_path <- file.path(stage, "manifest", "release_manifest.json")
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  writeLines(web_sha(manifest_path), file.path(stage, "manifest", "release_manifest.sha256"), useBytes = TRUE)
  writeLines(c("SR-MA Final Release Package", "", "Files are selected from immutable, Human-approved project/QC lineage.",
               "See manifest/release_manifest.json for checksums and provenance.",
               "The SHA-256 of that manifest is stored in manifest/release_manifest.sha256."), file.path(stage, "README.txt"), useBytes = TRUE)
  files <- sort(list.files(stage, recursive = TRUE, all.files = FALSE, no.. = TRUE))
  # Normalize mtimes so repeated builds with identical inputs are stable where the ZIP implementation permits it.
  fixed <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  suppressWarnings(Sys.setFileTime(file.path(stage, files), fixed))
  if (file.exists(output)) unlink(output)
  zip::zipr(zipfile = output, files = files, root = stage, include_directories = FALSE)
  web_assert(file.exists(output) && file.info(output)$size > 0L, "Release package creation failed.", "SYSTEM_FAILED")
  invisible(output)
}

web_c4_build <- function(con, settings, actor, project_id, qc_id) {
  catalog <- web_c4_catalog_internal(con, settings, actor, project_id, qc_id)
  web_assert(isTRUE(catalog$current), "Final QC is historical or stale. Run and approve Final QC again before release packaging.", "STALE_UPSTREAM_VERSION")
  web_assert(isTRUE(catalog$release_eligible), paste("Release packaging is blocked until mandatory external acceptance is complete:", paste(unlist(catalog$external), collapse = "; ")), "USER_ACTION_REQUIRED")
  manifest <- web_c4_manifest(catalog)
  manifest_sha <- web_c2_hash(manifest)
  existing <- web_query(con, "SELECT id FROM release_packages WHERE project_id=?p AND qc_id=?q AND manifest_sha256=?h", p = project_id, q = qc_id, h = manifest_sha)
  if (nrow(existing)) return(list(project_id = project_id, package_id = existing$id[1L], reused = TRUE))

  package_file <- tempfile("SRMA_FINAL_PACKAGE-", fileext = ".zip")
  on.exit(unlink(package_file), add = TRUE)
  web_c4_write_archive(catalog$.private_entries, manifest, package_file, settings$engine_root)
  web_no_file_secrets(package_file, settings$engine_root)
  id <- web_id(); stored <- web_store(settings)$put(package_file, paste0(project_id, "/", id))
  project <- web_authorize(con, actor, project_id)
  web_tx(con, function() {
    # Re-check release authority at publication time; no browser-supplied PASS is accepted.
    current <- web_c4_catalog_internal(con, settings, actor, project_id, qc_id)
    web_assert(isTRUE(current$current) && isTRUE(current$release_eligible) && identical(web_c2_hash(web_c4_manifest(current)), manifest_sha), "Release eligibility or package inputs changed before publication.", "STALE_UPSTREAM_VERSION")
    web_exec(con, "INSERT INTO release_packages(id,project_id,version_id,qc_id,manifest_sha256,package_checksum,storage_locator,eligibility_json,actor_id,created_at) VALUES(?i,?p,?v,?q,?m,?c,?l,?e,?a,?t)",
      i = id, p = project_id, v = catalog$manuscript_version, q = qc_id, m = manifest_sha, c = stored$checksum,
      l = stored$locator, e = as.character(web_json(list(current = catalog$current, release_eligible = catalog$release_eligible, external = catalog$external))), a = actor, t = web_now())
    web_audit(con, project, "RELEASE_PACKAGE_CREATED", id, actor, list(qc_id = qc_id, manifest_sha256 = manifest_sha, package_checksum = stored$checksum, files = length(manifest$files)))
  })
  list(project_id = project_id, package_id = id, reused = FALSE)
}

web_c4_package_list <- function(con, settings, actor, project_id) {
  web_authorize(con, actor, project_id)
  rows <- web_query(con, "SELECT id,version_id,qc_id,manifest_sha256,package_checksum,created_at FROM release_packages WHERE project_id=?p ORDER BY created_at DESC", p = project_id)
  if (!nrow(rows)) return(rows)
  rows$current <- vapply(rows$qc_id, function(q) tryCatch(isTRUE(web_c3_view(con, settings, actor, project_id, q)$eligibility$CURRENT), error = function(e) FALSE), logical(1))
  rows
}

web_c4_download_package <- function(con, settings, actor, project_id, package_id, destination) {
  web_authorize(con, actor, project_id)
  r <- web_query(con, "SELECT * FROM release_packages WHERE id=?i AND project_id=?p", i = package_id, p = project_id)
  web_assert(nrow(r) == 1L, "This release package is unavailable.")
  path <- web_store(settings)$get(r$storage_locator, r$package_checksum)
  web_assert(file.copy(path, destination, overwrite = TRUE), "Could not prepare the package download.", "SYSTEM_FAILED")
  project <- web_authorize(con, actor, project_id)
  web_audit(con, project, "RELEASE_PACKAGE_DOWNLOADED", package_id, actor, list(checksum = r$package_checksum, qc_id = r$qc_id))
  invisible(destination)
}
