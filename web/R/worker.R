web_claim <- function(con) {
  web_tx(con, function() {
    candidates <- DBI::dbGetQuery(con, "SELECT j.* FROM jobs j WHERE j.state='QUEUED' AND NOT EXISTS(SELECT 1 FROM jobs r WHERE r.project_id=j.project_id AND r.state='RUNNING') ORDER BY j.created_at LIMIT 1")
    if (!nrow(candidates)) return(NULL)
    j <- candidates[1, , drop = FALSE]
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = j$project_id)
    p <- web_query(con, "SELECT * FROM projects WHERE id=?p", p = j$project_id)
    if (web_gate_open(con, j$project_id, j$version_id, j$operation)) {
      changed <- web_exec(con, "UPDATE jobs SET state='USER_ACTION_REQUIRED',message='Human review is required before this step can continue.',updated_at=?now WHERE id=?id AND state='QUEUED'", now = web_now(), id = j$id)
      if (changed) web_audit(con, p, "JOB_USER_ACTION_REQUIRED", j$id)
      return(NULL)
    }
    token <- web_id()
    changed <- web_exec(con, "UPDATE jobs SET state='RUNNING',attempts=attempts+1,worker_token=?token,heartbeat=?time,progress=10,message='Preparing project inputs…',updated_at=?now WHERE id=?id AND state='QUEUED' AND NOT EXISTS(SELECT 1 FROM jobs r WHERE r.project_id=?p AND r.state='RUNNING')",
      token = token, time = as.numeric(Sys.time()), now = web_now(), id = j$id, p = j$project_id)
    if (changed != 1L) return(NULL)
    web_audit(con, p, "JOB_RUNNING", j$id, detail = list(worker_token = token))
    web_query(con, "SELECT * FROM jobs WHERE id=?id", id = j$id)
  })
}

web_screening_checkpoint_count <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) return(0L)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(0L)
  if (all(c("screening_id", "validation_status") %in% names(x))) {
    ok <- !is.na(x$screening_id) & nzchar(trimws(as.character(x$screening_id))) &
      as.character(x$validation_status) == "valid"
    return(length(unique(as.character(x$screening_id[ok]))))
  }
  nrow(x)
}

web_seed_prior_screening_checkpoint <- function(con, settings, j, work) {
  if (!identical(as.character(j$operation), "recommend_screening")) return(0L)
  prior <- web_query(con,
    "SELECT id,diagnostic_locator,created_at FROM jobs WHERE project_id=?p AND version_id=?v AND operation='recommend_screening' AND state IN ('FAILED','USER_ACTION_REQUIRED') AND id<>?id AND diagnostic_locator IS NOT NULL ORDER BY created_at DESC",
    p=j$project_id,v=j$version_id,id=j$id)
  if (!nrow(prior)) return(0L)
  # The newest failed attempt may contain FEWER valid decisions than an
  # earlier checkpoint. Never silently regress or mix divergent checkpoints.
  candidates <- list()
  for (i in seq_len(nrow(prior))) {
    loc <- as.character(prior$diagnostic_locator[[i]])
    if (is.na(loc) || !nzchar(loc)) next
    old_project <- file.path(settings$data_root, loc, "project")
    if (!dir.exists(old_project)) next
    hits <- list.files(old_project, pattern="^agent_screening_checkpoint[.]csv$", recursive=TRUE, full.names=TRUE)
    if (length(hits) != 1L) next
    src <- hits[[1L]]
    rows <- tryCatch(utils::read.csv(src, stringsAsFactors=FALSE, check.names=FALSE), error=function(e) NULL)
    if (is.null(rows) || !all(c("screening_id", "validation_status") %in% names(rows))) next
    ok <- !is.na(rows$screening_id) & nzchar(trimws(as.character(rows$screening_id))) &
      !is.na(rows$validation_status) & as.character(rows$validation_status) == "valid"
    ok[is.na(ok)] <- FALSE
    valid <- rows[ok, , drop=FALSE]
    ids <- as.character(valid$screening_id)
    if (!length(ids)) next
    if (anyDuplicated(ids)) stop("Prior screening checkpoint has duplicate valid screening IDs; reconcile it before retrying.")
    candidates[[length(candidates)+1L]] <- list(src=src, root=old_project, ids=ids, records=valid, source_job_id=as.character(prior$id[[i]]))
  }
  if (!length(candidates)) return(0L)
  # Competing retries may legitimately produce different AI recommendations.
  # Never merge or overwrite rows across attempts. Select ONE checkpoint only
  # when it covers every valid ID found in the prior attempts, and preserve
  # historical disagreements in the JOB_CHECKPOINT_RESUMED audit metadata.
  all_ids <- unique(unlist(lapply(candidates, function(x) x$ids), use.names=FALSE))
  covering <- which(vapply(candidates, function(x) {
    !length(setdiff(all_ids, x$ids))
  }, logical(1)))
  if (!length(covering)) {
    stop("Prior screening checkpoints contain divergent valid decisions; operator reconciliation is required before retry.")
  }
  # Original query was newest-first: a covering checkpoint at the smallest
  # index wins. It must contain every previously completed valid screening ID.
  best <- candidates[[covering[[1L]]]]
  judgment_fields <- c("decision", "exclusion_reason_code", "exclusion_reason")
  annotation_fields <- c("confidence", "rationale")
  different_judgments <- character()
  different_annotations <- character()
  norm <- function(values) {
    values <- as.character(values)
    values[is.na(values)] <- ""
    values
  }
  for (i in seq_along(candidates)) {
    if (i == covering[[1L]]) next
    other <- candidates[[i]]
    shared <- intersect(best$ids, other$ids)
    if (!length(shared)) next
    for (field in c(judgment_fields, annotation_fields)) {
      if (!field %in% names(best$records) || !field %in% names(other$records)) next
      a <- norm(best$records[[field]][match(shared, best$ids)])
      b <- norm(other$records[[field]][match(shared, other$ids)])
      changed <- shared[a != b]
      if (field %in% judgment_fields)
        different_judgments <- union(different_judgments, changed)
      else
        different_annotations <- union(different_annotations, changed)
    }
  }
  old_norm <- normalizePath(best$root, winslash="/", mustWork=TRUE)
  src_norm <- normalizePath(best$src, winslash="/", mustWork=TRUE)
  rel <- substring(src_norm, nchar(old_norm)+2L)
  dest <- file.path(work, rel)
  dir.create(dirname(dest), recursive=TRUE, showWarnings=FALSE)
  if (!isTRUE(file.copy(best$src, dest, overwrite=TRUE)) ||
      !identical(web_sha(best$src), web_sha(dest))) {
    stop("Preserved screening checkpoint copy failed checksum verification.")
  }
  completed <- as.integer(length(best$ids))
  attr(completed, "source_job_id") <- best$source_job_id
  attr(completed, "checkpoint_sha256") <- web_sha(best$src)
  attr(completed, "historical_judgment_disagreements") <- as.integer(length(different_judgments))
  attr(completed, "historical_annotation_differences") <- as.integer(length(different_annotations))
  completed
}

# Automatic cross-version screening resume.
# The decision unit stays one intact terminal-job checkpoint, never row-wise merge.
web_auto_screening_compatible <- function(source, target) {
  if (!is.list(source$agent_screening) || !is.list(target$agent_screening)) return(FALSE)
  a <- source; b <- target
  a$agent_screening$provider$model <- NULL
  b$agent_screening$provider$model <- NULL
  a$agent_screening$provider$reasoning_effort <- NULL
  b$agent_screening$provider$reasoning_effort <- NULL
  a$agent_screening$cost_optimization <- NULL
  b$agent_screening$cost_optimization <- NULL
  normalize <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x))]
    lapply(x, normalize)
  }
  identical(normalize(a), normalize(b)) &&
    (!identical(as.character(source$agent_screening$provider$model),
                as.character(target$agent_screening$provider$model)) ||
     !identical(as.character(source$agent_screening$provider$reasoning_effort),
                as.character(target$agent_screening$provider$reasoning_effort))) &&
    isTRUE(target$agent_screening$processing$resume_from_checkpoint) &&
    !isTRUE(target$agent_screening$processing$overwrite_existing_agent_decisions)
}

# Validate the historic screening prompt without silently weakening newer
# manifest-backed jobs. The legacy route does NOT prove immutable historical
# prompt provenance; it only confirms matching preserved workdir/current files.
web_auto_screening_prompt_evidence <- function(engine_root, work, historic_files,
                                                prompt, allow_legacy_untracked=FALSE) {
  if (!is.character(prompt) || length(prompt)!=1L || is.na(prompt) || !nzchar(prompt))
    return(NULL)
  normalized_prompt <- gsub("\\\\","/",prompt)
  if (grepl("^(/|~|[A-Za-z]:)",normalized_prompt) ||
      any(strsplit(normalized_prompt,"/",fixed=TRUE)[[1L]] %in% c("", ".", "..")))
    return(NULL)
  current_path <- web_inside(file.path(engine_root,prompt),engine_root,exists=FALSE)
  if (!file.exists(current_path)) return(NULL)
  current_sha <- web_sha(current_path)
  recorded <- historic_files[[prompt]]
  if (!is.null(recorded)) {
    if (!is.character(recorded) || length(recorded)!=1L ||
        !grepl("^[0-9a-f]{64}$",recorded) || !identical(current_sha,recorded))
      return(NULL)
    return(list(mode="ENGINE_MANIFEST_PINNED",sha256=current_sha))
  }
  if (!isTRUE(allow_legacy_untracked)) return(NULL)
  legacy_path <- web_inside(file.path(work,prompt),work,exists=FALSE)
  if (!file.exists(legacy_path)) return(NULL)
  link <- Sys.readlink(legacy_path)
  if (length(link)!=1L || is.na(link) || nzchar(link)) return(NULL)
  if (!identical(web_sha(legacy_path),current_sha)) return(NULL)
  list(mode="LEGACY_WORKDIR_MATCH_UNTRACKED",sha256=current_sha)
}

web_auto_screening_candidate <- function(con, settings, prior, j, target_manifest, target_root, target_cfg) {
  # Return NULL when scientifically incompatible. Never touch a prior checkpoint.
  source_version <- web_query(con,
    "SELECT manifest_json,snapshot_locator FROM project_versions WHERE id=?v AND project_id=?p",
    v=prior$version_id,p=j$project_id)
  if (nrow(source_version) != 1L) return(NULL)
  old_manifest <- web_parse(as.character(source_version$manifest_json[[1L]]))
  if (!is.list(old_manifest) || !length(old_manifest)) return(NULL)
  policy <- "config/agent_screening.yml"
  science_old <- setdiff(names(old_manifest), policy)
  science_new <- setdiff(names(target_manifest), policy)
  if (!setequal(science_old, science_new) ||
      any(vapply(science_old, function(n) {
        !identical(as.character(old_manifest[[n]]),as.character(target_manifest[[n]]))
      },logical(1)))) return(NULL)
  old_root_candidate <- file.path(settings$data_root,as.character(source_version$snapshot_locator[[1L]]))
  if (!dir.exists(old_root_candidate)) return(NULL)
  old_root <- web_inside(old_root_candidate,settings$data_root)
  for (n in names(old_manifest)) {
    source_file <- file.path(old_root,n)
    if (!file.exists(source_file)) return(NULL)
    p <- web_inside(source_file,old_root)
    if (!identical(web_sha(p),as.character(old_manifest[[n]]))) return(NULL)
  }
  loc <- as.character(prior$diagnostic_locator)
  if (is.na(loc) || !nzchar(loc)) return(NULL)
  work_candidate <- file.path(settings$data_root,loc,"project")
  if (!dir.exists(work_candidate)) return(NULL)
  work <- web_inside(work_candidate,settings$data_root)
  if (policy %in% names(old_manifest)) {
    source_cfg_path <- web_inside(file.path(old_root,policy),old_root)
  } else {
    # Legacy jobs did not snapshot model policy; their own completed workdir
    # retains the actual configuration used by the scientific R/19 run.
    # Older terminal jobs can lack a local policy file. They are not bridge
    # candidates; check existence before strict canonical path resolution.
    source_cfg_unverified <- file.path(work,policy)
    if (!file.exists(source_cfg_unverified)) return(NULL)
    source_cfg_path <- web_inside(source_cfg_unverified,work)
    historic <- web_query(con,"SELECT * FROM engine_releases WHERE id=?id",id=prior$engine_id)
    if (nrow(historic)!=1L) return(NULL)
    field <- intersect(c("manifest_json","source_manifest_json","manifest"),names(historic))
    if (!length(field)) return(NULL)
    engine_manifest <- web_parse(as.character(historic[[field[[1L]]]][[1L]]))
    if (is.null(engine_manifest[[policy]]) ||
        !identical(web_sha(source_cfg_path),as.character(engine_manifest[[policy]]))) return(NULL)
  }
  source_cfg <- tryCatch(yaml::read_yaml(source_cfg_path),error=function(e) NULL)
  if (is.null(source_cfg) || !web_auto_screening_compatible(source_cfg,target_cfg)) return(NULL)
  # Historic engine policy remains hash-pinned above. Prompt evidence:
  # (1) recorded engine manifest => require EXACT recorded SHA;
  # (2) legacy versions lacking a saved policy AND prompt manifest entry =>
  #     accept only when the historical worker's local prompt and the current
  #     canonical prompt have identical SHA. This is weaker than an immutable
  #     engine manifest, so record the provenance mode in the bridge audit.
  prompt <- as.character(source_cfg$agent_screening$prompt$template_file)
  historic_engine <- web_query(con,"SELECT * FROM engine_releases WHERE id=?id",id=prior$engine_id)
  field <- intersect(c("manifest_json","source_manifest_json","manifest"),names(historic_engine))
  if (nrow(historic_engine)!=1L || !length(field)) return(NULL)
  historic_files <- web_parse(as.character(historic_engine[[field[[1L]]]][[1L]]))
  evidence <- web_auto_screening_prompt_evidence(settings$engine_root,work,historic_files,
    prompt,allow_legacy_untracked=!(policy %in% names(old_manifest)))
  if (is.null(evidence)) return(NULL)
  hits <- list.files(work,pattern="^agent_screening_checkpoint[.]csv$",recursive=TRUE,full.names=TRUE)
  if (length(hits)!=1L) return(NULL)
  rows <- tryCatch(utils::read.csv(hits[[1L]],colClasses="character",check.names=FALSE),error=function(e) NULL)
  required <- c("screening_id","decision","validation_status","model","response_id",
                "exclusion_reason_code","exclusion_reason","confidence","rationale")
  if (is.null(rows) || !nrow(rows) || !all(required %in% names(rows)) ||
      anyNA(rows$screening_id) || any(!nzchar(trimws(rows$screening_id))) ||
      anyDuplicated(rows$screening_id) || anyNA(rows$validation_status) ||
      any(rows$validation_status!="valid") || anyNA(rows$decision) ||
      any(!rows$decision %in% c("Include","Exclude","Unclear")) ||
      anyNA(rows$model) || any(!nzchar(rows$model))) return(NULL)
  nonex <- rows$decision != "Exclude"
  norm <- function(x) {x[is.na(x)] <- ""; trimws(x)}
  if (any(nzchar(norm(rows$exclusion_reason_code[nonex]))) ||
      any(nzchar(norm(rows$exclusion_reason[nonex]))) ||
      any(!nzchar(norm(rows$exclusion_reason_code[!nonex]))) ||
      any(!nzchar(norm(rows$exclusion_reason[!nonex])))) return(NULL)
  list(path=hits[[1L]], source_job_id=as.character(prior$id),
       source_version_id=as.character(prior$version_id),
       ids=as.character(rows$screening_id), rows=rows,
       work=work, sha=web_sha(hits[[1L]]),
       prompt_evidence_mode=evidence$mode, prompt_sha256=evidence$sha256)
}

web_auto_seed_screening_checkpoint <- function(con, settings, j, work, same_version_count=0L) {
  if (!identical(as.character(j$operation),"recommend_screening")) return(0L)
  version <- web_query(con,"SELECT manifest_json,snapshot_locator FROM project_versions WHERE id=?v AND project_id=?p",
                       v=j$version_id,p=j$project_id)
  if (nrow(version)!=1L) stop("Target screening version is unavailable.")
  target_manifest <- web_parse(as.character(version$manifest_json[[1L]]))
  policy <- "config/agent_screening.yml"
  if (!(policy %in% names(target_manifest))) return(0L) # Historic same-version job path.
  root <- web_inside(file.path(settings$data_root,as.character(version$snapshot_locator[[1L]])),settings$data_root)
  # Independently verify every immutable target input before any bridge action.
  for (rel in names(target_manifest)) {
    input <- web_inside(file.path(root,rel),root)
    if (!file.exists(input) || !identical(web_sha(input),as.character(target_manifest[[rel]])))
      stop("Target version input failed checksum verification before bridge.")
  }
  cfg_file <- web_inside(file.path(root,policy),root)
  if (!identical(web_sha(cfg_file),as.character(target_manifest[[policy]])))
    stop("Target version screening configuration failed checksum verification.")
  target_cfg <- yaml::read_yaml(cfg_file)
  prior <- web_query(con,
    "SELECT id,version_id,engine_id,diagnostic_locator FROM jobs WHERE project_id=?p AND version_id<>?v AND operation='recommend_screening' AND state IN ('FAILED','USER_ACTION_REQUIRED','COMPLETE') AND diagnostic_locator IS NOT NULL ORDER BY created_at DESC",
    p=j$project_id,v=j$version_id)
  if (!nrow(prior)) return(0L)
  candidates <- list()
  for (i in seq_len(nrow(prior))) {
    found <- web_auto_screening_candidate(con,settings,as.list(prior[i,,drop=FALSE]),j,target_manifest,root,target_cfg)
    if (!is.null(found)) candidates[[length(candidates)+1L]] <- found
  }
  if (!length(candidates)) {
    # A previous terminal screening exists but the scientific contract changed.
    # Never bill for an accidental full re-screen without explicit human decision.
    web_error("USER_ACTION_REQUIRED","Past screening exists but automatic checkpoint transfer is not safe. Review the changed inputs or model policy.")
  }
  all_ids <- unique(unlist(lapply(candidates,`[[`,"ids"),use.names=FALSE))
  covering <- which(vapply(candidates,function(x) !length(setdiff(all_ids,x$ids)),logical(1)))
  if (!length(covering)) web_error("USER_ACTION_REQUIRED","Historic screening checkpoints cannot be safely reconciled automatically.")
  chosen <- candidates[[covering[[1L]]]] # prior sorted latest first
  if (same_version_count > 0L) {
    if (as.integer(same_version_count) < length(chosen$ids))
      web_error("USER_ACTION_REQUIRED","A newer-version checkpoint does not cover all older valid screening IDs. Review before resuming.")
    return(0L)
  }
  # Copy ONE complete checkpoint with byte-for-byte hash validation.
  source_work <- normalizePath(chosen$work,winslash="/",mustWork=TRUE)
  source_path <- normalizePath(chosen$path,winslash="/",mustWork=TRUE)
  rel <- substring(source_path,nchar(source_work)+2L)
  if (!nzchar(rel) || grepl("(^[/~]|^[A-Za-z]:|(^|/)\\.\\.(/|$))",rel))
    stop("Unsafe checkpoint relative path.")
  dest <- web_inside(file.path(work,rel),work,exists=FALSE)
  if (file.exists(dest)) stop("Destination checkpoint already exists; refusing overwrite.")
  dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  if (!isTRUE(file.copy(chosen$path,dest,overwrite=FALSE)) ||
      !identical(web_sha(dest),chosen$sha))
    stop("Automatic checkpoint transfer failed checksum verification.")
  completed <- as.integer(length(chosen$ids))
  attr(completed,"source_job_id") <- chosen$source_job_id
  attr(completed,"source_version_id") <- chosen$source_version_id
  attr(completed,"checkpoint_sha256") <- chosen$sha
  attr(completed,"prompt_evidence_mode") <- chosen$prompt_evidence_mode
  attr(completed,"prompt_sha256") <- chosen$prompt_sha256
  attr(completed,"historical_judgment_disagreements") <- NA_integer_
  attr(completed,"historical_annotation_differences") <- NA_integer_
  completed
}


# Resume only from a single, auditable failed attempt for the SAME project,
# frozen input version and registered engine. Never merge divergent AI batches.
web_seed_prior_design_checkpoint <- function(con,settings,j,work) {
  if(!identical(as.character(j$operation),"recommend_study_design"))return(0L)
  prior<-web_query(con,
    "SELECT id,diagnostic_locator FROM jobs WHERE project_id=?p AND version_id=?v AND engine_id=?e AND operation='recommend_study_design' AND state IN ('FAILED','USER_ACTION_REQUIRED') AND id<>?id AND diagnostic_locator IS NOT NULL ORDER BY created_at DESC",
    p=j$project_id,v=j$version_id,e=j$engine_id,id=j$id)
  if(!nrow(prior))return(0L)
  candidates<-list()
  for(i in seq_len(nrow(prior))) {
    loc<-as.character(prior$diagnostic_locator[[i]])
    if(is.na(loc)||!nzchar(loc))next
    # Never use an arbitrary DB path: the attempt must match its job ID.
    token<-basename(loc)
    if(!grepl("^[0-9a-fA-F]{32}$",token) ||
       !identical(basename(dirname(loc)),as.character(prior$id[[i]])))next
    checkpoint_dir<-file.path(settings$data_root,"jobs",as.character(prior$id[[i]]),token,
      "project","results","logs")
    if(!dir.exists(checkpoint_dir))next
    files<-sort(list.files(checkpoint_dir,
      pattern="^rc286_design_checkpoint_[0-9]{5}[.]rds$",full.names=TRUE),decreasing=TRUE)
    if(!length(files))next
    path<-files[[1L]]
    cp<-tryCatch(readRDS(path),error=function(e)NULL)
    if(!is.list(cp)||!is.list(cp$rows)||!length(cp$rows))
      web_error("USER_ACTION_REQUIRED","An earlier design checkpoint is unreadable. Operator reconciliation is required before retry.")
    candidates[[length(candidates)+1L]]<-list(path=path,cp=cp,job=as.character(prior$id[[i]]))
  }
  if(!length(candidates))return(0L)
  lengths<-vapply(candidates,function(c)length(c$cp$rows),integer(1))
  best<-candidates[[which.max(lengths)]]
  for(c in candidates) {
    k<-length(c$cp$rows)
    if(!identical(c$cp$source_digest,best$cp$source_digest) ||
       !identical(c$cp$policy_sha256,best$cp$policy_sha256) ||
       !identical(c$cp$include_ids,best$cp$include_ids) ||
       !identical(c$cp$schema_version,best$cp$schema_version) ||
       !identical(c$cp$rows,head(best$cp$rows,k)))
      web_error("USER_ACTION_REQUIRED","Divergent AI design checkpoints exist. Reconcile them before retrying; no AI calls were made.")
  }
  target<-file.path(work,"results","logs",basename(best$path))
  dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
  web_assert(isTRUE(file.copy(best$path,target,overwrite=FALSE)) &&
    identical(web_sha(best$path),web_sha(target)),
    "Could not preserve the prior AI design checkpoint.","SYSTEM_FAILED")
  completed<-as.integer(length(best$cp$rows))
  attr(completed,"source_job_id")<-best$job
  attr(completed,"checkpoint_sha256")<-web_sha(best$path)
  completed
}

web_operation_stale_seconds <- function(operation, settings) {
  if (identical(as.character(operation), "recommend_screening")) {
    x <- suppressWarnings(as.numeric(Sys.getenv("SRMA_WEB_AI_SCREENING_TIMEOUT", "43200")))
    if (!is.finite(x) || x < 3600) x <- 43200
    return(x + 60)
  }
  if(identical(as.character(operation),"recommend_study_design")) {
    x<-suppressWarnings(as.numeric(Sys.getenv("SRMA_WEB_AI_DESIGN_TIMEOUT","7200")))
    if(!is.finite(x)||x<3600)x<-7200
    return(max(x,as.numeric(settings$timeout))+60)
  }
  as.numeric(settings$timeout) + 60
}

web_worker_once <- function(con, settings) {
  j <- web_claim(con)
  if (is.null(j)) return(invisible(NULL))
  p <- web_query(con, "SELECT * FROM projects WHERE id=?p", p = j$project_id)
  relative <- file.path("jobs", j$id, j$worker_token)
  attempt <- file.path(settings$data_root, relative)
  work <- file.path(attempt, "project")
  dir.create(work, recursive = TRUE, mode = "0700")
  web_exec(con, "UPDATE jobs SET diagnostic_locator=?loc WHERE id=?id AND worker_token=?token", loc = relative, id = j$id, token = j$worker_token)
  sources <- data.frame(source_job_id=character())
  result <- tryCatch({
    web_assert(identical(web_register_engine(con, settings), j$engine_id), "The registered engine is unavailable.", "SYSTEM_FAILED")
    v <- web_query(con, "SELECT * FROM project_versions WHERE id=?v AND project_id=?p", v = j$version_id, p = j$project_id)
    manifest <- web_parse(v$manifest_json)
    snapshot <- web_inside(file.path(settings$data_root, v$snapshot_locator), settings$data_root)
    for (name in names(manifest)) {
      source <- web_inside(file.path(snapshot, name), snapshot)
      web_assert(identical(web_sha(source), manifest[[name]]), "Saved project inputs failed integrity verification.")
      dest <- web_inside(file.path(work, name), work, exists = FALSE)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      web_assert(file.copy(source, dest), "Could not prepare project inputs.", "SYSTEM_FAILED")
    }
    sources <- web_query(con,"SELECT source_job_id FROM job_sources WHERE job_id=?j AND project_id=?p AND version_id=?v",j=j$id,p=j$project_id,v=j$version_id)
    owner <- web_query(con,"SELECT user_id FROM workspace_members WHERE workspace_id=?w LIMIT 1",w=p$workspace_id)$user_id
    for(source in sources$source_job_id) web_overlay_job(con,settings,owner,j$project_id,j$version_id,source,work)
    # Legacy screening must be revised by web_submit/web_retry BEFORE queuing.
    # This fence remains in place if any alternate caller bypasses that boundary.
    if (identical(j$operation,"recommend_screening") &&
        !("config/agent_screening.yml" %in% names(manifest)))
      web_error("USER_ACTION_REQUIRED",
        "Legacy screening inputs require an automatically saved, verified policy version. Retry through the Web screening action.")
    if(identical(j$operation,"recommend_study_design")) {
      design_resumed<-web_seed_prior_design_checkpoint(con,settings,j,work)
      if(design_resumed>0L) {
        web_audit(con,p,"JOB_DESIGN_CHECKPOINT_RESUMED",j$id,
          detail=list(completed=as.integer(design_resumed),source_version=j$version_id,
            source_job_id=attr(design_resumed,"source_job_id"),
            checkpoint_sha256=attr(design_resumed,"checkpoint_sha256")))
        web_exec(con,"UPDATE jobs SET message=?m,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
          m=paste0("Resuming AI Include design classification: ",design_resumed," preserved records."),
          now=web_now(),id=j$id,token=j$worker_token)
      }
    }
    resumed <- web_seed_prior_screening_checkpoint(con,settings,j,work)
    bridge <- web_auto_seed_screening_checkpoint(con,settings,j,work,resumed)
    if (bridge > 0L) {
      resumed <- bridge
      web_audit(con,p,"JOB_SCREENING_CHECKPOINT_BRIDGED",j$id,detail=list(
        source_job_id=attr(bridge,"source_job_id"),
        source_version_id=attr(bridge,"source_version_id"),
        checkpoint_sha256=attr(bridge,"checkpoint_sha256"),
        prompt_evidence_mode=attr(bridge,"prompt_evidence_mode"),
        prompt_sha256=attr(bridge,"prompt_sha256"),
        completed=as.integer(bridge),target_version_id=as.character(j$version_id)))
    }
    if(resumed>0L) {
      web_exec(con,"UPDATE jobs SET message=?m,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
        m=paste0("Resuming AI screening from ",resumed," preserved checkpoint decisions…"),now=web_now(),id=j$id,token=j$worker_token)
      web_audit(con,p,"JOB_CHECKPOINT_RESUMED",j$id,detail=list(
        completed=as.integer(resumed),
        source_job_id=attr(resumed,"source_job_id"),
        checkpoint_sha256=attr(resumed,"checkpoint_sha256"),
        historical_judgment_disagreements=attr(resumed,"historical_judgment_disagreements"),
        historical_annotation_differences=attr(resumed,"historical_annotation_differences")))
    }
    web_b2_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
    web_c1_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
  web_c2_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
  web_c3_guard(con,settings,owner,j$project_id,j$version_id,j$operation)
    if(j$operation=="run_final_qc") {proof<-web_c3_collect(con,settings,owner,j$project_id,j$version_id,work);jsonlite::write_json(proof,file.path(work,"config/generated/final_qc_input.json"),auto_unbox=TRUE,null="null",digits=NA)}
    web_require_activated(con,j$project_id,j$version_id,j$operation)
    starting_progress <- if(j$operation %in% c("recommend_screening","recommend_study_design")) 0L else 30L
    starting_message <- if(identical(j$operation,"recommend_screening"))
      "AI title/abstract screening is running. Large reviews may take several hours; checkpointed decisions are preserved." else if(identical(j$operation,"recommend_study_design"))
      "AI Include-only design classification is running; completed batches are checkpointed and can be resumed." else
      web_registry()[[j$operation]]$progress
    web_exec(con, "UPDATE jobs SET progress=?progress,message=?message,heartbeat=?time,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
      progress=starting_progress,message=starting_message,time=as.numeric(Sys.time()),now=web_now(),id=j$id,token=j$worker_token)
    web_audit(con,p,"JOB_PROGRESS",j$id,detail=list(progress=starting_progress))
    srma_run(settings, p$canonical_id, j$operation, list(), j$id, work, j$engine_id, file.path(attempt, "engine"),version_id=j$version_id)
  }, error = function(e) web_result(j$operation, p$canonical_id, j$id, "FAILED",
       if (inherits(e, "srma_web_error")) conditionMessage(e) else "Processing could not finish. The operator can inspect the diagnostic record.",
       j$engine_id, if (inherits(e, "srma_web_error")) e$code else "SYSTEM_FAILED", conditionMessage(e)))
  # Storage writes may leave unreferenced immutable objects after interruption; never publish partial registry rows.
  stored <- tryCatch({
    if (result$status %in% c("COMPLETE", "USER_ACTION_REQUIRED")) lapply(result$artifacts, function(a) {
      web_inside(a$path, work)
      web_no_file_secrets(a$path,settings$engine_root)
      id <- web_id()
      obj <- web_store(settings)$put(a$path, paste0(j$project_id, "/", id))
      c(list(id = id, logical_name = a$logical_name, type = a$type, relative_path = substring(normalizePath(a$path),nchar(normalizePath(work))+2L)), obj)
    }) else list()
  }, error = identity)
  if (inherits(stored, "error")) {
    result <- web_result(j$operation, p$canonical_id, j$id, "FAILED", "The output could not be stored. The operator can retry this check.", j$engine_id, "SYSTEM_FAILED", conditionMessage(stored))
    stored <- list()
  }
  web_tx(con, function() {
    # Fence late completions from a failed/recovered attempt before registry publication.
    current <- web_exec(con, "UPDATE jobs SET updated_at=updated_at WHERE id=?id AND state='RUNNING' AND worker_token=?token", id = j$id, token = j$worker_token)
    if (current != 1L) return(invisible(FALSE))
    web_exec(con, "UPDATE projects SET title=title WHERE id=?p", p = j$project_id)
    for (a in stored) {
      version <- web_query(con, "SELECT COALESCE(MAX(version),0)+1 AS n FROM artifacts WHERE project_id=?p AND logical_name=?n", p = j$project_id, n = a$logical_name)$n
      web_exec(con, "INSERT INTO artifacts VALUES(?id,?p,?v,?j,?name,?type,?n,'AVAILABLE',?sha,?loc,?e,'UNREVIEWED',?now)",
        id = a$id, p = j$project_id, v = j$version_id, j = j$id, name = a$logical_name, type = a$type,
        n = version, sha = a$checksum, loc = a$locator, e = j$engine_id, now = web_now())
    }
    result$artifacts <- lapply(stored, function(a) list(artifact_id = a$id, logical_name = a$logical_name, checksum = a$checksum, relative_path = a$relative_path))
    result$provenance$project_version_id <- j$version_id
    result$provenance$attempt <- j$attempts
    result$provenance$source_job_ids <- as.list(sources$source_job_id)
    web_exec(con, "UPDATE jobs SET state=?s,message=?m,progress=?progress,result_json=?result,heartbeat=?time,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token",
      s = result$status, m = result$user_message, progress = if (result$status == "COMPLETE") 100L else 30L,
      result = as.character(web_json(result)), time = as.numeric(Sys.time()), now = web_now(), id = j$id, token = j$worker_token)
    web_audit(con, p, paste0("JOB_", result$status), j$id, detail = list(artifacts = length(stored), attempt = j$attempts))
    TRUE
  })
  invisible(j$id)
}
web_recover_stale <- function(con, settings) {
  # Long-running screening uses a longer lease; ordinary jobs retain the
  # standard timeout. Recovery remains fenced by the current worker token.
  web_tx(con, function() {
    jobs <- web_query(con, "SELECT * FROM jobs WHERE state='RUNNING'")
    if (!nrow(jobs)) return(0L)
    recovered <- 0L
    now_num <- as.numeric(Sys.time())
    for (i in seq_len(nrow(jobs))) {
      j <- jobs[i, , drop = FALSE]
      cutoff <- now_num - web_operation_stale_seconds(j$operation, settings)
      hb <- suppressWarnings(as.numeric(j$heartbeat))
      if (!is.finite(hb) || hb >= cutoff) next
      r <- web_result(j$operation, j$project_id, j$id, "FAILED", "Processing was interrupted. The operator can retry this check.", j$engine_id, "SYSTEM_FAILED", "Worker lease expired; late completion is fenced.")
      n <- web_exec(con, "UPDATE jobs SET state='FAILED',message=?m,result_json=?r,worker_token=NULL,updated_at=?now WHERE id=?id AND state='RUNNING' AND worker_token=?token AND heartbeat<?before",
        before=cutoff,m=r$user_message,r=as.character(web_json(r)),now=web_now(),id=j$id,token=j$worker_token)
      if (n) {
        recovered <- recovered + 1L
        web_audit(con, web_query(con, "SELECT * FROM projects WHERE id=?p", p=j$project_id), "WORKER_LEASE_EXPIRED", j$id)
      }
    }
    recovered
  })
}
