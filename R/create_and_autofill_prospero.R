#!/usr/bin/env Rscript

cat("PROSPERO MODULE VERSION: 2026-08-15-AUTO-PROJECT-v2\n")

suppressPackageStartupMessages({
  library(openxlsx)
  library(yaml)
})

find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, "templates")) &&
        dir.exists(file.path(current, "projects"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not find toolkit root.")
    current <- parent
  }
}

ask_yes_no <- function(prompt) {
  repeat {
    ans <- trimws(tolower(readline(prompt)))
    if (ans %in% c("y","yes","예","네")) return(TRUE)
    if (ans %in% c("n","no","아니오","아니요")) return(FALSE)
    cat("Please enter Yes or No.\n")
  }
}

detect_project <- function(repo_root) {
  projects_root <- file.path(repo_root, "projects")
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  pr <- normalizePath(projects_root, winslash = "/", mustWork = TRUE)

  # 1. Current working directory is inside projects/<project>
  prefix <- paste0(pr, "/")
  if (startsWith(wd, prefix)) {
    rel <- substring(wd, nchar(prefix) + 1)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    if (length(parts) >= 1 && nzchar(parts[[1]])) {
      cand <- parts[[1]]
      if (dir.exists(file.path(projects_root, cand))) {
        cat("Project detected from working directory: ", cand, "\n", sep = "")
        return(cand)
      }
    }
  }

  # 2. Enumerate project folders
  entries <- list.dirs(projects_root, full.names = FALSE, recursive = FALSE)
  entries <- entries[nzchar(entries)]
  entries <- entries[!tolower(entries) %in% c(
    "archive","archived","_archive","_templates","template","templates"
  )]

  if (length(entries) == 0) stop("No project folders found under projects/.")

  if (length(entries) == 1) {
    cat("Project detected: ", entries[[1]], "\n", sep = "")
    return(entries[[1]])
  }

  cat("\nAvailable SR-MA projects:\n\n")
  for (i in seq_along(entries)) {
    cat(sprintf("  %d. %s\n", i, entries[[i]]))
  }

  repeat {
    ans <- trimws(readline(sprintf("\nSelect project [1-%d]: ", length(entries))))
    idx <- suppressWarnings(as.integer(ans))
    if (!is.na(idx) && idx >= 1 && idx <= length(entries)) {
      cat("Selected project: ", entries[[idx]], "\n", sep = "")
      return(entries[[idx]])
    }
    cat("Please enter one of the project numbers shown above.\n")
  }
}

# --- Load the full module from previous canonical implementation if present ---
# To keep this file self-contained, the rest of the functions are defined below.

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) ||
    !nzchar(trimws(paste(x, collapse = "")))
}

value_to_text <- function(x) {
  if (is_blank(x)) return("")
  if (is.atomic(x) && length(x) == 1) return(as.character(x))
  if (is.atomic(x)) {
    x <- x[!is.na(x)]
    return(paste(as.character(x), collapse = "; "))
  }
  if (is.list(x)) {
    flat <- unlist(x, recursive = TRUE, use.names = FALSE)
    flat <- flat[!is.na(flat)]
    return(paste(as.character(flat), collapse = "; "))
  }
  as.character(x)
}

get_nested_value <- function(x, dotted_path) {
  parts <- strsplit(dotted_path, "\\.")[[1]]
  cur <- x
  for (p in parts) {
    if (!is.list(cur) || is.null(cur[[p]])) return(NULL)
    cur <- cur[[p]]
  }
  cur
}

first_nonblank <- function(...) {
  vals <- list(...)
  for (v in vals) if (!is_blank(v)) return(v)
  NULL
}

parse_sheet_cell <- function(ref) {
  bits <- strsplit(ref, "!", fixed = TRUE)[[1]]
  list(sheet = bits[[1]], cell = bits[[2]])
}

cell_row <- function(cell) as.integer(gsub("[^0-9]", "", cell))
cell_col <- function(cell) openxlsx::convertFromExcelRef(gsub("[^A-Za-z]", "", cell))

read_project_config <- function(repo_root, project_slug) {
  candidates <- c(
    file.path(repo_root, "projects", project_slug, "config", "project_config.yml"),
    file.path(repo_root, "config", "project_config.yml")
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0) stop("project_config.yml was not found.")
  config_path <- hits[[1]]
  cat("Using config: ", normalizePath(config_path, winslash="/", mustWork=TRUE), "\n", sep="")
  list(path=config_path, data=yaml::read_yaml(config_path))
}

read_text_safely <- function(path, max_chars=30000) {
  txt <- tryCatch(paste(readLines(path, warn=FALSE, encoding="UTF-8"), collapse="\n"),
                  error=function(e) "")
  substr(txt, 1, min(nchar(txt), max_chars))
}

collect_project_text <- function(project_dir) {
  dirs <- c(project_dir, file.path(project_dir,"protocol"),
            file.path(project_dir,"plans"), file.path(project_dir,"search"),
            file.path(project_dir,"config"))
  dirs <- dirs[dir.exists(dirs)]
  files <- unique(unlist(lapply(dirs, function(d)
    list.files(d, pattern="\\.(md|txt|yml|yaml)$", recursive=TRUE,
               full.names=TRUE, ignore.case=TRUE))))
  files <- files[!grepl("[/\\\\]registration[/\\\\]prospero[/\\\\]", files, ignore.case=TRUE)]
  if (!length(files)) return("")
  paste(vapply(files, function(f)
    paste0("\n### FILE: ", basename(f), "\n", read_text_safely(f)),
    character(1)), collapse="\n")
}

extract_after_label <- function(text, labels, max_chars=2500) {
  if (!nzchar(text)) return("")
  lines <- unlist(strsplit(text, "\n", fixed=TRUE))
  low <- tolower(lines)
  for (label in labels) {
    idx <- grep(tolower(label), low, fixed=TRUE)
    if (!length(idx)) next
    i <- idx[[1]]
    candidate <- sub(paste0("^.*", label, "\\s*[:=-]?\\s*"), "", lines[[i]], ignore.case=TRUE)
    if (nzchar(trimws(candidate))) return(substr(trimws(candidate),1,max_chars))
  }
  ""
}

pick_config <- function(cfg, paths) {
  for (p in paths) {
    v <- get_nested_value(cfg,p)
    if (!is_blank(v)) return(v)
  }
  NULL
}

infer_project_information <- function(cfg, project_text) {
  info <- list()
  info$title <- first_nonblank(pick_config(cfg,c("review.title","project.title","title")),
                               extract_after_label(project_text,c("Review title","Title")))
  info$review_type <- first_nonblank(pick_config(cfg,c("review.type","review_type")),
                                     "Systematic review with meta-analysis where quantitative synthesis is appropriate.")
  info$domain <- first_nonblank(pick_config(cfg,c("review.domain","condition","domain","condition_or_domain")),
                                extract_after_label(project_text,c("Condition or domain","Clinical domain","Domain")))
  info$rationale <- first_nonblank(pick_config(cfg,c("review.rationale","protocol.rationale","rationale")),
                                   extract_after_label(project_text,c("Rationale for the review","Rationale","Background")))
  info$objectives <- first_nonblank(pick_config(cfg,c("review.objectives","review.question","research_question","objective","objectives")),
                                    extract_after_label(project_text,c("Review objectives","Objective","Research question")))
  info$keywords <- first_nonblank(pick_config(cfg,c("review.keywords","keywords","search.keywords")),
                                  extract_after_label(project_text,c("Keywords")))
  info$country <- pick_config(cfg,c("project.country","country"))
  info$population <- first_nonblank(pick_config(cfg,c("pico.population","eligibility.population","population")),
                                    extract_after_label(project_text,c("Population")))
  info$intervention <- first_nonblank(pick_config(cfg,c("pico.intervention","pico.exposure","eligibility.intervention","intervention","exposure")),
                                      extract_after_label(project_text,c("Intervention","Exposure")))
  info$comparator <- first_nonblank(pick_config(cfg,c("pico.comparator","eligibility.comparator","comparator","control")),
                                    extract_after_label(project_text,c("Comparator","Control")))
  info$study_design <- first_nonblank(pick_config(cfg,c("eligibility.study_designs","study_designs","study_design")),
                                      extract_after_label(project_text,c("Study design")))
  info$context <- first_nonblank(pick_config(cfg,c("eligibility.context","context")),
                                 extract_after_label(project_text,c("Context")))
  info$timeline <- first_nonblank(
    pick_config(cfg,c("project.timeline","review.timeline")),
    {
      s <- pick_config(cfg,c("project.start_date","review.start_date","start_date"))
      e <- pick_config(cfg,c("project.end_date","review.end_date","anticipated_completion_date"))
      if (!is_blank(s) || !is_blank(e))
        paste(c(if (!is_blank(s)) paste0("Review start: ",value_to_text(s)),
                if (!is_blank(e)) paste0("Anticipated completion: ",value_to_text(e))),
              collapse="; ")
      else NULL
    })
  info$protocol_availability <- first_nonblank(
    pick_config(cfg,c("protocol.availability","protocol.url","protocol.doi")),
    "A full protocol is maintained within the project records and will be made available as appropriate.")
  info$unpublished_search <- first_nonblank(
    pick_config(cfg,c("search.unpublished_studies","search.grey_literature")),
    "Unpublished or ongoing studies will be considered when identified through planned sources or supplementary searching.")
  info$databases <- first_nonblank(
    pick_config(cfg,c("search.databases","databases","search.sources")),
    extract_after_label(project_text,c("Databases","Main sources","Information sources")))
  info$language_restrictions <- first_nonblank(
    pick_config(cfg,c("search.language_restrictions","eligibility.language_restrictions")),
    "No language restrictions are planned unless explicitly specified in the project configuration.")
  info$date_restrictions <- first_nonblank(
    pick_config(cfg,c("search.date_restrictions","eligibility.date_restrictions")),
    "No publication-date restrictions are planned unless explicitly specified in the project configuration.")
  info$other_identification <- first_nonblank(
    pick_config(cfg,c("search.other_methods","search.supplementary_methods")),
    "Reference lists of included studies and relevant reviews will be checked for additional eligible studies.")
  info$selection_process <- first_nonblank(
    pick_config(cfg,c("screening.method","selection_process")),
    "Records will be deduplicated and screened against prespecified eligibility criteria. Full-text eligibility decisions and exclusion reasons will be documented.")
  info$search_other <- first_nonblank(pick_config(cfg,c("search.other_information")),
                                      "Search strategies and screening decisions will be retained in the project audit trail.")
  info$extraction <- first_nonblank(
    pick_config(cfg,c("extraction.method","data_extraction.method")),
    "Data will be extracted into a structured study-level dataset using prespecified fields. Values used for quantitative synthesis will undergo human verification before analysis.")
  info$risk_of_bias <- first_nonblank(
    pick_config(cfg,c("risk_of_bias","risk_of_bias.method","rob.method")),
    "Risk of bias will be assessed using an appropriate design-specific tool and recorded at study level.")
  info$reporting_bias <- first_nonblank(
    pick_config(cfg,c("analysis.reporting_bias","reporting_bias")),
    "Reporting bias will be evaluated when the number and structure of included studies permit meaningful assessment; otherwise this limitation will be reported.")
  info$certainty <- first_nonblank(
    pick_config(cfg,c("analysis.certainty_assessment","certainty_assessment")),
    "Certainty of evidence will be assessed when appropriate for the review question and available evidence.")
  info$primary_outcomes <- first_nonblank(
    pick_config(cfg,c("outcomes.primary","primary_outcomes","outcome.primary")),
    extract_after_label(project_text,c("Primary outcome","Main outcomes")))
  info$secondary_outcomes <- first_nonblank(
    pick_config(cfg,c("outcomes.secondary","secondary_outcomes","outcome.secondary")),
    extract_after_label(project_text,c("Secondary outcome","Additional outcomes")))
  info$synthesis <- first_nonblank(
    pick_config(cfg,c("analysis.synthesis","analysis.plan","meta_analysis.plan")),
    "Where studies are sufficiently comparable, quantitative synthesis will be performed using an appropriate effect measure. A random-effects approach will be used when clinically and methodologically appropriate. Heterogeneity will be assessed and prespecified sensitivity or subgroup analyses will be performed when supported by the data.")
  info$review_stage <- first_nonblank(pick_config(cfg,c("registration.review_stage","review.stage")),
                                      "Protocol development / review preparation before formal database searching.")
  info$publication_status <- first_nonblank(pick_config(cfg,c("registration.publication_status")),
                                            "Review results have not yet been published.")
  # Review team members and affiliation are registration-critical identity fields.
  # Do not infer these from free text and do not fabricate default statements.
  # If absent from structured project configuration, the form will mark them
  # USER REQUIRED for author completion.
  info$team <- pick_config(
    cfg,
    c("team", "review.team", "authors")
  )

  info$affiliation <- pick_config(
    cfg,
    c("affiliation", "review.affiliation", "project.affiliation")
  )
  info$funding <- first_nonblank(pick_config(cfg,c("funding","funding.source","project.funding")),
                                 extract_after_label(project_text,c("Funding","Funding source")))
  info$peer_review <- first_nonblank(pick_config(cfg,c("peer_review","review.peer_review")),
                                     "The review protocol has not undergone external peer review unless otherwise stated.")
  info$additional_information <- first_nonblank(
    pick_config(cfg,c("registration.additional_information","additional_information")),
    "No additional information at this stage.")
  info$conflicts <- first_nonblank(pick_config(cfg,c("conflicts","conflict_of_interest","review.conflicts")),
                                   extract_after_label(project_text,c("Conflict of interest","Conflicts of interest")))
  info$mesh_terms <- first_nonblank(pick_config(cfg,c("search.mesh_terms","search.mesh","mesh_terms")),
                                    extract_after_label(project_text,c("Medical Subject Headings","MeSH")))
  info
}

prospero_sections <- function() {
  list(
    "REVIEW TITLE AND BASIC DETAILS" = list(
      c("PR001","Review title","title","AUTO"),
      c("PR002","Review type","review_type","AUTO"),
      c("PR003","Condition or domain being studied","domain","AUTO"),
      c("PR004","Rationale for the review","rationale","AUTO"),
      c("PR005","Review objectives","objectives","AUTO"),
      c("PR006","Keywords","keywords","AUTO"),
      c("PR007","Country","country","AUTO")),
    "ELIGIBILITY CRITERIA" = list(
      c("PR008","Population","population","AUTO"),
      c("PR009","Intervention(s) or exposure(s)","intervention","AUTO"),
      c("PR010","Comparator(s) or control(s)","comparator","AUTO"),
      c("PR011","Study design","study_design","AUTO"),
      c("PR012","Context","context","AUTO")),
    "SIMILAR REVIEWS" = list(
      c("PR013","Check for similar records already in PROSPERO","similar_reviews","USER REQUIRED")),
    "TIMELINE OF THE REVIEW" = list(
      c("PR014","Date of first submission to PROSPERO","first_submission_date","AUTO"),
      c("PR015","Review timeline","timeline","AUTO")),
    "AVAILABILITY OF FULL PROTOCOL" = list(
      c("PR017","Availability of full protocol","protocol_availability","AUTO")),
    "SEARCHING AND SCREENING" = list(
      c("PR018","Search for unpublished studies","unpublished_search","AUTO"),
      c("PR019","Main sources that will be searched","databases","AUTO"),
      c("PR020","Search language restrictions","language_restrictions","AUTO"),
      c("PR021","Search date restrictions","date_restrictions","AUTO"),
      c("PR022","Other methods of identifying studies","other_identification","AUTO"),
      c("PR023","Link to search strategy","search_strategy_link","USER REQUIRED"),
      c("PR024","Selection process","selection_process","AUTO"),
      c("PR025","Other relevant information about searching and screening","search_other","AUTO")),
    "DATA COLLECTION PROCESS" = list(
      c("PR026","Data extraction from published articles and reports","extraction","AUTO"),
      c("PR027","Study risk of bias or quality assessment","risk_of_bias","AUTO"),
      c("PR028","Reporting bias assessment","reporting_bias","AUTO"),
      c("PR029","Certainty assessment","certainty","AUTO")),
    "OUTCOMES TO BE ANALYSED" = list(
      c("PR030","Main outcomes","primary_outcomes","AUTO"),
      c("PR031","Additional outcomes","secondary_outcomes","AUTO")),
    "PLANNED DATA SYNTHESIS" = list(
      c("PR032","Strategy for data synthesis","synthesis","AUTO")),
    "CURRENT REVIEW STAGE" = list(
      c("PR033","Stage of the review at this submission","review_stage","AUTO"),
      c("PR034","Publication of review results","publication_status","AUTO")),
    "REVIEW AFFILIATION, FUNDING AND PEER REVIEW" = list(
      # AUTO only when structured config values exist;
      # otherwise autofill_prospero() marks these USER REQUIRED.
      c("PR035","Review team members","team","AUTO"),
      c("PR036","Review affiliation","affiliation","AUTO"),
      c("PR037","Funding source","funding","AUTO"),
      c("PR038","Peer review","peer_review","AUTO")),
    "ADDITIONAL INFORMATION" = list(
      c("PR039","Additional information","additional_information","AUTO"),
      c("PR040","Review conflict of interest","conflicts","AUTO"),
      c("PR041","Medical Subject Headings","mesh_terms","AUTO"),
      c("PR043","Disclaimer","disclaimer","SYSTEM OUTPUT"))
  )
}

create_prospero_template <- function(template_path) {
  dir.create(dirname(template_path), recursive=TRUE, showWarnings=FALSE)
  wb <- createWorkbook()
  addWorksheet(wb,"PROSPERO_Form",gridLines=FALSE)
  addWorksheet(wb,"Field_Map",gridLines=TRUE)

  title_style <- createStyle(fgFill="#1F4E78",fontColour="#FFFFFF",textDecoration="bold",fontSize=14)
  section_style <- createStyle(fgFill="#D9EAF7",fontColour="#1F4E79",textDecoration="bold")
  label_style <- createStyle(textDecoration="bold",wrapText=TRUE)
  answer_style <- createStyle(fgFill="#FFFDF2",wrapText=TRUE,valign="top",
                              border="TopBottomLeftRight",borderColour="#C9C9C9")
  meta_style <- createStyle(fgFill="#F2F2F2",fontColour="#666666",fontSize=9,wrapText=TRUE)
  header_style <- createStyle(fgFill="#1F4E78",fontColour="#FFFFFF",textDecoration="bold",
                              halign="center",wrapText=TRUE)

  writeData(wb,"PROSPERO_Form","PROSPERO Universal Registration Form",startRow=1,startCol=1)
  mergeCells(wb,"PROSPERO_Form",cols=1:6,rows=1)
  addStyle(wb,"PROSPERO_Form",title_style,rows=1,cols=1:6,gridExpand=TRUE)

  fmap <- data.frame(Field_ID=character(),Section=character(),PROSPERO.field=character(),
                     Answer_Cell=character(),Auto.fill.key=character(),Default.status=character(),
                     User.editable=character(),stringsAsFactors=FALSE)

  row <- 3
  for (sec in names(prospero_sections())) {
    writeData(wb,"PROSPERO_Form",sec,startRow=row,startCol=1)
    mergeCells(wb,"PROSPERO_Form",cols=1:6,rows=row)
    addStyle(wb,"PROSPERO_Form",section_style,rows=row,cols=1:6,gridExpand=TRUE)
    row <- row+1
    for (item in prospero_sections()[[sec]]) {
      label_row <- row; answer_row <- row+1
      writeData(wb,"PROSPERO_Form",item[[2]],startRow=label_row,startCol=1)
      mergeCells(wb,"PROSPERO_Form",cols=1:6,rows=label_row)
      addStyle(wb,"PROSPERO_Form",label_style,rows=label_row,cols=1:6,gridExpand=TRUE)

      mergeCells(wb,"PROSPERO_Form",cols=1:4,rows=answer_row)
      addStyle(wb,"PROSPERO_Form",answer_style,rows=answer_row,cols=1:4,gridExpand=TRUE)
      writeData(wb,"PROSPERO_Form","Status",startRow=answer_row,startCol=5)
      writeData(wb,"PROSPERO_Form",item[[4]],startRow=answer_row,startCol=6)
      addStyle(wb,"PROSPERO_Form",meta_style,rows=answer_row,cols=5:6,gridExpand=TRUE)
      dataValidation(wb,"PROSPERO_Form",col=6,rows=answer_row,type="list",
                     value='"AUTO,AUTO + EDITED,USER REQUIRED,SYSTEM OUTPUT"')

      fmap <- rbind(fmap,data.frame(Field_ID=item[[1]],Section=sec,PROSPERO.field=item[[2]],
                                    Answer_Cell=paste0("PROSPERO_Form!A",answer_row),
                                    Auto.fill.key=item[[3]],Default.status=item[[4]],
                                    User.editable=ifelse(item[[4]]=="SYSTEM OUTPUT","No","Yes"),
                                    stringsAsFactors=FALSE))
      row <- row+3
    }
  }

  setColWidths(wb,"PROSPERO_Form",cols=1,widths=28)
  setColWidths(wb,"PROSPERO_Form",cols=2:4,widths=16)
  setColWidths(wb,"PROSPERO_Form",cols=5,widths=12)
  setColWidths(wb,"PROSPERO_Form",cols=6,widths=22)

  writeData(wb,"Field_Map",fmap,headerStyle=header_style)
  saveWorkbook(wb,template_path,overwrite=TRUE)
  invisible(loadWorkbook(template_path))
}

autofill_prospero <- function(template_path, output_path, info) {
  file.copy(template_path,output_path,overwrite=TRUE)
  wb <- loadWorkbook(output_path)
  fmap <- read.xlsx(output_path,sheet="Field_Map")
  auto_n <- 0L; user_n <- 0L; sys_n <- 0L

  for (i in seq_len(nrow(fmap))) {
    ref <- parse_sheet_cell(fmap$Answer_Cell[[i]])
    key <- fmap$Auto.fill.key[[i]]
    if (fmap$Default.status[[i]]=="SYSTEM OUTPUT") { sys_n <- sys_n+1L; next }

    if (key=="similar_reviews") {
      txt <- "AUTHOR CHECK REQUIRED: Search PROSPERO for similar ongoing or completed records before submission."
    } else if (key=="first_submission_date") {
      txt <- format(Sys.Date(),"%Y-%m-%d")
    } else if (key=="search_strategy_link") {
      txt <- "AUTHOR CHECK REQUIRED: Add the final public link/URL to the search strategy if applicable."
    } else {
      txt <- value_to_text(info[[key]])
    }

    status <- if (nzchar(txt) && !grepl("^AUTHOR CHECK REQUIRED:",txt)) "AUTO" else "USER REQUIRED"

    if (nzchar(txt)) {
      writeData(wb,ref$sheet,txt,startCol=cell_col(ref$cell),startRow=cell_row(ref$cell),
                colNames=FALSE,rowNames=FALSE)
    }
    writeData(wb,ref$sheet,status,startCol=6,startRow=cell_row(ref$cell),
              colNames=FALSE,rowNames=FALSE)
    if (status=="AUTO") auto_n <- auto_n+1L else user_n <- user_n+1L
  }

  saveWorkbook(wb,output_path,overwrite=TRUE)
  invisible(loadWorkbook(output_path))
  list(auto=auto_n,user_required=user_n,system_output=sys_n)
}

# MAIN
repo_root <- find_repo_root()
project_slug <- detect_project(repo_root)
project_dir <- file.path(repo_root,"projects",project_slug)

cat("\nPROSPERO 등록을 먼저 하는 것이 순서입니다. 등록에 필요한 정보 준비를 진행하시겠습니까? ",
    "만약 이미 https://www.crd.york.ac.uk/prospero/ 에 등록을 진행하셨다면 No 를 선택하세요.\n", sep="")

if (!ask_yes_no("[Yes/No]: ")) {
  cat("PROSPERO preparation skipped. You may continue to the search step.\n")
  quit(save="no",status=0)
}

template_path <- file.path(repo_root,"templates","registration","Universal_PROSPERO_Form_Template.xlsx")
out_dir <- file.path(project_dir,"registration","prospero")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
autofilled_path <- file.path(out_dir,"prospero_autofilled.xlsx")
final_path <- file.path(out_dir,"prospero_final.xlsx")

cat("\nCreating canonical PROSPERO template...\n")
create_prospero_template(template_path)
cat("PASS: template created and re-opened successfully.\n")

cfg <- read_project_config(repo_root,project_slug)
project_text <- collect_project_text(project_dir)
info <- infer_project_information(cfg$data,project_text)

cat("Creating near-complete PROSPERO draft...\n")
summary <- autofill_prospero(template_path,autofilled_path,info)

cat("\nPROSPERO AUTO-FILL SUMMARY\n")
cat("Project: ",project_slug,"\n",sep="")
cat("AUTO: ",summary$auto,"\n",sep="")
cat("USER REQUIRED: ",summary$user_required,"\n",sep="")
cat("SYSTEM OUTPUT: ",summary$system_output,"\n",sep="")
cat("Created: ",normalizePath(autofilled_path,winslash="/",mustWork=TRUE),"\n",sep="")

cat("\nPlease review the draft. If you edit an AUTO answer, change its status to 'AUTO + EDITED'.\n")

if (ask_yes_no("\n이 내용을 실제 PROSPERO 등록에 사용할 최종 양식으로 확정하시겠습니까? [Yes/No]: ")) {
  file.copy(autofilled_path,final_path,overwrite=TRUE)
  invisible(loadWorkbook(final_path))
  cat("PASS: prospero_final.xlsx created.\n")
  cat("Please register directly at https://www.crd.york.ac.uk/prospero/\n")
} else {
  cat("Working file kept for further editing.\n")
}
