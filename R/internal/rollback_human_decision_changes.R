# ============================================================
# Universal SR-MA Agent Toolkit
# R/internal/rollback_human_decision_changes.R
# ============================================================

rollback_human_decision_changes <- function(
    backup_manifest,
    generated_files = character(0)
) {
  rollback_errors <- character(0)

  for (record in backup_manifest$files) {
    original <- record$original
    backup <- record$backup
    existed <- isTRUE(record$existed)

    if (existed) {
      if (!file.exists(backup)) {
        rollback_errors <- c(
          rollback_errors,
          paste0("Backup missing: ", backup)
        )
        next
      }

      restored <- file.copy(
        backup,
        original,
        overwrite = TRUE
      )

      if (!isTRUE(restored)) {
        rollback_errors <- c(
          rollback_errors,
          paste0("Failed to restore: ", original)
        )
      }
    } else if (file.exists(original)) {
      removed <- unlink(
        original,
        force = TRUE
      )

      if (!identical(removed, 0L)) {
        rollback_errors <- c(
          rollback_errors,
          paste0("Failed to remove new file: ", original)
        )
      }
    }
  }

  for (file in generated_files) {
    if (file.exists(file)) {
      removed <- unlink(
        file,
        recursive = TRUE,
        force = TRUE
      )

      if (!identical(removed, 0L)) {
        rollback_errors <- c(
          rollback_errors,
          paste0("Failed to remove generated file: ", file)
        )
      }
    }
  }

  if (length(rollback_errors) > 0L) {
    warning(
      "Rollback completed with problems:\n- ",
      paste(rollback_errors, collapse = "\n- "),
      call. = FALSE
    )

    return(invisible(FALSE))
  }

  invisible(TRUE)
}
