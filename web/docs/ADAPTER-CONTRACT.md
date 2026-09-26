# Semantic operation contract

`srma_run(settings, project_id, operation, payload, job_id, project_dir, engine_version, diagnostic_dir)` is the private adapter entry. UI callers use `web_submit`, which resolves and pins the saved project version and engine; callers cannot specify scripts or worker paths. Block A accepts only `validate_search` with an empty payload. Other registry operations are future contracts, not working features.

Results contain operation/project/job IDs, status, a plain user message, a separate `technical` diagnostic field, error code, engine identity, artifact references, warnings, start/finish timestamps and provenance. Child exceptions never become normal UI text. Provenance includes project version, exact inputs, engine script hash and actual R runtime. Successful worker publication replaces private artifact paths with opaque artifact IDs.

Error meanings:
- USER_ACTION_REQUIRED: missing configured inputs or unmet human review requirement.
- VALIDATION_FAILED: invalid configuration, unsupported request, unsafe path or failed canonical syntax checks.
- ENGINE_FAILED: unexpected scientific engine exception.
- DEPENDENCY_FAILED: required processing package unavailable.
- SYSTEM_FAILED: runtime/process/storage failure or engine drift.

A failed validation requires a corrected input version; it is not silently retried. Only replay-safe SYSTEM/DEPENDENCY failures and resolved user-action pauses can be explicitly retried, with at most two attempts. Diagnostic logs remain operator-only. An unavailable required JSON parser produces SYSTEM_FAILED at the parent boundary because the child cannot emit a structured response.
