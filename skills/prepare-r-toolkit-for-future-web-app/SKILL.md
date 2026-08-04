# Skill: Prepare the R Toolkit for a Future Web Application

## Purpose
Guide development of the Universal SR-MA Agent Toolkit so the core research workflow is completed and stabilized first, while minimizing later web-conversion cost.

## When to Use
Use this skill when creating or revising R scripts, deciding whether to build the web app now or later, reviewing interactive scripts for API compatibility, defining workflow inputs and outputs, or refactoring console workflows.

## Strategic Decision
Do not build the full web application before the toolkit has completed and survived at least one end-to-end real research workflow.

Proceed in this order:

```text
Complete core toolkit
→ run a real end-to-end SR-MA project
→ correct workflow and edge cases
→ stabilize schemas and outputs
→ functionalize core R logic
→ add non-interactive execution
→ expose APIs
→ build the web front-end
```

## Minimum Toolkit Completion Target

```text
Project initialization
→ protocol and configuration
→ search strategy generation and validation
→ database record import
→ merge and deduplication
→ title/abstract screening
→ full-text screening
→ data extraction
→ risk of bias
→ meta-analysis
→ forest plots and PRISMA outputs
→ manuscript or report generation
```

## Rules for Current R Development

### Separate Core Logic from Interaction
Avoid placing scientific logic directly inside `readline()`, `menu()`, `cat()`, or top-level `source()` workflows.

Prefer:

```r
result <- finalize_human_decisions(
  decisions = decisions,
  responses = responses,
  project_config = project_config,
  search_terms = search_terms
)
```

A console wrapper may display menus, but the core function should not require a console.

### Use Structured Inputs and Outputs
Each stage should have explicit inputs, schema validation, a structured result object, deterministic output locations or artifact identifiers, and machine-readable errors where practical.

### Isolate UI Adapters

```text
Core function
Console adapter
Future API adapter
Future web front-end
```

The future API adapter should call the same core function without simulating keyboard input.

### Centralize Configuration
Keep topic, PICO, eligibility criteria, database selection, filenames, exclusion reasons, model settings, and workflow policies in configuration rather than hard-coded scripts.

### Preserve Auditability
Human Decisions and consequential state changes should record decision ID, options, recommendation, selected option, approval source, timestamp, affected components, applied patch, verification result, and rollback outcome.

### Design for Idempotency
Repeated execution should not silently duplicate records, overwrite irreplaceable evidence, or create inconsistent state. Archive prior state, verify outputs after writing, and restore previous state after partial failure.

## Recommended Refactoring Sequence
1. Finish and validate current workflow behavior.
2. Identify the scientific core in each interactive script.
3. Extract the core into a named function.
4. Keep the script as a thin console wrapper.
5. Add tests for valid, invalid, empty, and repeated inputs.
6. Standardize result and error objects.
7. Add a non-interactive runner.
8. Add Plumber or another API adapter after stabilization.
9. Build TypeScript screens against stable API schemas.

## Anti-Patterns
Do not rewrite the scientific toolkit in JavaScript merely to build a web interface; expose internal R paths as the public API contract; call browser prompts from scientific functions; use LLM output as the sole project state; keep approval only in console memory; start front-end development while schemas change daily; or duplicate scientific logic in R and JavaScript.

## Completion Criteria
The toolkit is web-ready when core R workflows run without `readline()` or `menu()`, console scripts are thin adapters, each stage has stable structured inputs and outputs, project state and decisions are machine-readable, errors do not require parsing console text, long-running operations can be represented as jobs, and an API can call the functions without changing scientific logic.
