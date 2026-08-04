# Architecture

## Current Principle
Preserve the existing root structure. Do not move `R/`, `scripts/`, `data/`, `templates/`, `skills/`, or configuration files without an approved migration task.

## Main Layers
1. Governance: project rules, plans, decisions, and reviews
2. Configuration: topic- and project-specific settings
3. Workflow scripts: search through manuscript outputs
4. Data: raw, interim, processed, and output artifacts
5. Validation and tests
6. Agent instructions

## Core Workflow
Search strategy generation
→ validation
→ database retrieval
→ import/merge/deduplication
→ title/abstract screening
→ full-text screening
→ extraction
→ risk of bias
→ meta-analysis
→ reporting

## Architectural Rules
- Config supplies variable project values.
- Scripts use documented input/output contracts.
- Shared logic should be reusable and centralized.
- Human decisions must remain auditable.
- Generated artifacts must be reproducible from source data and configuration.
