# Universal SR-MA Agent Toolkit — Project Constitution

## Mission

Build a reusable, topic-independent, reproducible toolkit for systematic reviews and meta-analyses.

## Core Principles

1. Preserve the existing working workflow.
2. Keep reusable code topic-agnostic.
3. Store topic-specific values in `config/project_config.yml`.
4. Review upstream and downstream dependencies before changing code.
5. Preserve filenames, schemas, and interfaces unless a migration plan is approved.
6. Prefer small, reversible changes over large refactors.
7. Test every meaningful change.
8. Keep human approval at key scientific and release decisions.
9. Record important architectural decisions.
10. Do not implement optional features before core workflow stability.

## Authority

When documents conflict, use this order:

1. PROJECT_CONSTITUTION.md
2. Approved decision in DECISIONS.md
3. ARCHITECTURE.md and DATA_CONTRACTS.md
4. Approved task plan
5. Implementation details
