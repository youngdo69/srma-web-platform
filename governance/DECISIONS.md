# Architecture and Project Decisions

## Decision Record Template

### DEC-XXX — <Title>
- Date:
- Status: Proposed / Approved / Superseded
- Context:
- Decision:
- Reason:
- Consequences:
- Related tasks:

---

### DEC-001 — Preserve Existing Root Structure
- Date: 2026-08-04
- Status: Approved
- Context: Existing scripts depend on the current project layout.
- Decision: Keep existing `R/`, `scripts/`, `data/`, `templates/`, `skills/`, and config locations. Add governance folders at the project root.
- Reason: Avoid unnecessary path breakage and large migration risk.
- Consequences: Governance is added as a new layer without moving working code.

### DEC-002 — Use Role-Separated Agent Workflow
- Date: 2026-08-04
- Status: Approved
- Decision: Use separate PM, Architect, Coder, and Reviewer roles. One role should remain fixed per chat.
- Consequences: Work is transferred through task, plan, implementation, and review files.

### DEC-003 — Prioritize Current SR-MA Completion
- Date: 2026-08-04
- Status: Approved
- Decision: Generalize incrementally while completing the current SGLT2i–cardiac surgery SR-MA.
- Consequences: Large refactors and front-end development are deferred.
