# Development Workflow

## Standard Flow
1. PM defines scope and priority.
2. Architect reviews relevant files and writes a plan.
3. Jason approves the plan.
4. Coder implements only the approved scope.
5. Reviewer checks architecture, compatibility, and tests.
6. Coder corrects findings when required.
7. Reviewer approves.
8. Integration test is completed.
9. Git commit, changelog, and dashboard are updated.

## Rules
- One role per chat.
- Claude may serve as Architect or Reviewer.
- Codex serves as Coder unless otherwise assigned.
- Agents communicate through project files, not assumed chat memory.
- No significant code change without an approved task and plan.
- Emergency fixes must still receive retrospective review and documentation.

## Decision Outcomes
- APPROVED
- APPROVED WITH MINOR CHANGES
- CHANGES REQUIRED
- REJECTED
