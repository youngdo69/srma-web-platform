# Initialization delivery fix

Root cause: the provider returned the complete structured response, but
R/00_run_project_initialization_agent.R rebuilt each human-decision object
using only four legacy fields and overwrote the complete response before saving.

Changes:
- preserve parsed nested JSON with simplifyVector = FALSE;
- preserve the entire human_decisions_required objects;
- fail fast if schema-v3 fields are missing;
- save the schema actually sent, exact model output, and exact API response;
- keep the existing screening-provider interface backward compatible.
