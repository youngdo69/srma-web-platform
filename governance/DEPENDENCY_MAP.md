# Dependency Map

## High-Level Pipeline
R/11 search strategy generation
→ R/12 search validation
→ database-specific live search/download scripts
→ R/16 PubMed retrieval
→ R/17 import, merge, and deduplication
→ R/18 screening workbook
→ title/abstract screening
→ full-text screening
→ extraction
→ risk of bias
→ meta-analysis
→ reporting

## Dependency Review Rule
Before modifying a file, identify:
- Upstream producers
- Downstream consumers
- Shared helpers
- Config keys
- Expected files and schemas
- Tests and documentation affected

## Status
This map is provisional and must be updated after reviewing the actual repository tree and scripts.
