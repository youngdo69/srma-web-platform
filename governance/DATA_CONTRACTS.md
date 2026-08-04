# Data Contracts

## Purpose
Document stable inputs, outputs, filenames, sheets, columns, and data types used between scripts.

## Contract Template

### Component
`<script or module>`

### Input
- Path:
- Format:
- Required columns:
- Optional columns:

### Output
- Path:
- Format:
- Sheets or tables:
- Required columns:

### Consumers
- 

### Stability
- Stable / provisional

### Change Rule
Any schema, filename, or path change requires:
1. impact analysis,
2. downstream review,
3. migration or compatibility handling,
4. tests.

## Immediate Priority
Document contracts for:
- R/16 PubMed retrieval
- R/17 import, merge, and deduplication
- R/18 screening workbook
