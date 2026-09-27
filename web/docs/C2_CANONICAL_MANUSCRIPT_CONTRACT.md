# WEB-012 canonical manuscript contract

Baseline: C1 045ce28c954a5804f17f60f0f6d9c9958fa6100b. Audited before implementation.

| Canonical component | Inputs and outputs | Web use |
|---|---|---|
| R/38 prepare manuscript evidence package, v2.1.3 | Config; R27/R33, PRISMA R35, finalized RoB R37; frozen evidence/artifact registries, hashes, factual blocks and scaffold | Reuse formatting/factual-table helpers. Freeze exact approved B2/C1 registries and artifacts, never discover another job's files. Missing PRISMA/registration facts remain unavailable, never computed from manuscript rows. |
| R/39 draft with verified literature, v2.4.0 | Frozen evidence plus PubMed ESearch/EFetch; verified selected library, PMID token prose, numbered Vancouver references | Reuse PubMed fetch/parser, stable `[[PMID:...]]`, first-appearance numbering and Vancouver formatter. Human selects PMID set and approves verified records. Reuse bounded Responses API function for optional single-section drafting. |
| R/40 consistency validation, v1.6.0 | R38 facts, R39 manuscript/library; deterministic numerical/citation audits plus credentialed semantic/reverification gates | Reuse number tokenizer, DOI normalization and citation extraction. WEB-012 requires protected source tokens; free numeric claims fail closed. Do not run or counterfeit full R40 Final QC or submission-candidate PASS. |
| R/41 author-editable Word | R40 PASS candidate plus frozen artifacts; editable paragraphs/tables and figures | Reuse canonical Markdown parser, paragraph insertion and editable table functions for a journal-neutral author-review draft. No invented R40 PASS state. Full CLI final-publication path remains for WEB-013 acceptance. |

Canonical required packages: yaml/readr/dplyr/tibble/stringr/openxlsx/digest; R39/40 additionally jsonlite/httr2/xml2; R41 officer/flextable and image renderers as needed. Exact approved runtime remains externally gated; locks are not modified.

## Authority and lifecycle

ProjectVersion remains the snapshot authority. ManuscriptVersion adds immutable PLAN/DRAFT/HUMAN_EDITED/REVIEW_REQUIRED/APPROVED lineage, exact EvidenceVersion, approved analysis IDs, source/reference/specification hashes, Job, EngineRelease and actor/time. Sections preserve draft and edit history. No approval inheritance: plan gate belongs to a fixed source/specification; section edits retain that explicit upstream plan reference but never manuscript approval. A new plan needs a new gate.

The source JSON freezes project/protocol/search metadata, B2 Human evidence and file hashes, selected C1 result objects, and registered artifacts. Canonical number values are copied without recomputation. R38 formatting renders values deterministically. Protected `{{FACT:key}}` references are a Web editing guard absent from the canonical CLI; canonical PMID tokens are retained rather than duplicated. Standalone typed numeric claims must be converted to a source token; no silent rewriting of author prose. System Results/Methods blocks retain source roles and warnings and remain protected.

Reference verification accepts PMIDs only, never user-supplied bibliographic metadata/verified flags. Production retrieval uses canonical PubMed EFetch/parser. Human approval binds exact returned library hash. Defaults for minimum total/Introduction/Discussion references remain R39's 25/8/15 unless canonical project config explicitly overrides them.

Jobs render fixed immutable snapshots. Acceptance checks exact job/version/source and latest manuscript lineage so an old async job cannot replace a later Human edit. Every source Artifact is authorized and checksum-verified. Historical approvals remain readable, but later evidence, analysis or material reference-set revisions make them non-current. Manuscript pre-check is explicitly not Final QC; APPROVED means author reviewed, never FINAL_RELEASED.

Manual section entry remains available without AI. Optional AI receives only section-relevant source material; no hidden reasoning or raw response is published. Failed credentials/dependencies are actionable and preserve manual work. Authentication, CSRF and request allowlists remain the public boundary.
