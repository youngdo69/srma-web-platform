# B2 canonical integration audit

Baseline fb303704; selectively reviewed experimental work, see B2_SALVAGE_REVIEW.md.

- R17 merge/dedup preserves raw IDs/source and duplicate map; R18 creates two reviewer sheets.
- R21 deterministically resolves agreement and explicit conflict adjudication; R22 full-text registry distinguishes missing/invalid/available PDF.
- R24 validates explicit Author_Final_Review Include/Exclude and canonical exclusion labels.
- R25 schema is topic-neutral and provider proposals are unverified. Manual data entry will populate the same canonical fields with HUMAN provenance, not fabricated AI output. R26 validates human-approved extraction and creates analysis-ready sets WITHOUT fitting models.
- R36 accepts R26 meta-analysis-ready files/roles (no meta-analysis execution required), uses design-specific RoB questions, and supports SRMA_ROB_AGENT_RUN=false. R37 contains canonical finalization and domain validators reusable without plots.
- Study/report association requires explicit human mapping; screening_id remains canonical report identity.
- Web additions are orchestration, immutable ProjectVersion review snapshots, Gate, Job, Artifact and audit. Canonical source remains authoritative. No new dedup, statistical or RoB scoring algorithm.

## Adopted canonical contracts

| Semantic operation | Canonical component | Inputs → registered outputs | Human boundary |
|---|---|---|---|
| prepare_screening | R17, R18 | exact database inputs → dedup records, duplicate map, review workbook | exact-version protocol gate |
| finalize_screening | R21, R22 | two human reviewer tables/conflict adjudication → full-text candidates/registry | screening gate; uncertainties blocked |
| register_fulltext | R22 | version-scoped uploaded PDFs → canonical registry | no eligibility approval implied |
| recommend_screening / recommend_eligibility | R19 / R23 | selected evidence + real provider → separate proposals | UNVERIFIED, never a human vote |
| finalize_included_studies | R24 | Author_Final_Review → included/excluded/final eligibility CSVs and report-study crosswalk | inclusion gate, controlled reasons |
| prepare_extraction | canonical data_extraction_schema.yml | approved included reports → blank topic-neutral study/outcome tables | source location, values and Human actor |
| recommend_extraction | R25 | approved included reports/PDFs → study/group/outcome proposals | UNVERIFIED |
| finalize_extraction | R26 | canonical targeted verification workbook → human_verified_dataset/analysis-ready manifest | independent verifier and extraction gate |
| prepare_risk_of_bias | R36, provider disabled | verified dataset/design rules → canonical questions/domains/workbook | no final judgments supplied |
| recommend_risk_of_bias | R36, real provider enabled | selected verified evidence → separate AI assessments | UNVERIFIED |
| finalize_evidence | R37 import_and_finalize_author_workbook, validate_tool_domains | explicit human workbook → finalized judgments/evidence manifest | RoB gate; no plot/model invocation |

Persistent scientific rows live inside hashed immutable ProjectVersion snapshots, with the evidence_versions lineage table extending the existing project/version/job foreign keys. Gates, workers, artifacts, identity and audit reuse accepted B1 services. Each output acceptance links to its exact source job and version. AI diagnostics/prompts/raw responses are excluded from published artifacts. Canonical source and both dependency locks are unchanged.

The local E2E explicitly uses synthetic bibliographic and signature-only PDF content. Canonical deterministic transforms are real; the development test process substitutes only the unavailable scientific-runtime environment check. Production code retains that check. Provider calls are not simulated as successful science.
