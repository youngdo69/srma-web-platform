# B2 canonical integration audit

Baseline fb303704; selectively reviewed experimental work, see B2_SALVAGE_REVIEW.md.

- R17 merge/dedup preserves raw IDs/source and duplicate map; R18 creates two reviewer sheets.
- R21 deterministically resolves agreement and explicit conflict adjudication; R22 full-text registry distinguishes missing/invalid/available PDF.
- R24 validates explicit Author_Final_Review Include/Exclude and canonical exclusion labels.
- R25 schema is topic-neutral and provider proposals are unverified. Manual data entry will populate the same canonical fields with HUMAN provenance, not fabricated AI output. R26 validates human-approved extraction and creates analysis-ready sets WITHOUT fitting models.
- R36 accepts R26 meta-analysis-ready files/roles (no meta-analysis execution required), uses design-specific RoB questions, and supports SRMA_ROB_AGENT_RUN=false. R37 contains canonical finalization and domain validators reusable without plots.
- Study/report association requires explicit human mapping; screening_id remains canonical report identity.
- Web additions are orchestration, immutable ProjectVersion review snapshots, Gate, Job, Artifact and audit. Canonical source remains authoritative. No new dedup, statistical or RoB scoring algorithm.
