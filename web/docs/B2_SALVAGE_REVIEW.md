# B2 v1.1 starting and salvage audit

New isolated checkout srma-b2-v11, branch web/block-b2, initial HEAD fb303704d7a7412eedf2d8736bd0de7886d1f8ef; clean before adoption. All B1 implementation bytes match the earlier accepted 2bbe205; intervening changes are status/evidence only. B1 targeted logs/tests available. Original Block A, accepted B1 and experimental B2 checkouts protected.

Reviewed exact diff against 2bbe205 and uncommitted additions. Selectively ported these units, not commits/history wholesale:
- WEB-009/010 service: version snapshot packet, immutable lineage migration, request allowlists, Human Gates; extends existing ProjectVersion/Job/Artifact/Audit.
- Canonical adapters R17/18/21/22/24/26/36/37: reuse actual transforms, no dedup/scoring/model recreation.
- Topic-neutral schema-driven extraction, report-study map and two reviewer controls.
- Shiny evidence panel and synthetic canonical E2E test.
- Pending draft improvements: downstream invalidation, multiple outcome rows, canonical AI boundary. Adopted for targeted review/testing, not assumed correct.

Rejected old roadmap/status, historical test logs as current proof, old Git baseline, and wholesale merging. Current fresh tests must validate all adopted units. Known review follow-ups: stale review invalidation, value provenance, provider metadata/output isolation, UI state, PDF provenance and security attacks.
