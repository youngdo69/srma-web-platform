CREATE TRIGGER qc_no_update BEFORE UPDATE ON qc_versions BEGIN SELECT RAISE(ABORT,'QC decisions are immutable'); END;
-- NEXT
CREATE TRIGGER qc_no_delete BEFORE DELETE ON qc_versions BEGIN SELECT RAISE(ABORT,'QC decisions are immutable'); END;
-- NEXT
CREATE TRIGGER acceptance_no_update BEFORE UPDATE ON release_acceptances BEGIN SELECT RAISE(ABORT,'Acceptance evidence is immutable'); END;
-- NEXT
CREATE TRIGGER acceptance_no_delete BEFORE DELETE ON release_acceptances BEGIN SELECT RAISE(ABORT,'Acceptance evidence is immutable'); END;
