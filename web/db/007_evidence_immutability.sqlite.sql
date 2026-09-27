CREATE TRIGGER evidence_no_update BEFORE UPDATE ON evidence_versions BEGIN SELECT RAISE(ABORT,'Evidence lineage is immutable'); END;
-- NEXT
CREATE TRIGGER evidence_no_delete BEFORE DELETE ON evidence_versions BEGIN SELECT RAISE(ABORT,'Evidence lineage is immutable'); END;
