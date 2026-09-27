CREATE TRIGGER manuscript_no_update BEFORE UPDATE ON manuscript_versions BEGIN SELECT RAISE(ABORT,'Manuscript lineage is immutable'); END;
-- NEXT
CREATE TRIGGER manuscript_no_delete BEFORE DELETE ON manuscript_versions BEGIN SELECT RAISE(ABORT,'Manuscript lineage is immutable'); END;
