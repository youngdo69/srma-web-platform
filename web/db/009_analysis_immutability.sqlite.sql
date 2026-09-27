CREATE TRIGGER analysis_no_update BEFORE UPDATE ON analysis_versions BEGIN SELECT RAISE(ABORT,'Analysis lineage is immutable'); END;
-- NEXT
CREATE TRIGGER analysis_no_delete BEFORE DELETE ON analysis_versions BEGIN SELECT RAISE(ABORT,'Analysis lineage is immutable'); END;
