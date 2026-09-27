CREATE TRIGGER version_origins_no_update BEFORE UPDATE ON version_origins BEGIN SELECT RAISE(ABORT,'Workflow provenance is immutable'); END;
-- NEXT
CREATE TRIGGER version_origins_no_delete BEFORE DELETE ON version_origins BEGIN SELECT RAISE(ABORT,'Workflow provenance is immutable'); END;
-- NEXT
CREATE TRIGGER job_sources_no_update BEFORE UPDATE ON job_sources BEGIN SELECT RAISE(ABORT,'Workflow provenance is immutable'); END;
-- NEXT
CREATE TRIGGER job_sources_no_delete BEFORE DELETE ON job_sources BEGIN SELECT RAISE(ABORT,'Workflow provenance is immutable'); END;
