CREATE TRIGGER gate_no_delete BEFORE DELETE ON gates BEGIN SELECT RAISE(ABORT,'Review gates are immutable'); END;
-- NEXT
CREATE TRIGGER version_no_update BEFORE UPDATE ON project_versions BEGIN SELECT RAISE(ABORT,'Project versions are immutable'); END;
-- NEXT
CREATE TRIGGER version_no_delete BEFORE DELETE ON project_versions BEGIN SELECT RAISE(ABORT,'Project versions are immutable'); END;
