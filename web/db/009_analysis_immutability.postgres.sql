CREATE TRIGGER analysis_no_mutation BEFORE UPDATE OR DELETE ON analysis_versions FOR EACH ROW EXECUTE FUNCTION guard_audit();
