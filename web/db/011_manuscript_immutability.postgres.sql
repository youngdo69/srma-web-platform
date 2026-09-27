CREATE TRIGGER manuscript_no_mutation BEFORE UPDATE OR DELETE ON manuscript_versions FOR EACH ROW EXECUTE FUNCTION guard_audit();
