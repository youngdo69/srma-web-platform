CREATE TRIGGER evidence_no_mutation BEFORE UPDATE OR DELETE ON evidence_versions FOR EACH ROW EXECUTE FUNCTION guard_audit();
