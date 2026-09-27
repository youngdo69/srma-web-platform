CREATE TRIGGER qc_no_mutation BEFORE UPDATE OR DELETE ON qc_versions FOR EACH ROW EXECUTE FUNCTION guard_audit();
-- NEXT
CREATE TRIGGER acceptance_no_mutation BEFORE UPDATE OR DELETE ON release_acceptances FOR EACH ROW EXECUTE FUNCTION guard_audit();
