CREATE TRIGGER version_origins_immutable BEFORE UPDATE OR DELETE ON version_origins FOR EACH ROW EXECUTE FUNCTION guard_immutable_review_record();
-- NEXT
CREATE TRIGGER job_sources_immutable BEFORE UPDATE OR DELETE ON job_sources FOR EACH ROW EXECUTE FUNCTION guard_immutable_review_record();
