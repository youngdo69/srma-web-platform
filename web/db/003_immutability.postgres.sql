CREATE FUNCTION guard_immutable_review_record() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'Persistent review records are immutable'; END $$;
-- NEXT
CREATE TRIGGER gate_no_delete BEFORE DELETE ON gates FOR EACH ROW EXECUTE FUNCTION guard_immutable_review_record();
-- NEXT
CREATE TRIGGER version_immutable BEFORE UPDATE OR DELETE ON project_versions FOR EACH ROW EXECUTE FUNCTION guard_immutable_review_record();
