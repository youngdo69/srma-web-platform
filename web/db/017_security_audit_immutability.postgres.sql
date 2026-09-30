CREATE FUNCTION guard_auth_audit_immutable() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'Authentication audit events are immutable'; END $$;
-- NEXT
CREATE TRIGGER auth_audit_no_change BEFORE UPDATE OR DELETE ON auth_audit_events FOR EACH ROW EXECUTE FUNCTION guard_auth_audit_immutable();
