CREATE TRIGGER auth_audit_no_update BEFORE UPDATE ON auth_audit_events BEGIN SELECT RAISE(ABORT,'Authentication audit events are immutable'); END;
-- NEXT
CREATE TRIGGER auth_audit_no_delete BEFORE DELETE ON auth_audit_events BEGIN SELECT RAISE(ABORT,'Authentication audit events are immutable'); END;
