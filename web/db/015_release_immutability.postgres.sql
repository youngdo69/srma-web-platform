CREATE FUNCTION guard_release_package_immutable() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'Release packages are immutable'; END $$;
-- NEXT
CREATE TRIGGER release_package_no_update BEFORE UPDATE OR DELETE ON release_packages FOR EACH ROW EXECUTE FUNCTION guard_release_package_immutable();
