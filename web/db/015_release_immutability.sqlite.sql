CREATE TRIGGER release_package_no_update BEFORE UPDATE ON release_packages BEGIN SELECT RAISE(ABORT,'Release packages are immutable'); END;
-- NEXT
CREATE TRIGGER release_package_no_delete BEFORE DELETE ON release_packages BEGIN SELECT RAISE(ABORT,'Release packages are immutable'); END;
