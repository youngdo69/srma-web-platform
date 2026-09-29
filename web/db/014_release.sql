CREATE TABLE release_packages (
  id TEXT PRIMARY KEY,
  project_id TEXT NOT NULL REFERENCES projects(id),
  version_id TEXT NOT NULL,
  qc_id TEXT NOT NULL REFERENCES qc_versions(id),
  manifest_sha256 TEXT NOT NULL,
  package_checksum TEXT NOT NULL,
  storage_locator TEXT NOT NULL,
  eligibility_json TEXT NOT NULL,
  actor_id TEXT NOT NULL REFERENCES app_users(id),
  created_at TEXT NOT NULL,
  UNIQUE(project_id,qc_id,manifest_sha256),
  FOREIGN KEY(version_id,project_id) REFERENCES project_versions(id,project_id)
);
