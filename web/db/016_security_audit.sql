CREATE TABLE auth_audit_events (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL REFERENCES app_users(id),
  provider TEXT NOT NULL,
  event_type TEXT NOT NULL,
  detail_json TEXT NOT NULL,
  created_at TEXT NOT NULL
);
CREATE INDEX auth_audit_user_time ON auth_audit_events(user_id,created_at);
