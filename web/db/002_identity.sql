CREATE TABLE external_identities (provider TEXT NOT NULL, subject TEXT NOT NULL, user_id TEXT NOT NULL REFERENCES app_users(id), PRIMARY KEY(provider,subject));
CREATE TABLE auth_sessions (token_hash TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES app_users(id), provider TEXT NOT NULL, issued_at DOUBLE PRECISION NOT NULL, expires_at DOUBLE PRECISION NOT NULL, revoked INTEGER NOT NULL DEFAULT 0 CHECK(revoked IN (0,1)));
CREATE INDEX sessions_user ON auth_sessions(user_id,expires_at);
