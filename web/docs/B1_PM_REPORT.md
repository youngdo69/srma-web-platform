# Autonomous Block B1 — PM/Reviewer report

Date: 2026-09-27. Mission: Autonomous Block B1 Master Mission v1.0.
Branch: `web/block-b1`. Approved Block A: `50f1e3b95cba13f9c8bd3ef2abf744bf98b32cc5`.
Frozen scientific baseline: `e7b98da4cfd01abbfccc86205f670209eba6ae12`.

## A. Executive status

| Item | Status | Evidence / acceptance boundary |
|---|---|---|
| PostgreSQL hardening | BLOCKED | Actual PostgreSQL 16.2 binaries installed; server initialization prohibited by root-only execution environment. No PostgreSQL test PASS claimed. |
| Web clean restore | PASS | Fresh R 4.3.3 library, 56 exact locked packages, supported setup completion, full tests and HTTP/static assets. No previous Block A packages borrowed. |
| Scientific Runtime Gate | UPDATE_READY | Original R 4.6.1 lock restored and all 79 packages load; DOCX maintenance proposal status recorded below. |
| WEB-005 | VALIDATION PENDING | Identity/workspace implementation and SQLite security tests complete. PM requires real PostgreSQL evidence for PASS. |
| WEB-006 | PASS | Authenticated project dashboard, requested navigation, actionable progress and review state; downstream functions disabled. |
| WEB-007 | PASS | Canonical validated wizard; immutable save/edit/resume, audit and pending review gates; actual Shiny handlers tested. |
| WEB-008 | FAIL | Partial implementation: manual import and actual canonical preflight PASS. Successful credentialed search QC and PubMed retrieval are not demonstrated; operations remain disabled. |
| Block B1 end-to-end workflow | FAIL | Full new-project → generated approved search → PubMed/manual import → preflight chain is not complete. Manual-import vertical path succeeds. |

B1 전체 완료나 production readiness를 선언하지 않는다. 통과한 구성 요소와 실패·미검증 영역을 분리해 인계한다. WEB-009 이후 작업, 공개 배포, main 병합 및 원격 push는 수행하지 않았다.

## B. Architecture changes

기존 Shiny → application service → persistent job → fenced worker → private semantic adapter → canonical engine → artifact registry 구조를 유지했다.

추가 사항은 provider-neutral identity/session, immutable configuration wizard, server-owned upload context, database status projection 및 별도 scientific-runtime probe이다. 과학적 검색식을 Web에서 생성하지 않는다. Canonical configuration schema와 config/path validation을 원본 코드에서 사용한다. Migration 001은 변경하지 않았고 002(identity/session), 003(gate deletion 및 version mutation 금지)을 추가했다.

Enabled semantic operations는 `validate_search`, `validate_database_inputs` 두 개다. 나머지 16개는 비활성이다. `initialize_project`는 metadata wizard와 구분되는 scientific initialization이므로 활성화하지 않았다.

## C. PostgreSQL evidence

실제 설치 binary: `initdb (PostgreSQL) 16.2 (Ubuntu 16.2-1ubuntu4)`.
실행 계정은 uid 0이며 uid mapping은 한 개만 허용된다. `initdb`는 root 실행을 거부했다. `runuser -u nobody`는 `cannot set groups: Operation not permitted`, 정상적인 `setpriv --reuid=65534 --regid=65534 --keep-groups id`도 `setresuid failed: Invalid argument`로 실패했다. 사용 가능한 Docker/Podman은 없었다. 권한 검사를 변조하거나 우회하지 않았다.

`evidence/b1/postgres-attempts.txt`에 실제 출력이 있다. **실행된 PostgreSQL server, migration 결과, PostgreSQL concurrency 결과는 없다.** 아래 테스트는 PostgreSQL용으로 준비됐지만 이 환경에서 실행되지 않았다.

```sh
SRMA_WEB_DB=postgres Rscript --vanilla web/tests/postgres.R
SRMA_WEB_DB=postgres python3 web/tests/vertical.py
SRMA_WEB_DB=postgres Rscript --vanilla web/tests/b1.R
SRMA_WEB_DB=postgres Rscript --vanilla web/tests/b1_security.R
```

각 테스트군에 별도의 비어 있는 disposable DB와 PGHOST/PGPORT/PGDATABASE/PGUSER를 제공해야 한다. Schema/checksum/foreign-key/state/audit/gate/unique-job/resume/isolation 및 두 워커 경쟁 검증 결과는 현재 **SQLite 결과만** 존재한다. WEB-005는 `IMPLEMENTED — VALIDATION PENDING`이다.

## D. Runtime/dependency evidence

| Environment | Actual result |
|---|---|
| Web R 4.3.3 | `web/renv.lock`의 56개 패키지를 fresh library에 설치하고 namespace/version/path 확인. 별도 fresh renv 1.2.4 bootstrap. |
| Scientific R 4.6.1 | Root lock의 79개 패키지 restore 및 exact-version namespace load 확인. Scientific library는 Web library와 분리. |
| Original canonical files | Root `renv.lock`, R 및 config 원본 미수정. 98-file integrity / 78 R-file parse 검증 PASS. |

첫 설치 시도에서 끝내지 않았다. Web R 4.6.1과 구버전 lock 패키지의 C API 불일치를 확인하고 lock에 맞는 R 4.3.3으로 분리했다. libpq 개발 헤더/pg_config, XML/ICU 등의 로컬 SDK 및 archive dependency order 문제를 수정했다. 정확한 버전의 다운로드 archive로 누락 패키지를 fresh library에 설치한 후 지원 setup/restore를 다시 실행해 동기화를 확인했다. 따라서 한 번의 무수정 setup 명령이 처음부터 성공했다는 주장은 하지 않는다.

Web 성공 증거: `web-clean-restore-verified.log`, `web-clean-runtime.json`, `clean-suite.log`. Scientific 원본 lock 증거: `scientific-runtime-original-lock.json`. OS package manager가 project-local SDK를 인식하지 못한 경고는 남아 있으나 실제 compile/load 결과를 별도로 확인했다.

**ENGINE_DEPENDENCY_UPDATE_READY**: 원본 `UPDATE_RENV_DOCX_DEPENDENCIES_ONCE.R`를 isolated copy에서 실행해 officer 0.7.6 / flextable 0.10.1 설치 및 snapshot을 완료했다. 원본 root lock은 변경하지 않았다.

유틸리티의 raw snapshot은 27개 패키지를 추가하면서 `meta`, `lme4` 등 기존 12개를 제거했다. 이 raw 결과는 채택하면 안 된다. 정확한 raw lock/diff와 package-diff.json을 보존했다. 별도의 권고안 `ADDITIVE_PM_REVIEW_renv.lock`은 기존 79개 package record와 R/repository metadata를 그대로 유지하고 새 27개만 추가한다(총 106개, 기존 version 변경/삭제 0).

권고안의 106개 exact-version namespace, R/41의 9개 필수 의존성, 실제 DOCX 생성 및 native editable Word table XML을 검증했다. Isolated copy에 권고안을 놓고 원본 `START_HERE.R`의 environment-only 모드도 실행해 restore synchronization 및 13 direct workflow dependencies PASS를 확인했다. 이것은 dependency 검증이며 R/41 manuscript workflow나 clinical regression 완료가 아니다. 제안서와 두 diff는 `evidence/b1/engine-dependency-proposal/`에 있다.

## E. Identity/session implementation

`web_identity_provider(name, verify)`는 서버에서 설치하는 provider interface이다. 실제 OIDC login 구현 완료를 의미하지 않는다. 향후 verifier가 issuer/signature/JWKS/audience/expiry/state/nonce/PKCE를 검증한 후 provider-scoped subject를 반환해야 한다.

외부 subject는 명시적으로 provision한 user와 연결하고 email 기반 자동 병합이나 workspace 자동 가입을 하지 않는다. 256-bit OS random credential은 SHA-256 hash로만 DB에 보관한다. 최대 1시간 만료, 매 요청 session 재확인 및 logout revoke를 구현했다. Browser actor/user_id override를 거부한다. Mutation은 Shiny connection nonce를 확인한다. URL token/ambient auth cookie는 사용하지 않는다.

Local identity는 `SRMA_WEB_TEST_IDP=1`일 때만 발급·검증되며 자동화 테스트 전용이다. 앱의 고정 actor fallback을 제거했다. Production OIDC/provider selection/TLS/security certification은 미완료다.

## F. Project wizard evidence

Title, identifier, question, PICO/PICOS, primary/secondary outcomes, study designs, inclusion/exclusion, language, enabled databases, inception/start/end dates, optional investigator를 지원한다. 주제·질환·중재의 과학적 기본값을 주입하지 않는다. 사용자가 입력한 과학적 판단을 그대로 canonical schema에 저장한다.

Canonical validator를 통과한 config만 immutable ProjectVersion으로 저장한다. 매 save/edit는 새 checksum snapshot과 audit를 생성하고 configuration review/protocol registration 요구를 PENDING으로 추가한다. 원래 버전과 승인 상태는 덮어쓰지 않는다. Search concept가 없는 draft는 빈 concept로 남으며 검색 준비 완료로 표시하지 않는다.

`b1.R`, `b1_ui.R`에서 실제 save/edit/resume, 중복 identifier, 날짜/경로 오류, workspace 격리, 2개 버전과 8개 gate를 검증했다. 추가 재검증으로 scalar PICO/boolean database 형식의 기존 프로젝트도 edit/save가 가능함을 확인했다.

## G. Search/import evidence

| Operation / database | Actually executed | Not demonstrated |
|---|---|---|
| `validate_search` — enabled | 실제 canonical R/12, persistent job, 독립 worker, 2개 checksummed report 및 다운로드 | 임상 검색 품질 승인이나 database 검색 아님 |
| `validate_database_inputs` — enabled | 별도 R 4.6.1의 원본 TASK-018, 제출/worker 양쪽 runtime gate, 실제 CSV/report 생성 및 등록 | Merge/dedup/screening 실행 없음 |
| Embase — manual | Synthetic RIS 업로드 → immutable version → canonical parser/preflight | 실제 Embase 접속·검색·export는 미실행 |
| CENTRAL — manual | 미업로드 시 USER_ACTION_REQUIRED; 업로드 후 Embase와 함께 READY/COMPLETE | 실제 CENTRAL 접속·검색·export는 미실행 |
| `generate_search` — disabled | 실제 R/11을 synthetic concept와 credential-free 조건에서 실행; `REVIEW_REQUIRED`를 service result `USER_ACTION_REQUIRED`로 보존 | Credentialed LLM QC PASS, 완전한 wizard/upstream approval integration |
| `retrieve_pubmed` — disabled | 실제 R/16이 위 미승인 검색을 EFetch 전에 차단함을 확인; 다운로드 0건 | 성공적인 PubMed raw-record retrieval 및 artifact registration |

Synthetic RIS는 parser fixture이고 실제 논문·임상 관측값이 아니다. Engine 호출이나 parser를 mock하지 않았다. Shiny upload-context 테스트의 websocket만 test double을 사용했다. 별도 negative-path 테스트는 API credential을 의도적으로 제거하며, 실행 당시 operator environment에도 해당 credential이 없었다. Private key 값이나 `.env`를 결과물에 포함하지 않았다.

UI는 저장된 canonical strategy를 표시·복사·다운로드하고 canonical registry의 DB별 export 지침을 보여준다. Manual upload는 extension, 20 MB 제한, 실제 크기, filename, symlink/경로 격리, 프로젝트·버전 연결, SHA-256을 검증한다. 업로드 성공과 canonical format validation 성공을 구분한다. 실제 preflight report에서 completed/missing/status/count/next-action을 표시한다.

## H. Human Gate preservation

Protocol registration/configuration approval 요구를 draft와 함께 생성했다. 삭제 및 immutable decision/version 변경을 DB에서 거부한다. 새 버전은 이전 승인을 자동 승계하지 않는다. Automated reports는 `UNREVIEWED`로 등록한다.

실제 preflight 성공 후에도 모든 해당 gate가 PENDING이고 실행 job은 preflight뿐임을 확인했다. Canonical R/16의 exact-query hash 및 search-quality PASS 검사를 변경하지 않았다. 검색 품질 승인·protocol registration·R/40 PASS를 만들어내지 않았고 R/41 workflow도 실행하지 않았다.

## I. Security tests

실행 검증: unauthenticated/invalid/expired/revoked sessions, actor override, cross-workspace/project/version/job/artifact 접근, unauthorized download, CSRF mismatch, malformed config, traversal filename, out-of-root file, symlink, unsupported extension, empty/oversized/size-mismatch upload, private argument injection, sanitized UI errors, gate/version immutability.

2개 workspace의 격리를 실제 서비스와 SQLite persistence에서 확인했다. Security certification/production penetration test 또는 PostgreSQL 보안 검증 완료를 주장하지 않는다. WEB-015 범위를 앞당기지 않았다.

## J. Tests actually run

명령의 workspace-specific prefix와 전체 test command/exit code는 `clean-test-summary.json`에 있다. 실행 공통 환경은 다음과 같다.

```sh
export SRMA_WEB_LIBRARY=/workspace/scratch/f22ae3d8c5eb/runtime/web-clean433
export SRMA_RSCRIPT=/workspace/scratch/f22ae3d8c5eb/runtime/Rscript-clean433
export SRMA_SCIENTIFIC_RSCRIPT=/workspace/scratch/f22ae3d8c5eb/runtime/Rscript461
export SRMA_SCIENTIFIC_LIBRARY=/workspace/scratch/f22ae3d8c5eb/runtime/scientific-b1/renv/library/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu
```

UI의 missing-runtime 테스트는 scientific 변수를 unset한 별도 프로세스에서 실행했다. 아래 각 R test는 `$SRMA_RSCRIPT --vanilla web/tests/<name>.R`로 실행했다. `clean-b1_ui-final.log`, `clean-b1_search_guard.log`, `clean-b1_preflight-post-maintenance.log`는 최초 clean suite 이후의 추가 검증이다.

| Test | Actual result |
|---|---|
| clean_runtime | PASS: exact 56 packages, fresh library only |
| baseline | PASS: 98 source hashes, 78 canonical R parses, canonical search validator smoke |
| foundation, contract, state | PASS: interface/schema/runtime restrictions and job-state contract |
| run | PASS: 27 integrated test groups |
| b1 | PASS: identity, configuration, version/gate rules |
| b1_import | PASS: real canonical DB metadata, manual uploads, rejection cases |
| b1_security | PASS: 2-workspace access isolation, CSRF, actual Shiny upload context |
| b1_ui | PASS: actual authenticated Shiny handlers; final legacy-config regression also PASS |
| b1_preflight | PASS: missing CENTRAL → action required; both manual RIS → complete, 2 artifacts, gates retained |
| b1_search_guard | PASS: actual R/11 review-required; R/16 blocked before EFetch |
| `python3 web/tests/vertical.py` | PASS: Shiny HTTP 200, 8 JS/CSS assets, app closed, 2 workers, exactly 1 attempt, 2 checksummed downloads |

Exact successful Web setup command:

```sh
PG_CONFIG=/workspace/scratch/f22ae3d8c5eb/runtime/pg_config \
TAR_OPTIONS=--no-same-owner \
SRMA_WEB_LIBRARY=/workspace/scratch/f22ae3d8c5eb/runtime/web-clean433 \
SRMA_RENV_BOOTSTRAP=/workspace/scratch/f22ae3d8c5eb/runtime/renv-bootstrap433 \
RENV_CONFIG_CACHE_ENABLED=FALSE \
/workspace/scratch/f22ae3d8c5eb/runtime/Rscript-clean433 --vanilla web/bin/setup.R
```

DOCX 검증 명령(exit 0):

```sh
/workspace/scratch/f22ae3d8c5eb/runtime/Rscript461 --vanilla \
  web/tests/scientific_docx.R \
  /workspace/scratch/f22ae3d8c5eb/source/srma-agent-toolkit \
  /workspace/scratch/f22ae3d8c5eb/runtime/scientific-b1/renv/library/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu \
  web/evidence/b1/engine-dependency-proposal/ADDITIVE_PM_REVIEW_renv.lock
```

Maintenance command는 workspace root에서 `PATH=/workspace/scratch/f22ae3d8c5eb/runtime/bin:$PATH CPATH=/workspace/scratch/f22ae3d8c5eb/runtime/root/usr/include LIBRARY_PATH=/workspace/scratch/f22ae3d8c5eb/runtime/root/usr/lib/x86_64-linux-gnu TAR_OPTIONS=--no-same-owner RENV_CONFIG_INSTALL_TRANSACTIONAL=FALSE RENV_CONFIG_CACHE_ENABLED=FALSE runtime/Rscript461 --vanilla runtime/run-docx-maintenance.R`로 실행했다(exit 0).

권고안을 isolated copy의 lock에만 적용한 후 `TAR_OPTIONS=--no-same-owner RENV_CONFIG_CACHE_ENABLED=FALSE runtime/Rscript461 --vanilla runtime/verify-start-here.R`를 실행했다(exit 0). 이 script는 해당 copy에서 `options(srma.start_here.environment_only=TRUE); source("START_HERE.R")`를 호출한다. 실제 실행 recipe는 `evidence/b1/runtime-recipes/`에 보존했다. 추가 의존성 설치 후 `b1_preflight.R`도 다시 실행해 동일한 실제 canonical preflight 경로를 확인했다.

HTTP/static-resource tests와 Shiny testServer 검증을 실제 browser-click automation으로 표현하지 않는다. `*-development.log`는 이전 개발 runtime 증거이며 clean proof에는 `clean-*` 및 runtime JSON을 사용한다. 실패한 설치 로그도 성공 증거와 구분해 보존했다.

## K. Git checkpoints

| Commit | Checkpoint |
|---|---|
| `50f1e3b` | PM-approved Block A |
| `086317b` | B1 identity/session, dashboard, wizard, isolated imports |
| `d0dff24` | Immutable records, authenticated UI, runtime checks |
| `407af7f` | Isolated runtime validation, guarded canonical preflight, search-quality result handling |

추가 validation/report commit은 delivery의 `GIT_CHECKPOINTS.txt`에 기록한다. ZIP은 frozen 파일의 원래 bytes를 보존한 source snapshot이며 `.git`, runtime binaries, secrets, 운영 데이터는 제외한다. PM-approved Block A를 prerequisite로 하는 incremental Git bundle과 Block A 기준 patch를 포함해 B1 이력을 복원할 수 있게 한다. main merge/push/deployment 없음.

## L. Self-review findings and fixes

실제 검사에서 발견하고 수정한 사항:

- 상대 경로가 아직 존재하지 않을 때 containment 검사가 실패하던 문제: absolute resolution 및 existing ancestor 검사를 유지하며 수정.
- Session-specific Shiny upload 위치가 맞지 않던 문제: pinned Shiny upload context를 서버 소유 root에 연결하고 실제 file write 검증.
- Preflight pause 결과의 보고서가 artifact로 등록되지 않던 문제: USER_ACTION_REQUIRED에서도 검증된 report를 등록.
- Runtime 분리 없이 새로운 operation을 실행할 위험: submission과 worker 양쪽에서 full original scientific lock 검증.
- Legacy scalar PICO/boolean database edit 오류: schema-compatible writer와 UI 읽기 처리 수정, 실제 edit/save 재검증.
- R/11 fallback을 COMPLETE로 오해할 가능성: canonical quality status를 읽어 USER_ACTION_REQUIRED로 보존, R/16 guard 실행으로 재검증.
- 오래된 README의 단일 operation/restore 미검증 설명을 B1의 실제 상태로 갱신.

## M. Remaining limitations

PostgreSQL 실서버 검증은 권한 제약으로 미실행이다. Production OIDC는 구성되지 않았다. Session randomness 구현은 Linux `/dev/urandom`을 사용하며 Windows Web hosting은 검증하지 않았다. 기존 canonical toolkit의 Windows 동작 변경은 없다.

Wizard는 metadata draft를 만든다. AI-assisted initialization, search concept preparation와 upstream human approval의 완전한 Web 흐름은 미완료다. Credentialed canonical QC 및 실제 PubMed 수집의 성공 경로를 검증하지 않아 generation/retrieval을 활성화하지 않았다. 이 부분은 단순 설정만으로 완료되는 것으로 보고하지 않는다.

외부 Embase/CENTRAL search/export는 사용자 작업이다. S3, public deployment, production auth, final security certification, WEB-009 이후 기능은 범위 밖이다. Scratch runtime 경로는 이번 검증 환경의 경로이며 다른 환경에서는 별도로 R/system dependencies를 설치해야 한다.

## N. Human decisions required

1. PM이 별도 scientific dependency proposal을 검토하고 canonical baseline 변경 여부를 결정해야 한다. 원본 lock은 보존했다.
2. 정상 unprivileged PostgreSQL 실행이 가능한 검증 환경을 제공해야 한다. 새로운 유료 인프라는 요구하지 않는다.
3. 검색 QC 성공 경로에는 canonical provider용 private credential의 안전한 operator-side 구성이 필요하다. 채팅에 key를 붙여 넣지 않아도 된다. 실제 연구의 protocol/configuration/search 관련 human approval은 연구자가 수행해야 한다.

이외의 남은 Web integration은 구현 과제로 명시하며 승인 문제로 돌리지 않는다. 중단 근거는 실환경 PostgreSQL 제한과 credentialed scientific path 검증 제한이고, B1 전체가 완료됐다는 의미가 아니다.

## O. Recommendation

**Block B2 (WEB-009–010) 진입을 권고하지 않는다.** WEB-006/007과 manual-import/preflight 부분은 리뷰 가능하다. 먼저 PostgreSQL acceptance evidence, scientific dependency proposal 결정, credentialed search generation/PubMed retrieval 및 upstream Web integration을 완료하고 B1 end-to-end를 다시 평가해야 한다.

`PROJECT_STATUS.md`를 현재 통과·차단·미완료 상태로 갱신했다. Canonical scientific engine과 Human Gates는 보존했다.
