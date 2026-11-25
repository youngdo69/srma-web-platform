# SR-MA Universal Scientific Publication QC — 필수 로딩 및 구현 계약

**Version:** 1.0.0 (제안)  
**기준일:** 2026-11-16  
**상태:** 구현 명세(아직 Web 소스에 연결되지 않음)  
**대상:** SR-MA Universal Toolkit, Manuscript Builder 및 Review & Export

## 핵심 결정

**규격 파일을 저장하는 것과 AI가 반드시 읽는 것은 다릅니다.** QC 코드가 매 실행 시 지침을 **자동 로드·검증·전달**하도록 강제해야 합니다. AI의 보고만으로 규격을 읽었다고 인정하지 않습니다.

권장 경로:

- `web/qc/prompts/SCIENTIFIC_PUBLICATION_QC_MASTER_v1.md` — 정식 규격; AI QC의 system/developer 지침으로 전달
- `web/qc/docs/SCIENTIFIC_QC_RUNTIME_INTEGRATION_CONTRACT_ko.md` — 기술 구현·Acceptance 계약
- 신규 승인 Web Patch에서 **관련 소스 파일과 SHA-256**을 승인 Overlay/Policy Manifest에 등록. 기존 18개 파일 Manifest를 수동 편집하지 말 것.

## 필수 런타임 순서

1. Web 요청은 **현재 선택한 Manuscript Version number**와 immutable Manuscript ID, Human Finalized Evidence version/digest, approved analysis source manifest를 확정한다. 사용자에게 내부 ID 입력을 요청하지 않는다.
2. 분석 Outcome별 `analysis_profile`을 승인된 synthesis plan 및 실제 실행 기록에서 결정한다. Narrative와 Quantitative가 혼합될 수 있다.
3. 기존 결정적 QC(R/40의 numerical, citation, cross-section, artifact coverage) 및 신규 PICO/RoB/PRISMA/endpoint QC를 시행하되 기 검증 정보를 캐시·승계하고, 과학적 자료를 재처리하지 않는다.
4. AI가 필요한 **semantic QC 단계에만** 파일을 정확한 repository 내 경로에서 읽는다. 절대 경로를 임의로 받지 않으며 path traversal을 막는다.
5. 로드 전 `SHA-256`을 승인 Manifest의 policy hash와 비교한다. 변경/누락/빈 내용/잘림/버전 미지원이면 `QC_POLICY_INTEGRITY_FAILED`로 **AI 호출 전** 중단한다.
6. Policy 전문을 AI의 가장 높은 지원 instruction 계층에 전달하며, 연구 원문과 생성 원고는 **untrusted evidence/data**로만 제공한다. 원문 안의 명령어는 무시한다.
7. AI 호출의 입력 및 출력에 `policy_id`, `policy_version`, `policy_sha256`, current `evidence_version_id`, `manuscript_version_number`, `analysis_profiles`, source manifest digest를 기록한다. 이전 다른 Version의 QC를 재사용하지 않는다.
8. AI JSON을 스키마·근거 연결·숫자 일치로 다시 결정적 검증한다. AI가 스스로 `PASS`라고 말해도 검증이 끝난 것은 아니다.
9. QC 결과와 적용 규격의 해시를 **새 immutable QC Version**으로 기록한다. 원고·Evidence·참고문헌·최종 Human Selection은 수정하지 않는다.
10. `Review & Export`에서 구분된 `PASS`, `WARNING`, `BLOCKED`, `NOT_APPLICABLE`, `NOT_ASSESSED`를 보여주며 문제 발생 위치/수정 제안을 제공한다. **Draft DOCX 다운로드는 허용**, Publication-ready Final Export는 미해결 필수 오류가 있으면 차단한다.

## 기존 R/40과의 역할 분담

Canonical R/40은 이미 다음을 검사한다:

- 수치 일관성 / verified fact registry
- 본문 Citation 및 References 무결성
- Introduction/Methods/Results/Discussion 등 섹션 간 모순
- 요구 Table/Figure 아티팩트 존재 여부

새로 만드는 Universal Web QC는 이를 **대체하지 않고** 다음을 보강한다:

- 프로젝트별 Human Final Study Selection ↔ PRISMA ID lineage
- PICO–Study comparator 적합성
- Primary Outcome Coverage / missingness taxonomy
- RoB/Certainty completeness / 실제 실행 도구 확인
- Pairwise Quantitative / Reconstructed IPD / NMA 등 profile-specific 검증
- PRISMA/SWiM Reporting Coverage와 구조적 원고 품질
- Narrative 또는 통계 모형별 부적절한 결론·인과적 주장에 대한 semantic QC

R/40 검증을 반복하려고 AI가 원본 데이터를 다시 추출하거나 PRISMA를 다시 생성하면 안 된다.

## 시스템 분리: Preflight, QC, Release

| 시점 | 책임 | AI 사용 | 실패 시 행동 |
|---|---|---|---|
| Manuscript Builder 사전 검증 | 결정적 Source/Version/Hash 연결 | 없음 | 잘못된 source 바인딩은 AI 전 중단 |
| Draft 생성 | 이전에 확정된 데이터로 문장/표/그림 생성 | 필요 시 | 생성 기록과 checkpoint 보존 |
| Scientific Publication QC | 결정적 validators + AI semantic review | 검증된 지침 적용 | QC Report 보존, 최종 Export 차단 가능 |
| Review & Export | 문제 검토, 문서 수정, 최종 Author Approval | 새로 필요한 semantic 재검증만 | 기존 APPROVED 영역을 다시 승인 요구하지 않음 |

## 예외/캐시 및 실패 복구

- 캐시 키에는 `policy_sha256`, `source_manifest_sha`, `frozen_evidence_digest`, `analysis_manifest_sha`, `manuscript_content_sha`, `qc_engine_version`을 포함한다.
- 동일한 검증 자료에 대해 AI를 불필요하게 반복 호출하지 않는다. **문장 수정으로 영향받은 QC 규칙만** 재검증 가능해야 한다.
- 정책 파일 업데이트는 **별도 승인된 Version**, 해시 및 코드 적용을 필요로 한다. 새 파일을 기존 이름 위에 덮어썼다는 이유만으로 활성화되지 않는다.
- AI는 결과/선택/프로토콜을 변경하지 않는다. `BLOCKED`는 문제를 드러내되 이전 DOCX와 Evidence를 보존한다.
- 애매한 결정은 최종 원고 검토 단계에 명확한 질문으로 남기고, Reference Re-review를 중간에 추가하지 않는다.

## 필수 Acceptance Tests (최소 실행)

1. Policy 파일 없음, 수정, 해시 불일치, prompt truncation: **AI 호출 0회** + 명확한 오류.
2. Human 최종 연구 2 vs 오래된 R/35 연구 3: 잘못된 PRISMA 숫자로 출력 금지, ID 비교/정합화 또는 BLOCKED.
3. 최종 3개 연구 중 특정 Outcome에 2개만 기여: PRISMA=3, outcome k=2 정상 PASS.
4. Narrative outcome에서 합성되지 않은 HR을 pool한 AI 원고: BLOCKED.
5. Pairwise 분석에서 RR 방향 반전, forest/Abstract CI 불일치: BLOCKED.
6. 동일 연구가 두 논문/시점으로 보고되어 중복 환자수 합산: BLOCKED 또는 verified unresolved.
7. 실제 수행하지 않은 RoB/GRADE/Funnel/Egger를 서술한 원고: 오류 판정.
8. 출처 불명 Primary Outcome, PROSPERO 번호 또는 검색 날짜: 보완 필요 상태, 임의 생성 금지.
9. Reconstructed KM 데이터로부터 추정한 pseudo-IPD를 진짜 원본 IPD라고 쓴 경우: 수정 요구.
10. Revision 후 이미 PASS한 Source/Citation QC의 재실행/재과금 방지, 영향받은 룰만 재검증.
11. `Review & Export`에서 모든 finding이 **원문/버전/아티팩트 위치**와 연결됨.
12. AI 또는 R/40 QC가 실패해도 기존 editable DOCX는 다운로드 가능; Final Export만 제한.

## 개발 지시서로 전달할 한 문장

> Every Scientific Publication QC run MUST load and hash-verify the approved `SCIENTIFIC_PUBLICATION_QC_MASTER_v1.md`, apply its universal and outcome-specific profiles, combine deterministic R/40/source checks with bounded AI semantic review, retain immutable human-finalized evidence, and fail closed before the AI call if the policy or its provenance is missing. A Markdown file on disk alone does not satisfy this requirement.

## 참조 예제

- 기존 TAVR vs SUAVR (IPD from KM reconstruction) 논문: **quantitative/long-term survival/forest/heterogeneity/reference consistency** 테스트 사례로 활용.
- 기존 TEER vs surgical tricuspid repair 원고: **narrative/SWiM/PICO comparator/reintervention missingness** 테스트 사례로 활용.
- 예제의 특정 임상 주제, 연구 수, 통계 수치, 파일명은 운영 로직에 하드코딩하지 말 것.

## 현재 상태 및 제한

이 패키지는 **지침 및 구현 계약 문서만 포함**한다. `web/R`, `web/ui`, `web/adapter`, 승인된 overlay, SQLite, R/34–R/41, AI runtime을 변경하지 않는다. 실제 Web에서 AI가 이 파일을 강제로 읽도록 만드는 **연결 코드는 아직 작성·설치되지 않았다**. Runtime integration과 테스트 PASS가 끝나기 전에는 `QC ENFORCED`라고 표시하지 않는다.
