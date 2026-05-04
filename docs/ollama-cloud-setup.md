# Ollama Cloud Setup

OAS의 Ollama backend는 로컬 (`http://127.0.0.1:11434`) 외에 [Ollama Cloud](https://docs.ollama.com/cloud) 원격 엔드포인트를 별도 변경 없이 지원한다. Cloud는 native `/api/chat` 형식을 그대로 노출하며, 로컬과 wire format이 동일하다.

## Provider kind alias

`provider_kind.ml`은 `"ollama"`, `"llama"`, `"ollama_cloud"` 세 문자열을 모두 동일한 `Ollama` variant로 매핑한다 — Cloud용 별도 kind는 없다. cascade.toml에서는 `ollama_cloud`라는 alias를 쓰면 의도가 명확해진다.

```ocaml
(* lib/llm_provider/provider_kind.ml:75 *)
| "ollama" | "llama" | "ollama_cloud" -> Some Ollama
```

Capability 테이블도 마찬가지로 alias만 분기한다.

```ocaml
(* lib/llm_provider/capabilities.ml:746 *)
| "ollama" | "ollama_cloud" -> Some ollama_capabilities
```

## 환경 변수

| 변수 | 용도 | 예시 |
|------|------|------|
| `OLLAMA_API_KEY` | Cloud Bearer 인증 토큰 | `sk-ollama-…` |

`provider_kind.api_key_env_var`는 `Ollama` variant에 대해 `None`을 반환하도록 정의되어 있다 (`provider_kind.ml:62`). 이는 *로컬 Ollama가 인증 없이 작동하기 때문에 강제하지 않는다*는 의미이며, Cloud 사용 시에는 호출 측이 `OLLAMA_API_KEY`를 명시적으로 읽어 `Provider_config.headers`에 주입해야 한다.

## Provider_config 예시

```ocaml
let cloud_cfg : Provider_config.t =
  Provider_config.make
    ~kind:Ollama
    ~base_url:"https://ollama.com"
    ~model_id:"gpt-oss:120b"
    ~headers:[
      ("Authorization",
       "Bearer " ^ Sys.getenv "OLLAMA_API_KEY");
    ]
    ()
```

`Complete.complete` 경로에서 `~headers:config.headers`로 직접 forward되므로 (`complete.ml:406, 942`) 별도의 backend 수정 없이 Bearer 헤더가 모든 요청에 동봉된다.

## 모델 ID

Cloud는 `gpt-oss:120b`, `gpt-oss:120b-cloud` 등 cloud-suffix 모델을 노출한다 ([Ollama Cloud docs](https://docs.ollama.com/cloud)). 로컬과 모델 카드가 다를 수 있으므로 cascade.toml에서는 cloud-only 모델을 별도 cascade로 분리하는 것이 안전하다.

## cascade.toml 예시

```toml
[[cascade.cloud_oss]]
provider = "ollama_cloud"
base_url = "https://ollama.com"
model_id = "gpt-oss:120b"
# headers는 코드에서 OLLAMA_API_KEY env로 주입.

[[cascade.local_dev]]
provider = "ollama"
base_url = "http://127.0.0.1:11434"
model_id = "qwen3:14b"
# 로컬은 인증 없음.
```

## 한계 및 주의

- **Usage tokens**: Ollama Cloud의 `/api/chat` 응답이 토큰 카운트를 포함하는지 공식 문서는 명시하지 않는다 — 운영 환경에서 직접 확인 필요. `ollama_capabilities.emits_usage_tokens` 기본값이 거짓이면 텍스트 전용 turn은 metric coverage 경고가 발생할 수 있다.
- **Streaming**: 로컬 Ollama와 동일하게 `stream: true` 시 NDJSON 라인 단위 응답. `complete.ml`의 stream parser가 그대로 적용된다.
- **Network failure**: cloud는 transient 네트워크 오류가 로컬보다 빈번하므로, cascade에 `Direct API fallback` (Anthropic/Gemini 등)을 함께 배치하는 것이 권장된다.

## References

- Ollama Cloud overview: https://docs.ollama.com/cloud (확인일시 2026-05-04, 신뢰도 High)
- `lib/llm_provider/provider_kind.ml:55-83`
- `lib/llm_provider/capabilities.ml:746`
- `lib/llm_provider/complete.ml:406, 942` (headers forward)
