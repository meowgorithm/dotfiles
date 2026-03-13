---
name: crush-config
description: Configure Crush settings including providers, LSPs, MCPs, skills, permissions, and behavior options. Use when the user needs help with crush.json configuration, setting up providers, configuring LSPs, adding MCP servers, or changing Crush behavior.
---

# Crush Configuration

JSON config files, highest priority first:

1. `.crush.json` (project-local, hidden)
2. `crush.json` (project-local)
3. `$HOME/.config/crush/crush.json` (global)

Add `"$schema": "https://charm.land/crush.json"` for IDE autocomplete.

## Options

All under the `options` key:

| Key                            | Type     | Default       | Description                                                                                                                                                 |
| ------------------------------ | -------- | ------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `context_paths`                | string[] | —             | Extra files injected into AI system prompt. Merged with built-in defaults (`CRUSH.md`, `AGENTS.md`, `.cursorrules`, etc.). Paths ending `/` are directories |
| `skills_paths`                 | string[] | —             | Directories containing skills (folders with `SKILL.md`)                                                                                                     |
| `auto_lsp`                     | bool     | `true`        | Auto-add LSP tools based on detected root markers                                                                                                           |
| `data_directory`               | string   | `".crush"`    | Where Crush stores sessions, logs, commands, temp files                                                                                                     |
| `initialize_as`                | string   | `"AGENTS.md"` | Filename for context file generated on first project launch                                                                                                 |
| `disable_auto_summarize`       | bool     | `false`       | Prevent auto-summarization when approaching context window limit                                                                                            |
| `disable_default_providers`    | bool     | `false`       | Ignore built-in providers; you must fully define all providers                                                                                              |
| `disable_provider_auto_update` | bool     | `false`       | Use bundled provider list instead of fetching on startup                                                                                                    |
| `disable_metrics`              | bool     | `false`       | Disable PostHog analytics                                                                                                                                   |
| `progress`                     | bool     | `true`        | Show terminal progress bar while agent works                                                                                                                |
| `disabled_tools`               | string[] | —             | Hide built-in tools from the agent (e.g. `["bash", "sourcegraph"]`)                                                                                         |
| `debug`                        | bool     | `false`       | Enable debug logging                                                                                                                                        |
| `debug_lsp`                    | bool     | `false`       | Enable LSP debug logging                                                                                                                                    |

### Attribution

Under `options.attribution`:

| Key              | Type                                            | Default         | Description                                      |
| ---------------- | ----------------------------------------------- | --------------- | ------------------------------------------------ |
| `trailer_style`  | `"none"` / `"co-authored-by"` / `"assisted-by"` | `"assisted-by"` | Git trailer style                                |
| `generated_with` | bool                                            | `true`          | Add "Generated with Crush" to commits/issues/PRs |

### TUI

Under `options.tui`:

| Key                     | Type                    | Default | Description                                                           |
| ----------------------- | ----------------------- | ------- | --------------------------------------------------------------------- |
| `transparent`           | bool                    | `false` | Remove app background color, letting terminal background show through |
| `compact_mode`          | bool                    | `false` | Hide sidebar for full-width chat. Toggle: `ctrl+p` → "Toggle Sidebar" |
| `diff_mode`             | `"unified"` / `"split"` | —       | How file diffs display in permissions dialog                          |
| `completions.max_depth` | int                     | `0`     | Directory traversal depth for autocomplete (0 = unlimited)            |
| `completions.max_items` | int                     | `1000`  | Max entries in autocomplete popup                                     |

## Models

Two keys: `large` (primary) and `small` (sub-agents like fetch summarizer). Requires `model` (API model ID) and `provider` (must match a key in `providers`).

```json
{
  "models": {
    "large": { "model": "claude-sonnet-4-20250514", "provider": "anthropic" }
  }
}
```

Optional overrides: `temperature`, `max_tokens`, `reasoning_effort` (`low`/`medium`/`high`), `think` (bool, Anthropic extended thinking), `top_p`, `top_k`, `frequency_penalty`, `presence_penalty`, `provider_options`.

## Tools

Under the `tools` key:

| Key            | Type | Default | Description                             |
| -------------- | ---- | ------- | --------------------------------------- |
| `ls.max_depth` | int  | `0`     | Max directory recursion (0 = unlimited) |
| `ls.max_items` | int  | `1000`  | Max items returned                      |
| `grep.timeout` | int  | `5`     | Timeout in seconds                      |

## LSP

Servers defined as named objects under `lsp`:

```json
{
  "lsp": { "go": { "command": "gopls", "env": { "GOTOOLCHAIN": "go1.24.5" } } }
}
```

| Field          | Type     | Default | Description                                                |
| -------------- | -------- | ------- | ---------------------------------------------------------- |
| `command`      | string   | —       | Command to launch the server                               |
| `args`         | string[] | —       | Arguments passed to command                                |
| `env`          | object   | —       | Environment variables for server process                   |
| `filetypes`    | string[] | —       | File extensions this server handles (e.g. `["go", "mod"]`) |
| `root_markers` | string[] | —       | Files/dirs indicating project root (e.g. `["go.mod"]`)     |
| `init_options` | object   | —       | LSP `InitializationOptions`                                |
| `options`      | object   | —       | Server-specific settings (e.g. gopls settings)             |
| `timeout`      | int      | `30`    | Seconds to wait for initialization                         |
| `disabled`     | bool     | `false` | Prevent this server from starting                          |

## MCP Servers

Named objects under `mcp`. Type is `stdio` (default), `http`, or `sse`:

```json
{
  "mcp": {
    "github": {
      "type": "http",
      "url": "https://api.githubcopilot.com/mcp/",
      "headers": { "Authorization": "Bearer $GH_PAT" }
    }
  }
}
```

| Field            | Type                           | Default   | Description                                    |
| ---------------- | ------------------------------ | --------- | ---------------------------------------------- |
| `type`           | `"stdio"` / `"http"` / `"sse"` | `"stdio"` | Connection type                                |
| `command`        | string                         | —         | Command for stdio servers                      |
| `args`           | string[]                       | —         | Arguments passed to command                    |
| `env`            | object                         | —         | Environment variables                          |
| `url`            | string                         | —         | URL for http/sse servers                       |
| `headers`        | object                         | —         | HTTP headers. Values support env var expansion |
| `timeout`        | int                            | `15`      | Seconds to wait for connection                 |
| `disabled`       | bool                           | `false`   | Skip this server                               |
| `disabled_tools` | string[]                       | —         | Filter out specific tools by name              |

## Providers

Named objects under `providers`. Types: `openai`, `openai-compat`, `anthropic`, `gemini`, `azure`, `vertexai`.

```json
{
  "providers": {
    "deepseek": {
      "type": "openai-compat",
      "base_url": "https://api.deepseek.com/v1",
      "api_key": "$DEEPSEEK_API_KEY",
      "models": [
        {
          "id": "deepseek-chat",
          "name": "Deepseek V3",
          "context_window": 64000
        }
      ]
    }
  }
}
```

| Field                  | Type   | Default    | Description                                                           |
| ---------------------- | ------ | ---------- | --------------------------------------------------------------------- |
| `type`                 | string | `"openai"` | Provider type                                                         |
| `base_url`             | string | —          | Base URL for the API                                                  |
| `api_key`              | string | —          | API key. Supports `$ENV_VAR` syntax                                   |
| `models`               | array  | —          | Available models with id, name, context_window, pricing, capabilities |
| `disable`              | bool   | `false`    | Hide from model selection, skip during loading                        |
| `extra_headers`        | object | —          | Additional HTTP headers on every request                              |
| `extra_body`           | object | —          | Additional JSON in request body (`openai-compat` only)                |
| `system_prompt_prefix` | string | —          | Prepended to system prompt for all requests                           |
| `provider_options`     | object | —          | Provider-specific options (merged: defaults < provider < model)       |
| `oauth`                | object | —          | OAuth2 token for OAuth providers (e.g. Copilot). Auto-refreshed       |

## Permissions

```json
{ "permissions": { "allowed_tools": ["view", "ls", "grep", "edit"] } }
```

Tools listed here won't prompt for confirmation.

## Environment Variables

| Variable                             | Description                        |
| ------------------------------------ | ---------------------------------- |
| `CRUSH_GLOBAL_CONFIG`                | Override global config location    |
| `CRUSH_GLOBAL_DATA`                  | Override data directory location   |
| `CRUSH_SKILLS_DIR`                   | Override default skills directory  |
| `CRUSH_DISABLE_METRICS`              | Disable metrics (`1`)              |
| `CRUSH_DISABLE_PROVIDER_AUTO_UPDATE` | Disable provider auto-update (`1`) |
| `DO_NOT_TRACK`                       | Disable metrics (`1`)              |
