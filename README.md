# emacs-copilot-agent

An AI coding agent for Emacs, similar to VSCode Copilot Chat.  Connects to
large language models (Anthropic Claude, Google Gemini, Alibaba Qwen) and uses
**tool calling** to read files, run shell commands, search code, and edit
your project — all from a chat buffer inside Emacs.

Works transparently in **remote SSH environments** via TRAMP: when you are
editing a file on a remote host, tool commands execute on that host
automatically.

```
┌─────────────────────────────────────────────┐
│ Copilot Agent                               │
│ Type your message below and press RET       │
│─────────────────────────────────────────────│
│ You                                         │
│ Why is the build failing?                   │
│                                             │
│ Assistant                                   │
│ Let me check the build output.              │
│                                             │
│ [Tool: shell_command]                       │
│   command: "make 2>&1 | tail -20"          │
│                                             │
│ [Result: shell_command]                     │
│ error: undeclared identifier 'ctx' ...      │
│                                             │
│ The error is on line 42 of server.c …      │
│─────────────────────────────────────────────│
│ > _                                         │
└─────────────────────────────────────────────┘
```

---

## Quick Start

**1. Clone the repository**

```bash
git clone https://github.com/yuankuns/emacs-copilot-agent.git \
    ~/.emacs.d/emacs-copilot-agent
```

**2. Add to your `~/.emacs.d/init.el`**

```elisp
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent")
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent/providers")
(require 'copilot-agent)
```

**3. Authenticate with a provider** (pick one):

- **Anthropic** — add to `~/.authinfo`:
  ```
  machine api.anthropic.com login apikey password YOUR_ANTHROPIC_KEY
  ```
- **Gemini** — add to `~/.authinfo`:
  ```
  machine generativelanguage.googleapis.com login apikey password YOUR_GEMINI_KEY
  ```
  Or log in with the free Gemini CLI (no API key needed):
  ```elisp
  (setq copilot-agent-provider 'gemini)
  (setq copilot-agent-gemini-auth-mode 'cli)
  ```
  Then run `M-x copilot-agent-gemini-login` once.
- **Qwen** (free, no API key) — run `M-x copilot-agent-qwen-login` once
  and follow the browser prompt, then set:
  ```elisp
  (setq copilot-agent-provider 'qwen)
  ```
- **GitHub Copilot** (requires Copilot subscription, no API key) — run
  `M-x copilot-agent-github-copilot-login` once and follow the browser prompt,
  then set:
  ```elisp
  (setq copilot-agent-provider 'github-copilot)
  ```

**4. Open the chat**

```
M-x copilot-agent
```

---

## Requirements

| Requirement | Version |
|---|---|
| Emacs | 27.1 or later |
| curl | any recent version (used for HTTP) |
| An API key or free account | Anthropic, Gemini, Qwen, or GitHub Copilot |

---

## Installation

### Manual

```bash
git clone https://github.com/yuankuns/emacs-copilot-agent.git \
    ~/.emacs.d/emacs-copilot-agent
```

Add to `init.el`:

```elisp
;; Both paths are required — providers/ holds the backend files
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent")
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent/providers")
(require 'copilot-agent)
```

### With `use-package` + `straight.el`

```elisp
(use-package copilot-agent
  :straight (:host github :repo "yuankuns/emacs-copilot-agent"
             :files ("*.el" "providers/*.el"))
  :config
  (copilot-agent-setup-keybindings))
```

---

## Provider Setup

### Anthropic Claude (API key)

Get a key at <https://console.anthropic.com/>.  Add to `~/.authinfo`:

```
machine api.anthropic.com login apikey password YOUR_ANTHROPIC_KEY
```

This is the default provider.  No further configuration needed.

### Google Gemini (API key)

Get a key at <https://aistudio.google.com/>.  Add to `~/.authinfo`:

```
machine generativelanguage.googleapis.com login apikey password YOUR_GEMINI_KEY
```

Then in `init.el`:

```elisp
(setq copilot-agent-provider 'gemini)
```

### Google Gemini (free — via Gemini CLI, no API key)

1. Install the Gemini CLI:
   ```bash
   npm install -g @google/gemini-cli   # or: brew install gemini-cli
   ```
2. In `init.el`:
   ```elisp
   (setq copilot-agent-provider 'gemini)
   (setq copilot-agent-gemini-auth-mode 'cli)
   ```
3. Run once inside Emacs:
   ```
   M-x copilot-agent-gemini-login
   ```
   A browser window opens for Google OAuth.  Tokens are saved to
   `~/.emacs-copilot-agent/gemini_oauth_creds.json` and auto-refreshed.

### Alibaba Qwen (free — OAuth, no API key)

1. In `init.el`:
   ```elisp
   (setq copilot-agent-provider 'qwen)
   ```
2. Run once inside Emacs:
   ```
   M-x copilot-agent-qwen-login
   ```
   A browser window opens to <https://chat.qwen.ai>.  Approve the device,
   then return to Emacs.  Tokens are saved to `~/.qwen/oauth_creds.json`
   and auto-refreshed.  Free tier: 2 000 requests/day.

### GitHub Copilot (requires active Copilot subscription, no API key)

1. In `init.el`:
   ```elisp
   (setq copilot-agent-provider 'github-copilot)
   ```
2. Run once inside Emacs:
   ```
   M-x copilot-agent-github-copilot-login
   ```
   A browser window opens to `github.com/login/device`; the one-time code
   is copied to your clipboard.  After approval, the token is saved to
   `~/.emacs-copilot-agent/github_copilot_creds.json`.  Short-lived session
   tokens (~30 min) are fetched and refreshed automatically.
3. Browse available models (GPT-4o, Claude, Gemini, …):
   ```
   M-x copilot-agent-select-model
   ```
   Or list models for your subscription: `M-x copilot-agent-github-copilot-list-models`

---

## API Key Security

Keys are read from `~/.authinfo` or `~/.authinfo.gpg` via Emacs'
built-in `auth-source` library.  **No keys are stored in Emacs Lisp
variables or your init file.**

To use the encrypted version:

```bash
gpg --output ~/.authinfo.gpg --symmetric ~/.authinfo
rm ~/.authinfo   # keep only the encrypted copy
```

Emacs will prompt for the GPG passphrase when the key is first needed.

---

## Configuration

All settings live under the `copilot-agent` customisation group
(`M-x customize-group RET copilot-agent RET`).  Common variables:

```elisp
;; Provider: 'anthropic (default), 'gemini, 'qwen, or 'github-copilot
(setq copilot-agent-provider 'anthropic)

;; Default model (overrides the provider's built-in default)
;; Anthropic default:       "claude-sonnet-4-6"
;; Gemini default:          "gemini-2.0-flash"
;; Qwen default:            "coder-model"
;; GitHub Copilot default:  "gpt-4o"  (use M-x copilot-agent-select-model to browse)
;; (setq copilot-agent-anthropic-default-model      "claude-opus-4-6")
;; (setq copilot-agent-gemini-default-model         "gemini-2.0-pro")
;; (setq copilot-agent-github-copilot-default-model "claude-3.7-sonnet")

;; Include the current buffer's file path in the system prompt
(setq copilot-agent-auto-context t)   ; default: t

;; Maximum tokens per response
(setq copilot-agent-max-tokens 8192)  ; default: 8192

;; Bind the command map to a prefix key (C-c / by default)
(copilot-agent-setup-keybindings)

;; Custom system prompt (optional)
;; (setq copilot-agent-system-prompt "You are a concise coding assistant…")
```

### Complete `init.el` example

```elisp
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent")
(add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent/providers")
(require 'copilot-agent)

;; Choose a provider
(setq copilot-agent-provider 'anthropic)

;; Optional: bind C-c / prefix
(copilot-agent-setup-keybindings)
```

---

## Usage

### Open the chat buffer

```
M-x copilot-agent
```

This opens the `*Copilot Agent*` side window and sets the working context
to the directory of your current buffer (including remote paths via TRAMP).

### Key bindings inside the chat buffer

| Key | Action |
|---|---|
| `RET` or `C-c C-c` | Send message |
| `C-c C-n` | Start a new session (clear history) |
| `C-c C-k` | Clear message history, keep session |
| `q` | Hide the chat window |

### Command map (after `copilot-agent-setup-keybindings`)

| Key | Command |
|---|---|
| `C-c / a` | `copilot-agent` — open chat |
| `C-c / e` | `copilot-agent-explain-region` — explain selection |
| `C-c / f` | `copilot-agent-fix-errors` — fix compile/flycheck errors |
| `C-c / m` | `copilot-agent-select-model` — switch provider/model |
| `C-c / n` | `copilot-agent-new-chat` — fresh session |

### Explain a region

Select code with the region, then:

```
M-x copilot-agent-explain-region
```

or `C-c / e` — the selection is sent to the agent with an explanation
request pre-filled.

### Fix errors

Run `M-x compile` or enable `flycheck-mode`, then:

```
M-x copilot-agent-fix-errors
```

or `C-c / f` — the agent receives the error output and can read and edit
the relevant files using its tools.

---

## Tool Approval

Before any tool runs, a minibuffer prompt appears:

```
Run tool `shell_command'
  command: "rm -rf build/"
[y]es / [a]ll / [n]o:
```

| Key | Effect |
|---|---|
| `y` | Run this tool, prompt again for the next one |
| `a` | Run this tool **and** all remaining tools in this turn without prompting |
| `n` | Skip this tool; the agent receives "User declined" as the result |

---

## Available Tools

The agent can call the following tools.  All file paths and shell commands
respect the active working directory, including remote TRAMP paths.

| Tool | Description |
|---|---|
| `shell_command` | Run a shell command; output is returned to the agent |
| `read_file` | Read the full contents of a file |
| `write_file` | Write content to a file (creates parent directories) |
| `list_directory` | List directory contents with sizes and timestamps |
| `find_in_files` | Grep for a regex pattern across files (like `grep -rn`) |
| `create_directory` | Create a directory and any missing parents |
| `delete_file` | Delete a file (requires approval) |

---

## Remote SSH (TRAMP) Workflow

No special configuration is needed.  Open a remote file normally:

```
C-x C-f /ssh:user@myserver:/home/user/project/main.py RET
```

Then open the agent:

```
M-x copilot-agent
```

The agent's working directory is now `/ssh:user@myserver:/home/user/project/`.
When the agent runs `shell_command` or any file tool, it executes on
`myserver` via TRAMP — as if you had an SSH session open in the background.

### ProxyJump / multi-hop

TRAMP handles multi-hop paths transparently:

```
/ssh:bastion|ssh:internal-host:/path/to/file
```

The agent will run commands on `internal-host`.

---

## Adding a New Provider

1. Create `providers/copilot-agent-YOURPROVIDER.el`.

2. Implement two functions:

   ```elisp
   (defun my-provider-send (session callback)
     "Send SESSION to the API.  Call CALLBACK with (RESPONSE NIL) or (NIL ERROR)."
     ;; RESPONSE plist must have: :text :tool-calls :stop-reason :raw-content
     ...)

   (defun my-provider-make-tool-result (results)
     "Convert RESULTS (list of (:tool-use-id ID :content STRING)) to a message alist."
     ...)
   ```

3. Register after `copilot-agent-api` loads:

   ```elisp
   (with-eval-after-load 'copilot-agent-api
     (copilot-agent-api-register-provider
      'myprovider
      (list :display-name          "My Provider"
            :default-model         "my-model-v1"
            :send-fn               #'my-provider-send
            :make-tool-result-fn   #'my-provider-make-tool-result
            :format-tools-fn       #'my-provider-format-tools)))
   ```

4. Set it as the default:

   ```elisp
   (setq copilot-agent-provider 'myprovider)
   ```

---

## Running the Tests

The test suite uses Emacs' built-in ERT framework.

```bash
# From the project root:
emacs --batch -l test/run-tests.el

# Or a single suite:
emacs --batch -L . -L providers \
      -l test/test-copilot-agent-tools.el \
      -f ert-run-tests-batch-and-exit
```

---

## Project Structure

```
emacs-copilot-agent/
├── copilot-agent.el            Entry point, user commands, configuration
├── copilot-agent-api.el        Provider registry, async HTTP, agentic loop
├── copilot-agent-tools.el      Tool schema + TRAMP-aware implementations
├── copilot-agent-ui.el         Chat buffer, rendering, input, approval UI
├── copilot-agent-status.el     Model/status line helpers
├── providers/
│   ├── copilot-agent-anthropic.el       Anthropic Claude backend
│   ├── copilot-agent-gemini.el          Google Gemini backend
│   ├── copilot-agent-qwen.el            Alibaba Qwen backend (free OAuth)
│   └── copilot-agent-github-copilot.el  GitHub Copilot backend (OAuth device flow)
└── test/
    ├── run-tests.el                  Test runner (all suites)
    ├── test-copilot-agent-tools.el   Tool tests
    ├── test-copilot-agent-api.el     API/loop tests
    ├── test-copilot-agent-anthropic.el  Anthropic provider tests
    ├── test-copilot-agent-gemini.el     Gemini provider tests
    └── test-copilot-agent-qwen.el       Qwen provider tests
```

---

## License

MIT License.  See `LICENSE` for details.
