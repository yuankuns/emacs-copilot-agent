# emacs-copilot-agent

An AI coding agent for Emacs, similar to VSCode Copilot Chat.  Connects to
large language models (Anthropic Claude, Google Gemini, …) and uses
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

## Features

- **Multi-provider** — Anthropic Claude and Google Gemini out of the box;
  additional providers can be registered without modifying core code.
- **Agentic tool use** — the LLM can run shell commands, read/write files,
  list directories, and grep for patterns in a loop until the task is done.
- **Tool approval** — every tool call is shown to you before execution with
  a `[y]es / [a]ll / [n]o` prompt.  Choose *all* to approve the rest of
  the turn automatically.
- **TRAMP-aware** — editing `/ssh:user@host:/path/to/file`?  Tool commands
  run on `user@host`, not on your local machine.
- **Context injection** — automatically includes the current buffer's file
  path in the system prompt so the agent knows what you are working on.
- **Dedicated chat buffer** — persistent conversation history, rendered
  sections for user/assistant/tool output, and a live input area at the
  bottom.

---

## Requirements

| Requirement | Version |
|---|---|
| Emacs | 27.1 or later |
| curl | any recent version (used for HTTP) |
| An Anthropic or Gemini API key | — |

---

## Installation

### Manual (recommended for now)

1. Clone the repository:

   ```bash
   git clone https://github.com/your-org/emacs-copilot-agent.git \
       ~/.emacs.d/emacs-copilot-agent
   ```

2. Add to your `init.el`:

   ```elisp
   (add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent")
   (add-to-list 'load-path "~/.emacs.d/emacs-copilot-agent/providers")
   (require 'copilot-agent)
   ```

### With `use-package` + `straight.el`

```elisp
(use-package copilot-agent
  :straight (:host github :repo "your-org/emacs-copilot-agent"
             :files ("*.el" "providers/*.el"))
  :config
  (copilot-agent-setup-keybindings))
```

---

## API Key Setup

Keys are read from `~/.authinfo` or `~/.authinfo.gpg` via Emacs'
built-in `auth-source` library.  **No keys are stored in Emacs Lisp
variables or your init file.**

Add one or both of the following lines to `~/.authinfo`:

```
# Anthropic Claude
machine api.anthropic.com login apikey password YOUR_ANTHROPIC_KEY

# Google Gemini
machine generativelanguage.googleapis.com login apikey password YOUR_GEMINI_KEY
```

To use the encrypted version (`~/.authinfo.gpg`), create it with:

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
;; Provider to use: 'anthropic (default) or 'gemini
(setq copilot-agent-provider 'anthropic)

;; Default model (overrides the provider's built-in default)
;; Anthropic default: "claude-sonnet-4-6"
;; Gemini default:    "gemini-2.0-flash"
;; (setq copilot-agent-anthropic-default-model "claude-opus-4-6")
;; (setq copilot-agent-gemini-default-model    "gemini-2.0-pro")

;; Include the current buffer's file path in the system prompt
(setq copilot-agent-auto-context t)   ; default: t

;; Maximum tokens per response
(setq copilot-agent-max-tokens 8192)  ; default: 8192

;; Bind the command map to a prefix key
(copilot-agent-setup-keybindings)     ; binds C-c / by default

;; Custom system prompt
(setq copilot-agent-system-prompt "You are a concise coding assistant…")
```

### Switch provider per session

```elisp
;; Temporarily use Gemini for the current session
(setq copilot-agent-provider 'gemini)
(copilot-agent)
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

The test suite uses Emacs' built-in ERT framework (105 tests).

```bash
# From the project root:
emacs --batch -l test/run-tests.el

# Or a single suite:
emacs --batch -L . -L providers \
      -l test/test-copilot-agent-tools.el \
      -f ert-run-tests-batch-and-exit
```

Expected output:

```
Ran 105 tests, 105 results as expected (0.09 sec)
```

---

## Project Structure

```
emacs-copilot-agent/
├── copilot-agent.el            Entry point, user commands, configuration
├── copilot-agent-api.el        Provider registry, async HTTP, agentic loop
├── copilot-agent-tools.el      Tool schema + TRAMP-aware implementations
├── copilot-agent-ui.el         Chat buffer, rendering, input, approval UI
├── providers/
│   ├── copilot-agent-anthropic.el   Anthropic Claude backend
│   └── copilot-agent-gemini.el      Google Gemini backend
└── test/
    ├── run-tests.el                  Test runner (all suites)
    ├── test-copilot-agent-tools.el   Tool tests (40 tests)
    ├── test-copilot-agent-api.el     API/loop tests (20 tests)
    ├── test-copilot-agent-anthropic.el  Anthropic provider tests (25 tests)
    └── test-copilot-agent-gemini.el     Gemini provider tests (20 tests)
```

---

## License

MIT License.  See `LICENSE` for details.
