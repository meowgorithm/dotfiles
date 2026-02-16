---
name: bash
description: Create, edit, and improve bash scripts with automated linting and formatting. Use when working with shell scripts (.sh files), writing bash functions, fixing script errors, or when the user asks to create or modify any bash/shell script.
---

# Bash

Write robust, well-formatted bash scripts with automated quality checks.

## Workflow

1. **Before writing**: Format with `shfmt -w <file>` if available
2. **After writing**: Lint with `shellcheck <file>` if available
3. **Iterate**: Fix errors, re-format if needed

## Formatting with shfmt

Always format before writing if `shfmt` is installed:

```bash
shfmt -w script.sh
```

Default style (POSIX-compatible):
- Indent with tabs
- Simplify code where possible
- Add spaces around operators

Options:
- `-i 4` - 4-space indent instead of tabs
- `-ci` - indent switch cases
- `-bn` - binary operators start lines

## Linting with shellcheck

Run after writing/modifying scripts:

```bash
shellcheck script.sh
```

Common issues to address:
- SC2086: Double quote variables to prevent globbing
- SC2034: Unused variables
- SC2164: Use `cd ... || exit` for error handling
- SC2181: Check exit status directly instead of `$?`

## Best Practices

### Shebang and Headers

```bash
#!/usr/bin/env bash
set -euo pipefail
```

### Error Handling

```bash
# Check command success
if ! command -v foo &>/dev/null; then
    echo "error: foo not found" >&2
    exit 1
fi

# Safe cd
cd "$DIR" || exit 1
```

### Variables

```bash
# Quote all variables
"$VAR"
"${ARRAY[@]}"

# Prefer local
local var="value"

# Defaults
: "${VAR:=default}"
```

### Functions

```bash
my_func() {
    local arg1="$1"
    # ...
}
```

### Conditionals

```bash
# Prefer [[ ]] over [ ]
[[ -f "$file" ]]

# String matching
[[ "$str" == *pattern* ]]
```

### Pipes

```bash
# Use pipefail to catch errors in pipelines
set -o pipefail
cmd1 | cmd2
```
