---
name: jq
description: Process, query, and transform JSON data using jq. Use when working with JSON files, API responses, or any JSON data that needs filtering, extracting, reshaping, or analyzing. Triggers include mentions of "jq", JSON processing, JSON filtering, JSON transformation, extracting data from JSON, or any task involving jq commands or expressions.
---

# jq

Command-line JSON processor. Filter, transform, and analyze JSON data with concise expressions.

## Quick Reference

```bash
# Pretty-print
jq '.' data.json

# Extract field
jq '.name' data.json

# Extract nested
jq '.user.email' data.json

# Array element
jq '.items[0]' data.json

# All array elements
jq '.items[]' data.json

# From stdin
curl -s https://api.example.com | jq '.results'
```

## Common Patterns

### Selection & Filtering

```bash
# Select objects matching condition
jq '.[] | select(.status == "active")'
jq '.[] | select(.age >= 18)'

# Select with multiple conditions
jq '.[] | select(.role == "admin" and .active)'

# Has key check
jq '.[] | select(has("email"))'

# Unique values
jq 'unique'               # unique sorted
jq 'unique_by(.name)'     # unique by field
```

### Transformation

```bash
# Create new object
jq '{name: .user.name, email: .user.email}'

# Map over array
jq '.items | map(.price)'

# Map with transform
jq '.items | map({name: .title, cost: .price * 1.1})'

# Flatten nested arrays
jq '[.[] | .tags[]]'

# Group and count
jq 'group_by(.category) | map({key: .[0].category, count: length})'
```

### Sorting & Statistics

```bash
# Sort
jq 'sort_by(.age)'
jq 'sort_by(.name | ascii_downcase)'

# Reverse
jq 'reverse'

# Min/max
jq '. | max_by(.score)'
jq '. | min_by(.price)'

# Stats
jq '[.[].score] | add / length'   # average
jq '[.[].price] | add'            # sum
jq 'length'                        # count
```

### String Operations

```bash
# Format strings
jq '.name | "Hello, \(.)!"'

# Split/join
jq '.csv | split(",")'
jq '.tags | join(", ")'

# Regex
jq '.url | capture("https://(?<host>[^/]+)")'
jq '.names | map(select(test("^admin"; "i")))'

# Case
jq '.name | ascii_downcase'
jq '.name | ascii_upcase'

# Trim
jq '.name | ltrimstr("prefix-") | rtrimstr("-suffix")'
```

### Advanced

```bash
# Recursive descent
jq '..'

# Paths to matching values
jq '[paths(.score > 90)]'

# Get value at path
jq 'getpath([0, "name"])'

# Reduce
jq 'reduce .[] as $x (0; . + $x.value)'

# Variables
jq '.items | to_entries | map(.key + "=" + (.value | tostring))'

# Conditionals
jq 'if .score >= 90 then "A" elif .score >= 80 then "B" else "C" end'

# Try-catch
jq '.[] | try .code catch "N/A"'

# Read from multiple files
jq -s '.[0] + .[1]' file1.json file2.json

# Slurp (wrap multiple objects into array)
jq -s '.' objects*.json

# Raw string output (no quotes)
jq -r '.name'

# Raw input (each line as string, not JSON)
jq -R '.'
```

## Flags

| Flag | Purpose |
|------|---------|
| `-r` | Output raw strings (no quotes) |
| `-R` | Read raw input (each line as string) |
| `-s` | Slurp all inputs into array |
| `-e` | Exit with error if result is null/false |
| `-n` | Don't read input (use with `jq -n '...'`) |
| `-f file` | Read filter from file |
| `-c` | Compact output |
| `-C` | Color output |
| `-M` | Monochrome output |
| `--arg name val` | Pass variable |
| `--argjson name val` | Pass JSON variable |

## Tips

- Chain filters with `|` (pipe), like Unix pipes
- Use `select()` to filter — it's jq's `grep`
- `map(f)` is shorthand for `[.[] | f]`
- Use `to_entries` / `from_entries` to iterate object key-value pairs
- `has("key")` checks for key existence; `.key // "default"` provides fallbacks
- Always quote jq expressions in shell: `jq '.foo'` not `jq .foo`
- For large files, combine `jq` with `head`/`tail` for inspection before full processing
- Use `jq -n` with `--arg` to construct JSON: `jq -n --arg name "foo" '{name: $name}'`

## Advanced Patterns

For recursive operations, complex merging, date/time handling, stream processing, and debugging — see [references/advanced.md](references/advanced.md).