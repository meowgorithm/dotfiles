# Advanced jq Patterns

## Table of Contents

- [Recursive Operations](#recursive-operations)
- [Object Manipulation](#object-manipulation)
- [Merge & Combine](#merge--combine)
- [Date & Time](#date--time)
- [Stream Processing](#stream-processing)
- [Complex Aggregation](#complex-aggregation)
- [Debugging](#debugging)

## Recursive Operations

```bash
# Find all values matching condition at any depth
jq '[.. | select(type == "string" and test("error"))]'

# Get all leaf values
jq '[.. | scalars]'

# Replace all nulls recursively
jq '(.. | nulls) = "N/A"'

# Pick all keys named "id" at any depth
jq '[paths(.. | .id?) as $p | getpath($p + ["id"])]'
```

## Object Manipulation

```bash
# Add/update a field
jq '. + {updated: true}'
jq '.field = "new_value"'
jq '.field |= . + 1'

# Remove a field
jq 'del(.internal)'

# Rename a key
jq '. | {name: .title, (.other_keys): .other_values}'

# Pick specific keys
jq '{name, email, age}'

# Merge two objects
jq '. + .defaults'

# Convert object to array of key-value pairs
jq 'to_entries'
jq 'to_entries | map({key: .key, value: .value})'

# Convert back from entries
jq 'from_entries'

# Update specific nested value
jq '.config.settings.timeout = 30'
jq '.config.settings |= . + {retries: 3}'
```

## Merge & Combine

```bash
# Merge two JSON files
jq -s '.[0] * .[1]' base.json override.json

# Deep merge (arrays replaced, objects merged)
jq -s '.[0] * .[1]' a.json b.json

# Concatenate arrays from multiple files
jq -s 'add' part1.json part2.json

# Zip two arrays
jq -s 'transpose' keys.json values.json

# Join array of objects by a key
jq -s '[.[0][] as $a | .[1][] | select(.id == $a.id) + $a]' users.json details.json
```

## Date & Time

```bash
# Current time (requires jq 1.6+)
jq -n 'now'

# Format epoch as ISO date
jq '.timestamp | todate'

# Parse ISO date to epoch
jq '.date | fromdate'

# Extract date components
jq '.ts | todate | split("T") | .[0]'

# Calculate duration
jq '(.end | fromdate) - (.start | fromdate)'
```

## Stream Processing

```bash
# Process newline-delimited JSON (ndjson)
cat logs.ndjson | jq 'select(.level == "error")'

# Convert JSON array to ndjson
jq '.[]'

# Convert ndjson to JSON array
jq -s '.'

# Process ndjson with accumulator
jq -n --slurpfile data data.ndjson '$data | group_by(.user) | map({user: .[0].user, count: length})'
```

## Complex Aggregation

```bash
# Pivot table: group by one field, count another
jq 'group_by(.category) | map({
  category: .[0].category,
  total: length,
  by_status: (group_by(.status) | map({key: .[0].status, value: length}) | from_entries)
})'

# Running total
jq '. as $data | range(length) | {x: ., y: ($data[0:.+1] | map(.value) | add)}'

# Percentile
jq 'sort | .[length * 90 / 100 | floor]'

# Frequency count
jq 'group_by(.) | map({value: .[0], count: length}) | sort_by(-.count)'

# Flatten with parent reference
jq '.children[] | {name, parent: $parent} as $child | $child'
```

## Debugging

```bash
# Show input unchanged (identity with side effect)
jq '. | debug("intermediate") | .name'

# Trace filter evaluation
jq -e '.missing'  # exits 1 if null/false

# Pretty-print compact JSON
jq '.' compact.json

# Validate JSON
jq '.' suspect.json

# Show types
jq 'map(type)'

# Show structure without values
jq 'map(keys)'

# Count elements at each level
jq 'paths | length'

# Extract error messages from curl
curl -s https://api.example.com | jq '.' 2>&1 | head -20
```