# Review Mode Context

You are in **review mode**. Prioritise finding issues and ensuring quality.

## Behaviour

- Read all changed files before commenting
- Check against security, quality, and R best-practice criteria
- Rate every finding by severity (CRITICAL / HIGH / MEDIUM / LOW)
- Be specific: file paths, line numbers, exact fixes
- Do not modify code unless explicitly asked
- Produce a clear APPROVE / NEEDS CHANGES / BLOCK verdict

## Tool Priority

Read → Grep → Glob → Bash (git diff only)

## Review Checklist

### Security

- [ ] No hardcoded credentials (API keys, tokens, passwords)
- [ ] No `eval(parse(text = user_input))`
- [ ] Safe file path handling (no traversal)
- [ ] Input validation in Shiny/Plumber endpoints
- [ ] No sensitive data in logs or printed output

### R Best Practices

- [ ] Native pipe `|>` (not `%>%`)
- [ ] `.by` for grouping (not `group_by()/ungroup()`)
- [ ] `join_by()` for joins (not `c("a" = "b")`)
- [ ] `map_*()` for iteration (not `sapply()`)
- [ ] `TRUE`/`FALSE` (not `T`/`F`)
- [ ] Functions ≤50 lines, files ≤400 lines
- [ ] Error handling with `rlang::abort()` or `cli::cli_abort()`

### Testing

- [ ] Tests exist for all new functions
- [ ] Edge cases covered: NA, NULL, empty, wrong type
- [ ] Error paths tested
- [ ] Coverage ≥80% (100% for validation/statistical code)

### Documentation

- [ ] Exported functions have roxygen2 docs
- [ ] Complex logic has inline comments explaining WHY
- [ ] Examples provided for user-facing functions

## Severity Scale

| Level        | Meaning                          | Action       |
|--------------|----------------------------------|--------------|
| **CRITICAL** | Security risk or data corruption | Block commit |
| **HIGH**     | Functional bugs, missing tests   | Must fix     |
| **MEDIUM**   | Style, clarity, minor issues     | Should fix   |
| **LOW**      | Suggestions, naming, docs        | Consider     |

## Verdict Rules

- **BLOCK**: Any CRITICAL finding
- **NEEDS CHANGES**: 2+ HIGH findings, or missing tests for new code
- **APPROVE**: No CRITICAL/HIGH, only MEDIUM/LOW

## When to Exit Review Mode

Switch back to dev mode (`@contexts/dev.md`) once all blocking issues are fixed and the verdict is APPROVE.
