---
name: verify
description: Run full R verification loop before committing. Checks build, lint, style, test coverage, and code quality.
---

# /verify - R Verification Loop

Run all quality gates before committing code. Produces a READY or NOT READY verdict.

## When to Use

- Before every commit
- After implementing a feature or fix
- Before opening a pull request
- Any time you want a full quality check

## Verification Phases

Run each phase in order. A failure in any phase blocks the verdict.

### Phase 1: Build Check

```r
devtools::check(quiet = TRUE)
```

Pass criteria: 0 errors, 0 warnings, 0 notes (or only expected notes).

### Phase 2: Style Check

```r
styler::style_pkg()
```

Auto-fixes style. Re-check for any manual fixes needed.

### Phase 3: Lint Check

```r
lintr::lint_package()
```

Pass criteria: 0 lints. Fix all flagged issues before proceeding.

### Phase 4: Test Coverage

```r
covr::package_coverage()
```

Pass criteria: ≥80% overall coverage. Critical paths (validation, stats) require 100%.

### Phase 5: Code Review

Invoke the code-reviewer agent to check uncommitted changes:

```
/code-review
```

Pass criteria: No CRITICAL or HIGH issues.

### Phase 6: Diff Review

```bash
git diff --stat HEAD
git diff HEAD
```

Manually confirm:
- No unintended files changed
- No debug code or `print()`/`cat()` left in
- No hardcoded paths or credentials
- Commit message ready

## Verdict Format

```
## Verification Report

| Phase          | Status | Notes                          |
|----------------|--------|--------------------------------|
| Build check    | ✅ PASS | 0 errors, 0 warnings           |
| Style check    | ✅ PASS | Auto-fixed 3 files             |
| Lint check     | ✅ PASS | 0 lints                        |
| Test coverage  | ✅ PASS | 87% overall                    |
| Code review    | ✅ PASS | No CRITICAL/HIGH issues        |
| Diff review    | ✅ PASS | Changes look correct           |

## Verdict: ✅ READY TO COMMIT

Next step: commit with a descriptive message.
```

Or if issues found:

```
## Verdict: ❌ NOT READY

**Blocking issues:**
1. [Phase] - [specific issue]
2. [Phase] - [specific issue]

Fix all blocking issues and re-run /verify.
```

## Coverage Standards

| Code Type                | Required |
|--------------------------|----------|
| General logic            | ≥80%     |
| Statistical calculations | 100%     |
| Input validation         | 100%     |
| Error handling           | 100%     |

## Related Commands

- `/plan` - Plan before implementing
- `/tdd` - Write tests first
- `/code-review` - Standalone code review
