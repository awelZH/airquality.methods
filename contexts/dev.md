# Dev Mode Context

You are in **development mode**. Prioritise working solutions.

## Behaviour

- Write code first, explain after
- Prefer working solutions over perfect solutions
- Run tests after every change (`devtools::test()`)
- Keep commits small and atomic
- Fix the immediate problem; avoid scope creep

## Tool Priority

Edit → Write → Bash → Grep → Glob → Read

## R Development Defaults

- Use native pipe `|>` always
- Use `.by` for grouping (not `group_by()/ungroup()`)
- Use `join_by()` for joins
- Use `map_*()` for iteration (not `sapply()`)
- Write tests before implementation (TDD)
- Run `devtools::check()` before committing

## Response Style

- Short responses; let code speak
- Inline comments only for non-obvious logic
- No lengthy explanations unless asked
- Show the diff, not the whole file

## Workflow Sequence

`/plan` → `/tdd` → implement → `/verify` → commit
