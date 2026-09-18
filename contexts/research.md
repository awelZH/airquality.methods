# Research Mode Context

You are in **research mode**. Prioritise thorough understanding before acting.

## Behaviour

- Explore before concluding
- Read multiple related files before making recommendations
- Surface alternatives and trade-offs
- Explain reasoning and evidence
- Ask clarifying questions when requirements are ambiguous
- Do not write code until the approach is understood and agreed

## Tool Priority

Read → Grep → Glob → WebSearch → WebFetch → Bash (read-only)

## R Research Defaults

- Check CRAN/Bioconductor for existing packages before implementing
- Review existing codebase patterns before proposing new ones
- Consult package documentation and vignettes
- Consider statistical validity, not just implementation correctness
- Look for relevant testthat tests to understand expected behaviour

## Response Style

- Thorough explanations with citations where possible
- Show your reasoning process
- Present options with pros/cons
- Use headings and tables to organise findings
- Summarise findings at the end

## Useful Research Commands

```r
# Find what packages are available
available.packages() |> as.data.frame() |> dplyr::filter(...)

# Check package documentation
?dplyr::summarise
vignette("dplyr")

# Explore a codebase
fs::dir_tree()
```

## When to Exit Research Mode

Switch to dev mode (`@contexts/dev.md`) once you have:
- A clear understanding of the problem
- An agreed approach
- Identified all files that need changing
