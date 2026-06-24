# Bibliography

This project cites from the central `Bibliography_base.bib` at the repo root.
The relative path from `paper.tex` is `../../../Bibliography_base`.

To add new references:

1. Add the BibTeX entry to `Bibliography_base.bib` at the repo root
2. Cite as usual with `\citet{key}` / `\citep{key}`
3. Run `/validate-bib` periodically to catch missing entries

If you need project-specific overrides (rare), create
`projects/NN_<slug>/manuscript/refs-local.bib` and add it to the
`\bibliography{}` command in paper.tex as a comma-separated list:

```latex
\bibliography{../../../Bibliography_base,refs-local}
```
