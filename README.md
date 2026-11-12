# scs.nba.blog

Quarto blog for Shaggy Camel Sports.

## Render and publish

```bash
Rscript -e 'renv::restore()'
quarto preview
quarto publish netlify
```

`_publish.yml` also carries the Quarto Pub target. `_site/` and `_freeze/` are
generated and untracked; keep them out of git.
