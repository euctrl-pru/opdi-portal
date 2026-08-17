# OPDI methodology papers

Benchmarks of OPDI algorithms against EUROCONTROL reference data, written up as
Quarto documents.

## Separate project, on purpose

`papers/` has its own `_quarto.yml` and is excluded from the site's render list
(`../_quarto.yml`). A paper in progress therefore cannot break the portal build,
and the papers can be rendered on their own:

```bash
quarto render papers/
```

They are linked from `content/resources.qmd` under *Methodology studies*.

## The offline guarantee

**`quarto render` must never need credentials, a database or a Spark cluster.**
Every figure and table reads a CSV committed under `<paper>/data/`. This is the
same guarantee the rest of the portal makes, and it is what allows a reader to
reproduce a result without access to PRISME or the OpenSky cluster.

Refreshing a cache is a deliberate, separate step that runs in the `opdi`
repository, where the credentials live:

```bash
python benchmarks/export_results.py
```

That script reads the research outputs from S3 and rewrites the CSVs here. The
datasets it reads are registered in `opdi/benchmarks/DATASETS.md`.

## Writing one

- State the sample the numbers rest on, in a callout, before any result.
- Report negative results. A method that failed, and the reason it failed, is
  usually the most reusable part of the study — see the cascade section of
  `adep-ades-detection` for the shape of it.
- Keep the R chunks to packages the portal already renders with. Adding a
  dependency for two text labels is not worth the build fragility.
