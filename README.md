# Immunity landscape

Quantifying the immunity landscape for vaccine preventable diseases using immunization data using measles vaccination in Canada as a case study.

# One-time setup

## Virtual environment

This project uses `{renv}` to manage R package versions. If you are unfamiliar with this software, please first review the package's [get started guide](https://rstudio.github.io/renv/articles/renv.html).

When opening this project in a R session, be sure that the `{renv}` project has been activated. Be sure to resolve any inconsistencies in the state of the project when prompted. When you open this project for the first time, you will need to `renv::restore()` to install all a packages required by this project.

# Working with the code

## Generating manuscript figures

Manuscript figures are generated in `results/ms_figs_table.qmd`, which refers to a number of stand-alone scripts called`results/fig-*.R` files. To (re)generate figs, run the `setup` chunk in `ms_figs_table.qmd`, then run the chunk corresponding to the figure you want to regenerate (or run the entire notebook from top to bottom).

# Conventions

## Keeping notes with Quarto

We use [Quarto](https://quarto.org/docs/guide/) to keep notes in this repo, which is set up as a [Quarto Project](https://quarto.org/docs/projects/quarto-projects.html). [This tutorial](https://quarto.org/docs/get-started/hello/rstudio.html) can help you get started with Quarto in RStudio. 

Quarto documents (_e.g._, notes) should be kept at the top level. They inherit the metadata specified in `_quarto.yml`. You can additionally specify document-specific metadata in the YAML header of each file; this will get [merged](https://quarto.org/docs/projects/quarto-projects.html#metadata-merging) with the project-level metadata.

Rendered documents appear in `rendered/draft/`. Since these are generated files that are likely to change a lot as we work on the associated `.qmd`s, tracking changes to them with git would not be useful (we're already tracking the source docs anyway). Thus, `rendered/draft/` is ignored by git. If you would like to share a rendered report over GitHub, please copy it over to `rendered/` (not ignored), having first rendered with the following setting to ensure the generated `.html` is self-contained:

```
format: 
  html:
    embed-resources: true
```

## File paths

Please use [`here::here()`](https://here.r-lib.org/) to specify file paths. These should _always_ be relative to the top-level of the project.

## Using functions

If a bit of code is re-used more than twice, it should be written up as a function. Functions that are specific to a document can remain defined in that document. Functions that are shared across documents should be defined in the `R/` subdirectory (one function per file, where the file shares a name with the function). You can source all functions in  `R/` easily with the following snippet:

```
invisible(lapply(list.files(here::here("R"), full.names = TRUE), source))
```