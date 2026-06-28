# `set_guide_layout()` Design Spec

## Status

- Final implementation target
- Scope: API, internal design, and acceptance criteria
- Target package: `aplot`
- Dependency context: guide collection and guide placement remain delegated to
  `patchwork` and `ggplot2`

## Background

`set_guide_area()` solves the placement problem: where collected guides should
be rendered in an `aplot` layout.

`set_guide_layout()` should solve the arrangement problem, but "guide layout"
actually contains two different layers:

- how each individual legend is arranged internally;
- how multiple collected legend boxes are arranged relative to one another.

The current `ncol`-style API only covers the first layer. This is insufficient
for cases where multiple collected legends should be wrapped into two columns in
the guide area, while each legend keeps its own internal one-column layout.

## Design Goal

Keep a single public helper, `set_guide_layout()`, but split its parameters
into two semantic groups:

- `legend_*` controls the internal layout of each individual legend;
- `guides_*` controls the layout of multiple collected legend boxes as a group.

The intended use case is still compact guide arrangement for corner guide
areas, but the API must now distinguish between single-legend layout and
multi-guide-box layout.

## Non-goals

- Do not change where guides are placed. That remains the job of
  `set_guide_area()`.
- Do not introduce automatic multi-cell corner merging.
- Do not expose the full `theme()` surface.
- Do not expose arbitrary `guide_legend()` internals.
- Do not try to automatically detect legend overflow in the completed first release.
- Do not try to merge semantically different guides into one.

## User Model

The user configures guide handling in two independent steps:

- `set_guide_area()` decides where collected guides go.
- `set_guide_layout()` decides how collected guides are arranged.

If the collected guide still does not fit after layout adjustment, the user
should place the guide on a side rather than in a corner.

This is an important product rule:

- corner guide area = compact mode
- side guide placement = relaxed mode

## Final API

```r
set_guide_layout <- function(
    x,
    legend_ncol = NULL,
    legend_byrow = FALSE,
    legend_title_position = NULL,
    legend_direction = NULL,
    legend_keywidth = NULL,
    legend_keyheight = NULL,
    legend_spacing_x = NULL,
    legend_spacing_y = NULL,
    guides_ncol = NULL,
    guides_byrow = FALSE,
    guides_direction = NULL
)
```

## Parameter Intent

- `legend_ncol`: number of columns used inside each individual legend
- `legend_byrow`: whether items inside an individual legend are filled row-wise
- `legend_title_position`: title placement inside each individual legend
- `legend_direction`: direction of each individual legend
- `legend_keywidth`: width of keys inside each individual legend
- `legend_keyheight`: height of keys inside each individual legend
- `legend_spacing_x`: horizontal spacing between items inside an individual legend
- `legend_spacing_y`: vertical spacing between items inside an individual legend
- `guides_ncol`: number of columns used to arrange multiple collected legend
  boxes as a group
- `guides_byrow`: whether collected legend boxes are filled row-wise
- `guides_direction`: overall direction for arranging collected legend boxes

## Why This Parameter Set

This set is intentionally small. It covers the common compaction cases:

- one narrow legend becoming two columns internally;
- a vertical legend becoming horizontal;
- title wasting too much space inside a legend;
- keys or inter-item gaps being too loose;
- multiple legends needing to wrap as two columns in a guide area.

It avoids opening the door to full guide customization, which would make the
helper harder to reason about and harder to document.

## Parameters Explicitly Not Included

The completed first release should not support:

- `nrow`
- arbitrary `theme` injection
- arbitrary `guide` objects
- guide title text replacement
- per-aesthetic guide layout settings
- automatic fit-to-cell behavior
- plot-specific legend overrides through this helper
- arbitrary box-level spacing controls in the first implementation

### Rationale

- `nrow` is redundant once `ncol` exists for the main compaction use case.
- arbitrary `theme` injection would turn this helper into a generic theme
  forwarding layer.
- arbitrary `guide` objects would make behavior unpredictable after guide
  collection.
- per-aesthetic settings are likely to fight against guide collection instead of
  helping it.

## Migration and Compatibility

Current released arguments:

- `ncol`
- `byrow`
- `title.position`
- `direction`
- `keywidth`
- `keyheight`
- `spacing.x`
- `spacing.y`

These should become compatibility aliases for:

- `ncol` -> `legend_ncol`
- `byrow` -> `legend_byrow`
- `title.position` -> `legend_title_position`
- `direction` -> `legend_direction`
- `keywidth` -> `legend_keywidth`
- `keyheight` -> `legend_keyheight`
- `spacing.x` -> `legend_spacing_x`
- `spacing.y` -> `legend_spacing_y`

Compatibility policy:

- new `legend_*` names become the documented API;
- old names remain accepted for backward compatibility in the first transition;
- supplying both old and new names for the same semantic field should `stop()`;
- documentation and examples should use only the new `legend_*` names.

## Semantic Boundary

`set_guide_layout()` should only influence the appearance of the collected
guide, not the structure of the `aplot` layout.

Specifically, it must not:

- modify `x$layout`
- modify `width` or `height`
- affect axis alignment
- affect subplot indexing semantics
- override `set_guide_area()`

## Internal Object Design

Add a new optional field to the `aplot` object:

```r
x$guide_layout
```

Stored value:

```r
list(
    legend = list(
        ncol = 2,
        byrow = FALSE,
        title.position = "top",
        direction = "horizontal",
        keywidth = NULL,
        keyheight = NULL,
        spacing.x = NULL,
        spacing.y = NULL
    ),
    guides = list(
        ncol = 2,
        byrow = FALSE,
        direction = "horizontal"
    )
)
```

### Rationale

- Mirrors the existing object-level configuration style used by `x$spacing` and
  `x$guide_area`
- Keeps the guide layout configuration attached to the final `aplot` object
- Separates single-legend policy from multi-guide-box policy

## Validation Rules

Add an internal validator:

```r
.validate_guide_layout <- function(
    legend_ncol = NULL,
    legend_byrow = FALSE,
    legend_title_position = NULL,
    legend_direction = NULL,
    legend_keywidth = NULL,
    legend_keyheight = NULL,
    legend_spacing_x = NULL,
    legend_spacing_y = NULL,
    guides_ncol = NULL,
    guides_byrow = FALSE,
    guides_direction = NULL,
    ncol = deprecated(),
    byrow = deprecated(),
    title.position = deprecated(),
    direction = deprecated(),
    keywidth = deprecated(),
    keyheight = deprecated(),
    spacing.x = deprecated(),
    spacing.y = deprecated()
)
```

Validation rules:

- `legend_ncol` and `guides_ncol` must be `NULL` or positive integer scalars
- `legend_byrow` and `guides_byrow` must be single logical values
- `legend_title_position` must be `NULL` or one of a small allowed set
  supported by `ggplot2`
- `legend_direction` and `guides_direction` must be `NULL` or
  `"horizontal"` / `"vertical"`
- `legend_keywidth`, `legend_keyheight`, `legend_spacing_x`, and
  `legend_spacing_y` must be `NULL` or non-negative numeric scalars
- old and new parameter names cannot both be supplied for the same field

The completed first release should keep numeric sizing values simple and
interpret them in a single consistent way:

- `legend_keywidth` and `legend_keyheight` become `grid::unit(..., "mm")` inside
  `guide_legend()`
- `legend_spacing_x` and `legend_spacing_y` become `grid::unit(..., "mm")` inside
  `theme(legend.spacing.x/y)`

## Integration Strategy

`patchwork` collects guides, but does not provide a dedicated high-level API for
compacting a collected guide inside `guide_area()`.

Therefore the helper should use two internal layers:

- apply a consistent single-legend policy to plots before collection;
- apply a separate multi-guide-box policy after guide collection, at the guide
  box level.

High-level render flow:

1. `as.patchwork()` resolves `x$guide_layout`
2. If guide layout is not set, behavior stays unchanged
3. If legend layout is set, apply the single-legend policy to each plot in
   `x$plotlist`
4. Continue existing guide collection and guide area placement flow
5. If guides layout is set and multiple guide boxes exist, apply a separate
   box-level arrangement policy

## Rendering Design

Add an internal helper:

```r
.apply_legend_layout <- function(plotlist, legend_layout)
```

Responsibilities:

- return `plotlist` unchanged when `legend_layout` is `NULL`
- add a compact single-legend policy to each plot in `plotlist`
- keep the policy consistent across plots so collected guides remain compatible

Add a second internal helper:

```r
.apply_guides_layout <- function(collected_guides, guides_layout)
```

Responsibilities:

- return the collected guides unchanged when `guides_layout` is `NULL`
- arrange multiple guide boxes as a group
- support at least `guides_ncol` in the completed first implementation

### Important Constraint

The single-legend helper must apply a uniform guide layout policy. It should
not try to style different plots differently.

If the helper allows per-plot or per-aesthetic layout divergence, guide
 collection may stop behaving predictably.

## Relationship With Guide Collection

This helper cannot guarantee that separate guides become one guide.

For example:

- one plot using `colour`
- another plot using `fill`

may still produce separate collected guides even if both map the same variable.

This should be documented explicitly:

- `set_guide_layout()` compacts guide arrangement
- it does not merge semantically different guides

## Documentation Target

The help page should communicate:

- use `set_guide_area()` to place the collected guide
- use `set_guide_layout()` to compact the collected guide
- `legend_*` parameters style each legend internally
- `guides_*` parameters arrange multiple collected legend boxes
- if the guide still does not fit, prefer side placement over corner placement

This last point matters. It keeps user expectations sane and avoids making the
feature sound magically auto-fitting.

## Example Scenario

```r
ap <- p |>
    insert_top(p_top, height = 0.3) |>
    insert_right(p_right, width = 0.25) |>
    set_guide_area("top-right") |>
    set_guide_layout(
        legend_ncol = 1,
        legend_direction = "vertical",
        legend_title_position = "top",
        guides_ncol = 2,
        guides_byrow = TRUE
    )
```

Interpretation:

- place the collected guide in the top-right corner
- keep each legend internally simple
- wrap multiple legend boxes into two columns

## Failure and Fallback Philosophy

The completed first release should not try to calculate whether the guide will
actually fit in the
 selected corner cell.

Instead, the design philosophy should be:

- provide a small number of compaction controls
- keep guide placement explicit
- if still too large, advise the user to use side placement

This is simpler and more honest than pretending the package can always solve
overflow automatically.

## Future Extension Path

Later versions may consider:

- optional `nrow`
- optional support for title omission or title compaction
- optional overflow warnings when guide area is likely too small
- integration with future multi-cell guide area regions
- optional guides-level spacing controls

These should remain follow-up features. They are not required for the completed
first release of `set_guide_layout()`.

## Acceptance Checklist

- add `guide_layout = NULL` in `as.aplot()`
- add exported `set_guide_layout()`
- add internal validation helper for guide layout parameters
- add internal render helper to apply a uniform single-legend policy to
  `plotlist`
- add internal helper for arranging multiple collected legend boxes
- ensure `set_guide_layout()` returns `NULL` configuration when all parameters
  are left at defaults
- ensure the helper applies the same guide policy to all guideable aesthetics in
  a plot
- ensure `guides_ncol` controls multiple collected guide boxes rather than the
  internal columns of a single legend
- ensure backward-compatible old parameter names still work during transition
- document the interaction with `set_guide_area()`
- add tests for parameter validation and the compact layout flow

## Recommendation

Proceed with a narrow completed first release:

- one helper: `set_guide_layout()`
- one role: arrange collected guides at two semantic levels
- no automatic fit logic
- no placement logic
- no generic theme forwarding

This keeps the semantics clean:

- `set_guide_area()` answers "where"
- `set_guide_layout()` answers "how arranged"
