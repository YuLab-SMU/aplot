# `set_guide_area()` Design Spec

## Status

- Final implementation target
- Scope: API, internal design, and acceptance criteria
- Target package: `aplot`
- Goal: finish `set_guide_area()` in one pass against this spec

## Background

`aplot` models a composition as one main plot plus associated plots inserted on
the top, bottom, left, and right sides. Corner cells are not formal subplots.
They are structural blanks represented by `NA` in `x$layout`, and are rendered
as spacers during `as.patchwork()`.

Issue `#13` asks whether the empty top-right region can be reused for legends.
That is a strong use case, but the feature should not weaken the semantic model
of `aplot` by turning corner cells into general-purpose subplot slots.

The first design draft exposed `corner` and later an intermediate `side`
argument. That shape worked for incremental coding, but it imposed unnecessary
API choices on users. The refined design collapses the user model to a single
question: where should collected guides go?

## Design Goal

Add a narrow object-level API that lets users declare guide placement by
position, while keeping layout expansion as an internal strategy.

This feature is about legend placement during rendering. It is not about adding
another aligned subplot type.

## Non-goals

- Do not turn corner cells into regular editable subplot slots.
- Do not add `insert_corner()`.
- Do not support arbitrary grobs or arbitrary plots in guide cells.
- Do not expose "open slot", "merge cells", or "span" as user-facing controls in
  the current design.
- Do not change axis alignment semantics.
- Do not introduce global options for this feature.

## User Model

The user still builds an `aplot` object using the existing `insert_*()`
functions. They then call `set_guide_area()` once to describe the desired guide
placement position.

Conceptually:

- `insert_*()` controls plot structure.
- `set_panel_spacing()` controls panel spacing.
- `set_guide_layout()` controls how collected guides are packed.
- `set_guide_area()` controls where collected guides are placed.

`set_guide_area()` is therefore an object-level rendering configuration, not a
subplot insertion API.

## Proposed API

```r
set_guide_area <- function(x, position = c(
    "top-right",
    "top-left",
    "bottom-right",
    "bottom-left",
    "right",
    "left",
    "bottom",
    "top"
))
```

## Rationale

- `position` is the only decision the user should make.
- Whether `aplot` reuses an empty corner or opens a guide strip is an internal
  layout strategy, not a user-facing concept.
- This keeps the API aligned with the most common intent: "put the legend here".

## Target Behavior

### Corner positions

- `top-left`
- `top-right`
- `bottom-left`
- `bottom-right`

If the requested corner cell exists and is empty, `as.patchwork()` replaces that
cell with `patchwork::guide_area()`.

### Side-strip positions

- `right`
- `left`
- `bottom`
- `top`

These positions always create a new guide strip on the corresponding side by
extending `x$layout`.

### Corner fallback

When a requested corner is occupied, fallback should be deterministic rather
than adaptive.

Required mapping:

- `top-right` -> `right`
- `bottom-right` -> `right`
- `top-left` -> `left`
- `bottom-left` -> `left`

This keeps the rule simple:

- corner positions collapse to the corresponding vertical side;
- explicit `top` and `bottom` remain user-declared strip positions rather than
  implicit fallback targets.

This is preferable to a more adaptive policy such as "`top-right` may fall back
to either `top` or `right`", because such a rule would make guide placement
harder to predict.

### No-guide case

If guides are not collected, `guide_area()` behaves like a spacer. No special
handling is required.

### Invalid case

The function still fails fast when:

- `x` is not an `aplot` object;
- `position` is invalid;
- layout structure is malformed and cannot be resolved.

Recommended error style for malformed or unsupported layout states:

```r
stop("The selected corner is not an empty outer cell.")
```

## Final Scope

The completed first release should support:

- only one guide area designation per `aplot` object;
- explicit side-strip positions `"right"`, `"left"`, `"bottom"`, and `"top"`;
- deterministic corner fallback for both left and right corners;
- no implicit fallback from corners to `top` or `bottom`;
- no merged multi-cell guide regions.

## Internal Object Design

`aplot` stores the user declaration in:

```r
x$guide_area
```

Current shape:

```r
list(
    position = "top-right"
)
```

Resolved rendering metadata is produced later by `.resolve_guide_area(x)`.
Repeated calls overwrite the previous setting.

## Validation Rules

Internal validator:

```r
.validate_guide_area_position <- function(position)
```

Rules:

- `position` must be length 1;
- `position` must be one of:
  - `"top-right"`
  - `"top-left"`
  - `"bottom-right"`
  - `"bottom-left"`
  - `"right"`
  - `"left"`
  - `"bottom"`
  - `"top"`

Internal resolver:

```r
.resolve_guide_area <- function(x)
```

Responsibilities:

- return `NULL` when no guide area has been set;
- interpret `position`;
- reuse an empty corner when appropriate;
- apply deterministic corner fallback when appropriate;
- open a side strip when appropriate;
- return enough information for rendering, including:
  - `mode`
  - `position`
  - `layout`
  - `guide_indices`
  - optional strip `width`
  - optional strip `height`

## Mapping Rules

Corner mapping is fixed by matrix position:

- `top-left` -> `(1, 1)`
- `top-right` -> `(1, ncol(layout))`
- `bottom-left` -> `(nrow(layout), 1)`
- `bottom-right` -> `(nrow(layout), ncol(layout))`

Side-strip mapping is defined by extending the layout:

- left: `cbind(rep(NA, nrow(layout)), layout)`
- `extended_layout <- cbind(layout, rep(NA, nrow(layout)))`
- bottom: `rbind(layout, rep(NA, ncol(layout)))`
- top: `rbind(rep(NA, ncol(layout)), layout)`
- guide indices point to the new edge column or row of the extended layout

Corner fallback mapping is defined independently of layout shape:

- `top-right` -> `right`
- `bottom-right` -> `right`
- `top-left` -> `left`
- `bottom-left` -> `left`

The fallback rule is semantic rather than geometric. It does not attempt to
choose the visually largest free edge.

## Rendering Design

Current rendering in `as.patchwork()` works like this:

1. Resolve spacing, guide area, and guide layout.
2. Use `guide_area_spec$layout` when guide placement needs an expanded layout.
3. Build `idx <- as.vector(layout)`.
4. Add one blank spacer plot.
5. If a guide area is active:
   - add one `patchwork::guide_area()` placeholder to `plotlist`;
   - replace the resolved `guide_indices` in `idx` with that guide-area index;
   - replace remaining `NA` entries with the blank spacer index.
6. Compose the plot list and call `plot_layout()`.

Important invariant:

- guide placement changes rendering layout only;
- it does not create a new editable subplot;
- all non-target empty cells remain spacers.

## Alignment Semantics

`set_guide_area()` must not:

- participate in `xlim2()` propagation;
- participate in `ylim2()` propagation;
- alter `main_row` or `main_col`;
- alter subplot accessor semantics;
- behave as a regular plot in `[`, `[[`, `[<-`, or `[[<-`.

If a side strip is opened, `as.patchwork()` may prepend or append one extra
entry to the `width` or `height` vector. This is a rendering-side layout
adjustment, not a change in axis semantics.

## Interaction With Guide Collection

`aplot` passes `getOption("aplot_guides", default = "collect")` into
`plot_layout()`.

Implications:

- if guides are collected, the guide area becomes active;
- if guides are kept, the guide area stays visually empty.

This is acceptable and should remain documented.

## Function Contract

### Input

- `x`: an `aplot` object
- `position`: a supported guide-placement position

### Output

- returns an updated `aplot` object

### Failure

The function should `stop()` when:

- `x` is not an `aplot` object;
- `position` is invalid;
- internal layout resolution fails unexpectedly.

## Suggested Documentation Notes

The help page should explain:

- this function places collected guides by position;
- it does not add a new subplot;
- `right`, `left`, `bottom`, and `top` open guide strips on the corresponding side;
- corner positions first try the requested corner cell;
- if that corner is occupied, fallback follows a fixed rule:
  - right corners -> `right`
  - left corners -> `left`
- explicit `top` and `bottom` are not implicit corner fallbacks;
- if guides are not collected, the area behaves like a spacer.

## Example Scenarios

### Empty top-right corner

```r
ap <- p |>
    insert_top(p_top, height = 0.3) |>
    insert_right(p_right, width = 0.2) |>
    set_guide_area("top-right")
```

Result:

- the existing top-right empty corner is used;
- other empty cells remain blank.

### Explicit right-side strip

```r
ap <- p |>
    insert_right(p_right, width = 0.25) |>
    set_guide_area("right")
```

Result:

- `aplot` opens a new right-side strip;
- collected guides are rendered there.

### Explicit left-side strip

```r
ap <- p |>
    insert_left(p_left, width = 0.25) |>
    set_guide_area("left")
```

Result:

- `aplot` opens a new left-side strip;
- collected guides are rendered there.

### Explicit top strip

```r
ap <- p |>
    insert_bottom(p_bottom, height = 0.25) |>
    set_guide_area("top")
```

Result:

- `aplot` opens a new top strip;
- collected guides are rendered there.

### Occupied bottom-right corner

```r
ap <- p |>
    insert_top(p_top, height = 0.3) |>
    insert_right(p_right, width = 0.25) |>
    set_guide_area("bottom-right")
```

Result:

- the bottom-right corner is occupied by the right subplot;
- guide placement falls back to a right-side strip.

### Occupied top-left corner

```r
ap <- p |>
    insert_bottom(p_bottom, height = 0.25) |>
    insert_left(p_left, width = 0.25) |>
    set_guide_area("top-left")
```

Result:

- if the top-left corner is occupied;
- guide placement falls back to a left-side strip rather than to `top`.

## Future Extension Path

Possible follow-up extensions after this spec is complete:

- optional multi-cell guide regions
- optional overflow-aware warnings
- revisiting whether `top` / `bottom` should ever become implicit fallback
  targets

Merged regions should remain a separate design step because they introduce new
questions:

- how to identify a valid continuous empty region;
- whether region size should be automatic or user-declared;
- whether fallback should remain implicit;
- how to preserve the simple single-parameter user model.

## Acceptance Checklist

- keep `guide_area = NULL` in `as.aplot()`;
- expose `set_guide_area(x, position)`;
- validate `position`;
- resolve either:
  - an existing corner cell, or
  - a side strip;
- implement corner fallback for:
  - `top-right` -> `right`
  - `bottom-right` -> `right`
  - `top-left` -> `left`
  - `bottom-left` -> `left`
- update `as.patchwork()` to use resolved layout metadata;
- add tests for:
  - valid corner placement
  - explicit side-strip placement
  - corner fallback
  - invalid positions
- update Rd documentation;
- keep NEWS aligned with current behavior.

## Open Questions

- Should strip width/height remain fixed internally, or later become a
  style-level option rather than a placement argument?
- Should multi-cell guide regions reuse this same `position` API or require a
  separate helper?

## Recommendation

Proceed with the refined version:

- one function: `set_guide_area()`;
- one user-facing option: `position`;
- one user intent: guide placement;
- internal automatic strategy with deterministic corner-to-side fallback.

This keeps the API simple, preserves the core `aplot` model, and leaves room
for future edge positions without increasing user cognitive load.
