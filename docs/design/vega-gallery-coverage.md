# Vega gallery coverage

Every example of the Vega gallery (https://vega.github.io/vega/examples/)
has an eas template, `templates/vega/NAME.json`, with an example binding
`examples/vega/NAME.data.json` that reproduces it from the vendored data in
`test/vega-examples/data`.  Each template takes its data and fields as slots,
so a caller binds their own data: `eas-resolve "vega/NAME" BINDINGS`.

Status is each template's `x-eas.vega` verdict, against
`test/vega-examples/ref/NAME.png`; `manifest.json` carries the same per
example.  A pass is within the template's recorded pixel threshold of the
reference; a partial renders and works but misses something, named in its
note.  The ratios are measured against the vg2png (node-canvas) references
with rsvg-convert and Arimo; see "Re-measured against the canvas references"
for how they moved from the first (vg2svg) references.

| category | pass | partial | examples |
|---|--:|--:|--:|
| Bar Charts | 5 | 0 | 5 |
| Line & Area Charts | 4 | 1 | 5 |
| Circular Charts | 5 | 0 | 5 |
| Scatter Plots | 6 | 2 | 8 |
| Distributions | 14 | 1 | 15 |
| Geographic Maps | 7 | 3 | 10 |
| Tree Diagrams | 5 | 0 | 5 |
| Network Diagrams | 3 | 2 | 5 |
| Other Chart Types | 5 | 3 | 8 |
| Custom Designs | 11 | 2 | 13 |
| Interaction Techniques | 10 | 5 | 15 |
| **all** | **75** | **19** | **94** |

## Partial

- **job-voyager** (Line & Area Charts): Stacked areas, per-series opacity, peak labels (sized, faded by quantile, aligned by year) and the sex and search filters match; clicking an area to search for its job is not reproduced, and area seams and label fonts differ slightly. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0675 (was 0.0716 against vg2svg).
- **barley-trellis-plot** (Scatter Plots): Every panel, point, dashed grid, both side axes and the legend are drawn, but rows drift a pixel from Vega's (scale round: true is not reproduced), which the thin dashed grid magnifies. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0890 (was 0.0863 against vg2svg).
- **labeled-scatter-plot** (Scatter Plots): Points, trend and axes match; the label transform places labels clear of points, trend and each other, but not label for label as Vega's bitmap does. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0334 (was 0.049 against vg2svg); the canvas reference draws Vega's label transform (the vg2svg one left every label at the origin); labels are placed clear of points and each other but not label for label.
- **time-units** (Distributions): Weekday bars, axis, title and italic subtitle match and hover fills a bar firebrick, but the band scale is not rounded (scale.round is not drawn natively), so bar edges sit up to a pixel off. The time unit and measure selects are slots (unit, format, op). Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0376 (was 0.0355 against vg2svg); the template declares Vega's autosize fit (600x300, padding included) instead of a 549x230 plot, so the canvas matches; the example keeps label_extent 23, nearer Vega's.
- **dorling-cartogram** (Geographic Maps): Circles, labels and colors match Vega (2.4% of pixels differ); the binding carries each state's d3 geoCentroid (eas reads no TopoJSON; scripts/eas-vega-network-data.mjs) and the force transform projects it with albersUsa at scale 1100. The legend is a color gradient: Vega's merged size-and-fill symbol legend is not drawn. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0240 (was 0.0242 against vg2svg).
- **projections** (Geographic Maps): all 24 gallery projections draw natively and their country and sphere paths equal d3-geo-projection's to 1e-9 (peirceQuincuncial's graticule meridians resample differently near its poles); against ref/projections.png, the Vega site's 360 px thumbnail, the 940x1740 render scaled to it differs by ratio 0.0747 (resvg); Vega clips the graticule and countries to each projection's sphere outline, which Vega-Lite cannot express, so the interrupted projections draw the parts of countries their cuts split across the gaps. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0791.
- **annual-precipitation** (Geographic Maps): Countries from TopoJSON and isocontours of the lon/lat grid (500..2500 mm) through geopath under naturalEarth1 (scale 110, translate [300 150]), quantized bluepurple legend labelled 500..2,500 with its title on the left: the map is pixel-identical to ref/annual-precipitation.png with the legend masked (ratio 0.0), 0.032 overall because Vega centres the legend (config.legend.layout, which Vega-Lite cannot express) and truncates its title at titleLimit; legend titleAnchor is reported unsupported. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0292 (was 0.0323 against vg2svg).
- **edge-bundling** (Network Diagrams): Bundled links and leaf labels match Vega. Hovering a label draws its links in colorOut/colorIn as Vega does, but the linked labels are not bolded or recolored (Vega's indata tests), and the tension, radius, extent, rotate, text and layout widgets are slots. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0350 (was 0.02 against vg2svg); geometry matches; the rest is the font-raster floor: the references' sans-serif is Helvetica (node-canvas on macOS), native SVG here is drawn in Arimo (Arial metrics), so glyph edges differ while positions match.
- **airport-connections** (Network Diagrams): Airports, their sizes, the fixed albersUsa projection (scale 1200), the Voronoi cells (cells slot) and the routes of the hovered airport match Vega; the state shapes behind them are TopoJSON geopaths, which eas does not draw, hence 46% of pixels differ. The title does not follow the hovered airport. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.4578 (was 0.458 against vg2svg).
- **density-heatmaps** (Other Chart Types): Faceted kde2d heatmaps (viridis of value/max per Origin, smoothed images), bold headers, axes and the 0..1 gradient legend match; the legend title is one line (Vega-Lite's array titles are not drawn natively). Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0344 (was 0.0946 against vg2svg); it was measured against the site thumbnail; the reference is now full size, and the template carries Vega's axis defaults, 13 px headers and Vega's 10 y ticks. Still partial: the legend title is one line where Vega draws two, so the canvas is 75 px wider.
- **parallel-coordinates-interactive** (Other Chart Types): Every dimension gets its own nice axis, computed in transforms (fold, normalize, ticks flattened per axis), coloured by Horsepower on Vega's turbo extent. Clicking a line highlights it; Vega's per-axis brushes and draggable axes are not expressible in Vega-Lite and are not ported. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0524 (was 0.0619 against vg2svg).
- **word-cloud** (Other Chart Types): Reproduces the example's pipeline natively: countpattern with Vega's pattern and stopwords, sqrt font sizes 12-56px, angles picked from -45/0/45, VEGA in weight 600, the three-color ordinal range in data order, and hover fading the word by half (a white copy at opacity 0.5 over it, as fillOpacity 0.5 looks on white). Vega places words with Math.random, so pixels cannot match the reference: the layout is checked structurally (no two padded word boxes overlap, all inside 800x400) and its ink against the reference's (within a factor of two). Placement is seeded and collides measured text boxes rather than glyph sprites, so it is looser: 135 of 194 words fit. The canvas is 805x400, Vega-style text bounds overhanging the 800px plot. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.2919; Vega places words with Math.random, so the ratio measures a different layout; the structural checks stand.
- **flight-passengers** (Custom Designs): line, points, signed percent axis and dashed zero rule match. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0477 (was 0.0615 against vg2svg); the comparison no longer emulates vg2svg's text widths; the line still sits about 2 px higher than Vega's, as the fitted plot is a little shorter.
- **earthquakes-globe** (Custom Designs): the globe (orthographic, #000022 sphere, translucent white countries), the magnitude rings (stroke colored on Vega's 8-color linear scale, radius circle_size * sqrt(exp(mag) / 100), width 1 + 0.15 r, far side filtered as the circle clip hides it), the title and subtitle, and the one symbol legend of magnitudes 1-9 sized and colored as the rings (a size and a color of one field merge, as in Vega-Lite) match ref/earthquakes-globe.png (ratio 0.0232, resvg; canvas 626x658 against 625x658); not reproduced: the stars' positions (random in Vega; x random and y a hash of it here, as eas's random() repeats within a row), the sphere's gradient stroke (its midpoint color), the legend's row spacing by symbol size, the Tahoma font, and the timer rotation and click-to-pause (the angle is a range input). Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0170.
- **zoomable-binned-plot** (Interaction Techniques): The density heatmap and the points pan and zoom together (a param bound to scales), but the bins stay those of the full extent: Vega re-bins the visible domain while zooming, which Vega-Lite cannot express. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0318 (was 0.0317 against vg2svg).
- **zoomable-circle-packing** (Interaction Techniques): Native SVG vs test/vega-examples/ref at plot origin: differing-pixel ratio 0.0090 (Vega's own SVG through the same rasterizer: n/a (timer)). The initial view matches. Clicking a circle opens a zoomed view (the zoom-subtree action re-lays its subtree out, which is d3's zoom geometry) and clicking the focused circle zooms out, but without Vega's animated transition, the shift-click details panel or the slow-motion modifier; the help text describes eas's clicks. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0109 (was 0.015 against vg2svg).
- **map-with-tooltip** (Interaction Techniques): the map and its legend match ref/map-with-tooltip.png: ratio 0.0156 at 960x530 (resvg), mercator at scale 900 centred on [-95, 38]; Vega's 30 px top padding is a 530 px view with the map 30 px lower; the tooltip is Vega-Lite's own (the county's key and rate): Vega's custom tooltip, a rounded group following the pointer that redraws the hovered county under a second projection centred on its inverted centroid, has no Vega-Lite form. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0073.
- **pacman** (Interaction Techniques): The first frame matches. The arrows steer the eater on a 500 ms timer (x-eas.timer, x-eas.on); gums score 10 and power gums 50; ghosts chase along the longer axis and wander when blocked; a ghost on the eater restarts the game. Not ported: power mode (fleeing, edible ghosts), Vega's ghost decision tables and ghost shapes (drawn as discs). Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.0115 (was 0.0114 against vg2svg).
- **platformer** (Interaction Techniques): The first frame matches (the binding precomputes Vega's hsl terrain colours and a solid-cell grid). Gravity, landing, running (left, right), jumping (up) and dashing (z) run on a 50 ms timer; Emacs reports no key release, so a press runs for 6 frames. Not ported: the background image, the dash trail, Vega's exact collision response and its 17 ms frames. Against the vg2png (node-canvas) reference, re-measured in eas-7r1.12 with rsvg-convert and Arimo: ratio 0.3595 (was 0.0 against vg2svg); the canvas reference is not Vega's intended frame: node-canvas ignores the terrain's d3 hsl() fill objects (non-string fillStyle), so the blocks keep a stale dark fill, and it fetched the remote background image, which eas never downloads. The threshold covers that reference.

## Re-measured against the canvas references (eas-7r1.12)

The first references came from vg2svg without node-canvas, so Vega laid
them out with estimated text widths (0.8 em a character); several
templates, examples and oracles had been fitted to those layouts.  The
references are now vg2png renders with node-canvas, which measures real
glyphs.  Every :gallery comparison was rerun against them: "recorded" is
the ratio the template held against vg2svg, "before" the same template
against the canvas reference before this bead's fixes, "after" with them.
The bound is the template's threshold or the rule its test applies.

The font-raster floor: the references' sans-serif is Helvetica (node-canvas
on macOS) and native SVG is rasterized here with Arimo (Arial's metrics,
so positions agree), so label glyph edges differ.  Text-heavy charts sit on
it: 0.03 for top-k-plot's 20 labels and axis, 0.035 to 0.05 for the radial
tree, edge bundling, treemap and the 77x77 labelled matrix, 0.12 for the
tree layout's 252 labels, whose geometry is identical.

Engine causes fixed, which reach user charts too:

- `autosize` fit, fit-x and fit-y (contains content or padding) size a
  single view's whole chart, overhanging labels and marks included, to
  the spec's width and height (src/eas-container.el); before, autosize
  was ignored and the plot took the spec's size.
- An axis with an offset honours labelFlush false in SVG: its end labels
  were drawn flush while the layout centred them (src/eas-axis.el).
- Row facets sized the room left of their plots as the shared y axis
  plus whatever the marks overhang; Vega takes the farther of the two
  (src/eas-facet-layout.el).
- `eas-png-compare` aligned a transparent reference with an opaque
  native image by ink profiles that counted every white pixel as ink;
  ink is now judged composited over white, as the pixel count already
  was (src/eas-png.el).  84 of the 94 references are transparent.

| example | recorded (vg2svg) | before (canvas ref) | after | bound | cause |
|---|--:|--:|--:|--:|---|
| bar-chart | 0.0394 | 0.0393 | 0.0393 | 0.06 | noise, unchanged |
| stacked-bar-chart | 0.0233 | 0.0228 | 0.0228 | 0.05 | noise, unchanged |
| grouped-bar-chart | 0.0172 | 0.0172 | 0.0172 | 0.05 | noise, unchanged |
| nested-bar-chart | 0.0196 | 0.0176 | 0.0176 | 0.05 | noise, unchanged |
| population-pyramid | 0.0465 | 0.0493 | 0.0493 | 0.07 | noise, unchanged |
| line-chart | 0.0054 | 0.0067 | 0.0067 | 0.05 | noise, unchanged |
| area-chart | 0.0098 | 0.0117 | 0.0117 | 0.05 | noise, unchanged |
| stacked-area-chart | 0.0068 | 0.0075 | 0.0075 | 0.05 | noise, unchanged |
| horizon-graph | 0.0149 | 0.0150 | 0.0150 | 0.05 | noise, unchanged |
| job-voyager | 0.0716 | 0.0675 | 0.0675 | 0.1 | noise, unchanged |
| pie-chart | 0.0044 | 0.0040 | 0.0040 | 0.05 | noise, unchanged |
| donut-chart | 0.0031 | 0.0028 | 0.0028 | 0.05 | noise, unchanged |
| donut-chart-labelled | 0.0160 | 0.0160 | 0.0160 | 0.05 | noise, unchanged |
| radial-plot | 0.0059 | 0.0034 | 0.0034 | 0.05 | noise, unchanged |
| radar-chart | 0.0093 | 0.0057 | 0.0057 | 0.05 | noise, unchanged |
| scatter-plot | 0.0384 | 0.0340 | 0.0340 | 0.06 | noise, unchanged |
| scatter-plot-null-values | 0.0367 | 0.1339 | 0.0078 | 0.06 | autosize fit 450x450 instead of a 369x390 plot; SVG honours labelFlush false on offset axes (engine) |
| connected-scatter-plot | 0.0347 | 0.0301 | 0.0301 | 0.06 | noise, unchanged |
| error-bars | 0.0226 | 0.0227 | 0.0227 | 0.05 | noise, unchanged |
| barley-trellis-plot | 0.0863 | 0.0890 | 0.0890 | 0.11 | noise, unchanged |
| regression | 0.0047 | 0.0045 | 0.0045 | 0.05 | noise, unchanged |
| loess-regression | 0.0047 | 0.0046 | 0.0046 | 0.05 | noise, unchanged |
| labeled-scatter-plot | 0.0490 | 0.0334 | 0.0334 | 0.07 | reference now draws Vega's labels; note corrected (partial) |
| top-k-plot | 0.0134 | 0.1252 | 0.0313 | 0.04 | stale label_extent 135 / width 309 binding; now autosize fit 500x410 (engine: autosize fit); label glyphs remain, threshold 0.04 |
| top-k-plot-with-others | 0.0133 | 0.1257 | 0.0306 | 0.04 | as top-k-plot; threshold 0.04 for label glyphs |
| histogram | 0.0244 | 0.0273 | 0.0277 | 0.05 | stale label_extent 31 dropped (canvas now exact) |
| histogram-null-values | 0.0207 | 0.0440 | 0.0440 | 0.05 | noise, unchanged |
| dot-plot | 0.0077 | 0.0150 | 0.0150 | 0.03 | noise, unchanged |
| probability-density | 0.0223 | 0.0215 | 0.0215 | 0.05 | noise, unchanged |
| box-plot | 0.0100 | 0.0550 | 0.0131 | 0.03 | stale label_extent 79 binding dropped |
| violin-plot | 0.0098 | 0.0740 | 0.0121 | 0.03 | stale label_extent 79 binding dropped |
| binned-scatter-plot | 0.0293 | 0.0443 | 0.0400 | 0.06 | stale label_extent 23 dropped (canvas now exact) |
| contour-plot | 0.0147 | 0.1822 | 0.0078 | 0.03 | was scaled to the 360 px thumbnail; full-size ref, Vega axis defaults added |
| wheat-plot | 0.0221 | 0.0223 | 0.0223 | 0.05 | noise, unchanged |
| quantile-quantile-plot | 0.0091 | 0.0932 | 0.0174 | 0.03 | stale label_extent 39 and spacing 21 dropped |
| quantile-dot-plot | 0.0081 | 0.0155 | 0.0155 | 0.03 | noise, unchanged |
| hypothetical-outcome-plots | 0.1239 | 0.1143 | 0.1143 | 0.17 | Math.random draws |
| time-units | 0.0355 | 0.0365 | 0.0376 | 0.08 | autosize fit 600x300 (padding) instead of a fitted plot size |
| county-unemployment |  | 0.0078 | 0.0078 | 0.03 | noise, unchanged |
| dorling-cartogram | 0.0242 | 0.0240 | 0.0240 | ratio+0.02 | noise, unchanged |
| world-map |  | 0.0000 | 0.0000 | 0.03 | noise, unchanged |
| earthquakes |  | 0.0067 | 0.0067 | 0.03 | noise, unchanged |
| projections |  | 0.0791 | 0.0791 | 0.09 | noise, unchanged |
| zoomable-world-map |  | 0.0000 | 0.0000 | 0.03 | noise, unchanged |
| distortion-comparison |  | 0.0000 | 0.0000 | 0.03 | noise, unchanged |
| volcano-contours | 0.0036 | 0.0024 | 0.0024 | 0.03 | noise, unchanged |
| wind-vectors | 0.0001 | 0.0000 | 0.0000 | 0.03 | noise, unchanged |
| annual-precipitation | 0.0323 | 0.0292 | 0.0292 | 0.05 | noise, unchanged |
| tree-layout | 0.0200 | 0.1230 | 0.1230 | ratio+0.005 | font raster floor (Helvetica vs Arimo), geometry identical |
| radial-tree-layout | 0.0250 | 0.0466 | 0.0466 | ratio+0.005 | font raster floor |
| treemap | 0.0400 | 0.0348 | 0.0348 | ratio+0.005 | font raster floor |
| circle-packing | 0.0050 | 0.0000 | 0.0000 | ratio+0.005 | noise, unchanged |
| sunburst | 0.0050 | 0.0000 | 0.0000 | ratio+0.005 | noise, unchanged |
| edge-bundling | 0.0200 | 0.0350 | 0.0350 | ratio+0.005 | font raster floor |
| force-directed-layout | 0.0001 | 0.0000 | 0.0000 | ratio+0.02 | noise, unchanged |
| reorderable-matrix | 0.0321 | 0.1562 | 0.0457 | ratio+0.02 | stale offset [55 55] → [0 0]; font raster floor |
| arc-diagram | 0.0448 | 0.0476 | 0.0534 | ratio+0.02 | height 436 → 387 (canvas now Vega's height); smaller blank denominator |
| airport-connections | 0.4580 | 0.4578 | 0.4578 | ratio+0.02 | noise, unchanged |
| heatmap | 0.0083 | 0.0604 | 0.0158 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| density-heatmaps | 0.0946 | 0.5442 | 0.0344 | 0.12 | was scaled to the thumbnail; full-size ref, Vega axis/header defaults; one-line legend title remains (partial) |
| parallel-coordinates | 0.0236 | 0.3497 | 0.0222 | 1.25·ratio+0.005 | comparison emulated vg2svg text widths; ink alignment on transparent refs (engine: eas-png) |
| parallel-coordinates-interactive | 0.0619 | 0.0524 | 0.0524 | 0.09 | noise, unchanged |
| word-cloud |  | 0.2919 | 0.2919 | structural | Math.random placement; structural checks only |
| beeswarm-plot | 0.0133 | 0.0126 | 0.0126 | ratio+0.02 | noise, unchanged |
| calendar-view | 0.0135 | 0.0304 | 0.0156 | 1.25·ratio+0.005 | facet rows summed axis width and mark overhang (engine: eas-facet-layout) |
| packed-bubble-chart | 0.0012 | 0.0013 | 0.0013 | ratio+0.02 | noise, unchanged |
| budget-forecasts | 0.0021 | 0.0023 | 0.0024 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| wheat-and-wages | 0.0101 | 0.0102 | 0.0103 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| falkensee-population | 0.0211 | 0.1198 | 0.0291 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| annual-temperature | 0.0019 | 0.0545 | 0.0085 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| weekly-temperature | 0.0256 | 0.0282 | 0.0286 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| flight-passengers | 0.0615 | 0.0717 | 0.0477 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths (partial: plot ~2 px shorter) |
| timelines | 0.0074 | 0.1421 | 0.0107 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| u-district-cuisine | 0.0286 | 0.0287 | 0.0381 | 0.045 | comparison no longer emulates vg2svg text widths; the reference's Avenir Next is not on this box (font floor), pass threshold 0.045 |
| clock | 0.0486 | 0.0637 | 0.0540 | 0.08 | re-pinned to the reference's 04:14:50 |
| watch | 0.0184 | 0.0440 | 0.0188 | 0.04 | re-pinned to the reference's 04:14:29 |
| warming-stripes | 0.0220 | 0.0274 | 0.0285 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| earthquakes-globe |  | 0.0170 | 0.0170 | 0.05 | noise, unchanged |
| serpentine-timeline | 0.0084 | 0.0492 | 0.0151 | 1.25·ratio+0.005 | comparison no longer emulates vg2svg text widths |
| crossfilter-flights | 0.0645 | 0.0658 | 0.0658 | 0.1 | noise, unchanged |
| overview-plus-detail | 0.0197 | 0.0220 | 0.0220 | 0.04 | noise, unchanged |
| brushing-scatter-plots | 0.0514 | 0.0587 | 0.0587 | 0.08 | noise, unchanged |
| zoomable-scatter-plot | 0.0121 | 0.0121 | 0.0121 | 0.03 | noise, unchanged |
| zoomable-binned-plot | 0.0317 | 0.0318 | 0.0318 | 0.05 | noise, unchanged |
| global-development | 0.0034 | 0.0031 | 0.0031 | 0.02 | noise, unchanged |
| interactive-legend | 0.0351 | 0.0404 | 0.0404 | 0.06 | noise, unchanged |
| stock-index-chart | 0.0345 | 0.0798 | 0.0512 | 0.06 | plot width 555 → 562 (real label widths) |
| pi-monte-carlo | 0.1731 | 0.1759 | 0.1759 | 0.23 | Math.random points |
| zoomable-circle-packing | 0.0150 | 0.0109 | 0.0109 | ratio+0.005 | noise, unchanged |
| table-scrollbar | 0.0557 | 0.0475 | 0.0475 | 0.08 | noise, unchanged |
| bar-line-toggle | 0.0457 | 0.2931 | 0.0379 | 0.07 | Math.random values re-bound to the new reference's |
| map-with-tooltip |  | 0.0073 | 0.0073 | 0.03 | noise, unchanged |
| pacman | 0.0114 | 0.0115 | 0.0115 | 0.03 | noise, unchanged |
| platformer | 0.0000 | 0.3595 | 0.3595 | 0.37 | reference artifact: node-canvas ignores d3 hsl() fills and draws the remote background image; threshold 0.37 (partial) |

## Beyond Vega-Lite

Templates that need what Vega-Lite lacks use x-eas transforms and handlers
(listed in `src/supported.json` under extensions): countpattern and wordcloud,
stratify/tree/cluster/treemap/partition/pack and linkpath, force and voronoi,
dotbin, kde2d and contour, the timer and key handlers of `x-eas.on`.  They
resolve to Vega-Lite with those materialized, so bin/chart can export them,
except `vega/projections`, whose d3-geo-projection projections Vega-Lite has
no names for (`x-eas.export`).
