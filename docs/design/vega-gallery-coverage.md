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
note.  The verdicts were recorded against the first (vg2svg) references;
the references are now rendered with node-canvas, and eas-7r1.12 re-measures
every ratio against them.

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

- **job-voyager** (Line & Area Charts): Stacked areas, per-series opacity, peak labels (sized, faded by quantile, aligned by year) and the sex and search filters match; clicking an area to search for its job is not reproduced, and area seams and label fonts differ slightly.
- **barley-trellis-plot** (Scatter Plots): Every panel, point, dashed grid, both side axes and the legend are drawn, but rows drift a pixel from Vega's (scale round: true is not reproduced), which the thin dashed grid magnifies.
- **labeled-scatter-plot** (Scatter Plots): The reference is defective: vg2svg could not run Vega's label transform without canvas, so every label sits at the origin and the axes are misplaced. Points, trend and axes match the live example; the label transform places labels clear of points, trend and each other, but not label for label as Vega's bitmap does.
- **time-units** (Distributions): Weekday bars, axis, title and italic subtitle match and hover fills a bar firebrick, but the band scale is not rounded (scale.round is not drawn natively), so bar edges sit up to a pixel off. The time unit and measure selects are slots (unit, format, op). Ratio 0.0355 at best alignment with resvg here.
- **dorling-cartogram** (Geographic Maps): Circles, labels and colors match Vega (2.4% of pixels differ); the binding carries each state's d3 geoCentroid (eas reads no TopoJSON; scripts/eas-vega-network-data.mjs) and the force transform projects it with albersUsa at scale 1100. The legend is a color gradient: Vega's merged size-and-fill symbol legend is not drawn.
- **projections** (Geographic Maps): all 24 gallery projections draw natively and their country and sphere paths equal d3-geo-projection's to 1e-9 (peirceQuincuncial's graticule meridians resample differently near its poles); against ref/projections.png, the Vega site's 360 px thumbnail, the 940x1740 render scaled to it differs by ratio 0.0747 (resvg); Vega clips the graticule and countries to each projection's sphere outline, which Vega-Lite cannot express, so the interrupted projections draw the parts of countries their cuts split across the gaps
- **annual-precipitation** (Geographic Maps): Countries from TopoJSON and isocontours of the lon/lat grid (500..2500 mm) through geopath under naturalEarth1 (scale 110, translate [300 150]), quantized bluepurple legend labelled 500..2,500 with its title on the left: the map is pixel-identical to ref/annual-precipitation.png with the legend masked (ratio 0.0), 0.032 overall because Vega centres the legend (config.legend.layout, which Vega-Lite cannot express) and truncates its title at titleLimit; legend titleAnchor is reported unsupported.
- **edge-bundling** (Network Diagrams): Native SVG vs test/vega-examples/ref at plot origin: differing-pixel ratio 0.0148 (Vega's own SVG through the same rasterizer: 0.0128). Bundled links and leaf labels match Vega. Hovering a label draws its links in colorOut/colorIn as Vega does, but the linked labels are not bolded or recolored (Vega's indata tests), and the tension, radius, extent, rotate, text and layout widgets are slots.
- **airport-connections** (Network Diagrams): Airports, their sizes, the fixed albersUsa projection (scale 1200), the Voronoi cells (cells slot) and the routes of the hovered airport match Vega; the state shapes behind them are TopoJSON geopaths, which eas does not draw, hence 46% of pixels differ. The title does not follow the hovered airport.
- **density-heatmaps** (Other Chart Types): Faceted kde2d heatmaps (viridis of value/max per Origin, smoothed images), bold headers, axes and the 0..1 gradient legend match; the legend title is one line (Vega-Lite's array titles are not drawn natively), so the chart is about 60 px wider than Vega's: ratio 0.095 against the site thumbnail at its height, 0.031 at the scale that aligns the plots with the legend masked.
- **parallel-coordinates-interactive** (Other Chart Types): Every dimension gets its own nice axis, computed in transforms (fold, normalize, ticks flattened per axis), coloured by Horsepower on Vega's turbo extent. Clicking a line highlights it; Vega's per-axis brushes and draggable axes are not expressible in Vega-Lite and are not ported. Pixel ratio against ref/parallel-coordinates-interactive.png: 0.062.
- **word-cloud** (Other Chart Types): Reproduces the example's pipeline natively: countpattern with Vega's pattern and stopwords, sqrt font sizes 12-56px, angles picked from -45/0/45, VEGA in weight 600, the three-color ordinal range in data order, and hover fading the word by half (a white copy at opacity 0.5 over it, as fillOpacity 0.5 looks on white). Vega places words with Math.random, so pixels cannot match the reference: the layout is checked structurally (no two padded word boxes overlap, all inside 800x400) and its ink against the reference's (within a factor of two). Placement is seeded and collides measured text boxes rather than glyph sprites, so it is looser: 135 of 194 words fit. The canvas is 805x400, Vega-style text bounds overhanging the 800px plot.
- **flight-passengers** (Custom Designs): line, points, signed percent axis and dashed zero rule match; the reference's autosize fit shrinks the plot to about 170 px to make room for its title as vg2svg over-estimates it (0.8 em a character), which eas's container fit does not reproduce
- **earthquakes-globe** (Custom Designs): the globe (orthographic, #000022 sphere, translucent white countries), the magnitude rings (stroke colored on Vega's 8-color linear scale, radius circle_size * sqrt(exp(mag) / 100), width 1 + 0.15 r, far side filtered as the circle clip hides it), the title and subtitle, and the one symbol legend of magnitudes 1-9 sized and colored as the rings (a size and a color of one field merge, as in Vega-Lite) match ref/earthquakes-globe.png (ratio 0.0232, resvg; canvas 626x658 against 625x658); not reproduced: the stars' positions (random in Vega; x random and y a hash of it here, as eas's random() repeats within a row), the sphere's gradient stroke (its midpoint color), the legend's row spacing by symbol size, the Tahoma font, and the timer rotation and click-to-pause (the angle is a range input)
- **zoomable-binned-plot** (Interaction Techniques): The density heatmap and the points pan and zoom together (a param bound to scales), but the bins stay those of the full extent: Vega re-bins the visible domain while zooming, which Vega-Lite cannot express. Pixel ratio against ref/zoomable-binned-plot.png: 0.032.
- **zoomable-circle-packing** (Interaction Techniques): Native SVG vs test/vega-examples/ref at plot origin: differing-pixel ratio 0.0090 (Vega's own SVG through the same rasterizer: n/a (timer)). The initial view matches. Clicking a circle opens a zoomed view (the zoom-subtree action re-lays its subtree out, which is d3's zoom geometry) and clicking the focused circle zooms out, but without Vega's animated transition, the shift-click details panel or the slow-motion modifier; the help text describes eas's clicks.
- **map-with-tooltip** (Interaction Techniques): the map and its legend match ref/map-with-tooltip.png: ratio 0.0156 at 960x530 (resvg), mercator at scale 900 centred on [-95, 38]; Vega's 30 px top padding is a 530 px view with the map 30 px lower; the tooltip is Vega-Lite's own (the county's key and rate): Vega's custom tooltip, a rounded group following the pointer that redraws the hovered county under a second projection centred on its inverted centroid, has no Vega-Lite form
- **pacman** (Interaction Techniques): The first frame matches. The arrows steer the eater on a 500 ms timer (x-eas.timer, x-eas.on); gums score 10 and power gums 50; ghosts chase along the longer axis and wander when blocked; a ghost on the eater restarts the game. Not ported: power mode (fleeing, edible ghosts), Vega's ghost decision tables and ghost shapes (drawn as discs). Pixel ratio against ref/pacman.png: 0.011.
- **platformer** (Interaction Techniques): The first frame matches (the binding precomputes Vega's hsl terrain colours and a solid-cell grid). Gravity, landing, running (left, right), jumping (up) and dashing (z) run on a 50 ms timer; Emacs reports no key release, so a press runs for 6 frames. Not ported: the background image, the dash trail, Vega's exact collision response and its 17 ms frames. Pixel ratio against ref/platformer.png: 0.000.

## Beyond Vega-Lite

Templates that need what Vega-Lite lacks use x-eas transforms and handlers
(listed in `src/supported.json` under extensions): countpattern and wordcloud,
stratify/tree/cluster/treemap/partition/pack and linkpath, force and voronoi,
dotbin, kde2d and contour, the timer and key handlers of `x-eas.on`.  They
resolve to Vega-Lite with those materialized, so bin/chart can export them,
except `vega/projections`, whose d3-geo-projection projections Vega-Lite has
no names for (`x-eas.export`).
