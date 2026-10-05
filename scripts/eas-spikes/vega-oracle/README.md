# vega-oracle: bin/chart's references, approximately, without bin/chart

For boxes that have neither bin/chart nor rsvg-convert (fc-qx1.38).
`setup.sh` installs Vega-Lite 6.4.1, Vega 6 and node-canvas (whose
build bundles librsvg, cairo and pango), fetches Liberation Sans (Arial's
metrics) and writes an `rsvg-convert` that rasterizes through that
librsvg, so `eas-vl-gallery` and the conformance oracle can run:

    eval "$(scripts/eas-spikes/vega-oracle/setup.sh)"
    node scripts/eas-spikes/vega-oracle/vl2svg.mjs test/vl-examples/bar/bar.vl.json /tmp/bar.svg
    rsvg-convert -o /tmp/bar.png /tmp/bar.svg

`vl2svg.mjs` builds what bin/chart builds: its default theme under the
spec's config, text measured and drawn as Arial.  Against the committed
references of the bar group it scores 0.0031 to 0.0079 with identical
canvas sizes.  Its rasterizer reads native SVG a little more strictly
than the CI's rsvg-convert (unchanged SVGs score 1 to 2.5 times their
recorded ratios), so verdicts measured with it are conservative.

It is a development aid: references committed to the repository are
bin/chart's (`eas-conformance-update-refs`, `eas-vl-custom-build-ref`).
See engine-spikes.md section 12.
