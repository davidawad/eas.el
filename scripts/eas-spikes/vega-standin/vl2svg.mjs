// Stand-in for `bin/chart build` (NOT bin/chart): Vega-Lite 6.4.1 -> Vega -> SVG.
// Usage: node vl2svg.mjs SPEC.vl.json OUT.svg
import fs from 'fs';
import * as vl from 'vega-lite';
import * as vega from 'vega';
const [,, input, output] = process.argv;
const spec = JSON.parse(fs.readFileSync(input, 'utf8'));
const vgSpec = vl.compile(spec).spec;
const view = new vega.View(vega.parse(vgSpec), {renderer: 'none'});
const svg = await view.toSVG();
fs.writeFileSync(output, svg);
