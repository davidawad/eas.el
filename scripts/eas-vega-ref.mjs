#!/usr/bin/env node
// Build a reference SVG for a Vega-Lite spec the way bin/chart does:
// Vega-Lite 6.4.1 compiles it with bin/chart's default theme
// (test/conformance/bin-chart-default-theme.json) and Vega draws it,
// measuring text with node-canvas.  Rasterize the SVG with rsvg-convert,
// as the oracle rasterizes native SVG.
//
//   node scripts/eas-vega-ref.mjs SPEC.vl.json OUT.svg [--config PATCH]
//
// PATCH is a JSON object merged into the theme's config one level deep
// (a null value deletes the key).  It exists for references bin/chart
// cannot produce: a gallery status.json entry that names such a
// reference ("ref") records the exact command in "ref_build".
//
// Dev-only, never a runtime dependency.  Needs, in EAS_VEGA_MODULES
// (a node_modules directory) or on node's resolution path:
//   npm install vega-lite@6.4.1 vega@6 canvas
// Build with TZ set to the references' zone (America/Chicago for
// test/vl-examples) and a sans-serif font metric-compatible with
// bin/chart's (Liberation Sans or Arimo).
import fs from 'node:fs';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL, fileURLToPath} from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const resolver = createRequire(process.env.EAS_VEGA_MODULES
  ? path.join(path.resolve(process.env.EAS_VEGA_MODULES), 'x.js')
  : import.meta.url);
const load = (name) => import(pathToFileURL(resolver.resolve(name)).href);

const args = process.argv.slice(2);
const at = args.indexOf('--config');
const patch = at >= 0 ? JSON.parse(args.splice(at, 2)[1]) : {};
const [file, out] = args;
if (!file || !out) {
  console.error('usage: eas-vega-ref.mjs SPEC.vl.json OUT.svg [--config PATCH]');
  process.exit(2);
}

const vl = await load('vega-lite');
const vega = await load('vega');
if (vl.version !== '6.4.1') console.error(`warning: vega-lite ${vl.version}, bin/chart pins 6.4.1`);

const theme = JSON.parse(fs.readFileSync(path.join(here, '../test/conformance/bin-chart-default-theme.json')));
const config = structuredClone(theme.config);
for (const [k, v] of Object.entries(patch)) {
  if (v === null) delete config[k];
  else if (typeof v === 'object' && !Array.isArray(v) && typeof config[k] === 'object') {
    config[k] = {...config[k], ...v};
    for (const [kk, vv] of Object.entries(v)) if (vv === null) delete config[k][kk];
  } else config[k] = v;
}

const spec = JSON.parse(fs.readFileSync(file));
const loader = vega.loader({baseURL: path.resolve(path.dirname(file)) + '/'});
const view = new vega.View(vega.parse(vl.compile(spec, {config}).spec), {renderer: 'none', loader});
fs.writeFileSync(out, await view.toSVG());
