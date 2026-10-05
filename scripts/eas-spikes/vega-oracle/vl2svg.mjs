// node vl2svg.mjs SPEC.vl.json OUT.svg: what bin/chart builds, as Vega SVG.
// bin/chart's default theme (vendored) under the spec's config; the
// generic sans-serif drawn and measured as Arial, which vl-convert
// resolves it to.  Data URLs resolve beside the spec.
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import * as vega from 'vega';
import * as vl from 'vega-lite';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../../..');
const [specFile, out] = process.argv.slice(2);
const spec = JSON.parse(fs.readFileSync(specFile, 'utf8'));
const theme = JSON.parse(fs.readFileSync(path.join(root, 'test/conformance/bin-chart-default-theme.json'), 'utf8')).config;
const merge = (a, b) => {
  const o = { ...a };
  for (const [k, v] of Object.entries(b || {}))
    o[k] = v && typeof v === 'object' && !Array.isArray(v) && o[k] && typeof o[k] === 'object' && !Array.isArray(o[k]) ? merge(o[k], v) : v;
  return o;
};
spec.config = merge(theme, spec.config);
if (!spec.config.font || spec.config.font === 'sans-serif') spec.config.font = 'Arial';
const loader = vega.loader({ baseURL: path.dirname(path.resolve(specFile)) + '/', mode: 'file' });
const view = new vega.View(vega.parse(vl.compile(spec).spec), { loader, renderer: 'none' });
fs.writeFileSync(out, await view.toSVG());
