#!/usr/bin/env node
// Build the example bindings of the network and force templates
// (templates/vega/*.json, bead eas-7r1.5) from the vendored Vega
// gallery data in test/vega-examples/data:
//
//   node scripts/eas-vega-network-data.mjs
//
// writes examples/vega/{force-directed-layout,arc-diagram,
// reorderable-matrix,beeswarm-plot,packed-bubble-chart,
// dorling-cartogram,airport-connections}.data.json.  eas reads no
// TopoJSON, so the Dorling binding carries each state's centroid as
// d3-geo's geoCentroid computes it (what Vega's geoCentroid
// expression does before projecting), and the airport binding keeps
// the airports some flight leaves from or lands at.
//
// Dev-only, never a runtime dependency.  Needs, in EAS_VEGA_MODULES
// (a node_modules directory) or on node's resolution path:
//   npm install d3-geo@3 topojson-client@3
import fs from 'node:fs';
import path from 'node:path';
import {createRequire} from 'node:module';
import {pathToFileURL, fileURLToPath} from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const resolver = createRequire(process.env.EAS_VEGA_MODULES
  ? path.join(path.resolve(process.env.EAS_VEGA_MODULES), 'x.js')
  : import.meta.url);
const load = (name) => import(pathToFileURL(resolver.resolve(name)).href);
const data = (file) => fs.readFileSync(path.join(root, 'test/vega-examples/data', file), 'utf8');
const json = (file) => JSON.parse(data(file));

// Split one CSV line, honouring double-quoted cells ("a, b").
const cellsOf = (line) => [...line.matchAll(/("(?:[^"]|"")*"|[^,]*)(,|$)/g)]
  .slice(0, -1).map((m) => m[1].startsWith('"') ? m[1].slice(1, -1).replace(/""/g, '"') : m[1]);

function csv(file) {
  const [head, ...lines] = data(file).trim().split(/\r?\n/);
  const keys = head.split(',');
  return lines.map((line) => {
    const cells = cellsOf(line);
    return Object.fromEntries(keys.map((k, i) => {
      const v = cells[i];
      return [k, v !== '' && !isNaN(+v) ? +v : v];
    }));
  });
}

// One row per line, so a binding diffs row by row.
function write(name, binding) {
  const parts = Object.entries(binding).map(([k, v]) => Array.isArray(v)
    ? `  ${JSON.stringify(k)}: [\n${v.map((r) => '    ' + JSON.stringify(r)).join(',\n')}\n  ]`
    : `  ${JSON.stringify(k)}: ${JSON.stringify(v)}`);
  fs.writeFileSync(path.join(root, 'examples/vega', `${name}.data.json`), `{\n${parts.join(',\n')}\n}\n`);
}

const {geoCentroid} = await load('d3-geo');
const {feature} = await load('topojson-client');

const mis = json('miserables.json');
write('force-directed-layout', {nodes: mis.nodes, links: mis.links});
write('arc-diagram', {nodes: mis.nodes, links: mis.links});
write('reorderable-matrix', {nodes: mis.nodes, links: mis.links});
write('beeswarm-plot', {data: mis.nodes});

const bubble = JSON.parse(fs.readFileSync(path.join(root, 'test/vega-examples/specs/packed-bubble-chart.vg.json'), 'utf8'));
// The reference is Vega's first frame: a non-static force has run one tick when it renders.
write('packed-bubble-chart', {iterations: 1, data: bubble.data[0].values});

const us = json('us-10m.json');
const states = new Map(feature(us, us.objects.states).features.map((f) => [f.id, f]));
const round = (v) => Math.round(v * 1e6) / 1e6;
write('dorling-cartogram', {
  data: json('obesity.json').filter((d) => states.has(d.id)).map((d) => {
    const [longitude, latitude] = geoCentroid(states.get(d.id));
    return {...d, longitude: round(longitude), latitude: round(latitude)};
  })
});

const flights = csv('flights-airport.csv');
const used = new Set(flights.flatMap((f) => [f.origin, f.destination]));
write('airport-connections', {
  airports: csv('airports.csv').filter((a) => used.has(a.iata))
    .map(({iata, name, latitude, longitude}) => ({iata, name, latitude, longitude})),
  flights
});
