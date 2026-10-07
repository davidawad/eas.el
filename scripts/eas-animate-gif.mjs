// SPDX-License-Identifier: GPL-3.0-or-later
// Rasterize DIR/frame-*.svg with @resvg/resvg-js and encode them as one
// looping GIF with gifenc (pure JS).  Called by scripts/eas-animate.sh.
//   node scripts/eas-animate-gif.mjs DIR OUT.gif DELAY_MS [FONT.ttf]
import { readdirSync, readFileSync, writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { join } from 'node:path';
import { createRequire } from 'node:module';

const require = createRequire(join(process.cwd(), 'noop.js'));
const { Resvg } = require('@resvg/resvg-js');
const { GIFEncoder, quantize, applyPalette } = require('gifenc');

const [dir, out, delayArg, fontArg] = process.argv.slice(2);
const delay = Number(delayArg || 250);
let font = fontArg;
if (!font) {
  for (const family of ['DejaVu Sans', 'Arimo']) {
    try {
      font = execFileSync('fc-match', ['-f', '%{file}', family]).toString();
      if (font) break;
    } catch { /* no fontconfig */ }
  }
}
const files = readdirSync(dir).filter((f) => /^frame-\d+\.svg$/.test(f)).sort();
const frames = files.map((f) => new Resvg(readFileSync(join(dir, f), 'utf8'), {
  background: 'white',
  fitTo: { mode: 'zoom', value: 1.5 },
  font: { loadSystemFonts: !font, fontFiles: font ? [font] : [], defaultFontFamily: 'DejaVu Sans' },
}).render());
// One global palette, from the last frame (the fullest one), so the
// colors do not flicker from frame to frame.
const last = frames[frames.length - 1];
const palette = quantize(last.pixels, 256);
const gif = GIFEncoder();
frames.forEach((png, i) => {
  gif.writeFrame(applyPalette(png.pixels, palette), png.width, png.height, {
    palette: i === 0 ? palette : undefined,
    // Hold the last frame before the loop starts over.
    delay: i === frames.length - 1 ? delay * 8 : delay,
  });
});
gif.finish();
writeFileSync(out, gif.bytes());
