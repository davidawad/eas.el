// Stand-in for `bin/chart diff`: pixel mismatch ratio of two PNGs (pixelmatch, threshold 0.1).
// Usage: node pngdiff.mjs A.png B.png [DIFF.png] -> prints JSON {w,h,sizeA,sizeB,mismatch,ratio}
import fs from 'fs';
import {PNG} from 'pngjs';
import pixelmatch from 'pixelmatch';
const [,, a, b, out] = process.argv;
const A = PNG.sync.read(fs.readFileSync(a)), B = PNG.sync.read(fs.readFileSync(b));
const w = Math.max(A.width, B.width), h = Math.max(A.height, B.height);
const pad = (img) => { const p = new PNG({width: w, height: h}); p.data.fill(255);
  PNG.bitblt(img, p, 0, 0, img.width, img.height, 0, 0); return p; };
const pa = pad(A), pb = pad(B), d = new PNG({width: w, height: h});
const n = pixelmatch(pa.data, pb.data, d.data, w, h, {threshold: 0.1});
if (out) fs.writeFileSync(out, PNG.sync.write(d));
console.log(JSON.stringify({w, h, sizeA: [A.width, A.height], sizeB: [B.width, B.height], mismatch: n, ratio: +(n / (w * h)).toFixed(4)}));
