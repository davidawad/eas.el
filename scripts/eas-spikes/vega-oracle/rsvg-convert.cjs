// rsvg-convert -o OUT.png IN.svg, through the librsvg node-canvas bundles.
const fs = require('fs');
const path = require('path');
const { createCanvas, loadImage } = require(path.join(__dirname, 'node_modules/canvas'));
const a = process.argv.slice(2);
let out = null, inp = null;
for (let i = 0; i < a.length; i++) { if (a[i] === '-o') out = a[++i]; else if (!a[i].startsWith('-')) inp = a[i]; }
(async () => {
  const img = await loadImage(fs.readFileSync(inp));
  const c = createCanvas(img.width, img.height);
  c.getContext('2d').drawImage(img, 0, 0);
  fs.writeFileSync(out, c.toBuffer('image/png'));
})().catch(e => { console.error(e); process.exit(1); });
