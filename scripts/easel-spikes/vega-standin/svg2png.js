// Usage: node svg2png.js IN.svg OUT.png  -- rasterize with resvg (system fonts).
const fs = require('fs');
const {Resvg} = require('@resvg/resvg-js');
const [,, input, output] = process.argv;
const r = new Resvg(fs.readFileSync(input, 'utf8'), {font: {loadSystemFonts: false, fontFiles: [__dirname + '/node_modules/dejavu-fonts-ttf/ttf/DejaVuSans.ttf', __dirname + '/node_modules/dejavu-fonts-ttf/ttf/DejaVuSans-Bold.ttf'], defaultFontFamily: 'DejaVu Sans', sansSerifFamily: 'DejaVu Sans'}, background: 'white'});
fs.writeFileSync(output, r.render().asPng());
