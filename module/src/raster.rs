// SPDX-License-Identifier: GPL-3.0-or-later
//! SVG to RGBA with resvg, for eas-output.el's terminal pixel modes.
//! The system fonts are read once, on the first raster.

use resvg::tiny_skia::{Pixmap, Transform};
use resvg::usvg::{fontdb, Options, Tree};
use std::sync::{Arc, OnceLock};

static FONTS: OnceLock<Arc<fontdb::Database>> = OnceLock::new();

fn fonts() -> Arc<fontdb::Database> {
    FONTS
        .get_or_init(|| {
            let mut db = fontdb::Database::new();
            db.load_system_fonts();
            // eas's SVG names CSS generic families; map them to fonts most
            // systems have, falling back to fontdb's own choice.
            for (fam, set) in [
                ("DejaVu Sans", 0),
                ("Liberation Sans", 0),
                ("Arial", 0),
                ("Helvetica", 0),
                ("DejaVu Sans Mono", 1),
                ("Liberation Mono", 1),
                ("Menlo", 1),
            ] {
                let known = db.faces().any(|f| f.families.iter().any(|(n, _)| n == fam));
                if known {
                    if set == 0 {
                        db.set_sans_serif_family(fam);
                    } else {
                        db.set_monospace_family(fam);
                    }
                }
            }
            Arc::new(db)
        })
        .clone()
}

/// SVG drawn at W x H pixels (stretched to fit) as straight RGBA.
pub fn rgba(svg: &str, w: u32, h: u32) -> Result<Vec<u8>, String> {
    if w == 0 || h == 0 {
        return Err(format!("bad raster size {w}x{h}"));
    }
    let opt = Options {
        fontdb: fonts(),
        font_family: "sans-serif".into(),
        ..Options::default()
    };
    let tree = Tree::from_str(svg, &opt).map_err(|e| format!("SVG parse: {e}"))?;
    let mut pm = Pixmap::new(w, h).ok_or_else(|| format!("cannot allocate {w}x{h}"))?;
    let size = tree.size();
    let t = Transform::from_scale(w as f32 / size.width(), h as f32 / size.height());
    resvg::render(&tree, t, &mut pm.as_mut());
    let mut out = Vec::with_capacity((w * h * 4) as usize);
    for p in pm.pixels() {
        let c = p.demultiply();
        out.extend_from_slice(&[c.red(), c.green(), c.blue(), c.alpha()]);
    }
    Ok(out)
}
