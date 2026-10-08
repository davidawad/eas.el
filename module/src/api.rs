// SPDX-License-Identifier: GPL-3.0-or-later
//! The functions the module gives Emacs.  Each works on whole batches:
//! a geometry is read once into a handle, and one call projects every
//! shape of a map and returns its items' paths and SVG path data.

use crate::ffi::{Error, Result, Value, E};
use crate::geom::{Geom, Node, Ring};
use crate::proj::{Params, Proj};
use crate::sink::{anchor, relative, PathSink, Relative};
use crate::stream::Shared;
use crate::svg;
use std::cell::RefCell;
use std::os::raw::c_void;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

unsafe extern "C" fn finalize_node(p: *mut c_void) {
    if !p.is_null() {
        // SAFETY: P came from Box::into_raw in `geometry'.
        drop(unsafe { Box::from_raw(p as *mut Arc<Node>) });
    }
}

fn node_of(e: E, v: Value) -> Result<Arc<Node>> {
    let ours = e.user_finalizer(v).is_some_and(|f| std::ptr::fn_addr_eq(f, finalize_node as unsafe extern "C" fn(*mut c_void)));
    if !ours {
        return Err(Error("not an eas-geo-module geometry".into()));
    }
    let p = e.get_user_ptr(v)? as *const Arc<Node>;
    // SAFETY: the finalizer check proves P is a live Box<Arc<Node>>.
    Ok(unsafe { (*p).clone() })
}

fn position(e: E, v: Value) -> Result<(f64, f64)> {
    let x = e.num(e.vec_get(v, 0))?;
    let y = e.num(e.vec_get(v, 1))?;
    Ok((x, y))
}

fn ring(e: E, v: Value) -> Result<Ring> {
    let n = e.vec_size(v)?;
    (0..n).map(|i| position(e, e.vec_get(v, i))).collect()
}

fn rings(e: E, v: Value) -> Result<Vec<Ring>> {
    let n = e.vec_size(v)?;
    (0..n).map(|i| ring(e, e.vec_get(v, i))).collect()
}

/// (eas-geo-module-geometry CODE COORDS): a handle on a geometry.
fn geometry(e: E, args: &[Value]) -> Result<Value> {
    let code = e.int(args[0])?;
    let c = args[1];
    let geom = match code {
        1 => Geom::Sphere,
        2 => {
            let (x, y) = position(e, c)?;
            Geom::Point(x, y)
        }
        3 => Geom::MultiPoint(ring(e, c)?),
        4 => Geom::LineString(ring(e, c)?),
        5 => Geom::MultiLineString(rings(e, c)?),
        6 => Geom::Polygon(rings(e, c)?),
        7 => {
            let n = e.vec_size(c)?;
            Geom::MultiPolygon((0..n).map(|i| rings(e, e.vec_get(c, i))).collect::<Result<_>>()?)
        }
        8 => {
            let n = e.vec_size(c)?;
            Geom::Collection((0..n).map(|i| node_of(e, e.vec_get(c, i))).collect::<Result<_>>()?)
        }
        _ => Geom::Empty,
    };
    e.check()?;
    let b = Box::new(Arc::new(Node::new(geom)));
    Ok(e.user_ptr(finalize_node, Box::into_raw(b) as *mut c_void))
}

fn opt_num(e: E, v: Value) -> Result<Option<f64>> {
    if e.is_not_nil(v) {
        e.num(v).map(Some)
    } else {
        Ok(None)
    }
}

/// The projection a parameter vector describes (`eas-geo-proj--native').
fn projection(e: E, v: Value) -> Result<Proj> {
    let n = e.vec_size(v)?;
    let name = e.get_string(e.vec_get(v, 0))?;
    let num = |i: usize| e.num(e.vec_get(v, i));
    if name == "albersUsa" {
        return Ok(Proj::albers_usa(num(1)?, num(2)?, num(3)?, num(4)?));
    }
    if n < 18 {
        return Err(Error("bad projection parameters".into()));
    }
    let extent = {
        let x = e.vec_get(v, 12);
        if e.is_not_nil(x) {
            Some([e.num(e.vec_get(x, 0))?, e.num(e.vec_get(x, 1))?, e.num(e.vec_get(x, 2))?, e.num(e.vec_get(x, 3))?])
        } else {
            None
        }
    };
    let a = Params {
        scale: num(3)?,
        translate: (num(4)?, num(5)?),
        center: (num(6)?, num(7)?),
        rotate: [num(8)?, num(9)?, num(10)?],
        clip_angle: opt_num(e, e.vec_get(v, 11))?,
        clip_extent: extent,
        reclip: e.int(e.vec_get(v, 13))? as u8,
        precision: num(14)?,
        angle: num(15)?,
        reflect: (e.is_not_nil(e.vec_get(v, 16)), e.is_not_nil(e.vec_get(v, 17))),
    };
    Proj::new(&name, (num(1)?, num(2)?), &a).ok_or_else(|| Error(format!("projection {name} not drawn natively")))
}

fn nodes(e: E, v: Value) -> Result<Vec<Arc<Node>>> {
    let n = e.vec_size(v)?;
    (0..n).map(|i| node_of(e, e.vec_get(v, i))).collect()
}

struct Out {
    vector: Value,
    t: Value,
    false_: Value,
    nil: Value,
}

impl Out {
    fn new(e: E) -> Out {
        Out { vector: e.intern("vector"), t: e.intern("t"), false_: e.intern(":false"), nil: e.intern("nil") }
    }
    fn vec(&self, e: E, mut items: Vec<Value>) -> Value {
        e.funcall(self.vector, &mut items)
    }
    fn floats(&self, e: E, xs: &[f64]) -> Value {
        self.vec(e, xs.iter().map(|&x| e.float(x)).collect())
    }
}

/// A shape projected and measured, ready for Emacs.
struct Done {
    anchor: Option<(f64, f64)>,
    rel: Relative,
    d: String,
}

/// Project, anchor and print one shape (`eas-geoshape--project').
fn project_one(proj: &Proj, g: &Node, ox: f64, oy: f64, tolerance: Option<f64>, radius: f64) -> Option<Done> {
    let sink = Shared(Rc::new(RefCell::new(PathSink::new(radius, tolerance))));
    proj.stream_node(g, &sink);
    let r = std::mem::take(&mut sink.0.borrow_mut().out);
    if r.paths.is_empty() && r.circles.is_empty() {
        return None;
    }
    let found = anchor(&r);
    let (ax, ay) = found.unwrap_or((0.0, 0.0));
    let rel = relative(&r, ax, ay);
    let mut d = String::new();
    svg::rings(ox + ax, oy + ay, &rel.paths, &mut d);
    Some(Done { anchor: found, rel, d })
}

/// The threads a batch may use: EAS_GEO_MODULE_THREADS, else 1; looked
/// up once.  Measured on an 8-core box, threads did not pay: a map's
/// batch is a few milliseconds, and the first render got slower.
fn threads() -> usize {
    static N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *N.get_or_init(|| {
        std::env::var("EAS_GEO_MODULE_THREADS").ok().and_then(|v| v.parse().ok()).unwrap_or(1).clamp(1, 64)
    })
}

/// Shapes per thread below which one thread does them all.
const PER_THREAD: usize = 24;

/// Every shape of GEOMS done, in order, on `threads' threads.  The
/// shapes are independent; each thread takes the next one not yet taken.
fn project_all(proj: &Proj, geoms: &[Arc<Node>], ox: f64, oy: f64, tolerance: Option<f64>, radius: f64) -> Vec<Option<Done>> {
    let n = geoms.len();
    let threads = threads().min(n / PER_THREAD).max(1);
    if threads == 1 {
        return geoms.iter().map(|g| project_one(proj, g, ox, oy, tolerance, radius)).collect();
    }
    let next = AtomicUsize::new(0);
    let mut out: Vec<Option<Done>> = (0..n).map(|_| None).collect();
    std::thread::scope(|s| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                s.spawn(|| {
                    let mut mine = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= n {
                            break mine;
                        }
                        mine.push((i, project_one(proj, &geoms[i], ox, oy, tolerance, radius)));
                    }
                })
            })
            .collect();
        for w in workers {
            for (i, d) in w.join().unwrap_or_else(|p| std::panic::resume_unwind(p)) {
                out[i] = d;
            }
        }
    });
    out
}

/// (eas-geo-module-shapes PARAMS GEOMS OX OY TOLERANCE RADIUS).
fn shapes(e: E, args: &[Value]) -> Result<Value> {
    let proj = projection(e, args[0])?;
    let geoms = nodes(e, args[1])?;
    let trace_name = if std::env::var_os("EAS_GEO_MODULE_TRACE").is_some() {
        e.get_string(e.vec_get(args[0], 0)).unwrap_or_default()
    } else {
        String::new()
    };
    let ox = e.num(args[2])?;
    let oy = e.num(args[3])?;
    let tolerance = opt_num(e, args[4])?;
    let radius = e.num(args[5])?;
    let trace = std::env::var_os("EAS_GEO_MODULE_TRACE").is_some();
    let t0 = std::time::Instant::now();
    let done = project_all(&proj, &geoms, ox, oy, tolerance, radius);
    let t1 = std::time::Instant::now();
    let out = Out::new(e);
    let results: Vec<Value> = done
        .iter()
        .map(|d| match d {
            None => out.nil,
            Some(d) => {
                let (axv, ayv) = match d.anchor {
                    Some((ax, ay)) => (e.float(ax), e.float(ay)),
                    None => (e.integer(0), e.integer(0)),
                };
                let paths = d
                    .rel
                    .paths
                    .iter()
                    .map(|(closed, flat)| out.vec(e, vec![if *closed { out.t } else { out.false_ }, out.floats(e, flat)]))
                    .collect();
                let circles = d.rel.circles.iter().map(|c| out.floats(e, c)).collect();
                let item = vec![axv, ayv, out.vec(e, paths), out.vec(e, circles), out.floats(e, &d.rel.bbox), e.string(&d.d)];
                out.vec(e, item)
            }
        })
        .collect();
    if trace {
        eprintln!(
            "eas-geo-module-shapes: {trace_name} {} shapes, project {:?}, build {:?}",
            results.len(),
            t1 - t0,
            t1.elapsed()
        );
    }
    e.check()?;
    Ok(out.vec(e, results))
}

/// (eas-geo-module-fit PARAMS GEOMS): [X0 Y0 X1 Y1] of GEOMS projected.
fn fit(e: E, args: &[Value]) -> Result<Value> {
    let proj = projection(e, args[0])?;
    let geoms = nodes(e, args[1])?;
    let b = proj.fit_bounds(&geoms);
    e.check()?;
    Ok(Out::new(e).floats(e, &b))
}

fn version(e: E, _args: &[Value]) -> Result<Value> {
    Ok(e.string(env!("CARGO_PKG_VERSION")))
}

/// Run F for Emacs: a Rust error or panic becomes an Elisp error.
fn call(env: *mut crate::ffi::Env, nargs: isize, args: *mut Value, f: fn(E, &[Value]) -> Result<Value>) -> Value {
    let e = E(env);
    let args: &[Value] = if nargs > 0 {
        // SAFETY: Emacs passes NARGS values.
        unsafe { std::slice::from_raw_parts(args, nargs as usize) }
    } else {
        &[]
    };
    match std::panic::catch_unwind(|| f(e, args)) {
        Ok(Ok(v)) => v,
        Ok(Err(Error(msg))) => {
            e.signal(if msg.is_empty() { "eas-geo-module: bad argument" } else { &msg });
            e.intern("nil")
        }
        Err(_) => {
            e.signal("eas-geo-module: internal error");
            e.intern("nil")
        }
    }
}

macro_rules! export {
    ($name:ident, $f:path) => {
        unsafe extern "C" fn $name(env: *mut crate::ffi::Env, n: isize, a: *mut Value, _d: *mut c_void) -> Value {
            call(env, n, a, $f)
        }
    };
}

export!(fn_geometry, geometry);
export!(fn_shapes, shapes);
export!(fn_fit, fit);
export!(fn_version, version);

/// Define the module's functions and provide its feature.
pub fn init(e: E) {
    e.defun(
        "eas-geo-module-geometry",
        2,
        2,
        fn_geometry,
        "Return a handle on a geometry: CODE and its COORDS.\n\n(fn CODE COORDS)",
    );
    e.defun(
        "eas-geo-module-shapes",
        6,
        6,
        fn_shapes,
        "Project GEOMS under PARAMS: per geometry nil or [AX AY PATHS CIRCLES BOX D].\n\n(fn PARAMS GEOMS OX OY TOLERANCE RADIUS)",
    );
    e.defun("eas-geo-module-fit", 2, 2, fn_fit, "Bounds [X0 Y0 X1 Y1] of GEOMS under PARAMS.\n\n(fn PARAMS GEOMS)");
    e.defun("eas-geo-module-version", 0, 0, fn_version, "The module's version string.");
    let provide = e.intern("provide");
    let feature = e.intern("eas-geo-module");
    e.funcall(provide, &mut [feature]);
}
