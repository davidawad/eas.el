// SPDX-License-Identifier: GPL-3.0-or-later
//! The polyhedral butterfly of eas-geo-polyhedral.el: the octahedron's
//! eight faces, each a gnomonic projection about its centroid, unfolded
//! into Cahill's butterfly, and the sphere's outline.

use crate::math::{asin, cartesian, pow, rem, Rotation, EPS, HALF_PI, RAD};
use crate::raw::{gnomonic, RawFn};
use crate::stream::Stream;
use std::sync::{Arc, OnceLock};

/// The octahedron's six vertices, [LON LAT] degrees.
const VERTS: [(f64, f64); 6] = [(0.0, 90.0), (-90.0, 0.0), (0.0, 0.0), (90.0, 0.0), (180.0, 0.0), (0.0, -90.0)];

/// The faces, clockwise vertex indices into `VERTS'.
const FACES: [[usize; 3]; 8] =
    [[0, 2, 1], [0, 3, 2], [5, 1, 2], [5, 2, 3], [0, 1, 4], [0, 4, 3], [5, 4, 1], [5, 3, 4]];

/// The parent of each face in the unfolding (-1: the root).
const PARENTS: [i32; 8] = [-1, 0, 0, 1, 0, 1, 4, 5];

/// An edge: a vertex pair (A . B) or the node across it.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Edge {
    Pair(usize, usize),
    Node(usize),
}

struct Node {
    face: [usize; 3],
    rot: Rotation,
    transform: Option<[f64; 6]>,
    edges: Vec<Edge>,
    children: Vec<usize>,
}

pub struct Butterfly {
    nodes: Vec<Node>,
}

/// `eas-geo-polyhedral--centroid' of the face's vertices.
fn centroid(face: &[usize; 3]) -> (f64, f64) {
    let (mut x, mut y, mut z) = (0.0f64, 0.0f64, 0.0f64);
    for &i in face {
        let p = VERTS[i];
        let c = cartesian(p.0 * RAD, p.1 * RAD);
        x += c[0];
        y += c[1];
        z += c[2];
    }
    (y.atan2(x) / RAD, (z / (x * x + y * y + z * z).sqrt()).asin() / RAD)
}

/// `eas-geo-polyhedral--interpolate' (A B TT), degrees.
fn interpolate(a: (f64, f64), b: (f64, f64), tt: f64) -> (f64, f64) {
    let x0 = a.0 * RAD;
    let y0 = a.1 * RAD;
    let x1 = b.0 * RAD;
    let y1 = b.1 * RAD;
    let cy0 = y0.cos();
    let sy0 = y0.sin();
    let cy1 = y1.cos();
    let sy1 = y1.sin();
    let kx0 = cy0 * x0.cos();
    let ky0 = cy0 * x0.sin();
    let kx1 = cy1 * x1.cos();
    let ky1 = cy1 * x1.sin();
    let hav = |v: f64| {
        let s = (v / 2.0).sin();
        s * s
    };
    let d = 2.0 * asin((hav(y1 - y0) + cy0 * cy1 * hav(x1 - x0)).sqrt());
    let k = d.sin();
    if d == 0.0 {
        return a;
    }
    let td = tt * d;
    let bb = td.sin() / k;
    let aa = (d - td).sin() / k;
    let x = aa * kx0 + bb * kx1;
    let y = aa * ky0 + bb * ky1;
    let z = aa * sy0 + bb * sy1;
    (y.atan2(x) / RAD, z.atan2((x * x + y * y).sqrt()) / RAD)
}

fn multiply(a: &[f64; 6], b: &[f64; 6]) -> [f64; 6] {
    [
        a[0] * b[0] + a[1] * b[3],
        a[0] * b[1] + a[1] * b[4],
        a[0] * b[2] + a[1] * b[5] + a[2],
        a[3] * b[0] + a[4] * b[3],
        a[3] * b[1] + a[4] * b[4],
        a[3] * b[2] + a[4] * b[5] + a[5],
    ]
}

/// `eas-geo-polyhedral--matrix': segment B onto segment A.
fn matrix(a: [(f64, f64); 2], b: [(f64, f64); 2]) -> [f64; 6] {
    let u = (a[1].0 - a[0].0, a[1].1 - a[0].1);
    let v = (b[1].0 - b[0].0, b[1].1 - b[0].1);
    let phi = (u.0 * v.1 - u.1 * v.0).atan2(u.0 * v.0 + u.1 * v.1);
    let s = (pow(u.0, 2.0) + pow(u.1, 2.0)).sqrt() / (pow(v.0, 2.0) + pow(v.1, 2.0)).sqrt();
    multiply(
        &[1.0, 0.0, a[0].0, 0.0, 1.0, a[0].1],
        &multiply(
            &[s, 0.0, 0.0, 0.0, s, 0.0],
            &multiply(
                &[phi.cos(), phi.sin(), 0.0, -phi.sin(), phi.cos(), 0.0],
                &[1.0, 0.0, -b[0].0, 0.0, 1.0, -b[0].1],
            ),
        ),
    )
}

/// The face's gnomonic projection (`eas-geo-proj--simple' at scale 1,
/// translate (0 0), rotated by the negated centroid) of LON LAT degrees.
fn project(rot: &Rotation, lon: f64, lat: f64) -> (f64, f64) {
    let c0 = gnomonic(0.0, 0.0);
    let cx = 0.0 + 1.0 * 1.0 * c0.0;
    let cy = 0.0 - 1.0 * 1.0 * c0.1;
    let dx = 0.0 - cx;
    let dy = 0.0 - cy;
    let (l, p) = rot.forward(lon * RAD, lat * RAD);
    let r = gnomonic(l, p);
    (dx + 1.0 * r.0, dy - 1.0 * r.1)
}

/// The two vertices faces A and B share, in A's order.
fn shared(a: &[usize; 3], b: &[usize; 3]) -> [usize; 2] {
    let mut found = None;
    for &x in a {
        if b.contains(&x) {
            match found {
                Some(f) => return [f, x],
                None => found = Some(x),
            }
        }
    }
    unreachable!("faces share an edge")
}

fn link(nodes: &mut Vec<Node>, node: usize, parent: Option<usize>) {
    let f = nodes[node].face;
    nodes[node].edges = vec![Edge::Pair(f[2], f[0]), Edge::Pair(f[0], f[1]), Edge::Pair(f[1], f[2])];
    if let Some(par) = parent {
        let sh = shared(&nodes[node].face, &nodes[par].face);
        let on = |n: &Node| -> [(f64, f64); 2] {
            let a = VERTS[sh[0]];
            let b = VERTS[sh[1]];
            [project(&n.rot, a.0, a.1), project(&n.rot, b.0, b.1)]
        };
        let m = matrix(on(&nodes[par]), on(&nodes[node]));
        nodes[node].transform = Some(match nodes[par].transform {
            Some(pt) => multiply(&pt, &m),
            None => m,
        });
        let hit = |e: &Edge| match *e {
            Edge::Pair(a, b) => (sh[0] == b && sh[1] == a) || (sh[0] == a && sh[1] == b),
            Edge::Node(_) => false,
        };
        for e in nodes[par].edges.iter_mut() {
            if hit(e) {
                *e = Edge::Node(node);
            }
        }
        for e in nodes[node].edges.iter_mut() {
            if hit(e) {
                *e = Edge::Node(par);
            }
        }
    }
    let children = nodes[node].children.clone();
    for c in children {
        link(nodes, c, Some(node));
    }
}

impl Butterfly {
    /// `eas-geo-polyhedral-butterfly'.
    pub fn new() -> Butterfly {
        let mut nodes: Vec<Node> = FACES
            .iter()
            .map(|f| {
                let c = centroid(f);
                let rot = Rotation::new(rem(-c.0, 360.0) * RAD, rem(-c.1, 360.0) * RAD, rem(0.0, 360.0) * RAD);
                Node { face: *f, rot, transform: None, edges: Vec::new(), children: Vec::new() }
            })
            .collect();
        for (i, &d) in PARENTS.iter().enumerate() {
            if d >= 0 {
                nodes[d as usize].children.push(i);
            }
        }
        link(&mut nodes, 0, None);
        Butterfly { nodes }
    }

    /// The raw projection of L P (radians).
    pub fn raw(&self, l: f64, p: f64) -> (f64, f64) {
        let i = if l < -HALF_PI {
            if p < 0.0 { 6 } else { 4 }
        } else if l < 0.0 {
            if p < 0.0 { 2 } else { 0 }
        } else if l < HALF_PI {
            if p < 0.0 { 3 } else { 1 }
        } else if p < 0.0 {
            7
        } else {
            5
        };
        let node = &self.nodes[i];
        let pt = project(&node.rot, l / RAD, p / RAD);
        match node.transform {
            Some(tm) => (tm[0] * pt.0 + tm[1] * pt.1 + tm[2], -(tm[3] * pt.0 + tm[4] * pt.1 + tm[5])),
            None => (pt.0, -pt.1),
        }
    }

    /// The sphere's outline (degrees) into SINK.
    pub fn sphere(&self, sink: &mut dyn Stream) {
        sink.polygon_start();
        sink.line_start();
        self.outline(sink, 0, None);
        sink.line_end();
        sink.polygon_end();
    }

    fn outline(&self, sink: &mut dyn Stream, node: usize, parent: Option<usize>) {
        let edges = &self.nodes[node].edges;
        let n = edges.len();
        let c = centroid(&self.nodes[node].face);
        let mut inside = false;
        let mut j = 0;
        if let Some(par) = parent {
            while j < n && edges[j] != Edge::Node(par) {
                j += 1;
            }
            j += 1;
        }
        for i in 0..n {
            match edges[(i + j) % n] {
                Edge::Pair(a, b) => {
                    if !inside {
                        let p = interpolate(VERTS[a], c, EPS);
                        sink.point(p.0, p.1, 0);
                        inside = true;
                    }
                    let p = interpolate(VERTS[b], c, EPS);
                    sink.point(p.0, p.1, 0);
                }
                Edge::Node(e) => {
                    inside = false;
                    if Some(e) != parent {
                        self.outline(sink, e, Some(node));
                    }
                }
            }
        }
    }
}

impl Default for Butterfly {
    fn default() -> Self {
        Butterfly::new()
    }
}

static BUTTERFLY: OnceLock<Arc<Butterfly>> = OnceLock::new();

fn butterfly() -> Arc<Butterfly> {
    BUTTERFLY.get_or_init(|| Arc::new(Butterfly::new())).clone()
}

/// The polyhedralButterfly raw projection.
pub fn butterfly_raw() -> RawFn {
    let b = butterfly();
    Box::new(move |l, p| b.raw(l, p))
}

/// The butterfly's outline (degrees) into SINK.
pub fn butterfly_sphere(sink: &mut dyn Stream) {
    butterfly().sphere(sink)
}
