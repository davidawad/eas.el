// SPDX-License-Identifier: GPL-3.0-or-later
//! The raster feature's Emacs functions (eas-output.el calls them).

use crate::encode;
use crate::ffi::{Error, Result, Value, E};
use crate::raster;
use std::os::raw::c_void;

fn bg_of(e: E, v: Value) -> Result<[u8; 3]> {
    if !e.is_not_nil(v) {
        return Ok([255, 255, 255]);
    }
    let s = e.get_string(v)?;
    let hex = s.trim_start_matches('#');
    let n = u32::from_str_radix(hex, 16).map_err(|_| Error(format!("bad colour {s}")))?;
    if hex.len() != 6 {
        return Err(Error(format!("bad colour {s}")));
    }
    Ok([(n >> 16) as u8, (n >> 8) as u8, n as u8])
}

/// (eas-raster-module-encode SVG W H FORMAT ID BG): SVG drawn at W x H
/// pixels, encoded as FORMAT: "kitty" (zlib RGBA transmit of image ID),
/// "kitty-png" (PNG transmit), "sixel" (over BG, "#rrggbb", default
/// white) or "png" (base64 of the PNG bytes).
fn encode_svg(e: E, args: &[Value]) -> Result<Value> {
    let svg = e.get_string(args[0])?;
    let w = e.int(args[1])?.clamp(1, 1 << 15) as u32;
    let h = e.int(args[2])?.clamp(1, 1 << 15) as u32;
    let format = e.get_string(args[3])?;
    let id = if args.len() > 4 && e.is_not_nil(args[4]) { e.int(args[4])? as u32 } else { 1 };
    let bg = if args.len() > 5 { bg_of(e, args[5])? } else { [255, 255, 255] };
    let px = raster::rgba(&svg, w, h).map_err(Error)?;
    let out = match format.as_str() {
        "kitty" => encode::kitty_rgba(&px, w, h, id),
        "kitty-png" => encode::kitty_png(&encode::png(&px, w, h), id),
        "sixel" => encode::sixel(&px, w, h, bg),
        "png" => encode::base64(&encode::png(&px, w, h)),
        f => return Err(Error(format!("unknown raster format {f}"))),
    };
    Ok(e.string(&out))
}

fn call(env: *mut crate::ffi::Env, nargs: isize, args: *mut Value, f: fn(E, &[Value]) -> Result<Value>) -> Value {
    let e = E(env);
    // SAFETY: Emacs passes NARGS values.
    let args: &[Value] = if nargs > 0 { unsafe { std::slice::from_raw_parts(args, nargs as usize) } } else { &[] };
    match f(e, args) {
        Ok(v) => v,
        Err(Error(msg)) => {
            e.signal(if msg.is_empty() { "eas-raster-module: bad argument" } else { &msg });
            e.intern("nil")
        }
    }
}

unsafe extern "C" fn fn_encode(env: *mut crate::ffi::Env, n: isize, a: *mut Value, _d: *mut c_void) -> Value {
    call(env, n, a, encode_svg)
}

/// Define the raster functions.
pub fn init(e: E) {
    e.defun(
        "eas-raster-module-encode",
        4,
        6,
        fn_encode,
        "SVG drawn at W x H pixels, encoded as FORMAT for a terminal.\nFORMAT is \"kitty\", \"kitty-png\", \"sixel\" or \"png\" (base64).\nID is the Kitty image id; BG the sixel background \"#rrggbb\".\n\n(fn SVG W H FORMAT &optional ID BG)",
    );
}
