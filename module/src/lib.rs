// SPDX-License-Identifier: GPL-3.0-or-later
//! eas-geo-module: an optional native accelerator for eas.el's geo path.
//!
//! A port of eas-geo-stream.el, eas-geo-clip.el, eas-geo-raw.el,
//! eas-geo-polyhedral.el and eas-geo-proj.el that returns what the Elisp
//! returns, number for number, and prints SVG path data as
//! eas-geoshape-render.el does.  Pure Elisp stays the source of truth.
//! The binding to emacs-module.h is hand-written (ffi.rs): no crates.
//!
//! With the `raster' feature (resvg, miniz_oxide) it also draws SVG to
//! pixels and encodes them for terminals (raster.rs, encode.rs) for
//! eas-output.el's kitty and sixel modes.

pub mod api;
#[cfg(feature = "raster")]
pub mod api_raster;
pub mod clip;
pub mod clip_circle;
pub mod clip_rect;
#[cfg(feature = "raster")]
pub mod encode;
pub mod ffi;
pub mod geom;
pub mod math;
pub mod polyhedral;
pub mod proj;
#[cfg(feature = "raster")]
pub mod raster;
#[cfg(all(test, feature = "raster"))]
mod raster_tests;
pub mod raw;
pub mod raw_extra;
pub mod resample;
pub mod sink;
pub mod stream;
pub mod svg;

/// Emacs loads only modules that declare a GPL-compatible license.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: i32 = 0;

/// The module's entry point.
///
/// # Safety
///
/// Called by Emacs with a valid runtime.
#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(runtime: *mut ffi::Runtime) -> i32 {
    // SAFETY: Emacs passes a valid runtime.
    let rt = unsafe { &mut *runtime };
    if (rt.size as usize) < std::mem::size_of::<ffi::Runtime>() {
        return 1;
    }
    // SAFETY: the runtime's function returns the init environment.
    let env = unsafe { (rt.get_environment)(runtime) };
    // SAFETY: ENV is valid; its size says which fields exist.
    if (unsafe { (*env).size } as usize) < std::mem::size_of::<ffi::Env>() {
        return 2;
    }
    api::init(ffi::E(env));
    #[cfg(feature = "raster")]
    api_raster::init(ffi::E(env));
    0
}
