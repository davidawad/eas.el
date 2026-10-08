// SPDX-License-Identifier: GPL-3.0-or-later
//! d3-geo streams (eas-geo-stream.el): the trait and the plain stages.

use std::cell::RefCell;
use std::rc::Rc;

/// A d3-geo stream.  M is the point's flag: 0 for none (Elisp nil), else
/// the clip intersection flag (1, 2 or 3).
pub trait Stream {
    fn point(&mut self, x: f64, y: f64, m: u8);
    fn line_start(&mut self);
    fn line_end(&mut self);
    fn polygon_start(&mut self);
    fn polygon_end(&mut self);
    fn sphere(&mut self);
}

pub type BoxStream = Box<dyn Stream>;

/// A stream shared by several upstreams (albersUsa's three insets).
pub struct Shared<S: Stream>(pub Rc<RefCell<S>>);

impl<S: Stream> Clone for Shared<S> {
    fn clone(&self) -> Self {
        Shared(self.0.clone())
    }
}

impl<S: Stream> Stream for Shared<S> {
    fn point(&mut self, x: f64, y: f64, m: u8) {
        self.0.borrow_mut().point(x, y, m)
    }
    fn line_start(&mut self) {
        self.0.borrow_mut().line_start()
    }
    fn line_end(&mut self) {
        self.0.borrow_mut().line_end()
    }
    fn polygon_start(&mut self) {
        self.0.borrow_mut().polygon_start()
    }
    fn polygon_end(&mut self) {
        self.0.borrow_mut().polygon_end()
    }
    fn sphere(&mut self) {
        self.0.borrow_mut().sphere()
    }
}

/// A stream that drops everything.
pub struct Null;

impl Stream for Null {
    fn point(&mut self, _x: f64, _y: f64, _m: u8) {}
    fn line_start(&mut self) {}
    fn line_end(&mut self) {}
    fn polygon_start(&mut self) {}
    fn polygon_end(&mut self) {}
    fn sphere(&mut self) {}
}

/// Report a skipped test: print why, and append "NAME: REASON" to the
/// file TEST_SKIP_LOG names, as the Elisp suite does.  A skip is not a
/// pass.
#[cfg(test)]
pub fn test_skip(name: &str, reason: &str) {
    use std::io::Write;
    eprintln!("SKIP {name}: {reason}");
    if let Some(log) = std::env::var_os("TEST_SKIP_LOG") {
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(log) {
            let _ = writeln!(f, "{name}: {reason}");
        }
    }
}
