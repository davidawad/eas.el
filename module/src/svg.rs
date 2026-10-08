// SPDX-License-Identifier: GPL-3.0-or-later
//! SVG path data as `eas-geoshape--svg-rings' prints it.

use std::os::raw::{c_char, c_int};

extern "C" {
    fn snprintf(buf: *mut c_char, n: usize, fmt: *const c_char, ...) -> c_int;
}

/// `eas-geoshape--n-slow': "%.1f" through the C library, as Emacs's
/// `format' prints it, without a trailing ".0".
fn n_slow(v: f64, out: &mut String) {
    let mut buf = [0u8; 400];
    // SAFETY: the format takes one double and BUF is large enough for
    // any "%.1f" of a double (at most 309 integer digits).
    let len = unsafe {
        snprintf(buf.as_mut_ptr() as *mut c_char, buf.len(), c"%.1f".as_ptr(), v)
    };
    let len = (len.max(0) as usize).min(buf.len() - 1);
    let s = std::str::from_utf8(&buf[..len]).unwrap_or("");
    out.push_str(s.strip_suffix(".0").unwrap_or(s));
}

/// `eas-geoshape--push-n': V as "%.1f" writes it, trimmed.
#[inline]
pub fn push_n(v: f64, out: &mut String) {
    let s = v * 10.0;
    if -81910.0 < s && s < 81910.0 {
        let n = s.round_ties_even();
        let d = s - n;
        if -0.4999 < d && d < 0.4999 {
            let n = n as i64;
            let m = n.unsigned_abs();
            if n < 0 || (n == 0 && 1.0f64.copysign(v) < 0.0) {
                out.push('-');
            }
            push_int(m / 10, out);
            let r = m % 10;
            if r != 0 {
                out.push('.');
                out.push((b'0' + r as u8) as char);
            }
            return;
        }
    }
    n_slow(v, out);
}

#[inline]
fn push_int(mut m: u64, out: &mut String) {
    let mut buf = [0u8; 20];
    let mut i = buf.len();
    loop {
        i -= 1;
        buf[i] = b'0' + (m % 10) as u8;
        m /= 10;
        if m == 0 {
            break;
        }
    }
    // SAFETY: ASCII digits.
    out.push_str(unsafe { std::str::from_utf8_unchecked(&buf[i..]) });
}

/// `eas-geoshape--svg-rings': PATHS (relative) moved by X Y.
pub fn rings(x: f64, y: f64, paths: &[(bool, Vec<f64>)], out: &mut String) {
    for (closed, flat) in paths {
        let mut i = 0;
        while i + 1 < flat.len() {
            out.push(if i == 0 { 'M' } else { 'L' });
            push_n(x + flat[i], out);
            out.push(',');
            push_n(y + flat[i + 1], out);
            i += 2;
        }
        if *closed {
            out.push('Z');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn n(v: f64) -> String {
        let mut s = String::new();
        push_n(v, &mut s);
        s
    }

    #[test]
    fn numbers_print_as_emacs_format() {
        assert_eq!(n(0.0), "0");
        assert_eq!(n(-0.0), "-0");
        assert_eq!(n(-0.03), "-0");
        assert_eq!(n(12.34), "12.3");
        assert_eq!(n(12.35), "12.3"); // 12.35 is 12.3499999...
        assert_eq!(n(0.25), "0.2"); // a tie: the C library rounds to even
        assert_eq!(n(0.75), "0.8");
        assert_eq!(n(-7.96), "-8");
        assert_eq!(n(8191.94), "8191.9");
        assert_eq!(n(9000.0), "9000");
        assert_eq!(n(1e300).len(), 301);
        assert_eq!(n(f64::NAN), "nan");
    }
}
