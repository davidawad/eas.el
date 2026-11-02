// SPDX-License-Identifier: GPL-3.0-or-later
//! Terminal image encoders for eas-output.el: the Kitty graphics
//! protocol (zlib RGBA, o=z, or PNG, f=100) and DEC sixel, plus the PNG
//! the goldens are kept as.  Input is straight (not premultiplied) RGBA,
//! row-major, 4 bytes a pixel.
//!
//! Every escape sequence is ASCII, so Emacs takes it as a plain string.
//! The Kitty transmit asks for no replies (q=2): a reply would arrive as
//! keyboard input.

use miniz_oxide::deflate::compress_to_vec_zlib;

/// Base64 payload bytes per Kitty chunk (the protocol's maximum).
pub const KITTY_CHUNK: usize = 4096;

const B64: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/// Standard base64 with padding.
pub fn base64(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    for c in data.chunks(3) {
        let n = (c[0] as u32) << 16
            | (*c.get(1).unwrap_or(&0) as u32) << 8
            | *c.get(2).unwrap_or(&0) as u32;
        out.push(B64[(n >> 18) as usize & 63] as char);
        out.push(B64[(n >> 12) as usize & 63] as char);
        out.push(if c.len() > 1 {
            B64[(n >> 6) as usize & 63] as char
        } else {
            '='
        });
        out.push(if c.len() > 2 {
            B64[n as usize & 63] as char
        } else {
            '='
        });
    }
    out
}

/// The Kitty transmit (a=t) of image ID: CONTROL holds the format keys,
/// PAYLOAD the raw bytes, sent base64 in chunks of `KITTY_CHUNK'.
pub fn kitty_chunks(control: &str, id: u32, payload: &[u8]) -> String {
    let b = base64(payload);
    let chunks: Vec<&[u8]> = if b.is_empty() {
        vec![&[][..]]
    } else {
        b.as_bytes().chunks(KITTY_CHUNK).collect()
    };
    let mut out = String::with_capacity(b.len() + chunks.len() * 16 + 64);
    for (i, c) in chunks.iter().enumerate() {
        let more = if i + 1 < chunks.len() { 1 } else { 0 };
        out.push_str("\x1b_G");
        if i == 0 {
            out.push_str(&format!("a=t,{control},i={id},q=2,"));
        }
        out.push_str(&format!("m={more};"));
        // SAFETY of the conversion: base64 is ASCII.
        out.push_str(std::str::from_utf8(c).unwrap());
        out.push_str("\x1b\\");
    }
    out
}

/// Kitty transmit of RGBA (W x H) as zlib-compressed f=32 data.
pub fn kitty_rgba(rgba: &[u8], w: u32, h: u32, id: u32) -> String {
    let z = compress_to_vec_zlib(rgba, 1);
    kitty_chunks(&format!("f=32,o=z,s={w},v={h}"), id, &z)
}

/// Kitty transmit of PNG bytes (f=100: the terminal decodes).
pub fn kitty_png(png: &[u8], id: u32) -> String {
    kitty_chunks("f=100", id, png)
}

/// The CRC-32 PNG chunks carry (zlib's).
pub fn crc32(data: &[u8]) -> u32 {
    let mut table = [0u32; 256];
    for (n, t) in table.iter_mut().enumerate() {
        let mut c = n as u32;
        for _ in 0..8 {
            c = if c & 1 != 0 {
                0xedb8_8320 ^ (c >> 1)
            } else {
                c >> 1
            };
        }
        *t = c;
    }
    let mut c = 0xffff_ffffu32;
    for &b in data {
        c = table[((c ^ b as u32) & 0xff) as usize] ^ (c >> 8);
    }
    c ^ 0xffff_ffff
}

fn png_chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
    out.extend_from_slice(&(data.len() as u32).to_be_bytes());
    let start = out.len();
    out.extend_from_slice(kind);
    out.extend_from_slice(data);
    let crc = crc32(&out[start..]);
    out.extend_from_slice(&crc.to_be_bytes());
}

/// RGBA (W x H) as an 8-bit RGBA PNG, filter 0 on every row.
pub fn png(rgba: &[u8], w: u32, h: u32) -> Vec<u8> {
    let mut raw = Vec::with_capacity((w as usize * 4 + 1) * h as usize);
    for row in rgba.chunks(w as usize * 4) {
        raw.push(0);
        raw.extend_from_slice(row);
    }
    let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
    let mut ihdr = Vec::with_capacity(13);
    ihdr.extend_from_slice(&w.to_be_bytes());
    ihdr.extend_from_slice(&h.to_be_bytes());
    ihdr.extend_from_slice(&[8, 6, 0, 0, 0]);
    png_chunk(&mut out, b"IHDR", &ihdr);
    png_chunk(&mut out, b"IDAT", &compress_to_vec_zlib(&raw, 6));
    png_chunk(&mut out, b"IEND", &[]);
    out
}

/// Pixel I of RGBA composited over BG, as [R G B].
#[inline]
fn over(rgba: &[u8], i: usize, bg: [u8; 3]) -> [u8; 3] {
    let a = rgba[i + 3] as u32;
    let mix = |c: u8, b: u8| ((c as u32 * a + b as u32 * (255 - a) + 127) / 255) as u8;
    [
        mix(rgba[i], bg[0]),
        mix(rgba[i + 1], bg[1]),
        mix(rgba[i + 2], bg[2]),
    ]
}

/// A palette of at most MAX colours for RGB pixels PX: the most common
/// 15-bit colours, each the mean of the pixels it stands for.  Returns
/// the palette and the 32768-entry map from a 15-bit colour to its
/// palette index (nearest by squared distance).
pub fn palette(px: &[[u8; 3]], max: usize) -> (Vec<[u8; 3]>, Vec<u16>) {
    let key = |p: &[u8; 3]| {
        ((p[0] as usize >> 3) << 10) | ((p[1] as usize >> 3) << 5) | (p[2] as usize >> 3)
    };
    let mut count = vec![0u32; 32768];
    let mut sum = vec![[0u64; 3]; 32768];
    for p in px {
        let k = key(p);
        count[k] += 1;
        for c in 0..3 {
            sum[k][c] += p[c] as u64;
        }
    }
    let mut used: Vec<usize> = (0..32768).filter(|&k| count[k] > 0).collect();
    // Most common first; ties by key, so the palette is deterministic.
    used.sort_by(|&a, &b| count[b].cmp(&count[a]).then(a.cmp(&b)));
    used.truncate(max.max(1));
    let pal: Vec<[u8; 3]> = used
        .iter()
        .map(|&k| {
            let n = count[k] as u64;
            [
                (sum[k][0] / n) as u8,
                (sum[k][1] / n) as u8,
                (sum[k][2] / n) as u8,
            ]
        })
        .collect();
    let mut map = vec![u16::MAX; 32768];
    for (i, &k) in used.iter().enumerate() {
        map[k] = i as u16;
    }
    for k in 0..32768 {
        if count[k] > 0 && map[k] == u16::MAX {
            let c = [
                ((k >> 10) << 3 | 4) as i32,
                (((k >> 5) & 31) << 3 | 4) as i32,
                ((k & 31) << 3 | 4) as i32,
            ];
            let best = pal
                .iter()
                .enumerate()
                .min_by_key(|(_, p)| (0..3).map(|j| (p[j] as i32 - c[j]).pow(2)).sum::<i32>())
                .map(|(i, _)| i)
                .unwrap_or(0);
            map[k] = best as u16;
        }
    }
    (pal, map)
}

fn sixel_run(out: &mut String, ch: u8, n: usize) {
    match n {
        0 => {}
        1..=3 => (0..n).for_each(|_| out.push(ch as char)),
        _ => {
            out.push('!');
            out.push_str(&n.to_string());
            out.push(ch as char);
        }
    }
}

/// RGBA (W x H) as a DEC sixel image (DCS P1;P2;P3 q ... ST), composited
/// over BG, with a palette of at most 256 registers.
pub fn sixel(rgba: &[u8], w: u32, h: u32, bg: [u8; 3]) -> String {
    let (w, h) = (w as usize, h as usize);
    let px: Vec<[u8; 3]> = (0..w * h).map(|i| over(rgba, i * 4, bg)).collect();
    let (pal, map) = palette(&px, 256);
    let key = |p: &[u8; 3]| {
        ((p[0] as usize >> 3) << 10) | ((p[1] as usize >> 3) << 5) | (p[2] as usize >> 3)
    };
    let idx: Vec<u16> = px.iter().map(|p| map[key(p)]).collect();
    let mut out = String::with_capacity(w * h / 2);
    // P2=1: pixels a band leaves 0 keep what is there (none here).
    out.push_str(&format!("\x1bP0;1;0q\"1;1;{w};{h}"));
    for (i, p) in pal.iter().enumerate() {
        let pct = |c: u8| (c as u32 * 100 + 127) / 255;
        out.push_str(&format!("#{i};2;{};{};{}", pct(p[0]), pct(p[1]), pct(p[2])));
    }
    let mut bits = vec![0u8; w];
    let mut present = vec![false; pal.len()];
    for top in (0..h).step_by(6) {
        let rows = (h - top).min(6);
        present.iter_mut().for_each(|p| *p = false);
        for r in 0..rows {
            for x in 0..w {
                present[idx[(top + r) * w + x] as usize] = true;
            }
        }
        let mut first = true;
        for (c, _) in present.iter().enumerate().filter(|(_, &p)| p) {
            for (x, b) in bits.iter_mut().enumerate() {
                *b = 0;
                for r in 0..rows {
                    if idx[(top + r) * w + x] as usize == c {
                        *b |= 1 << r;
                    }
                }
            }
            if !first {
                out.push('$');
            }
            first = false;
            out.push_str(&format!("#{c}"));
            let mut x = 0;
            while x < w {
                let b = bits[x];
                let mut n = 1;
                while x + n < w && bits[x + n] == b {
                    n += 1;
                }
                sixel_run(&mut out, 63 + b, n);
                x += n;
            }
        }
        if top + 6 < h {
            out.push('-');
        }
    }
    out.push_str("\x1b\\");
    out
}
