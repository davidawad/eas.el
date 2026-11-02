// SPDX-License-Identifier: GPL-3.0-or-later
//! Tests for raster.rs and encode.rs (cargo test --release --features raster).

use crate::encode::{
    base64, crc32, kitty_chunks, kitty_png, kitty_rgba, palette, png, sixel, KITTY_CHUNK,
};
use crate::raster::rgba;
use miniz_oxide::inflate::decompress_to_vec_zlib;

fn unbase64(s: &str) -> Vec<u8> {
    let val = |c: u8| match c {
        b'A'..=b'Z' => c - b'A',
        b'a'..=b'z' => c - b'a' + 26,
        b'0'..=b'9' => c - b'0' + 52,
        b'+' => 62,
        b'/' => 63,
        _ => panic!("bad base64 {c}"),
    };
    let mut out = Vec::new();
    for q in s.as_bytes().chunks(4) {
        let n = q
            .iter()
            .filter(|&&c| c != b'=')
            .fold(0u32, |a, &c| a << 6 | val(c) as u32)
            << (6 * q.iter().filter(|&&c| c == b'=').count());
        let bytes = [(n >> 16) as u8, (n >> 8) as u8, n as u8];
        out.extend_from_slice(&bytes[..3 - q.iter().filter(|&&c| c == b'=').count()]);
    }
    out
}

/// The payload of a Kitty transmit, its chunks' base64 joined, and the
/// first chunk's control keys.
fn kitty_payload(seq: &str) -> (String, Vec<u8>, usize) {
    let mut control = String::new();
    let mut b64 = String::new();
    let mut n = 0;
    for part in seq.split("\x1b_G").filter(|p| !p.is_empty()) {
        let body = part.strip_suffix("\x1b\\").expect("chunk ends in ST");
        let (keys, data) = body.split_once(';').unwrap();
        assert!(data.len() <= KITTY_CHUNK);
        if n == 0 {
            control = keys.to_string();
        }
        b64.push_str(data);
        n += 1;
    }
    (control, unbase64(&b64), n)
}

#[test]
fn base64_matches_rfc4648_vectors() {
    for (i, o) in [
        ("", ""),
        ("f", "Zg=="),
        ("fo", "Zm8="),
        ("foo", "Zm9v"),
        ("foob", "Zm9vYg=="),
        ("fooba", "Zm9vYmE="),
        ("foobar", "Zm9vYmFy"),
    ] {
        assert_eq!(base64(i.as_bytes()), o);
        assert_eq!(unbase64(o), i.as_bytes());
    }
}

#[test]
fn kitty_transmit_chunks_and_round_trips() {
    let (w, h) = (97u32, 53u32);
    // xorshift noise: zlib cannot shrink it below one chunk.
    let mut r = 0x2545_f491u32;
    let px: Vec<u8> = (0..w * h * 4)
        .map(|_| {
            r ^= r << 13;
            r ^= r >> 17;
            r ^= r << 5;
            r as u8
        })
        .collect();
    let seq = kitty_rgba(&px, w, h, 42);
    let (control, z, n) = kitty_payload(&seq);
    assert_eq!(control, "a=t,f=32,o=z,s=97,v=53,i=42,q=2,m=1");
    assert!(n > 1, "a large payload takes several chunks");
    assert_eq!(
        seq.matches("m=0;").count(),
        1,
        "only the last chunk ends the transmit"
    );
    assert_eq!(decompress_to_vec_zlib(&z).unwrap(), px);
    let one = kitty_chunks("f=100", 3, b"x");
    assert_eq!(one, "\x1b_Ga=t,f=100,i=3,q=2,m=0;eA==\x1b\\");
}

#[test]
fn png_is_valid_and_round_trips() {
    let (w, h) = (5u32, 3u32);
    let px: Vec<u8> = (0..w * h * 4).map(|i| i as u8).collect();
    let p = png(&px, w, h);
    assert_eq!(&p[..8], b"\x89PNG\r\n\x1a\n");
    assert_eq!(&p[12..16], b"IHDR");
    assert_eq!(u32::from_be_bytes(p[16..20].try_into().unwrap()), w);
    // The IHDR CRC covers its type and data; "IEND" has zlib's known CRC.
    assert_eq!(
        u32::from_be_bytes(p[29..33].try_into().unwrap()),
        crc32(&p[12..29])
    );
    assert_eq!(&p[33 + 4..33 + 8], b"IDAT");
    let idat_len = u32::from_be_bytes(p[33..37].try_into().unwrap()) as usize;
    let raw = decompress_to_vec_zlib(&p[41..41 + idat_len]).unwrap();
    assert_eq!(raw.len(), (w as usize * 4 + 1) * h as usize);
    assert_eq!(&raw[1..21], &px[..20]);
    assert!(p.ends_with(&[0, 0, 0, 0, b'I', b'E', b'N', b'D', 0xae, 0x42, 0x60, 0x82]));
    let (control, data, _) = kitty_payload(&kitty_png(&p, 9));
    assert_eq!(control, "a=t,f=100,i=9,q=2,m=0");
    assert_eq!(data, p);
}

#[test]
fn sixel_encodes_a_two_colour_image_exactly() {
    // 4 x 7: a red left half, a blue right half; row 6 is a second band.
    // Equal counts tie by 15-bit key, so blue is register 0.
    let (w, h) = (4u32, 7u32);
    let mut px = Vec::new();
    for _ in 0..h {
        for x in 0..w {
            px.extend_from_slice(if x < 2 {
                &[255, 0, 0, 255]
            } else {
                &[0, 0, 255, 255]
            });
        }
    }
    let s = sixel(&px, w, h, [255, 255, 255]);
    assert_eq!(
        s,
        "\x1bP0;1;0q\"1;1;4;7#0;2;0;0;100#1;2;100;0;0#0??~~$#1~~??-#0??@@$#1@@??\x1b\\"
    );
}

#[test]
fn sixel_composites_transparency_and_run_length_encodes() {
    let (w, h) = (10u32, 6u32);
    let px = vec![0u8; (w * h * 4) as usize];
    let s = sixel(&px, w, h, [0, 0, 0]);
    assert_eq!(s, "\x1bP0;1;0q\"1;1;10;6#0;2;0;0;0#0!10~\x1b\\");
}

#[test]
fn palette_keeps_at_most_max_colours_and_maps_every_pixel() {
    let px: Vec<[u8; 3]> = (0..4096)
        .map(|i| [(i % 256) as u8, (i / 16) as u8, (i * 3 % 256) as u8])
        .collect();
    let (pal, map) = palette(&px, 16);
    assert_eq!(pal.len(), 16);
    for p in &px {
        let k = ((p[0] as usize >> 3) << 10) | ((p[1] as usize >> 3) << 5) | (p[2] as usize >> 3);
        assert!((map[k] as usize) < 16);
    }
}

#[test]
fn raster_draws_shapes_at_the_asked_size() {
    let svg = r##"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="50" viewBox="0 0 100 50">
      <rect width="100" height="50" fill="#ffffff"/><rect x="0" y="0" width="50" height="50" fill="#ff0000"/></svg>"##;
    let px = rgba(svg, 200, 100).unwrap();
    assert_eq!(px.len(), 200 * 100 * 4);
    let at = |x: usize, y: usize| &px[(y * 200 + x) * 4..(y * 200 + x) * 4 + 4];
    assert_eq!(at(10, 10), &[255, 0, 0, 255]);
    assert_eq!(at(190, 90), &[255, 255, 255, 255]);
    assert!(rgba("<not svg", 10, 10).is_err());
    assert!(rgba(svg, 0, 10).is_err());
}

#[test]
fn raster_draws_text_with_system_fonts() {
    let svg = r##"<svg xmlns="http://www.w3.org/2000/svg" width="120" height="40">
      <rect width="120" height="40" fill="#fff"/><text x="4" y="30" font-family="sans-serif" font-size="28">eas</text></svg>"##;
    let px = rgba(svg, 120, 40).unwrap();
    let dark = px.chunks(4).filter(|p| p[0] < 128).count();
    if dark == 0 {
        eprintln!("skip: no system font drew text (fontdb found none)");
        return;
    }
    assert!(dark > 50, "text drew {dark} dark pixels");
}
