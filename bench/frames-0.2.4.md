# Per-frame cost, 0.2.3 to 0.2.4 (eas-b2s.10)

Measured 2026-10-08 on one box (Linux, 8 cores, Emacs 30.1), one Emacs at a
time.  The generated report is docs/perf.md (`make bench-report`); this file
holds what that report cannot: the before/after for the release and the
frame benches in scripts/.

## Suite: before (0.2.3 baseline) and after (0.2.4 baseline)

"Before" is the 0.2.3 row of bench/history.json, recorded 2026-10-07 with
the suite's first commit (93ae5f4: 0.2.3 plus the hover readout and the
retained-line text diff).  "After" is `make bench-record` of this commit,
labelled 0.2.4.  KB per frame is the gated, machine-independent figure.
Milliseconds are the suite's medians, and the two baselines were recorded
on different machines, so read them as a trend only.  0.2.3 itself
(85d350e) could not be checked out here: this clone is partial
(blob:none), the remote cannot be reached, and 40 of the 0.2.3 files
(most of src/) are missing.  So 0.2.3 and main could not be benched in
the same run.

| workload | mode | SVG ms before | SVG ms after | text ms before | text ms after | SVG KB before | SVG KB after | text KB before | text KB after |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| order-book ladder push, 25 levels | byte | 4.8 | 2.6 | 9.4 | 5.1 | 662.4 | 204.2 | 1366.7 | 351.4 |
| order-book ladder push, 25 levels | native | 6.9 | 1.4 | 8.0 | 3.0 | 662.4 | 204.2 | 1366.6 | 351.3 |
| order-book ladder push, 100 levels | byte | 14.8 | 3.2 | 22.9 | 11.6 | 1982.9 | 481.2 | 2638.7 | 831.5 |
| order-book ladder push, 100 levels | native | 14.4 | 3.2 | 19.6 | 8.8 | 1982.9 | 481.2 | 2638.7 | 831.5 |
| depth push, 25 levels | byte | 6.0 | 4.1 | 8.5 | 7.7 | 673.5 | 368.5 | 1005.8 | 426.5 |
| depth push, 25 levels | native | 4.4 | 3.1 | 6.6 | 6.0 | 673.5 | 368.5 | 1005.8 | 426.5 |
| depth push, 100 levels | byte | 6.9 | 4.8 | 10.0 | 7.3 | 1129.8 | 801.0 | 1483.8 | 814.7 |
| depth push, 100 levels | native | 4.1 | 4.9 | 8.7 | 6.9 | 1129.8 | 800.9 | 1483.7 | 814.7 |
| candle stream with indicators | byte | 15.2 | 6.0 | 21.8 | 16.3 | 2473.2 | 1186.4 | 3789.6 | 2111.9 |
| candle stream with indicators | native | 14.1 | 3.7 | 14.1 | 15.3 | 2473.1 | 1186.4 | 3789.5 | 2111.9 |
| clock tick | byte | 1.3 | 0.8 | 6.1 | 4.1 | 136.7 | 109.0 | 1154.3 | 280.3 |
| clock tick | native | 0.8 | 0.6 | 3.5 | 2.0 | 136.7 | 109.0 | 1154.2 | 280.2 |
| pacman tick | byte | 3.2 | 2.6 | 13.6 | 7.9 | 349.6 | 269.2 | 1860.0 | 566.4 |
| pacman tick | native | 2.4 | 1.4 | 8.6 | 3.8 | 349.5 | 269.2 | 1860.0 | 566.4 |
| pi-monte-carlo step | byte | 1413.9 | 71.5 | 1261.8 | 66.7 | 187558.5 | 8328.4 | 184204.8 | 7264.4 |
| pi-monte-carlo step | native | 904.6 | 41.6 | 795.3 | 55.0 | 187558.4 | 8328.3 | 184204.7 | 7264.4 |
| airport-connections hover | byte | 0.7 | 0.7 | 7.4 | 5.1 | 248.4 | 229.0 | 1795.9 | 750.0 |
| airport-connections hover | native | 0.5 | 0.7 | 4.7 | 3.3 | 248.4 | 229.0 | 1795.9 | 750.0 |
| county-unemployment hover | byte | 1331.5 | 4.7 | 54.4 | 1.6 | 269007.9 | 1300.1 | 73897.2 | 537.3 |
| county-unemployment hover | native | 1171.8 | 2.5 | 6.1 | 1.1 | 269007.9 | 1300.1 | 73897.2 | 537.2 |
| projections first render | byte | 3631.6 | 149.5 | 3875.0 | 188.4 | 832385.6 | 25451.7 | 813326.2 | 21416.3 |
| projections first render | native | 3324.9 | 108.1 | 3532.5 | 104.5 | 832385.6 | 25451.7 | 813326.2 | 21416.3 |

## Frame benches at 0.2.4 (main): mean of 3 repeats (min-max)

ms per frame.  Text (`scripts/bench-frame-text.el`, 3x30 frames each
repeat, garbage collection as in an interactive session, which sets
gc-cons-percentage to 0.1):

| workload | byte update | byte render | byte patch | byte GC | byte total | native total | KB/frame |
|---|---:|---:|---:|---:|---:|---:|---:|
| order-book ladder push | 2.07 | 1.10 | 1.10 | 9.69 | 13.96 (12.34-15.16) | 14.20 (11.85-16.28) | 257 |
| depth-live push | 2.75 | 4.03 | 3.29 | 15.50 | 25.57 (21.40-28.41) | 24.39 (19.44-27.70) | 449 |
| clock tick | 0.99 | 0.94 | 1.99 | 5.49 | 9.41 (7.93-10.38) | 8.23 (7.01-9.18) | 145 |
| pacman tick | 3.05 | 2.14 | 2.37 | 14.77 | 22.32 (17.69-24.79) | 20.24 (15.56-23.27) | 405 |
| pi-monte-carlo step | 25.04 | 10.91 | 4.34 | 49.40 | 89.70 (79.49-100.19) | 86.45 (81.12-93.03) | 3535 |
| airport-connections hover | 1.59 | 2.70 | 4.65 | 6.35 | 15.29 (14.04-16.83) | 11.56 (11.23-11.73) | 292 |

SVG (`scripts/bench-frame-svg.el`, 40 frames, a cold first render over 2 opens):

| workload | byte update | byte draw | byte frame | native frame | KB update | KB draw |
|---|---:|---:|---:|---:|---:|---:|
| ladder push (25 levels) | 1.91 | 0.74 | 2.64 (1.98-3.73) | 1.88 (1.55-2.05) | 145.5 | 96.1 |
| depth push (25 levels) | 2.49 | 0.82 | 3.32 (2.70-4.39) | 2.39 (2.13-2.60) | 249.0 | 69.1 |
| clock tick | 0.80 | 0.39 | 1.19 (0.98-1.50) | 0.83 (0.73-0.94) | 85.3 | 42.9 |
| pacman tick | 3.20 | 1.63 | 4.82 (4.41-5.60) | 2.89 (2.27-3.58) | 208.5 | 214.5 |
| pi-monte-carlo slider step | 38.61 | 18.77 | 57.38 (50.97-63.79) | 47.83 (47.76-47.96) | 4901.1 | 2002.6 |
| airport-connections hover | 0.90 | 0.32 | 1.22 (1.17-1.28) | 1.10 (1.02-1.18) | 103.2 | 181.7 |
| county-unemployment hover | 1.25 | 3.45 | 4.70 (3.62-5.44) | 4.63 (3.87-5.06) | 114.6 | 1262.4 |
| airport-connections sweep | 1.37 | 1.62 | 2.99 (2.12-4.27) | 2.28 (2.15-2.49) | 119.2 | 183.8 |
| projections first render | 1786.38 | 500.26 | 2286.64 (2033.23-2453.23) | 1773.88 (1553.25-1886.62) | 366309.7 | 38179.3 |

Update path only (`scripts/bench-frame-update.el`, 30 frames, no renderer):

| workload | target | byte ms | native ms | conses/frame |
|---|---|---:|---:|---:|
| ladder push | svg | 1.71 (1.63-1.78) | 1.25 (0.96-1.42) | 12400 |
| ladder push | text | 2.03 (1.84-2.13) | 1.33 (1.04-1.77) | 16111 |
| ladder-100 push | svg | 2.68 (2.29-2.91) | 2.02 (1.51-2.62) | 20183 |
| ladder-100 push | text | 2.93 (2.84-3.07) | 2.10 (1.73-2.79) | 24762 |
| depth push | svg | 1.50 (1.43-1.55) | 1.10 (0.98-1.34) | 14134 |
| depth push | text | 1.49 (1.40-1.62) | 1.07 (0.94-1.23) | 15882 |
| depth-fixed push | svg | 1.20 (1.14-1.24) | 0.89 (0.72-1.02) | 12874 |
| depth-fixed push | text | 1.31 (1.21-1.41) | 0.94 (0.83-1.05) | 14622 |
| candle stream | svg | 4.21 (3.70-4.86) | 2.81 (2.30-3.16) | 39027 |
| candle stream | text | 3.56 (3.39-3.76) | 2.20 (1.95-2.59) | 35693 |
| clock tick | svg | 0.70 (0.67-0.76) | 0.58 (0.48-0.71) | 7212 |
| clock tick | text | 0.70 (0.66-0.72) | 0.52 (0.45-0.57) | 7690 |
| pacman tick | svg | 2.50 (1.68-3.22) | 1.60 (1.20-1.86) | 22635 |
| pacman tick | text | 2.18 (1.84-2.39) | 1.50 (1.28-1.75) | 22968 |
| pi-mc step | svg | 22.39 (21.90-22.87) | 15.42 (13.81-16.66) | 227307 |
| pi-mc step | text | 21.99 (20.12-24.16) | 14.37 (13.36-14.97) | 201880 |
| airport hover | svg | 1.01 (0.94-1.09) | 0.71 (0.68-0.75) | 8206 |
| airport hover | text | 1.09 (0.97-1.29) | 0.73 (0.60-0.93) | 8470 |

Real terminal (`scripts/bench-frame-tty.el` in `emacs -nw` under `tmux -L eas`, 110x46,
xterm-256color, 40 frames; every frame of every repeat equalled a full render):

| workload | byte ms/frame | native ms/frame | byte ms per redisplay, 4 updates | native ms per redisplay, 4 updates |
|---|---:|---:|---:|---:|
| order-book ladder push | 9.42 (7.95-10.52) | 7.87 (7.21-9.01) | 14.32 (14.12-14.57) | 12.87 (12.27-13.64) |
| depth-live push | 17.83 (15.92-20.13) | 14.34 (13.20-15.67) | 24.56 (20.41-27.23) | 20.18 (17.39-24.45) |
| clock tick | 9.61 (7.88-10.86) | 7.57 (6.97-8.33) | 10.89 (8.49-12.63) | 9.52 (8.22-11.48) |
| pacman tick | 13.63 (11.24-15.11) | 12.58 (12.52-12.64) | 20.61 (15.91-23.67) | 16.53 (15.92-17.03) |
| airport-connections hover | 21.15 (19.57-22.05) | 17.80 (15.82-19.65) | 19.04 (16.82-21.04) | 13.89 (12.91-14.81) |

The SVG bench's projections first render is cold: each of its 2 opens
starts with no projection retained.  The suite's render/projections opens
once to warm up and then measures 2 more opens, which reuse the retained
projections (89eef0d).  The real-terminal bench reports eas-text-render
as "not byte-compiled" in native mode because its check only knows byte
code.  The native .eln files were loaded.
