# An agent drives eas: one transcript, end to end

This is a real session. Every command below was run in this repository
(eas 0.2.2, GNU Emacs 30, Linux, `TZ=UTC`). The outputs are pasted as
they came back. Long outputs are trimmed with `jq`, and the jq filter is
part of the command shown. Some pretty-printed JSON has been put on
fewer lines to save space. No values were changed. Where the transcript
itself cuts something, it says so with "…".

The loop follows `docs/design/engine.md` section 3: describe, example,
check, render as text, open a live view, let a "human" brush it, read
back what was picked, export.

## 1. Pick a template

The agent wants a time series with a brush on x. `describe templates`
lists every template. `overview-plus-detail` (S&P 500 prices, an
overview whose x brush zooms the detail chart) fits.

```sh
bin/eas describe templates \
  | jq '{ok, data: [.data.templates[] | select(.name=="overview-plus-detail")
                    | {name, doc, slots: (.slots|keys), example}], next}'
```

```json
{
  "ok": true,
  "data": [
    {
      "name": "overview-plus-detail",
      "doc": "A detail area chart over an overview area chart; brushing the overview zooms the detail (Vega gallery: overview-plus-detail).",
      "slots": ["color", "data", "description", "height", "overview_height",
                "title", "width", "x", "y"],
      "example": "/work/repo/examples/vega/overview-plus-detail.data.json"
    }
  ],
  "next": ["bin/eas example air-traffic", "bin/eas check SPEC.json"]
}
```

## 2. Get bindings that render as they are

The agent wants the binding shape, so it saves the example bindings
to a file.

```sh
bin/eas example overview-plus-detail \
  | jq -c '{contract, ok, next, rows: (.data.data|length),
            first: .data.data[0:2], last: .data.data[-1]}'
bin/eas example overview-plus-detail | jq .data > /tmp/sp500.json
```

```json
{"contract":"chart/v1","ok":true,
 "next":["bin/eas check overview-plus-detail --data /work/repo/examples/vega/overview-plus-detail.data.json",
         "bin/eas render overview-plus-detail --data /work/repo/examples/vega/overview-plus-detail.data.json --backend text"],
 "rows":123,
 "first":[{"date":"Jan 1 2000","price":1394.46},{"date":"Feb 1 2000","price":1366.42}],
 "last":{"date":"Mar 1 2010","price":1140.45}}
```

## 3. Check the bindings

The agent wants to resolve, validate and compile without drawing.

```sh
bin/eas check overview-plus-detail --data /tmp/sp500.json
```

```json
{
  "contract": "chart/v1",
  "data": {
    "hash": "sha256:25cec56cea6a4c15cddd547959933709a560b0955bbc46b01b3312ce3ccfa891",
    "native": true,
    "template": "overview-plus-detail",
    "warnings": []
  },
  "next": ["bin/eas render overview-plus-detail --data /tmp/sp500.json --backend text", "bin/eas explain overview-plus-detail --data /tmp/sp500.json --stage scene"],
  "ok": true
}
```

## 4. Look at it as text

The agent wants to see the chart in its own context, cheaply. `--raw`
prints the drawing on its own. Without `--raw`, the same text sits in
`.data.output` of the envelope, and `next` there suggests `export`,
`explain` and an `emacsclient` open.

```sh
bin/eas render overview-plus-detail --data /tmp/sp500.json --backend text --cols 80 --rows 24 --raw
```

```text
1,600┤                                                     ▂  ▃
     │ ▆▂▃▇▂                                            ▂▁▄█▅██▆
1,400┤▆█████▁▂                                     ▁  ▃▇████████▅▂█
     │████████ ▄▂                             ▂▂▄▆▇█▆▇█████████████▆▇
1,200┤████████▅██▃ ▃▃▂            ▁▃▂▂▁▁▂▇▇▇▆████████████████████████          ▁
     │████████████▄███▆▁         ▄███████████████████████████████████▃     ▁▄▇██
1,000┤██████████████████▁ ▂  ▁▅▇██████████████████████████████████████    ▂█████
     │███████████████████▂█▆▃██████████████████████████████████████████▃ ▇██████
  800┤██████████████████████████████████████████████████████████████████▆███████
     │██████████████████████████████████████████████████████████████████████████
  600┤██████████████████████████████████████████████████████████████████████████
     │██████████████████████████████████████████████████████████████████████████
  400┤██████████████████████████████████████████████████████████████████████████
     │██████████████████████████████████████████████████████████████████████████
  200┤██████████████████████████████████████████████████████████████████████████
     │██████████████████████████████████████████████████████████████████████████
    0└┬─────────────┬──────────────┬─────────────┬──────────────┬─────────────┬─
      2000         2002          2004           2006          2008          2010

      ▂▃▂▂▃▂▁▁ ▁                                ▁▁▁▁▁▁▁▂▂▂▃▄▃▃▄▃▁▁▂▁▁
      ████████▇██▇▆▇▇▇▆▅▄▃▄▃▃▄▄▅▅▆▆▇▆▆▆▆▇▇▇▇▇▇███████████████████████▅▄▃▂▃▄▅▆▆▆▆
      ██████████████████████████████████████████████████████████████████████████
      ┬─────────────┬──────────────┬─────────────┬──────────────┬─────────────┬─
      2000         2002          2004           2006          2008          2010
```

## 5. Open a live view in an Emacs

Live views live inside an Emacs, so the stateless `bin/eas` cannot open
one. The agent starts a private daemon and talks to it through
`emacsclient`. `emacsclient` prints a Lisp string, so `jq -r .` unwraps
it. `eas` does not load `eas-agent` by itself, so the first call
requires it.

```sh
emacs --daemon=eas-transcript -Q -L /work/repo/src -l eas
emacsclient -s eas-transcript --eval '(progn (require (quote eas-agent))
  (eas-agent-json "open" "overview-plus-detail" :data "/tmp/sp500.json" :id "sp500"))' \
  | jq -r . | jq '{ok, data: (.data | {id, template, interactive, target, size, rows,
                    views: [.views[] | {id, domains}], params}), next}'
```

```json
{
  "ok": true,
  "data": {
    "id": "sp500", "template": "overview-plus-detail", "interactive": true,
    "target": "svg", "size": {"w": 763, "h": 529, "cell": [7, 14]}, "rows": 123,
    "views": [
      {"id": "vconcat_0", "domains": {"x": ["2000-01-01", "2010-03-01"], "y": [0.0, 1600.0]}},
      {"id": "vconcat_1", "domains": {"x": ["2000-01-01", "2010-03-01"], "y": [0.0, 1600.0]}}
    ],
    "params": [
      {"name": "brush", "view": "vconcat_1", "type": "interval", "bind": null,
       "summary": "empty", "value": null}
    ]
  },
  "next": [
    "emacsclient --eval '(eas-agent-json \"dispatch\" \"sp500\" \"{\\\"type\\\":\\\"key\\\",\\\"key\\\":\\\"+\\\"}\")'",
    "emacsclient --eval '(eas-agent-json \"log\" \"sp500\")'",
    "emacsclient --eval '(eas-agent-json \"selection\" \"sp500\")'"
  ]
}
```

## 6. Find where to press

A person drags across the overview strip. To do the same, the agent
needs that strip's pixel box and x scale, which are in the scene.

```sh
bin/eas explain overview-plus-detail --data /tmp/sp500.json --stage scene \
  | jq -c '.data.artifact.views[] | {id, bounds, params, x: .scales.x}'
```

```json
{"id":"vconcat_0","bounds":[38,10,720,390],"params":[],"x":{"domain":[946684800000.0,1267401600000.0],"field":"date","range":[38,758],"reverse":false,"type":"time","utc":false}}
{"id":"vconcat_1","bounds":[38,437,720,70],"params":["brush"],"x":{"domain":[946684800000.0,1267401600000.0],"field":"date","range":[38,758],"reverse":false,"type":"time","utc":false}}
```

The overview runs from y 437 to 507, and x maps 2000-01-01..2010-03-01
onto 38..758. January 2007 falls near x 534 and March 2009 near x 687.
The press goes at y 470, in the middle of the strip.

## 7. Brush the way a human would

The agent sends pointerdown, two pointermoves and pointerup through the
`dispatch` verb, the same event/v1 data the GUI glue makes from mouse
events. Each dispatch returns inspect, trimmed here to the brush and
the detail chart's x domain.

```sh
J='{ok, last: .data."last-event", brush: .data.params[0].summary, detail_x: .data.views[0].domains.x}'
emacsclient -s eas-transcript --eval '(eas-agent-json "dispatch" "sp500" "{\"type\":\"pointerdown\",\"px\":[534,470]}")' | jq -r . | jq -c "$J"
emacsclient -s eas-transcript --eval '(eas-agent-json "dispatch" "sp500" "{\"type\":\"pointermove\",\"px\":[610,470]}")' | jq -r . | jq -c "$J"
emacsclient -s eas-transcript --eval '(eas-agent-json "dispatch" "sp500" "{\"type\":\"pointermove\",\"px\":[687,470]}")' | jq -r . | jq -c "$J"
emacsclient -s eas-transcript --eval '(eas-agent-json "dispatch" "sp500" "{\"type\":\"pointerup\",\"px\":[687,470]}")'   | jq -r . | jq -c "$J"
```

```json
{"ok":true,"last":"pointerdown at [534 470]","brush":"empty","detail_x":["2000-01-01","2010-03-01"]}
{"ok":true,"last":"pointermove at [610 470]","brush":"date 2007-01-01T03:44:00Z..2008-01-27T23:28:00Z","detail_x":["2007-01-01T03:44:00Z","2008-01-27T23:28:00Z"]}
{"ok":true,"last":"pointermove at [687 470]","brush":"date 2007-01-01T03:44:00Z..2009-02-27T22:56:00Z","detail_x":["2007-01-01T03:44:00Z","2009-02-27T22:56:00Z"]}
{"ok":true,"last":"pointerup at [687 470]","brush":"date 2007-01-01T03:44:00Z..2009-02-27T22:56:00Z","detail_x":["2007-01-01T03:44:00Z","2009-02-27T22:56:00Z"]}
```

The brush grows with the drag. The detail chart's x domain follows it
(`scale.domain: {"param": "brush"}`), and the release keeps the brush.
The odd minutes (03:44) are where whole pixels land in time.

## 8. Read back what was picked

The agent wants to know what the brush covers without a screenshot.
`inspect` gives domains, the brush and a summary of the visible range.

```sh
emacsclient -s eas-transcript --eval '(eas-agent-json "inspect" "sp500")' | jq -r . \
  | jq '{ok, data: (.data | {views: [.views[] | {id, x: .domains.x,
          visible: (.visible | {n, first, last, min, max, "change-pct"})}],
          params: [.params[] | {name, summary, value}], "last-event"}), next}'
```

```json
{
  "ok": true,
  "data": {
    "views": [
      {"id": "vconcat_0", "x": ["2007-01-01T03:44:00Z", "2009-02-27T22:56:00Z"],
       "visible": {"n": 25, "first": 1406.82, "last": 735.09, "min": 735.09,
                   "max": 1549.38, "change-pct": -47.75}},
      {"id": "vconcat_1", "x": ["2000-01-01", "2010-03-01"],
       "visible": {"n": 123, "first": 1394.46, "last": 1140.45, "min": 735.09,
                   "max": 1549.38, "change-pct": -18.22}}
    ],
    "params": [
      {"name": "brush", "summary": "date 2007-01-01T03:44:00Z..2009-02-27T22:56:00Z",
       "value": {"type": "interval", "x": [1167623040000.0, 1235775360000.0],
                 "fields": {"x": "date"}}}
    ],
    "last-event": "pointerup at [687 470]"
  },
  "next": [
    "emacsclient --eval '(eas-agent-json \"dispatch\" \"sp500\" \"{\\\"type\\\":\\\"key\\\",\\\"key\\\":\\\"+\\\"}\")'",
    "emacsclient --eval '(eas-agent-json \"log\" \"sp500\")'",
    "emacsclient --eval '(eas-agent-json \"selection\" \"sp500\")'"
  ]
}
```

So the person picked the 2007-2009 drawdown: 25 months, from 1406.82
down to 735.09, -47.75 %. The `selection` verb returns the rows
themselves, here as an Org table.

```sh
emacsclient -s eas-transcript --eval '(eas-agent-json "selection" "sp500" :name "brush" :as "org")' \
  | jq -r . | jq -r '"n=\(.data.n) next=\(.next)", .data.org'
```

```text
n=25 next=["emacsclient --eval '(eas-agent-json \"inspect\" \"sp500\")'"]
| date | price |
|---+---|
| Feb 1 2007 | 1406.82 |
| Mar 1 2007 | 1420.86 |
| Apr 1 2007 | 1482.37 |
…
| Oct 1 2007 | 1549.38 |
…
| Oct 1 2008 | 968.75 |
| Nov 1 2008 | 896.24 |
| Dec 1 2008 | 903.25 |
| Jan 1 2009 | 825.88 |
| Feb 1 2009 | 735.09 |
```

(… 17 rows cut. Jan 1 2007 is out because the brush starts at 03:44
that day.) The `log` verb shows what the "human" did, oldest first.

```sh
emacsclient -s eas-transcript --eval '(eas-agent-json "log" "sp500")' | jq -r . | jq -c '.data[] | {seq, summary}'
```

```json
{"seq":1,"summary":"pointerdown at [534 470]"}
{"seq":2,"summary":"pointermove at [610 470]"}
{"seq":3,"summary":"pointermove at [687 470]"}
{"seq":4,"summary":"pointerup at [687 470]"}
```

## 9. Export pure Vega-Lite

For a deliverable, the agent wants the resolved spec as plain
Vega-Lite, with no `x-eas` keys left in it. `--raw` writes the bare
spec, and the envelope's `next` names the static step.

```sh
bin/eas export overview-plus-detail --data /tmp/sp500.json --vl | jq -c '{ok, contract, next}'
bin/eas export overview-plus-detail --data /tmp/sp500.json --vl --raw > /tmp/sp500.vl.json
jq -c '{keys: keys, rows: (.data.values|length), detail_x: .vconcat[0].encoding.x,
        overview_params: .vconcat[1].params}' /tmp/sp500.vl.json
grep -c 'x-eas' /tmp/sp500.vl.json
```

```text
{"ok":true,"contract":"chart/v1","next":["chart check SPEC.vl.json","chart build SPEC.vl.json --out chart.svg"]}
{"keys":["$schema","config","data","description","padding","spacing","title","vconcat"],"rows":123,"detail_x":{"field":"date","scale":{"domain":{"param":"brush"}},"type":"temporal"},"overview_params":[{"name":"brush","select":{"encodings":["x"],"mark":{"fill":"#333","fillOpacity":0.2,"stroke":"firebrick","strokeWidth":1},"type":"interval"}}]}
0
```

`export` is stateless: it exports the template and its bindings, not
the live view. The interval param is in the spec, but the 2007-2009
brush is not.

## 10. Static build: bin/chart is not available here

`chart build` is the static export step that turns `/tmp/sp500.vl.json`
into an SVG or PNG. bin/chart is the test oracle and export door, never
a runtime dependency, and it is not installed on this machine.

```sh
command -v chart || echo "chart: not on PATH"; ls bin/chart
emacsclient -s eas-transcript --eval '(list (eas-chart-available-p) eas-chart-program (eas-chart-missing-reason))'
```

```text
chart: not on PATH
ls: cannot access 'bin/chart': No such file or directory
(nil "chart"
     "bin/chart (chart) is not on PATH; install a Vega-Lite CLI renderer or set `eas-chart-program'")
```

With bin/chart installed, the next command would be
`chart build /tmp/sp500.vl.json --out chart.svg`. It was not run here.

## 11. Clean up

Kill only the private daemon this session started.

```sh
emacsclient -s eas-transcript --eval '(kill-emacs)'
```
