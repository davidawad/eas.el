"""Flatten the BTS 'Air Traffic' December 2024 workbook (Tables 1-24) into one tidy CSV:
table, title, unit, year, month, value. Table A/B (single-row summaries) are skipped."""
import csv, re, sys, zipfile

src, out = sys.argv[1], sys.argv[2]
z = zipfile.ZipFile(src)
ss = [re.sub(r"<[^>]+>", "", s) for s in re.findall(r"<si>(.*?)</si>", z.read("xl/sharedStrings.xml").decode(), re.S)]
wb = z.read("xl/workbook.xml").decode()
names = re.findall(r'<sheet [^>]*name="([^"]+)"[^>]*r:id="([^"]+)"', wb)
rels = dict(re.findall(r'Id="([^"]+)"[^>]*Target="([^"]+)"', z.read("xl/_rels/workbook.xml.rels").decode()))
rels.update({k: v for v, k in re.findall(r'Target="([^"]+)"[^>]*Id="([^"]+)"', z.read("xl/_rels/workbook.xml.rels").decode())})
months = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]

def cells(xml):
    for r in re.findall(r"<row [^>]*>(.*?)</row>", xml, re.S):
        row = {}
        for c in re.finditer(r'<c r="([A-Z]+)\d+"([^>]*?)(?:/>|>(.*?)</c>)', r, re.S):
            ref, attrs, body = c.groups()
            v = re.search(r"<v>(.*?)</v>", body or "")
            v = v.group(1) if v else ""
            if 't="s"' in attrs and v:
                v = ss[int(v)]
            row[ref] = v.strip()
        yield row

rows = []
for name, rid in names:
    if not re.fullmatch(r"table\d+", name):
        continue
    path = "xl/" + rels[rid].lstrip("/").removeprefix("xl/")
    data = list(cells(z.read(path).decode()))
    title = re.sub(r"\s+", " ", data[0].get("A", ""))
    unit = re.sub(r"\s+", " ", data[1].get("A", "")) if len(data) > 1 else ""
    header = next((r for r in data if r.get("B", "").isdigit()), None)
    if not header:
        continue
    years = {col: int(v) for col, v in header.items() if v.isdigit()}
    for r in data:
        m = r.get("A", "")
        if m not in months:
            continue
        for col, year in years.items():
            v = r.get(col, "")
            try:
                val = round(float(v), 4)
            except ValueError:
                continue
            rows.append({"table": int(name[5:]), "title": title, "unit": unit,
                         "year": year, "month": months.index(m) + 1, "value": val})
with open(out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["table", "title", "unit", "year", "month", "value"])
    w.writeheader(); w.writerows(rows)
print(len(rows), "rows,", len({r["table"] for r in rows}), "tables")
for t in sorted({(r["table"], r["title"]) for r in rows}):
    print(" ", t[0], t[1][:110])
