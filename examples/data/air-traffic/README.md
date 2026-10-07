# U.S. airline traffic, 2021-2024

Monthly passenger enplanements, revenue passenger-miles, available
seat-miles and load factors for U.S. airlines, systemwide, domestic and
international, seasonally adjusted and not, January 2021 to December 2024.

- `december-2024-air-traffic-tables-1-24.xlsx`: the U.S. Department of
  Transportation, Bureau of Transportation Statistics "Air Traffic Data"
  release for December 2024 (Tables A, B and 1-24), as published. A work of
  the U.S. Government, in the public domain.
- `air-traffic-2021-2024.csv`: Tables 1-24 flattened into one tidy table,
  one row per table, year and month: `table, title, unit, year, month,
  value`. Regenerate it with `python3 scripts/air-traffic-csv.py XLSX CSV`.
