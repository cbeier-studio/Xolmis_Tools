---
description: "Use when editing Free Pascal/Lazarus source in Xolmis_Tools (.pp/.lpr). Enforces ObjFPC unit structure, naming consistency, and safe SQLDB/data-model patterns used by taxonomy_editor and dev_tools."
name: "Xolmis Pascal Conventions"
applyTo: ["**/*.pp", "**/*.lpr"]
---
# Xolmis Pascal Conventions

- This is a preference file: follow these conventions by default, and only diverge when the task explicitly requires it.
- Prefer the existing unit skeleton: `unit` header, `{$mode ObjFPC}{$H+}`, `interface`, `implementation`, then final `end.`.
- Prefer existing naming style: classes with `T` prefix, private/protected fields with `F` prefix, and PascalCase members.
- Preserve existing persistence-model contracts when present (`Clear`, `Assign`, `Clone`, `Diff`, `Validate`) and avoid changing method signatures unless requested.
- For string defaults and resets, prefer `EmptyStr` to match the current codebase style.
- For textual equality checks in models, prefer `SameText` unless case-sensitive comparison is explicitly required.
- Keep SQL/data mapping code explicit and defensive: assign defaults when reading JSON/DB fields and guard nullable inputs.
- Match current dependency style in `uses` clauses: keep them minimal and grouped by section (`interface` vs `implementation`).
- Target compatibility with Lazarus 4.0+ and Free Pascal 3.2+, with Win64 as the primary platform.
