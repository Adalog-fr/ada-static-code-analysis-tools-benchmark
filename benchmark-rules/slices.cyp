MATCH (e:A_SLICE)
WHERE toUpper(e.enclosing_unit) IN $unitList
RETURN e
 ORDER BY e.filename, e.line, e.column
