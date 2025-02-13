MATCH (e:A_BLOCK_STATEMENT)
WHERE toUpper(e.enclosing_unit) IN $unitList
RETURN e
 ORDER BY e.filename, e.line, e.column
