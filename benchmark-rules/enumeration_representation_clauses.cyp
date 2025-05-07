MATCH (e:AN_ENUMERATION_REPRESENTATION_CLAUSE)
WHERE toUpper(e.enclosing_unit) IN $unitList
RETURN e
 ORDER BY e.filename, e.line, e.column
