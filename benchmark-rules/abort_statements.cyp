MATCH (abortStmt:AN_ABORT_STATEMENT)
WHERE toUpper(abortStmt.enclosing_unit) IN $unitList
RETURN abortStmt
 ORDER BY abortStmt.filename, abortStmt.line, abortStmt.column
