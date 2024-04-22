MATCH (abortStmt:AN_ABORT_STATEMENT)
RETURN abortStmt
 ORDER BY abortStmt.filename, abortStmt.line, abortStmt.column
