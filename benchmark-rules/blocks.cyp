MATCH (e:A_BLOCK_STATEMENT)
RETURN e
 ORDER BY e.filename, e.line, e.column
