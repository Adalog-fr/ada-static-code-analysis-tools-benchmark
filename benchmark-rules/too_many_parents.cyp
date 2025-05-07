MATCH (typeDecl)<-[r:IS_PROGENITOR_OF|IS_ANCESTOR_OF]-(parent)
WITH typeDecl, count(r) as nbParents
WHERE toUpper(typeDecl.enclosing_unit) IN $unitList
AND nbParents >= $minNbParents
RETURN typeDecl, nbParents
 ORDER BY typeDecl.filename, typeDecl.line, typeDecl.column
