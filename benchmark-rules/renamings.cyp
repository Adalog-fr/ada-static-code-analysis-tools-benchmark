MATCH (e:AN_OBJECT_RENAMING_DECLARATION|A_PACKAGE_RENAMING_DECLARATION|A_FUNCTION_RENAMING_DECLARATION|A_PROCEDURE_RENAMING_DECLARATION|AN_EXCEPTION_RENAMING_DECLARATION|A_GENERIC_PACKAGE_RENAMING_DECLARATION)
WHERE toUpper(e.enclosing_unit) IN $unitList
RETURN e
 ORDER BY e.filename, e.line, e.column
