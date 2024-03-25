// Does not give the same results as GNATcheck, due to the way the GNATcheck rule is written
MATCH (e:A_DERIVED_RECORD_EXTENSION_DEFINITION|AN_INTERFACE_TYPE_DEFINITION|A_DERIVED_TYPE_DEFINITION|A_FORMAL_DERIVED_TYPE_DEFINITION|A_FORMAL_INTERFACE_TYPE_DEFINITION|A_PROTECTED_DEFINITION|A_TASK_DEFINITION)
MATCH (enclosingE)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(e)

CALL {
    WITH enclosingE
    MATCH path = (enclosingE)<-[:IS_ANCESTOR_OF|IS_PROGENITOR_OF*]-(endNode)

    CALL {
        WITH path
        WITH nodes(path) AS nodesInPath
        UNWIND nodesInPath AS node
        MATCH (node)-[:CORRESPONDING_TYPE_DECLARATION_VIEW]->(typeDecl)
        WHERE ANY(label IN labels(typeDecl) WHERE label IN ['A_TAGGED_RECORD_TYPE_DEFINITION', 'AN_INTERFACE_TYPE_DEFINITION'])
        WITH count(*) > 0 as hasCorrespondingLabel
        WITH collect(hasCorrespondingLabel) AS hasCorrespondingLabelList
        WITH any(label IN hasCorrespondingLabelList WHERE label = true) AS hasCorrespondingLabel
        RETURN hasCorrespondingLabel
    }

    WITH path, hasCorrespondingLabel
    ORDER BY length(path) DESC
    LIMIT 1
    RETURN length(path) AS longestDeepInheritanceHierarchy, hasCorrespondingLabel
}

WITH enclosingE, longestDeepInheritanceHierarchy, hasCorrespondingLabel
WHERE longestDeepInheritanceHierarchy > 2 AND hasCorrespondingLabel
WITH enclosingE, longestDeepInheritanceHierarchy

RETURN enclosingE.filename + ':' + enclosingE.line + ':' + enclosingE.column AS pathInfo,
       longestDeepInheritanceHierarchy
ORDER BY enclosingE.filename, enclosingE.line, enclosingE.column
