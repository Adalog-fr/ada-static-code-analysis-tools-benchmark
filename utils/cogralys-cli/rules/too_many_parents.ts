import { join } from "https://deno.land/std/path/mod.ts";
import { Query } from "https://deno.land/x/neo4j_driver_lite@5.18.0/core/types.ts";
import { RuleType, responseRecords, ruleConstructorParamsExtended } from "./types/rules.ts";

export default class TooManyParents extends RuleType {
    static readonly ruleName = 'Too_Many_Parents';
    query: string;
    minNbParents: number;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile, minNbParents: number) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "too_many_parents.cyp"));
        this.minNbParents = minNbParents;
    }

    static initialize(params: ruleConstructorParamsExtended): TooManyParents {
        return new TooManyParents(params.cypherQueriesPath, params.timing, params.resultFile, params.minNbParents);
    }

    getQuery(): Query {
      return this.query;
    }

    getQueryParameters(): any {
        return {
            minNbParents: this.minNbParents
        };
    };

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("typeDecl").properties;
            const nbParents = elt.get("nbParents");
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${TooManyParents.ruleName} ${nbParents} parent(s)\n`));
        })
    }
}
