import { join } from "https://deno.land/std/path/mod.ts";
import { Query } from "https://deno.land/x/neo4j_driver_lite@5.18.0/core/types.ts";
import { RuleType, responseRecords, ruleConstructorParamsExtended } from "./types/rules.ts";

export default class Constructors extends RuleType {
    static readonly ruleName = 'Constructors';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "constructors.cyp"));
    }

    static initialize(params: ruleConstructorParamsExtended): Constructors {
        return new Constructors(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("function").properties;
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${Constructors.ruleName}\n`));
        })
    }
}
