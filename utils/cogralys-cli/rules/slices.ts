import { join } from "https://deno.land/std/path/mod.ts";
import { Query } from "https://deno.land/x/neo4j_driver_lite@5.18.0/core/types.ts";
import { RuleType, responseRecords, ruleConstructorParamsExtended } from "./types/rules.ts";

export default class Slices extends RuleType {
    static readonly ruleName = 'Slices';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "slices.cyp"));
    }

    static initialize(params: ruleConstructorParamsExtended): Slices {
        return new Slices(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("e").properties;
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${Slices.ruleName}\n`));
        })
    }
}
