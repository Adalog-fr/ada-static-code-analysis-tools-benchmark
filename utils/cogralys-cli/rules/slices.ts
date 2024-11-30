import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class Slices extends RuleType {
    static override readonly ruleName = 'Slices';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "slices.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): Slices {
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
