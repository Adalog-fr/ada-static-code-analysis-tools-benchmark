import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class AbortStatements extends RuleType {
    static override readonly ruleName = 'Abort_Statements';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "abort_statements.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): AbortStatements {
      return new AbortStatements(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("abortStmt").properties;
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${AbortStatements.ruleName}\n`));
        })
    }
}
