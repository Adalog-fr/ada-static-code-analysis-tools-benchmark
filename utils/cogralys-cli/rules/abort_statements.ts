import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParams, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class AbortStatements extends RuleType {
    static override readonly ruleName = 'Abort_Statements';
    query: string;

    constructor(params : ruleConstructorParams) {
        super(params, "abortStmt");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "abort_statements.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): AbortStatements {
      return new AbortStatements(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
