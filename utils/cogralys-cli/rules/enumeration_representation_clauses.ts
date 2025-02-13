import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class EnumerationRepresentationClauses extends RuleType {
    static override readonly ruleName = 'Enumeration_Representation_Clauses';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "e");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "enumeration_representation_clauses.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): EnumerationRepresentationClauses {
        return new EnumerationRepresentationClauses(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
