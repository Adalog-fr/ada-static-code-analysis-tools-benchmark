import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class Constructors extends RuleType {
    static override readonly ruleName = 'Constructors';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "function");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "constructors.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): Constructors {
        return new Constructors(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
