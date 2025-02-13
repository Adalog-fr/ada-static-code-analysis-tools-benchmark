import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class Slices extends RuleType {
    static override readonly ruleName = 'Slices';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "e");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "slices.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): Slices {
        return new Slices(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
