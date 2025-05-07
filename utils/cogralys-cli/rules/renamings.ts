import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class Renamings extends RuleType {
    static override readonly ruleName = 'Renamings';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "e");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "renamings.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): Renamings {
        return new Renamings(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
