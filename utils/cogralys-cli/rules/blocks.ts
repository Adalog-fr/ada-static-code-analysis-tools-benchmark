import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class Blocks extends RuleType {
    static override readonly ruleName = 'Blocks';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "e");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "blocks.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): Blocks {
        return new Blocks(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
