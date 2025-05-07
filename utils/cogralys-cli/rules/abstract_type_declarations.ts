import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class AbstractTypeDeclarations extends RuleType {
    static override readonly ruleName = 'Abstract_Type_Declarations';
    query: string;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "n");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "abstract_type_declarations.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): AbstractTypeDeclarations {
        return new AbstractTypeDeclarations(params);
    }

    getQuery(): Query {
      return this.query;
    }
}
