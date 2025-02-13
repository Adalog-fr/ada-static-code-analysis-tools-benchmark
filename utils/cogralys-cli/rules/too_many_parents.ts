import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class TooManyParents extends RuleType {
    static override readonly ruleName = 'Too_Many_Parents';
    query: string;
    minNbParents: number;

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "typeDecl");
        this.query = Deno.readTextFileSync(join(params.cypherQueriesPath, "too_many_parents.cyp"));
        this.minNbParents = params.minNbParents;
    }

    static override initialize(params: ruleConstructorParamsExtended): TooManyParents {
        return new TooManyParents(params);
    }

    getQuery(): Query {
      return this.query;
    }

    override getQueryParameters(): any {
        return {
            minNbParents: this.minNbParents,
            unitList: this.unitList
        };
    };

    override saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get(this.cypherLocationPropertyName).properties;
            const nbParents = elt.get("nbParents");
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${TooManyParents.ruleName} ${nbParents} parent(s)\n`));
            this.found.push({
                filename: props.filename,
                line: props.line,
                column: props.column,
                ruleSpecific: { nbParents }
            })
        })
    }
}
