import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class TooManyParents extends RuleType {
    static override readonly ruleName = 'Too_Many_Parents';
    query: string;
    minNbParents: number;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile, minNbParents: number) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "too_many_parents.cyp"));
        this.minNbParents = minNbParents;
    }

    static override initialize(params: ruleConstructorParamsExtended): TooManyParents {
        return new TooManyParents(params.cypherQueriesPath, params.timing, params.resultFile, params.minNbParents);
    }

    getQuery(): Query {
      return this.query;
    }

    override getQueryParameters(): any {
        return {
            minNbParents: this.minNbParents
        };
    };

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("typeDecl").properties;
            const nbParents = elt.get("nbParents");
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${TooManyParents.ruleName} ${nbParents} parent(s)\n`));
        })
    }
}
