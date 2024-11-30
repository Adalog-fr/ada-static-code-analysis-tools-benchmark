import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";

export default class AbstractTypeDeclarations extends RuleType {
    static override readonly ruleName = 'Abstract_Type_Declarations';
    query: string;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query = Deno.readTextFileSync(join(cypherQueriesPath, "abstract_type_declarations.cyp"));
    }

    static override initialize(params: ruleConstructorParamsExtended): AbstractTypeDeclarations {
        return new AbstractTypeDeclarations(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const props = elt.get("n").properties;
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${AbstractTypeDeclarations.ruleName}\n`));
        })
    }
}
