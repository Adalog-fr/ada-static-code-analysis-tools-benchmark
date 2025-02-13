import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";
import { Session } from "npm:neo4j-driver@5.27.0";
import { formatDuration } from "../../utils.ts";

export default class Variable_Usage extends RuleType {
    static override readonly ruleName = 'Variable_Usage';
    query: string[] = [];

    constructor(params: ruleConstructorParamsExtended) {
        super(params, "Location");
        this.query.push(Deno.readTextFileSync(join(params.cypherQueriesPath, "variable_usage1.cyp")));
        this.query.push(Deno.readTextFileSync(join(params.cypherQueriesPath, "variable_usage2.cyp")));
    }

    static override initialize(params: ruleConstructorParamsExtended): Variable_Usage {
        return new Variable_Usage(params);
    }

    getQuery(): Query {
      return this.query;
    }

    override saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const location = elt.get("Location");
            const Variable = elt.get("Variable").properties;
            const isWrite: boolean = elt.get("isWrite");
            const isRead: boolean = elt.get("isRead");
            const origin: string = elt.get("origin");
            file.writeSync(new TextEncoder().encode(`${location.filename}:${location.line}:${location.column}: Found: ${Variable_Usage.ruleName}: (${origin}) ${Variable.content}, ${isWrite ? "" : "not "}written, ${isRead ? "" : "not "}read\n`));
            this.found.push({
                filename: location.filename,
                line: location.line,
                column: location.column,
                ruleSpecific: { variable: Variable.content, isRead, isWrite, origin }
            })
        })
    }

    override async executeRule(session: Session): Promise<number> {

        const result = [];
        performance.mark('queryStart');
        for(const query of this.query) {
            const { records } = await session.run(query, this.getQueryParameters());
            result.push(...records)
        }
        performance.mark('queryEnd');

        if (this.timing) {
            this.queryDuration = performance.measure('queryDuration', 'queryStart', 'queryEnd').duration;
            console.log(`${Variable_Usage.ruleName} done in: ${formatDuration(this.queryDuration)}`);
        }

        this.saveResult(result, this.resultFile);

        return this.queryDuration;
    }
}
