import { join } from "jsr:@std/path@^0.225.1";
import { RuleType, responseRecords, ruleConstructorParamsExtended, type Query } from "./types/rules.ts";
import { Session } from "npm:neo4j-driver@5.23.0";
import { formatDuration } from "../../utils.ts";

export default class Variable_Usage extends RuleType {
    static override readonly ruleName = 'Variable_Usage';
    query: string[] = [];

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        super(cypherQueriesPath, timing, resultFile);
        this.query.push(Deno.readTextFileSync(join(cypherQueriesPath, "variable_usage1.cyp")));
        this.query.push(Deno.readTextFileSync(join(cypherQueriesPath, "variable_usage2.cyp")));
    }

    static override initialize(params: ruleConstructorParamsExtended): Variable_Usage {
        return new Variable_Usage(params.cypherQueriesPath, params.timing, params.resultFile);
    }

    getQuery(): Query {
      return this.query;
    }

    saveResult(records: responseRecords, file: Deno.FsFile) {
        records.forEach(elt => {
            const Location = elt.get("Location");
            const Variable = elt.get("Variable").properties;
            const isWrite: boolean = elt.get("isWrite");
            const isRead: boolean = elt.get("isRead");
            const origin: string = elt.get("origin");
            file.writeSync(new TextEncoder().encode(`${Location.filename}:${Location.line}:${Location.column}: Found: USAGE: (${origin}) ${Variable.content}, ${isWrite ? "" : "not "}written, ${isRead ? "" : "not "}read\n`));
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
