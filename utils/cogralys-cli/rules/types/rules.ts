import { Session, RecordShape, Record } from "npm:neo4j-driver@5.27.0";
import { formatDuration } from "../../../utils.ts";
import { RuleAnalysisFoundElement, RuleAnalysisResult } from "../../cogralysCliTypes.ts";

export type responseRecords = Record<RecordShape, PropertyKey, RecordShape<PropertyKey, number>>[];

export type Query = string | { text: string, parameters?: any } | string[] | { text: string, parameters?: any }[];

export type ruleConstructorParams = {
    cypherQueriesPath: string,
    timing: boolean,
    resultFile: Deno.FsFile,
    unitList: string[]
}
export type ruleConstructorParamsExtended = ruleConstructorParams & {
    // deno-lint-ignore no-explicit-any
    [key: string]: any
};

export class UnknownRuleError extends Error {
    __proto__ = Error

    constructor(message: string) {
      super(message);
      Object.setPrototypeOf(this, UnknownRuleError.prototype);
    }

}

export abstract class RuleType<T extends typeof RuleType = typeof RuleType> {
    static readonly ruleName: string;
    readonly cypherQueriesPath: string;
    readonly timing: boolean;
    readonly resultFile: Deno.FsFile;
    readonly unitList: string[];
    queryDuration = 0;
    found: RuleAnalysisFoundElement[] = [];
    cypherLocationPropertyName: string;

    constructor(params: ruleConstructorParams, cypherLocationPropertyName: string) {
        this.cypherQueriesPath = params.cypherQueriesPath;
        this.timing = params.timing;
        this.resultFile = params.resultFile;
        this.unitList = params.unitList;
        this.cypherLocationPropertyName = cypherLocationPropertyName;
    }

    /**
     * Create an instance of the current, concrete class
     * @param params Parameters of this class constructor
     */
    static initialize : (params: ruleConstructorParamsExtended) => RuleType | void =
    (_: ruleConstructorParamsExtended) => {};

    /**
     *
     * @param records Records obtained from the queries related to this rule
     * @param file File where to save results
     * @see {executeRule}
     */
    protected saveResult(records: responseRecords, file: Deno.FsFile): void {
        records.forEach(elt => {
            const props = elt.get(this.cypherLocationPropertyName).properties;
            file.writeSync(new TextEncoder().encode(`${props.filename}:${props.line}:${props.column}: ${this.constructor.ruleName}\n`));
            this.found.push({
                filename: props.filename,
                line: props.line,
                column: props.column,
                ruleSpecific: {}
            })
        })
    }

    getReport(): RuleAnalysisResult {
        return {
            found: this.found,
            nbFound: this.found.length,
            analysisTime: this.queryDuration
        }
    }

    protected abstract getQuery(): Query;
    // deno-lint-ignore no-explicit-any
    protected getQueryParameters(): any {
        return {
            unitList: this.unitList
        };
    };

    /**
     * Performs the execution of queries associated with this rule using the provided Neo4j driver.
     * This method represents the default behavior for executing a single query and is suitable for the majority of cases.
     *
     * If a rule requires multiple queries or more complex interaction patterns, this method should be overridden
     * in the subclass to address those specific needs.
     *
     * @param driver The Neo4j driver used to execute database queries.
     * @returns A promise that resolves to the time taken to execute the query, measured in milliseconds.
     *
     * @remarks
     * - The execution time is only measured and logged if the `timing` property is set to `true`.
     * - The method internally marks the start and end of the query execution to measure performance.
     * - Results of the query are processed using the `saveResult` method.
     * @see {timing}
     * @see {saveResult}
     */
    async executeRule(session: Session): Promise<number> {

        performance.mark('queryStart');
        const { records } = await session.run(this.getQuery(), this.getQueryParameters());
        performance.mark('queryEnd');

        if (this.timing) {
            this.queryDuration = performance.measure('queryDuration', 'queryStart', 'queryEnd').duration;
            console.log(`${(this.constructor as T).ruleName} done in: ${formatDuration(this.queryDuration)}`);
        }

        this.saveResult(records, this.resultFile);

        return this.queryDuration;
    }
}
