import { Query } from "https://deno.land/x/neo4j_driver_lite@5.18.0/core/types.ts";
import { RecordShape, Record, Driver } from "https://deno.land/x/neo4j_driver_lite@5.18.0/mod.ts";
import { formatDuration } from "../../../utils.ts";

export type responseRecords = Record<RecordShape, PropertyKey, RecordShape<PropertyKey, number>>[];

export type ruleConstructorParams = {
    cypherQueriesPath: string,
    timing: boolean,
    resultFile: Deno.FsFile
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
    queryDuration = 0;

    constructor(cypherQueriesPath: string, timing: boolean, resultFile: Deno.FsFile) {
        this.cypherQueriesPath = cypherQueriesPath;
        this.timing = timing;
        this.resultFile = resultFile;
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
    protected abstract saveResult(records: responseRecords, file: Deno.FsFile): void;

    protected abstract getQuery(): Query;
    // deno-lint-ignore no-explicit-any
    protected getQueryParameters(): any {
        return {};
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
    async executeRule(driver: Driver): Promise<number> {
        performance.mark('queryStart');
        const { records } = await driver.executeQuery(this.getQuery(), this.getQueryParameters());
        performance.mark('queryEnd');

        if (this.timing) {
            this.queryDuration = performance.measure('queryDuration', 'queryStart', 'queryEnd').duration;
            console.log(`${(this.constructor as T).ruleName} done in: ${formatDuration(this.queryDuration)}`);
        }

        this.saveResult(records, this.resultFile);

        return this.queryDuration;
    }
}
