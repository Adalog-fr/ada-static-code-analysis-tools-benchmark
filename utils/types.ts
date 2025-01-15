import { LanguageSummary } from "./scc-types.ts";

/////////////////
// Crates info //
/////////////////

export interface UnifiedCrateData {
    crates: { [key: string]: Crate };
    ignoredCrates: string[];
}

export interface Crate {
    path: string;
    alireProjects: CrateInfo[];
    ignore: boolean;
    ignoreReason?: string;
}

export interface CrateInfo {
    alireTomlPath: string;
    projects: GPRProject[];
}

export interface CratesInNeo4j {
    workDir: string;
    projects: GPRProject[];
    isNeo4jDbFilesFullyComplete: boolean;
    isAdaCtlComplete: boolean;
}

export interface GPRProject {
    gprPath: string;
    isNeo4jDbFilesComplete: boolean;
    isAdaCtlComplete: boolean;
    ignore: boolean;
    ignoreReason?: string;
}

export interface ExtendedGPRProject extends GPRProject {
    alireTomlPath: string;
    crateName: string;
}

///////////////////////////////
// Time (/usr/bin/time) type //
///////////////////////////////

export interface TimeData<T extends string | number | StandardDeviationResult> {
    user_time: T;
    system_time: T;
    cpu_percent: T;
    elapsed_time: T;
    average_shared_text_size: T;
    average_unshared_data_size: T;
    average_stack_size: T;
    average_total_size: T;
    maximum_resident_set_size: T;
    average_resident_set_size: T;
    major_pagefaults: T;
    minor_pagefaults: T;
    voluntary_context_switches: T;
    involuntary_context_switches: T;
    swaps: T;
    block_input_operations: T;
    block_output_operations: T;
    messages_sent: T;
    messages_received: T;
    signals_delivered: T;
    page_size: T;
    exit_status: T;
}

export type TimeDataKeyNumber = keyof TimeData<number>;

export interface TimeDataWithCommand extends TimeData<string> {
    command_being_timed: string;
}

/////////////////////
// Benchmark Types //
/////////////////////

export interface StandardDeviationResult {
    value: number;      // The standard deviation value
    percentage: number; // The standard deviation as percentage of mean
}

export type DigestTimeResult = {
    overheadParsing: number;
    overheadPopulating: number;
    /**
     * A value between 0 and 1 to define how much of the execution time is considered as overhead
     */
    overheadThreshold: number;
    /**
     * Correspond to {@link DigestTimeResult.overheadParsing} if the value is lower than: {@link DigestTimeResult.executionTime} * {@link DigestTimeResult.overheadThreshold}
     */
    overhead: number;
    /**
     * Total execution time (in encompass analysis time + overhead parsing)
     */
    executionTime: number;
    /**
     * Time to process rule analysis.
     * It is obtained like this: {@link DigestTimeResult.executionTime} - {@link DigestTimeResult.overhead}
     */
    analysisTime: number;
};

export type BenchResultByStep = {
    allRuns: TimeData<number>[];
    nbValidRuns: number;
    nbRuns: number;
    average: TimeData<number>;
    standardDeviation: TimeData<StandardDeviationResult>;
};

// Define the structure for Ada Control results
export type AdaControlResult = BenchResultByStep & {
    adtSize: number;
    issuedMessages: {
        maxCount: number;
        allCounts: number[];
    };
};

// Define the structure for GNATcheck results
export type GNATcheckResult = BenchResultByStep & {
    issuedMessages: {
        maxCount: number;
        allCounts: number[];
    };
};

// Add new interface for rule execution results
export interface RuleExecutionResult {
    allRuns: number[];
    nbValidRuns: number;
    nbRuns: number;
    standardDeviation: StandardDeviationResult;
    issuedMessages: {
        maxCount: number;
        allCounts: number[];
    };
    digestTime: DigestTimeResult;
}

// Define the structure for Cogralys results
export type CogralysResults = {
    overhead: {
        parsing: BenchResultByStep;
        populatingDB: BenchResultByStep;
    };
    run: BenchResultByStep;
    ruleResults: {
        [rule: string]: RuleExecutionResult;
    };
    digestTime: DigestTimeResult;
};

// Define the structure for benchmark results
export type BenchmarkResult = {
    adactl: {
        overhead: {
            parsing: AdaControlResult;
        };
        run: AdaControlResult;
        digestTime: DigestTimeResult;
    };
    gnatcheck_1cores: {
        overhead: {
            parsing: GNATcheckResult;
        };
        run: GNATcheckResult;
        digestTime: DigestTimeResult;
    };
    gnatcheck_32cores: {
        overhead: {
            parsing: GNATcheckResult;
        };
        run: GNATcheckResult;
        digestTime: DigestTimeResult;
    };
    cogralys: CogralysResults;
};

export interface BenchmarkResultDB {
    crateName: string;
    workDir: string;
    gprPath: string;
    benchmarkResults: BenchmarkResult;
    scc: Omit<LanguageSummary, 'Files'>;
}

// Computed results

export type GlobalResultTime = {
    overheadParsing: number,
    overheadPopulating: number,
    analysisTime: number,
    executionTime: number,
    timeData: TimeData<number>,
    overheadTimeData: TimeData<number>,
    nbFails: number,
    nbProjectFails: number,
    analysisTimeValues: number[],
    r2Value: number,
    mean: number,
    standardDeviation: { value: number, percentage: number },
};

export const toolKey = ["adactl", "cogralys", "gnatcheck_1cores", "gnatcheck_32cores"] as const;
export type ToolKeyType = typeof toolKey[number];
export type SummaryType = Record<ToolKeyType, GlobalResultTime>;
export type DigestTimeResultByProject = Record<ToolKeyType, DigestTimeResult>;
export type DetailedResultType = {
    crateName: string;
    workDir: string;
    gprPath: string;
    scc: {
        nbLoC: number;
    complexity: number;
    nbFiles: number;
    };
    results: DigestTimeResultByProject;
};

export type SummaryTableElement = Record<ToolKeyType | string, string | number>;
export type SummaryTable = {
    overheadParsing: SummaryTableElement,
    overheadPopulating: SummaryTableElement,
    "Relative Overhead (0 is better)": SummaryTableElement,
    analysisTime: SummaryTableElement,
    "Analysis Relative Speed (0 is better)": SummaryTableElement,
    "R²": SummaryTableElement,
    "mean": SummaryTableElement,
    "Standard Deviation value": SummaryTableElement,
    "Standard Deviation in %": SummaryTableElement,
    executionTime: SummaryTableElement,
    "Execution Relative Speed (0 is better)": SummaryTableElement,
    "Nb run fails": SummaryTableElement,
    "Nb project fails": SummaryTableElement
};

export const projectCategory = ["all", "small", "medium", "large"] as const;
export type ProjectCategoryType = typeof projectCategory[number];

export type RuleSummaryData = {
    [size in ProjectCategoryType]: {
        [rule: string]: {
            [tool in ToolKeyType | string]?: string | number;
        }
    }
};

export type ResultAggregation = {
    [size in ProjectCategoryType]: {
        table: SummaryTable,
        nbProjects: number,
        totalLoC: number,
        projects: DetailedResultType[],
    }
}

/**
 * The following type describe the output JSON generated in `generate-report`.
 */
export type ResultData = {
    global: ResultAggregation;
    rules: Record<string, ResultAggregation>;
    summary: {
        analysisTime: RuleSummaryData;
        overheadParsing: RuleSummaryData;
    }
};
