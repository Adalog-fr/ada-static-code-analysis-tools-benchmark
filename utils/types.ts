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

export type IssuedMessages = {
    issuedMessages: {
        maxCount: number;
        allCounts: number[];
    };
}

export type BenchResultByStepWithIssuedMessages = BenchResultByStep & IssuedMessages;

// Define the structure for Ada Control results
export type AdaControlResult = BenchResultByStepWithIssuedMessages & {
    adtSize: number;
};

// Define the structure for GNATcheck results
export type GNATcheckResult = BenchResultByStepWithIssuedMessages;

// Add new interface for rule execution results
export interface RuleExecutionResult extends IssuedMessages {
    allRuns: number[];
    nbValidRuns: number;
    nbRuns: number;
    standardDeviation: StandardDeviationResult;
    digestTime: DigestTimeResult;
};

// Define the structure for Cogralys results
export type CogralysResults = {
    overhead: {
        parsing: BenchResultByStep;
        populatingDB: BenchResultByStep;
    };
    run: BenchResultByStepWithIssuedMessages;
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

export const LANGUAGE_FEATURE_USAGE_KEYS = [
    "Attr_Access_All",
    "Attr_Address_All",
    "Attr_Unchecked_Access_All",
    "Decls_Operators_Overloaded",
    "Derivations_Depth_Protected_GT0",
    "Derivations_Depth_Tagged_GT0",
    "Derivations_Depth_Task_GT0",
    "Derivations_Depth_Untagged_GT0",
    "Derivations_Parents_GT0",
    "Exceptions_Declared",
    "Generics_Decl_Local",
    "Generics_Inst_Local",
    "Generics_Inst_Private",
    "Generics_Inst_Public",
    "Generics_Units_All",
    "Handlers_Others_All",
    "Handlers_Others_Null",
    "Inst_Unchecked_Conv_Addr_To_Access_Full",
    "Inst_Unchecked_Conv_Addr_To_Access_Short",
    "Known_Exceptions_Access",
    "Known_Exceptions_Assignment",
    "Known_Exceptions_Index",
    "Known_Exceptions_Raise_Expression",
    "Known_Exceptions_Zero_Divide",
    "Metrics_Functions_Called",
    "Metrics_Objects_All",
    "Metrics_Procedures_Called",
    "Metrics_Statements_All",
    "Metrics_Types_Used",
    "Named_Number_Declarations",
    "Parameter_Aliasing_Certain",
    "Parameter_Aliasing_Possible",
    "Pragmas_All",
    "Pragmas_Nonstandard",
    "Protected_Objects_Declared",
    "Representation_Clauses_All",
    "Statements_Abort",
    "Statements_Accept",
    "Statements_Conditional_Entry_Call",
    "Statements_Delay_Relative",
    "Statements_Delay_Until",
    "Statements_Entry_Call",
    "Statements_Raise_All",
    "Statements_Raise_Standard",
    "Statements_Requeue",
    "Statements_Selective_Accept",
    "Statements_Terminate_Alternative",
    "Statements_Timed_Entry_Call",
    "Tasks_Declared",
    "Tasks_Terminating",
    "Type_Usage_Pos_On_Enum",
    "Types_Abstract",
    "Types_Access_Subprogram",
    "Types_Controlled",
    "Types_Derived",
    "Types_Tagged_With_Primitives",
    "Types_With_Discriminants",
] as const;

export type LanguageFeatureUsageKey = typeof LANGUAGE_FEATURE_USAGE_KEYS[number];
export type LanguageFeatureUsage = Record<LanguageFeatureUsageKey, number>;

export interface BenchmarkResultDB {
    crateName: string;
    workDir: string;
    gprPath: string;
    benchmarkResults: BenchmarkResult;
    scc: Omit<LanguageSummary, 'Files'>;
    languageFeatureUsage?: LanguageFeatureUsage;
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
    issuedMessage: number
};

export const toolKey = ["adactl", "cogralys", "gnatcheck_1cores", "gnatcheck_32cores"] as const;
export type ToolKeyType = typeof toolKey[number];
export type SummaryType = Record<ToolKeyType, GlobalResultTime>;
export type DigestTimeResultByProject = Record<ToolKeyType, DigestTimeResult & IssuedMessages>;
export type DetailedResultType = {
    crateName: string;
    workDir: string;
    gprPath: string;
    scc: {
        nbLoC: number;
        complexity: number;
        nbFiles: number;
    };
    languageFeatureUsage?: LanguageFeatureUsage;
    results: DigestTimeResultByProject;
};

export type SummaryTableElement = Record<ToolKeyType | string, string | number>;
export const SUMMARY_TABLE_KEYS = [
    'overheadParsing',
    'overheadPopulating',
    'Relative Overhead (0 is better)',
    'analysisTime',
    'Analysis Relative Speed (0 is better)',
    'executionTime',
    'Execution Relative Speed (0 is better)',
    'Nb run fails',
    'Nb project fails',
    'Issued Messages'
] as const;

export type SummaryTableKeys = typeof SUMMARY_TABLE_KEYS[number];
export type SummaryTable = Record<SummaryTableKeys, SummaryTableElement>;

export const projectCategory = ["all", "small", "medium", "large"] as const;
export type ProjectCategoryType = typeof projectCategory[number];

export type RuleSummaryData = {
    [size in ProjectCategoryType]: {
        [rule: string]: {
            [tool in ToolKeyType | string]?: string | number;
        }
    }
};

export type ResultAggregationByProjectCategory = {
    table: SummaryTable,
    nbProjects: number,
    totalLoC: number,
    projects: DetailedResultType[],
};

export type ResultAggregation = {
    [size in ProjectCategoryType]: ResultAggregationByProjectCategory
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
        issuedMessage: RuleSummaryData;
    }
};
