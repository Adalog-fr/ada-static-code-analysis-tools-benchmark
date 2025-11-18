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

export const LANGUAGE_FEATURE_DESCRIPTION_MAP: Record<LanguageFeatureUsageKey, string> = {
    Attr_Access_All: "Number of uses of attribute 'Access on any object.",
    Attr_Address_All: "Number of uses of attribute 'Address on any object.",
    Attr_Unchecked_Access_All: "Number of uses of attribute 'Unchecked_Access on any object.",
    Decls_Operators_Overloaded: "Number of declarations of overloaded operators.",
    Derivations_Depth_Protected_GT0: "Maximum inheritance depth of protected types (number of derivation levels above the root).",
    Derivations_Depth_Tagged_GT0: "Maximum inheritance depth of tagged types (number of derivation levels above the root).",
    Derivations_Depth_Task_GT0: "Maximum inheritance depth of task types (number of derivation levels above the root).",
    Derivations_Depth_Untagged_GT0: "Maximum inheritance depth of untagged types (number of derivation levels above the root).",
    Derivations_Parents_GT0: "Maximum number of parents (interfaces or base types) for any single type.",
    Exceptions_Declared: "Number of exception declarations.",
    Generics_Decl_Local: "Number of locally declared generic units (generic packages or subprograms declared inside another unit).",
    Generics_Inst_Local: "Number of local instantiations of generic units (inside subprograms or nested scopes).",
    Generics_Inst_Private: "Number of private generic instantiations.",
    Generics_Inst_Public: "Number of public generic instantiations.",
    Generics_Units_All: "Number of generic units (generic packages and subprograms).",
    Handlers_Others_All: "Number of exception handlers using the others choice.",
    Handlers_Others_Null: "Number of others exception handlers whose body is null.",
    Inst_Unchecked_Conv_Addr_To_Access_Full: "Number of instantiations of Ada.Unchecked_Conversion converting System.Address to an access type (fully qualified).",
    Inst_Unchecked_Conv_Addr_To_Access_Short: "Number of instantiations of Unchecked_Conversion converting System.Address to an access type (short form).",
    Known_Exceptions_Access: "Number of statically known access-related exceptions (invalid pointer dereferences).",
    Known_Exceptions_Assignment: "Number of statically known exceptions related to assignments (e.g., constraint errors on assignment).",
    Known_Exceptions_Index: "Number of statically known index-related exceptions (out-of-range array indexing).",
    Known_Exceptions_Raise_Expression: "Number of statically known exceptions raised by raise expressions.",
    Known_Exceptions_Zero_Divide: "Number of statically known zero-divide exceptions.",
    Metrics_Functions_Called: "Number of function call sites.",
    Metrics_Objects_All: "Number of object declarations or usages (rough measure of data size).",
    Metrics_Procedures_Called: "Number of procedure call sites.",
    Metrics_Statements_All: "Total number of executable statements.",
    Metrics_Types_Used: "Number of type usages in the source (rough measure of type variety/complexity).",
    Named_Number_Declarations: "Number of named number declarations (integer constants used as named numbers).",
    Parameter_Aliasing_Certain: "Number of parameter aliasing situations that are certainly aliasing (definite aliases).",
    Parameter_Aliasing_Possible: "Number of parameter aliasing situations that are possibly aliasing (potential aliases).",
    Pragmas_All: "Total number of pragmas.",
    Pragmas_Nonstandard: "Number of nonstandard (implementation-defined) pragmas.",
    Protected_Objects_Declared: "Number of protected object declarations.",
    Representation_Clauses_All: "Number of representation clauses (record layout, alignment, etc.).",
    Statements_Abort: "Number of abort statements.",
    Statements_Accept: "Number of accept statements.",
    Statements_Conditional_Entry_Call: "Number of conditional entry call statements.",
    Statements_Delay_Relative: "Number of relative delay statements (delay until a duration has elapsed).",
    Statements_Delay_Until: "Number of delay until statements (delay until a specific time).",
    Statements_Entry_Call: "Number of simple entry call statements.",
    Statements_Raise_All: "Number of raise statements (all exceptions).",
    Statements_Raise_Standard: "Number of raise statements raising predefined (standard) exceptions.",
    Statements_Requeue: "Number of requeue statements.",
    Statements_Selective_Accept: "Number of selective accept statements (select).",
    Statements_Terminate_Alternative: "Number of terminate alternatives in selective accept statements.",
    Statements_Timed_Entry_Call: "Number of timed entry call statements.",
    Tasks_Declared: "Number of task declarations.",
    Tasks_Terminating: "Number of tasks that are known to terminate (per static analysis).",
    Type_Usage_Pos_On_Enum: "Number of uses of attribute 'Pos on enumeration types.",
    Types_Abstract: "Number of abstract type declarations.",
    Types_Access_Subprogram: "Number of access-to-subprogram type declarations.",
    Types_Controlled: "Number of controlled type declarations.",
    Types_Derived: "Number of derived type declarations.",
    Types_Tagged_With_Primitives: "Number of tagged types that have at least one visible primitive operation.",
    Types_With_Discriminants: "Number of type declarations with discriminants.",
};

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
    languageFeatureUsageProjects: LanguageFeatureUsage,
    languageFeatureUsageSum: LanguageFeatureUsage,
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

export type SimilarProjectsTargetKind = "project" | "gpr" | "loc";

export interface SimilarProjectsTarget {
    type: SimilarProjectsTargetKind;
    value: string | number;
    benchmark?: BenchmarkResultDB | null;
}

export interface SimilarProjectsOptionsExport {
    metric: "loc" | "files";
    tolerance: number;
    tool: ToolKeyType;
    triggerNumber: number;
}

export interface SimilarProjectsTargetInfo {
    targetLoc: number;
    targetFiles: number;
    isSmall: boolean;
}

export interface SimilarProjectsRawData {
    options: SimilarProjectsOptionsExport;
    target: SimilarProjectsTarget;
    targetInfo: SimilarProjectsTargetInfo;
    similarProjects: BenchmarkResultDB[];
}
