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

export interface extendedGPRProject extends GPRProject {
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

// Define the structure for Ada Control results
export type AdaControlResult = {
    adtSize: number;
    allRuns: TimeData<number>[];
    nbValidRuns: number;
    nbRuns: number;
    average: TimeData<number>;
    standardDeviation: TimeData<StandardDeviationResult>;
};

// Define the structure for GNATcheck results
export type GNATcheckResult = {
    allRuns: TimeData<number>[];
    nbValidRuns: number;
    nbRuns: number;
    average: TimeData<number>;
    standardDeviation: TimeData<StandardDeviationResult>;
};

// Add new interface for rule execution results
export interface RuleExecutionResult {
    allRuns: number[];
    nbValidRuns: number;
    nbRuns: number;
    average: number;
    standardDeviation: StandardDeviationResult;
}

// Define the structure for Cogralys results
export type CogralysResults = {
    overhead: {
        parsing: {
            allRuns: TimeData<number>[];
            nbValidRuns: number;
            nbRuns: number;
            average: TimeData<number>;
            standardDeviation: TimeData<StandardDeviationResult>;
        };
        populatingDB: {
            allRuns: TimeData<number>[];
            nbValidRuns: number;
            nbRuns: number;
            average: TimeData<number>;
            standardDeviation: TimeData<StandardDeviationResult>;
        };
    };
    run: {
        allRuns: TimeData<number>[];
        nbValidRuns: number;
        nbRuns: number;
        average: TimeData<number>;
        standardDeviation: TimeData<StandardDeviationResult>;
    };
    ruleResults: {
        [rule: string]: RuleExecutionResult;
    };
};

// Define the structure for benchmark results
export type BenchmarkResult = {
    adactl: {
        overhead: {
            parsing: AdaControlResult;
        };
        run: AdaControlResult;
    };
    gnatcheck_1cores: {
        overhead: {
            parsing: GNATcheckResult;
        };
        run: GNATcheckResult;
    };
    gnatcheck_32cores: {
        overhead: {
            parsing: GNATcheckResult;
        };
        run: GNATcheckResult;
    };
    cogralys: CogralysResults;
};

export interface benchmarkResultDB {
    crateName: string;
    workDir: string;
    gprPath: string;
    benchmarkResults: BenchmarkResult;
    scc: Omit<LanguageSummary, 'Files'>;
}

// Computed results

export type globalResultTime = { overheadParsing: number, overheadPopulating: number, executionTime: number, timeData: TimeData<number>, overheadTimeData: TimeData<number> };

export type toolKey = "adactl" | "cogralys" | "gnatcheck_1cores" | "gnatcheck_32cores";
export type summaryType = Record<toolKey, globalResultTime>;
export type detailedResultType = {
    crateName: string;
    workDir: string;
    gprPath: string;
    scc: {
        loc: number;
        complexity: number;
        nbFiles: number;
    };
    results: summaryType;
 };
