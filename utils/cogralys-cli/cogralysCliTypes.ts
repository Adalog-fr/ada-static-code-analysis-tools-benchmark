import * as allRules from "./allRules.ts";

export type AllRulesName = keyof typeof allRules;
export type AllRulesNameLC = Lowercase<AllRulesName>;
export const ruleNames: AllRulesName[] = Object.keys(allRules) as AllRulesName[];

export type RuleAnalysisFoundElement = {
    filename: string,
    line: number,
    column: number,
    ruleSpecific: Record<string, any>
};

export type RuleAnalysisResult = {
    found: RuleAnalysisFoundElement[],
    nbFound: number,
    analysisTime: number
};

export type CogralysOutputType = {
    result: {
        [key in AllRulesNameLC]: RuleAnalysisResult
    },
    totalAnalysisTime: number,
    totalNbFound: number
};
