// Constants for token types
export enum TokenType {
    TString = 1,
    TSlcomment,
    TMlcomment,
    TComplexity,
    TUndefined
}

// Interface for Quote
export interface Quote {
    Start: string;
    End: string;
    IgnoreEscape: boolean;  // To enable turning off the \ check for C# @"\" string examples
    DocString: boolean;     // To enable docstring check for Python
}

// Interface for Language
export interface Language {
    LineComment: string[];
    ComplexityChecks: string[];
    Extensions: string[];
    ExtensionFile: boolean;
    MultiLine: string[][];
    Quotes: Quote[];
    NestedMultiLine: boolean;
    Keywords: string[];
    FileNames: string[];
    SheBangs: string[];
}

// Interface for LanguageFeature
export interface LanguageFeature {
    Complexity: Trie;
    MultiLineComments: Trie;
    SingleLineComments: Trie;
    Strings: Trie;
    Tokens: Trie;
    Nested: boolean;
    ComplexityCheckMask: number;
    SingleLineCommentMask: number;
    MultiLineCommentMask: number;
    StringCheckMask: number;
    ProcessMask: number;
    Keywords: string[];
    Quotes: Quote[];
}

// Interface for FileJobCallback
export interface FileJobCallback {
    processLine(job: FileJob, currentLine: number, lineType: number): boolean;
}

// Interface for FileJob
export interface FileJob {
    Language: string;
    PossibleLanguages: string[];
    Filename: string;
    Extension: string;
    Location: string;
    Symlocation: string;
    Content: number[];
    Bytes: number;
    Lines: number;
    Code: number;
    Comment: number;
    Blank: number;
    Complexity: number;
    WeightedComplexity: number;
    Hash: any;  // Specific hash type can be defined based on usage
    Callback: FileJobCallback;
    Binary: boolean;
    Minified: boolean;
    Generated: boolean;
    EndPoint: number;
    Uloc: number;
    LineLength: number[];
    /**
     * Not SCC data, but used to store the use of unit (`with clause`)
     */
    withUnit?: string[];
}

// Interface for LanguageSummary
export interface LanguageSummary {
    Name: string;
    Bytes: number;
    CodeBytes: number;
    Lines: number;
    Code: number;
    Comment: number;
    Blank: number;
    Complexity: number;
    /**
     * Number of files
     */
    Count: number;
    WeightedComplexity: number;
    Files: FileJob[];
    LineLength: number[];
    unitUsage?: Record<string, number>;
}

// Interface for OpenClose
export interface OpenClose {
    Open: number[];
    Close: number[];
}

// Interface for CheckDuplicates
export class CheckDuplicates {
    private hashes: Map<number, number[][]> = new Map();
    private mux: any;  // Mutex type can be added based on the concurrency library used

    add(key: number, hash: number[]) {
        let hashes = this.hashes.get(key);
        if (hashes) {
            hashes.push(hash);
            this.hashes.set(key, hashes);
        } else {
            this.hashes.set(key, [hash]);
        }
    }

    check(key: number, hash: number[]): boolean {
        let hashes = this.hashes.get(key);
        return !!hashes && hashes.some(h => Buffer.compare(h, hash) === 0);
    }
}

// Interface for Trie
export class Trie {
    Type: TokenType;
    Close: number[];
    Table: Trie[];

    constructor() {
        this.Type = TokenType.TUndefined;
        this.Close = [];
        this.Table = Array(256).fill(null);
    }

    insert(tokenType: TokenType, token: number[]) {
        let node: Trie = this;
        for (let c of token) {
            if (!node.Table[c]) {
                node.Table[c] = new Trie();
            }
            node = node.Table[c];
        }
        node.Type = tokenType;
    }

    insertClose(tokenType: TokenType, openToken: number[], closeToken: number[]) {
        let node: Trie = this;
        for (let c of openToken) {
            if (!node.Table[c]) {
                node.Table[c] = new Trie();
            }
            node = node.Table[c];
        }
        node.Type = tokenType;
        node.Close = closeToken;
    }

    match(token: number[]): [number, number, number[]] {
        let node: Trie = this;
        let depth: number = 0;
        let prevClosedNode: Trie | undefined;
        let prevClosedDepth: number = 0;
        for (let c of token) {
            if (!node.Table[c]) {
                break;
            }
            node = node.Table[c];
            depth++;
            if (node.Close.length > 0) {
                prevClosedNode = node;
                prevClosedDepth = depth;
            }
        }
        if (node.Close.length === 0 && prevClosedNode) {
            return [prevClosedNode.Type, prevClosedDepth, prevClosedNode.Close];
        }
        return [node.Type, depth, node.Close];
    }
}
