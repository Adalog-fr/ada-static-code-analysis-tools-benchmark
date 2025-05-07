import { FileJob, LanguageSummary } from "../utils/scc-types.ts";

export type repoInfo = {
    /**
     * Full name of the project, in the form of: <owner>/<repository-name>
     * Obtained form GitHub API.
     */
    full_name: string;

    /**
     * URL of the project.
     * Obtained from GitHub API.
     */
    html_url: string;

    /**
     * Size of the project.
     * Obtained from GitHub API.
     */
    size: number;

    /**
     * Sum of the size of all Ada source
     */
    ada_size: number;

    /**
     * Number of `*.gpr` found in the project
     */
    number_of_gpr: number;

    /**
     * Number of `alire.toml` found in the project
     */
    number_of_alire: number;

    /**
     * Global complexity of the project (computed on Ada source code only).
     * Computed with SCC.
     * @see {@link https://github.com/boyter/scc}
     */
    total_complexity: number;

    /**
     * The greatest complexity of the project (computed on Ada source code only).
     * Computed with SCC.
     * @see {@link https://github.com/boyter/scc}
     */
    greatest_complexity: FileJob;

    /**
     * Contain all information like number of LoC, complexity, file by file (Ada source code only) of the project.
     * Computed with SCC.
     * @see {@link https://github.com/boyter/scc}
     */
    scc: LanguageSummary;

    /**
     * Total number of lines of code.
     * Computed with SCC.
     * @see {@link https://github.com/boyter/scc}
     */
    total_LoC: number;

    /**
     * The greatest number of lines of code inside a single file on the project.
     */
    greatest_LoC: FileJob;

    /**
     * Number of Ada source files
     */
    number_of_ada_source: number;
}
