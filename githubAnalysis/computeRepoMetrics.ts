import { join, dirname, basename } from "https://deno.land/std/path/mod.ts";
import { existsSync } from "https://deno.land/std/fs/mod.ts";
import fg from "npm:fast-glob@3.2.12";
import { exec } from "../utils/utils.ts"
import { LanguageSummary, FileJob } from "../utils/scc-types.ts";
import { GhRepoSearchResultItem } from "./github-api.ts"
import * as log from "https://deno.land/std/log/mod.ts";
import { repoInfo } from "./types.ts";

const MAIN_CWD = Deno.cwd();

// Function to find the file with the greatest 'code' value
function findGreatestFileByCodeOrComplexity(languageSummary: LanguageSummary): { greatestLoC: FileJob, greatestComplexity: FileJob } | null {
    if (languageSummary.Files.length === 0) {
      return null; // Return null if there are no files
    }

    let greatestFile = languageSummary.Files[0];
    let greatestComplexity = languageSummary.Files[0];

    for (const file of languageSummary.Files) {
        if (file.Code > greatestFile.Code) {
          greatestFile = file;
        }
        if (file.Complexity > greatestComplexity.Complexity) {
            greatestComplexity = file;
        }
    }

    return { greatestLoC: greatestFile, greatestComplexity: greatestComplexity };
  }

function getRepoMetrics(repo: GhRepoSearchResultItem): repoInfo | undefined {
    const path = join("repos", repo.full_name.replace(/\//g, "__"));
    const resultPath = join(path, "scc_metrics.json");


    let metrics : LanguageSummary;

    if (!existsSync(path) || Deno.readDirSync(path).length === 0) {
        logger.error("The following project does not exists: " + repo.full_name);
        return;
    } else if (existsSync(resultPath)) {
        logger.info("Get previously computed metrics for the following project: " + repo.full_name);
        metrics = (JSON.parse(Deno.readTextFileSync(resultPath)) as repoInfo).scc;
        // return JSON.parse(Deno.readTextFileSync(resultPath));
    } else {
        Deno.chdir(path);

        const sccResult = exec(`scc`, ["--no-cocomo", "--by-file", "-f", "json", "--include-ext", "ads,adb,ada"], {
            cwd: Deno.cwd()
        });

        if (!sccResult.success) {
            logger.error(`Error during computation of complexity of "${repo.full_name}": ${sccResult.output}`);
            return;
        }

        metrics = JSON.parse(sccResult.output)[0];
        Deno.chdir(MAIN_CWD);
    }

    const number_of_gpr = fg.sync(`${path}/**/*.gpr`, { onlyFiles: true }).length;
    const number_of_alire = fg.sync(`${path}/**/alire.toml`, { onlyFiles: true }).length;

    if (!metrics) {
        logger.warning(`The following project does not contains any Ada source: ${repo.full_name}`);
        return;
    }

    const greatestValues = findGreatestFileByCodeOrComplexity(metrics);

    const result = {
        full_name: repo.full_name,
        html_url: repo.html_url,
        size: repo.size,
        ada_size: metrics.Bytes,
        number_of_gpr: number_of_gpr,
        number_of_alire: number_of_alire,
        total_complexity: metrics.Complexity,
        greatest_complexity: greatestValues!.greatestComplexity,
        scc: metrics,
        total_LoC: metrics.Code,
        greatest_LoC: greatestValues!.greatestLoC,
        number_of_ada_source: metrics.Count
    };

    Deno.writeTextFileSync(resultPath, JSON.stringify(result, null, 2));

    return result;
}

// MAIN

// Configure logs
log.setup({
handlers: {
    console: new log.handlers.ConsoleHandler("DEBUG"),

    file: new log.handlers.FileHandler("DEBUG", {
        filename: join(MAIN_CWD, "computeRepoMetrics.log"),
        formatter: "[{levelName}] {msg}",
        mode: "w"
    }),
},

loggers: {
    default: {
        level: "DEBUG",
        handlers: ["console", "file"],
    },

    tasks: {
        level: "ERROR",
        handlers: ["console"],
    },
},
});

const logger = log.getLogger();

const allGhRepos : GhRepoSearchResultItem[] = JSON.parse(Deno.readTextFileSync("allAdaGithubRepos.json"));

const allReposInfo: repoInfo[] = [];

let i = 1;
const maxProject = allGhRepos.length;
for (const repo of allGhRepos) {
    log.info(`[${i}/${maxProject}] ${repo.full_name}`)

    const ghRepoInfo = getRepoMetrics(repo);

    if (ghRepoInfo) {
        allReposInfo.push(ghRepoInfo);
    }

    i++;
}

Deno.writeTextFileSync(join(MAIN_CWD, "allReposMetrics.json"), JSON.stringify(allReposInfo, null, 2));
