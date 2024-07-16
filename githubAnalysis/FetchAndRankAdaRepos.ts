import { Octokit, App } from "https://esm.sh/octokit?dts";
import { load } from "https://deno.land/std/dotenv/mod.ts";
import { GhRepoSearchResultItem } from "./github-api.ts"
await load({export: true})
// Tokei type

export interface RootTokei {
    Ada: Ada
    Total: Total
  }

  export interface Ada {
    blanks: number
    children: Children
    code: number
    comments: number
    inaccurate: boolean
    reports: Report[]
  }

  export interface Children {}

  export interface Report {
    name: string
    stats: Stats
  }

  export interface Stats {
    blanks: number
    blobs: Blobs
    code: number
    comments: number
  }

  export interface Blobs {}

  export interface Total {
    blanks: number
    children: Children2
    code: number
    comments: number
    inaccurate: boolean
    reports: any[]
  }

  export interface Children2 {
    Ada: Ada2[]
  }

  export interface Ada2 {
    name: string
    stats: Stats2
  }

  export interface Stats2 {
    blanks: number
    blobs: Blobs2
    code: number
    comments: number
  }

  export interface Blobs2 {}

async function getAllIgnoredCrates() {
    // Octokit.js
    // https://github.com/octokit/core.js#readme
    const octokit = new Octokit({
        auth: Deno.env.get("GH_TOKEN")
    })

    let allRepositories : GhRepoSearchResultItem = [];

    let remainingRepo = true;
    let sizeFilter = "";
    let loopBreak = false;
    while (remainingRepo) {

        for (let page = 1; page <= 10; page++) {
            const result = await octokit.request('GET /search/repositories', {
                q: `language:Ada ${sizeFilter}`,
                sort: "size",
                order: "desc",
                per_page: 100,
                page: page
            });
            if (result.data.items.length) {
                allRepositories = allRepositories.concat(result.data.items);
            } else {
                loopBreak = true;
                break;
            }
        }

        Deno.writeTextFileSync("./allAdaGithubRepos.json", JSON.stringify(allRepositories, null, 2));

        if (loopBreak) {
            remainingRepo = false;
        } else {
            sizeFilter = `size:<${allRepositories[allRepositories.length - 1].size}`
        }
    }
}

const allRepositories : GhRepoSearchResultItem = JSON.parse(Deno.readTextFileSync("./allAdaGithubRepos.json"))


// Function to execute the `git-cloc-ada` command and extract Ada code lines
async function countAdaLines(repositoryUrl: string): Promise<number> {
    try {
        // Convert the URL into a file-system-friendly name by removing unwanted characters
        const safeName = repositoryUrl
            .replace(/https?:\/\//, '')  // Remove protocol (http, https)
            .replace(/[\/:]/g, '-')      // Replace slashes and colons with hyphens
            .replace(/\.[^/.]+$/, '');   // Remove file extension

        const process = new Deno.Command("git-cloc-ada", {
            args: [repositoryUrl, safeName.substring(safeName.length - 255)],
            stdout: "piped",
            stderr: "piped",
        });

        const { code, stderr, stdout } = await process.output();

        if (code === 0) {
            const outputJson: RootTokei = JSON.parse(new TextDecoder().decode(stdout));
            return outputJson.Ada.code;
        } else {
            const errorOutput = new TextDecoder().decode(stderr);
            console.error(repositoryUrl, errorOutput);
        }
    } catch (e) {
        console.error(repositoryUrl, e);
    }

    return -1;
}

// TODO: refactor to clone repo and compute metrics on it:
// - number of Ada lines of codes
// - number of alire.toml
// - number of .gpr

// Main function to orchestrate fetching and ranking by lines of Ada code
async function main() {
    const repositories = await fetchAdaRepositories();
    const countPromises = repositories.map(repo => countAdaLines(repo.clone_url));
    const linesOfCode = await Promise.all(countPromises);

    // Combine repository data with line counts
    const reposWithLineCounts = repositories.map((repo, index) => ({
        name: repo.full_name,
        lines_of_code: linesOfCode[index]
    }));

    // Sort repositories by lines of Ada code in descending order
    reposWithLineCounts.sort((a, b) => b.lines_of_code - a.lines_of_code);

    Deno.writeTextFileSync("largestGithubAdaRepo.json", JSON.stringify(reposWithLineCounts, null, 2));

    // Return the top 10 repositories by Ada code lines
    return reposWithLineCounts.slice(0, 10);
}

// Execute the main function and log the results
main().then(console.log).catch(console.error);
