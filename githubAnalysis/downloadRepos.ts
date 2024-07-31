import { GhRepoSearchResultItem } from "./github-api.ts";
import { exec } from "../utils/utils.ts"
import { join, dirname, basename } from "https://deno.land/std/path/mod.ts";
import { existsSync } from "https://deno.land/std/fs/mod.ts";


const allRepositories : GhRepoSearchResultItem[] = JSON.parse(Deno.readTextFileSync("./allAdaGithubRepos.json"))

for (const repo of allRepositories) {
    const path = `repos/${repo.full_name.replace(/\//g, "__")}`;

    if (existsSync(path)) {
        console.log(`Skip: `, repo.full_name);
        continue;
    }

    console.log("Cloning: ", repo.full_name);
    const result = exec(`git`, ["clone", "--depth=1", "--filter=tree:0", "--single-branch", "--branch",
        repo.default_branch, repo.clone_url, path]);
    if (!result.success) {
        console.error("Error during clone: ", result.output);
    }
}
