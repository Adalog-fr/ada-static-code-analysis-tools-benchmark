import { Octokit, App } from "https://esm.sh/octokit?dts";
import { load } from "https://deno.land/std/dotenv/mod.ts";
import { GhRepoSearchResultItem } from "./github-api.ts"
await load({export: true})

async function getAllIgnoredCrates() {
    // Octokit.js
    // https://github.com/octokit/core.js#readme
    const octokit = new Octokit({
        auth: Deno.env.get("GH_TOKEN")
    })

    let allRepositories : GhRepoSearchResultItem[] = [];

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

function removeDuplicates(arrayOfObjects: GhRepoSearchResultItem[]): GhRepoSearchResultItem[] {
    // Create a new Map to keep track of unique objects
    const uniqueObjects = new Map();

    // Iterate over each object in the array
    arrayOfObjects.forEach((object) => {
        // Use the object's 'id' as the key and the object itself as the value
        // Map will automatically handle duplication by overriding duplicates
        uniqueObjects.set(object.id, object);
    });

    // Convert the Map values back to an array and return it
    // This array will have only one object per unique 'id'
    return Array.from(uniqueObjects.values());
}

// MAIN

const allRepositories : GhRepoSearchResultItem[] = JSON.parse(Deno.readTextFileSync("./allAdaGithubRepos.json"))
const finalResult = removeDuplicates(allRepositories);
Deno.writeTextFileSync("./allAdaGithubRepos.json", JSON.stringify(finalResult, null, 2));
