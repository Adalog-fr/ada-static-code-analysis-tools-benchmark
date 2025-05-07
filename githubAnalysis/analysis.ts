import { join } from "https://deno.land/std/path/mod.ts";
import { repoInfo } from "./types.ts";

const MAIN_CWD = Deno.cwd();

const repositories: repoInfo[] = JSON.parse(Deno.readTextFileSync(join(MAIN_CWD, "allReposMetrics.json")));

// Sort repositories by the 'total_LoC' property in descending order
const sortedRepos = repositories.sort((a, b) => b.total_LoC - a.total_LoC);

// Filter repositories to find those with exactly 1 'number_of_gpr' and 1 'number_of_alire'
const filteredRepos = sortedRepos.filter(repo => repo.number_of_gpr >= 1 && repo.number_of_alire === 1);

// Select the first 5 repositories from the filtered list
const topFiveRepos = filteredRepos.slice(0, 5);

// Output the top 5 repositories
console.log(topFiveRepos.map(elt => {
    return {
        full_name: elt.full_name,
        html_url: elt.html_url,
        number_of_gpr: elt.number_of_gpr,
        number_of_alire: elt.number_of_alire,
        total_complexity: elt.total_complexity,
        greatest_complexity: elt.greatest_complexity.Complexity,
        total_LoC: elt.total_LoC,
        greatest_LoC: elt.greatest_LoC.Code,
        number_of_ada_source: elt.number_of_ada_source,
    }
}));

