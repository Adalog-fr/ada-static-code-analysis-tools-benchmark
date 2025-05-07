import { join } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT } from "../config.ts";

const content = Deno.readTextFileSync(join (PROJECT_ROOT, "Adactl_benchmark.output"));
const regex = /^ERROR:.*"(.*\.gpr).*/gm;

let m;

while ((m = regex.exec(content)) !== null) {
    // This is necessary to avoid infinite loops with zero-width matches
    if (m.index === regex.lastIndex) {
        regex.lastIndex++;
    }

    // The result can be accessed through the `m`-variable.
    if (m.length) {
        console.log(m[1]);

    }
}

