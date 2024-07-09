const content = Deno.readTextFileSync("/workspaces/bench-source/Adactl_benchmark.output");
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

