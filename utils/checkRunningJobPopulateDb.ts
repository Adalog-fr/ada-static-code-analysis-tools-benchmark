import { join } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT } from "../config.ts";

const inputFile = Deno.readTextFileSync(join(PROJECT_ROOT, "cogralysRunCommand-populate-neo4j.log")).split(/\r?\n/g);

const runningTasks: Record<string, string> = {};
const completeTasks: string[] = [];
const errorTasks: string[] = [];

for (const elt of inputFile) {
  const regexRunning = /Running '(\w+)' in '([^']+)/gm;
  const regexComplete = /End of '(\w+)'/gm;
  const regexError = /\[ERROR\] '(\w+)'/gm;
  const resultRunning = regexRunning.exec(elt);
  const resultComplete = regexComplete.exec(elt);
  const resultError = regexError.exec(elt);

  if (resultRunning) {
    runningTasks[resultRunning[1]] = resultRunning[2];
  } else if (resultComplete) {
    completeTasks.push(resultComplete[1]);
  } else if (resultError) {
    errorTasks.push(resultError[1]);
  }
}

const notCompletedTasks = Object.keys(runningTasks)
.filter((task) => !completeTasks.includes(task))
.filter((task) => !errorTasks.includes(task));

console.log("Running tasks not found in complete tasks:");
for (const task of notCompletedTasks) {
  console.log(task, runningTasks[task]);
}

console.log("Tasks errors:");
for (const task of errorTasks) {
  console.log(task, runningTasks[task]);
}
