import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import { initializeModule as initializeCommandModule } from "./runCommandFactory.ts";
import { join } from "https://deno.land/std/path/mod.ts";
import { PROJECT_ROOT } from "../../config.ts";

export function initializeModule(program: Command): void {
    let progress : ProgressBar;
    let completed = 0;
    initializeCommandModule(program, {
        description: "Generate a '.env' file for every `alire.toml` files fond in `alireTomlPath`.",
        commandName: "generate-env",
        command: [
            "deno",
            [
                "run",
                "--config",
                join(PROJECT_ROOT, "deno.jsonc"),
                "--allow-read",
                "--allow-write",
                "--allow-env",
                "--allow-run",
                "--allow-ffi",
                "--unstable-ffi",
                join(PROJECT_ROOT, "utils/executeEnvFileGeneration.ts"),
            ]
        ],
        concurrency: 8,
        beforeRun: (params) => {
            const title = "generating:";
            const total = params.alireTomlPath.length;
            progress = new ProgressBar({
                title,
                total,
            });
        },
        postTaskCb: (task) => {
            progress.render(completed++);

            if (!task.result?.success) {
                console.error(task.result.output);

                return;
            }
        }
    });
}
