import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import ProgressBar from "https://deno.land/x/progress@v1.3.8/mod.ts";
import { initializeModule as initializeCommandModule } from "./runCommandFactory.ts";
import { join } from "https://deno.land/std/path/mod.ts";
export function initializeModule(program: Command): void {
    let progress : ProgressBar;
    let completed = 0;
    initializeCommandModule(program, {
        description: "Generate a '.env' file for every `alire.toml` files fond in `alireTomlPath`.",
        commandName: "generate-env",
        command: ["alr", ["printenv"]],
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
                return;
            }

            const lines = task.result.output.split("\n");

            const envVariables: string[] = [];
            for (const line of lines) {
                const match = line.match(/export\s+([A-Z_]+)=(.+)/);
                if (match) {
                    const key = match[1];
                    const value = match[2];
                    envVariables.push(`${key}=${value}`);
                }
            }

            const envContent = envVariables.join("\n");

            Deno.writeTextFileSync(join(task.task.data.path, ".env"), envContent);
        }
    });
}
