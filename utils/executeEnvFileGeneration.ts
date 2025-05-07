// This script is called by `cogralys-bench-util generate-env`
import { join } from "jsr:@std/path@^0.225.1";
import { exec } from "./utils.ts";

try {
    const result = exec("alr", ["printenv"]);
    if (!result.success) {
        console.error(result.output);
        Deno.exit(1);
    }
    const lines = result.output.split("\n");

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

    Deno.writeTextFileSync(join(Deno.cwd(), ".env"), envContent);
} catch (error) {
    console.error("Error:", error);
    Deno.exit(1);
}
