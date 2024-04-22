import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { walkSync } from "https://deno.land/std/fs/mod.ts";
import { parse } from "https://deno.land/std/toml/mod.ts";

function readAlireTomlFiles(directory: string): string[] {
  const dependsOn = new Set<string>();

  for (const entry of walkSync(directory)) {
    if (entry.isFile && entry.name.match(/alire.*\.toml$/g)) {
      const content = Deno.readTextFileSync(entry.path);
      const parsedToml = parse(content);

      if ("depends-on" in parsedToml) {
        for (const dependencies of parsedToml["depends-on"])
        
        Object.keys(dependencies).forEach(elt => {
            dependsOn.add(elt)
        })        
      }
    }
  }

  return [...dependsOn].sort((a, b) => a.localeCompare(b));
}

export function initializeModule(program: Command): void {
  program
    .command("dependencies <path>")
    .description(
      "Extract a list of all dependencies of toml files found into path and subpath."
    )
    .action(
      (path: string) => {
        const dependsOn = readAlireTomlFiles(path);
        console.log(dependsOn);
      }
    );
}
