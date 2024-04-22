import { globToRegExp } from "https://deno.land/std/path/glob.ts";
import { parse, isGlob } from "https://deno.land/std/path/mod.ts";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";
import { collectOptionList } from "../utils.ts";
import fg from "npm:fast-glob@3.2.12";

interface WithStatementMap {
  [key: string]: string[];
}

function extractWithStatements(
  file: string,
  all: boolean,
  ignoreList: RegExp[]
): string[] {
  const content = Deno.readTextFileSync(file);
  const regex = all ? /^with (\S+);/gm : /^with\s+([^\s;\/\\]+);/gm;
  const matches = content.match(regex);

  if (matches) {
    const filteredMatches = matches
      .map((match) =>
        match
          .substring(5, match.length - 1)
          .replaceAll('"', "")
          .trim()
      )
      .filter((statement) => {
        return !ignoreList.some((ignoreRegex) => ignoreRegex.test(statement));
      });

    return filteredMatches;
  }

  return [];
}

function processFiles(
  pathGlobs: string[],
  all: boolean,
  ignoreList: RegExp[]
): WithStatementMap {
  const allFiles = fg
    .sync(pathGlobs, { onlyFiles: true, dot: true })
    .sort((a, b) => a.localeCompare(b));
  const withStatements: WithStatementMap = {};
  const extensions = [".gpr"];

  for (const file of allFiles) {
    const fileParsed = parse(file);
    if (!extensions.includes(fileParsed.ext)) {
      continue;
    }

    const statements = extractWithStatements(file, all, ignoreList);

    for (const statement of statements) {
      if (!withStatements[statement]) {
        withStatements[statement] = [];
      }

      withStatements[statement].push(file);
    }
  }

  return withStatements;
}

function generateJsonFile(statements: WithStatementMap, filename: string) {
  const orderedStatements = Object.keys(statements)
    .sort((a, b) => a.localeCompare(b))
    .reduce((obj: WithStatementMap, key: string) => {
      obj[key] = statements[key];
      return obj;
    }, {});
  const jsonData = JSON.stringify(orderedStatements, null, 2);
  Deno.writeTextFileSync(filename, jsonData);
  console.log(`JSON file '${filename}' generated.`);
}

export function initializeModule(program: Command): void {
  program
    .command("extract-dep-gpr <path...>")
    .description(
      "Extract all dependencies (with clause) in all .gpr found in directory and subdirectory recursively.\nBy default, it only extract global dependencies like `with aws`, but no path dependencies like `/path/to/gpr` or `./my_gpr.gpr`\nIt generate a JSON file with an object at the root. Each key correspond to a dependency (without duplicates), and each values are an array corresponding to all .gpr files that use this dependence."
    )
    .option(
      "--ignore-path <paths>",
      "Sets of path / glob (pattern) to ignore",
      collectOptionList,
      []
    )
    .option(
      "-o, --output <name>",
      "Filename of the JSON output",
      "with_statements.json"
    )
    .option("-a, --all", "Extract all dependencies, (global and path)")
    .option(
      "-i, --ignore-pattern <items>",
      "Name of the dependency to ignore.",
      collectOptionList,
      []
    )
    .option(
      "--ignore-pattern-file <name>",
      "Name of a file that contains ignore pattern (glob format). One pattern per line. Additive with ignore option"
    )
    .action(
      (
        paths: string[],
        options: {
          output: string;
          all: boolean;
          ignorePattern: string[];
          ignorePatternFile: string;
          ignorePath: string[];
        }
      ) => {
        const ignorePath = options.ignorePath.map((elt) =>
          elt.startsWith("!") ? elt : `!${elt}`
        );

        let pathListGlob = [...ignorePath];

        for (const path of paths) {
          if (isGlob(path)) {
            pathListGlob.push(path);
          } else {
            pathListGlob.push(parse(path).base + "/**/*.gpr");
            if (path === ".") {
              pathListGlob.push("*.gpr");
            }
          }
        }

        let patternInIgnoredFile: string[] = [];
        if (options.ignorePatternFile) {
          try {
            const data = Deno.readTextFileSync(options.ignorePatternFile);
            patternInIgnoredFile = data.split("\n");
          } catch (error) {
            console.error(`Error reading file: ${error}`);
          }
        }

        const ignoredPattern = [
          ...new Set(
            [...options.ignorePattern, ...patternInIgnoredFile].filter(
              (str) =>
                str.trim() !== "" &&
                !str.startsWith("#") &&
                !str.startsWith("--") &&
                !str.startsWith("//")
            )
          ),
        ].map((elt) => globToRegExp(elt));

        // Start processing from the current directory
        const withStatements = processFiles(
          pathListGlob,
          options.all,
          ignoredPattern
        );
        // Generate JSON file
        generateJsonFile(withStatements, options.output);
      }
    );
}
