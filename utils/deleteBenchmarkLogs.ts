// Import the required fast-glob package
import fg from "npm:fast-glob@3.3.2";

// Define prefixes for the files to be deleted
const PREFIXES = ["gnatcheck", "adactl"];

const patterns: string[] = [];
PREFIXES.forEach(prefix => {
  patterns.push(`./**/${prefix}-all-*.log`);
  patterns.push(`./**/${prefix}-*.log`);
  patterns.push(`./**/${prefix}-*.report`);
});

const paths = fg.sync(patterns, { onlyFiles: true });

paths.forEach(path => {
    Deno.removeSync(path);
    console.log(path, "DELETED");
});
