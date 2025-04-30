#!/usr/bin/env deno  Deno.Command --allow-run --allow-write --allow-read --unstable

import { existsSync, copySync } from "https://deno.land/std/fs/mod.ts";
import { join } from "https://deno.land/std/path/mod.ts";
import { Command } from "https://deno.land/x/cmd@v1.2.0/mod.ts";

const program = new Command();

program.version("1.0.0");

program
  .command("install <pkgName>", undefined, { isDefault: false })
  .option("-b, --no-build", "Does this package not have to be built?")
  .option("-p, --prefix", "Install prefix")
  .option(
    "-e, --extension <item>",
    "Extension of the archive. Should contains the dot of the extension, like `-e .tar.gz` or `-e .zip`",
    ".tar.gz"
  )
  .action((pkgName, options) => {
    if (options.build) {
      installPackage(pkgName, options.prefix, options.extension);
    } else {
      installPackageNoBuild(pkgName, options.prefix, options.extension);
    }
  });

//////////////////////
//                  //
// Global variables //
//                  //
//////////////////////

const prefix = Deno.env.get("prefix") || "/usr/gnat";

function toArray<X>(xs: Iterable<X>): X[] {
  return [...xs];
}

function exec(
  command: string | URL,
  args?: string[] | undefined,
  options?: Deno.CommandOptions | undefined
) {
  console.log(`Execute "${Deno.cwd()}> ${command} ${args?.join(" ") || ""}"`);

  const process = new Deno.Command(command, {
    args,
    stdout: "piped",
    stderr: "piped",
    ...options,
  });

  const { code, stderr, stdout } = process.outputSync();

  if (code !== 0) {
    const errorOutput = new TextDecoder().decode(stderr);
    console.error(
      `Command "${Deno.cwd()}> ${command} ${args?.join(
        " "
      )}" failed: ${errorOutput}`
    );
    Deno.exit(1);
  }

  return new TextDecoder().decode(stdout);
}

const NBCORE = parseInt(exec("nproc")) + 1;

function unzipStrip(
  archive: string,
  destdir?: string,
  ...extraArgs: string[]
): void {
  const tmpdir = Deno.makeTempDirSync();

  exec("unzip", ["-qd", tmpdir, archive, ...extraArgs]);

  const files = toArray(Deno.readDirSync(tmpdir));
  let name: string;
  let fileList: string[];

  if (files.length === 1 && files[0].isDirectory) {
    name = files[0].name;
    fileList = toArray(Deno.readDirSync(tmpdir + "/" + name)).map(
      (entry) => entry.name
    );
  } else {
    name = archive.split("/").pop()?.split(".").slice(0, -1).join(".") || "";
    fileList = files.map((file) => file.name);
  }

  if (!destdir) {
    destdir = `./${name}`;
  }

  if (!existsSync(destdir)) {
    Deno.mkdirSync(destdir);
  }

  for (const file of fileList) {
    if (existsSync(`${tmpdir}/${name}/${file}`)) {
      copySync(`${tmpdir}/${name}/${file}`, join(destdir, file), {
        overwrite: true,
      });
    } else if (existsSync(`${tmpdir}/${file}`)) {
      copySync(`${tmpdir}/${file}`, join(destdir, file), {
        overwrite: true,
      });
    }
  }

  Deno.removeSync(tmpdir, { recursive: true });
}

////////////////////////////////
//                            //
// Install all dependencies   //
//                            //
////////////////////////////////

function installPackage(
  pkgName: string,
  targetDir = prefix,
  extension = ".tar.gz"
): void {
  console.log(`# Install package ${pkgName}`);

  const archivePath = `/tmp/${pkgName}${extension}`;

  if (existsSync(archivePath)) {
    Deno.chdir("/tmp");
    Deno.mkdirSync(pkgName, { recursive: true });

    switch (extension) {
      case ".tar.gz":
        exec("tar", [
          "-xf",
          archivePath,
          "-C",
          `${pkgName}/`,
          "--strip-components",
          "1",
        ]);
        break;
      case ".zip":
        unzipStrip(archivePath, `${pkgName}/`);
        break;
      default:
        console.error(`Unknown archive extension: ${extension}`);
        Deno.exit(1);
    }

    if (pkgName === "Adacontrol") {
      unzipStrip("/tmp/Comps.zip", "Comps/");
      unzipStrip("/tmp/Asiscomps.zip", "Asiscomps/");
    }

    Deno.chdir(pkgName);

    switch (pkgName) {
      case "florist":
      case "gtkada":
        exec("./configure", [`--prefix=${targetDir}`]);
        exec("make");
        exec("make", ["install"]);
        break;
      case "asis":
        if (existsSync("patch.sh")) {
          exec("sh", ["patch.sh"]);
        }
        exec("make", [`all`, `prefix=${targetDir}`]);
        exec("make", ["install"]);
        break;
      case "gnatcoll-core":
        exec("make", [`prefix=${targetDir}`, "setup"]);
        exec("make", [`prefix=${targetDir}`, "build"]);
        exec("make", ["install"]);
        break;
      case "gnatcoll-bindings":
        // GMP
        Deno.chdir("gmp");
        exec("./setup.py", ["build"]);
        exec("./setup.py", ["install", `--prefix=${targetDir}`]);
        Deno.chdir("..");

        // iconv
        Deno.chdir("iconv");
        exec("./setup.py", ["build"]);
        exec("./setup.py", ["install", `--prefix=${targetDir}`]);
        Deno.chdir("..");
        break;
      case "adasubst":
        exec("gprbuild", ["-p", `-j${NBCORE}`, "-P", "build.gpr"]);
        Deno.copyFileSync("adasubst", `${join(targetDir, "/bin/adasubst")}`);
        break;
      case "adadep":
        Deno.chdir("src");
        for (const entry of Deno.readDirSync(".")) {
          if (entry.isFile && entry.name.endsWith(".adb")) {
            const tempFile = Deno.makeTempFileSync();
            const fileContent = Deno.readTextFileSync(entry.name);
            const updatedContent = fileContent.replace(
              /pragma No_Return.*/g,
              ""
            );

            Deno.writeTextFileSync(tempFile, updatedContent);
            Deno.renameSync(tempFile, entry.name);
          }
        }
        exec("gprbuild", ["-p", `-j${NBCORE}`, "-P", "build.gpr"]);
        Deno.copyFileSync("adadep", `${join(targetDir, "/bin/adadep")}`);
        Deno.chdir("..");
        break;
      case "Comps":
        exec("gprbuild", ["-p", `-j${NBCORE}`, "-P", "adalog_comps.gpr"]);
        exec("gprinstall", ["-p", "-P", "adalog_comps.gpr"]);
        break;
      case "Asiscomps":
        exec("gprbuild", ["-p", `-j${NBCORE}`, "-P", "adalog_asiscomps.gpr"], {
          env: {
            GPR_PROJECT_PATH: ".:/tmp/Comps:",
          },
        });
        exec("gprinstall", ["-p", "-P", "adalog_asiscomps.gpr"]);
        break;
      case "Adacontrol":
        console.log(exec("gnatls", ["-v"]));
        exec("gprbuild", ["-p", `-j${NBCORE}`, "-P", "adactl.gpr"], {
          env: {
            GPR_PROJECT_PATH: ".:/tmp/Comps:/tmp/Asiscomps:",
          },
        });
        exec("gprinstall", ["-p", "-P", "adactl.gpr"]);
        break;
      default:
        break;
    }

    Deno.chdir("/tmp");
    if (!["Comps", "Asiscomps"].includes(pkgName)) {
      Deno.removeSync(pkgName, { recursive: true });
    }
    if (pkgName === "Adacontrol") {
      Deno.removeSync("Comps", { recursive: true });
      Deno.removeSync("Asiscomps", { recursive: true });
      Deno.removeSync("Comps.zip");
      Deno.removeSync("Asiscomps.zip");
    }

    Deno.removeSync(archivePath);
  }
}

function installPackageNoBuild(
  pkgName: string,
  targetDir: string = prefix,
  extension = ".tar.gz"
): void {
  const archivePath = `/tmp/${pkgName}${extension}`;
  console.log(`# Install package (without building) ${pkgName}`);

  if (existsSync(archivePath)) {
    Deno.chdir("/tmp");

    switch (extension) {
      case ".tar.gz":
        exec("tar", [
          "-xf",
          archivePath,
          "-C",
          `${join(targetDir, "/")}`,
          "--strip-components",
          "1",
        ]);
        break;
      case ".zip":
        unzipStrip(archivePath, `${join(targetDir, "/")}`);
        break;
      default:
        console.error(`Unknown archive extension: ${extension}`);
        Deno.exit(1);
    }
    Deno.chdir("/tmp");
    Deno.removeSync(archivePath);
  }
}

// Main
program.parse(Deno.args);

// Unset the prefix variable
Deno.env.delete("prefix");
