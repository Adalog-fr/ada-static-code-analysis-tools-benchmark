# Ada Static Code Analysis Benchmark

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

This repository aims to provide a benchmark comparison between static code analysis tools available in Ada. The goal is to provide a large amount of valid Ada code, totaling more than 5.7 million lines.

## Current Result

All present benchmarks are performed in a Docker container. You can find the Docker configuration in the [.devcontainer](./devcontainer) directory.

Benchmark tools used:
- GNATCheck
- AdaControl
- Cogralys (our solution)

### Result

The current benchmark results are yet to be added. Stay tuned for updates!

## How to Use

### Running a Benchmark

To run a benchmark, follow these steps:

1. TODO

### Adding Sources

In order to simplify dependency resolution, we use the [Alire](https://alire.ada.dev) package manager. To add new crates, please follow the instructions below:

1. Clone the project into the `src` directory or use `alr get -o CRATE_NAME`.
2. Add the path to the new crate into the [cratesPath.json](./cratesPath.json) file:
   `"CRATENAME": "path/to/src/CRATE_DIR"`
3. Add the path to `cratesPath.json`.
4. In the root crate folder, rename `alire.toml` to `alire.origin.toml`.
5. Run `cogralys-bench-util generate-alire -p .` to regenerate `alire.toml` with **pins** that point to crates located in the [src](./src) directory.
6. Check if the project compiles with `alr build`.

### Regenerating All Environment

To regenerate all files used for the benchmark environment, follow these instructions:

1. Go to the root of this repository.
2. Run `cogralys-bench-util generate-build-path -p src`. This will generate an `alireTomlPath.json` file containing all directories that contain an `alire.origin.toml` file. This file is used, for example, by the `build` command to build all projects.
3. Run `cogralys-bench-util generate-alire`. This will generate `alire.toml` from a list of directories (previously generated `alireTomlPath.json`) that contain an `alire.origin.toml` file. It will also delete the existing `alire` folder and generate an 'unknownCrates.json' file that contains a list of all unknown crate dependencies.
4. Run `cogralys-bench-util update-project`. This will concurrently run `alr -n update` in all directories listed in `alireTomlPath.json`.
5. (Optional but highly recommended for identifying future analysis problems) Run `cogralys-bench-util build`. This will run `alr -n build` in all directories listed in `alireTomlPath.json`.
