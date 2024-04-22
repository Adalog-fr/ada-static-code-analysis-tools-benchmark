# Ada Static Code Analysis Benchmark

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

This repository aims to provide a benchmark comparison between static code analysis tools available in Ada. The goal is to provide a large amount of valid Ada code to process benchmark for analysis tools.

Currently, due to ASIS limit, the project with Ada version up to 2012 is supported. Therefore, we have filtered projects for the benchmark via a set of files.

## Current Result

<!-- All present benchmarks are performed in a Docker container. You can find the Docker configuration in the [.devcontainer](./devcontainer) directory. -->

Benchmark tools used:
- GNATCheck: 24.0w (20230301)
- AdaControl: 1.23b4
- Cogralys: 0.1.0 (our solution)

The base code represents 1,047,941 lines of codes (counted using [Tokei](https://github.com/XAMPPRocky/tokei); so blank lines and comment lines are not included) in 162 projects (gpr files).

The benchmarking was performed on a computer with a Debian 12 operating system.

The computer specification:

- OS: Debian GNU/Linux 12 (bookworm) X86\_64
- Host: MS-7E12 1.0
- Kernel: 6.1.0-17-amd64
- CPU: AMD Ryzen 9 7950X3D (32) @ 4.2 GHz
- GPU: AMD ATI Radeon RX 7900 XTX
- Memory: 64 GB
- Storage: Crucial P5 Plus 1 TB SSD using M.2 PCIe Gen 4 connection, up to 6,600 MB/s in read operations and 5,000 MB/s in write operations

Regarding the software, the setup are:

- GNAT Pro 24.0w: Ada compiler.
- GNATcheck 24.0w: static analysis tools. libadalang version.
- AdaControl 1.23b4: static analysis tools. ASIS version.
- GNAT Pro 21lts: for ASIS support.
- Deno 1.38.3 with v8 12.0.267.1 and typescript 5.2.2: for benchmark scripts.

The Neo4J setup operates on Neo4J Desktop version 1.5.9.106. The database utilizes engine version 5.12.0, complemented by the APOC plugin.

### Result
| Rule                                     | Our approach      | AdaControl       | GNATcheck (monothread) | GNATcheck (multithread, 32 cores) |
| ---------------------------------------- | ----------------- | ---------------- | ----------------------- | ---------------------------------- |
| Constructors                             | 0.935s            | 1 min 5.7 s      | 1 min 54.513 s          | 2 min 5.042 s                      |
| Too many parents                         | 0.2s              | 1 min 3.1 s      | 1 min 45.978 s          | 2 min 2.94 s                       |
| Abort statements                         | 0.1s              | 1 min 4.3 s      | 46.434 s                | 1 min 33.557 s                     |
| Abstract type declarations               | 1.452s            | 1 min 5.1 s      | 1 min 25.576 s          | 1 min 52.851 s                     |
| Blocks                                   | 0.23s             | 1 min 3.9 s      | 46.508 s                | 1 min 33.563 s                     |
| Renamings                                | 0.44s             | 1 min 3.9 s      | 47.347 s                | 1 min 33.126 s                     |
| Slices                                   | 0.14s             | 1 min 12.6 s     | 1 min 58.599 s          | 2 min 4.714 s                      |
| Enumerated representation clauses        | 0.41s             | 1 min 3.1 s      | 46.683 s                | 1 min 33.487 s                     |
| **Analysis time (rule one by one)**      | **3.907 s**         | **9 min 49 s**    | **10 min 11.638 s**      | **14 min 19.28 s**                 |
| **Analysis time (rule in one batch)**    | **3.907 s**         | **21 min 13.864 s** | **9 min 40 s**          | **3 min 46 s**                     |

> [!NOTE]
> The analysis time does not factor in the overhead (read files, creating DB, etc.). Only the time required to check rules is accounted for.

## How to Use

### Requirements

Software requirements:

- GANT Community 2019 with ASIS or GNAT Pro >= 24 with ASIS.
- AdaControl >= 1.23b4
- Deno 1.38.3 with v8 12.0.267.1 and typescript 5.2.2: for benchmark scripts.
- [Cogralys Engine](https://github.com/Adalog-fr/cogralys-engine): core of our approach

### Running a Benchmark

To run a benchmark, follow these steps:

1. Run `./Adactl_benchmark.sh` to get the reslt benchmark for AdaControl
2. Run `./GNATcheck_benchmark.sh` to get the result benchmark for GNATcheck
3. Run `deno run --allow-all ./utils/cogralys-cli/cogralys-cli.ts -t` to get the result of our approach

### Adding Sources

To simplify dependency resolution, we use the [Alire](https://alire.ada.dev) package manager. To add new crates, please follow the instructions below:

1. Clone the project into the `src` directory or use `alr get -o CRATE_NAME`.
2. Add the path to the new crate into the [cratesPath.json](./cratesPath.json) file:
   `"CRATENAME": "path/to/src/CRATE_DIR"`
3. In the root crate folder, rename `alire.toml` to `alire.origin.toml`.
4. Run `cogralys-bench-util generate-alire -p .` to regenerate `alire.toml` with **pins** that point to crates located in the [src](./src) directory.
5. Check if the project compiles with `alr build`.
6. In the root crate folder, run `copy_load-system_into_obj.sh` to resolve issues related to ASIS that raise a Storage_Error when it attempts to access of some system packages.

### Regenerating All Environment

To regenerate all files used for the benchmark environment, follow these instructions:

1. Go to the root of this repository.
2. Run `cogralys-bench-util generate-build-path -p src`. This will generate an `alireTomlPath.json` file containing all directories that contain an `alire.origin.toml` file. This file is used, for example, by the `build` command to build all projects.
3. Run `cogralys-bench-util generate-alire`. This will generate `alire.toml` from a list of directories (previously generated `alireTomlPath.json`) that contain an `alire.origin.toml` file. It will also delete the existing `alire` folder and generate an 'unknownCrates.json' file that contains a list of all unknown crate dependencies.
4. Run `cogralys-bench-util update-project`. This will concurrently run `alr -n update` in all directories listed in `alireTomlPath.json`.
5. (Optional but highly recommended for identifying future analysis problems) Run `cogralys-bench-util build`. This will run `alr -n build` in all directories listed in `alireTomlPath.json`.
6. Run `cogralys-bench-util bench-adactl > /workspaces/bench-source/Adactl_benchmark.sh` to generate the benchmark experiment script for AdaControl.
7. Run `cogralys-bench-util bench-adactl > /workspaces/bench-source/GNATcheck_benchmark.sh` to generate the benchmark experiment script for AdaControl.
