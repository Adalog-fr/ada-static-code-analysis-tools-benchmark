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

The base code represents 2,643,887 lines of code (counted using [SCC](https://github.com/boyter/scc); blank lines and comment lines are not included) in 134 projects (GPR files).

The benchmarking was performed on a computer with a Debian 12 operating system.

The computer specification:

- OS: Debian GNU/Linux 12 (bookworm) X86\_64
- Host: MS-7E12 1.0
- Kernel: 6.1.0-17-amd64
- CPU: AMD Ryzen 9 7950X3D (32) @ 4.2 GHz
- GPU: AMD ATI Radeon RX 7900 XTX
- Memory: 64 GB
- Storage: Crucial P5 Plus 1 TB SSD using M.2 PCIe Gen 4 connection, up to 6,600 MB/s in read operations and 5,000 MB/s in write operations

Regarding the software, the setup is:

- GNAT Pro 24.0w: Ada compiler.
- GNATcheck 24.0w: static analysis tool. libadalang version.
- AdaControl 1.23b4: static analysis tool. ASIS version.
- GNAT Pro 21lts: for ASIS support.
- Deno 1.46.3 with v8 12.0.267.1 and typescript 5.2.2: for benchmark scripts.

The Neo4J setup operates on Neo4J Desktop version 1.5.9.106. The database utilizes engine version 5.12.0, complemented by the APOC plugin.
To prevent a bug in Deno 2.X.X (see https://github.com/denoland/deno/issues/27556), it is recommended to use the latest 1.X.X version of Deno: `deno upgrade 1.46.3`

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
- Deno 1.46.3 with v8 12.0.267.1 and typescript 5.2.2: for benchmark scripts.
- [Cogralys Engine](https://github.com/Adalog-fr/cogralys-engine): core of our approach

### Running the Benchmarks

Get started with our benchmarking suite in just a few simple steps:

1. **Setting Up Your Environment**  
   For a fresh installation, check out the [installation guide](./install/README.md). The quickest path is to navigate to the `install` directory and run `./install-main.sh` after downloading the required archives.

2. **Preparing the Benchmark Environment**  
   Run `./setup-benchmark.sh` to validate your environment and configure the benchmark repository. This script ensures all dependencies are properly installed and sets up the necessary components.

3. **Launching the Benchmarks**  
   Execute `./benchmark-all.sh` to run the complete benchmark suite. Results are generated as Markdown reports by default, making them easy to view and share. Result data are generated in `.json` files at the root of the repository.

Need help? All scripts support the `-h` or `--help` flag to display detailed usage information.

#### Customizing Your Benchmark Run

The benchmark suite is highly configurable through command-line options:

```
# Basic usage
./benchmark-all.sh

# Custom Neo4j connection
./benchmark-all.sh --neo4j-uri bolt://localhost:7687 --neo4j-user neo4j --neo4j-password password

# Run only specific tools
./benchmark-all.sh --skip-adactl --skip-gnatcheck
```

**Available Options:**

- **Database Connection**
  - `--neo4j-uri URI`: Neo4j connection URI
  - `--neo4j-user USER`: Neo4j username
  - `--neo4j-password PASSWORD`: Neo4j password

- **Output Control**
  - `--output-dir DIR`: Custom directory for benchmark results

- **Tool Selection**
  - `--skip-adactl`: Exclude AdaControl from benchmarks
  - `--skip-gnatcheck`: Exclude GNATcheck from benchmarks
  - `--skip-cogralys`: Exclude Cogralys from benchmarks

- **Performance Options**
  - `--use-cache`: Leverage cached results for faster runs. This could only be used by Cogralys. to prevent bias, make sure you only use this when the measurement phase of the database file generation is completely finished (this implies having completed a full benchmark run).
  - `--project-list LIST`: Target specific projects

- **Benchmark Modes**
  - `--benchmark-only`: Run only the all-rules-at-once benchmark
  - `--rule-by-rule-only`: Run only the rule-by-rule benchmark
  - `--generate-report-only`: Generate reports from existing data

### Adding New Projects

To add a new Ada project to the benchmark suite, follow these steps:

> [!NOTE]
> The commands below assume you have set up `cogralys-bench-util` as an alias: 
> ```
> cogralys-bench-util="deno run --config /path/to/benchmark/deno.jsonc --unstable-ffi --allow-all /path/to/benchmark/utils/cogralys-bench-util.ts $@"
> ```

1. **Clone and prepare the target project**
   ```sh
   # Clone the repository you want to add
   git clone https://github.com/username/ada-project.git
   cd ada-project
   
   # Rename the original alire.toml file to preserve it
   mv alire.toml alire.origin.toml
   ```

2. **Configure the project for local dependencies**
   ```sh
   # Generate a new alire.toml that uses local dependencies
   cogralys-bench-util generate-alire -p .
   
   # Generate the environment configuration
   deno run --config /path/to/benchmark/deno.jsonc --allow-all /path/to/benchmark/utils/executeEnvFileGeneration.ts
   
   # Build the project
   alr -n build
   
   # Apply the Storage_Unit fix
   copy_load-system_into_obj.sh .
   ```

3. **Generate compilation unit information**
   ```sh
   # Generate lists of compilation units in different formats
   cogralys-bench-util units -P alire alire.toml
   cogralys-bench-util units -P alire alire.toml -f file
   cogralys-bench-util units -P alire alire.toml -f path
   
   # Generate code metrics
   cogralys-bench-util generate-scc-metrics project.units_by_path
   ```
   Where `project` of `project.units_by_path` is the name of the `.gpr` file.

4. **Test with static analysis tools**
   ```sh
   # Run AdaControl analysis
   time alr exec -- adactl -f /path/to/benchmark/benchmark-rules/all_rules_in_one_file/_all.aru \
     -p /path/to/project/project.gpr @/path/to/project/project.units \
     -o adactl-report.log -w
   
   # Run GNATcheck analysis
   time alr exec -- gnatcheck --brief -q -t -l --show-rule \
     -o gnatcheck-report.log -P/path/to/project/project.gpr \
     -rules -from=/path/to/benchmark/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
   ```

5. **Add the project to the benchmark database**
   ```sh
   # Add the project to cratesDB.json
   cogralys-bench-util add-project -c project_name -w src/project_name -g src/project_name/project.gpr
   
   # Regenerate benchmark files
   cogralys-bench-util bench-adactl
   cogralys-bench-util bench-gnatcheck
   cogralys-bench-util bench-cogralys
   ```

This process ensures that each project is properly integrated into the benchmark suite, with all necessary metadata and configuration to run consistent comparisons across all static analysis tools.
