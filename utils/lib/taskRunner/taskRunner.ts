export type Task<T> = {
    id: string;
    dependencies: string[];
    data: T;
};

function taskIsReady<T>(task: Task<T>, completedTaskIds: Set<string>): boolean {
    return task.dependencies.every(dep => completedTaskIds.has(dep));
}

function findReadyTask<T>(tasks: Task<T>[], completedTaskIds: Set<string>): Task<T> | undefined {
    return tasks.find(task => taskIsReady(task, completedTaskIds));
}

class AsyncQueue<T> {
    private queue: T[] = [];
    private resolvers: ((value: T) => void)[] = [];

    get length() {
        return this.queue.length;
    }

    enqueue(value: T) {
        if (this.resolvers.length > 0) {
            const resolver = this.resolvers.shift()!;
            resolver(value);
        } else {
            this.queue.push(value);
        }
    }

    dequeue(): Promise<T> {
        if (this.queue.length > 0) {
            const value = this.queue.shift()!;
            return Promise.resolve(value);
        } else {
            return new Promise(resolve => this.resolvers.push(resolve));
        }
    }

    pop() {
        return this.queue.shift();
    }

    terminate() {
        this.queue = [];
    }
}

class ExtendedWorker extends Worker {
    public taskId = "";
    public index = 0;
}

function generateMissingDependenciesList<T>(tasks: Task<T>[], missingDeps: string[]): string {
    let markdownOutput = '';

    tasks.forEach(task => {
        // Find the intersection of task dependencies and missing dependencies
        const taskMissingDeps = task.dependencies.filter(dep => missingDeps.includes(dep));

        if (taskMissingDeps.length > 0) {
            // Append the task ID followed by its missing dependencies
            markdownOutput += `${task.id}:\n`;
            taskMissingDeps.forEach(dep => {
                markdownOutput += `  - ${dep}\n`;
            });
        }
    });

    return markdownOutput;
}

type taskResultType<T, U> = { id: string, result: U, status: "success", task: Task<T> } | { id: string, result: ErrorEvent | MessageEvent, status: "failure", task: Task<T> };

export type preTaskCbType<T> = (task: Task<T>, index: number) => Promise<void> | void;
export type postTaskCbType<T, U> = (result: taskResultType<T, U>, index: number) => Promise<void> | void;

export class TaskRunner<T, U> {
    private workerPool: AsyncQueue<ExtendedWorker> = new AsyncQueue<ExtendedWorker>();
    private taskQueue: Task<T>[] = [];
    private completedTaskIds = new Set<string>();
    private incompletedTaskIds = new Set<string>();
    private results: taskResultType<T,U>[] = [];
    private activeTasks = 0; // Track the number of active tasks

    private _preTaskCb: preTaskCbType<T> | undefined;
    private _postTaskCb: postTaskCbType<T, U> | undefined;
    private workerIndex = 0;

    private assocMap: Map<number, Task<T>> = new Map();

    constructor(private concurrency: number, private workerPath: string, tasks?: Task<T>[], private ignoreMissingDependencies: boolean = false) {
        if (typeof concurrency !== 'number' || concurrency < 1) {
            throw new Error("Concurrency must be a positive integer");
        }

        if (typeof workerPath !== 'string') {
            throw new Error("Worker path must be a string");
        }

        this.taskQueue = tasks || [];

        for (let i = 0; i < this.concurrency; i++) {
            const worker = new ExtendedWorker(new URL(this.workerPath, import.meta.url).href, { type: "module" });

            worker.onmessage = async (ev: MessageEvent) => {
                const { taskId, result, status, path }: { taskId: string, result: any, status: "success" | "failure", path: string } = ev.data;
                this.results.push({ id: taskId, result, status, task: this.assocMap.get(worker.index)! });
                if (status === "success") {
                    this.completedTaskIds.add(taskId);
                } else {
                    this.incompletedTaskIds.add(taskId);
                }
                if (this._postTaskCb) {
                    await this._postTaskCb(this.results[this.results.length - 1], worker.index);
                }
                this.activeTasks--;
                this.workerPool.enqueue(worker);
            };

            worker.onerror = async (error) => {
                this.results.push({ id: worker.taskId, result: error, status: "failure", task: this.assocMap.get(worker.index)! });
                this.incompletedTaskIds.add(worker.taskId);
                if (this._postTaskCb) {
                    await this._postTaskCb(this.results[this.results.length - 1], worker.index);
                }
                console.error("Worker error: ", error);
            };

            worker.onmessageerror = async (error) => {
                this.results.push({ id: worker.taskId, result: error, status: "failure", task: this.assocMap.get(worker.index)! });
                this.incompletedTaskIds.add(worker.taskId);
                if (this._postTaskCb) {
                    await this._postTaskCb(this.results[this.results.length - 1], worker.index);
                }
                console.error("Message error: ", error);
            };

            this.workerPool.enqueue(worker);
        }
    }

    set preTaskCb(value: preTaskCbType<T> | undefined) {
        this._preTaskCb = value;
    }

    set postTaskCb(value: postTaskCbType<T, U> | undefined) {
        this._postTaskCb = value;
    }

    addTask(id: string, dependencies: string[], data: T): void {
        if (typeof id !== 'string') {
            throw new Error("Task ID must be a string");
        }

        if (!Array.isArray(dependencies)) {
            throw new Error("Dependencies must be an array");
        }

        for (let dep of dependencies) {
            if (typeof dep !== 'string') {
                throw new Error("Each dependency must be a string");
            }
        }

        this.taskQueue.push({ id, dependencies, data });
    }

    async run() {
        if (!this.ignoreMissingDependencies) {
            // Check if all dependencies exist in the taskQueue
            const taskQueueIds = this.taskQueue.map(elt => elt.id);
            const dependencySet = new Set<string>(this.taskQueue.map(elt => elt.dependencies).flat());
            const missingDependencies: string[] = [];

            for (const depenedncy of dependencySet) {
                if (!taskQueueIds.includes(depenedncy)) {
                    missingDependencies.push(depenedncy);
                }
            }

            if (missingDependencies.length > 0) {
                throw new Error(`Missing dependencies: ${missingDependencies.join(', ')
                    }\n\nMissing dependencies by task:\n${generateMissingDependenciesList<T>(this.taskQueue, missingDependencies)}`);
            }
        }

        while (this.taskQueue.length > 0 || this.activeTasks > 0) {
            const task = findReadyTask(this.taskQueue, this.completedTaskIds);
            const notRunableTask = findReadyTask(this.taskQueue, this.incompletedTaskIds);

            if (task) {
                // If there are no idle workers, wait for one to become available
                while (this.workerPool.length === 0) {
                    await new Promise(resolve => setTimeout(resolve, 0));
                }

                this.taskQueue = this.taskQueue.filter(t => t !== task);
                const worker = await this.workerPool.dequeue();
                worker.taskId = task.id;
                worker.index = ++this.workerIndex;
                this.assocMap.set(worker.index, task);

                if (this._preTaskCb) {
                    await this._preTaskCb(task, worker.index);
                }

                worker.postMessage({ taskId: task.id, data: task.data });
                this.activeTasks++;
            } else if (notRunableTask) {
                this.taskQueue = this.taskQueue.filter(t => t !== notRunableTask);
                if (this._preTaskCb) {
                    await this._preTaskCb(notRunableTask, -1);
                }
                const result = { output: `This task is not running due to failure on the following tasks: ${
                    notRunableTask.dependencies.filter(dep => this.incompletedTaskIds.has(dep)).join(",")}` };
                const status = "failure";
                const resultObject = { id: notRunableTask.id, result, status, task: notRunableTask };
                this.results.push(resultObject);
                this.incompletedTaskIds.add(notRunableTask.id);
                if (this._postTaskCb) {
                    await this._postTaskCb(resultObject, -1);
                }
            } else {
                await new Promise(resolve => setTimeout(resolve, 0));
            }
        }
        return this.results;
    }

    terminate() {
        let worker;
        do {
            worker = this.workerPool.pop();
            try {
                worker?.terminate();
            } catch (error) {
                console.error('Error while terminating worker: ', error);
            }
        } while (worker);
        this.workerPool.terminate();
    }

    getNbTasks() {
        return this.taskQueue.length;
    }
}
