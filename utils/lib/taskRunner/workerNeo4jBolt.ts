import neo4j, { Driver, Session } from "npm:neo4j-driver@5.10.0";
import { sleep } from "https://deno.land/x/sleep@v1.2.1/mod.ts";
import { NiceError } from "../NiceError/NiceError.ts";

let driver: Driver;

const work = (self as any as Worker);

work.onmessage = async ({ data }: { data: { taskId: string, data: { path: string, url: string, username: string, password: string, skip?: boolean, query?: string } } }) => {
    if (data.data.skip) {
        work.postMessage({ taskId: data.taskId, result: "", status: "success", path: data.data.path });
        return;
    }
    if (!driver) {
        driver = neo4j.driver(data.data.url, neo4j.auth.basic(data.data.username, data.data.password));
    }

    try {
        const queries = JSON.parse(data.data.query || Deno.readTextFileSync(data.data.path)).statements;

        let success = false;
        for (const retryBackoffInSeconds of [3, 15, 30, 60, 120]) {
            if (success) {
                return;
            }

            const session: Session = driver.session();
            try {
                for (const query of queries) {
                    const result = await session.run(query.statement.replace(/CREATE(\s*)\(/g, "MERGE$1("), query.parameters);
                    work.postMessage({ taskId: data.taskId, result: result, status: "success", path: data.data.path });
                    success = true;
                }
            } catch (error) {
                const errorMessage = new NiceError ({ cause: error });

                // Deadlock due to other transaction, apply a retry policy
                if (errorMessage.fullMessage().includes("can't acquire ExclusiveLock")) {
                    await sleep(retryBackoffInSeconds);
                } else {
                    work.postMessage({ taskId: data.taskId, result: errorMessage.fullStack(), status: "failure", path: data.data.path });
                    success = true;
                }
            } finally {
                session.close();
            }
        }
    } catch (error) {
        work.postMessage({ taskId: data.taskId, result: (new NiceError ({ cause: error })).fullStack(), status: "failure", path: data.data.path });
    }
};
