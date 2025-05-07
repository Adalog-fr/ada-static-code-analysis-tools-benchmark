function errorToString(e) {
    return `${e.message}\n${e.stack}\n  name: ${e.name}\n  code:${e.code}`
}

self.onmessage = async ({ data }: { data: { taskId: string, data: { path: string, url: string, username: string, password: string, skip?: boolean, query?: string } } }) => {
    if (data.data.skip) {
        self.postMessage({ taskId: data.taskId, result: "", status: "success", path: data.data.path });
        return;
    }
    const instance = new URL(`http://${data.data.url}/db/neo4j/tx/commit`);

    try {

        const query = data.data.query || Deno.readTextFileSync(data.data.path);
        // let query: string | ReadableStream<Uint8Array>;
        // let file;
        // if (data.data.query) {
        //     query = data.data.query;
        // } else {
        //     file = Deno.openSync(data.data.path, { read: true });
        //     query = file.readable;
        // }

        const result = await fetch(instance.href, {
            method: "POST",
            keepalive: true,
            headers: {
                "Content-Type": "application/json",
                Accept: "application/json",
                Authorization: "Basic " + btoa(`${data.data.username}:${data.data.password}`),
            },
            body: query,
        });

        if (!result.ok) {
            self.postMessage({ taskId: data.taskId, result: await result.text(), status: "failure", path: data.data.path });
        }
    } catch (e) {
        console.error(e);
        self.postMessage({ taskId: data.taskId, result: errorToString(e), status: "failure", path: data.data.path });
    }

    self.postMessage({ taskId: data.taskId, result: "", status: "success", path: data.data.path });
};
