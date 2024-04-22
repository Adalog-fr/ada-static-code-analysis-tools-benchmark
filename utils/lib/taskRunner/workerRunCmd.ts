import { exec } from "../../utils.ts";

function errorToString(e) {
  return `${e.message}\n${e.stack}\n  name: ${e.name}\n  code:${e.code}`
}

self.onmessage = ({ data }: { data: { taskId: string, data: { path: string, command: [string, string[], {[key: string]: string}] } } }) => {
  try {
    Deno.chdir(data.data.path);
    const taskId = data.taskId;

    const result = exec(...data.data.command);
    self.postMessage({ taskId, result, status: result.success ? "success" : "failure", path: data.data.path });
  } catch (e) {
    console.error(e);
    self.postMessage({ taskId: data.taskId, result: errorToString(e), status: "failure", path: data.data.path });
  }
};
