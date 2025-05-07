self.onmessage = async ({ data }) => {
  console.log("start", data.taskId);

  const taskId = data.taskId;
  const result = await executeTask(taskId);
  // self.postMessage(result);
  self.postMessage({ taskId, result });
};

async function executeTask(taskId: string): Promise<any> {
  // Perform the task execution based on the task ID
  // You can implement your specific task logic here
  // For demonstration purposes, a simple delay is used

  const delayTime = Math.random() * 2000;
  await delay(delayTime);

  // Return the result of the task
  return `Task "${taskId}" completed after ${delayTime}ms`;
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
