export function collectOptionList(value, previous) {
  return previous.concat([value]);
}

export function exec(
  command: string | URL,
  args?: string[] | undefined,
  options?: Deno.CommandOptions | undefined,
  surroundEnvWithEnvFile?: boolean,
) {
  const process = new Deno.Command(command, {
      args,
      stdout: "piped",
      stderr: "piped",
      ...options,
  });

  const { code, stderr, stdout } = process.outputSync();

  if (code !== 0) {
      const errorOutput = new TextDecoder().decode(stderr);
      return { success: false, output: errorOutput || new TextDecoder().decode(stdout) };
  }

  return { success: true, output: new TextDecoder().decode(stdout) };
}

export function formatDuration(milliseconds: number): string {
  const seconds = Math.floor(milliseconds / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);

  const durationParts = [];
  if (days > 0) {
      durationParts.push(`${days}d`);
  }
  if (hours > 0) {
      durationParts.push(`${hours % 24}h`);
  }
  if (minutes > 0) {
      durationParts.push(`${minutes % 60}m`);
  }
  if (seconds > 0) {
      durationParts.push(`${seconds % 60}s`);
  }
  if (milliseconds > 0) {
      durationParts.push(`${milliseconds % 1000}ms`);
  }

  return durationParts.join(" ");
}
