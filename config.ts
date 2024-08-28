import { dirname, fromFileUrl } from "https://deno.land/std/path/mod.ts";

export const PROJECT_ROOT = dirname(fromFileUrl(import.meta.url));
export const COGRALYS_DIR_NAME = ".cogralys"
