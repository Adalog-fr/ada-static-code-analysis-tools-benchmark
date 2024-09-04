import { dirname, fromFileUrl } from "jsr:@std/path@^1.0.4";

export const PROJECT_ROOT = dirname(fromFileUrl(import.meta.url));
export const COGRALYS_DIR_NAME = ".cogralys";
export const COGRALYS_EXE_NAME = "atgdb";
