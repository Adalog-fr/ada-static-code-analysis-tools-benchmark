import { join } from "jsr:@std/path@^0.225.1";
import { PROJECT_ROOT } from "../../../config.ts";

import {
  methodList,
  gpr2RequestAnswer,
  GPR2Error,
  LoadTreeParams,
  LoadTreeResult,
  InvalidateSourceListParams,
  LoadViewParams,
  LoadViewResult,
  LogMessagesParams,
  LogMessagesResult,
  SourceDependenciesParams,
  TreeUpdateSourceInfosParams,
  UnloadTreeParams,
  UpdateSourceInfosParams,
  UpdateSourceListParams,
  ViewAttributeParams,
  ViewAttributeResult,
  ViewSourcesParams,
  ViewSourcesResult,
  ViewUnitsParams,
  ViewUnitsResult,
} from "./libgpr2-types.ts";

let libSuffix = "";
switch (Deno.build.os) {
  case "windows":
    libSuffix = "dll";
    break;
  case "darwin":
    libSuffix = "dylib";
    break;
  default:
    libSuffix = "so";
    break;
}

const libName = join(PROJECT_ROOT, `src/libgpr2/bindings/c/build/release/lib/libgpr2c.${libSuffix}`);

const lib = Deno.dlopen(libName, {
  gpr2_request: {
    parameters: ["i32", "buffer", "buffer"],
    result: "i32",
  },
  gpr2_free_answer: {
    parameters: ["pointer"],
    result: "void",
  },
});

/**
 * @param fun_id {methodList} is the function id (see number next to each binding method).
 * @param request {Uint8Array} is a JSON string containing one JSON object. The structure of the object depends on the called method (see documentation of each method).
 * @param answer {BigUint64Array} is a JSON string containing on JSON object. @see gpr2RequestAnswer
 * @returns {number} 0 for OK. If the value is not 0, #answer contains a field **error_msg** and **error_name** with details of the error.
 */
type gpr2_request = (
  fun_id: methodList,
  request: Uint8Array,
  answer: BigUint64Array
) => number;

// Create a wrapper function for gpr2_request
function gpr2RequestWrapper(
  funId: methodList,
  request: string
): gpr2RequestAnswer {
  const ptr = new BigUint64Array(1);

  const result = (lib.symbols.gpr2_request as gpr2_request)(
    funId,
    new TextEncoder().encode(request + "\0"),
    ptr
  );

  const answer: gpr2RequestAnswer = JSON.parse(
    new Deno.UnsafePointerView(Deno.UnsafePointer.create(ptr[0])).getCString()
  );

  if (result != 0) {
    throw new GPR2Error(answer.error_name, answer.error_msg);
  }

  return answer;
}

export function loadTree(params: LoadTreeParams): LoadTreeResult {
  return gpr2RequestWrapper(methodList.TREE_LOAD, JSON.stringify(params))
    .result;
}

export function unloadTree(params: UnloadTreeParams): any {
  return gpr2RequestWrapper(methodList.TREE_UNLOAD, JSON.stringify(params))
    .result;
}

export function logMessages(params: LogMessagesParams): LogMessagesResult {
  return gpr2RequestWrapper(
    methodList.TREE_LOG_MESSAGES,
    JSON.stringify(params)
  ).result;
}

export function invalidateSourceList(
  params: InvalidateSourceListParams
): any {
  return gpr2RequestWrapper(
    methodList.TREE_INVALIDATE_SOURCE_LIST,
    JSON.stringify(params)
  ).result;
}

export function updateSourceList(
  params: UpdateSourceListParams
): any {
  return gpr2RequestWrapper(
    methodList.TREE_UPDATE_SOURCE_LIST,
    JSON.stringify(params)
  ).result;
}

export function treeUpdateSourceInfos(params: TreeUpdateSourceInfosParams): any {
  return gpr2RequestWrapper(
    methodList.TREE_UPDATE_SOURCE_INFOS,
    JSON.stringify(params)
  ).result;
}

export function loadView(params: LoadViewParams): LoadViewResult {
  return gpr2RequestWrapper(methodList.VIEW_LOAD, JSON.stringify(params))
    .result;
}

export function viewAttribute(params: ViewAttributeParams): ViewAttributeResult {
  return gpr2RequestWrapper(methodList.VIEW_ATTRIBUTE, JSON.stringify(params))
    .result;
}

export function viewSources(params: ViewSourcesParams): ViewSourcesResult {
  return gpr2RequestWrapper(methodList.VIEW_SOURCES, JSON.stringify(params))
    .result;
}

export function viewUnits(params: ViewUnitsParams): ViewUnitsResult {
  return gpr2RequestWrapper(methodList.VIEW_UNITS, JSON.stringify(params))
    .result;
}

export function sourceDependencies(
  params: SourceDependenciesParams
): any {
  return gpr2RequestWrapper(
    methodList.SOURCE_DEPENDENCIES,
    JSON.stringify(params)
  ).result;
}

export function updateSourceInfos(
  params: UpdateSourceInfosParams
): any {
  return gpr2RequestWrapper(
    methodList.SOURCE_UPDATE_SOURCE_INFOS,
    JSON.stringify(params)
  ).result;
}
