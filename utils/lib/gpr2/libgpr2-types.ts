export const enum methodList {
  TREE_LOAD = 1,
  TREE_UNLOAD = 2,
  TREE_LOG_MESSAGES = 3,
  TREE_INVALIDATE_SOURCE_LIST = 4,
  TREE_UPDATE_SOURCE_LIST = 5,
  TREE_UPDATE_SOURCE_INFOS = 6,
  VIEW_LOAD = 7,
  VIEW_ATTRIBUTE = 8,
  VIEW_SOURCES = 9,
  VIEW_UNITS = 10,
  SOURCE_DEPENDENCIES = 11,
  SOURCE_UPDATE_SOURCE_INFOS = 12,
}

/**
 * Represents the source location in GPR2.
 */
export interface Source_Location {
  /**
   * The filename of the source.
   */
  filename: string;
  /**
   * The line number in the source.
   */
  line?: number;
  /**
   * The column number in the source.
   */
  column?: number;
}

/**
 * Represents a message in GPR2.
 */
export interface Message {
  /**
   * The level of the message (e.g., "information", "warning", "error").
   */
  level: string;
  /**
   * The content of the message.
   */
  message: string;
  /**
   * The source location associated with the message.
   */
  sloc: Source_Location;
}

/**
 * Represents an attribute in GPR2.
 */
export interface Attribute {
  /**
   * The value of the attribute.
   * For single values, it can be a string.
   * For list values, it can be an array of strings.
   */
  value?: string | string[];
  /**
   * Indicates if the attribute is the default value.
   */
  is_default: boolean;
}

/**
 * Represents a unit in GPR2.
 * TBD: Add the structure for Unit
 */
export interface Unit {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
  /**
   * Array of unit
   */
  units: { name: string }[];
}

export interface sourceGPR {
  /**
   * The path of the unit.
   */
  path: string;
  is_aggregated: boolean;
  is_compilable: boolean;
  is_interface: boolean;
  has_name_exception: boolean;
  is_main: boolean;
  language: string;
  /**
   * Unix timestamp
   */
  timestamp: number;
}

/**
 * Represents a source in GPR2.
 */
export interface Source {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
  /**
   * Array of sources.
   */
  sources: sourceGPR[];
}
/**
 * Parameters for the TREE_LOAD operation.
 */
export interface LoadTreeParams {
  /**
   * The path to the GPR file to load.
   */
  filename: string;
  /**
   * A JSON object that contains values for external variables.
   */
  context?: Record<string, string>;
  /**
   * The root directory where build artifacts are stored.
   * If not provided, the object, libraries, and executable locations are relative to the project location.
   * This parameter can be used to handle out-of-tree builds.
   */
  build_path?: string;
  /**
   * If not `null`, add subdirectories as suffix to object, library, and executable directories.
   */
  subdirs?: string;
  /**
   * If not `null`, add the specified source directory for each project as the subdirectory of the object directory.
   */
  src_subdirs?: string;
  /**
   * If defined, load behaves as if the filename is located in the specified project directory.
   * If `null`, the directory in which the project file is located is used.
   * Using a non-`null` value allows the implementation of "default projects" in tools.
   */
  project_dir?: string;
  /**
   * Optional path to a configuration file. If `null`, the default configuration is used.
   */
  configuration_id?: string;
  /**
   * Optional list of implicitly withed projects.
   */
  implicit_with?: string[];
  /**
   * Optional target name.
   */
  target?: string;
  /**
   * Optional runtimes as a JSON object associating a language name with a runtime name.
   */
  runtimes?: Record<string, string>;
  /**
   * If `true`, check in the project tree that all projects describing shared libraries
   * do not import static libraries or standard projects.
   */
  check_shared_lib?: boolean;
  /**
   * If `true`, check if some key directories such as object directories exist.
   */
  absent_dir_error?: boolean;
}

/**
 * Result of the TREE_LOAD operation.
 */
export interface LoadTreeResult {
  /**
   * The loaded tree ID.
   */
  id: string;
  /**
   * The view ID of the tree root view.
   */
  root_view: string;
  /**
   * The view ID of the config view (`null` if there is no configuration).
   */
  config_view?: string;
  /**
   * The view ID of the runtime view (`null` if there is no runtime view).
   */
  runtime_view?: string;
  /**
   * The target as set by the user.
   */
  target: string;
  /**
   * The normalized target.
   */
  canonical_target: string;
  /**
   * The list of search paths used during tree loading.
   */
  search_paths: string[];
  /**
   * The subdirectory to add as a source directory for each project.
   */
  src_subdirs: string;
  /**
   * The subdirectory to add as a suffix to object, library, and executable directories.
   */
  subdirs: string;
  /**
   * The root directory where build artifacts are stored.
   */
  build_path: string;
  /**
   * The list of view IDs sorted topologically.
   */
  views: string[];
  /**
   * The tree context as a JSON object.
   */
  context: Record<string, string>;
}

/**
 * Parameters for the TREE_UNLOAD operation.
 */
export interface UnloadTreeParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
}

/**
 * Parameters for the TREE_LOG_MESSAGES operation.
 */
export interface LogMessagesParams {
  /**
   * The GPR2 tree ID.
   */
  tree_id: string;
  /**
   * If `true`, select messages for which the level is Information.
   */
  information?: boolean;
  /**
   * If `true`, select messages for which the level is Warning.
   */
  warning?: boolean;
  /**
   * If `true`, select messages for which the level is Error.
   */
  error?: boolean;
  /**
   * If `true`, return already read messages.
   */
  read?: boolean;
  /**
   * If `true`, return unread messages. Note that all returned unread messages are marked read after this call.
   */
  unread?: boolean;
}

/**
 * Result of the TREE_LOG_MESSAGES operation.
 */
export interface LogMessagesResult {
  /**
   * The list of log messages.
   */
  messages: Message[];
}

/**
 * Parameters for the TREE_INVALIDATE_SOURCE_LIST operation.
 */
export interface InvalidateSourceListParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
}

/**
 * Parameters for the TREE_UPDATE_SOURCE_LIST operation.
 */
export interface UpdateSourceListParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
}

/**
 * Parameters for the TREE_UPDATE_SOURCE_INFOS operation.
 */
export interface TreeUpdateSourceInfosParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
}

/**
 * Parameters for the VIEW_LOAD operation.
 */
export interface LoadViewParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
}

/**
 * Result of the VIEW_LOAD operation.
 */
export interface LoadViewResult {
  /**
   * The loaded view ID.
   */
  id: string;
  /**
   * The path of the loaded view.
   */
  path: string;
  /**
   * The directory of the loaded view.
   */
  dir: string;
  /**
   * The name of the loaded view.
   */
  name: string;
  /**
   * The kind of the loaded view.
   */
  kind: string;
}

/**
 * Parameters for the VIEW_ATTRIBUTE operation.
 */
export interface ViewAttributeParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
  /**
   * The attribute name.
   */
  name: string;
  /**
   * The optional package name.
   */
  pkg?: string;
  /**
   * The optional filename used to query an attribute for a specific index.
   * If the index is a filename, use the `filename` and optionally `position` (unit index for an Ada source filename).
   * If the index is a language, use `language`.
   * In all other cases, use `index`.
   */
  filename?: string;
  /**
   * The optional position used to query an attribute for a specific index.
   * It is the unit index for an Ada source filename.
   */
  position?: number;
  /**
   * The optional language used to query an attribute for a specific index.
   */
  language?: string;
  /**
   * The optional index used to query an attribute for a specific index.
   */
  index?: string;
}

/**
 * Result of the VIEW_ATTRIBUTE operation.
 */
export interface ViewAttributeResult {
  /**
   * The attribute value.
   */
  attribute: Attribute;
}

/**
 * Parameters for the VIEW_SOURCES operation.
 */
export interface ViewSourcesParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
}

/**
 * Result of the VIEW_SOURCES operation.
 */
export interface ViewSourcesResult {
  /**
   * The list of sources associated with the view.
   */
  sources: sourceGPR[];
}

/**
 * Parameters for the VIEW_UNITS operation.
 */
export interface ViewUnitsParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
}

/**
 * Result of the VIEW_UNITS operation.
 */
export interface ViewUnitsResult {
  /**
   * The list of units associated with the view.
   */
  units: { name: string }[];
}

/**
 * Parameters for the SOURCE_DEPENDENCIES operation.
 */
export interface SourceDependenciesParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
  /**
   * The path of the source.
   */
  path: string;
  /**
   * If `true`, include closure (transitive) dependencies.
   */
  closure?: boolean;
}

/**
 * Parameters for the SOURCE_UPDATE_SOURCE_INFOS operation.
 */
export interface UpdateSourceInfosParams {
  /**
   * The project tree ID.
   */
  tree_id: string;
  /**
   * The GPR view ID.
   */
  view_id: string;
  /**
   * If `true`, allow source parsing during the update.
   */
  allow_source_parsing?: boolean;
}

/**
 * If status is set to 0 (OK), then the **result** member contains the return value. The structure of the returned value is
 * described in each function. If status is set to another value, the call failed. In that case, the answer object
 * contains the error name and message in **error_msg** and **error_name**.
 */
export type gpr2RequestAnswer = {
    result: LoadTreeResult | LogMessagesResult | LoadViewResult | ViewAttributeResult | ViewSourcesResult | ViewUnitsResult | any;
    status: number;
    error_msg: string;
    error_name: string;
  };

export class GPR2Error extends Error {
  constructor(name: string, message: string) {
    super(message);
    Object.setPrototypeOf(this, GPR2Error.prototype);
    this.name = name;
    this.message = message;
  }
}