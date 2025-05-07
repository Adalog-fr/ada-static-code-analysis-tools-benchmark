export type GhNullableLicenseSimple = {
    /** @example mit */
    key: string;
    /** @example MIT License */
    name: string;
    /**
     * Format: uri
     * @example https://api.github.com/licenses/mit
     */
    url: string | null;
    /** @example MIT */
    spdx_id: string | null;
    /** @example MDc6TGljZW5zZW1pdA== */
    node_id: string;
    /** Format: uri */
    html_url?: string;
} | null;

export type GhSearchResultTextMatches = {
    object_url?: string;
    object_type?: string | null;
    property?: string;
    fragment?: string;
    matches?: {
        text?: string;
        indices?: number[];
    }[];
}[];
export type GhNullableSimpleUser = {
    name?: string | null;
    email?: string | null;
    /** @example octocat */
    login: string;
    /**
     * Format: int64
     * @example 1
     */
    id: number;
    /** @example MDQ6VXNlcjE= */
    node_id: string;
    /**
     * Format: uri
     * @example https://github.com/images/error/octocat_happy.gif
     */
    avatar_url: string;
    /** @example 41d064eb2195891e12d0413f63227ea7 */
    gravatar_id: string | null;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat
     */
    url: string;
    /**
     * Format: uri
     * @example https://github.com/octocat
     */
    html_url: string;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat/followers
     */
    followers_url: string;
    /** @example https://api.github.com/users/octocat/following{/other_user} */
    following_url: string;
    /** @example https://api.github.com/users/octocat/gists{/gist_id} */
    gists_url: string;
    /** @example https://api.github.com/users/octocat/starred{/owner}{/repo} */
    starred_url: string;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat/subscriptions
     */
    subscriptions_url: string;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat/orgs
     */
    organizations_url: string;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat/repos
     */
    repos_url: string;
    /** @example https://api.github.com/users/octocat/events{/privacy} */
    events_url: string;
    /**
     * Format: uri
     * @example https://api.github.com/users/octocat/received_events
     */
    received_events_url: string;
    /** @example User */
    type: string;
    site_admin: boolean;
    /** @example "2020-07-09T00:17:55Z" */
    starred_at?: string;
} | null;

// repo-search-result-item
export interface GhRepoSearchResultItem {
    id: number;
    node_id: string;
    name: string;
    full_name: string;
    owner: GhNullableSimpleUser;
    private: boolean;
    /** Format: uri */
    html_url: string;
    description: string | null;
    fork: boolean;
    /** Format: uri */
    url: string;
    /** Format: date-time */
    created_at: string;
    /** Format: date-time */
    updated_at: string;
    /** Format: date-time */
    pushed_at: string;
    /** Format: uri */
    homepage: string | null;
    size: number;
    stargazers_count: number;
    watchers_count: number;
    language: string | null;
    forks_count: number;
    open_issues_count: number;
    master_branch?: string;
    default_branch: string;
    score: number;
    /** Format: uri */
    forks_url: string;
    keys_url: string;
    collaborators_url: string;
    /** Format: uri */
    teams_url: string;
    /** Format: uri */
    hooks_url: string;
    issue_events_url: string;
    /** Format: uri */
    events_url: string;
    assignees_url: string;
    branches_url: string;
    /** Format: uri */
    tags_url: string;
    blobs_url: string;
    git_tags_url: string;
    git_refs_url: string;
    trees_url: string;
    statuses_url: string;
    /** Format: uri */
    languages_url: string;
    /** Format: uri */
    stargazers_url: string;
    /** Format: uri */
    contributors_url: string;
    /** Format: uri */
    subscribers_url: string;
    /** Format: uri */
    subscription_url: string;
    commits_url: string;
    git_commits_url: string;
    comments_url: string;
    issue_comment_url: string;
    contents_url: string;
    compare_url: string;
    /** Format: uri */
    merges_url: string;
    archive_url: string;
    /** Format: uri */
    downloads_url: string;
    issues_url: string;
    pulls_url: string;
    milestones_url: string;
    notifications_url: string;
    labels_url: string;
    releases_url: string;
    /** Format: uri */
    deployments_url: string;
    git_url: string;
    ssh_url: string;
    clone_url: string;
    /** Format: uri */
    svn_url: string;
    forks: number;
    open_issues: number;
    watchers: number;
    topics?: string[];
    /** Format: uri */
    mirror_url: string | null;
    has_issues: boolean;
    has_projects: boolean;
    has_pages: boolean;
    has_wiki: boolean;
    has_downloads: boolean;
    has_discussions?: boolean;
    archived: boolean;
    /** @description Returns whether or not this repository disabled. */
    disabled: boolean;
    /** @description The repository visibility: public, private, or internal. */
    visibility?: string;
    license: GhNullableLicenseSimple;
    permissions?: {
        admin: boolean;
        maintain?: boolean;
        push: boolean;
        triage?: boolean;
        pull: boolean;
    };
    text_matches?: GhSearchResultTextMatches;
    temp_clone_token?: string;
    allow_merge_commit?: boolean;
    allow_squash_merge?: boolean;
    allow_rebase_merge?: boolean;
    allow_auto_merge?: boolean;
    delete_branch_on_merge?: boolean;
    allow_forking?: boolean;
    is_template?: boolean;
    /** @example false */
    web_commit_signoff_required?: boolean;
}
