#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]

struct State {
    client_socket: async_lsp::ClientSocket,
    projects: std::collections::HashMap<
        /* path to directory containing elm.json */ std::path::PathBuf,
        ProjectState,
    >,
}

struct ProjectState {
    source_directories: Vec<std::path::PathBuf>,
    modules: std::collections::HashMap<std::path::PathBuf, ModuleState>,
    dependency_exposed_module_names: std::collections::HashMap<String, ProjectModuleOrigin>,
    elm_make_errors: Vec<ElmMakeFileCompileError>,
}
#[derive(Debug)]
struct ProjectModuleOrigin {
    project_path: std::path::PathBuf,
    module_path: std::path::PathBuf,
}
struct ModuleState {
    syntax: ElmSyntaxModule,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _client_socket) = async_lsp::MainLoop::new_server(|client| {
        let mut router: async_lsp::router::Router<State> = async_lsp::router::Router::new(State {
            client_socket: client.clone(),
            projects: std::collections::HashMap::new(),
        });
        router.request::<lsp_types::request::Initialize, _>({
            |state, initialize_arguments| {
                initialize_state_for_workspace_directories_into(state, initialize_arguments);
                let file_watch_registration_options: lsp_types::DidChangeWatchedFilesRegistrationOptions =
                    lsp_types::DidChangeWatchedFilesRegistrationOptions {
                        watchers: state
                            .projects
                            .values()
                            .flat_map(|project| &project.source_directories)
                            .filter_map(|source_directory_path| {
                                lsp_types::Url::from_directory_path(source_directory_path).ok()
                            })
                            .map(|source_directory_url| lsp_types::FileSystemWatcher {
                                glob_pattern: lsp_types::GlobPattern::Relative(
                                    lsp_types::RelativePattern {
                                        base_uri: lsp_types::OneOf::Right(source_directory_url),
                                        pattern: "**/*.{elm,elm-testing}".to_string(),
                                    },
                                ),
                                kind: Some(
                                    lsp_types::WatchKind::Create
                                        | lsp_types::WatchKind::Change
                                        | lsp_types::WatchKind::Delete,
                                ),
                            })
                            .collect::<Vec<lsp_types::FileSystemWatcher>>(),
                    };
                match serde_json::to_value(file_watch_registration_options) {
                    Err(encode_error) => {
                        eprintln!(
                            "failed to register file watchers because encoding the request \
                            options failed: {encode_error}"
                        );
                    }
                    Ok(file_watch_registration_options_json) => {
                        let _ = state.client_socket.request::<lsp_types::request::RegisterCapability>(
                            lsp_types::RegistrationParams {
                                registrations: vec![lsp_types::Registration {
                                    id: "file-watch".to_string(),
                                    method: <lsp_types::notification::DidChangeWatchedFiles as lsp_types::notification::Notification>::METHOD
                                        .to_string(),
                                    register_options: Some(file_watch_registration_options_json),
                                }],
                            },
                        );
                    }
                }
                async move {
                    Ok(lsp_types::InitializeResult {
                        capabilities: lsp_types::ServerCapabilities {
                            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
                            definition_provider: Some(lsp_types::OneOf::Left(true)),
                            semantic_tokens_provider: Some(
                                lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                                    lsp_types::SemanticTokensOptions {
                                        work_done_progress_options:
                                            lsp_types::WorkDoneProgressOptions {
                                                work_done_progress: None,
                                            },
                                        legend: lsp_types::SemanticTokensLegend {
                                            token_modifiers: Vec::new(),
                                            token_types: Vec::from(token_types),
                                        },
                                        range: None,
                                        full: Some(lsp_types::SemanticTokensFullOptions::Bool(
                                            true,
                                        )),
                                    },
                                ),
                            ),
                            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                                lsp_types::TextDocumentSyncKind::FULL,
                            )),
                            rename_provider: Some(lsp_types::OneOf::Right(
                                lsp_types::RenameOptions {
                                    prepare_provider: Some(true),
                                    work_done_progress_options:
                                        lsp_types::WorkDoneProgressOptions {
                                            work_done_progress: None,
                                        },
                                },
                            )),
                            completion_provider: Some(lsp_types::CompletionOptions {
                                resolve_provider: Some(false),
                                trigger_characters: Some(vec![".".to_string()]),
                                all_commit_characters: None,
                                work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                                completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
                                    label_details_support: None,
                                }),
                            }),
                            ..lsp_types::ServerCapabilities::default()
                        },
                        server_info: Some(lsp_types::ServerInfo {
                            name: "elm-language-server-rs".to_string(),
                            version: Some("pre-release".to_string())
                        }),
                    })
                }
            }
        });
        router.request::<lsp_types::request::HoverRequest, _>(
            |state, hover_arguments: lsp_types::HoverParams| {
                let maybe_hover_result: Option<lsp_types::Hover> =
                    respond_to_hover(state, hover_arguments);
                async move { Ok(maybe_hover_result) }
            },
        );
        router.request::<lsp_types::request::GotoDefinition, _>(
            |state, goto_definition_arguments: lsp_types::GotoDefinitionParams| {
                let response: Option<lsp_types::GotoDefinitionResponse> =
                    respond_to_goto_definition(state, goto_definition_arguments);
                async move { Ok(response) }
            },
        );
        router.request::<lsp_types::request::PrepareRenameRequest, _>(
            |state, prepare_rename_arguments: lsp_types::TextDocumentPositionParams| {
                let prepared: Option<
                    Result<lsp_types::PrepareRenameResponse, async_lsp::ResponseError>,
                > = respond_to_prepare_rename(state, prepare_rename_arguments);
                async move {
                    match prepared {
                        None => Ok(None),
                        Some(result) => result.map(Some),
                    }
                }
            },
        );
        router.request::<lsp_types::request::Rename, _>(
            |state, rename_arguments: lsp_types::RenameParams| {
                let maybe_rename_edits: Option<Vec<lsp_types::TextDocumentEdit>> =
                    respond_to_rename(state, rename_arguments);
                async move {
                    Ok(
                        maybe_rename_edits.map(|rename_edits| lsp_types::WorkspaceEdit {
                            changes: None,
                            document_changes: Some(lsp_types::DocumentChanges::Edits(rename_edits)),
                            change_annotations: None,
                        }),
                    )
                }
            },
        );
        router.request::<lsp_types::request::SemanticTokensFullRequest, _>(
            |state, semantic_tokens_arguments: lsp_types::SemanticTokensParams| {
                let semantic_tokens: Option<lsp_types::SemanticTokensResult> =
                    respond_to_semantic_tokens_full(state, semantic_tokens_arguments);
                async move { Ok(semantic_tokens) }
            },
        );
        router.request::<lsp_types::request::Completion, _>(
            |state, completion_arguments: lsp_types::CompletionParams| {
                let maybe_completions: Option<lsp_types::CompletionResponse> =
                    respond_to_completion(state, completion_arguments);
                async { Ok(maybe_completions) }
            },
        );
        router.request::<lsp_types::request::Shutdown, _>(|_state, ()| {
            // ?
            async { Ok(()) }
        });
        router.notification::<lsp_types::notification::Initialized>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        });
        router.notification::<lsp_types::notification::DidOpenTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        });
        router.notification::<lsp_types::notification::DidChangeTextDocument>(
            |state, did_change_text_document| {
                let maybe_changed_file_path: Option<std::path::PathBuf> = did_change_text_document
                    .text_document
                    .uri
                    .to_file_path()
                    .ok();
                if let Some(changed_file_path) = maybe_changed_file_path {
                    let maybe_changed_file_source = did_change_text_document
                        .content_changes
                        .into_iter()
                        .find_map(|change| {
                            // range: None, range_length: None marks full new document content
                            match (change.range, change.range_length) {
                                (None, None) => Some(change.text),
                                (Some(_), _) | (_, Some(_)) => None,
                            }
                        });
                    match maybe_changed_file_source {
                        None => {
                            // bug in client. full document should be sent
                        }
                        Some(changed_file_source) => {
                            state_update_source_at_path(
                                state,
                                &changed_file_path,
                                changed_file_source,
                            );
                        }
                    }
                }
                std::ops::ControlFlow::Continue(())
            },
        );
        router.notification::<lsp_types::notification::DidChangeWatchedFiles>(
            |state, did_change_watched_files| {
                for file_change_event in did_change_watched_files.changes {
                    match file_change_event.uri.to_file_path() {
                        Err(()) => {}
                        Ok(changed_file_path) => match file_change_event.typ {
                            lsp_types::FileChangeType::DELETED => {
                                'removing_module: for project_state in state.projects.values_mut() {
                                    if project_state.modules.contains_key(&changed_file_path) {
                                        project_state.modules.remove(&changed_file_path);
                                        break 'removing_module;
                                    }
                                }
                            }
                            lsp_types::FileChangeType::CREATED
                            | lsp_types::FileChangeType::CHANGED => {
                                match std::fs::read_to_string(&changed_file_path) {
                                    Err(_) => {}
                                    Ok(changed_file_source) => {
                                        state_update_source_at_path(
                                            state,
                                            &changed_file_path,
                                            changed_file_source,
                                        );
                                    }
                                }
                            }
                            unknown_file_change_type => {
                                eprintln!(
                                    "unknown file change type sent by LSP client: {:?}",
                                    unknown_file_change_type
                                )
                            }
                        },
                    }
                }
                std::ops::ControlFlow::Continue(())
            },
        );
        router.notification::<lsp_types::notification::DidSaveTextDocument>(
            |state, did_save_text_document_arguments| {
                update_state_and_publish_diagnostics_for_document(
                    state,
                    &did_save_text_document_arguments.text_document.uri,
                );
                std::ops::ControlFlow::Continue(())
            },
        );
        router.notification::<lsp_types::notification::DidCloseTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        });
        router.notification::<lsp_types::notification::Exit>(|_state, _| {
            // ?
            std::ops::ControlFlow::Continue(())
        });
        tower::ServiceBuilder::new()
            .layer(async_lsp::tracing::TracingLayer::default())
            .layer(async_lsp::server::LifecycleLayer::default())
            .layer(async_lsp::panic::CatchUnwindLayer::default())
            .layer(async_lsp::concurrency::ConcurrencyLayer::default())
            .layer(async_lsp::client_monitor::ClientProcessMonitorLayer::new(
                client,
            ))
            .service(router)
    });
    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );
    server.run_buffered(stdin, stdout).await.unwrap();
}

fn update_state_and_publish_diagnostics_for_document(
    state: &mut State,
    document_url: &lsp_types::Url,
) {
    if let Some(saved_project_module) = state_get_project_module_by_lsp_url(state, document_url) {
        match compute_diagnostics(
            saved_project_module.project_path,
            saved_project_module.project,
        ) {
            Ok(elm_make_errors) => {
                let saved_project_path_buf = saved_project_module.project_path.to_path_buf();
                let mut updated_diagnostics_to_publish = Vec::new();
                for elm_make_file_error in saved_project_module.project.modules.keys() {
                    // O(modules*errors), might be problematic in large projects
                    let maybe_new = elm_make_errors
                        .iter()
                        .find(|&file_error| &file_error.path == elm_make_file_error);
                    let maybe_updated_diagnostics = match maybe_new {
                        Some(new) => {
                            let diagnostics: Vec<lsp_types::Diagnostic> = new
                                .problems
                                .iter()
                                .map(elm_make_file_problem_to_diagnostic)
                                .collect::<Vec<_>>();
                            Some(diagnostics)
                        }
                        None => {
                            let was_error = saved_project_module
                                .project
                                .elm_make_errors
                                .iter()
                                .any(|file_error| &file_error.path == elm_make_file_error);
                            if was_error { Some(vec![]) } else { None }
                        }
                    };
                    if let Some(updated_diagnostics) = maybe_updated_diagnostics
                        && let Ok(url) = lsp_types::Url::from_file_path(elm_make_file_error)
                    {
                        updated_diagnostics_to_publish.push(lsp_types::PublishDiagnosticsParams {
                            uri: url,
                            diagnostics: updated_diagnostics,
                            version: None,
                        });
                    }
                }
                for updated_file_diagnostics_to_publish in updated_diagnostics_to_publish {
                    let _ = async_lsp::LanguageClient::publish_diagnostics(
                        &mut state.client_socket,
                        updated_file_diagnostics_to_publish,
                    );
                }
                if let Some(mut_saved_project_state) =
                    state.projects.get_mut(&saved_project_path_buf)
                {
                    mut_saved_project_state.elm_make_errors = elm_make_errors;
                }
            }
            Err(error) => {
                eprintln!("{error}");
            }
        }
    }
}

fn compute_diagnostics(
    project_path: &std::path::Path,
    project_state: &ProjectState,
) -> Result<Vec<ElmMakeFileCompileError>, String> {
    // if there is a better way, please open an issue <3
    let sink_path: &str = match std::env::consts::FAMILY {
        "windows" => "NUL",
        _ => "/dev/null",
    };
    let elm_make_process: std::process::Child = std::process::Command::new("elm")
            .args(
                std::iter::once("make")
                    .chain(
                        project_state
                            .modules
                            .keys()
                            .filter_map(|path| path.to_str()),
                    )
                    .chain(["--report", "json", "--output", sink_path]),
            )
            .current_dir(project_path)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn().map_err(|error| {
                format!(
                    "I tried to run elm make but it failed: {error}. Try installing elm via `npm install -g elm`."
                )
            })?;
    let elm_make_output: std::process::Output = elm_make_process
        .wait_with_output()
        .map_err(|error| format!("I wasn't able to read the output of elm make: {error}"))?;
    let elm_make_report: Vec<ElmMakeFileCompileError>;
    if elm_make_output.stderr.is_empty() {
        elm_make_report = vec![];
    } else {
        let elm_make_report_json: serde_json::Value =
            serde_json::from_slice(&elm_make_output.stderr).map_err(|parse_error| {
                format!("failed to parse elm make report json: {parse_error}")
            })?;
        elm_make_report = parse_elm_make_report(&elm_make_report_json)?;
    }
    Ok(elm_make_report)
}
#[derive(Debug)]
struct ElmMakeFileCompileError {
    path: String,
    problems: Vec<ElmMakeFileInternalCompileProblem>,
}
#[derive(Debug)]
struct ElmMakeFileInternalCompileProblem {
    title: String,
    range: lsp_types::Range,
    message_markdown: String,
}
#[derive(Debug, Clone, Copy)]
enum ElmMakeMessageSegment<'a> {
    Plain(&'a str),
    Colored {
        underline: bool,
        bold: bool,
        color: Option<&'a str>,
        text: &'a str,
    },
}

fn elm_make_message_segments_to_markdown(
    elm_make_message_segments: Vec<ElmMakeMessageSegment>,
) -> String {
    let mut builder: String = String::new();
    for elm_make_message_segment in elm_make_message_segments {
        match elm_make_message_segment {
            ElmMakeMessageSegment::Plain(text) => {
                builder.push_str(text);
            }
            ElmMakeMessageSegment::Colored {
                underline,
                bold,
                color: maybe_color,
                text,
            } => {
                // https://github.com/microsoft/vscode/issues/54272
                if let Some(_color) = maybe_color {
                    builder.push_str(text);
                } else {
                    if bold {
                        builder.push_str(&text.to_ascii_uppercase());
                    } else if underline {
                        builder.push_str(&text.to_ascii_uppercase());
                    } else {
                        // suspicious, would have expected ::Plain
                        builder.push_str(text);
                    }
                }
            }
        }
    }
    builder
}

fn parse_elm_make_report(json: &serde_json::Value) -> Result<Vec<ElmMakeFileCompileError>, String> {
    match json.get("type").and_then(serde_json::Value::as_str) {
        Some("compile-errors") => match json.get("errors") {
            Some(serde_json::Value::Array(file_error_jsons)) => file_error_jsons
                .into_iter()
                .map(|file_error_json| parse_elm_make_file_compile_error(file_error_json))
                .collect::<Result<Vec<_>, String>>(),
            _ => Err(format!("field errors must be array")),
        },
        Some(unknown_type) => Err(format!("unknown report type {unknown_type}")),
        None => Err(format!("report type must exist as a string")),
    }
}
fn parse_elm_make_file_compile_error(
    json: &serde_json::Value,
) -> Result<ElmMakeFileCompileError, String> {
    let path: &str = json
        .get("path")
        .and_then(serde_json::Value::as_str)
        .ok_or("report file path must be string")?;
    let problems: Vec<ElmMakeFileInternalCompileProblem> = json
        .get("problems")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| "field problems must exist as array")?
        .iter()
        .map(|problem_json| parse_elm_make_file_internal_compile_problem(problem_json))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ElmMakeFileCompileError {
        path: path.to_string(),
        problems: problems,
    })
}
fn parse_elm_make_file_internal_compile_problem(
    json: &serde_json::Value,
) -> Result<ElmMakeFileInternalCompileProblem, String> {
    let title: &str = json
        .get("title")
        .and_then(serde_json::Value::as_str)
        .ok_or("report file path must be string")?;
    let range: lsp_types::Range = json
        .get("region")
        .ok_or_else(|| "report file region must be string".to_string())
        .and_then(parse_elm_make_region_as_lsp_range)?;
    let message_segments: Vec<ElmMakeMessageSegment> = json
        .get("message")
        .and_then(serde_json::Value::as_array)
        .ok_or_else(|| "report file message must be an array".to_string())?
        .iter()
        .map(|segment_json| parse_elm_make_message_segment(segment_json))
        .collect::<Result<Vec<ElmMakeMessageSegment>, _>>()?;
    Ok(ElmMakeFileInternalCompileProblem {
        title: title.to_string(),
        range: range,
        message_markdown: elm_make_message_segments_to_markdown(message_segments),
    })
}
fn parse_elm_make_region_as_lsp_range(
    json: &serde_json::Value,
) -> Result<lsp_types::Range, String> {
    let start: lsp_types::Position = json
        .get("start")
        .ok_or_else(|| "file region must have start".to_string())
        .and_then(parse_elm_make_position_as_lsp_position)?;
    let end: lsp_types::Position = json
        .get("end")
        .ok_or_else(|| "file region must have end".to_string())
        .and_then(parse_elm_make_position_as_lsp_position)?;
    Ok(lsp_types::Range {
        start: start,
        end: end,
    })
}
fn parse_elm_make_position_as_lsp_position(
    json: &serde_json::Value,
) -> Result<lsp_types::Position, String> {
    let line_1_based: i64 = json
        .get("line")
        .and_then(serde_json::Value::as_i64)
        .ok_or_else(|| "file region line must be integer")?;
    let column_1_based: i64 = json
        .get("column")
        .and_then(serde_json::Value::as_i64)
        .ok_or_else(|| "file region column must be integer")?;
    Ok(lsp_types::Position {
        line: (line_1_based - 1) as u32,
        character: (column_1_based - 1) as u32,
    })
}
fn parse_elm_make_message_segment<'a>(
    json: &'a serde_json::Value,
) -> Result<ElmMakeMessageSegment<'a>, String> {
    match json {
        serde_json::Value::String(plain) => Ok(ElmMakeMessageSegment::Plain(plain)),
        serde_json::Value::Object(fields_json) => {
            let text: &str = fields_json
                .get("string")
                .and_then(serde_json::Value::as_str)
                .ok_or_else(|| format!("report file problem message segment string must be string, all fields: {json}"))?;
            let color: Option<&str> = fields_json.get("color").and_then(serde_json::Value::as_str);
            let underline: bool = fields_json
                .get("underline")
                .and_then(serde_json::Value::as_bool)
                .ok_or("report file problem message segment underline must be string")?;
            let bold: bool = fields_json
                .get("bold")
                .and_then(serde_json::Value::as_bool)
                .ok_or("report file problem message segment bold must be string")?;
            Ok(ElmMakeMessageSegment::Colored {
                underline: underline,
                bold: bold,
                color: color,
                text: text,
            })
        }
        _ => Err(format!(
            "unknown report file problem message segment {json}"
        )),
    }
}

fn initialize_state_for_workspace_directories_into(
    state: &mut State,
    initialize_arguments: lsp_types::InitializeParams,
) {
    let workspace_directory_paths = initialize_arguments
        .workspace_folders
        .iter()
        .flatten()
        .filter_map(|workspace_folder| workspace_folder.uri.to_file_path().ok());
    let elm_home_path: std::path::PathBuf = match std::env::var("ELM_HOME") {
        Ok(elm_home_path) => std::path::PathBuf::from(elm_home_path),
        Err(_) => {
            std::path::Path::join(
                &std::env::home_dir()
                    .unwrap_or_else(|| {
                        eprintln!(
                            "I could not find an elm home directory (expected to find $HOME/.elm or $ELM_HOME environment variable).
This directory has cached information about installed packages like elm/core and is therefore required by this language server.
Running `elm` commands should create that directory.
This language server from now assumes there exists a local .elm directory.
If that is where you actually put installed elm packages, make sure to set the $ELM_HOME environment variable
accordingly so that tools like the elm compiler and language server can find them."
                        );
                        std::env::current_dir().ok().unwrap_or_else(|| std::path::PathBuf::new())
                    }),
                ".elm",
            )
        }
    };
    let _modules_exposed_from_workspace_packages = initialize_state_for_projects_into(
        state,
        &elm_home_path,
        // improvement possibility: search for elm.json in subdirectories
        workspace_directory_paths,
    );
}

/// returns exposed module names and their origins
fn initialize_state_for_projects_into(
    state: &mut State,
    elm_home_path: &std::path::PathBuf,
    project_paths: impl Iterator<Item = std::path::PathBuf>,
) -> std::collections::HashMap<String, ProjectModuleOrigin> {
    let mut dependency_exposed_module_names: std::collections::HashMap<
        String,
        ProjectModuleOrigin,
    > = std::collections::HashMap::new();
    for project_path in project_paths {
        initialize_state_for_project_into(
            state,
            &mut dependency_exposed_module_names,
            elm_home_path,
            project_path,
        );
    }
    dependency_exposed_module_names
}
fn initialize_state_for_project_into(
    state: &mut State,
    dependency_exposed_module_names_so_far: &mut std::collections::HashMap<
        String,
        ProjectModuleOrigin,
    >,
    elm_home_path: &std::path::PathBuf,
    project_path: std::path::PathBuf,
) {
    let elm_json_path: std::path::PathBuf = std::path::Path::join(&project_path, "elm.json");
    let maybe_elm_json_value: Option<serde_json::Value> = std::fs::read_to_string(&elm_json_path)
        .map_err(|io_error| {
            eprintln!("I couldn't read this elm.json file at {elm_json_path:?}: {io_error}")
        })
        .ok()
        .and_then(|elm_json_source| {
            serde_json::from_str(&elm_json_source)
                .map_err(|json_parse_error: serde_json::Error| {
                    eprintln!("I couldn't read this elm.json as JSON: {json_parse_error}")
                })
                .ok()
        });
    let maybe_elm_json: Option<ElmJson> =
        maybe_elm_json_value.as_ref().and_then(|elm_json_value| {
            parse_elm_json(elm_json_value)
                .map_err(|json_decode_error| {
                    eprintln!("I couldn't understand this elm.json: {}", json_decode_error)
                })
                .ok()
        });
    if maybe_elm_json.is_none() {
        eprintln!(
            "no valid elm.json found. Now looking for elm module files across the workspace and elm/core 1.0.5"
        );
    }
    let elm_json_source_directories: Vec<std::path::PathBuf> = match &maybe_elm_json {
        None => {
            vec![project_path.clone()]
        }
        Some(ElmJson::Application {
            source_directories,
            direct_dependencies: _,
        }) => source_directories
            .into_iter()
            .map(|elm_string| std::path::Path::join(&project_path, elm_string.to_string()))
            .collect::<Vec<_>>(),
        Some(ElmJson::Package { .. }) => {
            vec![std::path::Path::join(&project_path, "src")]
        }
    };
    let direct_dependencies: Box<dyn Iterator<Item = (&str, &str)>> = match &maybe_elm_json {
        None => Box::new(std::iter::once(("elm/core", "1.0.5"))),
        Some(ElmJson::Application {
            direct_dependencies,
            source_directories: _,
        }) => Box::new(direct_dependencies.iter().map(|(n, v)| (*n, *v))),
        Some(ElmJson::Package {
            dependency_minimum_versions,
            exposed_modules: _,
        }) => Box::new(dependency_minimum_versions.iter().map(|(n, v)| (*n, *v))),
    };
    let elm_source_files = elm_json_source_directories
        .iter()
        .filter_map(|source_directory_path| {
            list_elm_files_in_source_directory_at_path(source_directory_path).ok()
        })
        .flatten();
    let mut module_states: std::collections::HashMap<std::path::PathBuf, ModuleState> =
        std::collections::HashMap::new();
    for (module_path, module_source) in elm_source_files {
        module_states.insert(
            module_path,
            ModuleState {
                syntax: parse_elm_syntax_module(&module_source),
            },
        );
    }
    match &maybe_elm_json {
        None => {}
        Some(ElmJson::Application { .. }) => {}
        Some(ElmJson::Package {
            exposed_modules,
            dependency_minimum_versions: _,
        }) => {
            for exposed_module_name in exposed_modules {
                let maybe_module_origin_path: Option<&std::path::PathBuf> = module_states
                    .iter()
                    .find_map(|(module_path, module_state)| {
                        if module_state
                            .syntax
                            .header
                            .as_ref()
                            .is_some_and(|header_node| {
                                header_node
                                    .module_name
                                    .as_ref()
                                    .is_some_and(|module_name_node| {
                                        &module_name_node.value == exposed_module_name
                                    })
                            })
                        {
                            Some(module_path)
                        } else {
                            None
                        }
                    });
                match maybe_module_origin_path {
                    None => {}
                    Some(module_origin_path) => {
                        dependency_exposed_module_names_so_far.insert(
                            exposed_module_name.to_string(),
                            ProjectModuleOrigin {
                                project_path: project_path.clone(),
                                module_path: module_origin_path.clone(),
                            },
                        );
                    }
                }
            }
        }
    }
    let dependency_exposed_module_names = initialize_state_for_projects_into(
        state,
        elm_home_path,
        direct_dependencies.map(|(package_name, package_version)| {
            std::path::Path::join(
                &elm_home_path,
                format!("0.19.1/packages/{package_name}/{package_version}"),
            )
        }),
    );
    state.projects.insert(
        project_path,
        ProjectState {
            source_directories: elm_json_source_directories,
            modules: module_states,
            dependency_exposed_module_names,
            elm_make_errors: vec![],
        },
    );
}
enum ElmJson<'a> {
    Application {
        source_directories: Vec<&'a str>,
        direct_dependencies: std::collections::HashMap<&'a str, &'a str>,
    },
    Package {
        dependency_minimum_versions: std::collections::HashMap<&'a str, &'a str>,
        exposed_modules: Vec<&'a str>,
    },
}

fn parse_elm_json<'a>(json: &'a serde_json::Value) -> Result<ElmJson<'a>, String> {
    let json_object: &serde_json::Map<String, serde_json::Value> = match json {
        serde_json::Value::Object(json_object) => Ok(json_object),
        _ => Err("must be an object".to_string()),
    }?;
    match json_object.get("type") {
        Some(serde_json::Value::String(type_string)) => match type_string.as_str() {
            "application" => {
                let direct_dependencies: std::collections::HashMap<&str, &str> = match json_object
                    .get("dependencies")
                {
                    Some(serde_json::Value::Object(dependencies)) => {
                        match dependencies.get("direct") {
                            Some(serde_json::Value::Object(direct_dependencies_json)) => {
                                let mut direct_dependencies: std::collections::HashMap<&str, &str> =
                                    std::collections::HashMap::new();
                                for (direct_dependency_name, direct_dependency_version_json) in
                                    direct_dependencies_json
                                {
                                    let direct_dependency_version: &str =
                                        match direct_dependency_version_json {
                                            serde_json::Value::String(v) => Ok(v.as_str()),
                                            _ => Err(format!(
                                                "{direct_dependency_name} dependency version must be a string"
                                            )),
                                        }?;
                                    direct_dependencies.insert(
                                        direct_dependency_name.as_str(),
                                        direct_dependency_version,
                                    );
                                }
                                Ok::<std::collections::HashMap<&str, &str>, String>(
                                    direct_dependencies,
                                )
                            }
                            _ => Err("must have field direct in dependencies".to_string()),
                        }
                    }
                    _ => Err("must have field dependencies".to_string()),
                }?;
                let mut source_directories: Vec<&str> = Vec::new();
                match json_object.get("source-directories") {
                    Some(serde_json::Value::Array(source_directories_json)) => {
                        for source_directory_json in source_directories_json {
                            match source_directory_json {
                                serde_json::Value::String(source_directory) => {
                                    source_directories.push(source_directory)
                                }
                                _ => {
                                    return Err(
                                        "source directories must be all strings".to_string()
                                    );
                                }
                            }
                        }
                    }
                    _ => return Err("must have field source-directories".to_string()),
                }
                Ok(ElmJson::Application {
                    source_directories: source_directories,
                    direct_dependencies: direct_dependencies,
                })
            }
            "package" => {
                let dependency_minimum_versions: std::collections::HashMap<&str, &str> =
                    match json_object.get("dependencies") {
                        Some(serde_json::Value::Object(dependencies)) => {
                            let mut dependency_minimum_versions: std::collections::HashMap<
                                &str,
                                &str,
                            > = std::collections::HashMap::new();
                            for (direct_dependency_name, direct_dependency_version_json) in
                                dependencies
                            {
                                let dependency_version_constraint: &str =
                                    match direct_dependency_version_json {
                                        serde_json::Value::String(v) => Ok(v.as_str()),
                                        _ => Err(format!(
                                            "{direct_dependency_name} dependency version must be a string"
                                        )),
                                    }?;
                                let dependency_version_minimum: &str =
                                    elm_json_version_constraint_to_minimum_version(
                                        dependency_version_constraint,
                                    )?;
                                dependency_minimum_versions.insert(
                                    direct_dependency_name.as_str(),
                                    dependency_version_minimum,
                                );
                            }
                            Ok::<std::collections::HashMap<&str, &str>, String>(
                                dependency_minimum_versions,
                            )
                        }
                        _ => Err("must have field dependencies".to_string()),
                    }?;
                let mut exposed_modules: Vec<&str> = Vec::new();
                match json_object.get("exposed-modules") {
                    Some(serde_json::Value::Array(source_directories_json)) => {
                        for source_directory_json in source_directories_json {
                            match source_directory_json {
                                serde_json::Value::String(source_directory) => {
                                    exposed_modules.push(source_directory)
                                }
                                _ => {
                                    return Err("exposed modules must be all strings".to_string());
                                }
                            }
                        }
                    }
                    Some(serde_json::Value::Object(grouped)) => {
                        for group_values in grouped.values() {
                            match group_values {
                                serde_json::Value::Array(source_directories_json) => {
                                    for source_directory_json in source_directories_json {
                                        match source_directory_json {
                                            serde_json::Value::String(source_directory) => {
                                                exposed_modules.push(source_directory)
                                            }
                                            _ => {
                                                return Err("exposed modules must be all strings"
                                                    .to_string());
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    return Err("exposed module group must be an array".to_string());
                                }
                            }
                        }
                    }
                    _ => return Err("must have field exposed-modules".to_string()),
                }
                Ok(ElmJson::Package {
                    dependency_minimum_versions,
                    exposed_modules: exposed_modules,
                })
            }
            _ => Err("field type must be package or application".to_string()),
        },
        _ => Err("must have field type".to_string()),
    }
}
fn elm_json_version_constraint_to_minimum_version<'a>(
    elm_json_version_constraint: &'a str,
) -> Result<&'a str, String> {
    match elm_json_version_constraint.split_once(" <= v < ") {
        None => Err(format!(
            "dependency version constraints must be set in the form lo <= v < hi, found {elm_json_version_constraint}"
        )),
        Some((minimum_version, _maximum_version)) => {
            if !minimum_version
                .chars()
                .all(|c| c.is_ascii_digit() || c == '.')
            {
                Err(format!(
                    "dependency version constraint minimum version must only be composed of digits and .s, found: {minimum_version}"
                ))
            } else {
                Ok(minimum_version)
            }
        }
    }
}

fn project_state_get_module_with_name<'a>(
    state: &'a State,
    project_state: &'a ProjectState,
    module_name: &str,
) -> Option<(&'a std::path::PathBuf, &'a ModuleState)> {
    match project_state
        .dependency_exposed_module_names
        .get(module_name)
    {
        Some(dependency_module_origin) => state
            .projects
            .get(&dependency_module_origin.project_path)
            .and_then(|dependency| {
                dependency
                    .modules
                    .get_key_value(&dependency_module_origin.module_path)
            }),
        None => project_state
            .modules
            .iter()
            .find_map(|(module_path, module_state)| {
                if module_state
                    .syntax
                    .header
                    .as_ref()
                    .is_some_and(|header_node| {
                        header_node
                            .module_name
                            .as_ref()
                            .is_some_and(|module_name_node| &module_name_node.value == &module_name)
                    })
                {
                    Some((module_path, module_state))
                } else {
                    None
                }
            }),
    }
}
#[derive(Clone, Copy)]
struct ProjectModuleState<'a> {
    project_path: &'a std::path::Path,
    project: &'a ProjectState,
    module: &'a ModuleState,
}

fn state_get_project_module_by_lsp_url<'a>(
    state: &'a State,
    uri: &lsp_types::Url,
) -> Option<ProjectModuleState<'a>> {
    let file_path: std::path::PathBuf = uri.to_file_path().ok()?;
    state
        .projects
        .iter()
        .find_map(|(project_path, project_state)| {
            let module_state = project_state.modules.get(&file_path)?;
            Some(ProjectModuleState {
                project_path: project_path,
                project: project_state,
                module: module_state,
            })
        })
}

fn respond_to_hover(
    state: &State,
    hover_arguments: lsp_types::HoverParams,
) -> Option<lsp_types::Hover> {
    let hovered_project_module_state = state_get_project_module_by_lsp_url(
        state,
        &hover_arguments
            .text_document_position_params
            .text_document
            .uri,
    )?;
    let hovered_symbol_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &hovered_project_module_state.module.syntax,
            hover_arguments.text_document_position_params.position,
        )?;
    match hovered_symbol_node.value {
        ElmSyntaxSymbol::TypeVariable { .. } => None,
        ElmSyntaxSymbol::ModuleName(hovered_module_name)
        | ElmSyntaxSymbol::ImportAlias {
            module_origin: hovered_module_name,
            alias_name: _,
        } => {
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                hovered_project_module_state.project,
                hovered_module_name,
            )?;
            // also show list of exports maybe?
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: match origin_module_state.syntax.documentation {
                        None => "_module has no documentation comment_".to_string(),
                        Some(ref module_documentation) => {
                            documentation_comment_to_markdown(&module_documentation.value)
                        }
                    },
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name: hovered_name,
            all_exposes: _,
        } => {
            let hovered_module_origin: &str = hovered_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str())
                .unwrap_or("");
            let origin_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                );
            let origin_declaration_info_markdown: String = hovered_project_module_state
                .module
                .syntax
                .declarations
                .iter()
                .find_map(|documented_declaration| {
                    let declaration_node = documented_declaration.declaration.as_ref()?;
                    match &declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0_name: origin_module_declaration_variant0_name_node,
                            variant0_values: origin_module_declaration_variant0_values,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if origin_module_declaration_name
                                .as_ref()
                                .map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    origin_module_declaration_name
                                        .as_ref()
                                        .map(|node| node.value.as_str())
                                        .unwrap_or(""),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_variant0_values,
                                    &origin_module_declaration_variant1_up,
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            direction: maybe_declaration_direction,
                            precedence: maybe_declaration_precedence,
                            operator: maybe_declaration_operator,
                            equals_key_symbol_range: _,
                            function: maybe_declaration_function,
                        } => {
                            if maybe_declaration_operator.as_ref().map(|node| node.value)
                                == Some(hovered_name)
                            {
                                let maybe_origin_operator_function_declaration =
                                    maybe_declaration_function.as_ref().and_then(
                                        |origin_module_declaration_function_node| {
                                            hovered_project_module_state
                                                .module
                                                .syntax
                                                .declarations
                                                .iter()
                                                .find_map(|origin_module_declaration| {
                                                    let origin_module_declaration_node =
                                                        origin_module_declaration
                                                            .declaration
                                                            .as_ref()?;
                                                    match &origin_module_declaration_node.value {
                                                ElmSyntaxDeclaration::Variable {
                                                    start_name: origin_module_declaration_name,
                                                    signature: origin_module_declaration_signature,
                                                    parameters: _,
                                                    equals_key_symbol_range: _,
                                                    result: _,
                                                } if &origin_module_declaration_name.value
                                                    == &origin_module_declaration_function_node
                                                        .value =>
                                                {
                                                    Some((
                                                        origin_module_declaration_signature
                                                            .as_ref(),
                                                        origin_module_declaration
                                                            .documentation
                                                            .as_ref()
                                                            .map(|node| node.value.as_str()),
                                                    ))
                                                }
                                                _ => None,
                                            }
                                                })
                                        },
                                    );
                                Some(present_operator_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    maybe_declaration_operator.as_ref().map(|node| node.value),
                                    maybe_origin_operator_function_declaration,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    maybe_declaration_precedence.map(|node| node.value),
                                    maybe_declaration_direction.map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if declaration_name.as_ref().map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    &hovered_module_origin,
                                    hovered_name,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if declaration_name.as_ref().map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    hovered_name,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable {
                            start_name: declaration_name_node,
                            signature: declaration_maybe_signature,
                            parameters: _,
                            equals_key_symbol_range: _,
                            result: _,
                        } => {
                            if &declaration_name_node.value == (hovered_name) {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &declaration_name_node.value,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    declaration_maybe_signature.as_ref().and_then(|signature| {
                                        signature.type_.as_ref().map(|node| &node.value)
                                    }),
                                ))
                            } else {
                                None
                            }
                        }
                    }
                })?;
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: origin_declaration_info_markdown,
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
        ElmSyntaxSymbol::ModuleMemberDeclarationName {
            name: hovered_declaration_name,
            documentation,
            declaration,
        } => {
            let hovered_module_origin: &str = hovered_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str())
                .unwrap_or("");
            let origin_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                );
            let origin_declaration_info_markdown: String = match &declaration.value {
                ElmSyntaxDeclaration::ChoiceType {
                    name: origin_module_declaration_name,
                    parameters: origin_module_declaration_parameters,
                    equals_key_symbol_range: _,
                    variant0_name: origin_module_declaration_variant0_name_node,
                    variant0_values: origin_module_declaration_variant0_values,
                    variant1_up: origin_module_declaration_variant1_up,
                } => {
                    format!(
                        "{}{}",
                        if Some(hovered_declaration_name)
                            == origin_module_declaration_name
                                .as_ref()
                                .map(|node| node.value.as_str())
                        {
                            ""
                        } else {
                            "variant in\n"
                        },
                        &present_choice_type_declaration_info_markdown(
                            &origin_module_origin_lookup,
                            hovered_module_origin,
                            origin_module_declaration_name
                                .as_ref()
                                .map(|node| node.value.as_str())
                                .unwrap_or(""),
                            documentation,
                            &origin_module_declaration_parameters,
                            origin_module_declaration_variant0_name_node
                                .as_ref()
                                .map(|node| node.value.as_str()),
                            &origin_module_declaration_variant0_values,
                            &origin_module_declaration_variant1_up,
                        )
                    )
                }
                ElmSyntaxDeclaration::Operator {
                    direction: maybe_origin_module_declaration_direction,
                    precedence: maybe_origin_module_declaration_precedence,
                    operator: maybe_origin_module_declaration_operator,
                    equals_key_symbol_range: _,
                    function: maybe_origin_module_declaration_function,
                } => {
                    let maybe_origin_operator_function_declaration =
                        maybe_origin_module_declaration_function.as_ref().and_then(
                            |origin_module_declaration_function_node| {
                                hovered_project_module_state
                                    .module
                                    .syntax
                                    .declarations
                                    .iter()
                                    .find_map(|origin_module_declaration| {
                                        let origin_module_declaration_node =
                                            origin_module_declaration.declaration.as_ref()?;
                                        match &origin_module_declaration_node.value {
                                            ElmSyntaxDeclaration::Variable {
                                                start_name: origin_module_declaration_name,
                                                signature: origin_module_declaration_signature,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } if &origin_module_declaration_name.value
                                                == &origin_module_declaration_function_node
                                                    .value =>
                                            {
                                                Some((
                                                    origin_module_declaration_signature.as_ref(),
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref()
                                                        .map(|node| node.value.as_str()),
                                                ))
                                            }
                                            _ => None,
                                        }
                                    })
                            },
                        );
                    present_operator_declaration_info_markdown(
                        &origin_module_origin_lookup,
                        hovered_module_origin,
                        maybe_origin_module_declaration_operator
                            .as_ref()
                            .map(|node| node.value),
                        maybe_origin_operator_function_declaration,
                        documentation,
                        maybe_origin_module_declaration_precedence.map(|node| node.value),
                        maybe_origin_module_declaration_direction.map(|node| node.value),
                    )
                }
                ElmSyntaxDeclaration::Port {
                    name: _,
                    colon_key_symbol_range: _,
                    type_,
                } => present_port_declaration_info_markdown(
                    &origin_module_origin_lookup,
                    &hovered_module_origin,
                    hovered_declaration_name,
                    documentation,
                    type_.as_ref().map(|node| &node.value),
                ),
                ElmSyntaxDeclaration::TypeAlias {
                    alias_keyword_range: _,
                    name: _,
                    parameters: origin_module_declaration_parameters,
                    equals_key_symbol_range: _,
                    type_,
                } => present_type_alias_declaration_info_markdown(
                    &origin_module_origin_lookup,
                    hovered_module_origin,
                    hovered_declaration_name,
                    documentation,
                    &origin_module_declaration_parameters,
                    type_.as_ref().map(|node| &node.value),
                ),
                ElmSyntaxDeclaration::Variable {
                    start_name: origin_module_declaration_name_node,
                    signature: origin_module_declaration_maybe_signature,
                    parameters: _,
                    equals_key_symbol_range: _,
                    result: _,
                } => present_variable_declaration_info_markdown(
                    &origin_module_origin_lookup,
                    hovered_module_origin,
                    &origin_module_declaration_name_node.value,
                    documentation,
                    origin_module_declaration_maybe_signature
                        .as_ref()
                        .and_then(|signature| signature.type_.as_ref().map(|node| &node.value)),
                ),
            };
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: origin_declaration_info_markdown,
                }),
                range: Some(hovered_symbol_node.range),
            })
        }

        ElmSyntaxSymbol::ImportExpose {
            name: hovered_name,
            origin_module: hovered_expose_origin_module,
            all_exposes: _,
        } => {
            let hovered_module_origin: &str = hovered_expose_origin_module;
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                hovered_project_module_state.project,
                hovered_module_origin,
            )?;
            let origin_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &origin_module_state.syntax,
                );
            let origin_declaration_info_markdown: String = origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|documented_declaration| {
                    let declaration_node = documented_declaration.declaration.as_ref()?;
                    match &declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0_name: origin_module_declaration_variant0_name_node,
                            variant0_values: origin_module_declaration_variant0_values,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if origin_module_declaration_name
                                .as_ref()
                                .map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    origin_module_declaration_name
                                        .as_ref()
                                        .map(|node| node.value.as_str())
                                        .unwrap_or(""),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_variant0_values,
                                    &origin_module_declaration_variant1_up,
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            direction: maybe_declaration_direction,
                            precedence: maybe_declaration_precedence,
                            operator: maybe_declaration_operator,
                            equals_key_symbol_range: _,
                            function: maybe_declaration_function,
                        } => {
                            if maybe_declaration_operator.as_ref().map(|node| node.value)
                                == Some(hovered_name)
                            {
                                let maybe_origin_operator_function_declaration =
                                    maybe_declaration_function.as_ref().and_then(
                                        |origin_module_declaration_function_node| {
                                            origin_module_state.syntax.declarations.iter().find_map(
                                                |origin_module_declaration| {
                                                    let origin_module_declaration_node =
                                                        origin_module_declaration
                                                            .declaration
                                                            .as_ref()?;
                                                    match &origin_module_declaration_node.value {
                                                ElmSyntaxDeclaration::Variable {
                                                    start_name: origin_module_declaration_name,
                                                    signature: origin_module_declaration_signature,
                                                    parameters: _,
                                                    equals_key_symbol_range: _,
                                                    result: _,
                                                } if &origin_module_declaration_name.value
                                                    == &origin_module_declaration_function_node
                                                        .value =>
                                                {
                                                    Some((
                                                        origin_module_declaration_signature
                                                            .as_ref(),
                                                        origin_module_declaration
                                                            .documentation
                                                            .as_ref()
                                                            .map(|node| node.value.as_str()),
                                                    ))
                                                }
                                                _ => None,
                                            }
                                                },
                                            )
                                        },
                                    );
                                Some(present_operator_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    maybe_declaration_operator.as_ref().map(|node| node.value),
                                    maybe_origin_operator_function_declaration,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    maybe_declaration_precedence.map(|node| node.value),
                                    maybe_declaration_direction.map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if declaration_name.as_ref().map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    &hovered_module_origin,
                                    hovered_name,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if declaration_name.as_ref().map(|node| node.value.as_str())
                                == Some(hovered_name)
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    hovered_name,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable {
                            start_name: declaration_name_node,
                            signature: declaration_maybe_signature,
                            parameters: _,
                            equals_key_symbol_range: _,
                            result: _,
                        } => {
                            if &declaration_name_node.value == (hovered_name) {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &declaration_name_node.value,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    declaration_maybe_signature.as_ref().and_then(|signature| {
                                        signature.type_.as_ref().map(|node| &node.value)
                                    }),
                                ))
                            } else {
                                None
                            }
                        }
                    }
                })?;
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: origin_declaration_info_markdown,
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification: hovered_qualification,
            name: hovered_name,
            local_bindings,
        } => {
            if hovered_qualification.is_empty()
                && let Some((hovered_local_binding_origin, _)) =
                    find_local_binding_scope_expression(&local_bindings, hovered_name)
            {
                return Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: local_binding_info_markdown(
                            state,
                            hovered_project_module_state,
                            hovered_name,
                            hovered_local_binding_origin,
                        ),
                    }),
                    range: Some(hovered_symbol_node.range),
                });
            }
            let hovered_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                ),
                hovered_qualification,
                hovered_name,
            );
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                hovered_project_module_state.project,
                hovered_module_origin,
            )?;
            let origin_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &origin_module_state.syntax,
                );
            let origin_declaration_info_markdown: String =
                origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    let origin_module_declaration_node = origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0_name: origin_module_declaration_variant0_name_node,
                            variant0_values: origin_module_declaration_variant0_values,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            let any_declared_name_matches_hovered: bool =
                                (origin_module_declaration_variant0_name_node
                                    .as_ref()
                                    .is_some_and(|name_node| name_node.value == hovered_name))
                                    || (origin_module_declaration_variant1_up.iter().any(
                                        |variant| {
                                            variant.name.as_ref().is_some_and(|name_node| {
                                                &name_node.value == hovered_name
                                            })
                                        },
                                    ));
                            if any_declared_name_matches_hovered {
                                Some(format!(
                                    "variant in\n{}",
                                    &present_choice_type_declaration_info_markdown(
                                        &origin_module_origin_lookup,
                                        hovered_module_origin,
                                        origin_module_declaration_name
                                            .as_ref()
                                            .map(|node| node.value.as_str())
                                            .unwrap_or(""),
                                        origin_module_declaration
                                            .documentation
                                            .as_ref()
                                            .map(|node| node.value.as_str()),
                                        &origin_module_declaration_parameters,
                                        origin_module_declaration_variant0_name_node
                                            .as_ref()
                                            .map(|node| node.value.as_str()),
                                        &origin_module_declaration_variant0_values,
                                        &origin_module_declaration_variant1_up,
                                    )
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            direction: maybe_origin_module_declaration_direction,
                            precedence: maybe_origin_module_declaration_precedence,
                            operator: maybe_origin_module_declaration_operator,
                            equals_key_symbol_range: _,
                            function: maybe_origin_module_declaration_function,
                        } => {
                            if maybe_origin_module_declaration_operator
                                .as_ref()
                                .is_some_and(|operator_node| operator_node.value == hovered_name)
                            {
                                let maybe_origin_operator_function_declaration =
                                    maybe_origin_module_declaration_function.as_ref().and_then(
                                        |origin_module_declaration_function_node| {
                                            origin_module_state.syntax.declarations.iter().find_map(
                                                |origin_module_declaration| {
                                                    let origin_module_declaration_node = origin_module_declaration.declaration.as_ref()?;
                                                    match &origin_module_declaration_node.value {
                                                        ElmSyntaxDeclaration::Variable {
                                                            start_name: origin_module_declaration_name,
                                                            signature: origin_module_declaration_signature,
                                                            ..
                                                        } if &origin_module_declaration_name.value
                                                            == &origin_module_declaration_function_node.value => {
                                                            Some((
                                                                origin_module_declaration_signature.as_ref(),
                                                                origin_module_declaration
                                                                    .documentation
                                                                    .as_ref()
                                                                    .map(|node| node.value.as_str()),
                                                            ))
                                                        }
                                                        _ => None,
                                                    }
                                                })
                                            },
                                        );
                                Some(present_operator_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    maybe_origin_module_declaration_operator
                                        .as_ref()
                                        .map(|node| node.value),
                                    maybe_origin_operator_function_declaration,
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    maybe_origin_module_declaration_precedence
                                        .map(|node| node.value),
                                    maybe_origin_module_declaration_direction
                                        .map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: origin_module_declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if origin_module_declaration_name.as_ref().map(|node| node.value.as_str()) == Some(hovered_name) {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    &hovered_module_origin,
                                    hovered_name,
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: maybe_origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if maybe_origin_module_declaration_name
                                .as_ref()
                                .is_some_and(|name_node| &name_node.value == hovered_name)
                            {
                                Some(format!(
                                    "constructor function for record\n{}",
                                    &present_type_alias_declaration_info_markdown(
                                        &origin_module_origin_lookup,
                                        hovered_module_origin,
                                        hovered_name,
                                        origin_module_declaration
                                            .documentation
                                            .as_ref()
                                            .map(|node| node.value.as_str()),
                                        &origin_module_declaration_parameters,
                                        type_.as_ref().map(|node| &node.value)
                                    )
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable {
                            start_name: origin_module_declaration_name_node,
                            signature: origin_module_declaration_maybe_signature,
                            parameters: _,
                            equals_key_symbol_range: _,
                            result: _,
                        } => {
                            if &origin_module_declaration_name_node.value == hovered_name {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &origin_module_declaration_name_node.value,
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    origin_module_declaration_maybe_signature
                                        .as_ref()
                                        .and_then(|signature| {
                                            signature
                                                .type_
                                                .as_ref()
                                                .map(|node| &node.value)
                                        }),
                                ))
                            } else {
                                None
                            }
                        }
                    }
                })?;
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: origin_declaration_info_markdown,
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
        ElmSyntaxSymbol::Type {
            qualification: hovered_qualification,
            name: hovered_name,
        } => {
            let hovered_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                ),
                hovered_qualification,
                hovered_name,
            );
            if (hovered_module_origin == "List") && (hovered_name == "List") {
                // module List has no type List.List exposed in an oversight so we make one up.
                // See https://github.com/elm/core/issues/1037
                return Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: list_list_type_info_markdown.to_string(),
                    }),
                    range: Some(hovered_symbol_node.range),
                });
            }
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                hovered_project_module_state.project,
                hovered_module_origin,
            )?;
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                hovered_project_module_state.project,
                &origin_module_state.syntax,
            );
            let info_markdown: String = origin_module_state.syntax.declarations.iter().find_map(
                |origin_module_declaration| {
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0_name: maybe_origin_module_declaration_variant0_name_node,
                            variant0_values: maybe_origin_module_declaration_variant0_values,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if origin_module_declaration_name
                                .as_ref()
                                .is_some_and(|name_node| &name_node.value == hovered_name)
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    hovered_module_origin,
                                    hovered_name,
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    maybe_origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &maybe_origin_module_declaration_variant0_values,
                                    &origin_module_declaration_variant1_up,
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator { .. } => None,
                        ElmSyntaxDeclaration::Port { .. } => None,
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if origin_module_declaration_name
                                .as_ref()
                                .is_some_and(|name_node| &name_node.value == hovered_name)
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &module_origin_lookup,
                                    hovered_module_origin,
                                    hovered_name,
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                    &origin_module_declaration_parameters,
                                    type_.as_ref().map(|node| &node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable { .. } => None,
                    }
                },
            )?;
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: info_markdown,
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
    }
}

fn local_binding_info_markdown(
    state: &State,
    hovered_project_module_state: ProjectModuleState,
    hovered_name: &str,
    hovered_local_binding_origin: LocalBindingOrigin,
) -> String {
    match hovered_local_binding_origin {
        LocalBindingOrigin::PatternVariable(_) => format!("variable introduced in pattern"),
        LocalBindingOrigin::PatternRecordField(_) => {
            format!("variable bound to a field, introduced in a pattern")
        }
        LocalBindingOrigin::LetDeclaredVariable {
            signature_type: hovered_local_binding_maybe_signature_type,
            start_name_range: _,
        } => match hovered_local_binding_maybe_signature_type {
            None => {
                format!("```elm\nlet {}\n```\n", hovered_name)
            }
            Some(hovered_local_binding_signature) => {
                format!(
                    "```elm\nlet {} : {}\n```\n",
                    hovered_name,
                    &elm_syntax_type_to_single_line_string(
                        &elm_syntax_module_create_origin_lookup(
                            state,
                            hovered_project_module_state.project,
                            &hovered_project_module_state.module.syntax
                        ),
                        hovered_local_binding_signature,
                    )
                )
            }
        },
    }
}

fn respond_to_goto_definition(
    state: &State,
    goto_definition_arguments: lsp_types::GotoDefinitionParams,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let goto_project_module_state = state_get_project_module_by_lsp_url(
        state,
        &goto_definition_arguments
            .text_document_position_params
            .text_document
            .uri,
    )?;
    let goto_symbol_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &goto_project_module_state.module.syntax,
            goto_definition_arguments
                .text_document_position_params
                .position,
        )?;
    match goto_symbol_node.value {
        ElmSyntaxSymbol::ModuleMemberDeclarationName { .. } => {
            // already at declaration
            None
        }
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration,
            name: goto_type_variable_name,
        } => {
            match scope_declaration {
                ElmSyntaxDeclaration::ChoiceType {
                    name: _,
                    parameters: origin_type_parameters,
                    equals_key_symbol_range: _,
                    variant0_name: _,
                    variant0_values: _,
                    variant1_up: _,
                } => {
                    let goto_type_variable_name_origin_parameter_node = origin_type_parameters
                        .iter()
                        .find(|origin_choice_type_parameter| {
                            &origin_choice_type_parameter.value == goto_type_variable_name
                        })?;
                    Some(lsp_types::GotoDefinitionResponse::Scalar(
                        lsp_types::Location {
                            uri: goto_definition_arguments
                                .text_document_position_params
                                .text_document
                                .uri,
                            range: goto_type_variable_name_origin_parameter_node.range,
                        },
                    ))
                }
                ElmSyntaxDeclaration::Operator { .. } => None,
                ElmSyntaxDeclaration::Port { .. } => None,
                ElmSyntaxDeclaration::TypeAlias {
                    alias_keyword_range: _,
                    name: _,
                    parameters: origin_type_parameters,
                    equals_key_symbol_range: _,
                    type_: _,
                } => {
                    let goto_type_variable_name_origin_parameter_node = origin_type_parameters
                        .iter()
                        .find(|origin_choice_type_parameter| {
                            &origin_choice_type_parameter.value == goto_type_variable_name
                        })?;
                    Some(lsp_types::GotoDefinitionResponse::Scalar(
                        lsp_types::Location {
                            uri: goto_definition_arguments
                                .text_document_position_params
                                .text_document
                                .uri,
                            range: goto_type_variable_name_origin_parameter_node.range,
                        },
                    ))
                }
                ElmSyntaxDeclaration::Variable { .. } => None,
            }
        }
        ElmSyntaxSymbol::ModuleName(goto_module_name)
        | ElmSyntaxSymbol::ImportAlias {
            module_origin: goto_module_name,
            alias_name: _,
        } => {
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_project_module_state.project,
                    goto_module_name,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: match origin_module_state.syntax.header {
                        Some(ref module_header) => match module_header.module_name {
                            Some(ref module_name_node) => module_name_node.range,
                            None => match module_header.specific {
                                ElmSyntaxModuleHeaderSpecific::Pure {
                                    module_keyword_range,
                                } => module_keyword_range,
                                ElmSyntaxModuleHeaderSpecific::Port {
                                    port_keyword_range: _,
                                    module_keyword_range,
                                } => module_keyword_range,
                                ElmSyntaxModuleHeaderSpecific::Effect {
                                    effect_keyword_range: _,
                                    module_keyword_range,
                                    where_keyword_range: _,
                                    command: _,
                                    subscription: _,
                                } => module_keyword_range,
                            },
                        },
                        None => lsp_types::Range {
                            start: lsp_types::Position {
                                line: 0,
                                character: 0,
                            },
                            end: lsp_types::Position {
                                line: 1,
                                character: 0,
                            },
                        },
                    },
                },
            ))
        }
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name: goto_name,
            all_exposes: _,
        } => {
            let declaration_name_range: lsp_types::Range = goto_project_module_state
                .module
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && &origin_module_declaration_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            operator: maybe_origin_module_declaration_operator,
                            function: maybe_origin_module_declaration_function,
                            ..
                        } => {
                            if let Some(origin_module_declaration_operator_node) =
                                maybe_origin_module_declaration_operator
                                && origin_module_declaration_operator_node.value == goto_name
                            {
                                Some(origin_module_declaration_operator_node.range)
                            } else if let Some(origin_module_declaration_function_node) =
                                maybe_origin_module_declaration_function
                                && goto_name == &origin_module_declaration_function_node.value
                            {
                                Some(origin_module_declaration_function_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && &origin_module_declaration_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            // record type alias constructor function
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && &origin_module_declaration_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable {
                            start_name: origin_module_declaration_name_node,
                            ..
                        } => {
                            if &origin_module_declaration_name_node.value == goto_name {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                    }
                })?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: goto_definition_arguments
                        .text_document_position_params
                        .text_document
                        .uri,
                    range: declaration_name_range,
                },
            ))
        }
        ElmSyntaxSymbol::ImportExpose {
            origin_module: goto_module_origin,
            name: goto_name,
            all_exposes: _,
        } => {
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range =
                origin_module_state.syntax.declarations.iter().find_map(
                    |origin_module_declaration| {
                        let origin_module_declaration_node =
                            origin_module_declaration.declaration.as_ref()?;
                        match &origin_module_declaration_node.value {
                            ElmSyntaxDeclaration::ChoiceType {
                                name: maybe_origin_module_declaration_name,
                                ..
                            } => {
                                if let Some(origin_module_declaration_name_node) =
                                    maybe_origin_module_declaration_name
                                    && &origin_module_declaration_name_node.value == goto_name
                                {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::Operator {
                                operator: maybe_origin_module_declaration_operator,
                                ..
                            } => {
                                if let Some(origin_module_declaration_operator_node) =
                                    maybe_origin_module_declaration_operator
                                    && origin_module_declaration_operator_node.value == goto_name
                                {
                                    Some(origin_module_declaration_operator_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::Port {
                                name: maybe_origin_module_declaration_name,
                                ..
                            } => {
                                if let Some(origin_module_declaration_name_node) =
                                    maybe_origin_module_declaration_name
                                    && &origin_module_declaration_name_node.value == goto_name
                                {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::TypeAlias {
                                name: maybe_origin_module_declaration_name,
                                ..
                            } => {
                                if let Some(origin_module_declaration_name_node) =
                                    maybe_origin_module_declaration_name
                                    && &origin_module_declaration_name_node.value == goto_name
                                {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::Variable {
                                start_name: origin_module_declaration_name_node,
                                ..
                            } => {
                                if &origin_module_declaration_name_node.value == goto_name {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                        }
                    },
                )?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: declaration_name_range,
                },
            ))
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification: goto_qualification,
            name: goto_name,
            local_bindings,
        } => {
            if goto_qualification.is_empty()
                && let Some((goto_local_binding_origin, _)) =
                    find_local_binding_scope_expression(&local_bindings, goto_name)
            {
                return Some(lsp_types::GotoDefinitionResponse::Scalar(
                    lsp_types::Location {
                        uri: goto_definition_arguments
                            .text_document_position_params
                            .text_document
                            .uri,
                        range: match goto_local_binding_origin {
                            LocalBindingOrigin::PatternVariable(range) => range,
                            LocalBindingOrigin::PatternRecordField(range) => range,
                            LocalBindingOrigin::LetDeclaredVariable {
                                signature_type: _,
                                start_name_range,
                            } => start_name_range,
                        },
                    },
                ));
            }
            let goto_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    goto_project_module_state.project,
                    &goto_project_module_state.module.syntax,
                ),
                goto_qualification,
                goto_name,
            );
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range = origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            variant0_name: maybe_origin_module_declaration_variant0_name_node,
                            variant1_up: origin_module_declaration_variant1_up,
                            ..
                        } => {
                            if let Some(origin_module_declaration_variant0_name_node) =
                                maybe_origin_module_declaration_variant0_name_node
                                && &origin_module_declaration_variant0_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_variant0_name_node.range)
                            } else {
                                origin_module_declaration_variant1_up
                                    .iter()
                                    .find_map(|variant| {
                                        variant.name.as_ref().and_then(|variant_name_node| {
                                            if &variant_name_node.value == goto_name {
                                                Some(variant_name_node.range)
                                            } else {
                                                None
                                            }
                                        })
                                    })
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            operator: maybe_origin_module_declaration_operator,
                            function: maybe_origin_module_declaration_function,
                            ..
                        } => {
                            if let Some(origin_module_declaration_operator_node) =
                                maybe_origin_module_declaration_operator
                                && origin_module_declaration_operator_node.value == goto_name
                            {
                                Some(origin_module_declaration_operator_node.range)
                            } else if let Some(origin_module_declaration_function_node) =
                                maybe_origin_module_declaration_function
                                && goto_name == &origin_module_declaration_function_node.value
                            {
                                Some(origin_module_declaration_function_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && &origin_module_declaration_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            // record type alias constructor function
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && &origin_module_declaration_name_node.value == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Variable {
                            start_name: origin_module_declaration_name_node,
                            ..
                        } => {
                            if &origin_module_declaration_name_node.value == goto_name {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                    }
                })?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: declaration_name_range,
                },
            ))
        }
        ElmSyntaxSymbol::Type {
            qualification: goto_qualification,
            name: goto_name,
        } => {
            let goto_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    goto_project_module_state.project,
                    &goto_project_module_state.module.syntax,
                ),
                goto_qualification,
                goto_name,
            );
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range =
                origin_module_state.syntax.declarations.iter().find_map(
                    |origin_module_declaration| {
                        let origin_module_declaration_node =
                            origin_module_declaration.declaration.as_ref()?;
                        match &origin_module_declaration_node.value {
                            ElmSyntaxDeclaration::ChoiceType {
                                name: maybe_origin_module_declaration_name,
                                ..
                            } => {
                                if let Some(origin_module_declaration_name_node) =
                                    maybe_origin_module_declaration_name
                                    && &origin_module_declaration_name_node.value == goto_name
                                {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::Operator { .. } => None,
                            ElmSyntaxDeclaration::Port { .. } => None,
                            ElmSyntaxDeclaration::TypeAlias {
                                name: maybe_origin_module_declaration_name,
                                ..
                            } => {
                                if let Some(origin_module_declaration_name_node) =
                                    maybe_origin_module_declaration_name
                                    && &origin_module_declaration_name_node.value == goto_name
                                {
                                    Some(origin_module_declaration_name_node.range)
                                } else {
                                    None
                                }
                            }
                            ElmSyntaxDeclaration::Variable { .. } => None,
                        }
                    },
                )?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: declaration_name_range,
                },
            ))
        }
    }
}

fn respond_to_prepare_rename(
    state: &State,
    prepare_rename_arguments: lsp_types::TextDocumentPositionParams,
) -> Option<Result<lsp_types::PrepareRenameResponse, async_lsp::ResponseError>> {
    let project_module_state =
        state_get_project_module_by_lsp_url(state, &prepare_rename_arguments.text_document.uri)?;
    let prepare_rename_symbol_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &project_module_state.module.syntax,
            prepare_rename_arguments.position,
        )?;
    Some(match prepare_rename_symbol_node.value {
        ElmSyntaxSymbol::ImportAlias {
            module_origin: _,
            alias_name: alias_name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol_node.range,
            placeholder: alias_name.to_string(),
        }),
        ElmSyntaxSymbol::ModuleName(module_name) => {
            Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                range: prepare_rename_symbol_node.range,
                placeholder: module_name.to_string(),
            })
        }
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name,
            all_exposes: _,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol_node.range,
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::ImportExpose {
            name,
            origin_module: _,
            all_exposes: _,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol_node.range,
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::ModuleMemberDeclarationName {
            name,
            declaration: _,
            documentation: _,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol_node.range,
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification,
            name,
            local_bindings,
        } => {
            if qualification.is_empty()
                && let Some((local_binding_origin, _)) =
                    find_local_binding_scope_expression(&local_bindings, name)
            {
                match local_binding_origin {
                    LocalBindingOrigin::PatternVariable(_)
                    | LocalBindingOrigin::LetDeclaredVariable { .. } => {
                        Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                            range: prepare_rename_symbol_node.range,
                            placeholder: name.to_string(),
                        })
                    }
                    LocalBindingOrigin::PatternRecordField(_) => {
                        Err(async_lsp::ResponseError::new(
                            async_lsp::ErrorCode::REQUEST_FAILED,
                            "cannot rename a variable that is bound to a field name",
                        ))
                    }
                }
            } else {
                Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                    range: lsp_types::Range {
                        start: lsp_position_add_characters(
                            prepare_rename_symbol_node.range.end,
                            -(name.len() as i32),
                        ),
                        end: prepare_rename_symbol_node.range.end,
                    },
                    placeholder: name.to_string(),
                })
            }
        }
        ElmSyntaxSymbol::Type {
            qualification: _,
            name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: lsp_types::Range {
                start: lsp_position_add_characters(
                    prepare_rename_symbol_node.range.end,
                    -(name.len() as i32),
                ),
                end: prepare_rename_symbol_node.range.end,
            },
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration: _,
            name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol_node.range,
            placeholder: name.to_string(),
        }),
    })
}

struct ProjectModuleOriginAndState<'a> {
    project_state: &'a ProjectState,
    module_path: &'a std::path::PathBuf,
    module_state: &'a ModuleState,
}

fn state_iter_all_modules<'a>(
    state: &'a State,
) -> impl Iterator<Item = ProjectModuleOriginAndState<'a>> {
    state.projects.values().flat_map(|project_state| {
        project_state
            .modules
            .iter()
            .map(|(module_path, module_state)| ProjectModuleOriginAndState {
                project_state: project_state,
                module_path: module_path,
                module_state: module_state,
            })
    })
}

fn respond_to_rename(
    state: &State,
    rename_arguments: lsp_types::RenameParams,
) -> Option<Vec<lsp_types::TextDocumentEdit>> {
    let to_rename_project_module_state = state_get_project_module_by_lsp_url(
        state,
        &rename_arguments.text_document_position.text_document.uri,
    )?;
    let symbol_to_rename_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &to_rename_project_module_state.module.syntax,
            rename_arguments.text_document_position.position,
        )?;
    Some(match symbol_to_rename_node.value {
        ElmSyntaxSymbol::ImportAlias {
            module_origin: import_alias_to_rename_module_origin,
            alias_name: import_alias_to_rename,
        } => {
            let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_module_uses_of_reference_into(
                &mut all_uses_of_renamed_module_name,
                state,
                to_rename_project_module_state.project,
                &to_rename_project_module_state.module.syntax,
                ElmDeclaredSymbol::ImportAlias {
                    module_origin: import_alias_to_rename_module_origin,
                    alias_name: import_alias_to_rename,
                },
            );
            vec![lsp_types::TextDocumentEdit {
                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: rename_arguments.text_document_position.text_document.uri,
                    version: None,
                },
                edits: all_uses_of_renamed_module_name
                    .into_iter()
                    .map(|use_range_of_renamed_module| {
                        lsp_types::OneOf::Left(lsp_types::TextEdit {
                            range: use_range_of_renamed_module,
                            new_text: rename_arguments.new_name.clone(),
                        })
                    })
                    .collect::<Vec<_>>(),
            }]
        }
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration,
            name: type_variable_to_rename,
        } => {
            let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_declaration_uses_of_reference_into(
                &mut all_uses_of_renamed_module_name,
                to_rename_project_module_state
                    .module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|node| node.value.as_str())
                    .unwrap_or(""),
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_rename_project_module_state.project,
                    &to_rename_project_module_state.module.syntax,
                ),
                scope_declaration,
                ElmDeclaredSymbol::TypeVariable(type_variable_to_rename),
            );
            vec![lsp_types::TextDocumentEdit {
                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: rename_arguments.text_document_position.text_document.uri,
                    version: None,
                },
                edits: all_uses_of_renamed_module_name
                    .into_iter()
                    .map(|use_range_of_renamed_module| {
                        lsp_types::OneOf::Left(lsp_types::TextEdit {
                            range: use_range_of_renamed_module,
                            new_text: rename_arguments.new_name.clone(),
                        })
                    })
                    .collect::<Vec<_>>(),
            }]
        }
        ElmSyntaxSymbol::ModuleName(module_name_to_rename) => state
            .projects
            .values()
            .flat_map(|project| project.modules.iter())
            .filter_map(|(elm_module_file_path, elm_module_state)| {
                let elm_module_uri = lsp_types::Url::from_file_path(elm_module_file_path).ok()?;
                let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_module_uses_of_reference_into(
                    &mut all_uses_of_renamed_module_name,
                    state,
                    to_rename_project_module_state.project,
                    &elm_module_state.syntax,
                    ElmDeclaredSymbol::ModuleName(module_name_to_rename),
                );
                Some(lsp_types::TextDocumentEdit {
                    text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                        uri: elm_module_uri,
                        version: None,
                    },
                    edits: all_uses_of_renamed_module_name
                        .into_iter()
                        .map(|use_range_of_renamed_module| {
                            lsp_types::OneOf::Left(lsp_types::TextEdit {
                                range: use_range_of_renamed_module,
                                new_text: rename_arguments.new_name.clone(),
                            })
                        })
                        .collect::<Vec<_>>(),
                })
            })
            .collect::<Vec<_>>(),
        ElmSyntaxSymbol::ModuleMemberDeclarationName {
            name: to_rename_declaration_name,
            documentation: _,
            declaration: _,
        }
        | ElmSyntaxSymbol::ModuleHeaderExpose {
            name: to_rename_declaration_name,
            all_exposes: _,
        } => {
            let to_rename_module_origin: &str = to_rename_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str())
                .unwrap_or("");
            let elm_declared_symbol_to_rename: ElmDeclaredSymbol = if to_rename_declaration_name
                .starts_with(char::is_uppercase)
            {
                let to_rename_is_record_type_alias: bool = to_rename_project_module_state
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .any(|documented_declaration| {
                        documented_declaration.declaration.as_ref().is_some_and(
                            |declaration_node| match &declaration_node.value {
                                ElmSyntaxDeclaration::TypeAlias {
                                    type_:
                                        Some(ElmSyntaxNode {
                                            value: ElmSyntaxType::Record(_),
                                            range: _,
                                        }),
                                    name: Some(record_type_alias_name_node),
                                    ..
                                } => {
                                    record_type_alias_name_node.value == to_rename_declaration_name
                                }
                                _ => false,
                            },
                        )
                    });
                if to_rename_is_record_type_alias {
                    ElmDeclaredSymbol::RecordTypeAlias {
                        module_origin: to_rename_module_origin,
                        name: to_rename_declaration_name,
                    }
                } else {
                    ElmDeclaredSymbol::TypeNotRecordAlias {
                        module_origin: to_rename_module_origin,
                        name: to_rename_declaration_name,
                    }
                }
            } else {
                ElmDeclaredSymbol::VariableOrVariant {
                    module_origin: to_rename_module_origin,
                    name: to_rename_declaration_name,
                }
            };
            state_iter_all_modules(state)
                .filter_map(move |project_module| {
                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_renamed_module_name,
                        state,
                        project_module.project_state,
                        &project_module.module_state.syntax,
                        elm_declared_symbol_to_rename,
                    );
                    let elm_module_uri: lsp_types::Url =
                        lsp_types::Url::from_file_path(project_module.module_path).ok()?;
                    Some(lsp_types::TextDocumentEdit {
                        text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                            uri: elm_module_uri,
                            version: None,
                        },
                        edits: all_uses_of_renamed_module_name
                            .into_iter()
                            .map(|use_range_of_renamed_module| {
                                lsp_types::OneOf::Left(lsp_types::TextEdit {
                                    range: use_range_of_renamed_module,
                                    new_text: rename_arguments.new_name.clone(),
                                })
                            })
                            .collect::<Vec<_>>(),
                    })
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::ImportExpose {
            origin_module: to_rename_import_expose_origin_module,
            name: to_rename_import_expose_name,
            all_exposes: _,
        } => {
            let elm_declared_symbol_to_rename: ElmDeclaredSymbol =
                if to_rename_import_expose_name.starts_with(char::is_uppercase) {
                    let to_rename_is_record_type_alias: bool = to_rename_project_module_state
                        .module
                        .syntax
                        .declarations
                        .iter()
                        .any(|documented_declaration| {
                            documented_declaration.declaration.as_ref().is_some_and(
                                |declaration_node| match &declaration_node.value {
                                    ElmSyntaxDeclaration::TypeAlias {
                                        type_:
                                            Some(ElmSyntaxNode {
                                                value: ElmSyntaxType::Record(_),
                                                range: _,
                                            }),
                                        name: Some(record_type_alias_name_node),
                                        ..
                                    } => {
                                        record_type_alias_name_node.value
                                            == to_rename_import_expose_name
                                    }
                                    _ => false,
                                },
                            )
                        });
                    if to_rename_is_record_type_alias {
                        ElmDeclaredSymbol::RecordTypeAlias {
                            module_origin: to_rename_import_expose_origin_module,
                            name: to_rename_import_expose_name,
                        }
                    } else {
                        ElmDeclaredSymbol::TypeNotRecordAlias {
                            module_origin: to_rename_import_expose_origin_module,
                            name: to_rename_import_expose_name,
                        }
                    }
                } else {
                    ElmDeclaredSymbol::VariableOrVariant {
                        module_origin: to_rename_import_expose_origin_module,
                        name: to_rename_import_expose_name,
                    }
                };
            state_iter_all_modules(state)
                .filter_map(move |project_module| {
                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_renamed_module_name,
                        state,
                        project_module.project_state,
                        &project_module.module_state.syntax,
                        elm_declared_symbol_to_rename,
                    );
                    let elm_module_uri: lsp_types::Url =
                        lsp_types::Url::from_file_path(project_module.module_path).ok()?;
                    Some(lsp_types::TextDocumentEdit {
                        text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                            uri: elm_module_uri,
                            version: None,
                        },
                        edits: all_uses_of_renamed_module_name
                            .into_iter()
                            .map(|use_range_of_renamed_module| {
                                lsp_types::OneOf::Left(lsp_types::TextEdit {
                                    range: use_range_of_renamed_module,
                                    new_text: rename_arguments.new_name.clone(),
                                })
                            })
                            .collect::<Vec<_>>(),
                    })
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification: to_rename_qualification,
            name: to_rename_name,
            local_bindings,
        } => {
            if to_rename_qualification.is_empty()
                && let Some((
                    to_rename_local_binding_origin,
                    local_binding_to_rename_scope_expression,
                )) = find_local_binding_scope_expression(&local_bindings, to_rename_name)
            {
                let mut all_uses_of_local_binding_to_rename: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_expression_uses_of_reference_into(
                    &mut all_uses_of_local_binding_to_rename,
                    &elm_syntax_module_create_origin_lookup(
                        state,
                        to_rename_project_module_state.project,
                        &to_rename_project_module_state.module.syntax,
                    ),
                    &[ElmLocalBinding {
                        name: to_rename_name,
                        origin: to_rename_local_binding_origin,
                    }],
                    local_binding_to_rename_scope_expression,
                    ElmDeclaredSymbol::LocalBinding(to_rename_name),
                );
                match to_rename_local_binding_origin {
                    LocalBindingOrigin::PatternVariable(range) => {
                        all_uses_of_local_binding_to_rename.push(range);
                    }
                    LocalBindingOrigin::PatternRecordField(_) => {
                        // should never have been prepared for rename
                    }
                    LocalBindingOrigin::LetDeclaredVariable { .. } => {
                        // already included in scope expression
                    }
                }
                vec![lsp_types::TextDocumentEdit {
                    text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                        uri: rename_arguments.text_document_position.text_document.uri,
                        version: None,
                    },
                    edits: all_uses_of_local_binding_to_rename
                        .into_iter()
                        .map(|use_range_of_renamed_module| {
                            lsp_types::OneOf::Left(lsp_types::TextEdit {
                                range: use_range_of_renamed_module,
                                new_text: rename_arguments.new_name.clone(),
                            })
                        })
                        .collect::<Vec<_>>(),
                }]
            } else {
                let to_rename_module_origin: &str = look_up_origin_module(
                    &elm_syntax_module_create_origin_lookup(
                        state,
                        to_rename_project_module_state.project,
                        &to_rename_project_module_state.module.syntax,
                    ),
                    to_rename_qualification,
                    to_rename_name,
                );
                state_iter_all_modules(state)
                    .filter_map(|project_module| {
                        let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                        elm_syntax_module_uses_of_reference_into(
                            &mut all_uses_of_renamed_module_name,
                            state,
                            project_module.project_state,
                            &project_module.module_state.syntax,
                            ElmDeclaredSymbol::VariableOrVariant {
                                module_origin: to_rename_module_origin,
                                name: to_rename_name,
                            },
                        );
                        let elm_module_uri: lsp_types::Url =
                            lsp_types::Url::from_file_path(project_module.module_path).ok()?;
                        Some(lsp_types::TextDocumentEdit {
                            text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                uri: elm_module_uri,
                                version: None,
                            },
                            edits: all_uses_of_renamed_module_name
                                .into_iter()
                                .map(|use_range_of_renamed_module| {
                                    lsp_types::OneOf::Left(lsp_types::TextEdit {
                                        range: use_range_of_renamed_module,
                                        new_text: rename_arguments.new_name.clone(),
                                    })
                                })
                                .collect::<Vec<_>>(),
                        })
                    })
                    .collect::<Vec<_>>()
            }
        }
        ElmSyntaxSymbol::Type {
            qualification: to_rename_qualification,
            name: type_name_to_rename,
        } => {
            let to_rename_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_rename_project_module_state.project,
                    &to_rename_project_module_state.module.syntax,
                ),
                to_rename_qualification,
                type_name_to_rename,
            );
            let to_rename_is_record_type_alias: bool =
                project_state_get_module_with_name(
                    state,
                    to_rename_project_module_state.project,
                    to_rename_module_origin,
                )
                .is_some_and(|(_, to_rename_module_state)| {
                    to_rename_module_state.syntax.declarations.iter().any(
                        |documented_declaration| {
                            documented_declaration.declaration.as_ref().is_some_and(
                                |declaration_node| match &declaration_node.value {
                                    ElmSyntaxDeclaration::TypeAlias {
                                        type_:
                                            Some(ElmSyntaxNode {
                                                value: ElmSyntaxType::Record(_),
                                                range: _,
                                            }),
                                        name: Some(record_type_alias_name_node),
                                        ..
                                    } => record_type_alias_name_node.value == type_name_to_rename,
                                    _ => false,
                                },
                            )
                        },
                    )
                });
            let elm_declared_symbol_to_rename: ElmDeclaredSymbol = if to_rename_is_record_type_alias
            {
                ElmDeclaredSymbol::RecordTypeAlias {
                    module_origin: to_rename_module_origin,
                    name: type_name_to_rename,
                }
            } else {
                ElmDeclaredSymbol::TypeNotRecordAlias {
                    module_origin: to_rename_module_origin,
                    name: type_name_to_rename,
                }
            };
            state_iter_all_modules(state)
                .filter_map(|project_module| {
                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_renamed_module_name,
                        state,
                        project_module.project_state,
                        &project_module.module_state.syntax,
                        elm_declared_symbol_to_rename,
                    );
                    lsp_types::Url::from_file_path(project_module.module_path)
                        .ok()
                        .map(|elm_module_uri| lsp_types::TextDocumentEdit {
                            text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                uri: elm_module_uri,
                                version: None,
                            },
                            edits: all_uses_of_renamed_module_name
                                .into_iter()
                                .map(|use_range_of_renamed_module| {
                                    lsp_types::OneOf::Left(lsp_types::TextEdit {
                                        range: use_range_of_renamed_module,
                                        new_text: rename_arguments.new_name.clone(),
                                    })
                                })
                                .collect::<Vec<_>>(),
                        })
                })
                .collect::<Vec<_>>()
        }
    })
}

fn respond_to_semantic_tokens_full(
    state: &State,
    semantic_tokens_arguments: lsp_types::SemanticTokensParams,
) -> Option<lsp_types::SemanticTokensResult> {
    let project_module_state =
        state_get_project_module_by_lsp_url(state, &semantic_tokens_arguments.text_document.uri)?;
    let mut highlighting: Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>> = Vec::new();
    elm_syntax_highlight_module_into(&mut highlighting, &project_module_state.module.syntax);
    Some(lsp_types::SemanticTokensResult::Tokens(
        lsp_types::SemanticTokens {
            result_id: None,
            data: highlighting
                .into_iter()
                .scan(
                    lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    |previous_start_location, segment| {
                        if (segment.range.end.line != segment.range.start.line)
                            || (segment.range.end.character < segment.range.start.character)
                        {
                            eprintln!(
                                "bad highlight token range: must be single-line and positive {:?}",
                                segment.range
                            );
                            return None;
                        }
                        match lsp_position_positive_delta(
                            *previous_start_location,
                            segment.range.start,
                        ) {
                            Err(error) => {
                                eprintln!("bad highlight token order {error}");
                                None
                            }
                            Ok(delta) => {
                                let token = lsp_types::SemanticToken {
                                    delta_line: delta.line as u32,
                                    delta_start: delta.character as u32,
                                    length: (segment.range.end.character
                                        - segment.range.start.character)
                                        as u32,
                                    token_type: semantic_token_type_to_id(
                                        elm_syntax_highlight_kind_to_lsp_semantic_token_type(
                                            segment.value,
                                        ),
                                    ),
                                    token_modifiers_bitset: 0_u32,
                                };
                                segment.range.start.clone_into(previous_start_location);
                                Some(token)
                            }
                        }
                    },
                )
                .collect::<Vec<lsp_types::SemanticToken>>(),
        },
    ))
}

const token_types: [lsp_types::SemanticTokenType; 11] = [
    lsp_types::SemanticTokenType::NUMBER,
    lsp_types::SemanticTokenType::STRING,
    lsp_types::SemanticTokenType::NAMESPACE,
    lsp_types::SemanticTokenType::VARIABLE,
    lsp_types::SemanticTokenType::TYPE,
    lsp_types::SemanticTokenType::TYPE_PARAMETER,
    lsp_types::SemanticTokenType::KEYWORD,
    lsp_types::SemanticTokenType::ENUM_MEMBER,
    lsp_types::SemanticTokenType::PROPERTY,
    lsp_types::SemanticTokenType::COMMENT,
    lsp_types::SemanticTokenType::FUNCTION,
];

fn semantic_token_type_to_id(semantic_token: lsp_types::SemanticTokenType) -> u32 {
    token_types
        .iter()
        .enumerate()
        .find_map(|(i, token)| {
            if *token == semantic_token {
                Some(i as u32)
            } else {
                None
            }
        })
        .unwrap_or(0_u32)
}

fn present_variable_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&str>,
    maybe_signature_type: Option<&ElmSyntaxType>,
) -> String {
    let description = match maybe_signature_type {
        Some(signature_type) => format!(
            "```elm\n{}.{} : {}\n```\n",
            module_origin,
            name,
            &elm_syntax_type_to_single_line_string(&module_origin_lookup, signature_type,)
        ),
        None => format!("```elm\n{}.{}\n```\n", &module_origin, &name),
    };
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(documentation)
        }
    }
}
fn present_port_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&str>,
    maybe_type: Option<&ElmSyntaxType>,
) -> String {
    let description: String = format!(
        "```elm\nport {}.{} : {}\n```\n",
        module_origin,
        name,
        &match maybe_type {
            None => "".to_string(),
            Some(type_) => elm_syntax_type_to_single_line_string(&module_origin_lookup, type_),
        }
    );
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(documentation)
        }
    }
}
fn present_type_alias_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&str>,
    parameters: &[ElmSyntaxNode<String>],
    maybe_type: Option<&ElmSyntaxType>,
) -> String {
    let description = format!(
        "```elm\ntype alias {}.{}{} =\n    {}\n```\n",
        module_origin,
        name,
        &parameters
            .iter()
            .fold(String::new(), |so_far, parameter_node| so_far
                + " "
                + &parameter_node.value,),
        &match maybe_type {
            None => "".to_string(),
            Some(type_) => elm_syntax_type_to_single_line_string(&module_origin_lookup, type_),
        }
    );
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(documentation)
        }
    }
}
const list_list_type_info_markdown: &'static str = "```elm
type List.List element
```
-----
A list of elements. The elements in a list must have the same type. Here are some examples:
```elm
[ \"one\", \"two\", \"three\" ] --: List String
[ 3.14, 0.1234 ] --: List Float
[ 'a', 'Z', '0' ] --: List Char
[ 42, 43 ] --: List number
[] --: List any
```

If you want to match on all elements or the first few elements,
you can use these special patterns:
```elm
last list =
    case list of
        -- exactly 1 element
        [ onlyElement ] ->
            Just onlyElement

        -- exactly empty
        [] ->
            Nothing

        -- at least 2 elements
        firstElement :: secondElement :: thirdElementEtc ->
            last (secondElement :: thirdElementEtc)
```
";
fn present_choice_type_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&str>,
    parameters: &[ElmSyntaxNode<String>],
    variant0_name: Option<&str>,
    variant0_values: &[ElmSyntaxNode<ElmSyntaxType>],
    variant1_up: &[ElmSyntaxChoiceTypeDeclarationTailingVariant],
) -> String {
    let description = format!(
        "```elm\ntype {module_origin}.{name}{}\n    = {}{}{}\n```\n",
        &parameters
            .iter()
            .fold(String::new(), |so_far, parameter_node| so_far
                + " "
                + &parameter_node.value,),
        &variant0_name.unwrap_or(""),
        &variant0_values
            .iter()
            .fold(String::new(), |so_far, value_node| so_far
                + " "
                + &elm_syntax_type_to_single_line_string(
                    &module_origin_lookup,
                    &value_node.value,
                ),),
        &variant1_up
            .iter()
            .fold(String::new(), |so_far, variant| so_far
                + "\n    | "
                + &variant
                    .name
                    .as_ref()
                    .map(|node| node.value.as_str())
                    .unwrap_or("")
                + &variant.values.iter().fold(
                    String::new(),
                    |so_far, value_node| so_far
                        + " "
                        + &elm_syntax_type_to_single_line_string(
                            &module_origin_lookup,
                            &value_node.value,
                        ),
                ),),
    );
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(documentation)
        }
    }
}
fn present_operator_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    operator_symbol: Option<&str>,
    maybe_origin_operator_function_declaration: Option<(
        Option<&ElmSyntaxVariableDeclarationSignature>,
        Option<&str>,
    )>,
    maybe_documentation: Option<&str>,
    precedence: Option<i64>,
    // TODO swap direction ad precedence
    maybe_direction: Option<ElmSyntaxInfixDirection>,
) -> String {
    match maybe_origin_operator_function_declaration {
        Some((
            origin_operator_function_maybe_signature,
            origin_operator_function_maybe_documentation,
        )) => {
            let description = format!(
                "```elm\ninfix {} {} {module_origin}.({}){}\n```\n",
                maybe_direction
                    .map(elm_syntax_infix_direction_to_str)
                    .unwrap_or(""),
                &precedence
                    .map(|n| n.to_string())
                    .unwrap_or_else(|| "".to_string()),
                operator_symbol.unwrap_or(""),
                &(match origin_operator_function_maybe_signature {
                    None => "".to_string(),
                    Some(origin_operator_function_signature) => {
                        " : ".to_string()
                            + &match origin_operator_function_signature.type_ {
                                None => "".to_string(),
                                Some(ref origin_operator_function_type) => {
                                    elm_syntax_type_to_single_line_string(
                                        &module_origin_lookup,
                                        &origin_operator_function_type.value,
                                    )
                                }
                            }
                    }
                })
            );
            match origin_operator_function_maybe_documentation {
                None => description,
                Some(documentation) => {
                    description + "-----\n" + &documentation_comment_to_markdown(documentation)
                }
            }
        }
        None => {
            let description = format!(
                "```elm\ninfix {} {} {module_origin}.({})\n```\n",
                maybe_direction
                    .map(elm_syntax_infix_direction_to_str)
                    .unwrap_or(""),
                precedence
                    .map(|n| n.to_string())
                    .unwrap_or_else(|| "".to_string()),
                operator_symbol.unwrap_or("")
            );
            match maybe_documentation {
                None => description,
                Some(documentation) => {
                    description + "-----\n" + &documentation_comment_to_markdown(documentation)
                }
            }
        }
    }
}
fn elm_syntax_infix_direction_to_str(direction: ElmSyntaxInfixDirection) -> &'static str {
    match direction {
        ElmSyntaxInfixDirection::Left => "left",
        ElmSyntaxInfixDirection::Non => "non",
        ElmSyntaxInfixDirection::Right => "right",
    }
}

fn respond_to_completion(
    state: &State,
    completion_arguments: lsp_types::CompletionParams,
) -> Option<lsp_types::CompletionResponse> {
    let completion_project_module = state_get_project_module_by_lsp_url(
        state,
        &completion_arguments
            .text_document_position
            .text_document
            .uri,
    )?;
    let symbol_to_complete: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &completion_project_module.module.syntax,
            completion_arguments.text_document_position.position,
        )?;
    let maybe_completion_items: Option<Vec<lsp_types::CompletionItem>> = match symbol_to_complete
        .value
    {
        ElmSyntaxSymbol::ImportAlias { .. } => {
            // these are custom names you choose yourself, no need to suggest completion
            None
        }
        ElmSyntaxSymbol::ModuleName(module_name) => {
            Some(project_module_name_completions_for_except(
                state,
                &completion_project_module.project,
                &[],
                module_name,
                completion_project_module
                    .module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|node| node.value.as_str()),
            ))
        }
        ElmSyntaxSymbol::ModuleMemberDeclarationName { declaration, .. } => {
            match &declaration.value {
                ElmSyntaxDeclaration::ChoiceType { .. }
                | ElmSyntaxDeclaration::Port { .. }
                | ElmSyntaxDeclaration::Operator { .. }
                | ElmSyntaxDeclaration::TypeAlias { .. }
                | ElmSyntaxDeclaration::Variable {
                    signature: Some(_), ..
                } => {
                    // these are custom names you choose yourself, no need to suggest completion
                    None
                }
                ElmSyntaxDeclaration::Variable {
                    signature: None, ..
                } => {
                    // find previous signature
                    completion_project_module
                        .module
                        .syntax
                        .declarations
                        .iter()
                        .zip(
                            completion_project_module
                                .module
                                .syntax
                                .declarations
                                .iter()
                                .skip(1),
                        )
                        .find_map(|(previous_declaration, current_declaration)| {
                            if let Some(current_declaration_node) = &current_declaration.declaration
                                && let ElmSyntaxDeclaration::Variable {
                                    start_name: current_declaration_start_name_node,
                                    signature: None,
                                    ..
                                } = &current_declaration_node.value
                                && current_declaration_start_name_node.range
                                    == symbol_to_complete.range
                                && let Some(previous_declaration_node) =
                                    &previous_declaration.declaration
                                && let ElmSyntaxDeclaration::Variable {
                                    start_name: previous_declaration_start_name_node,
                                    signature:
                                        None
                                        | Some(ElmSyntaxVariableDeclarationSignature {
                                            implementation_name_range: None,
                                            ..
                                        }),
                                    ..
                                } = &previous_declaration_node.value
                            {
                                Some(vec![lsp_types::CompletionItem {
                                    label: previous_declaration_start_name_node.value.clone(),
                                    kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                    documentation: None,
                                    ..lsp_types::CompletionItem::default()
                                }])
                            } else {
                                None
                            }
                        })
                }
            }
        }
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name: to_complete_header_expose_name,
            all_exposes,
        } => {
            let module_origin: &str = completion_project_module
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str())
                .unwrap_or("");
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                completion_project_module.project,
                &completion_project_module.module.syntax,
            );
            let existing_expose_names: std::collections::HashSet<&str> = all_exposes
                .iter()
                .filter_map(|expose_node| {
                    let expose_name: &str = match &expose_node.value {
                        ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                            name: open_choice_type_name,
                            open_range: _,
                        } => Some(open_choice_type_name.value.as_str()),
                        ElmSyntaxExpose::Operator(operator_symbol) => {
                            operator_symbol.as_ref().map(|node| node.value)
                        }
                        ElmSyntaxExpose::Type(name) => Some(name.as_str()),
                        ElmSyntaxExpose::Variable(name) => Some(name.as_str()),
                    }?;
                    if expose_name == to_complete_header_expose_name {
                        None
                    } else {
                        Some(expose_name)
                    }
                })
                .collect::<std::collections::HashSet<_>>();
            let mut completion_items: Vec<lsp_types::CompletionItem> = Vec::new();
            for (origin_module_declaration_node, origin_module_declaration_documentation) in
                completion_project_module
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|documented_declaration| {
                        let declaration_node = documented_declaration.declaration.as_ref()?;
                        Some((
                            declaration_node,
                            documented_declaration
                                .documentation
                                .as_ref()
                                .map(|node| node.value.as_str()),
                        ))
                    })
            {
                match &origin_module_declaration_node.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: maybe_choice_type_name,
                        parameters,
                        equals_key_symbol_range: _,
                        variant0_name,
                        variant0_values,
                        variant1_up,
                    } => {
                        if let Some(choice_type_name_node) = maybe_choice_type_name
                            && !existing_expose_names.contains(choice_type_name_node.value.as_str())
                        {
                            let info_markdown: String =
                                present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_origin,
                                    &choice_type_name_node.value,
                                    origin_module_declaration_documentation,
                                    &parameters,
                                    variant0_name.as_ref().map(|node| node.value.as_str()),
                                    &variant0_values,
                                    &variant1_up,
                                );
                            completion_items.push(lsp_types::CompletionItem {
                                label: choice_type_name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::ENUM_MEMBER),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        // should the documentation code indicate the variants are hidden?
                                        value: info_markdown.clone(),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                            completion_items.push(lsp_types::CompletionItem {
                                label: format!("{}(..)", choice_type_name_node.value),
                                kind: Some(lsp_types::CompletionItemKind::ENUM_MEMBER),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: info_markdown.clone(),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                        }
                    }
                    ElmSyntaxDeclaration::Port {
                        name: maybe_name,
                        colon_key_symbol_range: _,
                        type_,
                    } => {
                        if let Some(name_node) = maybe_name
                            && !existing_expose_names.contains(name_node.value.as_str())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &name_node.value,
                                            origin_module_declaration_documentation,
                                            type_.as_ref().map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::TypeAlias {
                        alias_keyword_range: _,
                        name: maybe_name,
                        parameters,
                        equals_key_symbol_range: _,
                        type_: maybe_type,
                    } => {
                        if let Some(name_node) = maybe_name
                            && !existing_expose_names.contains(name_node.value.as_str())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::STRUCT),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &name_node.value,
                                            origin_module_declaration_documentation,
                                            parameters,
                                            maybe_type.as_ref().map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::Variable {
                        start_name: start_name_node,
                        signature: maybe_signature,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        if !existing_expose_names.contains(start_name_node.value.as_str()) {
                            completion_items.push(lsp_types::CompletionItem {
                                label: start_name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &start_name_node.value,
                                            origin_module_declaration_documentation,
                                            maybe_signature
                                                .as_ref()
                                                .and_then(|signature| signature.type_.as_ref())
                                                .map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::Operator { .. } => {
                        // no new operators will ever be added
                    }
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::ImportExpose {
            origin_module: to_complete_module_origin,
            name: to_complete_import_expose_name,
            all_exposes,
        } => {
            let (_, import_origin_module_state) = project_state_get_module_with_name(
                state,
                completion_project_module.project,
                to_complete_module_origin,
            )?;
            let import_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    completion_project_module.project,
                    &import_origin_module_state.syntax,
                );
            let existing_import_expose_names: std::collections::HashSet<&str> = all_exposes
                .iter()
                .filter_map(|expose_node| match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: open_choice_type_name,
                        open_range: _,
                    } => Some(open_choice_type_name.value.as_str()),
                    ElmSyntaxExpose::Operator(operator_symbol) => {
                        operator_symbol.as_ref().map(|node| node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_str()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_str()),
                })
                .collect::<std::collections::HashSet<_>>();
            let import_origin_module_expose_set: ElmModuleHeaderExposeSet =
                elm_syntax_module_header_expose_set(
                    import_origin_module_state.syntax.header.as_ref(),
                );
            let import_origin_module_declaration_can_still_be_import_expose = |name: &str| -> bool {
                elm_expose_set_contains(&import_origin_module_expose_set, name)
                    && (name == to_complete_import_expose_name
                        || !existing_import_expose_names.contains(name))
            };
            let mut completion_items: Vec<lsp_types::CompletionItem> = Vec::new();
            for (origin_module_declaration_node, origin_module_declaration_documentation) in
                import_origin_module_state
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|documented_declaration| {
                        documented_declaration
                            .declaration
                            .as_ref()
                            .map(|declaration_node| {
                                (
                                    declaration_node,
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_str()),
                                )
                            })
                    })
            {
                match &origin_module_declaration_node.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: maybe_choice_type_name,
                        parameters,
                        equals_key_symbol_range: _,
                        variant0_name,
                        variant0_values,
                        variant1_up,
                    } => {
                        if let Some(choice_type_name_node) = maybe_choice_type_name
                            && import_origin_module_declaration_can_still_be_import_expose(
                                choice_type_name_node.value.as_str(),
                            )
                        {
                            let info_markdown: String = format!(
                                "variant in\n{}",
                                present_choice_type_declaration_info_markdown(
                                    &import_module_origin_lookup,
                                    to_complete_module_origin,
                                    &choice_type_name_node.value,
                                    origin_module_declaration_documentation,
                                    &parameters,
                                    variant0_name.as_ref().map(|node| node.value.as_str()),
                                    &variant0_values,
                                    &variant1_up,
                                ),
                            );
                            completion_items.extend(
                                variant0_name
                                    .as_ref()
                                    .map(|node| node.value.clone())
                                    .into_iter()
                                    .chain(variant1_up.iter().filter_map(|variant| {
                                        variant.name.as_ref().map(|node| node.value.clone())
                                    }))
                                    .map(|variant_name: String| lsp_types::CompletionItem {
                                        label: variant_name,
                                        kind: Some(lsp_types::CompletionItemKind::ENUM_MEMBER),
                                        documentation: Some(
                                            lsp_types::Documentation::MarkupContent(
                                                lsp_types::MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
                                                    value: info_markdown.clone(),
                                                },
                                            ),
                                        ),
                                        ..lsp_types::CompletionItem::default()
                                    }),
                            );
                        }
                    }
                    ElmSyntaxDeclaration::Port {
                        name,
                        colon_key_symbol_range: _,
                        type_,
                    } => {
                        if let Some(name_node) = name
                            && import_origin_module_declaration_can_still_be_import_expose(
                                name_node.value.as_str(),
                            )
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &name_node.value,
                                            origin_module_declaration_documentation,
                                            type_.as_ref().map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::TypeAlias {
                        alias_keyword_range: _,
                        name: maybe_name,
                        parameters,
                        equals_key_symbol_range: _,
                        type_: maybe_type,
                    } => {
                        if let Some(name_node) = maybe_name
                            && import_origin_module_declaration_can_still_be_import_expose(
                                name_node.value.as_str(),
                            )
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::CONSTRUCTOR),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &name_node.value,
                                            origin_module_declaration_documentation,
                                            parameters,
                                            maybe_type.as_ref().map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::Variable {
                        start_name: start_name_node,
                        signature: maybe_signature,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        if import_origin_module_declaration_can_still_be_import_expose(
                            start_name_node.value.as_str(),
                        ) {
                            completion_items.push(lsp_types::CompletionItem {
                                label: start_name_node.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &start_name_node.value,
                                            origin_module_declaration_documentation,
                                            maybe_signature
                                                .as_ref()
                                                .and_then(|signature| signature.type_.as_ref())
                                                .map(|node| &node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::Operator {
                        direction: maybe_direction,
                        precedence: maybe_precedence,
                        operator: maybe_operator,
                        equals_key_symbol_range: _,
                        function: maybe_function,
                    } => {
                        if let Some(operator_node) = maybe_operator
                            && import_origin_module_declaration_can_still_be_import_expose(
                                operator_node.value,
                            )
                        {
                            let maybe_origin_operator_function_declaration = maybe_function
                                .as_ref()
                                .and_then(|origin_module_declaration_function_node| {
                                    import_origin_module_state
                                        .syntax
                                        .declarations
                                        .iter()
                                        .find_map(|origin_module_declaration| {
                                            let origin_module_declaration_node =
                                                origin_module_declaration.declaration.as_ref()?;
                                            match &origin_module_declaration_node.value {
                                                ElmSyntaxDeclaration::Variable {
                                                    start_name: origin_module_declaration_name,
                                                    signature: origin_module_declaration_signature,
                                                    ..
                                                } if &origin_module_declaration_name.value
                                                    == &origin_module_declaration_function_node
                                                        .value =>
                                                {
                                                    Some((
                                                        origin_module_declaration_signature
                                                            .as_ref(),
                                                        origin_module_declaration
                                                            .documentation
                                                            .as_ref()
                                                            .map(|node| node.value.as_str()),
                                                    ))
                                                }
                                                _ => None,
                                            }
                                        })
                                });
                            completion_items.push(lsp_types::CompletionItem {
                                label: format!("({})", operator_node.value),
                                kind: Some(lsp_types::CompletionItemKind::OPERATOR),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_operator_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            Some(operator_node.value),
                                            maybe_origin_operator_function_declaration,
                                            origin_module_declaration_documentation,
                                            maybe_precedence.map(|node| node.value),
                                            maybe_direction.map(|node| node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification: to_complete_qualification,
            name: to_complete_name,
            local_bindings,
        } => {
            let maybe_completion_module_name: Option<&str> = completion_project_module
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str());
            let full_name_to_complete: String = if to_complete_qualification.is_empty() {
                format!("{to_complete_name}")
            } else {
                format!("{to_complete_qualification}.{to_complete_name}")
            };
            let to_complete_module_import_alias_origin_lookup: Vec<ElmImportAliasAndModuleOrigin> =
                elm_syntax_imports_create_import_alias_origin_lookup(
                    &completion_project_module.module.syntax.imports,
                );
            let mut completion_items: Vec<lsp_types::CompletionItem> = if (to_complete_name
                .is_empty())
                || to_complete_name.starts_with(char::is_uppercase)
            {
                project_module_name_completions_for_except(
                    state,
                    &completion_project_module.project,
                    &to_complete_module_import_alias_origin_lookup,
                    &full_name_to_complete,
                    maybe_completion_module_name,
                )
            } else if to_complete_qualification.is_empty() {
                local_bindings
                    .into_iter()
                    .flat_map(|(_, scope_introduced_bindings)| {
                        scope_introduced_bindings.into_iter()
                    })
                    .map(|local_binding| lsp_types::CompletionItem {
                        label: local_binding.name.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::VARIABLE),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: local_binding_info_markdown(
                                    state,
                                    completion_project_module,
                                    local_binding.name,
                                    local_binding.origin,
                                ),
                            },
                        )),
                        ..lsp_types::CompletionItem::default()
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let to_complete_module_origins: Vec<&str> = if to_complete_qualification.is_empty() {
                vec![maybe_completion_module_name.unwrap_or("")]
            } else {
                look_up_import_alias_module_origins(
                    &to_complete_module_import_alias_origin_lookup,
                    to_complete_qualification,
                )
                .unwrap_or_else(|| vec![to_complete_qualification])
            };
            for to_complete_module_origin in to_complete_module_origins {
                if let Some((_, to_complete_origin_module_state)) =
                    project_state_get_module_with_name(
                        state,
                        completion_project_module.project,
                        to_complete_module_origin,
                    )
                {
                    let module_origin_lookup: ModuleOriginLookup =
                        elm_syntax_module_create_origin_lookup(
                            state,
                            completion_project_module.project,
                            &to_complete_origin_module_state.syntax,
                        );
                    let origin_module_expose_set: ElmModuleHeaderExposeSet =
                        if Some(to_complete_module_origin) == maybe_completion_module_name {
                            ElmModuleHeaderExposeSet::All
                        } else {
                            elm_syntax_module_header_expose_set(
                                to_complete_origin_module_state.syntax.header.as_ref(),
                            )
                        };
                    for (origin_module_declaration_node, origin_module_declaration_documentation) in
                        to_complete_origin_module_state
                            .syntax
                            .declarations
                            .iter()
                            .filter_map(|documented_declaration| {
                                documented_declaration.declaration.as_ref().map(
                                    |declaration_node| {
                                        (
                                            declaration_node,
                                            documented_declaration
                                                .documentation
                                                .as_ref()
                                                .map(|node| node.value.as_str()),
                                        )
                                    },
                                )
                            })
                    {
                        match &origin_module_declaration_node.value {
                            ElmSyntaxDeclaration::ChoiceType {
                                name: maybe_choice_type_name,
                                parameters,
                                equals_key_symbol_range: _,
                                variant0_name,
                                variant0_values,
                                variant1_up,
                            } => {
                                if let Some(choice_type_name_node) = maybe_choice_type_name
                                    && elm_expose_set_contains_choice_type_including_variants(
                                        &origin_module_expose_set,
                                        &choice_type_name_node.value,
                                    )
                                {
                                    let info_markdown: String = format!(
                                        "variant in\n{}",
                                        present_choice_type_declaration_info_markdown(
                                            &module_origin_lookup,
                                            to_complete_module_origin,
                                            &choice_type_name_node.value,
                                            origin_module_declaration_documentation,
                                            &parameters,
                                            variant0_name.as_ref().map(|node| node.value.as_str()),
                                            &variant0_values,
                                            &variant1_up,
                                        ),
                                    );
                                    completion_items.extend(
                                        variant0_name
                                            .as_ref()
                                            .map(|node| node.value.clone())
                                            .into_iter()
                                            .chain(variant1_up.iter().filter_map(|variant| {
                                                variant.name.as_ref().map(|node| node.value.clone())
                                            }))
                                            .map(|variant_name: String| {
                                                lsp_types::CompletionItem {
                                                    label: variant_name,
                                                    kind: Some(
                                                        lsp_types::CompletionItemKind::ENUM_MEMBER,
                                                    ),
                                                    documentation: Some(
                                                        lsp_types::Documentation::MarkupContent(
                                                            lsp_types::MarkupContent {
                                                                kind:
                                                                    lsp_types::MarkupKind::Markdown,
                                                                value: info_markdown.clone(),
                                                            },
                                                        ),
                                                    ),
                                                    ..lsp_types::CompletionItem::default()
                                                }
                                            }),
                                    );
                                }
                            }
                            ElmSyntaxDeclaration::Port {
                                name,
                                colon_key_symbol_range: _,
                                type_,
                            } => {
                                if let Some(name_node) = name
                                    && elm_expose_set_contains_variable(
                                        &origin_module_expose_set,
                                        &name_node.value,
                                    )
                                {
                                    completion_items.push(lsp_types::CompletionItem {
                                        label: name_node.value.clone(),
                                        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                        documentation: Some(
                                            lsp_types::Documentation::MarkupContent(
                                                lsp_types::MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
                                                    value: present_port_declaration_info_markdown(
                                                        &module_origin_lookup,
                                                        to_complete_module_origin,
                                                        &name_node.value,
                                                        origin_module_declaration_documentation,
                                                        type_.as_ref().map(|node| &node.value),
                                                    ),
                                                },
                                            ),
                                        ),
                                        ..lsp_types::CompletionItem::default()
                                    })
                                }
                            }
                            ElmSyntaxDeclaration::TypeAlias {
                                alias_keyword_range: _,
                                name: maybe_name,
                                parameters,
                                equals_key_symbol_range: _,
                                type_: maybe_type,
                            } => {
                                if let Some(name_node) = maybe_name
                                    && elm_expose_set_contains_type(
                                        &origin_module_expose_set,
                                        &name_node.value,
                                    )
                                {
                                    if let Some(type_node) = maybe_type
                                        && let ElmSyntaxType::Record(_) = type_node.value
                                    {
                                        completion_items.push(lsp_types::CompletionItem {
                                            label: name_node.value.clone(),
                                            kind: Some(lsp_types::CompletionItemKind::CONSTRUCTOR),
                                            documentation: Some(
                                                lsp_types::Documentation::MarkupContent(
                                                    lsp_types::MarkupContent {
                                                        kind: lsp_types::MarkupKind::Markdown,
                                                        value: format!(
                                                "constructor function for record\n{}",
                                                &present_type_alias_declaration_info_markdown(
                                                    &module_origin_lookup,
                                                    to_complete_module_origin,
                                                    &name_node.value,
                                                    origin_module_declaration_documentation,
                                                    parameters,
                                                    Some(&type_node.value),
                                                )
                                            ),
                                                    },
                                                ),
                                            ),
                                            ..lsp_types::CompletionItem::default()
                                        })
                                    }
                                }
                            }
                            ElmSyntaxDeclaration::Variable {
                                start_name: start_name_node,
                                signature: maybe_signature,
                                parameters: _,
                                equals_key_symbol_range: _,
                                result: _,
                            } => {
                                if elm_expose_set_contains_variable(
                                    &origin_module_expose_set,
                                    &start_name_node.value,
                                ) {
                                    completion_items.push(lsp_types::CompletionItem {
                                        label: start_name_node.value.clone(),
                                        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                        documentation: Some(
                                            lsp_types::Documentation::MarkupContent(
                                                lsp_types::MarkupContent {
                                                    kind: lsp_types::MarkupKind::Markdown,
                                                    value:
                                                        present_variable_declaration_info_markdown(
                                                            &module_origin_lookup,
                                                            to_complete_module_origin,
                                                            &start_name_node.value,
                                                            origin_module_declaration_documentation,
                                                            maybe_signature
                                                                .as_ref()
                                                                .and_then(|signature| {
                                                                    signature.type_.as_ref()
                                                                })
                                                                .map(|node| &node.value),
                                                        ),
                                                },
                                            ),
                                        ),
                                        ..lsp_types::CompletionItem::default()
                                    })
                                }
                            }
                            ElmSyntaxDeclaration::Operator { .. } => {
                                // suggesting operators is really confusing I think.
                                // Also, wether it needs to be surrounded by parens
                                // is not super easy to find out
                            }
                        }
                    }
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::Type {
            qualification: to_complete_qualification,
            name: to_complete_name,
        } => {
            let maybe_completion_module_name: Option<&str> = completion_project_module
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_str());
            let completion_module_name: &str = maybe_completion_module_name.unwrap_or("");
            let full_name_to_complete: String = if to_complete_qualification.is_empty() {
                format!("{to_complete_name}")
            } else {
                format!("{to_complete_qualification}.{to_complete_name}")
            };
            let to_complete_module_import_alias_origin_lookup: Vec<ElmImportAliasAndModuleOrigin> =
                elm_syntax_imports_create_import_alias_origin_lookup(
                    &completion_project_module.module.syntax.imports,
                );
            let mut completion_items: Vec<lsp_types::CompletionItem> =
                project_module_name_completions_for_except(
                    state,
                    completion_project_module.project,
                    &to_complete_module_import_alias_origin_lookup,
                    &full_name_to_complete,
                    maybe_completion_module_name,
                );
            let to_complete_module_origins: Vec<&str> = if to_complete_qualification.is_empty() {
                vec![completion_module_name]
            } else {
                look_up_import_alias_module_origins(
                    &to_complete_module_import_alias_origin_lookup,
                    to_complete_qualification,
                )
                .unwrap_or_else(|| vec![to_complete_qualification])
            };
            for to_complete_module_origin in to_complete_module_origins {
                if let Some((_, origin_module_state)) = project_state_get_module_with_name(
                    state,
                    completion_project_module.project,
                    to_complete_module_origin,
                ) {
                    let module_origin_lookup: ModuleOriginLookup =
                        elm_syntax_module_create_origin_lookup(
                            state,
                            completion_project_module.project,
                            &origin_module_state.syntax,
                        );
                    let origin_module_expose_set: ElmModuleHeaderExposeSet =
                        if to_complete_module_origin == completion_module_name {
                            ElmModuleHeaderExposeSet::All
                        } else {
                            elm_syntax_module_header_expose_set(
                                origin_module_state.syntax.header.as_ref(),
                            )
                        };
                    for (origin_module_declaration_node, origin_module_declaration_documentation) in
                        origin_module_state.syntax.declarations.iter().filter_map(
                            |documented_declaration| {
                                documented_declaration.declaration.as_ref().map(
                                    |declaration_node| {
                                        (
                                            declaration_node,
                                            documented_declaration
                                                .documentation
                                                .as_ref()
                                                .map(|node| node.value.as_str()),
                                        )
                                    },
                                )
                            },
                        )
                    {
                        match &origin_module_declaration_node.value {
                            ElmSyntaxDeclaration::ChoiceType {
                                name: maybe_name,
                                parameters,
                                equals_key_symbol_range: _,
                                variant0_name: maybe_variant0_name,
                                variant0_values,
                                variant1_up,
                            } => {
                                if let Some(name_node) = maybe_name.as_ref()
                                    && elm_expose_set_contains_type(
                                        &origin_module_expose_set,
                                        &name_node.value,
                                    )
                                {
                                    completion_items.push(lsp_types::CompletionItem {
                                    label: name_node.value.clone(),
                                    kind: Some(lsp_types::CompletionItemKind::ENUM),
                                    documentation: Some(lsp_types::Documentation::MarkupContent(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_choice_type_declaration_info_markdown(
                                                &module_origin_lookup,
                                                to_complete_module_origin,
                                                &name_node.value,
                                                origin_module_declaration_documentation,
                                                parameters,
                                                maybe_variant0_name
                                                    .as_ref()
                                                    .map(|node| node.value.as_str()),
                                                &variant0_values,
                                                &variant1_up,
                                            ),
                                        },
                                    )),
                                    ..lsp_types::CompletionItem::default()
                                });
                                }
                            }
                            ElmSyntaxDeclaration::TypeAlias {
                                alias_keyword_range: _,
                                name: maybe_name,
                                parameters,
                                equals_key_symbol_range: _,
                                type_,
                            } => {
                                if let Some(name_node) = maybe_name.as_ref()
                                    && elm_expose_set_contains_type(
                                        &origin_module_expose_set,
                                        &name_node.value,
                                    )
                                {
                                    completion_items.push(lsp_types::CompletionItem {
                                    label: name_node.value.clone(),
                                    kind: Some(lsp_types::CompletionItemKind::STRUCT),
                                    documentation: Some(lsp_types::Documentation::MarkupContent(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_type_alias_declaration_info_markdown(
                                                &module_origin_lookup,
                                                to_complete_module_origin,
                                                &name_node.value,
                                                origin_module_declaration_documentation,
                                                parameters,
                                                type_.as_ref().map(|node| &node.value),
                                            ),
                                        },
                                    )),
                                    ..lsp_types::CompletionItem::default()
                                });
                                }
                            }
                            ElmSyntaxDeclaration::Port { .. } => {}
                            ElmSyntaxDeclaration::Variable { .. } => {}
                            ElmSyntaxDeclaration::Operator { .. } => {}
                        }
                    }
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::TypeVariable { .. } => {
            // is this ever useful to add? elm tends to use single-letter names anyway most of the time
            // (or ones where the first letters don't match in the first place).
            // suggesting completions can get annoying and isn't free computationally so...
            None
        }
    };
    maybe_completion_items.map(lsp_types::CompletionResponse::Array)
}

fn elm_make_file_problem_to_diagnostic(
    problem: &ElmMakeFileInternalCompileProblem,
) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: problem.range,
        severity: Some(lsp_types::DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: None,
        message: format!("--- {} ---\n{}", &problem.title, &problem.message_markdown),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn project_module_name_completions_for_except(
    state: &State,
    completion_project: &ProjectState,
    import_alias_origin_lookup: &[ElmImportAliasAndModuleOrigin],
    module_name_to_complete: &str,
    module_name_exception: Option<&str>,
) -> Vec<lsp_types::CompletionItem> {
    let module_name_base_to_complete: String = module_name_to_complete
        .rsplit_once(".")
        .map(|(before_last_dot, _)| before_last_dot.to_string() + ".")
        .unwrap_or("".to_string());
    completion_project
        .dependency_exposed_module_names
        .iter()
        .flat_map(
            |(importable_dependency_module_name, importable_dependency_module_origin)| {
                let importable_dependency_module_name_or_aliases: Vec<&str> =
                    look_up_module_origin_import_aliases(
                        import_alias_origin_lookup,
                        importable_dependency_module_name,
                    )
                    .unwrap_or_else(|| vec![importable_dependency_module_name]);
                importable_dependency_module_name_or_aliases
                    .into_iter()
                    .filter_map(|importable_dependency_module_name_or_alias| {
                        if !importable_dependency_module_name_or_alias
                            .starts_with(&module_name_base_to_complete)
                            || module_name_base_to_complete
                                .starts_with(importable_dependency_module_name_or_alias)
                        {
                            return None;
                        }
                        let importable_dependency_module_state = state
                            .projects
                            .get(&importable_dependency_module_origin.project_path)
                            .and_then(|dependency_state| {
                                dependency_state
                                    .modules
                                    .get(&importable_dependency_module_origin.module_path)
                            })?;
                        Some((
                            importable_dependency_module_name_or_alias,
                            importable_dependency_module_state
                                .syntax
                                .documentation
                                .as_ref(),
                        ))
                    })
            },
        )
        .chain(
            completion_project
                .modules
                .iter()
                .flat_map(|(_, project_module)| {
                    project_module
                        .syntax
                        .header
                        .as_ref()
                        .and_then(|header| header.module_name.as_ref())
                        .map(|project_module_name_node| {
                            let project_module_name: &str = project_module_name_node.value.as_str();
                            let importable_dependency_module_name_or_aliases: Vec<&str> =
                                look_up_module_origin_import_aliases(
                                    import_alias_origin_lookup,
                                    project_module_name,
                                )
                                .unwrap_or_else(|| vec![project_module_name]);
                            importable_dependency_module_name_or_aliases
                                .into_iter()
                                .filter_map(|importable_dependency_module_name_or_alias| {
                                    if !importable_dependency_module_name_or_alias
                                        .starts_with(&module_name_base_to_complete)
                                        || module_name_base_to_complete
                                            .starts_with(importable_dependency_module_name_or_alias)
                                        || Some(importable_dependency_module_name_or_alias)
                                            == module_name_exception
                                    {
                                        None
                                    } else {
                                        Some((
                                            importable_dependency_module_name_or_alias,
                                            project_module.syntax.documentation.as_ref(),
                                        ))
                                    }
                                })
                        })
                        .into_iter()
                        .flatten()
                }),
        )
        .map(
            |(module_name, maybe_module_documentation)| lsp_types::CompletionItem {
                label: module_name.to_string(),
                insert_text: Some(
                    module_name
                        .strip_prefix(&module_name_base_to_complete)
                        .unwrap_or(module_name)
                        .to_string(),
                ),
                sort_text: Some(
                    module_name
                        .strip_prefix(&module_name_base_to_complete)
                        .unwrap_or(module_name)
                        .to_string(),
                ),
                kind: Some(lsp_types::CompletionItemKind::MODULE),
                documentation: Some(lsp_types::Documentation::MarkupContent(
                    lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: maybe_module_documentation
                            .map(|module_documentation| {
                                documentation_comment_to_markdown(&module_documentation.value)
                            })
                            .unwrap_or_else(|| "_module has no documentation comment_".to_string()),
                    },
                )),
                ..lsp_types::CompletionItem::default()
            },
        )
        .collect::<Vec<_>>()
}

fn state_update_source_at_path(
    state: &mut State,
    changed_path: &std::path::PathBuf,
    changed_source: String,
) {
    for project_state in state.projects.values_mut() {
        if project_state.modules.contains_key(changed_path)
            || project_state
                .source_directories
                .iter()
                .any(|source_directory_path| changed_path.starts_with(source_directory_path))
        {
            project_state.modules.insert(
                changed_path.clone(),
                ModuleState {
                    syntax: parse_elm_syntax_module(&changed_source),
                },
            );
            // we do not break out of the loop because
            // source directories can be (partially) shared between applications
        }
    }
}

fn documentation_comment_to_markdown(documentation: &str) -> String {
    let markdown_source: &str = documentation
        .trim_start_matches("{-|")
        .trim_end_matches("-}")
        .trim();

    // because I don't want to introduce a full markdown parser for just this tiny
    // improvement, the code below only approximates where code blocks are.
    let with_fenced_blocks_converted: String =
        markdown_convert_unspecific_fenced_code_blocks_to_elm(markdown_source);
    markdown_convert_indented_code_blocks_to_elm(&with_fenced_blocks_converted)
}

/// replace fenced no-language-specified code blocks by `elm...`
fn markdown_convert_unspecific_fenced_code_blocks_to_elm(markdown_source: &str) -> String {
    let mut result_builder: String = String::new();
    let mut current_source_index: usize = 0;
    'converting_fenced: while current_source_index < markdown_source.len() {
        match markdown_source[current_source_index..]
            .find("```")
            .map(|i| i + current_source_index)
        {
            None => {
                result_builder.push_str(&markdown_source[current_source_index..]);
                break 'converting_fenced;
            }
            Some(index_at_opening_fence) => {
                let index_after_opening_fence = index_at_opening_fence + 3;
                match markdown_source[index_after_opening_fence..]
                    .find("```")
                    .map(|i| i + index_after_opening_fence)
                {
                    None => {
                        result_builder.push_str(&markdown_source[current_source_index..]);
                        break 'converting_fenced;
                    }
                    Some(index_at_closing_fence) => {
                        match markdown_source[index_after_opening_fence..].chars().next() {
                            // fenced block without a specific language
                            Some('\n') => {
                                result_builder.push_str(
                                    &markdown_source[current_source_index..index_at_opening_fence],
                                );
                                result_builder.push_str("```elm");
                                result_builder.push_str(
                                    &markdown_source
                                        [index_after_opening_fence..index_at_closing_fence],
                                );
                                result_builder.push_str("```");
                                current_source_index = index_at_closing_fence + 3;
                            }
                            // fenced block with a specific language
                            _ => {
                                result_builder.push_str(
                                    &markdown_source
                                        [current_source_index..(index_at_closing_fence + 3)],
                                );
                                current_source_index = index_at_closing_fence + 3;
                            }
                        }
                    }
                }
            }
        }
    }
    result_builder
}

fn markdown_convert_indented_code_blocks_to_elm(markdown_source: &str) -> String {
    let mut result_builder: String = String::new();
    let mut current_indent: usize = 0;
    let mut is_in_code_block = false;
    for source_line in markdown_source.lines() {
        let current_line_indent = source_line
            .chars()
            .take_while(|c| c.is_ascii_whitespace())
            .count();
        if current_line_indent == source_line.len() {
            // ignore blank line
            result_builder.push_str(source_line);
            result_builder.push('\n');
        } else if current_line_indent >= current_indent + 4 {
            is_in_code_block = true;
            current_indent = current_line_indent;
            result_builder.push_str("```elm\n");
            result_builder.push_str(&source_line[current_line_indent..]);
            result_builder.push('\n');
        } else if is_in_code_block {
            if current_line_indent <= current_indent - 4 {
                is_in_code_block = false;
                current_indent = current_line_indent;
                result_builder.push_str("```\n");
                result_builder.push_str(source_line);
                result_builder.push('\n');
            } else {
                result_builder.push_str(&source_line[current_indent..]);
                result_builder.push('\n');
            }
        } else {
            current_indent = current_line_indent;
            result_builder.push_str(source_line);
            result_builder.push('\n');
        }
    }
    if is_in_code_block {
        result_builder.push_str("```\n");
    }
    result_builder
}

fn lsp_range_includes_position(range: lsp_types::Range, position: lsp_types::Position) -> bool {
    (
        // position >= range.start
        (position.line > range.start.line)
            || ((position.line == range.start.line)
                && (position.character >= range.start.character))
    ) && (
        // position <= range.end
        (position.line < range.end.line)
            || ((position.line == range.end.line) && (position.character <= range.end.character))
    )
}

fn lsp_position_compare(a: lsp_types::Position, b: lsp_types::Position) -> std::cmp::Ordering {
    if a.line < b.line {
        std::cmp::Ordering::Less
    } else if a.line > b.line {
        std::cmp::Ordering::Greater
    } else
    // (a.line == b.line)
    {
        std::cmp::Ord::cmp(&a, &b)
    }
}
struct PositionDelta {
    line: u32,
    character: u32,
}
fn lsp_position_positive_delta(
    before: lsp_types::Position,
    after: lsp_types::Position,
) -> Result<PositionDelta, String> {
    if before.line > after.line {
        Err(format!(
            "before line > after line (before: {}, after {})",
            lsp_position_to_string(before),
            lsp_position_to_string(after)
        ))
    } else if before.line == after.line {
        if before.character > after.character {
            Err(format!(
                "before character > after character (before: {}, after {})",
                lsp_position_to_string(before),
                lsp_position_to_string(after)
            ))
        } else {
            Ok(PositionDelta {
                line: 0,
                character: after.character - before.character,
            })
        }
    } else
    // before.line < after.line
    {
        Ok(PositionDelta {
            line: after.line - before.line,
            character: after.character,
        })
    }
}
fn lsp_position_to_string(lsp_position: lsp_types::Position) -> String {
    format!("{}:{}", lsp_position.line, lsp_position.character)
}

fn lsp_position_add_characters(
    position: lsp_types::Position,
    additional_character_count: i32,
) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line,
        character: (position.character as i32 + additional_character_count) as u32,
    }
}

fn elm_syntax_highlight_kind_to_lsp_semantic_token_type(
    elm_syntax_highlight_kind: ElmSyntaxHighlightKind,
) -> lsp_types::SemanticTokenType {
    match elm_syntax_highlight_kind {
        ElmSyntaxHighlightKind::KeySymbol => lsp_types::SemanticTokenType::KEYWORD,
        ElmSyntaxHighlightKind::Field => lsp_types::SemanticTokenType::PROPERTY,
        ElmSyntaxHighlightKind::ModuleNameOrAlias => lsp_types::SemanticTokenType::NAMESPACE,
        ElmSyntaxHighlightKind::Type => lsp_types::SemanticTokenType::TYPE,
        ElmSyntaxHighlightKind::Variable => lsp_types::SemanticTokenType::VARIABLE,
        ElmSyntaxHighlightKind::Variant => lsp_types::SemanticTokenType::ENUM_MEMBER,
        ElmSyntaxHighlightKind::VariableDeclaration => lsp_types::SemanticTokenType::FUNCTION,
        ElmSyntaxHighlightKind::Comment => lsp_types::SemanticTokenType::COMMENT,
        ElmSyntaxHighlightKind::Number => lsp_types::SemanticTokenType::NUMBER,
        ElmSyntaxHighlightKind::String => lsp_types::SemanticTokenType::STRING,
        ElmSyntaxHighlightKind::TypeVariable => lsp_types::SemanticTokenType::TYPE_PARAMETER,
    }
}

fn list_elm_files_in_source_directory_at_path(
    path: &std::path::PathBuf,
) -> std::io::Result<Vec<(std::path::PathBuf, String)>> {
    let mut result: Vec<(std::path::PathBuf, String)> = Vec::new();
    list_elm_files_in_source_directory_at_path_into(&mut result, path.into()).map(|()| result)
}

fn list_elm_files_in_source_directory_at_path_into(
    so_far: &mut Vec<(std::path::PathBuf, String)>,
    path: std::path::PathBuf,
) -> std::io::Result<()> {
    if path.is_dir() {
        for dir_sub in std::fs::read_dir(&path)? {
            list_elm_files_in_source_directory_at_path_into(so_far, dir_sub?.path())?;
        }
    } else {
        match path.extension() {
            Option::None => {}
            Option::Some(file_type) => {
                if (file_type == "elm") || (file_type == "elm-testing") {
                    let file_content: String = std::fs::read_to_string(&path)?;
                    so_far.push((path, file_content));
                }
            }
        }
    }
    Ok(())
}

// // // below persistent rust types and conversions to and from temporary elm types
#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxType {
    Unit,
    Variable(String),
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxType>>),
    Tuple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
    },
    Triple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
        part2: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
    },
    Function {
        input: ElmSyntaxNode<Box<ElmSyntaxType>>,
        arrow_key_symbol_range: lsp_types::Range,
        output: Option<ElmSyntaxNode<Box<ElmSyntaxType>>>,
    },
    Construct {
        reference: ElmSyntaxNode<ElmQualifiedName>,
        arguments: Vec<ElmSyntaxNode<ElmSyntaxType>>,
    },
    Record(Vec<ElmSyntaxTypeField>),
    RecordExtension {
        record_variable: Option<ElmSyntaxNode<String>>,
        bar_key_symbol_range: lsp_types::Range,
        fields: Vec<ElmSyntaxTypeField>,
    },
}
#[derive(Clone, Debug, PartialEq)]
struct ElmQualifiedName {
    qualification: String,
    name: String,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxTypeField {
    name: ElmSyntaxNode<String>,
    colon_key_symbol_range: Option<lsp_types::Range>,
    value: Option<ElmSyntaxNode<ElmSyntaxType>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxPattern {
    Unit,
    Ignored,
    Char(Option<char>),
    Int {
        base: ElmSyntaxIntBase,
        value: Option<i64>,
    },
    String {
        content: String,
        quoting_style: ElmSyntaxStringQuotingStyle,
    },
    Variable(String),
    As {
        pattern: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        as_keyword_range: lsp_types::Range,
        variable: Option<ElmSyntaxNode<String>>,
    },
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxPattern>>),
    Tuple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
    },
    Triple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
        part2: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
    },
    ListCons {
        head: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
        cons_key_symbol: lsp_types::Range,
        tail: Option<ElmSyntaxNode<Box<ElmSyntaxPattern>>>,
    },
    ListExact(Vec<ElmSyntaxNode<ElmSyntaxPattern>>),
    Record(Vec<ElmSyntaxNode<String>>),
    Variant {
        reference: ElmSyntaxNode<ElmQualifiedName>,
        values: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ElmSyntaxStringQuotingStyle {
    SingleQuoted,
    TripleQuoted,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxModuleHeaderSpecific {
    // if you have a better name for this, please tell me
    Pure {
        module_keyword_range: lsp_types::Range,
    },
    Port {
        port_keyword_range: lsp_types::Range,
        module_keyword_range: lsp_types::Range,
    },
    Effect {
        effect_keyword_range: lsp_types::Range,
        module_keyword_range: lsp_types::Range,
        where_keyword_range: lsp_types::Range,
        command: Option<EffectModuleHeaderEntry>,
        subscription: Option<EffectModuleHeaderEntry>,
    },
}
#[derive(Clone, Debug, PartialEq)]
struct EffectModuleHeaderEntry {
    key_range: lsp_types::Range,
    equals_range: lsp_types::Range,
    value_type_name: ElmSyntaxNode<String>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxLetDeclaration {
    Destructuring {
        pattern: ElmSyntaxNode<ElmSyntaxPattern>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        expression: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
    VariableDeclaration {
        start_name: ElmSyntaxNode<String>,
        signature: Option<ElmSyntaxVariableDeclarationSignature>,
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        result: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxVariableDeclarationSignature {
    colon_key_symbol_range: lsp_types::Range,
    type_: Option<ElmSyntaxNode<ElmSyntaxType>>,
    implementation_name_range: Option<lsp_types::Range>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExpression {
    Unit,
    Call {
        called: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        argument0: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        argument1_up: Vec<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
    CaseOf {
        matched: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        of_keyword_range: Option<lsp_types::Range>,
        cases: Vec<ElmSyntaxExpressionCase>,
    },
    Char(Option<char>),
    Float(Option<f64>),
    IfThenElse {
        condition: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        then_keyword_range: Option<lsp_types::Range>,
        on_true: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        else_keyword_range: Option<lsp_types::Range>,
        on_false: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
    /// elm-syntax for example uses a pratt parser
    /// to produce the actual, semantically correct tree as you would evaluate it.
    /// However, such a tree is irrelevant
    /// for the existing functionality of the language server,
    /// so we instead simply parse it "left, grouping right". For example:
    ///
    ///     3 + 4 * 5 - 6
    ///
    /// in elm-syntax:
    ///
    ///     Op (Op 3 "+" (Op 4 "*" 5)) "-" 6
    ///
    /// here:
    ///
    ///     Op 3 "+" (Op 4 "*" (Op 5 "-" 6))
    InfixOperationIgnoringPrecedence {
        left: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        operator: ElmSyntaxNode<&'static str>,
        right: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
    Integer {
        value: Option<i64>,
        base: ElmSyntaxIntBase,
    },
    Lambda {
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        arrow_key_symbol_range: Option<lsp_types::Range>,
        result: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
    LetIn {
        declarations: Vec<ElmSyntaxNode<ElmSyntaxLetDeclaration>>,
        in_keyword_range: Option<lsp_types::Range>,
        result: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
    List(Vec<ElmSyntaxNode<ElmSyntaxExpression>>),
    Negation(Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>),
    OperatorFunction(ElmSyntaxNode<&'static str>),
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxExpression>>),
    Record(Vec<ElmSyntaxExpressionField>),
    RecordAccess {
        record: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        field: Option<ElmSyntaxNode<String>>,
    },
    RecordAccessFunction(Option<ElmSyntaxNode<String>>),
    RecordUpdate {
        record_variable: Option<ElmSyntaxNode<String>>,
        bar_key_symbol_range: lsp_types::Range,
        fields: Vec<ElmSyntaxExpressionField>,
    },
    Reference {
        qualification: String,
        name: String,
    },
    String {
        content: String,
        quoting_style: ElmSyntaxStringQuotingStyle,
    },
    Triple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        part2: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
    Tuple {
        part0: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
        part1: Option<ElmSyntaxNode<Box<ElmSyntaxExpression>>>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ElmSyntaxIntBase {
    IntBase10,
    IntBase16,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxExpressionCase {
    arrow_key_symbol_range: Option<lsp_types::Range>,
    pattern: ElmSyntaxNode<ElmSyntaxPattern>,
    result: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxExpressionField {
    name: ElmSyntaxNode<String>,
    equals_key_symbol_range: Option<lsp_types::Range>,
    value: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxExposing {
    exposing_keyword_range: lsp_types::Range,
    specific: Option<ElmSyntaxNode<ElmSyntaxExposingSpecific>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExposingSpecific {
    All(lsp_types::Range),
    Explicit(Vec<ElmSyntaxNode<ElmSyntaxExpose>>),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExpose {
    ChoiceTypeIncludingVariants {
        name: ElmSyntaxNode<String>,
        open_range: Option<lsp_types::Range>,
    },
    Operator(Option<ElmSyntaxNode<&'static str>>),
    Type(String),
    Variable(String),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxDeclaration {
    ChoiceType {
        name: Option<ElmSyntaxNode<String>>,
        parameters: Vec<ElmSyntaxNode<String>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        variant0_name: Option<ElmSyntaxNode<String>>,
        variant0_values: Vec<ElmSyntaxNode<ElmSyntaxType>>,
        variant1_up: Vec<ElmSyntaxChoiceTypeDeclarationTailingVariant>,
    },
    Operator {
        direction: Option<ElmSyntaxNode<ElmSyntaxInfixDirection>>,
        precedence: Option<ElmSyntaxNode<i64>>,
        operator: Option<ElmSyntaxNode<&'static str>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        function: Option<ElmSyntaxNode<String>>,
    },
    Port {
        name: Option<ElmSyntaxNode<String>>,
        colon_key_symbol_range: Option<lsp_types::Range>,
        type_: Option<ElmSyntaxNode<ElmSyntaxType>>,
    },
    TypeAlias {
        alias_keyword_range: lsp_types::Range,
        name: Option<ElmSyntaxNode<String>>,
        parameters: Vec<ElmSyntaxNode<String>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        type_: Option<ElmSyntaxNode<ElmSyntaxType>>,
    },
    Variable {
        start_name: ElmSyntaxNode<String>,
        signature: Option<ElmSyntaxVariableDeclarationSignature>,
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        result: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ElmSyntaxInfixDirection {
    Left,
    Non,
    Right,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxChoiceTypeDeclarationTailingVariant {
    or_key_symbol_range: lsp_types::Range,
    name: Option<ElmSyntaxNode<String>>,
    values: Vec<ElmSyntaxNode<ElmSyntaxType>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct ElmSyntaxNode<Value> {
    range: lsp_types::Range,
    value: Value,
}

fn elm_syntax_node_as_ref<'a, Value>(
    elm_syntax_node: &'a ElmSyntaxNode<Value>,
) -> ElmSyntaxNode<&'a Value> {
    ElmSyntaxNode {
        range: elm_syntax_node.range,
        value: &elm_syntax_node.value,
    }
}

fn elm_syntax_node_unbox<'a, Value>(
    elm_syntax_node_box: &'a ElmSyntaxNode<Box<Value>>,
) -> ElmSyntaxNode<&'a Value> {
    ElmSyntaxNode {
        range: elm_syntax_node_box.range,
        value: &elm_syntax_node_box.value,
    }
}
fn elm_syntax_node_box<Value>(
    elm_syntax_node_box: ElmSyntaxNode<Value>,
) -> ElmSyntaxNode<Box<Value>> {
    ElmSyntaxNode {
        range: elm_syntax_node_box.range,
        value: Box::new(elm_syntax_node_box.value),
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxModuleHeader {
    specific: ElmSyntaxModuleHeaderSpecific,
    module_name: Option<ElmSyntaxNode<String>>,
    exposing: Option<ElmSyntaxExposing>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxModule {
    header: Option<ElmSyntaxModuleHeader>,
    documentation: Option<ElmSyntaxNode<String>>,
    imports: Vec<ElmSyntaxNode<ElmSyntaxImport>>,
    comments: Vec<ElmSyntaxNode<ElmSyntaxComment>>,
    declarations: Vec<ElmSyntaxDocumentedDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxDocumentedDeclaration {
    documentation: Option<ElmSyntaxNode<String>>,
    declaration: Option<ElmSyntaxNode<ElmSyntaxDeclaration>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxImport {
    module_name: Option<ElmSyntaxNode<String>>,
    alias: Option<EmSyntaxImportAs>,
    exposing: Option<ElmSyntaxExposing>,
}
#[derive(Clone, Debug, PartialEq)]
struct EmSyntaxImportAs {
    as_keyword_range: lsp_types::Range,
    name: Option<ElmSyntaxNode<String>>,
}
#[derive(Clone, Debug)]
enum ElmModuleHeaderExposeSet<'a> {
    All,
    Explicit {
        operators: Vec<&'a str>,
        variables: Vec<&'a str>,
        types: Vec<&'a str>,
        choice_types_including_variants: Vec<&'a str>,
    },
}
fn elm_syntax_module_header_expose_set<'a>(
    elm_syntax_module_header: Option<&'a ElmSyntaxModuleHeader>,
) -> ElmModuleHeaderExposeSet<'a> {
    match elm_syntax_module_header
        .and_then(|header| header.exposing.as_ref())
        .and_then(|exposing| exposing.specific.as_ref())
    {
        None => ElmModuleHeaderExposeSet::All,
        Some(module_header_expose_specific_node) => {
            elm_syntax_exposing_specific_to_set(&module_header_expose_specific_node.value)
        }
    }
}
fn elm_syntax_exposing_specific_to_set<'a>(
    elm_syntax_exposing_specific: &'a ElmSyntaxExposingSpecific,
) -> ElmModuleHeaderExposeSet<'a> {
    match elm_syntax_exposing_specific {
        ElmSyntaxExposingSpecific::All(_) => ElmModuleHeaderExposeSet::All,
        ElmSyntaxExposingSpecific::Explicit(exposes) => {
            let mut operators: Vec<&str> = Vec::new();
            let mut variables: Vec<&str> = Vec::new();
            let mut types: Vec<&str> = Vec::new();
            let mut choice_types_including_variants: Vec<&str> = Vec::new();
            for expose_node in exposes {
                match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: name_node,
                        open_range: _,
                    } => {
                        choice_types_including_variants.push(&name_node.value);
                    }
                    ElmSyntaxExpose::Operator(None) => {}
                    ElmSyntaxExpose::Operator(Some(operator_node)) => {
                        operators.push(operator_node.value)
                    }
                    ElmSyntaxExpose::Type(name) => {
                        types.push(name);
                    }
                    ElmSyntaxExpose::Variable(name) => {
                        variables.push(name);
                    }
                }
            }
            ElmModuleHeaderExposeSet::Explicit {
                operators: operators,
                variables: variables,
                types: types,
                choice_types_including_variants: choice_types_including_variants,
            }
        }
    }
}
fn elm_expose_set_contains_variable(
    expose_set: &ElmModuleHeaderExposeSet,
    name_to_check: &str,
) -> bool {
    match expose_set {
        ElmModuleHeaderExposeSet::All => true,
        ElmModuleHeaderExposeSet::Explicit {
            choice_types_including_variants: _,
            types: _,
            operators: _,
            variables,
        } => variables.contains(&name_to_check),
    }
}
fn elm_expose_set_contains_type(
    expose_set: &ElmModuleHeaderExposeSet,
    name_to_check: &str,
) -> bool {
    match expose_set {
        ElmModuleHeaderExposeSet::All => true,
        ElmModuleHeaderExposeSet::Explicit {
            choice_types_including_variants: _,
            types: types,
            operators: _,
            variables: _,
        } => types.contains(&name_to_check),
    }
}
fn elm_expose_set_contains_choice_type_including_variants(
    expose_set: &ElmModuleHeaderExposeSet,
    name_to_check: &str,
) -> bool {
    match expose_set {
        ElmModuleHeaderExposeSet::All => true,
        ElmModuleHeaderExposeSet::Explicit {
            choice_types_including_variants,
            types: _,
            operators: _,
            variables: _,
        } => choice_types_including_variants.contains(&name_to_check),
    }
}
fn elm_expose_set_contains(expose_set: &ElmModuleHeaderExposeSet, name_to_check: &str) -> bool {
    match expose_set {
        ElmModuleHeaderExposeSet::All => true,
        ElmModuleHeaderExposeSet::Explicit {
            operators,
            variables,
            types,
            choice_types_including_variants,
        } => {
            operators.contains(&name_to_check)
                || variables.contains(&name_to_check)
                || types.contains(&name_to_check)
                || choice_types_including_variants.contains(&name_to_check)
        }
    }
}

/// Create through module_origin_lookup_for_implicit_imports or
/// elm_syntax_module_create_origin_lookup
struct ModuleOriginLookup<'a> {
    unqualified: std::collections::HashMap<&'a str, &'a str>,
    uniquely_qualified: std::collections::HashMap<&'a str, &'a str>,
    // in theory, uniquely_qualified and ambiguously_qualified can be combined into a
    // unified map from qualified to origin module.
    //
    // Issue is that a ModuleOriginLookup should be cheap to construct (creating an
    // entry for every member of every imported module and always looking up by
    // qualification+name pair can get somewhat expensive) and so, because
    // qualifications are rarely ambiguous in practice, we split these into the
    // common, cheap and rare, expensive parts
    ambiguously_qualified: std::collections::HashMap<ElmQualified<'a>, &'a str>,
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
struct ElmQualified<'a> {
    qualification: &'a str,
    name: &'a str,
}

fn module_origin_lookup_for_implicit_imports() -> ModuleOriginLookup<'static> {
    // https://github.com/elm/core?tab=readme-ov-file#default-imports
    ModuleOriginLookup {
        unqualified: std::collections::HashMap::from([
            ("Int", "Basics"),
            ("Float", "Basics"),
            ("+", "Basics"),
            ("-", "Basics"),
            ("*", "Basics"),
            ("/", "Basics"),
            ("//", "Basics"),
            ("^", "Basics"),
            ("toFloat", "Basics"),
            ("round", "Basics"),
            ("floor", "Basics"),
            ("ceiling", "Basics"),
            ("truncate", "Basics"),
            ("==", "Basics"),
            ("/=", "Basics"),
            ("<", "Basics"),
            (">", "Basics"),
            ("<=", "Basics"),
            (">=", "Basics"),
            ("max", "Basics"),
            ("min", "Basics"),
            ("compare", "Basics"),
            ("Order", "Basics"),
            ("LT", "Basics"),
            ("EQ", "Basics"),
            ("GT", "Basics"),
            ("Bool", "Basics"),
            ("True", "Basics"),
            ("False", "Basics"),
            ("not", "Basics"),
            ("&&", "Basics"),
            ("||", "Basics"),
            ("xor", "Basics"),
            ("++", "Basics"),
            ("modBy", "Basics"),
            ("remainderBy", "Basics"),
            ("negate", "Basics"),
            ("abs", "Basics"),
            ("clamp", "Basics"),
            ("sqrt", "Basics"),
            ("logBase", "Basics"),
            ("e", "Basics"),
            ("pi", "Basics"),
            ("cos", "Basics"),
            ("sin", "Basics"),
            ("tan", "Basics"),
            ("acos", "Basics"),
            ("asin", "Basics"),
            ("atan", "Basics"),
            ("atan2", "Basics"),
            ("degrees", "Basics"),
            ("radians", "Basics"),
            ("turns", "Basics"),
            ("toPolar", "Basics"),
            ("fromPolar", "Basics"),
            ("isNaN", "Basics"),
            ("isInfinite", "Basics"),
            ("identity", "Basics"),
            ("always", "Basics"),
            ("<|", "Basics"),
            ("|>", "Basics"),
            ("<<", "Basics"),
            (">>", "Basics"),
            ("Never", "Basics"),
            ("never", "Basics"),
            ("List", "List"),
            ("::", "List"),
            ("Maybe", "Maybe"),
            ("Just", "Maybe"),
            ("Nothing", "Maybe"),
            ("Result", "Result"),
            ("Ok", "Result"),
            ("Err", "Result"),
            ("String", "String"),
            ("Char", "Char"),
            ("Program", "Platform"),
            ("Cmd", "Platform.Cmd"),
            ("Sub", "Platform.Sub"),
        ]),
        uniquely_qualified: std::collections::HashMap::from([
            ("Basics", "Basics"),
            ("List", "List"),
            ("Maybe", "Maybe"),
            ("Result", "Result"),
            ("String", "String"),
            ("Char", "Char"),
            ("Tuple", "Tuple"),
            ("Debug", "Debug"),
            ("Platform", "Platform"),
            ("Cmd", "Platform.Cmd"),
            ("Sub", "Platform.Sub"),
        ]),
        ambiguously_qualified: std::collections::HashMap::new(),
    }
}

fn look_up_origin_module<'a>(
    module_origin_lookup: &ModuleOriginLookup<'a>,
    qualification: &'a str,
    name: &'a str,
) -> &'a str {
    match match qualification {
        "" => module_origin_lookup.unqualified.get(name),
        qualification_module_or_alias => module_origin_lookup
            .uniquely_qualified
            .get(qualification_module_or_alias),
    } {
        Some(module_origin) => *module_origin,
        None => match module_origin_lookup
            .ambiguously_qualified
            .get(&ElmQualified {
                qualification: qualification,
                name: name,
            }) {
            Some(module_origin) => *module_origin,
            None => qualification,
        },
    }
}
#[derive(Clone, Copy, Debug)]
struct ElmImportAliasAndModuleOrigin<'a> {
    alias: &'a str,
    module_origin: &'a str,
}
fn elm_syntax_imports_create_import_alias_origin_lookup<'a>(
    elm_syntax_imports: &'a [ElmSyntaxNode<ElmSyntaxImport>],
) -> Vec<ElmImportAliasAndModuleOrigin<'a>> {
    elm_syntax_imports
        .iter()
        .filter_map(|import_node| {
            let module_origin_node = import_node.value.module_name.as_ref()?;
            let alias = import_node.value.alias.as_ref()?;
            let alias_name_node = alias.name.as_ref()?;
            Some(ElmImportAliasAndModuleOrigin {
                module_origin: &module_origin_node.value,
                alias: &alias_name_node.value,
            })
        })
        .collect::<Vec<_>>()
}
fn look_up_import_alias_module_origins<'a>(
    import_alias_origin_lookup: &[ElmImportAliasAndModuleOrigin<'a>],
    alias_to_collect_origins_for: &str,
) -> Option<Vec<&'a str>> {
    let module_origins = import_alias_origin_lookup
        .iter()
        .filter_map(move |alias_and_module_origin| {
            if alias_and_module_origin.alias == alias_to_collect_origins_for {
                Some(alias_and_module_origin.module_origin)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    if module_origins.is_empty() {
        None
    } else {
        Some(module_origins)
    }
}
fn look_up_module_origin_import_aliases<'a>(
    import_alias_origin_lookup: &[ElmImportAliasAndModuleOrigin<'a>],
    module_to_collect_aliases_for: &str,
) -> Option<Vec<&'a str>> {
    let module_origins = import_alias_origin_lookup
        .iter()
        .filter_map(move |alias_and_module_origin| {
            if alias_and_module_origin.module_origin == module_to_collect_aliases_for {
                Some(alias_and_module_origin.alias)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    if module_origins.is_empty() {
        None
    } else {
        Some(module_origins)
    }
}

fn elm_syntax_module_create_origin_lookup<'a>(
    state: &'a State,
    project_state: &'a ProjectState,
    elm_syntax_module: &'a ElmSyntaxModule,
) -> ModuleOriginLookup<'a> {
    let mut module_origin_lookup: ModuleOriginLookup = module_origin_lookup_for_implicit_imports();
    let self_module_name: &str = match elm_syntax_module
        .header
        .as_ref()
        .and_then(|header| header.module_name.as_ref())
    {
        None => "",
        Some(module_header) => &module_header.value,
    };
    for declaration_node in elm_syntax_module
        .declarations
        .iter()
        .filter_map(|documented_declaration| documented_declaration.declaration.as_ref())
    {
        match &declaration_node.value {
            ElmSyntaxDeclaration::ChoiceType {
                name: maybe_name,
                parameters: _,
                equals_key_symbol_range: _,
                variant0_name: maybe_variant0_name,
                variant0_values: _,
                variant1_up: variant1_up,
            } => {
                if let Some(name_node) = maybe_name {
                    module_origin_lookup
                        .unqualified
                        .insert(&name_node.value, self_module_name);
                }
                if let Some(variant0_name_node) = maybe_variant0_name {
                    module_origin_lookup
                        .unqualified
                        .insert(&variant0_name_node.value, self_module_name);
                }
                for variant in variant1_up.iter() {
                    if let Some(variant_name_node) = variant.name.as_ref() {
                        module_origin_lookup
                            .unqualified
                            .insert(&variant_name_node.value, self_module_name);
                    }
                }
            }
            ElmSyntaxDeclaration::Operator {
                direction: _,
                precedence: _,
                equals_key_symbol_range: _,
                operator: maybe_operator,
                function: _,
            } => {
                if let Some(operator_node) = maybe_operator {
                    module_origin_lookup
                        .unqualified
                        .insert(&operator_node.value, self_module_name);
                }
            }
            ElmSyntaxDeclaration::Port {
                name: maybe_name,
                colon_key_symbol_range: _,
                type_: _,
            } => {
                if let Some(name_node) = maybe_name {
                    module_origin_lookup
                        .unqualified
                        .insert(&name_node.value, self_module_name);
                }
            }
            ElmSyntaxDeclaration::TypeAlias {
                alias_keyword_range: _,
                equals_key_symbol_range: _,
                name: maybe_name,
                parameters: _,
                type_: _,
            } => {
                if let Some(name_node) = maybe_name {
                    module_origin_lookup
                        .unqualified
                        .insert(&name_node.value, self_module_name);
                }
            }
            ElmSyntaxDeclaration::Variable {
                start_name: start_name_node,
                signature: _,
                parameters: _,
                equals_key_symbol_range: _,
                result: _,
            } => {
                module_origin_lookup
                    .unqualified
                    .insert(&start_name_node.value, self_module_name);
            }
        }
    }
    for (import_module_name, import) in elm_syntax_module.imports.iter().filter_map(|import_node| {
        import_node
            .value
            .module_name
            .as_ref()
            .map(|module_name| (&module_name.value, &import_node.value))
    }) {
        let allowed_qualification: &str = match import.alias {
            None => import_module_name,
            Some(ref import_alias) => match import_alias.name {
                Some(ref import_alias_name) => &import_alias_name.value,
                None => import_module_name,
            },
        };
        match module_origin_lookup
            .uniquely_qualified
            .remove(allowed_qualification)
        {
            None => {
                module_origin_lookup
                    .uniquely_qualified
                    .insert(allowed_qualification, import_module_name);
            }
            Some(module_origin_for_existing_qualification) => {
                if let Some((_, origin_module_state_for_existing_qualification)) =
                    project_state_get_module_with_name(
                        state,
                        project_state,
                        module_origin_for_existing_qualification,
                    )
                {
                    for imported_module_expose in elm_syntax_module_exposed_symbols(
                        &origin_module_state_for_existing_qualification.syntax,
                    ) {
                        module_origin_lookup.ambiguously_qualified.insert(
                            ElmQualified {
                                qualification: allowed_qualification,
                                name: imported_module_expose,
                            },
                            module_origin_for_existing_qualification,
                        );
                    }
                }
                if let Some((_, imported_module_state)) =
                    project_state_get_module_with_name(state, project_state, import_module_name)
                {
                    for imported_module_expose in
                        elm_syntax_module_exposed_symbols(&imported_module_state.syntax)
                    {
                        module_origin_lookup.ambiguously_qualified.insert(
                            ElmQualified {
                                qualification: allowed_qualification,
                                name: imported_module_expose,
                            },
                            import_module_name,
                        );
                    }
                }
            }
        }
        if let Some(import_exposing) = &import.exposing {
            match import_exposing.specific.as_ref().map(|node| &node.value) {
                None => {}
                Some(ElmSyntaxExposingSpecific::All(_)) => {
                    if let Some((_, imported_module_state)) =
                        project_state_get_module_with_name(state, project_state, import_module_name)
                    {
                        for import_exposed_symbol in
                            elm_syntax_module_exposed_symbols(&imported_module_state.syntax)
                        {
                            module_origin_lookup
                                .unqualified
                                .insert(import_exposed_symbol, import_module_name);
                        }
                    }
                }
                Some(ElmSyntaxExposingSpecific::Explicit(exposes)) => {
                    for expose_node in exposes {
                        match &expose_node.value {
                            ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                                name: choice_type_expose_name,
                                open_range: _,
                            } => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(&choice_type_expose_name.value, import_module_name);
                                if let Some((_, imported_module_syntax)) =
                                    project_state_get_module_with_name(
                                        state,
                                        project_state,
                                        import_module_name,
                                    )
                                {
                                    'until_origin_choice_type_declaration_found: for documented_declaration in
                                        imported_module_syntax.syntax.declarations.iter()
                                    {
                                        match &documented_declaration
                                            .declaration
                                            .as_ref()
                                            .map(|node| &node.value)
                                        {
                                            Some(ElmSyntaxDeclaration::ChoiceType {
                                                name: maybe_imported_module_choice_type_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                variant0_name:
                                                    maybe_imported_module_choice_type_variant0_name,
                                                variant0_values: _,
                                                variant1_up: imported_module_choice_type_variant1_up,
                                            }) => {
                                                if Some(choice_type_expose_name.value.as_str())
                                                    == maybe_imported_module_choice_type_name
                                                        .as_ref()
                                                        .map(|node| node.value.as_str())
                                                {
                                                    if let Some(imported_module_choice_type_variant0_name_node) = maybe_imported_module_choice_type_variant0_name.as_ref() {
                                                            module_origin_lookup.unqualified.insert(
                                                                &imported_module_choice_type_variant0_name_node.value,
                                                                import_module_name,
                                                            );
                                                        }
                                                    for imported_module_choice_type_variant in
                                                        imported_module_choice_type_variant1_up
                                                    {
                                                        if let Some(
                                                            imported_module_choice_type_variant_name_node,
                                                        ) = imported_module_choice_type_variant
                                                            .name
                                                            .as_ref()
                                                        {
                                                            module_origin_lookup.unqualified.insert(
                                                                &imported_module_choice_type_variant_name_node.value,
                                                                import_module_name,
                                                            );
                                                        }
                                                    }
                                                    break 'until_origin_choice_type_declaration_found;
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            ElmSyntaxExpose::Operator(symbol) => {
                                if let Some(operator_symbol_node) = symbol {
                                    module_origin_lookup
                                        .unqualified
                                        .insert(operator_symbol_node.value, import_module_name);
                                }
                            }
                            ElmSyntaxExpose::Type(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name, import_module_name);
                            }
                            ElmSyntaxExpose::Variable(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name, import_module_name);
                            }
                        }
                    }
                }
            }
        }
    }
    module_origin_lookup
}

fn elm_syntax_module_exposed_symbols<'a>(elm_syntax_module: &'a ElmSyntaxModule) -> Vec<&'a str> {
    let mut exposed_symbols: Vec<&str> = Vec::new();
    match elm_syntax_module
        .header
        .as_ref()
        .and_then(|header| header.exposing.as_ref())
        .and_then(|exposing| exposing.specific.as_ref())
        .as_ref()
        .map(|node| &node.value)
    {
        None | Some(ElmSyntaxExposingSpecific::All(_)) => {
            for declaration_node in elm_syntax_module
                .declarations
                .iter()
                .filter_map(|documented_declaration| documented_declaration.declaration.as_ref())
            {
                match &declaration_node.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: maybe_exposed_choice_type_name,
                        variant0_name: maybe_exposed_choice_type_variant0_name,
                        variant1_up: exposed_choice_type_variant1_up,
                        ..
                    } => {
                        if let Some(exposed_choice_type_name_node) = maybe_exposed_choice_type_name
                        {
                            exposed_symbols.push(&exposed_choice_type_name_node.value);
                        }
                        if let Some(exposed_choice_type_variant0_name_node) =
                            maybe_exposed_choice_type_variant0_name
                        {
                            exposed_symbols.push(&exposed_choice_type_variant0_name_node.value);
                        }
                        for exposed_choice_type_variant in exposed_choice_type_variant1_up {
                            if let Some(exposed_choice_type_variant_name_node) =
                                &exposed_choice_type_variant.name
                            {
                                exposed_symbols.push(&exposed_choice_type_variant_name_node.value);
                            }
                        }
                    }
                    ElmSyntaxDeclaration::Port {
                        name: maybe_exposed_port_name,
                        ..
                    } => {
                        if let Some(exposed_port_name_node) = maybe_exposed_port_name {
                            exposed_symbols.push(&exposed_port_name_node.value);
                        }
                    }
                    ElmSyntaxDeclaration::TypeAlias {
                        name: maybe_exposed_type_alias_name,
                        ..
                    } => {
                        if let Some(exposed_type_alias_name_node) = maybe_exposed_type_alias_name {
                            exposed_symbols.push(&exposed_type_alias_name_node.value);
                        }
                    }
                    ElmSyntaxDeclaration::Operator {
                        operator: maybe_exposed_operator,
                        ..
                    } => {
                        if let Some(exposed_operator_node) = maybe_exposed_operator {
                            exposed_symbols.push(&exposed_operator_node.value);
                        }
                    }
                    ElmSyntaxDeclaration::Variable {
                        start_name: exposed_variable_name_node,
                        ..
                    } => {
                        exposed_symbols.push(&exposed_variable_name_node.value);
                    }
                }
            }
        }
        Some(ElmSyntaxExposingSpecific::Explicit(exposes)) => {
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: choice_type_expose_name,
                        open_range: _,
                    } => {
                        exposed_symbols.push(&choice_type_expose_name.value);
                        'until_origin_choice_type_declaration_found: for declaration_node in
                            elm_syntax_module.declarations.iter().filter_map(
                                |documented_declaration| {
                                    documented_declaration.declaration.as_ref()
                                },
                            )
                        {
                            match &declaration_node.value {
                                ElmSyntaxDeclaration::ChoiceType {
                                    name: Some(exposed_choice_type_name_node),
                                    parameters: _,
                                    equals_key_symbol_range: _,
                                    variant0_name: maybe_exposed_choice_type_variant0_name,
                                    variant0_values: _,
                                    variant1_up: exposed_choice_type_variant1_up,
                                } => {
                                    if &choice_type_expose_name.value
                                        == &exposed_choice_type_name_node.value
                                    {
                                        if let Some(exposed_choice_type_variant0_name_node) =
                                            maybe_exposed_choice_type_variant0_name
                                        {
                                            exposed_symbols.push(
                                                &exposed_choice_type_variant0_name_node.value,
                                            );
                                        }
                                        for exposed_choice_type_variant in
                                            exposed_choice_type_variant1_up
                                        {
                                            if let Some(exposed_choice_type_variant_name_node) =
                                                &exposed_choice_type_variant.name
                                            {
                                                exposed_symbols.push(
                                                    &exposed_choice_type_variant_name_node.value,
                                                );
                                            }
                                        }
                                        break 'until_origin_choice_type_declaration_found;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        if let Some(symbol_node) = maybe_symbol {
                            exposed_symbols.push(&symbol_node.value);
                        }
                    }
                    ElmSyntaxExpose::Type(name) => {
                        exposed_symbols.push(name);
                    }
                    ElmSyntaxExpose::Variable(name) => {
                        exposed_symbols.push(name);
                    }
                }
            }
        }
    }
    exposed_symbols
}

fn elm_syntax_type_to_single_line_string(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type: &ElmSyntaxType,
) -> String {
    let mut builder = String::new();
    elm_syntax_type_to_single_line_string_into(&mut builder, module_origin_lookup, elm_syntax_type);
    builder
}

fn elm_syntax_type_to_single_line_string_into(
    so_far: &mut String,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type: &ElmSyntaxType,
) {
    match elm_syntax_type {
        ElmSyntaxType::Construct {
            reference,
            arguments,
        } => {
            let origin_module = look_up_origin_module(
                module_origin_lookup,
                &reference.value.qualification,
                &reference.value.name,
            );
            if !origin_module.is_empty() {
                // consider not adding (full) qualification
                // when contained in implicit imports
                so_far.push_str(origin_module);
                so_far.push('.');
            }
            so_far.push_str(&reference.value.name);
            for argument in arguments {
                so_far.push(' ');
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &argument.value,
                );
            }
        }
        ElmSyntaxType::Function {
            input,
            arrow_key_symbol_range: _,
            output: maybe_output,
        } => {
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &input.value);
            so_far.push_str(" -> ");
            if let Some(output_node) = maybe_output {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &output_node.value,
                )
            }
        }
        ElmSyntaxType::Parenthesized(in_parens) => {
            so_far.push('(');
            elm_syntax_type_to_single_line_string_into(
                so_far,
                module_origin_lookup,
                &in_parens.value,
            );
            so_far.push(')');
        }
        ElmSyntaxType::Record(fields) => {
            let mut fields_iterator = fields.iter();
            match fields_iterator.next() {
                None => so_far.push_str("{}"),
                Some(field0) => {
                    so_far.push_str("{ ");
                    so_far.push_str(&field0.name.value);
                    so_far.push_str(" : ");
                    if let Some(field0_value_node) = &field0.value {
                        elm_syntax_type_to_single_line_string_into(
                            so_far,
                            module_origin_lookup,
                            &field0_value_node.value,
                        );
                    }
                    for field in fields_iterator {
                        so_far.push_str(", ");
                        so_far.push_str(&field.name.value);
                        so_far.push_str(" : ");
                        if let Some(field_value_node) = &field.value {
                            elm_syntax_type_to_single_line_string_into(
                                so_far,
                                module_origin_lookup,
                                &field_value_node.value,
                            );
                        }
                    }
                    so_far.push_str(" }")
                }
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            so_far.push_str("{ ");
            if let Some(record_variable_node) = maybe_record_variable {
                so_far.push_str(&record_variable_node.value);
            }
            so_far.push_str(" | ");
            for field in fields {
                so_far.push_str(", ");
                so_far.push_str(&field.name.value);
                so_far.push_str(" : ");
                if let Some(field_name_node) = &field.value {
                    elm_syntax_type_to_single_line_string_into(
                        so_far,
                        module_origin_lookup,
                        &field_name_node.value,
                    );
                }
            }
            so_far.push_str(" }")
        }
        ElmSyntaxType::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &part0_node.value,
                );
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &part1_node.value,
                );
            }
            so_far.push_str(", ");
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &part2_node.value,
                );
            }
            so_far.push_str(" )");
        }
        ElmSyntaxType::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &part0_node.value,
                );
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &part1_node.value,
                );
            }
            so_far.push_str(" )");
        }
        ElmSyntaxType::Unit => so_far.push_str("()"),
        ElmSyntaxType::Variable(name) => so_far.push_str(name),
    }
}

// //
#[derive(Clone, Debug)]
enum ElmSyntaxSymbol<'a> {
    ModuleName(&'a str),
    ImportAlias {
        module_origin: &'a str,
        alias_name: &'a str,
    },
    ModuleHeaderExpose {
        name: &'a str,
        all_exposes: &'a [ElmSyntaxNode<ElmSyntaxExpose>],
    },
    // includes variant
    ModuleMemberDeclarationName {
        name: &'a str,
        documentation: Option<&'a str>,
        declaration: ElmSyntaxNode<&'a ElmSyntaxDeclaration>,
    },
    ImportExpose {
        origin_module: &'a str,
        name: &'a str,
        all_exposes: &'a [ElmSyntaxNode<ElmSyntaxExpose>],
    },
    /// also covers local bindings like pattern variables and let signature names, and exposes
    /// TODO to avoid suggesting completions for let declaration names,
    /// consider splitting off ::LetDeclarationName variant or similar
    VariableOrVariantOrOperator {
        qualification: &'a str,
        name: &'a str,
        // consider wrapping in Option
        local_bindings: ElmLocalBindings<'a>,
    },
    Type {
        qualification: &'a str,
        name: &'a str,
    },
    TypeVariable {
        scope_declaration: &'a ElmSyntaxDeclaration,
        name: &'a str,
    },
}
type ElmLocalBindings<'a> = Vec<(
    ElmSyntaxNode<&'a ElmSyntaxExpression>,
    Vec<ElmLocalBinding<'a>>,
)>;
fn find_local_binding_scope_expression<'a>(
    local_bindings: &ElmLocalBindings<'a>,
    to_find: &str,
) -> Option<(
    LocalBindingOrigin<'a>,
    ElmSyntaxNode<&'a ElmSyntaxExpression>,
)> {
    local_bindings
        .iter()
        .find_map(|(scope_expression, local_bindings)| {
            local_bindings.iter().find_map(|local_binding| {
                if local_binding.name == to_find {
                    Some((local_binding.origin, *scope_expression))
                } else {
                    None
                }
            })
        })
}

fn elm_syntax_module_find_symbol_at_position<'a>(
    elm_syntax_module: &'a ElmSyntaxModule,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    elm_syntax_module
        .header
        .as_ref()
        .and_then(|module_header| {
            elm_syntax_module_header_find_reference_at_position(module_header, position)
        })
        .or_else(|| {
            elm_syntax_module.imports.iter().find_map(|import_node| {
                elm_syntax_import_find_reference_at_position(
                    elm_syntax_node_as_ref(import_node),
                    position,
                )
            })
        })
        .or_else(|| {
            elm_syntax_module
                .declarations
                .iter()
                .find_map(|documented_declaration| {
                    let declaration_node = documented_declaration.declaration.as_ref()?;
                    elm_syntax_declaration_find_reference_at_position(
                        elm_syntax_node_as_ref(declaration_node),
                        documented_declaration
                            .documentation
                            .as_ref()
                            .map(|node| node.value.as_str()),
                        position,
                    )
                })
        })
}
fn elm_syntax_module_header_find_reference_at_position<'a>(
    elm_syntax_module_header: &'a ElmSyntaxModuleHeader,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if let Some(module_name_node) = &elm_syntax_module_header.module_name
        && lsp_range_includes_position(module_name_node.range, position)
    {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ModuleName(&module_name_node.value),
            range: module_name_node.range,
        })
    } else {
        let exposing: &ElmSyntaxExposing = elm_syntax_module_header.exposing.as_ref()?;
        let exposing_specific_node: &ElmSyntaxNode<ElmSyntaxExposingSpecific> =
            exposing.specific.as_ref()?;
        elm_syntax_module_header_exposing_specific_from_module_find_reference_at_position(
            elm_syntax_node_as_ref(exposing_specific_node),
            position,
        )
    }
}

fn elm_syntax_import_find_reference_at_position<'a>(
    elm_syntax_import_node: ElmSyntaxNode<&'a ElmSyntaxImport>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_import_node.range, position) {
        return None;
    }
    let module_name_node = elm_syntax_import_node.value.module_name.as_ref()?;
    if lsp_range_includes_position(module_name_node.range, position) {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ModuleName(&module_name_node.value),
            range: module_name_node.range,
        })
    } else if let Some(import_alias) = &elm_syntax_import_node.value.alias
        && let Some(import_alias_name_node) = &import_alias.name
        && lsp_range_includes_position(import_alias_name_node.range, position)
    {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ImportAlias {
                module_origin: &module_name_node.value,
                alias_name: &import_alias_name_node.value,
            },
            range: module_name_node.range,
        })
    } else {
        elm_syntax_import_node
            .value
            .exposing
            .as_ref()
            .and_then(|exposing| {
                let exposing_specific = exposing.specific.as_ref()?;
                elm_syntax_import_exposing_specific_from_module_find_reference_at_position(
                    &module_name_node.value,
                    elm_syntax_node_as_ref(exposing_specific),
                    position,
                )
            })
    }
}

fn elm_syntax_module_header_exposing_specific_from_module_find_reference_at_position<'a>(
    elm_syntax_exposing_specific_node: ElmSyntaxNode<&'a ElmSyntaxExposingSpecific>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_exposing_specific_node.range, position) {
        return None;
    }
    match elm_syntax_exposing_specific_node.value {
        ElmSyntaxExposingSpecific::All(_) => None,
        ElmSyntaxExposingSpecific::Explicit(exposes) => exposes.iter().find_map(|expose_node| {
            if lsp_range_includes_position(expose_node.range, position) {
                let expose_name: &str = match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => Some(name.value.as_str()),
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        maybe_symbol.as_ref().map(|symbol_node| symbol_node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_str()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_str()),
                }?;
                Some(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::ModuleHeaderExpose {
                        name: expose_name,
                        all_exposes: exposes,
                    },
                    range: expose_node.range,
                })
            } else {
                None
            }
        }),
    }
}
fn elm_syntax_import_exposing_specific_from_module_find_reference_at_position<'a>(
    import_origin_module: &'a str,
    elm_syntax_exposing_specific_node: ElmSyntaxNode<&'a ElmSyntaxExposingSpecific>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_exposing_specific_node.range, position) {
        return None;
    }
    match elm_syntax_exposing_specific_node.value {
        ElmSyntaxExposingSpecific::All(_) => None,
        ElmSyntaxExposingSpecific::Explicit(exposes) => exposes.iter().find_map(|expose_node| {
            if lsp_range_includes_position(expose_node.range, position) {
                let expose_name: &str = match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => Some(name.value.as_str()),
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        maybe_symbol.as_ref().map(|symbol_node| symbol_node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_str()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_str()),
                }?;
                Some(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::ImportExpose {
                        origin_module: import_origin_module,
                        name: expose_name,
                        all_exposes: exposes,
                    },
                    range: expose_node.range,
                })
            } else {
                None
            }
        }),
    }
}

fn elm_syntax_declaration_find_reference_at_position<'a>(
    elm_syntax_declaration_node: ElmSyntaxNode<&'a ElmSyntaxDeclaration>,
    maybe_documentation: Option<&'a str>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_declaration_node.range, position) {
        None
    } else {
        match elm_syntax_declaration_node.value {
            ElmSyntaxDeclaration::ChoiceType {
                name: maybe_name,
                parameters: parameters,
                equals_key_symbol_range: _,
                variant0_name: maybe_variant0_name,
                variant0_values,
                variant1_up,
            } => {
                if let Some(name_node) = maybe_name
                    && lsp_range_includes_position(name_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &name_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: name_node.range,
                    })
                } else if let Some(variant0_name_node) = maybe_variant0_name
                    && lsp_range_includes_position(variant0_name_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &variant0_name_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: variant0_name_node.range,
                    })
                } else {
                    parameters
                        .iter()
                        .find_map(|parameter_node| {
                            if lsp_range_includes_position(parameter_node.range, position) {
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxSymbol::TypeVariable {
                                        scope_declaration: elm_syntax_declaration_node.value,
                                        name: &parameter_node.value,
                                    },
                                    range: parameter_node.range,
                                })
                            } else {
                                None
                            }
                        })
                        .or_else(|| {
                            variant0_values.iter().find_map(|variant_value| {
                                elm_syntax_type_find_reference_at_position(
                                    elm_syntax_declaration_node.value,
                                    elm_syntax_node_as_ref(variant_value),
                                    position,
                                )
                            })
                        })
                        .or_else(|| {
                            variant1_up.iter().find_map(|variant| {
                                if let Some(variant_name_node) = &variant.name
                                    && lsp_range_includes_position(
                                        variant_name_node.range,
                                        position,
                                    )
                                {
                                    Some(ElmSyntaxNode {
                                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                                            name: &variant_name_node.value,
                                            declaration: elm_syntax_declaration_node,
                                            documentation: maybe_documentation,
                                        },
                                        range: variant_name_node.range,
                                    })
                                } else {
                                    variant.values.iter().find_map(|variant_value| {
                                        elm_syntax_type_find_reference_at_position(
                                            elm_syntax_declaration_node.value,
                                            elm_syntax_node_as_ref(variant_value),
                                            position,
                                        )
                                    })
                                }
                            })
                        })
                }
            }
            ElmSyntaxDeclaration::Operator {
                direction: _,
                precedence: _,
                equals_key_symbol_range: _,
                operator: maybe_operator,
                function: maybe_function,
            } => {
                if let Some(operator_node) = maybe_operator
                    && lsp_range_includes_position(operator_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &operator_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: operator_node.range,
                    })
                } else if let Some(function_node) = maybe_function
                    && lsp_range_includes_position(function_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            qualification: "",
                            name: &function_node.value,
                            local_bindings: vec![],
                        },
                        range: function_node.range,
                    })
                } else {
                    None
                }
            }
            ElmSyntaxDeclaration::Port {
                name: maybe_name,
                colon_key_symbol_range: _,
                type_: maybe_type,
            } => {
                if let Some(name_node) = maybe_name
                    && lsp_range_includes_position(name_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &name_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: name_node.range,
                    })
                } else {
                    maybe_type.as_ref().and_then(|type_node| {
                        elm_syntax_type_find_reference_at_position(
                            elm_syntax_declaration_node.value,
                            elm_syntax_node_as_ref(type_node),
                            position,
                        )
                    })
                }
            }
            ElmSyntaxDeclaration::TypeAlias {
                alias_keyword_range: _,
                name: maybe_name,
                parameters,
                equals_key_symbol_range: _,
                type_: maybe_type,
            } => {
                if let Some(name_node) = maybe_name
                    && lsp_range_includes_position(name_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &name_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: name_node.range,
                    })
                } else {
                    parameters
                        .iter()
                        .find_map(|parameter_node| {
                            if lsp_range_includes_position(parameter_node.range, position) {
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxSymbol::TypeVariable {
                                        scope_declaration: elm_syntax_declaration_node.value,
                                        name: &parameter_node.value,
                                    },
                                    range: parameter_node.range,
                                })
                            } else {
                                None
                            }
                        })
                        .or_else(|| {
                            maybe_type.as_ref().and_then(|type_node| {
                                elm_syntax_type_find_reference_at_position(
                                    elm_syntax_declaration_node.value,
                                    elm_syntax_node_as_ref(type_node),
                                    position,
                                )
                            })
                        })
                }
            }
            ElmSyntaxDeclaration::Variable {
                start_name: start_name_node,
                signature: maybe_signature,
                parameters,
                equals_key_symbol_range: _,
                result: maybe_result,
            } => {
                if lsp_range_includes_position(start_name_node.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                            name: &start_name_node.value,
                            declaration: elm_syntax_declaration_node,
                            documentation: maybe_documentation,
                        },
                        range: start_name_node.range,
                    })
                } else {
                    maybe_signature
                        .as_ref()
                        .and_then(|signature: &ElmSyntaxVariableDeclarationSignature| {
                            if let Some(implementation_name_range) =
                                signature.implementation_name_range
                                && lsp_range_includes_position(implementation_name_range, position)
                            {
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxSymbol::ModuleMemberDeclarationName {
                                        name: &start_name_node.value,
                                        declaration: elm_syntax_declaration_node,
                                        documentation: maybe_documentation,
                                    },
                                    range: start_name_node.range,
                                })
                            } else {
                                signature.type_.as_ref().and_then(|signature_type_node| {
                                    elm_syntax_type_find_reference_at_position(
                                        elm_syntax_declaration_node.value,
                                        elm_syntax_node_as_ref(signature_type_node),
                                        position,
                                    )
                                })
                            }
                        })
                        .or_else(|| {
                            let mut parameter_introduced_bindings: Vec<ElmLocalBinding> =
                                Vec::new();
                            for parameter_node in parameters {
                                elm_syntax_pattern_bindings_into(
                                    &mut parameter_introduced_bindings,
                                    elm_syntax_node_as_ref(parameter_node),
                                );
                            }
                            maybe_result.as_ref().and_then(|result_node| {
                                elm_syntax_expression_find_reference_at_position(
                                    vec![(
                                        elm_syntax_node_as_ref(result_node),
                                        parameter_introduced_bindings,
                                    )],
                                    elm_syntax_declaration_node.value,
                                    elm_syntax_node_as_ref(result_node),
                                    position,
                                )
                                .break_value()
                            })
                        })
                        .or_else(|| {
                            parameters.iter().find_map(|parameter| {
                                elm_syntax_pattern_find_reference_at_position(
                                    elm_syntax_node_as_ref(parameter),
                                    position,
                                )
                            })
                        })
                }
            }
        }
    }
}

fn elm_syntax_pattern_find_reference_at_position<'a>(
    elm_syntax_pattern_node: ElmSyntaxNode<&'a ElmSyntaxPattern>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern,
            as_keyword_range: _,
            variable: _,
        } => {
            elm_syntax_pattern_find_reference_at_position(elm_syntax_node_unbox(pattern), position)
        }
        ElmSyntaxPattern::Char(_) => None,
        ElmSyntaxPattern::Ignored => None,
        ElmSyntaxPattern::Int { .. } => None,
        ElmSyntaxPattern::ListCons {
            head: maybe_head,
            cons_key_symbol: _,
            tail: maybe_tail,
        } => maybe_head
            .as_ref()
            .and_then(|head_node| {
                elm_syntax_pattern_find_reference_at_position(
                    elm_syntax_node_unbox(head_node),
                    position,
                )
            })
            .or_else(|| {
                maybe_tail.as_ref().and_then(|tail_node| {
                    elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_unbox(tail_node),
                        position,
                    )
                })
            }),
        ElmSyntaxPattern::ListExact(elements) => elements.iter().find_map(|element| {
            elm_syntax_pattern_find_reference_at_position(elm_syntax_node_as_ref(element), position)
        }),
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_find_reference_at_position(
                elm_syntax_node_unbox(in_parens),
                position,
            )
        }
        ElmSyntaxPattern::Record(_) => None,
        ElmSyntaxPattern::String { .. } => None,
        ElmSyntaxPattern::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => maybe_part0
            .as_ref()
            .and_then(|part0_node| {
                elm_syntax_pattern_find_reference_at_position(
                    elm_syntax_node_unbox(part0_node),
                    position,
                )
            })
            .or_else(|| {
                maybe_part1.as_ref().and_then(|part1_node| {
                    elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_unbox(part1_node),
                        position,
                    )
                })
            })
            .or_else(|| {
                maybe_part2.as_ref().and_then(|part2_node| {
                    elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_unbox(part2_node),
                        position,
                    )
                })
            }),
        ElmSyntaxPattern::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => maybe_part0
            .as_ref()
            .and_then(|part0_node| {
                elm_syntax_pattern_find_reference_at_position(
                    elm_syntax_node_unbox(part0_node),
                    position,
                )
            })
            .or_else(|| {
                maybe_part1.as_ref().and_then(|part1_node| {
                    elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_unbox(part1_node),
                        position,
                    )
                })
            }),
        ElmSyntaxPattern::Unit => None,
        ElmSyntaxPattern::Variable(_) => None,
        ElmSyntaxPattern::Variant { reference, values } => {
            if lsp_range_includes_position(reference.range, position) {
                Some(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                        qualification: &reference.value.qualification,
                        name: &reference.value.name,
                        local_bindings: vec![],
                    },
                    range: reference.range,
                })
            } else {
                values.iter().find_map(|value| {
                    elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_as_ref(value),
                        position,
                    )
                })
            }
        }
    }
}

fn elm_syntax_type_find_reference_at_position<'a>(
    scope_declaration: &'a ElmSyntaxDeclaration,
    elm_syntax_type_node: ElmSyntaxNode<&'a ElmSyntaxType>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_type_node.range, position) {
        None
    } else {
        match elm_syntax_type_node.value {
            ElmSyntaxType::Construct {
                reference,
                arguments,
            } => {
                if lsp_range_includes_position(reference.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::Type {
                            qualification: &reference.value.qualification,
                            name: &reference.value.name,
                        },
                        range: reference.range,
                    })
                } else {
                    arguments.iter().find_map(|argument| {
                        elm_syntax_type_find_reference_at_position(
                            scope_declaration,
                            elm_syntax_node_as_ref(argument),
                            position,
                        )
                    })
                }
            }
            ElmSyntaxType::Function {
                input,
                arrow_key_symbol_range: _,
                output: maybe_output,
            } => elm_syntax_type_find_reference_at_position(
                scope_declaration,
                elm_syntax_node_unbox(input),
                position,
            )
            .or_else(|| {
                maybe_output.as_ref().and_then(|output_node| {
                    elm_syntax_type_find_reference_at_position(
                        scope_declaration,
                        elm_syntax_node_unbox(output_node),
                        position,
                    )
                })
            }),
            ElmSyntaxType::Parenthesized(in_parens) => elm_syntax_type_find_reference_at_position(
                scope_declaration,
                elm_syntax_node_unbox(in_parens),
                position,
            ),
            ElmSyntaxType::Record(fields) => fields.iter().find_map(|field| {
                field.value.as_ref().and_then(|field_value_node| {
                    elm_syntax_type_find_reference_at_position(
                        scope_declaration,
                        elm_syntax_node_as_ref(&field_value_node),
                        position,
                    )
                })
            }),
            ElmSyntaxType::RecordExtension {
                record_variable: maybe_record_type_variable,
                bar_key_symbol_range: _,
                fields,
            } => {
                if let Some(record_type_variable_node) = maybe_record_type_variable
                    && lsp_range_includes_position(record_type_variable_node.range, position)
                {
                    Some(ElmSyntaxNode {
                        range: record_type_variable_node.range,
                        value: ElmSyntaxSymbol::TypeVariable {
                            scope_declaration: scope_declaration,
                            name: &record_type_variable_node.value,
                        },
                    })
                } else {
                    fields.iter().find_map(|field| {
                        field.value.as_ref().and_then(|field_value_node| {
                            elm_syntax_type_find_reference_at_position(
                                scope_declaration,
                                elm_syntax_node_as_ref(field_value_node),
                                position,
                            )
                        })
                    })
                }
            }
            ElmSyntaxType::Triple {
                part0: maybe_part0,
                part1: maybe_part1,
                part2: maybe_part2,
            } => maybe_part0
                .as_ref()
                .and_then(|part0_node| {
                    elm_syntax_type_find_reference_at_position(
                        scope_declaration,
                        elm_syntax_node_unbox(part0_node),
                        position,
                    )
                })
                .or_else(|| {
                    maybe_part1.as_ref().and_then(|part1_node| {
                        elm_syntax_type_find_reference_at_position(
                            scope_declaration,
                            elm_syntax_node_unbox(part1_node),
                            position,
                        )
                    })
                })
                .or_else(|| {
                    maybe_part2.as_ref().and_then(|part2| {
                        elm_syntax_type_find_reference_at_position(
                            scope_declaration,
                            elm_syntax_node_unbox(part2),
                            position,
                        )
                    })
                }),
            ElmSyntaxType::Tuple {
                part0: maybe_part0,
                part1: maybe_part1,
            } => maybe_part0
                .as_ref()
                .and_then(|part0_node| {
                    elm_syntax_type_find_reference_at_position(
                        scope_declaration,
                        elm_syntax_node_unbox(part0_node),
                        position,
                    )
                })
                .or_else(|| {
                    maybe_part1.as_ref().and_then(|part1_node| {
                        elm_syntax_type_find_reference_at_position(
                            scope_declaration,
                            elm_syntax_node_unbox(part1_node),
                            position,
                        )
                    })
                }),
            ElmSyntaxType::Unit => None,
            ElmSyntaxType::Variable(type_variable_value) => Some(ElmSyntaxNode {
                range: elm_syntax_type_node.range,
                value: ElmSyntaxSymbol::TypeVariable {
                    scope_declaration: scope_declaration,
                    name: &type_variable_value,
                },
            }),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum LocalBindingOrigin<'a> {
    // consider separately tracking parameter names (let or otherwise), including their origin declaration name and annotation type when available
    PatternVariable(lsp_types::Range),
    PatternRecordField(lsp_types::Range),
    LetDeclaredVariable {
        signature_type: Option<&'a ElmSyntaxType>,
        start_name_range: lsp_types::Range,
    },
}
#[derive(Clone, Copy, Debug)]
struct ElmLocalBinding<'a> {
    name: &'a str,
    origin: LocalBindingOrigin<'a>,
}

fn on_some_break<A>(maybe: Option<A>) -> std::ops::ControlFlow<A, ()> {
    match maybe {
        None => std::ops::ControlFlow::Continue(()),
        Some(value) => std::ops::ControlFlow::Break(value),
    }
}

fn elm_syntax_expression_find_reference_at_position<'a>(
    mut local_bindings: ElmLocalBindings<'a>,
    scope_declaration: &'a ElmSyntaxDeclaration,
    elm_syntax_expression_node: ElmSyntaxNode<&'a ElmSyntaxExpression>,
    position: lsp_types::Position,
) -> std::ops::ControlFlow<ElmSyntaxNode<ElmSyntaxSymbol<'a>>, ElmLocalBindings<'a>> {
    if !lsp_range_includes_position(elm_syntax_expression_node.range, position) {
        return std::ops::ControlFlow::Continue(local_bindings);
    }
    match elm_syntax_expression_node.value {
        ElmSyntaxExpression::Call {
            called,
            argument0,
            argument1_up,
        } => {
            local_bindings = elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(called),
                position,
            )?;
            local_bindings = elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(argument0),
                position,
            )?;
            argument1_up
                .iter()
                .try_fold(local_bindings, |local_bindings, argument| {
                    elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(argument),
                        position,
                    )
                })
        }
        ElmSyntaxExpression::CaseOf {
            matched: maybe_matched,
            of_keyword_range: _,
            cases,
        } => {
            if let Some(matched_node) = maybe_matched {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(matched_node),
                    position,
                )?;
            }
            cases
                .iter()
                .try_fold(local_bindings, |mut local_bindings, case| {
                    if let Some(found_symbol) = elm_syntax_pattern_find_reference_at_position(
                        elm_syntax_node_as_ref(&case.pattern),
                        position,
                    ) {
                        return std::ops::ControlFlow::Break(found_symbol);
                    };
                    if let Some(case_result_node) = &case.result
                    && // we need to check that the position is actually in that case before committing to mutating local bindings
                    lsp_range_includes_position(case_result_node.range, position)
                    {
                        let mut introduced_bindings: Vec<ElmLocalBinding> = Vec::new();
                        elm_syntax_pattern_bindings_into(
                            &mut introduced_bindings,
                            elm_syntax_node_as_ref(&case.pattern),
                        );
                        local_bindings.push((
                            elm_syntax_node_as_ref(case_result_node),
                            introduced_bindings,
                        ));
                        elm_syntax_expression_find_reference_at_position(
                            local_bindings,
                            scope_declaration,
                            elm_syntax_node_as_ref(case_result_node),
                            position,
                        )
                    } else {
                        std::ops::ControlFlow::Continue(local_bindings)
                    }
                })
        }
        ElmSyntaxExpression::Char(_) => std::ops::ControlFlow::Continue(local_bindings),
        ElmSyntaxExpression::Float(_) => std::ops::ControlFlow::Continue(local_bindings),
        ElmSyntaxExpression::IfThenElse {
            condition: maybe_condition,
            then_keyword_range: _,
            on_true: maybe_on_true,
            else_keyword_range: _,
            on_false: maybe_on_false,
        } => {
            if let Some(condition_node) = maybe_condition {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(condition_node),
                    position,
                )?;
            }
            if let Some(on_true_node) = maybe_on_true {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(on_true_node),
                    position,
                )?;
            }
            match maybe_on_false {
                Some(on_false_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(on_false_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left,
            operator,
            right: maybe_right,
        } => {
            if lsp_range_includes_position(operator.range, position) {
                return std::ops::ControlFlow::Break(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                        qualification: "",
                        name: &operator.value,
                        local_bindings: local_bindings,
                    },
                    range: operator.range,
                });
            }
            local_bindings = elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(left),
                position,
            )?;
            match maybe_right {
                Some(right_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(right_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::Integer { .. } => std::ops::ControlFlow::Continue(local_bindings),
        ElmSyntaxExpression::Lambda {
            arrow_key_symbol_range: _,
            parameters,
            result: maybe_result,
        } => {
            if let Some(found_symbol) = parameters.iter().find_map(|parameter| {
                elm_syntax_pattern_find_reference_at_position(
                    elm_syntax_node_as_ref(parameter),
                    position,
                )
            }) {
                return std::ops::ControlFlow::Break(found_symbol);
            }
            match maybe_result {
                Some(result_node) => {
                    let mut introduced_bindings: Vec<ElmLocalBinding> = Vec::new();
                    for parameter_node in parameters {
                        elm_syntax_pattern_bindings_into(
                            &mut introduced_bindings,
                            elm_syntax_node_as_ref(parameter_node),
                        );
                    }
                    local_bindings.push((elm_syntax_node_unbox(result_node), introduced_bindings));
                    elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_unbox(result_node),
                        position,
                    )
                }
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::LetIn {
            declarations,
            in_keyword_range: _,
            result: maybe_result,
        } => {
            let mut introduced_bindings: Vec<ElmLocalBinding> = Vec::new();
            for let_declaration_node in declarations {
                elm_syntax_let_declaration_introduced_bindings_into(
                    &mut introduced_bindings,
                    &let_declaration_node.value,
                );
            }
            local_bindings.push((elm_syntax_expression_node, introduced_bindings));
            local_bindings =
                declarations
                    .iter()
                    .try_fold(local_bindings, |local_bindings, declaration| {
                        elm_syntax_let_declaration_find_reference_at_position(
                            local_bindings,
                            scope_declaration,
                            elm_syntax_node_as_ref(declaration),
                            position,
                        )
                    })?;
            match maybe_result {
                Some(result_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(result_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::List(elements) => {
            elements
                .iter()
                .try_fold(local_bindings, |local_bindings, element| {
                    elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(element),
                        position,
                    )
                })
        }
        ElmSyntaxExpression::Negation(maybe_in_negation) => match maybe_in_negation {
            Some(in_negation_node) => elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(in_negation_node),
                position,
            ),
            None => std::ops::ControlFlow::Continue(local_bindings),
        },
        ElmSyntaxExpression::OperatorFunction(operator_node) => {
            std::ops::ControlFlow::Break(ElmSyntaxNode {
                value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                    qualification: "",
                    name: operator_node.value,
                    local_bindings: local_bindings,
                },
                range: elm_syntax_expression_node.range,
            })
        }
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(in_parens),
                position,
            )
        }
        ElmSyntaxExpression::Record(fields) => {
            fields
                .iter()
                .try_fold(local_bindings, |local_bindings, field| match field.value {
                    Some(ref field_value_node) => elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(field_value_node),
                        position,
                    ),
                    None => std::ops::ControlFlow::Continue(local_bindings),
                })
        }
        ElmSyntaxExpression::RecordAccess { record, field: _ } => {
            elm_syntax_expression_find_reference_at_position(
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(record),
                position,
            )
        }
        ElmSyntaxExpression::RecordAccessFunction(_) => {
            std::ops::ControlFlow::Continue(local_bindings)
        }
        ElmSyntaxExpression::RecordUpdate {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            if let Some(record_variable_node) = maybe_record_variable
                && lsp_range_includes_position(record_variable_node.range, position)
            {
                return std::ops::ControlFlow::Break(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                        qualification: "",
                        name: &record_variable_node.value,
                        local_bindings: local_bindings,
                    },
                    range: record_variable_node.range,
                });
            }
            fields
                .iter()
                .try_fold(local_bindings, |local_bindings, field| match field.value {
                    Some(ref field_value_node) => elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(field_value_node),
                        position,
                    ),
                    None => std::ops::ControlFlow::Continue(local_bindings),
                })
        }
        ElmSyntaxExpression::Reference {
            qualification,
            name,
        } => std::ops::ControlFlow::Break(ElmSyntaxNode {
            value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                qualification: qualification,
                name: name,
                local_bindings: local_bindings,
            },
            range: elm_syntax_expression_node.range,
        }),
        ElmSyntaxExpression::String { .. } => std::ops::ControlFlow::Continue(local_bindings),
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part0_node),
                    position,
                )?;
            }
            if let Some(part1_node) = maybe_part1 {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part1_node),
                    position,
                )?;
            }
            match maybe_part2 {
                Some(part2_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part2_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                local_bindings = elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part0_node),
                    position,
                )?;
            }
            match maybe_part1 {
                Some(part1_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part1_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxExpression::Unit => std::ops::ControlFlow::Continue(local_bindings),
    }
}

fn elm_syntax_let_declaration_find_reference_at_position<'a>(
    mut local_bindings: ElmLocalBindings<'a>,
    scope_declaration: &'a ElmSyntaxDeclaration,
    elm_syntax_let_declaration_node: ElmSyntaxNode<&'a ElmSyntaxLetDeclaration>,
    position: lsp_types::Position,
) -> std::ops::ControlFlow<ElmSyntaxNode<ElmSyntaxSymbol<'a>>, ElmLocalBindings<'a>> {
    if !lsp_range_includes_position(elm_syntax_let_declaration_node.range, position) {
        return std::ops::ControlFlow::Continue(local_bindings);
    }
    match elm_syntax_let_declaration_node.value {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern,
            equals_key_symbol_range: _,
            expression: maybe_expression,
        } => {
            on_some_break(elm_syntax_pattern_find_reference_at_position(
                elm_syntax_node_as_ref(pattern),
                position,
            ))?;
            match maybe_expression {
                Some(expression_node) => elm_syntax_expression_find_reference_at_position(
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_as_ref(expression_node),
                    position,
                ),
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
        ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: _,
            result: maybe_result,
        } => {
            if lsp_range_includes_position(start_name.range, position) {
                return std::ops::ControlFlow::Break(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                        qualification: "",
                        name: &start_name.value,
                        local_bindings: local_bindings,
                    },
                    range: start_name.range,
                });
            }
            on_some_break(parameters.iter().find_map(|parameter| {
                elm_syntax_pattern_find_reference_at_position(
                    elm_syntax_node_as_ref(parameter),
                    position,
                )
            }))?;
            if let Some(signature) = maybe_signature {
                if let Some(implementation_name_range) = signature.implementation_name_range
                    && lsp_range_includes_position(implementation_name_range, position)
                {
                    return std::ops::ControlFlow::Break(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            qualification: "",
                            name: &start_name.value,
                            local_bindings: local_bindings,
                        },
                        range: implementation_name_range,
                    });
                };
                if let Some(signature_type_node) = &signature.type_ {
                    on_some_break(elm_syntax_type_find_reference_at_position(
                        scope_declaration,
                        elm_syntax_node_as_ref(signature_type_node),
                        position,
                    ))?;
                }
            }
            match maybe_result {
                Some(result_node) => {
                    let mut introduced_bindings: Vec<ElmLocalBinding> = Vec::new();
                    for parameter_node in parameters {
                        elm_syntax_pattern_bindings_into(
                            &mut introduced_bindings,
                            elm_syntax_node_as_ref(parameter_node),
                        );
                    }
                    local_bindings.push((elm_syntax_node_as_ref(result_node), introduced_bindings));
                    elm_syntax_expression_find_reference_at_position(
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(result_node),
                        position,
                    )
                }
                None => std::ops::ControlFlow::Continue(local_bindings),
            }
        }
    }
}

// //
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ElmDeclaredSymbol<'a> {
    ModuleName(&'a str),
    ImportAlias {
        module_origin: &'a str,
        alias_name: &'a str,
    },
    TypeVariable(&'a str),
    // type is tracked separately from VariableOrVariant because e.g. variants and
    // type names are allowed to overlap
    TypeNotRecordAlias {
        module_origin: &'a str,
        name: &'a str,
    },
    VariableOrVariant {
        module_origin: &'a str,
        name: &'a str,
    },
    RecordTypeAlias {
        module_origin: &'a str,
        name: &'a str,
    },
    LocalBinding(&'a str),
}

fn elm_syntax_module_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    state: &State,
    project_state: &ProjectState,
    elm_syntax_module: &ElmSyntaxModule,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    let maybe_self_module_name: Option<&ElmSyntaxNode<String>> = elm_syntax_module
        .header
        .as_ref()
        .and_then(|header| header.module_name.as_ref());
    if let Some(self_module_name_node) = maybe_self_module_name
        && let ElmDeclaredSymbol::ModuleName(module_name_to_collect_uses_of) =
            symbol_to_collect_uses_of
        && module_name_to_collect_uses_of == &self_module_name_node.value
    {
        uses_so_far.push(self_module_name_node.range);
        // a module cannot reference itself within its declarations, imports etc
        return;
    }
    let symbol_to_collect_can_occur_here: bool = match symbol_to_collect_uses_of {
        ElmDeclaredSymbol::ModuleName(module_origin_to_collect_uses_of)
        | ElmDeclaredSymbol::RecordTypeAlias {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
        }
        | ElmDeclaredSymbol::TypeNotRecordAlias {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
        }
        | ElmDeclaredSymbol::VariableOrVariant {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
        } => {
            Some(module_origin_to_collect_uses_of)
                == maybe_self_module_name
                    .as_ref()
                    .map(|node| node.value.as_str())
                || elm_syntax_module.imports.iter().any(|import| {
                    import
                        .value
                        .module_name
                        .as_ref()
                        .map(|node| node.value.as_str())
                        == Some(module_origin_to_collect_uses_of)
                })
        }
        ElmDeclaredSymbol::ImportAlias { .. } => false,
        ElmDeclaredSymbol::TypeVariable(_) => false,
        ElmDeclaredSymbol::LocalBinding(_) => false,
    };
    if !symbol_to_collect_can_occur_here {
        // if not imported, that module name can never appear, so we can skip a bunch of
        // traversing! (unless implicitly imported, but those modules are never renamed!)
        return;
    }
    let self_module_name: &str = maybe_self_module_name
        .map(|node| node.value.as_str())
        .unwrap_or("");
    if let Some(module_header) = &elm_syntax_module.header
        && let Some(exposing) = &module_header.exposing
        && let Some(exposing_specific) = &exposing.specific
    {
        elm_syntax_exposing_specific_uses_of_reference_into(
            uses_so_far,
            self_module_name,
            &exposing_specific.value,
            symbol_to_collect_uses_of,
        );
    }
    for import in elm_syntax_module.imports.iter() {
        elm_syntax_import_uses_of_reference_into(
            uses_so_far,
            &import.value,
            symbol_to_collect_uses_of,
        );
    }
    let module_origin_lookup: ModuleOriginLookup =
        elm_syntax_module_create_origin_lookup(state, project_state, elm_syntax_module);
    for documented_declaration in elm_syntax_module.declarations.iter() {
        if let Some(declaration_node) = &documented_declaration.declaration {
            elm_syntax_declaration_uses_of_reference_into(
                uses_so_far,
                self_module_name,
                &module_origin_lookup,
                &declaration_node.value,
                symbol_to_collect_uses_of,
            );
        }
    }
}

fn elm_syntax_import_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    elm_syntax_import: &ElmSyntaxImport,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    if let Some(import_module_name_node) = &elm_syntax_import.module_name {
        if let ElmDeclaredSymbol::ModuleName(module_name_to_collect_uses_of) =
            symbol_to_collect_uses_of
        {
            if module_name_to_collect_uses_of == &import_module_name_node.value {
                uses_so_far.push(import_module_name_node.range);
            }
        } else if let ElmDeclaredSymbol::ImportAlias {
            module_origin: alias_to_collect_uses_of_origin,
            alias_name: alias_to_collect_uses_of_name,
        } = symbol_to_collect_uses_of
        {
            if alias_to_collect_uses_of_origin == &import_module_name_node.value
                && let Some(import_alias) = &elm_syntax_import.alias
                && let Some(import_alias_name_node) = &import_alias.name
                && alias_to_collect_uses_of_name == &import_alias_name_node.value
            {
                uses_so_far.push(import_alias_name_node.range);
            }
        } else if let Some(exposing) = &elm_syntax_import.exposing
            && let Some(exposing_specific) = &exposing.specific
        {
            elm_syntax_exposing_specific_uses_of_reference_into(
                uses_so_far,
                &import_module_name_node.value,
                &exposing_specific.value,
                symbol_to_collect_uses_of,
            );
        }
    }
}

fn elm_syntax_exposing_specific_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    elm_syntax_exposing: &ElmSyntaxExposingSpecific,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_exposing {
        ElmSyntaxExposingSpecific::All(_) => {}
        ElmSyntaxExposingSpecific::Explicit(exposes) => {
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => {
                        if symbol_to_collect_uses_of
                            == (ElmDeclaredSymbol::TypeNotRecordAlias {
                                name: &name.value,
                                module_origin: origin_module,
                            })
                        {
                            uses_so_far.push(name.range);
                        }
                    }
                    ElmSyntaxExpose::Operator(_) => {}
                    ElmSyntaxExpose::Type(name) => {
                        if (symbol_to_collect_uses_of
                            == (ElmDeclaredSymbol::TypeNotRecordAlias {
                                name: name,
                                module_origin: origin_module,
                            }))
                            || (symbol_to_collect_uses_of
                                == (ElmDeclaredSymbol::RecordTypeAlias {
                                    name: name,
                                    module_origin: origin_module,
                                }))
                        {
                            uses_so_far.push(expose.range);
                        }
                    }
                    ElmSyntaxExpose::Variable(name) => {
                        if symbol_to_collect_uses_of
                            == (ElmDeclaredSymbol::VariableOrVariant {
                                name: name,
                                module_origin: origin_module,
                            })
                        {
                            uses_so_far.push(expose.range);
                        }
                    }
                }
            }
        }
    }
}

fn elm_syntax_declaration_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_declaration: &ElmSyntaxDeclaration,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_declaration {
        ElmSyntaxDeclaration::ChoiceType {
            name: maybe_name,
            parameters,
            equals_key_symbol_range: _,
            variant0_name: maybe_variant0_name,
            variant0_values,
            variant1_up,
        } => {
            if let Some(name_node) = maybe_name
                && symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::TypeNotRecordAlias {
                        module_origin: origin_module,
                        name: &name_node.value,
                    })
            {
                uses_so_far.push(name_node.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::TypeVariable(&parameter_node.value)
                {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal;
                }
            }
            if let Some(variant0_name_node) = maybe_variant0_name
                && symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        name: &variant0_name_node.value,
                        module_origin: origin_module,
                    })
            {
                uses_so_far.push(variant0_name_node.range);
                return;
            }
            for variant0_value in variant0_values {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(variant0_value),
                    symbol_to_collect_uses_of,
                );
            }
            for variant in variant1_up {
                if let Some(variant_name_node) = &variant.name
                    && (ElmDeclaredSymbol::VariableOrVariant {
                        name: &variant_name_node.value,
                        module_origin: origin_module,
                    }) == symbol_to_collect_uses_of
                {
                    uses_so_far.push(variant_name_node.range);
                    return;
                }
                for variant0_value in variant.values.iter() {
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(variant0_value),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxDeclaration::Operator { .. } => {}
        ElmSyntaxDeclaration::Port {
            name: maybe_name,
            colon_key_symbol_range: _,
            type_: maybe_type,
        } => {
            if let Some(name_node) = maybe_name
                && symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        name: &name_node.value,
                        module_origin: origin_module,
                    })
            {
                uses_so_far.push(name_node.range);
            }
            if let Some(type_node) = maybe_type {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(type_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range: _,
            name: maybe_name,
            parameters,
            equals_key_symbol_range: _,
            type_: maybe_type,
        } => {
            if let Some(name_node) = maybe_name
                && ((symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::TypeNotRecordAlias {
                        name: &name_node.value,
                        module_origin: origin_module,
                    }))
                    || (symbol_to_collect_uses_of
                        == (ElmDeclaredSymbol::RecordTypeAlias {
                            name: &name_node.value,
                            module_origin: origin_module,
                        })))
            {
                uses_so_far.push(name_node.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::TypeVariable(&parameter_node.value)
                {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal;
                }
            }
            if let Some(type_node) = maybe_type {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(type_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxDeclaration::Variable {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: _,
            result: maybe_result,
        } => {
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &start_name_node.value,
                    module_origin: origin_module,
                })
            {
                uses_so_far.push(start_name_node.range);
            }
            if let Some(signature) = maybe_signature {
                if let Some(implementation_name_range) = signature.implementation_name_range
                    && symbol_to_collect_uses_of
                        == (ElmDeclaredSymbol::VariableOrVariant {
                            name: &start_name_node.value,
                            module_origin: origin_module,
                        })
                {
                    uses_so_far.push(implementation_name_range);
                }
                if let Some(signature_type_node) = &signature.type_ {
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(signature_type_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
            for parameter in parameters {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter),
                    symbol_to_collect_uses_of,
                );
            }
            let mut parameter_bindings = Vec::new();
            for parameter_node in parameters {
                elm_syntax_pattern_bindings_into(
                    &mut parameter_bindings,
                    elm_syntax_node_as_ref(parameter_node),
                )
            }
            if let Some(result_node) = maybe_result {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &parameter_bindings,
                    elm_syntax_node_as_ref(result_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
    }
}

fn elm_syntax_type_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type_node: ElmSyntaxNode<&ElmSyntaxType>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_type_node.value {
        ElmSyntaxType::Construct {
            reference,
            arguments,
        } => {
            let module_origin = look_up_origin_module(
                module_origin_lookup,
                &reference.value.qualification,
                &reference.value.name,
            );
            if (symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::TypeNotRecordAlias {
                    module_origin: module_origin,
                    name: &reference.value.name,
                }))
                || (symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::RecordTypeAlias {
                        module_origin: module_origin,
                        name: &reference.value.name,
                    }))
            {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(
                        reference.range.end,
                        -(reference.value.name.len() as i32),
                    ),
                    end: reference.range.end,
                });
            }
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::ImportAlias {
                    module_origin: module_origin,
                    alias_name: &reference.value.qualification,
                })
            {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            } else if (symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(module_origin))
                && (&reference.value.qualification == module_origin)
            {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            }
            for argument in arguments {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(argument),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::Function {
            input,
            arrow_key_symbol_range: _,
            output: maybe_output,
        } => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(input),
                symbol_to_collect_uses_of,
            );
            if let Some(output_node) = maybe_output {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(output_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::Parenthesized(in_parens) => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxType::Record(fields) => {
            for field in fields {
                if let Some(field_value_node) = &field.value {
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(field_value_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            if let Some(record_variable_node) = maybe_record_variable
                && symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::TypeVariable(&record_variable_node.value)
            {
                uses_so_far.push(record_variable_node.range);
            }
            for field in fields {
                if let Some(field_value_node) = &field.value {
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(field_value_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxType::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part2_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::Unit => {}
        ElmSyntaxType::Variable(variable) => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(variable) {
                uses_so_far.push(elm_syntax_type_node.range);
            }
        }
    }
}

fn elm_syntax_expression_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    local_bindings: &[ElmLocalBinding],
    elm_syntax_expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_expression_node.value {
        ElmSyntaxExpression::Call {
            called,
            argument0,
            argument1_up,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(called),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(argument0),
                symbol_to_collect_uses_of,
            );
            for argument_node in argument1_up {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_as_ref(argument_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::CaseOf {
            matched: maybe_matched,
            of_keyword_range: _,
            cases,
        } => {
            if let Some(matched_node) = maybe_matched {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(matched_node),
                    symbol_to_collect_uses_of,
                );
            }
            for case in cases {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&case.pattern),
                    symbol_to_collect_uses_of,
                );
                if let Some(case_result_node) = &case.result {
                    let mut local_bindings_including_from_case_pattern: Vec<ElmLocalBinding> =
                        local_bindings.to_vec();
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings_including_from_case_pattern,
                        elm_syntax_node_as_ref(&case.pattern),
                    );
                    elm_syntax_expression_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        &local_bindings_including_from_case_pattern,
                        elm_syntax_node_as_ref(case_result_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxExpression::Char(_) => {}
        ElmSyntaxExpression::Float(_) => {}
        ElmSyntaxExpression::IfThenElse {
            condition: maybe_condition,
            then_keyword_range: _,
            on_true: maybe_on_true,
            else_keyword_range: _,
            on_false: maybe_on_false,
        } => {
            if let Some(condition_node) = maybe_condition {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(condition_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(on_true_node) = maybe_on_true {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(on_true_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(on_false_node) = maybe_on_false {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(on_false_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left,
            operator: _,
            right: maybe_right,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(left),
                symbol_to_collect_uses_of,
            );
            if let Some(right_node) = maybe_right {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(right_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Integer { .. } => {}
        ElmSyntaxExpression::Lambda {
            parameters,
            arrow_key_symbol_range: _,
            result: maybe_result,
        } => {
            for parameter_node in parameters {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(result_node) = maybe_result {
                let mut local_bindings_including_from_lambda_parameters = local_bindings.to_vec();
                for parameter_node in parameters {
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings_including_from_lambda_parameters,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_from_lambda_parameters,
                    elm_syntax_node_unbox(result_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::LetIn {
            declarations,
            in_keyword_range: _,
            result: maybe_result,
        } => {
            let mut local_bindings_including_let_declaration_introduced: Vec<ElmLocalBinding> =
                local_bindings.to_vec();
            for let_declaration_node in declarations {
                elm_syntax_let_declaration_introduced_bindings_into(
                    &mut local_bindings_including_let_declaration_introduced,
                    &let_declaration_node.value,
                );
            }
            for let_declaration_node in declarations {
                elm_syntax_let_declaration_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_let_declaration_introduced,
                    &let_declaration_node.value,
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(result) = maybe_result {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_let_declaration_introduced,
                    elm_syntax_node_unbox(result),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::List(elements) => {
            for element_node in elements {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_as_ref(element_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Negation(maybe_in_negation) => {
            if let Some(in_negation_node) = maybe_in_negation {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(in_negation_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::OperatorFunction(_) => {}
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::Record(fields) => {
            for field in fields {
                if let Some(field_value_node) = &field.value {
                    elm_syntax_expression_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        local_bindings,
                        elm_syntax_node_as_ref(field_value_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxExpression::RecordAccess { record, field: _ } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(record),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::RecordAccessFunction(_) => {}
        ElmSyntaxExpression::RecordUpdate {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            if let Some(record_variable_node) = maybe_record_variable {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::LocalBinding(&record_variable_node.value)
                {
                    if local_bindings
                        .iter()
                        .any(|local_binding| local_binding.name == &record_variable_node.value)
                    {
                        uses_so_far.push(record_variable_node.range);
                    }
                } else if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        module_origin: look_up_origin_module(
                            module_origin_lookup,
                            "",
                            &record_variable_node.value,
                        ),
                        name: &record_variable_node.value,
                    })
                {
                    uses_so_far.push(record_variable_node.range);
                }
            }
            for field in fields {
                if let Some(field_value_node) = &field.value {
                    elm_syntax_expression_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        local_bindings,
                        elm_syntax_node_as_ref(field_value_node),
                        symbol_to_collect_uses_of,
                    );
                }
            }
        }
        ElmSyntaxExpression::Reference {
            qualification,
            name,
        } => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(name) {
                if qualification.is_empty()
                    && local_bindings
                        .iter()
                        .any(|local_binding| local_binding.name == name)
                {
                    uses_so_far.push(elm_syntax_expression_node.range);
                }
            } else {
                let module_origin =
                    look_up_origin_module(module_origin_lookup, &qualification, &name);
                if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        module_origin: module_origin,
                        name: name,
                    })
                {
                    uses_so_far.push(lsp_types::Range {
                        start: lsp_position_add_characters(
                            elm_syntax_expression_node.range.end,
                            -(name.len() as i32),
                        ),
                        end: elm_syntax_expression_node.range.end,
                    });
                } else if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::ImportAlias {
                        module_origin: module_origin,
                        alias_name: &qualification,
                    })
                {
                    uses_so_far.push(lsp_types::Range {
                        start: elm_syntax_expression_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            qualification.len() as i32,
                        ),
                    });
                } else if (symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::ModuleName(module_origin))
                    && (qualification == module_origin)
                {
                    uses_so_far.push(lsp_types::Range {
                        start: elm_syntax_expression_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            qualification.len() as i32,
                        ),
                    });
                }
            }
        }
        ElmSyntaxExpression::String { .. } => {}
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(part2_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Unit => {}
    }
}

fn elm_syntax_let_declaration_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    local_bindings: &[ElmLocalBinding],
    elm_syntax_let_declaration: &ElmSyntaxLetDeclaration,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern,
            equals_key_symbol_range: _,
            expression: maybe_expression,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(pattern),
                symbol_to_collect_uses_of,
            );
            if let Some(expression_node) = maybe_expression {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_as_ref(expression_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: _,
            result: maybe_result,
        } => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(&start_name_node.value)
            {
                uses_so_far.push(start_name_node.range);
                if let Some(signature) = maybe_signature
                    && let Some(implementation_name_range) = signature.implementation_name_range
                {
                    uses_so_far.push(implementation_name_range);
                }
                return;
            }
            if let Some(signature) = maybe_signature
                && let Some(signature_type_node) = &signature.type_
            {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(signature_type_node),
                    symbol_to_collect_uses_of,
                );
            }
            for parameter in parameters {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(result_node) = maybe_result {
                let mut local_bindings_including_from_let_function_parameters: Vec<
                    ElmLocalBinding,
                > = local_bindings.to_vec();
                for parameter_node in parameters {
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings_including_from_let_function_parameters,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_from_let_function_parameters,
                    elm_syntax_node_as_ref(result_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
    }
}

fn elm_syntax_pattern_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern: alias_pattern,
            as_keyword_range: _,
            variable: maybe_variable,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(alias_pattern),
                symbol_to_collect_uses_of,
            );
            if let Some(variable_node) = maybe_variable
                && symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::LocalBinding(&variable_node.value)
            {
                uses_so_far.push(variable_node.range);
            }
        }
        ElmSyntaxPattern::Char(_) => {}
        ElmSyntaxPattern::Ignored => {}
        ElmSyntaxPattern::Int { .. } => {}
        ElmSyntaxPattern::ListCons {
            head: maybe_head,
            cons_key_symbol: _,
            tail: maybe_tail,
        } => {
            if let Some(head_node) = maybe_head {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(head_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(tail_node) = maybe_tail {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(tail_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxPattern::ListExact(elements) => {
            for element in elements {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(element),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxPattern::Record(field_names) => {
            'find_in_field_names: for field_name_node in field_names {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::LocalBinding(&field_name_node.value)
                {
                    uses_so_far.push(field_name_node.range);
                    break 'find_in_field_names;
                }
            }
        }
        ElmSyntaxPattern::String { .. } => {}
        ElmSyntaxPattern::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part2_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxPattern::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part0_node),
                    symbol_to_collect_uses_of,
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1_node),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxPattern::Unit => {}
        ElmSyntaxPattern::Variable(variable) => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(variable) {
                uses_so_far.push(elm_syntax_pattern_node.range);
            }
        }
        ElmSyntaxPattern::Variant { reference, values } => {
            let module_origin = look_up_origin_module(
                module_origin_lookup,
                &reference.value.qualification,
                &reference.value.name,
            );
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::VariableOrVariant {
                    module_origin: module_origin,
                    name: &reference.value.name,
                })
            {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(
                        reference.range.end,
                        -(reference.value.name.len() as i32),
                    ),
                    end: reference.range.end,
                });
            }
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::ImportAlias {
                    module_origin: module_origin,
                    alias_name: &reference.value.qualification,
                })
            {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            } else if (symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(module_origin))
                && (&reference.value.qualification == module_origin)
            {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            }
            for value in values {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(value),
                    symbol_to_collect_uses_of,
                );
            }
        }
    }
}

fn elm_syntax_let_declaration_introduced_bindings_into<'a>(
    bindings_so_far: &mut Vec<ElmLocalBinding<'a>>,
    elm_syntax_let_declaration: &'a ElmSyntaxLetDeclaration,
) {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring { pattern, .. } => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_as_ref(pattern));
        }
        ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name: start_name_node,
            signature,
            ..
        } => {
            bindings_so_far.push(ElmLocalBinding {
                name: &start_name_node.value,
                origin: LocalBindingOrigin::LetDeclaredVariable {
                    signature_type: signature
                        .as_ref()
                        .and_then(|signature| signature.type_.as_ref())
                        .map(|node| &node.value),
                    start_name_range: start_name_node.range,
                },
            });
        }
    }
}

fn elm_syntax_pattern_bindings_into<'a>(
    bindings_so_far: &mut Vec<ElmLocalBinding<'a>>,
    elm_syntax_pattern_node: ElmSyntaxNode<&'a ElmSyntaxPattern>,
) {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern: aliased_pattern_node,
            as_keyword_range: _,
            variable: maybe_variable,
        } => {
            elm_syntax_pattern_bindings_into(
                bindings_so_far,
                elm_syntax_node_unbox(aliased_pattern_node),
            );
            if let Some(variable_node) = maybe_variable {
                bindings_so_far.push(ElmLocalBinding {
                    origin: LocalBindingOrigin::PatternVariable(variable_node.range),
                    name: &variable_node.value,
                });
            }
        }
        ElmSyntaxPattern::Char(_) => {}
        ElmSyntaxPattern::Ignored => {}
        ElmSyntaxPattern::Int { .. } => {}
        ElmSyntaxPattern::ListCons {
            head: maybe_head,
            cons_key_symbol: _,
            tail: maybe_tail,
        } => {
            if let Some(head_node) = maybe_head {
                elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(head_node));
            }
            if let Some(tail_node) = maybe_tail {
                elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(tail_node));
            }
        }
        ElmSyntaxPattern::ListExact(elements) => {
            for element_node in elements {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_as_ref(element_node),
                );
            }
        }
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(in_parens));
        }
        ElmSyntaxPattern::Record(field_names) => {
            bindings_so_far.extend(field_names.iter().map(|field_name_node| ElmLocalBinding {
                origin: LocalBindingOrigin::PatternRecordField(field_name_node.range),
                name: &field_name_node.value,
            }));
        }
        ElmSyntaxPattern::String { .. } => {}
        ElmSyntaxPattern::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_unbox(part2_node),
                );
            }
        }

        ElmSyntaxPattern::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
        }
        ElmSyntaxPattern::Unit => {}
        ElmSyntaxPattern::Variable(variable) => {
            bindings_so_far.push(ElmLocalBinding {
                origin: LocalBindingOrigin::PatternVariable(elm_syntax_pattern_node.range),
                name: variable,
            });
        }
        ElmSyntaxPattern::Variant {
            reference: _,
            values,
        } => {
            for value_node in values {
                elm_syntax_pattern_bindings_into(
                    bindings_so_far,
                    elm_syntax_node_as_ref(value_node),
                );
            }
        }
    }
}

enum ElmSyntaxHighlightKind {
    Type,
    TypeVariable,
    Variant,
    Field,
    ModuleNameOrAlias,
    Variable,
    Comment,
    String,
    Number,
    VariableDeclaration,
    KeySymbol,
}

fn elm_syntax_highlight_module_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_module: &ElmSyntaxModule,
) {
    if let Some(module_header) = &elm_syntax_module.header {
        elm_syntax_highlight_module_header_into(highlighted_so_far, module_header);
    }
    if let Some(documentation_node) = &elm_syntax_module.documentation {
        highlighted_so_far.extend(elm_syntax_highlight_and_comment(
            elm_syntax_node_as_ref(documentation_node),
            3,
            2,
        ));
    }
    for import_node in elm_syntax_module.imports.iter() {
        elm_syntax_highlight_import_into(highlighted_so_far, elm_syntax_node_as_ref(import_node));
    }
    for documented_declaration in elm_syntax_module.declarations.iter() {
        if let Some(documentation_node) = &documented_declaration.documentation {
            highlighted_so_far.extend(elm_syntax_highlight_and_comment(
                elm_syntax_node_as_ref(documentation_node),
                3,
                2,
            ));
        }
        if let Some(declaration_node) = &documented_declaration.declaration {
            elm_syntax_highlight_declaration_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(declaration_node),
            );
        }
    }
    // Inserting many comments in the middle can get expensive (having so many comments to make it matter will be rare).
    // A possible solution (when comment count exceeds other syntax by some factor) is just pushing all comments an sorting the whole thing at once.
    // Feels like overkill, though so I'll hold on on this until issues are opened :)
    for comment_node in elm_syntax_module.comments.iter() {
        elm_syntax_highlight_and_place_comment_into(
            highlighted_so_far,
            elm_syntax_node_as_ref(comment_node),
        );
    }
}

fn elm_syntax_highlight_module_header_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_module_header: &ElmSyntaxModuleHeader,
) {
    match &elm_syntax_module_header.specific {
        ElmSyntaxModuleHeaderSpecific::Pure {
            module_keyword_range,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: *module_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(module_name_range) = &elm_syntax_module_header.module_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: module_name_range.range,
                    value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
                });
            }
            if let Some(exposing) = &elm_syntax_module_header.exposing {
                elm_syntax_highlight_exposing_into(highlighted_so_far, exposing);
            }
        }
        ElmSyntaxModuleHeaderSpecific::Effect {
            effect_keyword_range,
            module_keyword_range,
            where_keyword_range,
            command: maybe_command,
            subscription: maybe_subscription,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: *effect_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: *module_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(module_name_node) = &elm_syntax_module_header.module_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: module_name_node.range,
                    value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
                });
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *where_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(command_node) = maybe_command {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: command_node.key_range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: command_node.equals_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: command_node.value_type_name.range,
                    value: ElmSyntaxHighlightKind::VariableDeclaration,
                });
            }
            if let Some(subscription_node) = maybe_subscription {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: subscription_node.key_range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: subscription_node.equals_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: subscription_node.value_type_name.range,
                    value: ElmSyntaxHighlightKind::VariableDeclaration,
                });
            }
        }
        ElmSyntaxModuleHeaderSpecific::Port {
            module_keyword_range,
            port_keyword_range,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: *port_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: *module_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(module_name_node) = &elm_syntax_module_header.module_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: module_name_node.range,
                    value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
                });
            }
            if let Some(exposing) = &elm_syntax_module_header.exposing {
                elm_syntax_highlight_exposing_into(highlighted_so_far, exposing);
            }
        }
    }
}

fn elm_syntax_highlight_and_place_comment_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_comment_node: ElmSyntaxNode<&ElmSyntaxComment>,
) {
    let insert_index: usize = highlighted_so_far
        .binary_search_by(|token| {
            lsp_position_compare(token.range.start, elm_syntax_comment_node.range.start)
        })
        .unwrap_or_else(|i| i);
    match elm_syntax_comment_node.value.kind {
        ElmSyntaxCommentKind::UntilLinebreak => {
            highlighted_so_far.insert(
                insert_index,
                ElmSyntaxNode {
                    range: elm_syntax_comment_node.range,
                    value: ElmSyntaxHighlightKind::Comment,
                },
            );
        }
        ElmSyntaxCommentKind::Block => {
            highlighted_so_far.splice(
                insert_index..insert_index,
                elm_syntax_highlight_and_comment(
                    ElmSyntaxNode {
                        range: elm_syntax_comment_node.range,
                        value: &elm_syntax_comment_node.value.content,
                    },
                    2,
                    2,
                ),
            );
        }
    }
}
fn elm_syntax_highlight_and_comment(
    elm_syntax_comment_node: ElmSyntaxNode<&String>,
    characters_before_content: usize,
    characters_after_content: usize,
) -> impl Iterator<Item = ElmSyntaxNode<ElmSyntaxHighlightKind>> {
    let content_does_not_break_line: bool =
        elm_syntax_comment_node.range.start.line == elm_syntax_comment_node.range.end.line;
    elm_syntax_comment_node
        .value
        .lines()
        .chain(
            // str::lines() eats the last linebreak. Restore it
            if elm_syntax_comment_node.value.ends_with("\n") {
                Some("\n").into_iter()
            } else {
                None.into_iter()
            },
        )
        .enumerate()
        .map(move |(inner_line, inner_line_str)| {
            let line: u32 = elm_syntax_comment_node.range.start.line + (inner_line as u32);
            let line_length_utf16: usize = inner_line_str.encode_utf16().count();
            ElmSyntaxNode {
                range: if inner_line == 0 {
                    lsp_types::Range {
                        start: elm_syntax_comment_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_comment_node.range.start,
                            (characters_before_content
                                + line_length_utf16
                                + if content_does_not_break_line {
                                    characters_after_content
                                } else {
                                    0
                                }) as i32,
                        ),
                    }
                } else {
                    lsp_types::Range {
                        start: lsp_types::Position {
                            line: line,
                            character: 0,
                        },
                        end: if line == elm_syntax_comment_node.range.end.line {
                            elm_syntax_comment_node.range.end
                        } else {
                            lsp_types::Position {
                                line: line,
                                character: (line_length_utf16 + characters_after_content) as u32,
                            }
                        },
                    }
                },
                value: ElmSyntaxHighlightKind::Comment,
            }
        })
}

fn elm_syntax_highlight_import_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_import_node: ElmSyntaxNode<&ElmSyntaxImport>,
) {
    highlighted_so_far.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: elm_syntax_import_node.range.start,
            end: lsp_position_add_characters(elm_syntax_import_node.range.start, 6),
        },
        value: ElmSyntaxHighlightKind::KeySymbol,
    });
    if let Some(module_name_node) = &elm_syntax_import_node.value.module_name {
        highlighted_so_far.push(ElmSyntaxNode {
            range: module_name_node.range,
            value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
        });
    }
    if let Some(alias_node) = &elm_syntax_import_node.value.alias {
        highlighted_so_far.push(ElmSyntaxNode {
            range: alias_node.as_keyword_range,
            value: ElmSyntaxHighlightKind::KeySymbol,
        });
        if let Some(alias_name_node) = &alias_node.name {
            highlighted_so_far.push(ElmSyntaxNode {
                range: alias_name_node.range,
                value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
            });
        }
    }
    if let Some(exposing) = &elm_syntax_import_node.value.exposing {
        elm_syntax_highlight_exposing_into(highlighted_so_far, exposing);
    }
}

fn elm_syntax_highlight_exposing_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_exposing: &ElmSyntaxExposing,
) {
    highlighted_so_far.push(ElmSyntaxNode {
        range: elm_syntax_exposing.exposing_keyword_range,
        value: ElmSyntaxHighlightKind::KeySymbol,
    });
    match elm_syntax_exposing
        .specific
        .as_ref()
        .map(|node| &node.value)
    {
        None => {}
        Some(ElmSyntaxExposingSpecific::All(ellipsis_range)) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: *ellipsis_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
        }
        Some(ElmSyntaxExposingSpecific::Explicit(exposes)) => {
            for expose_node in exposes {
                match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: type_name_node,
                        open_range: maybe_open_range,
                    } => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: type_name_node.range,
                            value: ElmSyntaxHighlightKind::Type,
                        });
                        if let &Some(open_range) = maybe_open_range {
                            highlighted_so_far.push(ElmSyntaxNode {
                                range: open_range,
                                value: ElmSyntaxHighlightKind::Variant,
                            });
                        }
                    }
                    ElmSyntaxExpose::Operator(maybe_operator) => {
                        if let Some(operator_node) = maybe_operator {
                            highlighted_so_far.push(ElmSyntaxNode {
                                range: operator_node.range,
                                value: ElmSyntaxHighlightKind::KeySymbol,
                            });
                        }
                    }
                    ElmSyntaxExpose::Type(_) => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: expose_node.range,
                            value: ElmSyntaxHighlightKind::Type,
                        });
                    }
                    ElmSyntaxExpose::Variable(_) => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: expose_node.range,
                            value: ElmSyntaxHighlightKind::VariableDeclaration,
                        });
                    }
                }
            }
        }
    }
}

fn elm_syntax_highlight_declaration_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_declaration_node: ElmSyntaxNode<&ElmSyntaxDeclaration>,
) {
    match elm_syntax_declaration_node.value {
        ElmSyntaxDeclaration::Variable {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: start_name_node.range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
            if let Some(signature) = maybe_signature {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: signature.colon_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                if let Some(signature_type_node) = &signature.type_ {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(signature_type_node),
                    );
                }
                if let Some(implementation_name_range) = signature.implementation_name_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: implementation_name_range,
                        value: ElmSyntaxHighlightKind::VariableDeclaration,
                    });
                }
            }
            for parameter_node in parameters {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(result_node) = maybe_result {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(result_node),
                );
            }
        }
        ElmSyntaxDeclaration::ChoiceType {
            name: maybe_name,
            parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            variant0_name: maybe_variant0_name,
            variant0_values,
            variant1_up,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(name_node) = maybe_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: name_node.range,
                    value: ElmSyntaxHighlightKind::Type,
                });
            }
            for parameter_name_node in parameters {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: parameter_name_node.range,
                    value: ElmSyntaxHighlightKind::TypeVariable,
                });
            }
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(variant0_name_node) = maybe_variant0_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: variant0_name_node.range,
                    value: ElmSyntaxHighlightKind::Variant,
                });
            }
            for variant0_value_node in variant0_values {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(variant0_value_node),
                );
            }
            for variant in variant1_up {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: variant.or_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                if let Some(variant_name_node) = &variant.name {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: variant_name_node.range,
                        value: ElmSyntaxHighlightKind::Variant,
                    });
                }
                for variant_value_node in variant.values.iter() {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(variant_value_node),
                    );
                }
            }
        }
        ElmSyntaxDeclaration::Operator {
            direction: maybe_direction,
            precedence: maybe_precedence,
            operator: maybe_operator,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            function: maybe_function_name,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 5),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(direction_node) = maybe_direction {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: direction_node.range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(precedence_node) = maybe_precedence {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: precedence_node.range,
                    value: ElmSyntaxHighlightKind::Number,
                });
            }
            if let Some(operator_node) = maybe_operator {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: operator_node.range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(function_name_node) = maybe_function_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: function_name_node.range,
                    value: ElmSyntaxHighlightKind::VariableDeclaration,
                });
            }
        }
        ElmSyntaxDeclaration::Port {
            name: maybe_name,
            colon_key_symbol_range: maybe_colon_key_symbol_range,
            type_: maybe_type,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(name_node) = maybe_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: name_node.range,
                    value: ElmSyntaxHighlightKind::VariableDeclaration,
                });
            }
            if let &Some(colon_key_symbol_range) = maybe_colon_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: colon_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(type_node) = maybe_type {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(type_node),
                );
            }
        }
        ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range,
            name: maybe_name,
            parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            type_: maybe_type,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: *alias_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(name_node) = maybe_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: name_node.range,
                    value: ElmSyntaxHighlightKind::Type,
                });
            }
            for parameter_name_node in parameters {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: parameter_name_node.range,
                    value: ElmSyntaxHighlightKind::TypeVariable,
                });
            }
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(type_node) = maybe_type {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(type_node),
                );
            }
        }
    }
}

fn elm_syntax_highlight_qualified_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    qualified_node: ElmSyntaxNode<ElmQualified>,
    kind: ElmSyntaxHighlightKind,
) {
    if qualified_node.value.qualification.is_empty() {
        highlighted_so_far.push(ElmSyntaxNode {
            range: qualified_node.range,
            value: kind,
        })
    } else {
        let name_start_position: lsp_types::Position = lsp_position_add_characters(
            qualified_node.range.end,
            -(qualified_node.value.name.encode_utf16().count() as i32),
        );
        highlighted_so_far.push(ElmSyntaxNode {
            range: lsp_types::Range {
                start: qualified_node.range.start,
                end: name_start_position,
            },
            value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
        });
        highlighted_so_far.push(ElmSyntaxNode {
            range: lsp_types::Range {
                start: name_start_position,
                end: qualified_node.range.end,
            },
            value: kind,
        });
    }
}
fn elm_syntax_highlight_pattern_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
) {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern: alias_pattern_node,
            as_keyword_range,
            variable: maybe_variable,
        } => {
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_unbox(alias_pattern_node),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *as_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(variable_node) = maybe_variable {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: variable_node.range,
                    value: ElmSyntaxHighlightKind::Variable,
                });
            }
        }
        ElmSyntaxPattern::Char(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxPattern::Ignored => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
        }
        ElmSyntaxPattern::Int { .. } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::Number,
            });
        }
        ElmSyntaxPattern::ListCons {
            head: maybe_head,
            cons_key_symbol,
            tail: maybe_tail,
        } => {
            if let Some(head_node) = maybe_head {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(head_node),
                );
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *cons_key_symbol,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(tail_node) = maybe_tail {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(tail_node),
                );
            }
        }
        ElmSyntaxPattern::ListExact(elements) => {
            for element_node in elements {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(element_node),
                );
            }
        }
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(in_parens));
        }
        ElmSyntaxPattern::Record(field_names) => {
            for field_name_node in field_names {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field_name_node.range,
                    value: ElmSyntaxHighlightKind::Variable,
                });
            }
        }
        ElmSyntaxPattern::String {
            content: _,
            quoting_style: _,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxPattern::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part2_node),
                );
            }
        }
        ElmSyntaxPattern::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
        }
        ElmSyntaxPattern::Unit => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::Variant,
            });
        }
        ElmSyntaxPattern::Variable(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_pattern_node.range,
                value: ElmSyntaxHighlightKind::Variable,
            });
        }
        ElmSyntaxPattern::Variant {
            reference: reference_node,
            values,
        } => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                ElmSyntaxNode {
                    range: reference_node.range,
                    value: ElmQualified {
                        qualification: &reference_node.value.qualification,
                        name: &reference_node.value.name,
                    },
                },
                ElmSyntaxHighlightKind::Variant,
            );
            for value_node in values {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(value_node),
                );
            }
        }
    }
}
fn elm_syntax_highlight_type_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_type_node: ElmSyntaxNode<&ElmSyntaxType>,
) {
    match elm_syntax_type_node.value {
        ElmSyntaxType::Construct {
            reference: reference_node,
            arguments,
        } => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                ElmSyntaxNode {
                    range: reference_node.range,
                    value: ElmQualified {
                        qualification: &reference_node.value.qualification,
                        name: &reference_node.value.name,
                    },
                },
                ElmSyntaxHighlightKind::Type,
            );
            for argument_node in arguments {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(argument_node),
                );
            }
        }
        ElmSyntaxType::Function {
            input,
            arrow_key_symbol_range,
            output: maybe_output,
        } => {
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(input));
            highlighted_so_far.push(ElmSyntaxNode {
                range: *arrow_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(output_node) = maybe_output {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(output_node),
                );
            }
        }
        ElmSyntaxType::Parenthesized(in_parens) => {
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(in_parens));
        }
        ElmSyntaxType::Record(fields) => {
            for field in fields {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                if let Some(colon_key_symbol_range) = field.colon_key_symbol_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: colon_key_symbol_range,
                        value: ElmSyntaxHighlightKind::KeySymbol,
                    });
                }
                if let Some(field_value_node) = &field.value {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(field_value_node),
                    );
                }
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable: maybe_record_variable,
            bar_key_symbol_range,
            fields,
        } => {
            if let Some(record_variable_node) = maybe_record_variable {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: record_variable_node.range,
                    value: ElmSyntaxHighlightKind::TypeVariable,
                });
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *bar_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            for field in fields {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                if let Some(colon_key_symbol_range) = field.colon_key_symbol_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: colon_key_symbol_range,
                        value: ElmSyntaxHighlightKind::KeySymbol,
                    });
                }
                if let Some(field_value_node) = &field.value {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(field_value_node),
                    );
                }
            }
        }
        ElmSyntaxType::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part2_node),
                );
            }
        }
        ElmSyntaxType::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
        }
        ElmSyntaxType::Unit => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_type_node.range,
                value: ElmSyntaxHighlightKind::Type,
            });
        }
        ElmSyntaxType::Variable(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_type_node.range,
                value: ElmSyntaxHighlightKind::TypeVariable,
            });
        }
    }
}

fn elm_syntax_highlight_expression_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    match elm_syntax_expression_node.value {
        ElmSyntaxExpression::Call {
            called,
            argument0,
            argument1_up,
        } => {
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(called));
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(argument0),
            );
            for argument_node in argument1_up {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(argument_node),
                );
            }
        }
        ElmSyntaxExpression::CaseOf {
            matched: maybe_matched,
            of_keyword_range: maybe_of_keyword_range,
            cases,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(matched_node) = maybe_matched {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(matched_node),
                );
            }
            if let &Some(of_keyword_range) = maybe_of_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: of_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            for case in cases {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&case.pattern),
                );
                if let Some(arrow_key_symbol_range) = case.arrow_key_symbol_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: arrow_key_symbol_range,
                        value: ElmSyntaxHighlightKind::KeySymbol,
                    });
                }
                if let Some(result_node) = &case.result {
                    elm_syntax_highlight_expression_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(result_node),
                    );
                }
            }
        }
        ElmSyntaxExpression::Char(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxExpression::Float(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::Number,
            });
        }
        ElmSyntaxExpression::IfThenElse {
            condition: maybe_condition,
            then_keyword_range: maybe_then_keyword_range,
            on_true: maybe_on_true,
            else_keyword_range: maybe_else_keyword_range,
            on_false: maybe_on_false,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 2),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(condition_node) = maybe_condition {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(condition_node),
                );
            }
            if let &Some(then_keyword_range) = maybe_then_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: then_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(on_true_node) = maybe_on_true {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(on_true_node),
                );
            }
            if let Some(else_keyword_range) = maybe_else_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: *else_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(on_false_node) = maybe_on_false {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(on_false_node),
                );
            }
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left,
            operator: operator_node,
            right: maybe_right,
        } => {
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(left));
            highlighted_so_far.push(ElmSyntaxNode {
                range: operator_node.range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(right_node) = maybe_right {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(right_node),
                );
            }
        }
        ElmSyntaxExpression::Integer { .. } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::Number,
            });
        }
        ElmSyntaxExpression::Lambda {
            parameters,
            arrow_key_symbol_range: maybe_arrow_key_symbol_range,
            result: maybe_result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 1),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            for parameter_node in parameters {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            if let &Some(arrow_key_symbol_range) = maybe_arrow_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: arrow_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(result_node) = maybe_result {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(result_node),
                );
            }
        }
        ElmSyntaxExpression::LetIn {
            declarations,
            in_keyword_range: maybe_in_keyword_range,
            result: maybe_result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 3),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            for let_declaration_node in declarations {
                elm_syntax_highlight_let_declaration_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(let_declaration_node),
                );
            }
            if let &Some(in_keyword_range) = maybe_in_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: in_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(result_node) = maybe_result {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(result_node),
                );
            }
        }
        ElmSyntaxExpression::List(elements) => {
            for element_node in elements {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(element_node),
                );
            }
        }
        ElmSyntaxExpression::Negation(maybe_in_negation) => {
            if let Some(in_negation_node) = maybe_in_negation {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(in_negation_node),
                );
            }
        }
        ElmSyntaxExpression::OperatorFunction(operator_node) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: operator_node.range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
        }
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(in_parens),
            );
        }
        ElmSyntaxExpression::Record(fields) => {
            for field in fields {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                if let Some(equals_key_symbol_range) = field.equals_key_symbol_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: equals_key_symbol_range,
                        value: ElmSyntaxHighlightKind::KeySymbol,
                    });
                }
                if let Some(value_node) = &field.value {
                    elm_syntax_highlight_expression_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(value_node),
                    );
                }
            }
        }
        ElmSyntaxExpression::RecordAccess {
            record: record_node,
            field: maybe_field_name,
        } => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(record_node),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: record_node.range.end,
                    end: lsp_position_add_characters(record_node.range.end, 1),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            if let Some(field_name_node) = maybe_field_name {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field_name_node.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
            }
        }
        ElmSyntaxExpression::RecordAccessFunction(_) => {
            let field_name_start_position: lsp_types::Position =
                lsp_position_add_characters(elm_syntax_expression_node.range.start, 1);
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: field_name_start_position,
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: field_name_start_position,
                    end: elm_syntax_expression_node.range.end,
                },
                value: ElmSyntaxHighlightKind::Field,
            });
        }
        ElmSyntaxExpression::RecordUpdate {
            record_variable: maybe_record_variable,
            bar_key_symbol_range,
            fields,
        } => {
            if let Some(record_variable_node) = maybe_record_variable {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: record_variable_node.range,
                    value: ElmSyntaxHighlightKind::Variable,
                });
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *bar_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            for field in fields {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                if let Some(equals_key_symbol_range) = field.equals_key_symbol_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: equals_key_symbol_range,
                        value: ElmSyntaxHighlightKind::KeySymbol,
                    });
                }
                if let Some(value_node) = &field.value {
                    elm_syntax_highlight_expression_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(value_node),
                    );
                }
            }
        }
        ElmSyntaxExpression::Reference {
            qualification,
            name,
        } => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                ElmSyntaxNode {
                    range: elm_syntax_expression_node.range,
                    value: ElmQualified {
                        qualification: qualification.as_str(),
                        name: name.as_str(),
                    },
                },
                if name.starts_with(|c: char| c.is_uppercase()) {
                    ElmSyntaxHighlightKind::Variant
                } else {
                    ElmSyntaxHighlightKind::Variable
                },
            );
        }
        ElmSyntaxExpression::String {
            content: _,
            quoting_style: _,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part2_node),
                );
            }
        }
        ElmSyntaxExpression::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
        }
        ElmSyntaxExpression::Unit => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::Variant,
            });
        }
    }
}

fn elm_syntax_highlight_let_declaration_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_let_declaration_node: ElmSyntaxNode<&ElmSyntaxLetDeclaration>,
) {
    match elm_syntax_let_declaration_node.value {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern: destructuring_pattern_node,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            expression: maybe_destructured_expression,
        } => {
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(destructuring_pattern_node),
            );
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(destructured_expression_node) = maybe_destructured_expression {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(destructured_expression_node),
                );
            }
        }
        ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: start_name_node.range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
            if let Some(signature) = maybe_signature {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: signature.colon_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                if let Some(signature_type_node) = &signature.type_ {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(signature_type_node),
                    );
                }
                if let Some(implementation_name_range) = signature.implementation_name_range {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: implementation_name_range,
                        value: ElmSyntaxHighlightKind::VariableDeclaration,
                    });
                }
            }
            for parameter_node in parameters {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            if let &Some(equals_key_symbol_range) = maybe_equals_key_symbol_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(result_node) = maybe_result {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(result_node),
                );
            }
        }
    }
}

// //
struct ParseState<'a> {
    source: &'a str,
    offset_utf8: usize,
    position: lsp_types::Position,
    indent: u16,
    lower_indents_stack: Vec<u16>,
    comments: Vec<ElmSyntaxNode<ElmSyntaxComment>>,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxComment {
    kind: ElmSyntaxCommentKind,
    content: String,
}
#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxCommentKind {
    /// --
    UntilLinebreak,
    /// {- ... -}
    Block,
}

fn parse_state_push_indent(state: &mut ParseState, new_indent: u16) {
    state.lower_indents_stack.push(state.indent);
    state.indent = new_indent;
}
fn parse_state_pop_indent(state: &mut ParseState) {
    state.indent = state.lower_indents_stack.pop().unwrap_or(0);
}

fn str_starts_with_linebreak(str: &str) -> bool {
    // \r allowed because both \r and \r\n are counted as linebreak
    // see EOL in https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocuments
    str.starts_with("\n") || str.starts_with("\r")
}
fn parse_linebreak(state: &mut ParseState) -> bool {
    // see EOL in https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocuments
    if state.source[state.offset_utf8..].starts_with("\n") {
        state.offset_utf8 += 1;
        state.position.line += 1;
        state.position.character = 0;
        true
    } else if state.source[state.offset_utf8..].starts_with("\r\n") {
        state.offset_utf8 += 2;
        state.position.line += 1;
        state.position.character = 0;
        true
    } else if state.source[state.offset_utf8..].starts_with("\r") {
        state.offset_utf8 += 1;
        state.position.line += 1;
        state.position.character = 0;
        true
    } else {
        false
    }
}
fn parse_linebreak_as_str<'a>(state: &mut ParseState<'a>) -> Option<&'a str> {
    // see EOL in https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocuments
    if state.source[state.offset_utf8..].starts_with("\n") {
        state.offset_utf8 += 1;
        state.position.line += 1;
        state.position.character = 0;
        Some("\n")
    } else if state.source[state.offset_utf8..].starts_with("\r\n") {
        state.offset_utf8 += 2;
        state.position.line += 1;
        state.position.character = 0;
        Some("\r\n")
    } else if state.source[state.offset_utf8..].starts_with("\r") {
        state.offset_utf8 += 1;
        state.position.line += 1;
        state.position.character = 0;
        Some("\r")
    } else {
        None
    }
}
/// prefer using after parse_line_break or similar failed
fn parse_any_guaranteed_non_linebreak_char(state: &mut ParseState) -> bool {
    match state.source[state.offset_utf8..].chars().next() {
        None => false,
        Some(parsed_char) => {
            state.offset_utf8 += parsed_char.len_utf8();
            state.position.character += parsed_char.len_utf16() as u32;
            true
        }
    }
}
/// prefer using after parse_line_break or similar failed
fn parse_any_guaranteed_non_linebreak_char_as_char(state: &mut ParseState) -> Option<char> {
    match state.source[state.offset_utf8..].chars().next() {
        None => None,
        Some(parsed_char) => {
            state.offset_utf8 += parsed_char.len_utf8();
            state.position.character += parsed_char.len_utf16() as u32;
            Some(parsed_char)
        }
    }
}
/// symbol cannot be non-utf8 characters or \n
fn parse_char_symbol_as_char(state: &mut ParseState, symbol: char) -> Option<char> {
    if state.source[state.offset_utf8..].starts_with(symbol) {
        state.offset_utf8 += symbol.len_utf8();
        state.position.character += symbol.len_utf16() as u32;
        Some(symbol)
    } else {
        None
    }
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_symbol(state: &mut ParseState, symbol: &str) -> bool {
    if state.source[state.offset_utf8..].starts_with(symbol) {
        state.offset_utf8 += symbol.len();
        state.position.character += symbol.len() as u32;
        true
    } else {
        false
    }
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_symbol_as<A>(state: &mut ParseState, symbol: &'static str, result: A) -> Option<A> {
    if parse_symbol(state, symbol) {
        Some(result)
    } else {
        None
    }
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_symbol_as_str(state: &mut ParseState, symbol: &'static str) -> Option<&'static str> {
    parse_symbol_as(state, symbol, symbol)
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_symbol_as_range(state: &mut ParseState, symbol: &str) -> Option<lsp_types::Range> {
    let start_position: lsp_types::Position = state.position;
    if parse_symbol(state, symbol) {
        Some(lsp_types::Range {
            start: start_position,
            end: state.position,
        })
    } else {
        None
    }
}
/// given condition must not succeed on linebreak
fn parse_same_line_while(state: &mut ParseState, char_is_valid: impl Fn(char) -> bool) {
    let consumed_chars_iterator = state.source[state.offset_utf8..]
        .chars()
        .take_while(|&c| char_is_valid(c));
    let consumed_length_utf8: usize = consumed_chars_iterator.clone().map(char::len_utf8).sum();
    let consumed_length_utf16: usize = consumed_chars_iterator.clone().map(char::len_utf16).sum();
    state.offset_utf8 += consumed_length_utf8;
    state.position.character += consumed_length_utf16 as u32;
}
/// given condition must not succeed on linebreak
fn parse_same_line_while_as_str<'a>(
    state: &mut ParseState<'a>,
    char_is_valid: impl Fn(char) -> bool,
) -> &'a str {
    let start_offset_utf8: usize = state.offset_utf8;
    parse_same_line_while(state, char_is_valid);
    &state.source[start_offset_utf8..state.offset_utf8]
}
/// given condition must not succeed on linebreak
fn parse_same_line_char_if(state: &mut ParseState, char_is_valid: impl Fn(char) -> bool) -> bool {
    if let Some(next_char) = state.source[state.offset_utf8..].chars().next()
        && char_is_valid(next_char)
    {
        state.offset_utf8 += next_char.len_utf8();
        state.position.character += next_char.len_utf16() as u32;
        true
    } else {
        false
    }
}
fn parse_unsigned_integer_base10(state: &mut ParseState) -> bool {
    if parse_symbol(state, "0") {
        true
    } else if parse_same_line_char_if(state, |c| c >= '1' && c <= '9') {
        parse_same_line_while(state, |c| c.is_ascii_digit());
        true
    } else {
        false
    }
}

/// a valid elm symbol that must be followed by a character that could not be part of an elm identifier
fn parse_elm_keyword_as_range(state: &mut ParseState, symbol: &str) -> Option<lsp_types::Range> {
    if state.source[state.offset_utf8..].starts_with(symbol)
        && !(state.source[(state.offset_utf8 + symbol.len())..]
            .starts_with(|c: char| c.is_alphanumeric() || c == '_'))
    {
        let start_position: lsp_types::Position = state.position;
        state.offset_utf8 += symbol.len();
        state.position.character += symbol.len() as u32;
        Some(lsp_types::Range {
            start: start_position,
            end: state.position,
        })
    } else {
        None
    }
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(
    state: &mut ParseState,
    end_symbol: &'static str,
) {
    parse_elm_whitespace_and_comments(state);
    'until_including_symbol_or_end_of_source: while !parse_symbol(state, end_symbol) {
        if state.position.character <= 0 {
            break 'until_including_symbol_or_end_of_source;
        }
        if parse_any_guaranteed_non_linebreak_char(state) {
            parse_elm_whitespace_and_comments(state);
        } else {
            break 'until_including_symbol_or_end_of_source;
        }
    }
}
/// symbol cannot contain non-utf8 characters or \n
fn parse_until_including_symbol_or_excluding_end_of_line(
    state: &mut ParseState,
    end_symbol: &'static str,
) {
    'until_including_symbol_or_excluding_end_of_line: while !parse_symbol(state, end_symbol) {
        if str_starts_with_linebreak(&state.source[state.offset_utf8..]) {
            break 'until_including_symbol_or_excluding_end_of_line;
        } else if parse_any_guaranteed_non_linebreak_char(state) {
        } else {
            break 'until_including_symbol_or_excluding_end_of_line;
        }
    }
}

fn parse_elm_whitespace_and_comments(state: &mut ParseState) {
    while parse_linebreak(state)
        || parse_same_line_char_if(state, |c| c.is_whitespace())
        || parse_elm_comment(state)
    {}
}
fn parse_elm_comment(state: &mut ParseState) -> bool {
    parse_elm_comment_until_linebreak(state) || parse_elm_comment_block(state)
}
fn parse_elm_comment_until_linebreak(state: &mut ParseState) -> bool {
    let position_before: lsp_types::Position = state.position;
    if !parse_symbol(state, "--") {
        return false;
    }
    let comment: String = state.source[state.offset_utf8..]
        .lines()
        .next()
        .unwrap_or("")
        .to_string();
    state.offset_utf8 += comment.len();
    state.position.character += comment.len() as u32;
    let full_range: lsp_types::Range = lsp_types::Range {
        start: position_before,
        end: state.position,
    };
    // because comment.len() does not include the line break
    let _: bool = parse_linebreak(state);
    state.comments.push(ElmSyntaxNode {
        range: full_range,
        value: ElmSyntaxComment {
            content: comment,
            kind: ElmSyntaxCommentKind::UntilLinebreak,
        },
    });
    true
}
/// does not parse documentation comment (starting with {-|)
fn parse_elm_comment_block(state: &mut ParseState) -> bool {
    if state.source[state.offset_utf8..].starts_with("{-|") {
        return false;
    }
    let start_position: lsp_types::Position = state.position;
    if !parse_symbol(state, "{-") {
        return false;
    }
    let content_start_offset_utf8: usize = state.offset_utf8;
    let mut nesting_level: u32 = 1;
    'until_fully_unnested: loop {
        if parse_linebreak(state) {
        } else if parse_symbol(state, "{-") {
            nesting_level += 1;
        } else if parse_symbol(state, "-}") {
            if nesting_level <= 1 {
                break 'until_fully_unnested;
            } else {
                nesting_level -= 1;
            }
        } else if parse_any_guaranteed_non_linebreak_char(state) {
        } else {
            // end of source
            break 'until_fully_unnested;
        }
    }
    state.comments.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: ElmSyntaxComment {
            content: state.source[content_start_offset_utf8..state.offset_utf8]
                .trim_end_matches("-}")
                .to_string(),
            kind: ElmSyntaxCommentKind::Block,
        },
    });
    true
}
fn parse_elm_documentation_comment_block_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<String>> {
    let start_position: lsp_types::Position = state.position;
    if !parse_symbol(state, "{-|") {
        return None;
    }
    let content_start_offset_utf8: usize = state.offset_utf8;
    let mut nesting_level: u32 = 1;
    'until_fully_unnested: loop {
        if parse_linebreak(state) {
        } else if parse_symbol(state, "{-") {
            nesting_level += 1;
        } else if parse_symbol(state, "-}") {
            if nesting_level <= 1 {
                break 'until_fully_unnested;
            } else {
                nesting_level -= 1;
            }
        } else if parse_any_guaranteed_non_linebreak_char(state) {
        } else {
            // end of source
            break 'until_fully_unnested;
        }
    }
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: state.source[content_start_offset_utf8..state.offset_utf8]
            .trim_end_matches("-}")
            .to_string(),
    })
}
fn parse_elm_lowercase_as_string(state: &mut ParseState) -> Option<String> {
    let mut chars_from_offset: std::str::Chars = state.source[state.offset_utf8..].chars();
    if let Some(first_char) = chars_from_offset.next()
        && first_char.is_lowercase()
    {
        let mut parsed_name: String = first_char.to_string();
        parsed_name.extend(chars_from_offset.take_while(|&c| c.is_alphanumeric() || c == '_'));
        state.offset_utf8 += parsed_name.len();
        state.position.character += parsed_name.encode_utf16().count() as u32;
        Some(parsed_name)
    } else {
        None
    }
}
fn parse_elm_lowercase_as_node(state: &mut ParseState) -> Option<ElmSyntaxNode<String>> {
    let start_position: lsp_types::Position = state.position;
    parse_elm_lowercase_as_string(state).map(|name| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: name,
    })
}
fn parse_elm_uppercase(state: &mut ParseState) -> Option<String> {
    let mut chars_from_offset = state.source[state.offset_utf8..].chars();
    if let Some(first_char) = chars_from_offset.next()
        && first_char.is_uppercase()
    {
        let mut parsed_name: String = first_char.to_string();
        parsed_name.extend(chars_from_offset.take_while(|&c| c.is_alphanumeric() || c == '_'));
        state.offset_utf8 += parsed_name.len();
        state.position.character += parsed_name.len() as u32;
        Some(parsed_name)
    } else {
        None
    }
}
fn parse_elm_uppercase_node(state: &mut ParseState) -> Option<ElmSyntaxNode<String>> {
    let start_position: lsp_types::Position = state.position;
    parse_elm_uppercase(state).map(|name| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: name,
    })
}
fn parse_elm_standalone_module_name_node(state: &mut ParseState) -> Option<ElmSyntaxNode<String>> {
    // very lenient, even allowing lowercase in most places it's usually forbidden
    // to allow for more convenient autocomplete without pressing shift
    let start_offset_utf8: usize = state.offset_utf8;
    let start_position: lsp_types::Position = state.position;
    if !parse_same_line_char_if(state, |c| c.is_alphabetic()) {
        return None;
    }
    parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_' || c == '.');
    let parsed_name_node: ElmSyntaxNode<String> = ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: state.source[start_offset_utf8..state.offset_utf8].to_string(),
    };
    Some(parsed_name_node)
}
fn parse_elm_operator_node(state: &mut ParseState) -> Option<ElmSyntaxNode<&'static str>> {
    // can be optimized by only slicing once for each symbol length
    let start_position = state.position;
    parse_symbol_as_str(state, "</>")
        .or_else(|| parse_symbol_as_str(state, "<?>"))
        .or_else(|| parse_symbol_as_str(state, "=="))
        .or_else(|| parse_symbol_as_str(state, "/="))
        .or_else(|| parse_symbol_as_str(state, "::"))
        .or_else(|| parse_symbol_as_str(state, "++"))
        .or_else(|| parse_symbol_as_str(state, "<|"))
        .or_else(|| parse_symbol_as_str(state, "|>"))
        .or_else(|| parse_symbol_as_str(state, "||"))
        .or_else(|| parse_symbol_as_str(state, "&&"))
        .or_else(|| parse_symbol_as_str(state, "<="))
        .or_else(|| parse_symbol_as_str(state, ">="))
        .or_else(|| parse_symbol_as_str(state, "|="))
        .or_else(|| parse_symbol_as_str(state, "|."))
        .or_else(|| parse_symbol_as_str(state, "//"))
        .or_else(|| parse_symbol_as_str(state, "<<"))
        .or_else(|| parse_symbol_as_str(state, ">>"))
        .or_else(|| parse_symbol_as_str(state, "<"))
        .or_else(|| parse_symbol_as_str(state, ">"))
        .or_else(|| parse_symbol_as_str(state, "+"))
        .or_else(|| parse_symbol_as_str(state, "-"))
        .or_else(|| parse_symbol_as_str(state, "*"))
        .or_else(|| parse_symbol_as_str(state, "/"))
        .or_else(|| parse_symbol_as_str(state, "^"))
        .map(|parsed_symbol| ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: parsed_symbol,
        })
}
fn parse_elm_operator_followed_by_closing_paren(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<&'static str>> {
    // can be optimized by only slicing once for each symbol length
    let start_position = state.position;
    parse_symbol_as(state, "</>)", "</>")
        .or_else(|| parse_symbol_as(state, "<?>)", "<?>"))
        .or_else(|| parse_symbol_as(state, "==)", "=="))
        .or_else(|| parse_symbol_as(state, "/=)", "/="))
        .or_else(|| parse_symbol_as(state, "::)", "::"))
        .or_else(|| parse_symbol_as(state, "++)", "++"))
        .or_else(|| parse_symbol_as(state, "<|)", "<|"))
        .or_else(|| parse_symbol_as(state, "|>)", "|>"))
        .or_else(|| parse_symbol_as(state, "||)", "||"))
        .or_else(|| parse_symbol_as(state, "&&)", "&&"))
        .or_else(|| parse_symbol_as(state, "<=)", "<="))
        .or_else(|| parse_symbol_as(state, ">=)", ">="))
        .or_else(|| parse_symbol_as(state, "|=)", "|="))
        .or_else(|| parse_symbol_as(state, "|.)", "|."))
        .or_else(|| parse_symbol_as(state, "//)", "//"))
        .or_else(|| parse_symbol_as(state, "<<)", "<<"))
        .or_else(|| parse_symbol_as(state, ">>)", ">>"))
        .or_else(|| parse_symbol_as(state, "<)", "<"))
        .or_else(|| parse_symbol_as(state, ">)", ">"))
        .or_else(|| parse_symbol_as(state, "+)", "+"))
        .or_else(|| parse_symbol_as(state, "-)", "-"))
        .or_else(|| parse_symbol_as(state, "*)", "*"))
        .or_else(|| parse_symbol_as(state, "/)", "/"))
        .or_else(|| parse_symbol_as(state, "^)", "^"))
        .map(|parsed_symbol| ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: parsed_symbol,
        })
}

fn parse_elm_syntax_expose_node(state: &mut ParseState) -> Option<ElmSyntaxNode<ElmSyntaxExpose>> {
    let start_position: lsp_types::Position = state.position;
    if parse_symbol(state, "(") {
        parse_elm_whitespace_and_comments(state);
        let maybe_operator_symbol: Option<ElmSyntaxNode<&str>> = parse_elm_operator_node(state);
        parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: ElmSyntaxExpose::Operator(maybe_operator_symbol),
        })
    } else if let Some(variable_name) = parse_elm_lowercase_as_string(state) {
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: ElmSyntaxExpose::Variable(variable_name),
        })
    } else if let Some(type_name_node) = parse_elm_uppercase_node(state) {
        parse_elm_whitespace_and_comments(state);
        if parse_symbol(state, "(") {
            parse_elm_whitespace_and_comments(state);
            let maybe_exposing_variants_range: Option<lsp_types::Range> =
                parse_symbol_as_range(state, "..");
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            Some(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: start_position,
                    end: state.position,
                },
                value: ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                    name: type_name_node,
                    open_range: maybe_exposing_variants_range,
                },
            })
        } else {
            Some(ElmSyntaxNode {
                range: type_name_node.range,
                value: ElmSyntaxExpose::Type(type_name_node.value),
            })
        }
    } else {
        None
    }
}

fn parse_elm_syntax_import_node(state: &mut ParseState) -> Option<ElmSyntaxNode<ElmSyntaxImport>> {
    let import_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "import")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_module_name_node: Option<ElmSyntaxNode<String>> =
        parse_elm_standalone_module_name_node(state);
    parse_elm_whitespace_and_comments(state);
    let maybe_alias: Option<EmSyntaxImportAs> =
        parse_symbol_as_range(state, "as").and_then(|as_keyword_range| {
            parse_elm_whitespace_and_comments(state);
            let maybe_alias_name_node = parse_elm_uppercase_node(state);
            Some(EmSyntaxImportAs {
                as_keyword_range: as_keyword_range,
                name: maybe_alias_name_node,
            })
        });
    parse_elm_whitespace_and_comments(state);
    let maybe_exposing: Option<ElmSyntaxExposing> = parse_elm_syntax_exposing(state);
    let end_position: lsp_types::Position = maybe_exposing
        .as_ref()
        .map(|exposing| {
            exposing
                .specific
                .as_ref()
                .map(|node| node.range.end)
                .unwrap_or_else(|| exposing.exposing_keyword_range.end)
        })
        .or_else(|| {
            maybe_alias.as_ref().map(|alias| {
                alias
                    .name
                    .as_ref()
                    .map(|node| node.range.end)
                    .unwrap_or_else(|| alias.as_keyword_range.end)
            })
        })
        .or_else(|| maybe_module_name_node.as_ref().map(|node| node.range.end))
        .unwrap_or_else(|| import_keyword_range.end);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: import_keyword_range.start,
            end: end_position,
        },
        value: ElmSyntaxImport {
            module_name: maybe_module_name_node,
            alias: maybe_alias,
            exposing: maybe_exposing,
        },
    })
}
fn parse_elm_syntax_exposing(state: &mut ParseState) -> Option<ElmSyntaxExposing> {
    let exposing_keyword_range = parse_symbol_as_range(state, "exposing")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_specific = parse_elm_syntax_exposing_specific_node(state);
    Some(ElmSyntaxExposing {
        exposing_keyword_range: exposing_keyword_range,
        specific: maybe_specific,
    })
}
fn parse_elm_syntax_exposing_specific_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExposingSpecific>> {
    let start_position: lsp_types::Position = state.position;
    if !parse_symbol(state, "(") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let exposing_specific: ElmSyntaxExposingSpecific = match parse_symbol_as_range(state, "..") {
        Some(all_range) => {
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            ElmSyntaxExposingSpecific::All(all_range)
        }
        None => {
            let mut expose_nodes: Vec<ElmSyntaxNode<ElmSyntaxExpose>> = Vec::new();
            'parsing_exposes: while !parse_symbol(state, ")") {
                if state.position.character <= 0 {
                    break 'parsing_exposes;
                }
                match parse_elm_syntax_expose_node(state) {
                    Some(expose_node) => {
                        expose_nodes.push(expose_node);
                        parse_elm_whitespace_and_comments(state);
                        let _ = parse_symbol(state, ",");
                        parse_elm_whitespace_and_comments(state);
                    }
                    None => {
                        if parse_any_guaranteed_non_linebreak_char(state) {
                            parse_elm_whitespace_and_comments(state);
                        } else {
                            break 'parsing_exposes;
                        }
                    }
                }
            }
            ElmSyntaxExposingSpecific::Explicit(expose_nodes)
        }
    };
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: exposing_specific,
    })
}

fn parse_elm_syntax_module_header(state: &mut ParseState) -> Option<ElmSyntaxModuleHeader> {
    if let Some(module_keyword_range) = parse_symbol_as_range(state, "module") {
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<String>> =
            parse_elm_standalone_module_name_node(state);
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxExposing> = parse_elm_syntax_exposing(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Pure {
                module_keyword_range: module_keyword_range,
            },
            module_name: maybe_module_name_node,
            exposing: maybe_exposing,
        })
    } else if let Some(port_keyword_range) = parse_symbol_as_range(state, "port") {
        parse_elm_whitespace_and_comments(state);
        let module_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "module")?;
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<String>> =
            parse_elm_standalone_module_name_node(state);
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxExposing> = parse_elm_syntax_exposing(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Port {
                port_keyword_range: port_keyword_range,
                module_keyword_range: module_keyword_range,
            },
            module_name: maybe_module_name_node,
            exposing: maybe_exposing,
        })
    } else if let Some(effect_keyword_range) = parse_symbol_as_range(state, "effect") {
        parse_elm_whitespace_and_comments(state);
        let module_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "module")?;
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<String>> =
            parse_elm_standalone_module_name_node(state);
        parse_elm_whitespace_and_comments(state);
        let where_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "where")?;
        parse_elm_whitespace_and_comments(state);

        let maybe_command_entry: Option<EffectModuleHeaderEntry>;
        let maybe_subscription_entry: Option<EffectModuleHeaderEntry>;
        if parse_symbol(state, "{") {
            parse_elm_whitespace_and_comments(state);
            maybe_command_entry =
                parse_elm_syntax_effect_module_header_where_entry(state, "command");
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, ",");
            parse_elm_whitespace_and_comments(state);
            maybe_subscription_entry =
                parse_elm_syntax_effect_module_header_where_entry(state, "subscription");
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, "}");
        } else {
            maybe_command_entry = None;
            maybe_subscription_entry = None;
        }

        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxExposing> = parse_elm_syntax_exposing(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Effect {
                effect_keyword_range: effect_keyword_range,
                module_keyword_range,
                where_keyword_range: where_keyword_range,
                command: maybe_command_entry,
                subscription: maybe_subscription_entry,
            },
            module_name: maybe_module_name_node,
            exposing: maybe_exposing,
        })
    } else {
        None
    }
}

fn parse_elm_syntax_effect_module_header_where_entry(
    state: &mut ParseState,
    key: &'static str,
) -> Option<EffectModuleHeaderEntry> {
    let key_range: lsp_types::Range = parse_symbol_as_range(state, key)?;
    parse_elm_whitespace_and_comments(state);
    let equals_range: lsp_types::Range = parse_symbol_as_range(state, "=")?;
    parse_elm_whitespace_and_comments(state);
    let type_name_node: ElmSyntaxNode<String> = parse_elm_uppercase_node(state)?;
    Some(EffectModuleHeaderEntry {
        key_range: key_range,
        equals_range: equals_range,
        value_type_name: type_name_node,
    })
}
fn parse_elm_syntax_type_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxType>> {
    let start_type_node: ElmSyntaxNode<ElmSyntaxType> =
        parse_elm_syntax_type_not_function_node(state)?;
    parse_elm_whitespace_and_comments(state);
    if let Some(arrow_key_symbol_range) = parse_symbol_as_range(state, "->") {
        parse_elm_whitespace_and_comments(state);
        let maybe_output_type: Option<ElmSyntaxNode<ElmSyntaxType>> =
            if state.position.character > state.indent as u32 {
                parse_elm_syntax_type_space_separated_node(state)
            } else {
                None
            };
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_type_node.range.start,
                end: match maybe_output_type {
                    None => arrow_key_symbol_range.end,
                    Some(ref output_type_node) => output_type_node.range.end,
                },
            },
            value: ElmSyntaxType::Function {
                input: elm_syntax_node_box(start_type_node),
                arrow_key_symbol_range: arrow_key_symbol_range,
                output: maybe_output_type.map(elm_syntax_node_box),
            },
        })
    } else {
        Some(start_type_node)
    }
}
fn parse_elm_syntax_type_not_function_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxType>> {
    parse_elm_syntax_type_construct_node(state).or_else(|| {
        let start_position: lsp_types::Position = state.position;
        parse_symbol_as(state, "()", ElmSyntaxType::Unit)
            .or_else(|| parse_elm_lowercase_as_string(state).map(ElmSyntaxType::Variable))
            .or_else(|| parse_elm_syntax_type_parenthesized_or_tuple_or_triple(state))
            .or_else(|| parse_elm_syntax_type_record_or_record_extension(state))
            .map(|type_| ElmSyntaxNode {
                range: lsp_types::Range {
                    start: start_position,
                    end: state.position,
                },
                value: type_,
            })
    })
}
fn parse_elm_syntax_type_not_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxType>> {
    let start_position = state.position;
    parse_elm_syntax_type_not_space_separated(state).map(|type_| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: type_,
    })
}
fn parse_elm_syntax_type_not_space_separated(state: &mut ParseState) -> Option<ElmSyntaxType> {
    parse_symbol_as(state, "()", ElmSyntaxType::Unit)
        .or_else(|| parse_elm_lowercase_as_string(state).map(ElmSyntaxType::Variable))
        .or_else(|| parse_elm_syntax_type_parenthesized_or_tuple_or_triple(state))
        .or_else(|| {
            parse_elm_qualified_uppercase_reference_node(state).map(|reference_node| {
                ElmSyntaxType::Construct {
                    reference: reference_node,
                    arguments: vec![],
                }
            })
        })
        .or_else(|| parse_elm_syntax_type_record_or_record_extension(state))
}
fn parse_elm_syntax_type_record_or_record_extension(
    state: &mut ParseState,
) -> Option<ElmSyntaxType> {
    if state.source[state.offset_utf8..].starts_with("{-|") {
        return None;
    }
    if !parse_symbol(state, "{") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let maybe_start_name: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
    parse_elm_whitespace_and_comments(state);
    if let Some(bar_key_symbol_range) = parse_symbol_as_range(state, "|") {
        parse_elm_whitespace_and_comments(state);
        let mut fields: Vec<ElmSyntaxTypeField> = Vec::new();
        if let Some(field0) = parse_elm_syntax_type_field(state) {
            fields.push(field0);
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, ",");
            parse_elm_whitespace_and_comments(state);
            'parsing_field1_up: while !parse_symbol(state, "}") {
                if state.position.character <= 0 {
                    break 'parsing_field1_up;
                }
                parse_elm_whitespace_and_comments(state);
                match parse_elm_syntax_type_field(state) {
                    Some(field) => {
                        fields.push(field);
                        parse_elm_whitespace_and_comments(state);
                        let _: bool = parse_symbol(state, ",");
                        parse_elm_whitespace_and_comments(state);
                    }
                    None => {
                        if parse_any_guaranteed_non_linebreak_char(state) {
                            parse_elm_whitespace_and_comments(state);
                        } else {
                            break 'parsing_field1_up;
                        }
                    }
                }
            }
        }
        Some(ElmSyntaxType::RecordExtension {
            record_variable: maybe_start_name,
            bar_key_symbol_range: bar_key_symbol_range,
            fields: fields,
        })
    } else if let Some(field0_name_node) = maybe_start_name {
        let maybe_field0_colon_key_symbol_range = parse_symbol_as_range(state, ":");
        parse_elm_whitespace_and_comments(state);
        let maybe_field0_value: Option<ElmSyntaxNode<ElmSyntaxType>> =
            parse_elm_syntax_type_space_separated_node(state);
        let mut fields: Vec<ElmSyntaxTypeField> = vec![ElmSyntaxTypeField {
            name: field0_name_node,
            colon_key_symbol_range: maybe_field0_colon_key_symbol_range,
            value: maybe_field0_value,
        }];
        parse_elm_whitespace_and_comments(state);
        let _: bool = parse_symbol(state, ",");
        parse_elm_whitespace_and_comments(state);
        'parsing_field1_up: while !parse_symbol(state, "}") {
            if state.position.character <= 0 {
                break 'parsing_field1_up;
            }
            parse_elm_whitespace_and_comments(state);
            match parse_elm_syntax_type_field(state) {
                Some(field) => {
                    fields.push(field);
                    parse_elm_whitespace_and_comments(state);
                    let _: bool = parse_symbol(state, ",");
                    parse_elm_whitespace_and_comments(state);
                }
                None => {
                    if parse_any_guaranteed_non_linebreak_char(state) {
                        parse_elm_whitespace_and_comments(state);
                    } else {
                        break 'parsing_field1_up;
                    }
                }
            }
        }
        Some(ElmSyntaxType::Record(fields))
    } else {
        parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, "}");
        Some(ElmSyntaxType::Record(vec![]))
    }
}
fn parse_elm_syntax_type_field(state: &mut ParseState) -> Option<ElmSyntaxTypeField> {
    let maybe_name: ElmSyntaxNode<String> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    let maybe_colon_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, ":");
    parse_elm_whitespace_and_comments(state);
    let maybe_value: Option<ElmSyntaxNode<ElmSyntaxType>> =
        parse_elm_syntax_type_space_separated_node(state);
    Some(ElmSyntaxTypeField {
        name: maybe_name,
        colon_key_symbol_range: maybe_colon_key_symbol_range,
        value: maybe_value,
    })
}
fn parse_elm_syntax_type_construct_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxType>> {
    let reference_node: ElmSyntaxNode<ElmQualifiedName> =
        parse_elm_qualified_uppercase_reference_node(state)?;
    let mut arguments: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
    let mut construct_end_position: lsp_types::Position = reference_node.range.end;
    'parsing_arguments: loop {
        parse_elm_whitespace_and_comments(state);
        if state.position.character <= state.indent as u32 {
            break 'parsing_arguments;
        }
        match parse_elm_syntax_type_not_space_separated_node(state) {
            None => {
                break 'parsing_arguments;
            }
            Some(argument_node) => {
                construct_end_position = argument_node.range.end;
                arguments.push(argument_node);
            }
        }
    }
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: reference_node.range.start,
            end: construct_end_position,
        },
        value: ElmSyntaxType::Construct {
            reference: reference_node,
            arguments: arguments,
        },
    })
}
fn parse_elm_qualified_uppercase_reference_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmQualifiedName>> {
    let start_position: lsp_types::Position = state.position;
    let start_offset_utf8: usize = state.offset_utf8;
    if !parse_same_line_char_if(state, char::is_uppercase) {
        return None;
    }
    parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_');
    if parse_symbol(state, ".") {
        loop {
            let after_last_dot_offset_utf8: usize = state.offset_utf8;
            if parse_same_line_char_if(state, char::is_uppercase) {
                parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_');
                if !parse_symbol(state, ".") {
                    return Some(ElmSyntaxNode {
                        range: lsp_types::Range {
                            start: start_position,
                            end: state.position,
                        },
                        value: ElmQualifiedName {
                            qualification: state.source
                                [start_offset_utf8..(after_last_dot_offset_utf8 - 1)]
                                .to_string(),
                            name: state.source[after_last_dot_offset_utf8..state.offset_utf8]
                                .to_string(),
                        },
                    });
                }
            } else {
                // stopping at . and in effect having an empty name is explicitly allowed!
                return Some(ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: start_position,
                        end: state.position,
                    },
                    value: ElmQualifiedName {
                        qualification: state.source[start_offset_utf8..(state.offset_utf8 - 1)]
                            .to_string(),
                        name: "".to_string(),
                    },
                });
            }
        }
    } else {
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: ElmQualifiedName {
                qualification: "".to_string(),
                name: state.source[start_offset_utf8..state.offset_utf8].to_string(),
            },
        })
    }
}
fn parse_elm_syntax_type_parenthesized_or_tuple_or_triple(
    state: &mut ParseState,
) -> Option<ElmSyntaxType> {
    if !parse_symbol(state, "(") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let maybe_in_parens_0: Option<ElmSyntaxNode<ElmSyntaxType>> =
        parse_elm_syntax_type_space_separated_node(state);
    parse_elm_whitespace_and_comments(state);
    if !parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
        Some(match maybe_in_parens_0 {
            None => ElmSyntaxType::Unit,
            Some(in_parens) => ElmSyntaxType::Parenthesized(elm_syntax_node_box(in_parens)),
        })
    } else {
        parse_elm_whitespace_and_comments(state);
        let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxType>> =
            parse_elm_syntax_type_space_separated_node(state);
        parse_elm_whitespace_and_comments(state);
        if !parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            Some(ElmSyntaxType::Tuple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
            })
        } else {
            parse_elm_whitespace_and_comments(state);
            let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxType>> =
                parse_elm_syntax_type_space_separated_node(state);
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            Some(ElmSyntaxType::Triple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
                part2: maybe_part2.map(elm_syntax_node_box),
            })
        }
    }
}
fn parse_elm_syntax_pattern_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    let start_pattern: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_not_as_or_cons_node(state)?;
    parse_elm_whitespace_and_comments(state);
    let mut consed_left_to_right: Vec<(lsp_types::Range, Option<ElmSyntaxNode<ElmSyntaxPattern>>)> =
        vec![];
    while let Some(cons_key_symbol_range) = parse_symbol_as_range(state, "::") {
        parse_elm_whitespace_and_comments(state);
        let maybe_tail_pattern: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
            parse_elm_syntax_pattern_not_as_or_cons_node(state);
        consed_left_to_right.push((cons_key_symbol_range, maybe_tail_pattern));
        parse_elm_whitespace_and_comments(state);
    }
    parse_elm_whitespace_and_comments(state);
    let maybe_combined_tail_cons: Option<(
        lsp_types::Range,
        Option<ElmSyntaxNode<ElmSyntaxPattern>>,
    )> = consed_left_to_right.into_iter().rev().reduce(
        |(cons_tail_key_symbol_range, maybe_tail), (cons_head_key_symbol_range, maybe_head)| {
            (
                cons_head_key_symbol_range,
                Some(ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: match maybe_head {
                            Some(ref head_node) => head_node.range.start,
                            None => cons_tail_key_symbol_range.start,
                        },
                        end: match maybe_tail {
                            Some(ref tail_node) => tail_node.range.end,
                            None => cons_tail_key_symbol_range.end,
                        },
                    },
                    value: ElmSyntaxPattern::ListCons {
                        head: maybe_head.map(elm_syntax_node_box),
                        cons_key_symbol: cons_tail_key_symbol_range,
                        tail: maybe_tail.map(elm_syntax_node_box),
                    },
                }),
            )
        },
    );
    let pattern_with_conses: ElmSyntaxNode<ElmSyntaxPattern> = match maybe_combined_tail_cons {
        None => start_pattern,
        Some((cons_key_symbol_range, maybe_tail)) => ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_pattern.range.start,
                end: match maybe_tail {
                    Some(ref tail_node) => tail_node.range.end,
                    None => cons_key_symbol_range.end,
                },
            },
            value: ElmSyntaxPattern::ListCons {
                head: Some(elm_syntax_node_box(start_pattern)),
                cons_key_symbol: cons_key_symbol_range,
                tail: maybe_tail.map(elm_syntax_node_box),
            },
        },
    };
    match parse_symbol_as_range(state, "as") {
        None => Some(pattern_with_conses),
        Some(as_keyword_range) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_variable: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
            Some(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: pattern_with_conses.range.start,
                    end: match maybe_variable {
                        Some(ref variable_node) => variable_node.range.end,
                        None => as_keyword_range.end,
                    },
                },
                value: ElmSyntaxPattern::As {
                    pattern: elm_syntax_node_box(pattern_with_conses),
                    as_keyword_range: as_keyword_range,
                    variable: maybe_variable,
                },
            })
        }
    }
}

fn parse_elm_syntax_pattern_not_as_or_cons_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    parse_elm_syntax_pattern_construct_node(state).or_else(|| {
        let start_position = state.position;
        parse_symbol_as(state, "()", ElmSyntaxPattern::Unit)
            .or_else(|| parse_symbol_as(state, "_", ElmSyntaxPattern::Ignored))
            .or_else(|| parse_elm_lowercase_as_string(state).map(ElmSyntaxPattern::Variable))
            .or_else(|| parse_elm_char(state).map(ElmSyntaxPattern::Char))
            .or_else(|| parse_elm_syntax_pattern_string(state))
            .or_else(|| parse_elm_syntax_pattern_parenthesized_or_tuple_or_triple(state))
            .or_else(|| parse_elm_syntax_pattern_list_exact(state))
            .or_else(|| parse_elm_syntax_pattern_record(state))
            .or_else(|| parse_elm_syntax_pattern_integer(state))
            .map(|pattern| ElmSyntaxNode {
                range: lsp_types::Range {
                    start: start_position,
                    end: state.position,
                },
                value: pattern,
            })
    })
}
fn parse_elm_syntax_pattern_not_space_separated(
    state: &mut ParseState,
) -> Option<ElmSyntaxPattern> {
    parse_symbol_as(state, "()", ElmSyntaxPattern::Unit)
        .or_else(|| parse_symbol_as(state, "_", ElmSyntaxPattern::Ignored))
        .or_else(|| parse_elm_syntax_pattern_parenthesized_or_tuple_or_triple(state))
        .or_else(|| parse_elm_lowercase_as_string(state).map(ElmSyntaxPattern::Variable))
        .or_else(|| {
            parse_elm_qualified_uppercase_reference_node(state).map(|reference_node| {
                ElmSyntaxPattern::Variant {
                    reference: reference_node,
                    values: vec![],
                }
            })
        })
        .or_else(|| parse_elm_char(state).map(ElmSyntaxPattern::Char))
        .or_else(|| parse_elm_syntax_pattern_string(state))
        .or_else(|| parse_elm_syntax_pattern_list_exact(state))
        .or_else(|| parse_elm_syntax_pattern_record(state))
        .or_else(|| parse_elm_syntax_pattern_integer(state))
}
fn parse_elm_syntax_pattern_list_exact(state: &mut ParseState) -> Option<ElmSyntaxPattern> {
    if !parse_symbol(state, "[") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let mut elements: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    'parsing_elements: while !parse_symbol(state, "]") {
        if state.position.character <= 0 {
            break 'parsing_elements;
        }
        match parse_elm_syntax_pattern_space_separated_node(state) {
            Some(pattern_node) => {
                elements.push(pattern_node);
                parse_elm_whitespace_and_comments(state);
                let _ = parse_symbol(state, ",");
                parse_elm_whitespace_and_comments(state);
            }
            None => {
                if parse_any_guaranteed_non_linebreak_char(state) {
                    parse_elm_whitespace_and_comments(state);
                } else {
                    break 'parsing_elements;
                }
            }
        }
    }
    Some(ElmSyntaxPattern::ListExact(elements))
}
fn parse_elm_syntax_pattern_record(state: &mut ParseState) -> Option<ElmSyntaxPattern> {
    if state.source[state.offset_utf8..].starts_with("{-|") {
        return None;
    }
    if !parse_symbol(state, "{") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let mut field_names: Vec<ElmSyntaxNode<String>> = Vec::new();
    'parsing_fields: while !parse_symbol(state, "}") {
        if state.position.character <= 0 {
            break 'parsing_fields;
        }
        match parse_elm_lowercase_as_node(state) {
            Some(field_name_node) => {
                field_names.push(field_name_node);
                parse_elm_whitespace_and_comments(state);
                let _: bool = parse_symbol(state, ",");
                parse_elm_whitespace_and_comments(state);
            }
            None => {
                if parse_any_guaranteed_non_linebreak_char(state) {
                    parse_elm_whitespace_and_comments(state);
                } else {
                    break 'parsing_fields;
                }
            }
        }
    }
    Some(ElmSyntaxPattern::Record(field_names))
}
fn parse_elm_syntax_pattern_not_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    let start_position = state.position;
    parse_elm_syntax_pattern_not_space_separated(state).map(|pattern| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: pattern,
    })
}

fn parse_elm_syntax_pattern_construct_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    let reference_node: ElmSyntaxNode<ElmQualifiedName> =
        parse_elm_qualified_uppercase_reference_node(state)?;
    let mut values: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    let mut variant_end_position: lsp_types::Position = reference_node.range.end;
    'parsing_arguments: loop {
        parse_elm_whitespace_and_comments(state);
        match parse_elm_syntax_pattern_not_space_separated_node(state) {
            None => {
                break 'parsing_arguments;
            }
            Some(value_node) => {
                variant_end_position = value_node.range.end;
                values.push(value_node);
            }
        }
    }
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: reference_node.range.start,
            end: variant_end_position,
        },
        value: ElmSyntaxPattern::Variant {
            reference: reference_node,
            values: values,
        },
    })
}
fn parse_elm_syntax_pattern_string(state: &mut ParseState) -> Option<ElmSyntaxPattern> {
    parse_elm_string_triple_quoted(state)
        .map(|content| ElmSyntaxPattern::String {
            content: content,
            quoting_style: ElmSyntaxStringQuotingStyle::TripleQuoted,
        })
        .or_else(|| {
            parse_elm_string_single_quoted(state).map(|content| ElmSyntaxPattern::String {
                content: content,
                quoting_style: ElmSyntaxStringQuotingStyle::SingleQuoted,
            })
        })
}

fn parse_elm_syntax_pattern_integer(state: &mut ParseState) -> Option<ElmSyntaxPattern> {
    parse_elm_unsigned_integer_base10_as_i64(state)
        .map(|value| ElmSyntaxPattern::Int {
            base: ElmSyntaxIntBase::IntBase10,
            value: value,
        })
        .or_else(|| {
            parse_elm_unsigned_integer_base16_as_i64(state).map(|value| ElmSyntaxPattern::Int {
                base: ElmSyntaxIntBase::IntBase16,
                value: value,
            })
        })
}
fn parse_elm_syntax_pattern_parenthesized_or_tuple_or_triple(
    state: &mut ParseState,
) -> Option<ElmSyntaxPattern> {
    if !parse_symbol(state, "(") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let maybe_in_parens_0: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
        parse_elm_syntax_pattern_space_separated_node(state);
    parse_elm_whitespace_and_comments(state);
    if !parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
        Some(match maybe_in_parens_0 {
            None => ElmSyntaxPattern::Unit,
            Some(in_parens) => ElmSyntaxPattern::Parenthesized(elm_syntax_node_box(in_parens)),
        })
    } else {
        parse_elm_whitespace_and_comments(state);
        let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
            parse_elm_syntax_pattern_space_separated_node(state);
        parse_elm_whitespace_and_comments(state);
        if !parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            Some(ElmSyntaxPattern::Tuple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
            })
        } else {
            parse_elm_whitespace_and_comments(state);
            let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
                parse_elm_syntax_pattern_space_separated_node(state);
            parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
            Some(ElmSyntaxPattern::Triple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
                part2: maybe_part2.map(elm_syntax_node_box),
            })
        }
    }
}
fn parse_elm_unsigned_integer_base16_as_i64(state: &mut ParseState) -> Option<Option<i64>> {
    if !parse_symbol(state, "0x") {
        return None;
    }
    let hex_str: &str = parse_same_line_while_as_str(state, |c| c.is_ascii_hexdigit());
    Some(i64::from_str_radix(hex_str, 16).ok())
}
fn parse_elm_unsigned_integer_base10_as_i64(state: &mut ParseState) -> Option<Option<i64>> {
    let start_offset_utf8: usize = state.offset_utf8;
    if parse_unsigned_integer_base10(state) {
        let decimal_str: &str = &state.source[start_offset_utf8..state.offset_utf8];
        Some(i64::from_str_radix(decimal_str, 10).ok())
    } else {
        None
    }
}
fn parse_elm_syntax_expression_number(state: &mut ParseState) -> Option<ElmSyntaxExpression> {
    if let Some(unsigned_int_base16) = parse_elm_unsigned_integer_base16_as_i64(state) {
        return Some(ElmSyntaxExpression::Integer {
            base: ElmSyntaxIntBase::IntBase16,
            value: unsigned_int_base16,
        });
    }
    let start_offset_utf8: usize = state.offset_utf8;
    if !parse_unsigned_integer_base10(state) {
        return None;
    }
    let has_decimal_point: bool = parse_symbol(state, ".");
    if has_decimal_point {
        parse_same_line_while(state, |c| c.is_ascii_digit());
    }
    let has_exponent_plus: Option<bool> =
        if parse_same_line_char_if(state, |c| c == 'e' || c == 'E') {
            if parse_symbol(state, "+") {
                let _: bool = parse_unsigned_integer_base10(state);
                Some(true)
            } else {
                let _: bool = parse_symbol(state, "-");
                let _: bool = parse_unsigned_integer_base10(state);
                Some(false)
            }
        } else {
            None
        };
    let full_chomped_str: &str = &state.source[start_offset_utf8..state.offset_utf8];
    Some(if has_decimal_point || has_exponent_plus.is_some() {
        ElmSyntaxExpression::Float(
            if has_exponent_plus.is_some_and(|exponent_is_plus| exponent_is_plus) {
                str::parse::<f64>(&full_chomped_str.replace("+", "")).ok()
            } else {
                str::parse::<f64>(full_chomped_str).ok()
            },
        )
    } else {
        ElmSyntaxExpression::Integer {
            base: ElmSyntaxIntBase::IntBase10,
            value: i64::from_str_radix(full_chomped_str, 10).ok(),
        }
    })
}
fn parse_elm_char(state: &mut ParseState) -> Option<Option<char>> {
    if !parse_symbol(state, "'") {
        return None;
    }
    let result: Option<char> = parse_elm_text_content_char(state);
    parse_until_including_symbol_or_excluding_end_of_line(state, "'");
    Some(result)
}
/// commits after a single quote, so check for triple quoted beforehand
fn parse_elm_string_single_quoted(state: &mut ParseState) -> Option<String> {
    if !parse_symbol(state, "\"") {
        return None;
    }
    let mut result: String = String::new();
    while !(parse_symbol(state, "\"")
        || str_starts_with_linebreak(&state.source[state.offset_utf8..]))
    {
        match parse_elm_text_content_char(state) {
            Some(next_content_char) => {
                result.push(next_content_char);
            }
            None => match parse_any_guaranteed_non_linebreak_char_as_char(state) {
                Some(next_content_char) => {
                    result.push(next_content_char);
                }
                None => return Some(result),
            },
        }
    }
    Some(result)
}
fn parse_elm_string_triple_quoted(state: &mut ParseState) -> Option<String> {
    if !parse_symbol(state, "\"\"\"") {
        return None;
    }
    let mut result: String = String::new();
    while !parse_symbol(state, "\"\"\"") {
        match parse_linebreak_as_str(state) {
            Some(linebreak) => result.push_str(linebreak),
            None => match parse_char_symbol_as_char(state, '\"')
                .or_else(|| parse_elm_text_content_char(state))
            {
                Some(next_content_char) => {
                    result.push(next_content_char);
                }
                None => match parse_any_guaranteed_non_linebreak_char_as_char(state) {
                    Some(next_content_char) => {
                        result.push(next_content_char);
                    }
                    None => return Some(result),
                },
            },
        }
    }
    Some(result)
}
fn parse_elm_text_content_char(state: &mut ParseState) -> Option<char> {
    parse_symbol_as(state, "\\\\", '\\')
        .or_else(|| parse_symbol_as(state, "\\'", '\''))
        .or_else(|| parse_symbol_as(state, "\\\n", '\n'))
        .or_else(|| parse_symbol_as(state, "\\\r", '\r'))
        .or_else(|| parse_symbol_as(state, "\\\t", '\t'))
        .or_else(|| parse_symbol_as(state, "\\\"", '"'))
        .or_else(|| {
            if parse_symbol(state, "\\u{") {
                let unicode_hex_start_offset_utf8: usize = state.offset_utf8;
                parse_same_line_while(state, |c| c.is_ascii_hexdigit());
                let unicode_hex_end_offset_utf8: usize = state.offset_utf8;
                parse_until_including_symbol_or_excluding_end_of_line(state, "}");
                let unicode_hex_str: &str =
                    &state.source[unicode_hex_start_offset_utf8..unicode_hex_end_offset_utf8];
                u32::from_str_radix(unicode_hex_str, 16)
                    .ok()
                    .and_then(|unicode| char::from_u32(unicode))
            } else {
                None
            }
        })
        .or_else(|| {
            if str_starts_with_linebreak(&state.source[state.offset_utf8..]) {
                None
            } else {
                match state.source[state.offset_utf8..].chars().next() {
                    None => None,
                    Some(plain_char) => {
                        state.offset_utf8 += plain_char.len_utf8();
                        state.position.character += plain_char.len_utf16() as u32;
                        Some(plain_char)
                    }
                }
            }
        })
}

fn parse_elm_syntax_expression_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    parse_elm_syntax_expression_if_then_else(state)
        .or_else(|| parse_elm_syntax_expression_case_of(state))
        .or_else(|| parse_elm_syntax_expression_let_in(state))
        .or_else(|| parse_elm_syntax_expression_lambda(state))
        .or_else(|| {
            let left_node: ElmSyntaxNode<ElmSyntaxExpression> =
                parse_elm_syntax_expression_call_or_not_space_separated_node(state)?;
            parse_elm_whitespace_and_comments(state);
            Some(match parse_elm_operator_node(state) {
                None => left_node,
                Some(operator_node) => {
                    parse_elm_whitespace_and_comments(state);
                    let maybe_right: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                        parse_elm_syntax_expression_space_separated_node(state);
                    ElmSyntaxNode {
                        range: lsp_types::Range {
                            start: left_node.range.start,
                            end: match maybe_right {
                                None => operator_node.range.end,
                                Some(ref right_node) => right_node.range.end,
                            },
                        },
                        value: ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
                            left: elm_syntax_node_box(left_node),
                            operator: operator_node,
                            right: maybe_right.map(elm_syntax_node_box),
                        },
                    }
                }
            })
        })
}
fn parse_elm_syntax_expression_call_or_not_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let called_node: ElmSyntaxNode<ElmSyntaxExpression> =
        parse_elm_syntax_expression_not_space_separated_node(state)?;
    parse_elm_whitespace_and_comments(state);
    Some(
        if (state.position.character > state.indent as u32)
            && let Some(argument0_node) =
                parse_elm_syntax_expression_not_space_separated_node(state)
        {
            let mut argument1_up: Vec<ElmSyntaxNode<ElmSyntaxExpression>> = Vec::new();
            let mut call_end_position: lsp_types::Position = argument0_node.range.end;
            'parsing_argument1_up: loop {
                parse_elm_whitespace_and_comments(state);
                if state.position.character <= state.indent as u32 {
                    break 'parsing_argument1_up;
                }
                match parse_elm_syntax_expression_not_space_separated_node(state) {
                    None => {
                        break 'parsing_argument1_up;
                    }
                    Some(argument_node) => {
                        call_end_position = argument_node.range.end;
                        argument1_up.push(argument_node);
                    }
                }
            }
            ElmSyntaxNode {
                range: lsp_types::Range {
                    start: called_node.range.start,
                    end: call_end_position,
                },
                value: ElmSyntaxExpression::Call {
                    called: elm_syntax_node_box(called_node),
                    argument0: elm_syntax_node_box(argument0_node),
                    argument1_up: argument1_up,
                },
            }
        } else {
            called_node
        },
    )
}
fn parse_elm_syntax_expression_not_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let start_position: lsp_types::Position = state.position;
    let start_expression: ElmSyntaxExpression =
        parse_symbol_as(state, "()", ElmSyntaxExpression::Unit)
            .or_else(|| parse_elm_syntax_expression_string(state))
            .or_else(|| parse_elm_syntax_expression_list(state))
            .or_else(|| {
                parse_elm_syntax_expression_operator_function_or_parenthesized_or_tuple_or_triple(
                    state,
                )
            })
            .or_else(|| parse_elm_syntax_expression_record_access_function(state))
            .or_else(|| parse_elm_syntax_expression_reference(state))
            .or_else(|| parse_elm_syntax_expression_record_or_record_update(state))
            .or_else(|| parse_elm_syntax_expression_number(state))
            .or_else(|| parse_elm_char(state).map(ElmSyntaxExpression::Char))
            .or_else(|| parse_elm_syntax_expression_negation(state))?;
    let mut result_node: ElmSyntaxNode<ElmSyntaxExpression> = ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: start_expression,
    };
    while parse_symbol(state, ".") {
        let maybe_field_name: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
        result_node = ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: ElmSyntaxExpression::RecordAccess {
                record: elm_syntax_node_box(result_node),
                field: maybe_field_name,
            },
        }
    }
    Some(result_node)
}
fn parse_elm_syntax_expression_record_access_function(
    state: &mut ParseState,
) -> Option<ElmSyntaxExpression> {
    if !parse_symbol(state, ".") {
        return None;
    }
    Some(ElmSyntaxExpression::RecordAccessFunction(
        parse_elm_lowercase_as_node(state),
    ))
}
fn parse_elm_syntax_expression_negation(state: &mut ParseState) -> Option<ElmSyntaxExpression> {
    if state.source[state.offset_utf8..]
        .chars()
        .skip(1)
        .next()
        .is_some_and(char::is_whitespace)
    {
        // exit if - is followed by whitespace, as that means it is a subtraction operation instead
        return None;
    }
    if !parse_symbol(state, "-") {
        return None;
    }
    Some(ElmSyntaxExpression::Negation(
        parse_elm_syntax_expression_not_space_separated_node(state).map(elm_syntax_node_box),
    ))
}
fn str_starts_with_keyword(source: &str, keyword: &'static str) -> bool {
    source.starts_with(keyword)
        && source
            .chars()
            .skip(keyword.len())
            .next()
            .is_some_and(|c| c != '_' && !c.is_alphanumeric())
}
fn parse_elm_syntax_expression_reference(state: &mut ParseState) -> Option<ElmSyntaxExpression> {
    // can be optimized by e.g. adding a non-state-mutating parse_elm_lowercase_as_string
    // that checks for keywords on successful chomp and returns None only then (and if no keyword, mutate the state)
    if str_starts_with_keyword(&state.source[state.offset_utf8..], "in")
        || str_starts_with_keyword(&state.source[state.offset_utf8..], "of")
        || str_starts_with_keyword(&state.source[state.offset_utf8..], "then")
        || str_starts_with_keyword(&state.source[state.offset_utf8..], "else")
    {
        return None;
    }
    parse_elm_lowercase_as_string(state)
        .map(|name| ElmSyntaxExpression::Reference {
            qualification: "".to_string(),
            name: name,
        })
        .or_else(|| {
            let start_offset_utf8: usize = state.offset_utf8;
            if !parse_same_line_char_if(state, char::is_uppercase) {
                return None;
            }
            parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_');
            if parse_symbol(state, ".") {
                loop {
                    let after_last_dot_offset_utf8: usize = state.offset_utf8;
                    if let Some(name) = parse_elm_lowercase_as_string(state) {
                        return Some(ElmSyntaxExpression::Reference {
                            qualification: state.source
                                [start_offset_utf8..(after_last_dot_offset_utf8 - 1)]
                                .to_string(),
                            name: name,
                        });
                    } else if parse_same_line_char_if(state, char::is_uppercase) {
                        parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_');
                        if !parse_symbol(state, ".") {
                            return Some(ElmSyntaxExpression::Reference {
                                qualification: state.source
                                    [start_offset_utf8..(after_last_dot_offset_utf8 - 1)]
                                    .to_string(),
                                name: state.source[after_last_dot_offset_utf8..state.offset_utf8]
                                    .to_string(),
                            });
                        }
                    } else {
                        // stopping at . and in effect having an empty name is explicitly allowed!
                        return Some(ElmSyntaxExpression::Reference {
                            qualification: state.source[start_offset_utf8..(state.offset_utf8 - 1)]
                                .to_string(),
                            name: "".to_string(),
                        });
                    }
                }
            } else {
                Some(ElmSyntaxExpression::Reference {
                    qualification: "".to_string(),
                    name: state.source[start_offset_utf8..state.offset_utf8].to_string(),
                })
            }
        })
}
fn parse_elm_syntax_expression_record_or_record_update(
    state: &mut ParseState,
) -> Option<ElmSyntaxExpression> {
    if state.source[state.offset_utf8..].starts_with("{-|") {
        return None;
    }
    if !parse_symbol(state, "{") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let maybe_start_name: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
    parse_elm_whitespace_and_comments(state);
    if let Some(bar_key_symbol_range) = parse_symbol_as_range(state, "|") {
        parse_elm_whitespace_and_comments(state);
        let mut fields: Vec<ElmSyntaxExpressionField> = Vec::new();
        if let Some(field0) = parse_elm_syntax_expression_field(state) {
            fields.push(field0);
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, ",");
            parse_elm_whitespace_and_comments(state);
            'parsing_field1_up: while !parse_symbol(state, "}") {
                if state.position.character <= 0 {
                    break 'parsing_field1_up;
                }
                parse_elm_whitespace_and_comments(state);
                match parse_elm_syntax_expression_field(state) {
                    Some(field) => {
                        fields.push(field);
                        parse_elm_whitespace_and_comments(state);
                        let _: bool = parse_symbol(state, ",");
                        parse_elm_whitespace_and_comments(state);
                    }
                    None => {
                        if parse_any_guaranteed_non_linebreak_char(state) {
                            parse_elm_whitespace_and_comments(state);
                        } else {
                            break 'parsing_field1_up;
                        }
                    }
                }
            }
        }
        Some(ElmSyntaxExpression::RecordUpdate {
            record_variable: maybe_start_name,
            bar_key_symbol_range: bar_key_symbol_range,
            fields: fields,
        })
    } else if let Some(field0_name_node) = maybe_start_name {
        let maybe_field0_equals_key_symbol_range: Option<lsp_types::Range> =
            parse_symbol_as_range(state, "=");
        parse_elm_whitespace_and_comments(state);
        let maybe_field0_value: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
            parse_elm_syntax_expression_space_separated_node(state);
        let mut fields: Vec<ElmSyntaxExpressionField> = vec![ElmSyntaxExpressionField {
            name: field0_name_node,
            equals_key_symbol_range: maybe_field0_equals_key_symbol_range,
            value: maybe_field0_value,
        }];
        parse_elm_whitespace_and_comments(state);
        let _: bool = parse_symbol(state, ",");
        parse_elm_whitespace_and_comments(state);
        'parsing_field1_up: while !parse_symbol(state, "}") {
            if state.position.character <= 0 {
                break 'parsing_field1_up;
            }
            parse_elm_whitespace_and_comments(state);
            match parse_elm_syntax_expression_field(state) {
                Some(field) => {
                    fields.push(field);
                    parse_elm_whitespace_and_comments(state);
                    let _: bool = parse_symbol(state, ",");
                    parse_elm_whitespace_and_comments(state);
                }
                None => {
                    if parse_any_guaranteed_non_linebreak_char(state) {
                        parse_elm_whitespace_and_comments(state);
                    } else {
                        break 'parsing_field1_up;
                    }
                }
            }
        }
        Some(ElmSyntaxExpression::Record(fields))
    } else {
        parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, "}");
        Some(ElmSyntaxExpression::Record(vec![]))
    }
}
fn parse_elm_syntax_expression_field(state: &mut ParseState) -> Option<ElmSyntaxExpressionField> {
    let name_node: ElmSyntaxNode<String> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    let maybe_equals_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    let maybe_value: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        parse_elm_syntax_expression_space_separated_node(state);
    Some(ElmSyntaxExpressionField {
        name: name_node,
        equals_key_symbol_range: maybe_equals_key_symbol_range,
        value: maybe_value,
    })
}
fn parse_elm_syntax_expression_if_then_else(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let if_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "if")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_condition: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        parse_elm_syntax_expression_space_separated_node(state);
    parse_elm_whitespace_and_comments(state);
    Some(
        if let Some(then_keyword_range) = parse_symbol_as_range(state, "then") {
            parse_elm_whitespace_and_comments(state);
            let maybe_on_true: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                parse_elm_syntax_expression_space_separated_node(state);
            parse_elm_whitespace_and_comments(state);
            if let Some(else_keyword_range) = parse_symbol_as_range(state, "else") {
                parse_elm_whitespace_and_comments(state);
                let maybe_on_false: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                    parse_elm_syntax_expression_space_separated_node(state);
                ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: if_keyword_range.start,
                        end: match maybe_on_false {
                            None => else_keyword_range.end,
                            Some(ref on_false_node) => on_false_node.range.end,
                        },
                    },
                    value: ElmSyntaxExpression::IfThenElse {
                        condition: maybe_condition.map(elm_syntax_node_box),
                        then_keyword_range: Some(then_keyword_range),
                        on_true: maybe_on_true.map(elm_syntax_node_box),
                        else_keyword_range: Some(else_keyword_range),
                        on_false: maybe_on_false.map(elm_syntax_node_box),
                    },
                }
            } else {
                ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: if_keyword_range.start,
                        end: match maybe_on_true {
                            None => then_keyword_range.end,
                            Some(ref on_true_node) => on_true_node.range.end,
                        },
                    },
                    value: ElmSyntaxExpression::IfThenElse {
                        condition: maybe_condition.map(elm_syntax_node_box),
                        then_keyword_range: Some(then_keyword_range),
                        on_true: maybe_on_true.map(elm_syntax_node_box),
                        else_keyword_range: None,
                        on_false: None,
                    },
                }
            }
        } else {
            ElmSyntaxNode {
                range: lsp_types::Range {
                    start: if_keyword_range.start,
                    end: match maybe_condition {
                        None => if_keyword_range.end,
                        Some(ref condition_node) => condition_node.range.end,
                    },
                },
                value: ElmSyntaxExpression::IfThenElse {
                    condition: maybe_condition.map(elm_syntax_node_box),
                    then_keyword_range: None,
                    on_true: None,
                    else_keyword_range: None,
                    on_false: None,
                },
            }
        },
    )
}
fn parse_elm_syntax_expression_lambda(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let backslash_key_symbol_range: lsp_types::Range = parse_symbol_as_range(state, "\\")?;
    let mut syntax_before_result_end_position: lsp_types::Position = backslash_key_symbol_range.end;
    parse_elm_whitespace_and_comments(state);
    let mut parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    while let Some(parameter_node) = parse_elm_syntax_pattern_not_space_separated_node(state) {
        syntax_before_result_end_position = parameter_node.range.end;
        parameters.push(parameter_node);
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_arrow_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "->");
    parse_elm_whitespace_and_comments(state);
    let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        if state.position.character > state.indent as u32 {
            parse_elm_syntax_expression_space_separated_node(state)
        } else {
            None
        };
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: backslash_key_symbol_range.start,
            end: match maybe_result {
                None => syntax_before_result_end_position,
                Some(ref result_node) => result_node.range.end,
            },
        },
        value: ElmSyntaxExpression::Lambda {
            parameters: parameters,
            arrow_key_symbol_range: maybe_arrow_key_symbol_range,
            result: maybe_result.map(elm_syntax_node_box),
        },
    })
}
fn parse_elm_syntax_expression_case_of(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let case_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "case")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_matched: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        parse_elm_syntax_expression_space_separated_node(state);
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, "of") {
        None => ElmSyntaxNode {
            range: lsp_types::Range {
                start: case_keyword_range.start,
                end: match maybe_matched {
                    None => case_keyword_range.end,
                    Some(ref matched_node) => matched_node.range.end,
                },
            },
            value: ElmSyntaxExpression::CaseOf {
                matched: maybe_matched.map(elm_syntax_node_box),
                of_keyword_range: None,
                cases: vec![],
            },
        },
        Some(of_keyword_range) => {
            parse_elm_whitespace_and_comments(state);
            if state.position.character <= state.indent as u32 {
                ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: case_keyword_range.start,
                        end: of_keyword_range.end,
                    },
                    value: ElmSyntaxExpression::CaseOf {
                        matched: maybe_matched.map(elm_syntax_node_box),
                        of_keyword_range: Some(of_keyword_range),
                        cases: vec![],
                    },
                }
            } else {
                parse_state_push_indent(state, state.position.character as u16);
                let mut full_end_position: lsp_types::Position = of_keyword_range.end;
                let mut cases: Vec<ElmSyntaxExpressionCase> = Vec::new();
                while let Some(case) = parse_elm_syntax_expression_case(state) {
                    full_end_position = case
                        .result
                        .as_ref()
                        .map(|result| result.range.end)
                        .or_else(|| case.arrow_key_symbol_range.as_ref().map(|range| range.end))
                        .unwrap_or_else(|| case.pattern.range.end);
                    cases.push(case);
                    parse_elm_whitespace_and_comments(state);
                }
                parse_state_pop_indent(state);
                ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: case_keyword_range.start,
                        end: full_end_position,
                    },
                    value: ElmSyntaxExpression::CaseOf {
                        matched: maybe_matched.map(elm_syntax_node_box),
                        of_keyword_range: Some(of_keyword_range),
                        cases,
                    },
                }
            }
        }
    })
}
fn parse_elm_syntax_expression_case(state: &mut ParseState) -> Option<ElmSyntaxExpressionCase> {
    if state.position.character < state.indent as u32 {
        return None;
    }
    let case_pattern_node: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_not_space_separated_node(state)?;
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, "->") {
        None => ElmSyntaxExpressionCase {
            pattern: case_pattern_node,
            arrow_key_symbol_range: None,
            result: None,
        },
        Some(arrow_key_symbol_range) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_result = parse_elm_syntax_expression_space_separated_node(state);
            ElmSyntaxExpressionCase {
                pattern: case_pattern_node,
                arrow_key_symbol_range: Some(arrow_key_symbol_range),
                result: maybe_result,
            }
        }
    })
}

fn parse_elm_syntax_expression_let_in(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExpression>> {
    let let_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "let")?;
    parse_elm_whitespace_and_comments(state);
    Some(if state.position.character <= state.indent as u32 {
        ElmSyntaxNode {
            range: let_keyword_range,
            value: ElmSyntaxExpression::LetIn {
                declarations: vec![],
                in_keyword_range: None,
                result: None,
            },
        }
    } else {
        parse_state_push_indent(state, state.position.character as u16);
        let mut syntax_before_in_key_symbol_end_position: lsp_types::Position =
            let_keyword_range.end;
        let mut declarations: Vec<ElmSyntaxNode<ElmSyntaxLetDeclaration>> = Vec::new();
        let maybe_in_keyword_range: Option<lsp_types::Range>;
        'parsing_declarations: loop {
            if let Some(in_keyword_range) = parse_elm_keyword_as_range(state, "in") {
                maybe_in_keyword_range = Some(in_keyword_range);
                break 'parsing_declarations;
            }
            match parse_elm_syntax_let_declaration(state) {
                None => {
                    if state.position.character <= state.indent as u32 {
                        maybe_in_keyword_range = None;
                        break 'parsing_declarations;
                    }
                    if parse_any_guaranteed_non_linebreak_char(state) {
                        parse_elm_whitespace_and_comments(state);
                    } else {
                        maybe_in_keyword_range = None;
                        break 'parsing_declarations;
                    }
                }
                Some(declaration_node) => {
                    syntax_before_in_key_symbol_end_position = declaration_node.range.end;
                    declarations.push(declaration_node);
                    parse_elm_whitespace_and_comments(state);
                }
            }
        }
        parse_state_pop_indent(state);

        parse_elm_whitespace_and_comments(state);
        let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
            parse_elm_syntax_expression_space_separated_node(state);
        ElmSyntaxNode {
            range: lsp_types::Range {
                start: let_keyword_range.start,
                end: match maybe_result {
                    None => maybe_in_keyword_range
                        .map(|range| range.end)
                        .unwrap_or(syntax_before_in_key_symbol_end_position),
                    Some(ref result_node) => result_node.range.end,
                },
            },
            value: ElmSyntaxExpression::LetIn {
                declarations: declarations,
                in_keyword_range: maybe_in_keyword_range,
                result: maybe_result.map(elm_syntax_node_box),
            },
        }
    })
}
fn parse_elm_syntax_let_declaration(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxLetDeclaration>> {
    if state.position.character < state.indent as u32 {
        return None;
    }
    parse_elm_syntax_let_variable_declaration(state)
        .or_else(|| parse_elm_syntax_let_destructuring(state))
}
fn parse_elm_syntax_let_destructuring(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxLetDeclaration>> {
    let pattern_node: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_space_separated_node(state)?;
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, "=") {
        None => ElmSyntaxNode {
            range: pattern_node.range,
            value: ElmSyntaxLetDeclaration::Destructuring {
                pattern: pattern_node,
                equals_key_symbol_range: None,
                expression: None,
            },
        },
        Some(equals_key_symbol_range) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_expression: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                parse_elm_syntax_expression_space_separated_node(state);
            ElmSyntaxNode {
                range: lsp_types::Range {
                    start: pattern_node.range.start,
                    end: match maybe_expression {
                        None => equals_key_symbol_range.end,
                        Some(ref expression_node) => expression_node.range.end,
                    },
                },
                value: ElmSyntaxLetDeclaration::Destructuring {
                    pattern: pattern_node,
                    equals_key_symbol_range: Some(equals_key_symbol_range),
                    expression: maybe_expression,
                },
            }
        }
    })
}
fn parse_elm_syntax_let_variable_declaration(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxLetDeclaration>> {
    let start_name_node: ElmSyntaxNode<String> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    let maybe_signature: Option<ElmSyntaxVariableDeclarationSignature> =
        parse_symbol_as_range(state, ":").map(|colon_key_symbol_range| {
            parse_elm_whitespace_and_comments(state);
            let maybe_type: Option<ElmSyntaxNode<ElmSyntaxType>> =
                parse_elm_syntax_type_space_separated_node(state);
            parse_elm_whitespace_and_comments(state);
            let maybe_implementation_name_range: Option<lsp_types::Range> =
                if state.position.character < state.indent as u32 {
                    None
                } else {
                    parse_symbol_as_range(state, &start_name_node.value)
                };
            parse_elm_whitespace_and_comments(state);
            ElmSyntaxVariableDeclarationSignature {
                colon_key_symbol_range: colon_key_symbol_range,
                type_: maybe_type,
                implementation_name_range: maybe_implementation_name_range,
            }
        });
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position =
        match maybe_signature {
            Some(ref signature) => signature
                .implementation_name_range
                .map(|range| range.end)
                .or_else(|| signature.type_.as_ref().map(|node| node.range.end))
                .unwrap_or_else(|| signature.colon_key_symbol_range.end),
            None => start_name_node.range.end,
        };
    let mut parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    while let Some(parameter_node) = parse_elm_syntax_pattern_not_space_separated_node(state) {
        syntax_before_equals_key_symbol_end_location = parameter_node.range.end;
        parameters.push(parameter_node);
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_equals_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        parse_elm_syntax_expression_space_separated_node(state);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_name_node.range.start,
            end: maybe_result
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or_else(|| syntax_before_equals_key_symbol_end_location),
        },
        value: ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters: parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        },
    })
}
fn parse_elm_syntax_expression_string(state: &mut ParseState) -> Option<ElmSyntaxExpression> {
    parse_elm_string_triple_quoted(state)
        .map(|content| ElmSyntaxExpression::String {
            content: content,
            quoting_style: ElmSyntaxStringQuotingStyle::TripleQuoted,
        })
        .or_else(|| {
            parse_elm_string_single_quoted(state).map(|content| ElmSyntaxExpression::String {
                content: content,
                quoting_style: ElmSyntaxStringQuotingStyle::SingleQuoted,
            })
        })
}
fn parse_elm_syntax_expression_list(state: &mut ParseState) -> Option<ElmSyntaxExpression> {
    if !parse_symbol(state, "[") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let mut elements: Vec<ElmSyntaxNode<ElmSyntaxExpression>> = Vec::new();
    'parsing_elements: while !parse_symbol(state, "]") {
        if state.position.character <= 0 {
            break 'parsing_elements;
        }
        match parse_elm_syntax_expression_space_separated_node(state) {
            Some(expression_node) => {
                elements.push(expression_node);
                parse_elm_whitespace_and_comments(state);
                let _ = parse_symbol(state, ",");
                parse_elm_whitespace_and_comments(state);
            }
            None => {
                if parse_any_guaranteed_non_linebreak_char(state) {
                    parse_elm_whitespace_and_comments(state);
                } else {
                    break 'parsing_elements;
                }
            }
        }
    }
    Some(ElmSyntaxExpression::List(elements))
}
fn parse_elm_syntax_expression_operator_function_or_parenthesized_or_tuple_or_triple(
    state: &mut ParseState,
) -> Option<ElmSyntaxExpression> {
    if !parse_symbol(state, "(") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    Some(
        if let Some(operator_node) = parse_elm_operator_followed_by_closing_paren(state) {
            // needs to be this cursed to differentiate (-) and (-negated)
            ElmSyntaxExpression::OperatorFunction(operator_node)
        } else {
            let maybe_in_parens_0: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                parse_elm_syntax_expression_space_separated_node(state);
            parse_elm_whitespace_and_comments(state);
            if !parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(state, ")");
                match maybe_in_parens_0 {
                    None => ElmSyntaxExpression::Unit,
                    Some(in_parens) => {
                        ElmSyntaxExpression::Parenthesized(elm_syntax_node_box(in_parens))
                    }
                }
            } else {
                parse_elm_whitespace_and_comments(state);
                let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                    parse_elm_syntax_expression_space_separated_node(state);
                parse_elm_whitespace_and_comments(state);
                if !parse_symbol(state, ",") {
                    parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(
                        state, ")",
                    );
                    ElmSyntaxExpression::Tuple {
                        part0: maybe_in_parens_0.map(elm_syntax_node_box),
                        part1: maybe_part1.map(elm_syntax_node_box),
                    }
                } else {
                    parse_elm_whitespace_and_comments(state);
                    let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                        parse_elm_syntax_expression_space_separated_node(state);
                    parse_elm_whitespace_and_comments_or_any_until_including_symbol_or_before_0_indented_or_end_of_source(
                        state, ")",
                    );
                    ElmSyntaxExpression::Triple {
                        part0: maybe_in_parens_0.map(elm_syntax_node_box),
                        part1: maybe_part1.map(elm_syntax_node_box),
                        part2: maybe_part2.map(elm_syntax_node_box),
                    }
                }
            }
        },
    )
}
fn parse_elm_syntax_declaration_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    parse_elm_syntax_declaration_choice_type_or_type_alias_node(state)
        .or_else(|| parse_elm_syntax_declaration_port_node(state))
        .or_else(|| parse_elm_syntax_declaration_operator_node(state))
        .or_else(|| parse_elm_syntax_declaration_variable_node(state))
}
fn parse_elm_syntax_declaration_port_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let port_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "port")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_name: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
    parse_elm_whitespace_and_comments(state);
    let maybe_colon_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, ":");
    parse_elm_whitespace_and_comments(state);
    let maybe_type: Option<ElmSyntaxNode<ElmSyntaxType>> =
        parse_elm_syntax_type_space_separated_node(state);
    let full_end_position: lsp_types::Position = maybe_type
        .as_ref()
        .map(|type_node| type_node.range.end)
        .or_else(|| maybe_colon_key_symbol_range.map(|range| range.end))
        .or_else(|| maybe_name.as_ref().map(|node| node.range.end))
        .unwrap_or_else(|| port_keyword_range.end);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: port_keyword_range.start,
            end: full_end_position,
        },
        value: ElmSyntaxDeclaration::Port {
            name: maybe_name,
            colon_key_symbol_range: maybe_colon_key_symbol_range,
            type_: maybe_type,
        },
    })
}
fn parse_elm_syntax_declaration_operator_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let infix_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "infix")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_direction: Option<ElmSyntaxNode<ElmSyntaxInfixDirection>> =
        parse_elm_syntax_infix_declaration_node(state);
    parse_elm_whitespace_and_comments(state);
    let precedence_start_position: lsp_types::Position = state.position;
    let maybe_precedence: Option<ElmSyntaxNode<i64>> =
        match parse_elm_unsigned_integer_base10_as_i64(state).flatten() {
            None => None,
            Some(precedence) => {
                let precedence_range: lsp_types::Range = lsp_types::Range {
                    start: precedence_start_position,
                    end: state.position,
                };
                parse_elm_whitespace_and_comments(state);
                Some(ElmSyntaxNode {
                    range: precedence_range,
                    value: precedence,
                })
            }
        };
    let _: bool = parse_symbol(state, "(");
    parse_elm_whitespace_and_comments(state);
    let maybe_operator_symbol = parse_elm_operator_node(state);
    parse_elm_whitespace_and_comments(state);
    let _: bool = parse_symbol(state, ")");
    parse_elm_whitespace_and_comments(state);
    let maybe_equals_key_symbol_range = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    let maybe_function: Option<ElmSyntaxNode<String>> = parse_elm_lowercase_as_node(state);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: infix_keyword_range.start,
            end: state.position,
        },
        value: ElmSyntaxDeclaration::Operator {
            direction: maybe_direction,
            precedence: maybe_precedence,
            operator: maybe_operator_symbol,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            function: maybe_function,
        },
    })
}
fn parse_elm_syntax_infix_declaration_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxInfixDirection>> {
    let start_position: lsp_types::Position = state.position;
    let direction: ElmSyntaxInfixDirection =
        parse_symbol_as(state, "left", ElmSyntaxInfixDirection::Left)
            .or_else(|| parse_symbol_as(state, "right", ElmSyntaxInfixDirection::Right))
            .or_else(|| parse_symbol_as(state, "non", ElmSyntaxInfixDirection::Non))?;
    let end_position = state.position;
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: end_position,
        },
        value: direction,
    })
}
fn parse_elm_syntax_declaration_choice_type_or_type_alias_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let type_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "type")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_alias_keyword_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "alias");
    parse_elm_whitespace_and_comments(state);
    let maybe_name_node: Option<ElmSyntaxNode<String>> = parse_elm_uppercase_node(state);
    parse_elm_whitespace_and_comments(state);
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position = maybe_name_node
        .as_ref()
        .map(|name_node| name_node.range.end)
        .or_else(|| maybe_alias_keyword_range.map(|range| range.end))
        .unwrap_or_else(|| type_keyword_range.end);
    let mut parameters: Vec<ElmSyntaxNode<String>> = Vec::new();
    while let Some(parameter_node) = parse_elm_lowercase_as_node(state) {
        syntax_before_equals_key_symbol_end_location = parameter_node.range.end;
        parameters.push(parameter_node);
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_equals_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    Some(match maybe_alias_keyword_range {
        Some(alias_keyword_range) => {
            let maybe_type: Option<ElmSyntaxNode<ElmSyntaxType>> =
                if state.position.character <= state.indent as u32 {
                    None
                } else {
                    parse_elm_syntax_type_space_separated_node(state)
                };
            let full_end_location: lsp_types::Position = maybe_type
                .as_ref()
                .map(|type_node| type_node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or(syntax_before_equals_key_symbol_end_location);
            ElmSyntaxNode {
                range: lsp_types::Range {
                    start: type_keyword_range.start,
                    end: full_end_location,
                },
                value: ElmSyntaxDeclaration::TypeAlias {
                    alias_keyword_range: alias_keyword_range,
                    name: maybe_name_node,
                    parameters: parameters,
                    equals_key_symbol_range: maybe_equals_key_symbol_range,
                    type_: maybe_type,
                },
            }
        }
        None => {
            let maybe_variant0_name: Option<ElmSyntaxNode<String>> =
                parse_elm_uppercase_node(state);
            let mut variant0_values: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
            let mut full_end_position: lsp_types::Position = maybe_variant0_name
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or(syntax_before_equals_key_symbol_end_location);
            'parsing_arguments: loop {
                parse_elm_whitespace_and_comments(state);
                if state.position.character <= state.indent as u32 {
                    break 'parsing_arguments;
                }
                match parse_elm_syntax_type_not_space_separated_node(state) {
                    None => {
                        break 'parsing_arguments;
                    }
                    Some(argument_node) => {
                        full_end_position = argument_node.range.end;
                        variant0_values.push(argument_node);
                    }
                }
            }
            parse_elm_whitespace_and_comments(state);
            let mut variant1_up: Vec<ElmSyntaxChoiceTypeDeclarationTailingVariant> = Vec::new();
            while let Some(variant_node) =
                parse_elm_syntax_choice_type_declaration_trailing_variant_node(state)
            {
                variant1_up.push(variant_node.value);
                full_end_position = variant_node.range.end;
            }
            ElmSyntaxNode {
                range: lsp_types::Range {
                    start: type_keyword_range.start,
                    end: full_end_position,
                },
                value: ElmSyntaxDeclaration::ChoiceType {
                    name: maybe_name_node,
                    parameters: parameters,
                    equals_key_symbol_range: maybe_equals_key_symbol_range,
                    variant0_name: maybe_variant0_name,
                    variant0_values: variant0_values,
                    variant1_up: variant1_up,
                },
            }
        }
    })
}
fn parse_elm_syntax_choice_type_declaration_trailing_variant_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxChoiceTypeDeclarationTailingVariant>> {
    let or_key_symbol_range: lsp_types::Range = parse_symbol_as_range(state, "|")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_name: Option<ElmSyntaxNode<String>> = parse_elm_uppercase_node(state);
    let mut values: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
    let mut full_end_position: lsp_types::Position = maybe_name
        .as_ref()
        .map(|node| node.range.end)
        .unwrap_or_else(|| or_key_symbol_range.end);
    'parsing_arguments: loop {
        parse_elm_whitespace_and_comments(state);
        if state.position.character <= state.indent as u32 {
            break 'parsing_arguments;
        }
        match parse_elm_syntax_type_not_space_separated_node(state) {
            None => {
                break 'parsing_arguments;
            }
            Some(argument_node) => {
                full_end_position = argument_node.range.end;
                values.push(argument_node);
            }
        }
    }
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: or_key_symbol_range.start,
            end: full_end_position,
        },
        value: ElmSyntaxChoiceTypeDeclarationTailingVariant {
            or_key_symbol_range: or_key_symbol_range,
            name: maybe_name,
            values: values,
        },
    })
}
fn parse_elm_syntax_declaration_variable_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let start_name_node: ElmSyntaxNode<String> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    match parse_symbol_as_range(state, ":") {
        None => parse_elm_syntax_declaration_variable_node_after_maybe_signature_and_name(
            state,
            start_name_node,
            None,
        ),
        Some(colon_key_symbol_range) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_type: Option<ElmSyntaxNode<ElmSyntaxType>> =
                parse_elm_syntax_type_space_separated_node(state);
            parse_elm_whitespace_and_comments(state);
            match parse_symbol_as_range(state, &start_name_node.value) {
                None => {
                    return Some(ElmSyntaxNode {
                        range: lsp_types::Range {
                            start: start_name_node.range.start,
                            end: maybe_type
                                .as_ref()
                                .map(|node| node.range.end)
                                .unwrap_or_else(|| colon_key_symbol_range.end),
                        },
                        value: ElmSyntaxDeclaration::Variable {
                            start_name: start_name_node,
                            signature: Some(ElmSyntaxVariableDeclarationSignature {
                                colon_key_symbol_range: colon_key_symbol_range,
                                type_: maybe_type,
                                implementation_name_range: None,
                            }),
                            parameters: vec![],
                            equals_key_symbol_range: None,
                            result: None,
                        },
                    });
                }
                Some(implementation_name_range) => {
                    parse_elm_whitespace_and_comments(state);
                    parse_elm_syntax_declaration_variable_node_after_maybe_signature_and_name(
                        state,
                        start_name_node,
                        Some(ElmSyntaxVariableDeclarationSignature {
                            colon_key_symbol_range: colon_key_symbol_range,
                            type_: maybe_type,
                            implementation_name_range: Some(implementation_name_range),
                        }),
                    )
                }
            }
        }
    }
}
fn parse_elm_syntax_declaration_variable_node_after_maybe_signature_and_name(
    state: &mut ParseState,
    start_name_node: ElmSyntaxNode<String>,
    maybe_signature: Option<ElmSyntaxVariableDeclarationSignature>,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position =
        match maybe_signature {
            Some(ref signature) => signature
                .implementation_name_range
                .map(|range| range.end)
                .or_else(|| signature.type_.as_ref().map(|node| node.range.end))
                .unwrap_or_else(|| signature.colon_key_symbol_range.end),
            None => start_name_node.range.end,
        };
    let mut parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    while let Some(parameter_node) = parse_elm_syntax_pattern_not_space_separated_node(state) {
        syntax_before_equals_key_symbol_end_location = parameter_node.range.end;
        parameters.push(parameter_node);
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_equals_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        if state.position.character <= state.indent as u32 {
            None
        } else {
            parse_elm_syntax_expression_space_separated_node(state)
        };
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_name_node.range.start,
            end: maybe_result
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or_else(|| syntax_before_equals_key_symbol_end_location),
        },
        value: ElmSyntaxDeclaration::Variable {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters: parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        },
    })
}
fn parse_elm_syntax_documented_declaration(
    state: &mut ParseState,
) -> Option<ElmSyntaxDocumentedDeclaration> {
    match parse_elm_documentation_comment_block_node(state) {
        None => parse_elm_syntax_declaration_node(state).map(|declaration_node| {
            ElmSyntaxDocumentedDeclaration {
                documentation: None,
                declaration: Some(declaration_node),
            }
        }),
        Some(documentation_node) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_declaration: Option<ElmSyntaxNode<ElmSyntaxDeclaration>> =
                parse_elm_syntax_declaration_node(state);
            Some(ElmSyntaxDocumentedDeclaration {
                documentation: Some(documentation_node),
                declaration: maybe_declaration,
            })
        }
    }
}
fn parse_elm_syntax_module(module_source: &str) -> ElmSyntaxModule {
    let mut state: ParseState = ParseState {
        source: module_source,
        offset_utf8: 0,
        position: lsp_types::Position {
            line: 0,
            character: 0,
        },
        indent: 0,
        lower_indents_stack: vec![],
        comments: vec![],
    };
    parse_elm_whitespace_and_comments(&mut state);
    let maybe_header: Option<ElmSyntaxModuleHeader> = parse_elm_syntax_module_header(&mut state);
    parse_elm_whitespace_and_comments(&mut state);
    let maybe_module_documentation: Option<ElmSyntaxNode<String>> =
        parse_elm_documentation_comment_block_node(&mut state);
    parse_elm_whitespace_and_comments(&mut state);
    let mut imports: Vec<ElmSyntaxNode<ElmSyntaxImport>> = Vec::new();
    while let Some(import_node) = parse_elm_syntax_import_node(&mut state) {
        imports.push(import_node);
        parse_elm_whitespace_and_comments(&mut state);
    }
    let mut declarations: Vec<ElmSyntaxDocumentedDeclaration> = Vec::new();
    'parsing_declarations: loop {
        match parse_elm_syntax_documented_declaration(&mut state) {
            Some(documented_declaration) => {
                declarations.push(documented_declaration);
                parse_elm_whitespace_and_comments(&mut state);
            }
            None => {
                if parse_any_guaranteed_non_linebreak_char(&mut state) {
                    parse_elm_whitespace_and_comments(&mut state);
                } else {
                    break 'parsing_declarations;
                }
            }
        }
    }
    ElmSyntaxModule {
        header: maybe_header,
        documentation: maybe_module_documentation,
        comments: state.comments,
        imports: imports,
        declarations: declarations,
    }
}
