// lsp still reports this specific error even when it is allowed in the cargo.toml
#![allow(non_upper_case_globals)]

struct State {
    projects: std::collections::HashMap<
        /* path to directory containing elm.json */ std::path::PathBuf,
        ProjectState,
    >,
    open_text_document_uris: std::collections::HashSet<lsp_types::Url>,
    configured_elm_path: Option<String>,
    configured_elm_test_path: Option<String>,
    configured_elm_formatter: Option<ConfiguredElmFormatter>,
}
enum ConfiguredElmFormatter {
    Builtin,
    Custom { path: String },
}

struct ProjectState {
    source_directories: Vec<std::path::PathBuf>,
    modules: std::collections::HashMap<std::path::PathBuf, ModuleState>,
    dependency_exposed_module_names: std::collections::HashMap<String, ProjectModuleOrigin>,
    elm_make_errors: Vec<ElmMakeFileCompileError>,
    kind: ProjectKind,
}
#[derive(Clone, Copy, PartialEq, Eq)]
enum ProjectKind {
    Dependency,
    InWorkspace,
    Test,
}
#[derive(Debug, Clone)]
struct ProjectModuleOrigin {
    project_path: std::path::PathBuf,
    module_path: std::path::PathBuf,
}
struct ModuleState {
    syntax: ElmSyntaxModule,
    source: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (connection, io_thread) = lsp_server::Connection::stdio();

    let (initialize_request_id, initialize_arguments_json) = connection.initialize_start()?;
    connection.initialize_finish(
        initialize_request_id,
        serde_json::to_value(lsp_types::InitializeResult {
            capabilities: server_capabilities(),
            server_info: Some(lsp_types::ServerInfo {
                name: "elm-language-server-rs".to_string(),
                version: Some("0.0.2".to_string()),
            }),
        })?,
    )?;
    let initialize_arguments: lsp_types::InitializeParams =
        serde_json::from_value(initialize_arguments_json)?;
    let state: State = initialize(&connection, &initialize_arguments)?;
    server_loop(&connection, state)?;
    // shut down gracefully
    drop(connection);
    io_thread.join()?;
    Ok(())
}
fn initialize(
    connection: &lsp_server::Connection,
    initialize_arguments: &lsp_types::InitializeParams,
) -> Result<State, Box<dyn std::error::Error>> {
    let mut state: State = initialize_state_for_workspace_directories_into(initialize_arguments);
    if let Some(config_json) = &initialize_arguments.initialization_options {
        update_state_with_configuration(&mut state, config_json);
    } else {
        connection
            .sender
            .send(lsp_server::Message::Request(configuration_request()?))?;
    }
    // only initializing diagnostics once the `elmPath` configuration is received would be better
    publish_and_initialize_state_for_diagnostics_for_projects_in_workspace(connection, &mut state);
    connection.sender.send(lsp_server::Message::Notification(
        lsp_server::Notification {
            method: <lsp_types::request::RegisterCapability as lsp_types::request::Request>::METHOD
                .to_string(),
            params: serde_json::to_value(lsp_types::RegistrationParams {
                registrations: initial_additional_capability_registrations(&state)?,
            })?,
        },
    ))?;
    Ok(state)
}
fn initial_additional_capability_registrations(
    state: &State,
) -> Result<Vec<lsp_types::Registration>, Box<dyn std::error::Error>> {
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
                    glob_pattern: lsp_types::GlobPattern::Relative(lsp_types::RelativePattern {
                        base_uri: lsp_types::OneOf::Right(source_directory_url),
                        pattern: "**/*.elm".to_string(),
                    }),
                    kind: Some(
                        lsp_types::WatchKind::Create
                            | lsp_types::WatchKind::Change
                            | lsp_types::WatchKind::Delete,
                    ),
                })
                .collect::<Vec<lsp_types::FileSystemWatcher>>(),
        };
    let file_watch_registration_options_json: serde_json::Value =
        serde_json::to_value(file_watch_registration_options)?;
    let file_watch_registration: lsp_types::Registration = lsp_types::Registration {
        id: "file-watch".to_string(),
        method: <lsp_types::notification::DidChangeWatchedFiles as lsp_types::notification::Notification>::METHOD.to_string(),
        register_options: Some(file_watch_registration_options_json),
    };
    let workspace_configuration_change_registration: lsp_types::Registration = lsp_types::Registration {
        id: "workspace-configuration".to_string(),
        method: <lsp_types::notification::DidChangeConfiguration as lsp_types::notification::Notification>::METHOD.to_string(),
        register_options: None,
    };
    Ok(vec![
        file_watch_registration,
        workspace_configuration_change_registration,
    ])
}
fn server_capabilities() -> lsp_types::ServerCapabilities {
    lsp_types::ServerCapabilities {
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        semantic_tokens_provider: Some(
            lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                lsp_types::SemanticTokensOptions {
                    work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: lsp_types::SemanticTokensLegend {
                        token_modifiers: vec![],
                        token_types: Vec::from(token_types),
                    },
                    range: None,
                    full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                },
            ),
        ),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::INCREMENTAL,
        )),
        rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                work_done_progress: None,
            },
        })),
        references_provider: Some(lsp_types::OneOf::Left(true)),
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
        document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
        code_action_provider: Some(lsp_types::CodeActionProviderCapability::Options(
            lsp_types::CodeActionOptions {
                code_action_kinds: Some(vec![lsp_types::CodeActionKind::QUICKFIX]),
                resolve_provider: None,
                work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            },
        )),
        ..lsp_types::ServerCapabilities::default()
    }
}
fn server_loop(
    connection: &lsp_server::Connection,
    mut state: State,
) -> Result<(), Box<dyn std::error::Error>> {
    for client_message in &connection.receiver {
        match client_message {
            lsp_server::Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    break;
                }
                if let Err(error) = handle_request(
                    connection,
                    &state,
                    request.id,
                    &request.method,
                    request.params,
                ) {
                    eprintln!("request {} failed: {error}", &request.method);
                }
            }
            lsp_server::Message::Notification(notification) => {
                if let Err(err) = handle_notification(
                    connection,
                    &mut state,
                    &notification.method,
                    notification.params,
                ) {
                    eprintln!("notification {} failed: {err}", notification.method);
                }
            }
            lsp_server::Message::Response(response) => {
                if let Err(err) = handle_response(&mut state, &response.id, response.result) {
                    eprintln!("failed to handle response {}: {err}", response.id);
                }
            }
        }
    }
    Ok(())
}
fn handle_notification(
    connection: &lsp_server::Connection,
    state: &mut State,
    notification_method: &str,
    notification_arguments_json: serde_json::Value,
) -> Result<(), Box<dyn std::error::Error>> {
    match notification_method {
        <lsp_types::notification::DidOpenTextDocument as lsp_types::notification::Notification>::METHOD => {
            let arguments: <lsp_types::notification::DidOpenTextDocument as lsp_types::notification::Notification>::Params =
                serde_json::from_value(notification_arguments_json)?;
            state.open_text_document_uris.insert(arguments.text_document.uri);
        }
        <lsp_types::notification::DidCloseTextDocument as lsp_types::notification::Notification>::METHOD => {
            let arguments: <lsp_types::notification::DidCloseTextDocument as lsp_types::notification::Notification>::Params =
                serde_json::from_value(notification_arguments_json)?;
            state.open_text_document_uris.remove(&arguments.text_document.uri);
        }
        <lsp_types::notification::DidChangeTextDocument as lsp_types::notification::Notification>::METHOD => {
            let arguments: <lsp_types::notification::DidChangeTextDocument as lsp_types::notification::Notification>::Params =
                serde_json::from_value(notification_arguments_json)?;
            update_state_on_did_change_text_document(state, arguments);
        }
        <lsp_types::notification::DidSaveTextDocument as lsp_types::notification::Notification>::METHOD => {
            let arguments: <lsp_types::notification::DidSaveTextDocument as lsp_types::notification::Notification>::Params =
                serde_json::from_value(notification_arguments_json)?;
            if let Ok(saved_path) = &arguments
                .text_document
                .uri
                .to_file_path()
                && let Some((saved_project_path, saved_project_state)) =
                    state_get_mut_project_by_module_path(&mut state.projects, saved_path)
            {
                publish_and_update_state_for_diagnostics_for_document(
                    connection,
                    state.configured_elm_path.as_deref(),
                    state.configured_elm_test_path.as_deref(),
                    saved_project_path,
                    saved_project_state,
                    std::iter::empty(),
                );
            }
        }
        <lsp_types::notification::DidChangeWatchedFiles as lsp_types::notification::Notification>::METHOD => {
            let arguments: <lsp_types::notification::DidChangeWatchedFiles as lsp_types::notification::Notification>::Params =
                serde_json::from_value(notification_arguments_json)?;
            update_state_on_did_change_watched_files(connection, state, arguments);
        }
        <lsp_types::notification::DidChangeConfiguration as lsp_types::notification::Notification>::METHOD => {
            connection.sender.send(lsp_server::Message::Request(
                configuration_request()?
            ))?;
        }
        <lsp_types::notification::Exit as lsp_types::notification::Notification>::METHOD => {}
        _ => {}
    }
    Ok(())
}
fn update_state_on_did_change_watched_files(
    connection: &lsp_server::Connection,
    state: &mut State,
    mut arguments: lsp_types::DidChangeWatchedFilesParams,
) {
    arguments.changes.retain(|file_event| {
        // exclude changes to opened documents are already handled by DidChangeTextDocument.
        // Then why listen to DidChangeWatchedFiles at all?
        // E.g. go to definition needs an up to date module syntax tree
        // in a potentially un-opened file that could have been changed externally,
        // e.g. by calling elm-format, elm-codegen, ...
        !(file_event.typ == lsp_types::FileChangeType::CHANGED
            && state.open_text_document_uris.contains(&file_event.uri))
    });
    if arguments.changes.is_empty() {
        return;
    }
    for (project_path, project_state) in state.projects.iter_mut() {
        let mut project_was_updated: bool = false;
        let mut removed_paths: Vec<lsp_types::Url> = Vec::new();
        'updating_project: for file_change_event in &arguments.changes {
            match file_change_event.uri.to_file_path() {
                Err(()) => {}
                Ok(changed_file_path) => {
                    if changed_file_path.extension().is_none_or(|ext| ext != "elm")
                        || !project_state
                            .source_directories
                            .iter()
                            .any(|dir| changed_file_path.starts_with(dir))
                    {
                        continue 'updating_project;
                    }
                    match file_change_event.typ {
                        lsp_types::FileChangeType::DELETED => {
                            if project_state.modules.remove(&changed_file_path).is_some() {
                                project_was_updated = true;
                                removed_paths.push(file_change_event.uri.clone());
                            }
                        }
                        lsp_types::FileChangeType::CREATED | lsp_types::FileChangeType::CHANGED => {
                            match std::fs::read_to_string(&changed_file_path) {
                                Err(_) => {}
                                Ok(changed_file_source) => {
                                    project_was_updated = true;
                                    project_state.modules.insert(
                                        changed_file_path,
                                        initialize_module_state_from_source(changed_file_source),
                                    );
                                }
                            }
                        }
                        unknown_file_change_type => {
                            eprintln!(
                                "unknown file change type sent by LSP client: {:?}",
                                unknown_file_change_type
                            );
                        }
                    }
                }
            }
        }
        if project_was_updated {
            publish_and_update_state_for_diagnostics_for_document(
                connection,
                state.configured_elm_path.as_deref(),
                state.configured_elm_test_path.as_deref(),
                project_path,
                project_state,
                removed_paths.into_iter(),
            );
        }
    }
}
fn configuration_request() -> Result<lsp_server::Request, Box<dyn std::error::Error>> {
    let requested_configuration: <lsp_types::request::WorkspaceConfiguration as lsp_types::request::Request>::Params =
        lsp_types::ConfigurationParams {
            items: vec![
                lsp_types::ConfigurationItem {
                    scope_uri: None,
                    section: Some("elm-language-server-rs".to_string())
                }
            ]
        };
    Ok(lsp_server::Request {
        id: lsp_server::RequestId::from(ServerRequestId::WorkspaceConfiguration as i32),
        method: <lsp_types::request::WorkspaceConfiguration as lsp_types::request::Request>::METHOD
            .to_string(),
        params: serde_json::to_value(requested_configuration)?,
    })
}
enum ServerRequestId {
    WorkspaceConfiguration,
}
fn handle_response(
    state: &mut State,
    response_id: &lsp_server::RequestId,
    maybe_response_result: Option<serde_json::Value>,
) -> Result<(), Box<dyn std::error::Error>> {
    if response_id == &lsp_server::RequestId::from(ServerRequestId::WorkspaceConfiguration as i32)
        && let Some(response_result) = maybe_response_result
    {
        let response_parsed: <lsp_types::request::WorkspaceConfiguration as lsp_types::request::Request>::Result =
            serde_json::from_value(response_result)?;
        if let Some(config_json) = response_parsed.first() {
            update_state_with_configuration(state, config_json);
        }
    }
    Ok(())
}
fn handle_request(
    connection: &lsp_server::Connection,
    state: &State,
    request_id: lsp_server::RequestId,
    request_method: &str,
    request_arguments_json: serde_json::Value,
) -> Result<(), Box<dyn std::error::Error>> {
    let response: Result<serde_json::Value, lsp_server::ResponseError> = match request_method {
        <lsp_types::request::HoverRequest as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::HoverRequest as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let maybe_hover_result: <lsp_types::request::HoverRequest as lsp_types::request::Request>::Result =
                respond_to_hover(state, &arguments);
            Ok(serde_json::to_value(maybe_hover_result)?)
        }
        <lsp_types::request::GotoDefinition as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::GotoDefinition as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let maybe_hover_result: <lsp_types::request::GotoDefinition as lsp_types::request::Request>::Result =
                respond_to_goto_definition(state, arguments);
            Ok(serde_json::to_value(maybe_hover_result)?)
        }
        <lsp_types::request::PrepareRenameRequest as lsp_types::request::Request>::METHOD => {
            let prepare_rename_arguments: <lsp_types::request::PrepareRenameRequest as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let prepared: Option<
                Result<lsp_types::PrepareRenameResponse, lsp_server::ResponseError>,
            > = respond_to_prepare_rename(state, &prepare_rename_arguments);
            let response_result: Result<
                <lsp_types::request::PrepareRenameRequest as lsp_types::request::Request>::Result,
                lsp_server::ResponseError,
            > = match prepared {
                None => Ok(None),
                Some(result) => result.map(Some),
            };
            match response_result {
                Err(error) => Err(error),
                Ok(maybe_response) => Ok(serde_json::to_value(maybe_response)?),
            }
        }
        <lsp_types::request::Rename as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::Rename as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let maybe_rename_edits: Option<Vec<lsp_types::TextDocumentEdit>> =
                respond_to_rename(state, arguments);
            let result: <lsp_types::request::Rename as lsp_types::request::Request>::Result =
                maybe_rename_edits.map(|rename_edits| lsp_types::WorkspaceEdit {
                    changes: None,
                    document_changes: Some(lsp_types::DocumentChanges::Edits(rename_edits)),
                    change_annotations: None,
                });
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::References as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::References as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::References as lsp_types::request::Request>::Result =
                respond_to_references(state, arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::SemanticTokensFullRequest as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::SemanticTokensFullRequest as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::SemanticTokensFullRequest as lsp_types::request::Request>::Result =
                respond_to_semantic_tokens_full(state, &arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::Completion as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::Completion as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::Completion as lsp_types::request::Request>::Result =
                respond_to_completion(state, &arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::Formatting as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::Formatting as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::Formatting as lsp_types::request::Request>::Result =
                respond_to_document_formatting(state, &arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::DocumentSymbolRequest as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::DocumentSymbolRequest as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::DocumentSymbolRequest as lsp_types::request::Request>::Result =
                respond_to_document_symbols(state, &arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::CodeActionRequest as lsp_types::request::Request>::METHOD => {
            let arguments: <lsp_types::request::CodeActionRequest as lsp_types::request::Request>::Params =
                serde_json::from_value(request_arguments_json)?;
            let result: <lsp_types::request::CodeActionRequest as lsp_types::request::Request>::Result =
                respond_to_code_action(state, arguments);
            Ok(serde_json::to_value(result)?)
        }
        <lsp_types::request::Shutdown as lsp_types::request::Request>::METHOD => {
            let result: <lsp_types::request::Shutdown as lsp_types::request::Request>::Result = ();
            Ok(serde_json::to_value(result)?)
        }
        _ => Err(lsp_server::ResponseError {
            code: lsp_server::ErrorCode::MethodNotFound as i32,
            message: "unhandled method".to_string(),
            data: None,
        }),
    };
    match response {
        Ok(response_value) => {
            send_response_ok(connection, request_id, response_value)?;
        }
        Err(response_error) => send_response_error(connection, request_id, response_error)?,
    }
    Ok(())
}

fn send_response_ok(
    connection: &lsp_server::Connection,
    id: lsp_server::RequestId,
    result: serde_json::Value,
) -> Result<(), Box<dyn std::error::Error>> {
    let response: lsp_server::Response = lsp_server::Response {
        id,
        result: Some(result),
        error: None,
    };
    connection
        .sender
        .send(lsp_server::Message::Response(response))?;
    Ok(())
}
fn send_response_error(
    connection: &lsp_server::Connection,
    id: lsp_server::RequestId,
    error: lsp_server::ResponseError,
) -> Result<(), Box<dyn std::error::Error>> {
    let response: lsp_server::Response = lsp_server::Response {
        id,
        result: None,
        error: Some(error),
    };
    connection
        .sender
        .send(lsp_server::Message::Response(response))?;
    Ok(())
}
fn publish_diagnostics(
    connection: &lsp_server::Connection,
    diagnostics: <lsp_types::notification::PublishDiagnostics as lsp_types::notification::Notification>::Params,
) -> Result<(), Box<dyn std::error::Error>> {
    connection.sender.send(lsp_server::Message::Notification(
        lsp_server::Notification {
            method: <lsp_types::notification::PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
            params: serde_json::to_value(diagnostics)?,
        },
    ))?;
    Ok(())
}

fn update_state_on_did_change_text_document(
    state: &mut State,
    did_change_text_document: lsp_types::DidChangeTextDocumentParams,
) {
    let changed_file_path: std::path::PathBuf =
        if let Ok(changed_file_path) = did_change_text_document.text_document.uri.to_file_path() {
            changed_file_path
        } else {
            return;
        };
    for project_state in state.projects.values_mut() {
        if let Some(module_state) = project_state.modules.remove(&changed_file_path) {
            let mut changed_source: String = module_state.source;
            for change in did_change_text_document.content_changes {
                match (change.range, change.range_length) {
                    (None, None) => {
                        // means full replacement
                        project_state.modules.insert(
                            changed_file_path,
                            initialize_module_state_from_source(change.text),
                        );
                        return;
                    }
                    (Some(range), Some(range_length)) => {
                        string_replace_lsp_range(
                            &mut changed_source,
                            range,
                            range_length as usize,
                            &change.text,
                        );
                    }
                    (None, _) | (_, None) => {}
                }
            }
            project_state.modules.insert(
                changed_file_path,
                initialize_module_state_from_source(changed_source),
            );
            break;
        }
    }
}

fn publish_and_initialize_state_for_diagnostics_for_projects_in_workspace(
    connection: &lsp_server::Connection,
    state: &mut State,
) {
    for (in_workspace_project_path, in_workspace_project_state) in
        state
            .projects
            .iter_mut()
            .filter(|(_, project)| match project.kind {
                ProjectKind::InWorkspace | ProjectKind::Test => true,
                ProjectKind::Dependency => false,
            })
    {
        match compute_diagnostics(
            state.configured_elm_path.as_deref(),
            state.configured_elm_test_path.as_deref(),
            in_workspace_project_path,
            in_workspace_project_state,
        ) {
            Err(error) => {
                eprintln!("{error}");
            }
            Ok(elm_make_errors) => {
                let diagnostics_to_publish: Vec<lsp_types::PublishDiagnosticsParams> =
                    elm_make_errors
                        .iter()
                        .filter_map(|elm_make_file_error| {
                            let url: lsp_types::Url =
                                lsp_types::Url::from_file_path(&elm_make_file_error.path).ok()?;
                            let diagnostics: Vec<lsp_types::Diagnostic> = elm_make_file_error
                                .problems
                                .iter()
                                .map(elm_make_file_problem_to_diagnostic)
                                .collect::<Vec<_>>();
                            Some(lsp_types::PublishDiagnosticsParams {
                                uri: url,
                                diagnostics: diagnostics,
                                version: None,
                            })
                        })
                        .collect::<Vec<_>>();
                for file_diagnostics_to_publish in diagnostics_to_publish {
                    let _ = publish_diagnostics(connection, file_diagnostics_to_publish);
                }
                in_workspace_project_state.elm_make_errors = elm_make_errors;
            }
        }
    }
}

fn publish_and_update_state_for_diagnostics_for_document(
    connection: &lsp_server::Connection,
    configured_elm_path: Option<&str>,
    configured_elm_test_path: Option<&str>,
    project_path: &std::path::Path,
    project: &mut ProjectState,
    removed_paths: impl Iterator<Item = lsp_types::Url>,
) {
    match compute_diagnostics(
        configured_elm_path,
        configured_elm_test_path,
        project_path,
        project,
    ) {
        Err(error) => {
            eprintln!("{error}");
        }
        Ok(elm_make_errors) => {
            let mut updated_diagnostics_to_publish: Vec<lsp_types::PublishDiagnosticsParams> =
                Vec::new();
            for elm_make_file_error in project.modules.keys() {
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
                        let was_error: bool = project
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
            for removed_url in removed_paths {
                updated_diagnostics_to_publish.push(lsp_types::PublishDiagnosticsParams {
                    uri: removed_url,
                    diagnostics: vec![],
                    version: None,
                });
            }
            for updated_file_diagnostics_to_publish in updated_diagnostics_to_publish {
                let _ = publish_diagnostics(connection, updated_file_diagnostics_to_publish);
            }
            project.elm_make_errors = elm_make_errors;
        }
    }
}

fn compute_diagnostics(
    configured_elm_path: Option<&str>,
    configured_elm_test_path: Option<&str>,
    project_path: &std::path::Path,
    project_state: &ProjectState,
) -> Result<Vec<ElmMakeFileCompileError>, String> {
    if !std::path::Path::exists(project_path) {
        // project zombie. Probably got deleted
        return Ok(vec![]);
    }
    // if there is a better way, please open an issue <3
    let sink_path: &str = match std::env::consts::FAMILY {
        "windows" => "NUL",
        _ => "/dev/null",
    };
    let compiler_executable_name = match project_state.kind {
        ProjectKind::Dependency | ProjectKind::InWorkspace => configured_elm_path.unwrap_or("elm"),
        ProjectKind::Test => configured_elm_test_path.unwrap_or("elm-test"),
    };
    let elm_make_process: std::process::Child = std::process::Command::new(compiler_executable_name)
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
                "I tried to run {compiler_executable_name} make at path {project_path:?} but it failed: {error}. Try installing elm via `npm install -g elm`."
            )
        })?;
    let elm_make_output: std::process::Output = elm_make_process
        .wait_with_output()
        .map_err(|error| format!("I wasn't able to read the output of elm make: {error}"))?;
    Ok(if elm_make_output.stderr.is_empty() {
        vec![]
    } else {
        let elm_make_report_json: serde_json::Value =
            serde_json::from_slice(&elm_make_output.stderr).map_err(|parse_error| {
                format!(
                    "failed to parse elm make report json: {parse_error}, full text: {}",
                    str::from_utf8(&elm_make_output.stderr).unwrap_or("")
                )
            })?;
        parse_elm_make_report(&elm_make_report_json)?
    })
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
                } else if bold || underline {
                    builder.push_str(&text.to_ascii_uppercase());
                } else {
                    // suspicious, would have expected ::Plain
                    builder.push_str(text);
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
                .iter()
                .map(parse_elm_make_file_compile_error)
                .collect::<Result<Vec<_>, String>>(),
            _ => Err("field errors must be array".to_string()),
        },
        Some(unknown_type) => Err(format!("unknown report type {unknown_type}")),
        None => Err("report type must exist as a string".to_string()),
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
        .ok_or("field problems must exist as array")?
        .iter()
        .map(parse_elm_make_file_internal_compile_problem)
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
        .map(parse_elm_make_message_segment)
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
        .ok_or("file region line must be integer")?;
    let column_1_based: i64 = json
        .get("column")
        .and_then(serde_json::Value::as_i64)
        .ok_or("file region column must be integer")?;
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
fn update_state_with_configuration(state: &mut State, config_json: &serde_json::Value) {
    state.configured_elm_path = config_json
        .get("elmPath")
        .and_then(|path_json| path_json.as_str())
        .and_then(|path| {
            if path.is_empty() {
                None
            } else {
                Some(path.to_string())
            }
        });
    state.configured_elm_test_path = config_json
        .get("elmTestPath")
        .and_then(|path_json| path_json.as_str())
        .and_then(|path| {
            if path.is_empty() {
                None
            } else {
                Some(path.to_string())
            }
        });
    state.configured_elm_formatter = config_json
        .get("elmFormatPath")
        .and_then(|path_json| path_json.as_str())
        .and_then(|path| {
            if path.is_empty() {
                None
            } else {
                Some(if path == "builtin" {
                    ConfiguredElmFormatter::Builtin
                } else {
                    ConfiguredElmFormatter::Custom {
                        path: path.to_string(),
                    }
                })
            }
        });
}
fn initialize_state_for_workspace_directories_into(
    initialize_arguments: &lsp_types::InitializeParams,
) -> State {
    let mut state: State = State {
        projects: std::collections::HashMap::new(),
        open_text_document_uris: std::collections::HashSet::new(),
        configured_elm_path: None,
        configured_elm_test_path: None,
        configured_elm_formatter: None,
    };
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
    let mut skipped_dependencies: std::collections::HashSet<std::path::PathBuf> =
        std::collections::HashSet::new();
    let _modules_exposed_from_workspace_packages = initialize_state_for_projects_into(
        &mut state,
        &mut std::collections::HashMap::new(),
        &mut skipped_dependencies,
        &elm_home_path,
        ProjectKind::InWorkspace,
        list_elm_project_directories_in_directory_at_path(workspace_directory_paths).into_iter(),
    );
    if !skipped_dependencies.is_empty() {
        eprintln!(
            "I will skip initializing these dependency {}: {}. \
            I can only load packages that you've actively downloaded with `elm install`. \
            If you did and don't care about LSP functionality for indirect dependencies, ignore this message.",
            if skipped_dependencies.len() == 1 {
                "project"
            } else {
                "projects"
            },
            skipped_dependencies
                .into_iter()
                .map(|dep| dep.to_string_lossy().into_owned())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    let (fully_parsed_project_sender, fully_parsed_project_receiver) = std::sync::mpsc::channel();
    std::thread::scope(|thread_scope| {
        for (uninitialized_project_path, uninitialized_project_state) in &state.projects {
            let projects_that_finished_full_parse_sender = fully_parsed_project_sender.clone();
            thread_scope.spawn(move || {
                let mut fully_parsed_modules: std::collections::HashMap<
                    std::path::PathBuf,
                    ModuleState,
                > = std::collections::HashMap::new();
                for uninitialized_module_path in uninitialized_project_state.modules.keys() {
                    if let Ok(module_source) = std::fs::read_to_string(uninitialized_module_path) {
                        fully_parsed_modules.insert(
                            uninitialized_module_path.clone(),
                            initialize_module_state_from_source(module_source),
                        );
                    }
                }
                projects_that_finished_full_parse_sender
                    .send((uninitialized_project_path.clone(), fully_parsed_modules))
            });
        }
    });
    drop(fully_parsed_project_sender);
    while let Ok((fully_parsed_project_path, fully_parsed_project_modules)) =
        fully_parsed_project_receiver.recv()
    {
        if let Some(project_state_to_update) = state.projects.get_mut(&fully_parsed_project_path) {
            project_state_to_update.modules = fully_parsed_project_modules;
        }
    }
    state
}

/// returns exposed module names and their origins
fn initialize_state_for_projects_into(
    state: &mut State,
    all_dependency_exposed_module_names: &mut std::collections::HashMap<
        std::path::PathBuf,
        std::collections::HashMap<String, ProjectModuleOrigin>,
    >,
    skipped_dependencies: &mut std::collections::HashSet<std::path::PathBuf>,
    elm_home_path: &std::path::PathBuf,
    project_kind: ProjectKind,
    project_paths: impl Iterator<Item = std::path::PathBuf>,
) -> std::collections::HashMap<String, ProjectModuleOrigin> {
    let mut dependency_exposed_module_names: std::collections::HashMap<
        String,
        ProjectModuleOrigin,
    > = std::collections::HashMap::new();
    for project_path in project_paths {
        dependency_exposed_module_names.extend(initialize_state_for_project_into(
            state,
            all_dependency_exposed_module_names,
            skipped_dependencies,
            elm_home_path,
            project_kind,
            project_path,
        ));
    }
    dependency_exposed_module_names
}
fn initialize_state_for_project_into(
    state: &mut State,
    all_dependency_exposed_module_names: &mut std::collections::HashMap<
        std::path::PathBuf,
        std::collections::HashMap<String, ProjectModuleOrigin>,
    >,
    skipped_dependencies: &mut std::collections::HashSet<std::path::PathBuf>,
    elm_home_path: &std::path::PathBuf,
    project_kind: ProjectKind,
    project_path: std::path::PathBuf,
) -> std::collections::HashMap<String, ProjectModuleOrigin> {
    if let Some(project_exposed_module_names) =
        all_dependency_exposed_module_names.get(&project_path)
    {
        return project_exposed_module_names.clone();
    }
    let elm_json_path: std::path::PathBuf = std::path::Path::join(&project_path, "elm.json");
    let maybe_elm_json_value: Option<serde_json::Value> = std::fs::read_to_string(&elm_json_path)
        .ok()
        .and_then(|elm_json_source| {
            serde_json::from_str(&elm_json_source)
                .map_err(|json_parse_error: serde_json::Error| {
                    eprintln!("I couldn't read this elm.json as JSON: {json_parse_error}");
                })
                .ok()
        });
    let maybe_elm_json: Option<ElmJson> =
        maybe_elm_json_value.as_ref().and_then(|elm_json_value| {
            parse_elm_json(elm_json_value)
                .map_err(|json_decode_error| {
                    eprintln!("I couldn't understand this elm.json: {}", json_decode_error);
                })
                .ok()
        });
    if maybe_elm_json.is_none() {
        match project_kind {
            ProjectKind::InWorkspace => {
                eprintln!(
                    "I couldn't find a valid elm.json found at path {elm_json_path:?}. Now looking for elm module files across the workspace and elm/core 1.0.5"
                );
            }
            ProjectKind::Dependency => {
                skipped_dependencies.insert(project_path);
                return std::collections::HashMap::new();
            }
            ProjectKind::Test => {}
        }
    }
    let elm_json_source_directories: Vec<std::path::PathBuf> = match &maybe_elm_json {
        None => {
            vec![project_path.clone()]
        }
        Some(ElmJson::Application {
            source_directories,
            direct_dependencies: _,
            test_direct_dependencies: _,
        }) => source_directories
            .iter()
            .copied()
            .map(|relative| std::path::Path::join(&project_path, relative))
            .collect::<Vec<_>>(),
        Some(ElmJson::Package { .. }) => vec![std::path::Path::join(&project_path, "src")],
    };
    let dependency_path = |package_name: &str, package_version: &str| {
        std::path::Path::join(
            elm_home_path,
            format!("0.19.1/packages/{package_name}/{package_version}"),
        )
    };
    let direct_dependency_paths: Vec<std::path::PathBuf> = match &maybe_elm_json {
        None => vec![dependency_path("elm/core", "1.0.5")],
        Some(ElmJson::Application {
            direct_dependencies,
            source_directories: _,
            test_direct_dependencies,
        }) => direct_dependencies
            .iter()
            .chain(test_direct_dependencies)
            .map(|(name, version)| dependency_path(name, version))
            .collect::<Vec<_>>(),
        Some(ElmJson::Package {
            dependency_minimum_versions,
            exposed_modules: _,
            test_dependency_minimum_versions,
        }) => dependency_minimum_versions
            .iter()
            .chain(test_dependency_minimum_versions)
            .map(|(n, v)| dependency_path(n, v))
            .collect::<Vec<_>>(),
    };
    let module_states: std::collections::HashMap<std::path::PathBuf, ModuleState> =
        list_elm_files_in_directory_at_paths(elm_json_source_directories.iter().cloned())
            .into_iter()
            .map(|module_path| (module_path, uninitialized_module_state))
            .collect::<std::collections::HashMap<_, _>>();
    let mut exposed_module_names: std::collections::HashMap<String, ProjectModuleOrigin> =
        std::collections::HashMap::new();
    if let Some(ElmJson::Package {
        exposed_modules,
        dependency_minimum_versions: _,
        test_dependency_minimum_versions: _,
    }) = &maybe_elm_json
    {
        for &exposed_module_name in exposed_modules {
            let maybe_module_origin_path: Option<&std::path::PathBuf> =
                module_states.keys().find(|module_path| {
                    derive_module_name_from_path(&elm_json_source_directories, module_path)
                        .is_some_and(|derived_module_name| {
                            derived_module_name == exposed_module_name
                        })
                });
            if let Some(module_origin_path) = maybe_module_origin_path {
                exposed_module_names.insert(
                    exposed_module_name.to_string(),
                    ProjectModuleOrigin {
                        project_path: project_path.clone(),
                        module_path: module_origin_path.clone(),
                    },
                );
            }
        }
    }
    let direct_dependency_exposed_module_names: std::collections::HashMap<
        String,
        ProjectModuleOrigin,
    > = initialize_state_for_projects_into(
        state,
        all_dependency_exposed_module_names,
        skipped_dependencies,
        elm_home_path,
        ProjectKind::Dependency,
        direct_dependency_paths.into_iter(),
    );
    match project_kind {
        ProjectKind::Dependency | ProjectKind::Test => {}
        ProjectKind::InWorkspace => {
            let tests_source_directory_path: std::path::PathBuf =
                std::path::Path::join(&project_path, "tests");
            let test_only_dependencies: Box<dyn Iterator<Item = std::path::PathBuf>> =
                match &maybe_elm_json {
                    None => Box::new(std::iter::empty()),
                    Some(ElmJson::Application {
                        direct_dependencies: _,
                        source_directories: _,
                        test_direct_dependencies,
                    }) => Box::new(
                        test_direct_dependencies
                            .iter()
                            .map(|(n, v)| dependency_path(n, v)),
                    ),
                    Some(ElmJson::Package {
                        dependency_minimum_versions: _,
                        exposed_modules: _,
                        test_dependency_minimum_versions,
                    }) => Box::new(
                        test_dependency_minimum_versions
                            .iter()
                            .map(|(n, v)| dependency_path(n, v)),
                    ),
                };
            let mut test_direct_dependency_exposed_module_names: std::collections::HashMap<
                String,
                ProjectModuleOrigin,
            > = initialize_state_for_projects_into(
                state,
                all_dependency_exposed_module_names,
                skipped_dependencies,
                elm_home_path,
                ProjectKind::Dependency,
                test_only_dependencies,
            );
            test_direct_dependency_exposed_module_names
                .extend(direct_dependency_exposed_module_names.clone());
            test_direct_dependency_exposed_module_names.extend(module_states.iter().filter_map(
                |(module_path, module_state)| {
                    let module_header = module_state.syntax.header.as_ref()?;
                    let module_name_node = module_header.module_name.as_ref()?;
                    Some((
                        module_name_node.value.to_string(),
                        ProjectModuleOrigin {
                            project_path: project_path.clone(),
                            module_path: module_path.clone(),
                        },
                    ))
                },
            ));
            let test_module_states: std::collections::HashMap<std::path::PathBuf, ModuleState> =
                list_elm_files_in_directory_at_paths(std::iter::once(
                    tests_source_directory_path.clone(),
                ))
                .into_iter()
                .map(|module_path| (module_path, uninitialized_module_state))
                .collect::<std::collections::HashMap<_, _>>();
            state.projects.insert(
                tests_source_directory_path.clone(),
                ProjectState {
                    kind: ProjectKind::Test,
                    source_directories: vec![tests_source_directory_path],
                    modules: test_module_states,
                    dependency_exposed_module_names: test_direct_dependency_exposed_module_names,
                    elm_make_errors: vec![],
                },
            );
        }
    }
    state.projects.insert(
        project_path.clone(),
        ProjectState {
            kind: project_kind,
            source_directories: elm_json_source_directories,
            modules: module_states,
            dependency_exposed_module_names: direct_dependency_exposed_module_names,
            elm_make_errors: vec![],
        },
    );
    if !exposed_module_names.is_empty() {
        all_dependency_exposed_module_names.insert(project_path, exposed_module_names.clone());
    }
    exposed_module_names
}
/// A yet to be initialized dummy [`ModuleState`]
const uninitialized_module_state: ModuleState = ModuleState {
    source: String::new(),
    syntax: ElmSyntaxModule {
        header: None,
        documentation: None,
        comments: vec![],
        imports: vec![],
        declarations: vec![],
    },
};
fn initialize_module_state_from_source(source: String) -> ModuleState {
    ModuleState {
        syntax: parse_elm_syntax_module(&source),
        source: source,
    }
}
enum ElmJson<'a> {
    Application {
        source_directories: Vec<&'a str>,
        direct_dependencies: std::collections::HashMap<&'a str, &'a str>,
        test_direct_dependencies: std::collections::HashMap<&'a str, &'a str>,
    },
    Package {
        dependency_minimum_versions: std::collections::HashMap<&'a str, &'a str>,
        exposed_modules: Vec<&'a str>,
        test_dependency_minimum_versions: std::collections::HashMap<&'a str, &'a str>,
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
                    .and_then(|dependencies| dependencies.get("direct"))
                {
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
                            direct_dependencies
                                .insert(direct_dependency_name.as_str(), direct_dependency_version);
                        }
                        Ok::<std::collections::HashMap<&str, &str>, String>(direct_dependencies)
                    }
                    _ => Err("must have field dependencies.direct".to_string()),
                }?;
                let test_direct_dependencies: std::collections::HashMap<&str, &str> =
                    match json_object
                        .get("test-dependencies")
                        .and_then(|dependencies| dependencies.get("direct"))
                    {
                        Some(serde_json::Value::Object(direct_dependencies_json)) => {
                            let mut test_direct_dependencies: std::collections::HashMap<
                                &str,
                                &str,
                            > = std::collections::HashMap::new();
                            for (
                                test_direct_dependency_name,
                                test_direct_dependency_version_json,
                            ) in direct_dependencies_json
                            {
                                let test_direct_dependency_version: &str =
                                    match test_direct_dependency_version_json {
                                        serde_json::Value::String(v) => Ok(v.as_str()),
                                        _ => Err(format!(
                                            "{test_direct_dependency_name} dependency version must be a string"
                                        )),
                                    }?;
                                test_direct_dependencies.insert(
                                    test_direct_dependency_name.as_str(),
                                    test_direct_dependency_version,
                                );
                            }
                            Ok::<std::collections::HashMap<&str, &str>, String>(
                                test_direct_dependencies,
                            )
                        }
                        _ => Err("must have field dependencies.direct".to_string()),
                    }?;
                let mut source_directories: Vec<&str> = Vec::new();
                match json_object.get("source-directories") {
                    Some(serde_json::Value::Array(source_directories_json)) => {
                        for source_directory_json in source_directories_json {
                            match source_directory_json {
                                serde_json::Value::String(source_directory) => {
                                    source_directories.push(source_directory);
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
                    test_direct_dependencies: test_direct_dependencies,
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
                                let dependency_version_minimum: &str =
                                    match direct_dependency_version_json {
                                        serde_json::Value::String(
                                            dependency_version_constraint,
                                        ) => elm_json_version_constraint_to_minimum_version(
                                            dependency_version_constraint,
                                        ),
                                        _ => Err(format!(
                                            "{direct_dependency_name} dependency version must be a string"
                                        )),
                                    }?;
                                dependency_minimum_versions.insert(
                                    direct_dependency_name.as_str(),
                                    dependency_version_minimum,
                                );
                            }
                            Ok(dependency_minimum_versions)
                        }
                        _ => Err("must have field dependencies".to_string()),
                    }?;
                let test_dependency_minimum_versions: std::collections::HashMap<&str, &str> =
                    match json_object.get("test-dependencies") {
                        Some(serde_json::Value::Object(dependencies)) => {
                            let mut test_dependency_minimum_versions: std::collections::HashMap<
                                &str,
                                &str,
                            > = std::collections::HashMap::new();
                            for (
                                test_direct_dependency_name,
                                test_direct_dependency_version_json,
                            ) in dependencies
                            {
                                let test_dependency_version_minimum: &str =
                                    match test_direct_dependency_version_json {
                                        serde_json::Value::String(
                                            dependency_version_constraint,
                                        ) => elm_json_version_constraint_to_minimum_version(
                                            dependency_version_constraint,
                                        ),
                                        _ => Err(format!(
                                            "{test_direct_dependency_name} dependency version must be a string"
                                        )),
                                    }?;
                                test_dependency_minimum_versions.insert(
                                    test_direct_dependency_name.as_str(),
                                    test_dependency_version_minimum,
                                );
                            }
                            Ok(test_dependency_minimum_versions)
                        }
                        _ => Err("must have field dependencies".to_string()),
                    }?;
                let mut exposed_modules: Vec<&str> = Vec::new();
                match json_object.get("exposed-modules") {
                    Some(serde_json::Value::Array(source_directories_json)) => {
                        for source_directory_json in source_directories_json {
                            match source_directory_json {
                                serde_json::Value::String(source_directory) => {
                                    exposed_modules.push(source_directory);
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
                                                exposed_modules.push(source_directory);
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
                    test_dependency_minimum_versions: test_dependency_minimum_versions,
                })
            }
            _ => Err("field type must be package or application".to_string()),
        },
        _ => Err("must have field type".to_string()),
    }
}
fn elm_json_version_constraint_to_minimum_version(
    elm_json_version_constraint: &str,
) -> Result<&str, String> {
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
                            .is_some_and(|module_name_node| {
                                module_name_node.value.as_ref() == module_name
                            })
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
    state_get_project_module_by_path(state, &file_path)
}
fn state_get_project_module_by_path<'a>(
    state: &'a State,
    file_path: &std::path::PathBuf,
) -> Option<ProjectModuleState<'a>> {
    state
        .projects
        .iter()
        .find_map(|(project_path, project_state)| {
            let module_state = project_state.modules.get(file_path)?;
            Some(ProjectModuleState {
                project_path: project_path,
                project: project_state,
                module: module_state,
            })
        })
}
fn state_get_mut_project_by_module_path<'a>(
    projects: &'a mut std::collections::HashMap<std::path::PathBuf, ProjectState>,
    file_path: &std::path::PathBuf,
) -> Option<(&'a std::path::PathBuf, &'a mut ProjectState)> {
    projects
        .iter_mut()
        .find_map(|(project_path, project_state)| {
            if project_state.modules.contains_key(file_path) {
                Some((project_path, project_state))
            } else {
                None
            }
        })
}
fn respond_to_hover(
    state: &State,
    hover_arguments: &lsp_types::HoverParams,
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
            let (origin_module_path, origin_module_state) = project_state_get_module_with_name(
                state,
                hovered_project_module_state.project,
                hovered_module_name,
            )?;
            let origin_module_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_path).ok()?;
            // also show list of exports maybe?
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: match &origin_module_state.syntax.documentation {
                        None => "_module has no documentation comment_".to_string(),
                        Some(module_documentation) => elm_syntax_module_documentation_to_markdown(
                            &origin_module_url,
                            &origin_module_state.syntax,
                            &module_documentation.value,
                        ),
                    },
                }),
                range: Some(hovered_symbol_node.range),
            })
        }
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name: hovered_name,
            all_exposes: _,
        }
        | ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name: hovered_name,
            module_documentation: _,
        } => {
            let hovered_module_origin: &str = hovered_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
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
                .find_map(|documented_declaration_or_err| {
                    let documented_declaration = documented_declaration_or_err.as_ref().ok()?;
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
                                .map(|node| node.value.as_ref())
                                == Some(hovered_name)
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state
                                        .module
                                        .syntax
                                        .comments,
                                    declaration_node.range,
                                    origin_module_declaration_name
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    origin_module_declaration_variant0_values,
                                    origin_module_declaration_variant1_up,
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
                                                .find_map(|origin_module_declaration_or_err| {
                                                    let origin_module_declaration = origin_module_declaration_or_err.as_ref().ok()?;
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
                                                        } if origin_module_declaration_name.value
                                                            == origin_module_declaration_function_node
                                                                .value =>
                                                        {
                                                            Some((
                                                                origin_module_declaration_signature
                                                                    .as_ref(),
                                                                origin_module_declaration
                                                                    .documentation
                                                                    .as_ref()
                                                                    .map(|node| node.value.as_ref()),
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
                                        .map(|node| node.value.as_ref()),
                                    maybe_declaration_direction.map(|node| node.value),
                                    maybe_declaration_precedence.map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if let Some(declaration_name_node) = maybe_declaration_name &&
                                declaration_name_node.value.as_ref() == hovered_name
                            {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(declaration_name_node, Box::as_ref)),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    type_.as_ref().map(elm_syntax_node_as_ref),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: maybe_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if let Some(declaration_name_node) = maybe_declaration_name.as_ref() &&
                               declaration_name_node.value.as_ref() == hovered_name
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(declaration_name_node, Box::as_ref)),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    type_.as_ref().map(elm_syntax_node_as_ref),
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
                            if declaration_name_node.value.as_ref() == hovered_name {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    elm_syntax_node_as_ref_map(declaration_name_node, Box::as_ref),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    declaration_maybe_signature.as_ref().and_then(|signature| {
                                        signature.type_.as_ref().map(elm_syntax_node_as_ref)
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
            declaration: declaration_node,
        } => {
            let hovered_module_origin: &str = hovered_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            let origin_module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                );
            let origin_declaration_info_markdown: String = match &declaration_node.value {
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
                                .map(|node| node.value.as_ref())
                        {
                            ""
                        } else {
                            "variant in\n"
                        },
                        &present_choice_type_declaration_info_markdown(
                            &origin_module_origin_lookup,
                            hovered_module_origin,
                            &hovered_project_module_state.module.syntax.comments,
                            declaration_node.range,
                            origin_module_declaration_name
                                .as_ref()
                                .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                            documentation,
                            origin_module_declaration_parameters,
                            origin_module_declaration_variant0_name_node
                                .as_ref()
                                .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                            origin_module_declaration_variant0_values,
                            origin_module_declaration_variant1_up,
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
                                    .find_map(|origin_module_declaration_or_err| {
                                        let origin_module_declaration =
                                            origin_module_declaration_or_err.as_ref().ok()?;
                                        let origin_module_declaration_node =
                                            origin_module_declaration.declaration.as_ref()?;
                                        match &origin_module_declaration_node.value {
                                            ElmSyntaxDeclaration::Variable {
                                                start_name: origin_module_declaration_name,
                                                signature: origin_module_declaration_signature,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } if origin_module_declaration_name.value
                                                == origin_module_declaration_function_node
                                                    .value =>
                                            {
                                                Some((
                                                    origin_module_declaration_signature.as_ref(),
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref()
                                                        .map(|node| node.value.as_ref()),
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
                        maybe_origin_module_declaration_direction.map(|node| node.value),
                        maybe_origin_module_declaration_precedence.map(|node| node.value),
                    )
                }
                ElmSyntaxDeclaration::Port {
                    name: maybe_declaration_name,
                    colon_key_symbol_range: _,
                    type_,
                } => present_port_declaration_info_markdown(
                    &origin_module_origin_lookup,
                    hovered_module_origin,
                    &hovered_project_module_state.module.syntax.comments,
                    declaration_node.range,
                    maybe_declaration_name
                        .as_ref()
                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                    documentation,
                    type_.as_ref().map(elm_syntax_node_as_ref),
                ),
                ElmSyntaxDeclaration::TypeAlias {
                    alias_keyword_range: _,
                    name: maybe_declaration_name,
                    parameters: origin_module_declaration_parameters,
                    equals_key_symbol_range: _,
                    type_,
                } => present_type_alias_declaration_info_markdown(
                    &origin_module_origin_lookup,
                    hovered_module_origin,
                    &hovered_project_module_state.module.syntax.comments,
                    declaration_node.range,
                    maybe_declaration_name
                        .as_ref()
                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                    documentation,
                    origin_module_declaration_parameters,
                    type_.as_ref().map(elm_syntax_node_as_ref),
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
                    &hovered_project_module_state.module.syntax.comments,
                    elm_syntax_node_as_ref_map(origin_module_declaration_name_node, Box::as_ref),
                    documentation,
                    origin_module_declaration_maybe_signature
                        .as_ref()
                        .and_then(|signature| signature.type_.as_ref())
                        .map(elm_syntax_node_as_ref),
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
                .find_map(|documented_declaration_or_err| {
                    let documented_declaration = documented_declaration_or_err.as_ref().ok()?;
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
                                .map(|node| node.value.as_ref())
                                == Some(hovered_name)
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    declaration_node.range,
                                    origin_module_declaration_name
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    origin_module_declaration_variant0_values,
                                    origin_module_declaration_variant1_up,
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
                                                |origin_module_declaration_or_err| {
                                                    let origin_module_declaration =
                                                        origin_module_declaration_or_err
                                                            .as_ref()
                                                            .ok()?;
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
                                                } if origin_module_declaration_name.value
                                                    == origin_module_declaration_function_node
                                                        .value =>
                                                {
                                                    Some((
                                                        origin_module_declaration_signature
                                                            .as_ref(),
                                                        origin_module_declaration
                                                            .documentation
                                                            .as_ref()
                                                            .map(|node| node.value.as_ref()),
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
                                        .map(|node| node.value.as_ref()),
                                    maybe_declaration_direction.map(|node| node.value),
                                    maybe_declaration_precedence.map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if let Some(declaration_name_node) = maybe_declaration_name
                                && declaration_name_node.value.as_ref() == hovered_name
                            {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        declaration_name_node,
                                        Box::as_ref,
                                    )),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    type_.as_ref().map(elm_syntax_node_as_ref),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: maybe_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if let Some(declaration_name_node) = maybe_declaration_name
                                && declaration_name_node.value.as_ref() == hovered_name
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        declaration_name_node,
                                        Box::as_ref,
                                    )),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    type_.as_ref().map(elm_syntax_node_as_ref),
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
                            if declaration_name_node.value.as_ref() == hovered_name {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    elm_syntax_node_as_ref_map(declaration_name_node, Box::as_ref),
                                    documented_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    declaration_maybe_signature
                                        .as_ref()
                                        .and_then(|signature| signature.type_.as_ref())
                                        .map(elm_syntax_node_as_ref),
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
        ElmSyntaxSymbol::LetDeclarationName {
            name: hovered_name,
            signature_type: maybe_signature_type,
            start_name_range,
            scope_expression: _,
        } => Some(lsp_types::Hover {
            contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: let_declaration_info_markdown(
                    state,
                    hovered_project_module_state.project,
                    &hovered_project_module_state.module.syntax,
                    ElmSyntaxNode {
                        range: start_name_range,
                        value: hovered_name,
                    },
                    maybe_signature_type,
                ),
            }),
            range: Some(hovered_symbol_node.range),
        }),
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
                            hovered_project_module_state.project,
                            &hovered_project_module_state.module.syntax,
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
                ElmQualified {
                    qualification: hovered_qualification,
                    name: hovered_name,
                },
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
                .find_map(|origin_module_declaration_or_err| {
                    let origin_module_declaration = origin_module_declaration_or_err.as_ref().ok()?;
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
                                    .is_some_and(|name_node| name_node.value.as_ref() == hovered_name))
                                    || (origin_module_declaration_variant1_up.iter().any(
                                        |variant| {
                                            variant.name.as_ref().is_some_and(|name_node| {
                                                name_node.value.as_ref() == hovered_name
                                            })
                                        },
                                    ));
                            if any_declared_name_matches_hovered {
                                Some(format!(
                                    "variant in\n{}",
                                    &present_choice_type_declaration_info_markdown(
                                        &origin_module_origin_lookup,
                                        hovered_module_origin,
                                        &hovered_project_module_state.module.syntax.comments,
                                        origin_module_declaration_node.range,
                                        origin_module_declaration_name
                                            .as_ref()
                                            .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                        origin_module_declaration
                                            .documentation
                                            .as_ref()
                                            .map(|node| node.value.as_ref()),
                                        origin_module_declaration_parameters,
                                        origin_module_declaration_variant0_name_node
                                            .as_ref()
                                            .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                        origin_module_declaration_variant0_values,
                                        origin_module_declaration_variant1_up,
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
                                                |origin_module_potential_function_declaration_or_err| {
                                                    let origin_module_potential_function_declaration = origin_module_potential_function_declaration_or_err.as_ref().ok()?;
                                                    let origin_module_potential_function_declaration_node = origin_module_potential_function_declaration.declaration.as_ref()?;
                                                    match &origin_module_potential_function_declaration_node.value {
                                                        ElmSyntaxDeclaration::Variable {
                                                            start_name: origin_module_declaration_name,
                                                            signature: origin_module_declaration_signature,
                                                            ..
                                                        } if origin_module_declaration_name.value
                                                            == origin_module_declaration_function_node.value => {
                                                            Some((
                                                                origin_module_declaration_signature.as_ref(),
                                                                origin_module_potential_function_declaration
                                                                    .documentation
                                                                    .as_ref()
                                                                    .map(|node| node.value.as_ref()),
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
                                        .map(|node| node.value.as_ref()),
                                    maybe_origin_module_declaration_direction
                                        .map(|node| node.value),
                                    maybe_origin_module_declaration_precedence
                                        .map(|node| node.value),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_origin_module_declaration_name,
                            colon_key_symbol_range: _,
                            type_,
                        } => {
                            if let Some(origin_module_declaration_name_node) = maybe_origin_module_declaration_name &&
                                origin_module_declaration_name_node.value.as_ref() == hovered_name {
                                Some(present_port_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(origin_module_declaration_name_node, Box::as_ref)),
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    type_.as_ref().map(elm_syntax_node_as_ref),
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
                            if let Some(origin_module_declaration_name_node) = maybe_origin_module_declaration_name
                                && origin_module_declaration_name_node.value.as_ref() == hovered_name
                            {
                                Some(format!(
                                    "constructor function for record\n{}",
                                    &present_type_alias_declaration_info_markdown(
                                        &origin_module_origin_lookup,
                                        hovered_module_origin,
                                        &hovered_project_module_state.module.syntax.comments,
                                        origin_module_declaration_node.range,
                                        Some(elm_syntax_node_as_ref_map(origin_module_declaration_name_node, Box::as_ref)),
                                        origin_module_declaration
                                            .documentation
                                            .as_ref()
                                            .map(|node| node.value.as_ref()),
                                        origin_module_declaration_parameters,
                                        type_.as_ref().map(elm_syntax_node_as_ref)
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
                            if origin_module_declaration_name_node.value.as_ref()== hovered_name {
                                Some(present_variable_declaration_info_markdown(
                                    &origin_module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    elm_syntax_node_as_ref_map(origin_module_declaration_name_node, Box::as_ref),
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_maybe_signature
                                        .as_ref()
                                        .and_then(|signature|
                                            signature
                                                .type_
                                                .as_ref()
                                        ).map(elm_syntax_node_as_ref),
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
                ElmQualified {
                    qualification: hovered_qualification,
                    name: hovered_name,
                },
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
                |origin_module_declaration_or_err| {
                    let origin_module_declaration =
                        origin_module_declaration_or_err.as_ref().ok()?;
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: maybe_origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0_name: maybe_origin_module_declaration_variant0_name_node,
                            variant0_values: maybe_origin_module_declaration_variant0_values,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && origin_module_declaration_name_node.value.as_ref()
                                    == hovered_name
                            {
                                Some(present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        origin_module_declaration_name_node,
                                        Box::as_ref,
                                    )),
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    maybe_origin_module_declaration_variant0_name_node
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    maybe_origin_module_declaration_variant0_values,
                                    origin_module_declaration_variant1_up,
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
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && origin_module_declaration_name_node.value.as_ref()
                                    == hovered_name
                            {
                                Some(present_type_alias_declaration_info_markdown(
                                    &module_origin_lookup,
                                    hovered_module_origin,
                                    &hovered_project_module_state.module.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        origin_module_declaration_name_node,
                                        Box::as_ref,
                                    )),
                                    origin_module_declaration
                                        .documentation
                                        .as_ref()
                                        .map(|node| node.value.as_ref()),
                                    origin_module_declaration_parameters,
                                    type_.as_ref().map(elm_syntax_node_as_ref),
                                ))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator { .. }
                        | ElmSyntaxDeclaration::Port { .. }
                        | ElmSyntaxDeclaration::Variable { .. } => None,
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
    project_state: &ProjectState,
    module_syntax: &ElmSyntaxModule,
    binding_name: &str,
    binding_origin: LocalBindingOrigin,
) -> String {
    match binding_origin {
        LocalBindingOrigin::PatternVariable(_) => "variable introduced in pattern".to_string(),
        LocalBindingOrigin::PatternRecordField(_) => {
            "variable bound to a field, introduced in a pattern".to_string()
        }
        LocalBindingOrigin::LetDeclaredVariable {
            signature: maybe_signature,
            start_name_range,
        } => let_declaration_info_markdown(
            state,
            project_state,
            module_syntax,
            ElmSyntaxNode {
                value: binding_name,
                range: start_name_range,
            },
            maybe_signature
                .and_then(|signature| signature.type_.as_ref())
                .map(elm_syntax_node_as_ref),
        ),
    }
}
fn let_declaration_info_markdown(
    state: &State,
    project_state: &ProjectState,
    module_syntax: &ElmSyntaxModule,
    start_name_node: ElmSyntaxNode<&str>,
    maybe_signature_type: Option<ElmSyntaxNode<&ElmSyntaxType>>,
) -> String {
    match maybe_signature_type {
        None => {
            format!("```elm\nlet {}\n```\n", start_name_node.value)
        }
        Some(hovered_local_binding_signature) => {
            let signature_type_internal_comments = elm_syntax_comments_in_range(
                &module_syntax.comments,
                hovered_local_binding_signature.range,
            );
            format!(
                "```elm\nlet {} :{}{}\n```\n",
                start_name_node.value,
                match elm_syntax_range_line_span(
                    lsp_types::Range {
                        start: start_name_node.range.end,
                        end: hovered_local_binding_signature.range.end
                    },
                    signature_type_internal_comments
                ) {
                    LineSpan::Single => " ",
                    LineSpan::Multiple => "\n    ",
                },
                &elm_syntax_type_to_string(
                    &elm_syntax_module_create_origin_lookup(state, project_state, module_syntax),
                    hovered_local_binding_signature,
                    4,
                    signature_type_internal_comments
                )
            )
        }
    }
}

fn respond_to_goto_definition(
    state: &State,
    goto_definition_arguments: lsp_types::GotoDefinitionParams,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let goto_symbol_project_module_state = state_get_project_module_by_lsp_url(
        state,
        &goto_definition_arguments
            .text_document_position_params
            .text_document
            .uri,
    )?;
    let goto_symbol_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &goto_symbol_project_module_state.module.syntax,
            goto_definition_arguments
                .text_document_position_params
                .position,
        )?;
    match goto_symbol_node.value {
        ElmSyntaxSymbol::LetDeclarationName { .. }
        | ElmSyntaxSymbol::ModuleMemberDeclarationName { .. } => {
            // already at definition
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
                            origin_choice_type_parameter.value.as_ref() == goto_type_variable_name
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
                            origin_choice_type_parameter.value.as_ref() == goto_type_variable_name
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
                ElmSyntaxDeclaration::Variable { .. }
                | ElmSyntaxDeclaration::Operator { .. }
                | ElmSyntaxDeclaration::Port { .. } => None,
            }
        }
        ElmSyntaxSymbol::ModuleName(goto_module_name) => {
            if let Some(goto_symbol_module_header) =
                &goto_symbol_project_module_state.module.syntax.header
                && let Some(goto_symbol_module_name_node) = &goto_symbol_module_header.module_name
                && goto_symbol_module_name_node.value.as_ref() == goto_module_name
            {
                return None;
            }
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_symbol_project_module_state.project,
                    goto_module_name,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: match &origin_module_state.syntax.header {
                        Some(module_header) => match &module_header.module_name {
                            Some(module_name_node) => module_name_node.range,
                            None => match module_header.specific {
                                ElmSyntaxModuleHeaderSpecific::Pure {
                                    module_keyword_range,
                                }
                                | ElmSyntaxModuleHeaderSpecific::Port {
                                    port_keyword_range: _,
                                    module_keyword_range,
                                }
                                | ElmSyntaxModuleHeaderSpecific::Effect {
                                    module_keyword_range,
                                    ..
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
        ElmSyntaxSymbol::ImportAlias {
            module_origin: goto_module_name,
            alias_name: _,
        } => {
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_symbol_project_module_state.project,
                    goto_module_name,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: match &origin_module_state.syntax.header {
                        Some(module_header) => match &module_header.module_name {
                            Some(module_name_node) => module_name_node.range,
                            None => match module_header.specific {
                                ElmSyntaxModuleHeaderSpecific::Pure {
                                    module_keyword_range,
                                }
                                | ElmSyntaxModuleHeaderSpecific::Port {
                                    port_keyword_range: _,
                                    module_keyword_range,
                                }
                                | ElmSyntaxModuleHeaderSpecific::Effect {
                                    module_keyword_range,
                                    ..
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
        }
        | ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name: goto_name,
            module_documentation: _,
        } => {
            let declaration_name_range: lsp_types::Range = goto_symbol_project_module_state
                .module
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration_or_err| {
                    let origin_module_declaration =
                        origin_module_declaration_or_err.as_ref().ok()?;
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: maybe_origin_module_choice_type_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_choice_type_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                                && origin_module_declaration_function_node.value.as_ref()
                                    == goto_name
                            {
                                Some(origin_module_declaration_function_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_origin_module_port_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_port_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                            if origin_module_declaration_name_node.value.as_ref() == goto_name {
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
                    goto_symbol_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range = origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration_or_err| {
                    let origin_module_declaration =
                        origin_module_declaration_or_err.as_ref().ok()?;
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: maybe_origin_module_choice_type_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_choice_type_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            operator: maybe_origin_module_operator,
                            ..
                        } => {
                            if let Some(origin_module_declaration_operator_node) =
                                maybe_origin_module_operator
                                && origin_module_declaration_operator_node.value == goto_name
                            {
                                Some(origin_module_declaration_operator_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: maybe_origin_module_port_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_port_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            name: maybe_origin_module_type_alias_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_type_alias_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                            if origin_module_declaration_name_node.value.as_ref() == goto_name {
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
                            LocalBindingOrigin::PatternVariable(range)
                            | LocalBindingOrigin::PatternRecordField(range) => range,
                            LocalBindingOrigin::LetDeclaredVariable {
                                signature: _,
                                start_name_range,
                            } => start_name_range,
                        },
                    },
                ));
            }
            let goto_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    goto_symbol_project_module_state.project,
                    &goto_symbol_project_module_state.module.syntax,
                ),
                ElmQualified {
                    qualification: goto_qualification,
                    name: goto_name,
                },
            );
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_symbol_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range = origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration_or_err| {
                    let origin_module_declaration =
                        origin_module_declaration_or_err.as_ref().ok()?;
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
                                && origin_module_declaration_variant0_name_node.value.as_ref()
                                    == goto_name
                            {
                                Some(origin_module_declaration_variant0_name_node.range)
                            } else {
                                origin_module_declaration_variant1_up
                                    .iter()
                                    .find_map(|variant| {
                                        variant.name.as_ref().and_then(|variant_name_node| {
                                            if variant_name_node.value.as_ref() == goto_name {
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
                                && origin_module_declaration_function_node.value.as_ref()
                                    == goto_name
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
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
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
                            if origin_module_declaration_name_node.value.as_ref() == goto_name {
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
                    goto_symbol_project_module_state.project,
                    &goto_symbol_project_module_state.module.syntax,
                ),
                ElmQualified {
                    qualification: goto_qualification,
                    name: goto_name,
                },
            );
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    goto_symbol_project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_file_url: lsp_types::Url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            let declaration_name_range: lsp_types::Range = origin_module_state
                .syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration_or_err| {
                    let origin_module_declaration =
                        origin_module_declaration_or_err.as_ref().ok()?;
                    let origin_module_declaration_node =
                        origin_module_declaration.declaration.as_ref()?;
                    match &origin_module_declaration_node.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: maybe_origin_module_declaration_name,
                            ..
                        }
                        | ElmSyntaxDeclaration::TypeAlias {
                            name: maybe_origin_module_declaration_name,
                            ..
                        } => {
                            if let Some(origin_module_declaration_name_node) =
                                maybe_origin_module_declaration_name
                                && origin_module_declaration_name_node.value.as_ref() == goto_name
                            {
                                Some(origin_module_declaration_name_node.range)
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator { .. }
                        | ElmSyntaxDeclaration::Port { .. }
                        | ElmSyntaxDeclaration::Variable { .. } => None,
                    }
                })?;
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
    prepare_rename_arguments: &lsp_types::TextDocumentPositionParams,
) -> Option<Result<lsp_types::PrepareRenameResponse, lsp_server::ResponseError>> {
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
            alias_name,
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
        }
        | ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name,
            module_documentation: _,
        }
        | ElmSyntaxSymbol::ImportExpose {
            name,
            origin_module: _,
            all_exposes: _,
        }
        | ElmSyntaxSymbol::ModuleMemberDeclarationName {
            name,
            declaration: _,
            documentation: _,
        }
        | ElmSyntaxSymbol::LetDeclarationName {
            name,
            signature_type: _,
            start_name_range: _,
            scope_expression: _,
        }
        | ElmSyntaxSymbol::TypeVariable {
            scope_declaration: _,
            name,
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
                    LocalBindingOrigin::PatternRecordField(_) => Err(lsp_server::ResponseError {
                        code: lsp_server::ErrorCode::RequestFailed as i32,
                        message: "cannot rename a variable that is bound to a field name"
                            .to_string(),
                        data: None,
                    }),
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
            let mut all_uses_of_renamed_import_alias: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_module_uses_of_reference_into(
                &mut all_uses_of_renamed_import_alias,
                state,
                to_rename_project_module_state.project,
                &to_rename_project_module_state.module.syntax,
                ElmSymbolToReference::ImportAlias {
                    module_origin: import_alias_to_rename_module_origin,
                    alias_name: import_alias_to_rename,
                },
            );
            vec![lsp_types::TextDocumentEdit {
                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: rename_arguments.text_document_position.text_document.uri,
                    version: None,
                },
                edits: all_uses_of_renamed_import_alias
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
            let mut all_uses_of_renamed_type_variable: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_declaration_uses_of_reference_into(
                &mut all_uses_of_renamed_type_variable,
                to_rename_project_module_state
                    .module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|node| node.value.as_ref())
                    .unwrap_or(""),
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_rename_project_module_state.project,
                    &to_rename_project_module_state.module.syntax,
                ),
                scope_declaration,
                ElmSymbolToReference::TypeVariable(type_variable_to_rename),
            );
            vec![lsp_types::TextDocumentEdit {
                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: rename_arguments.text_document_position.text_document.uri,
                    version: None,
                },
                edits: all_uses_of_renamed_type_variable
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
                    ElmSymbolToReference::ModuleName(module_name_to_rename),
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
        }
        | ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name: to_rename_declaration_name,
            module_documentation: _,
        } => {
            let to_rename_module_origin: &str = to_rename_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            let elm_declared_symbol_to_rename: ElmSymbolToReference = if to_rename_declaration_name
                .starts_with(char::is_uppercase)
            {
                let to_rename_is_record_type_alias: bool = to_rename_project_module_state
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                    .filter_map(|documented_declaration| {
                        documented_declaration.declaration.as_ref()
                    })
                    .any(|declaration_node| match &declaration_node.value {
                        ElmSyntaxDeclaration::TypeAlias {
                            type_:
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxType::Record(_),
                                    range: _,
                                }),
                            name: Some(record_type_alias_name_node),
                            ..
                        } => {
                            record_type_alias_name_node.value.as_ref() == to_rename_declaration_name
                        }
                        _ => false,
                    });
                if to_rename_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_rename_module_origin,
                        name: to_rename_declaration_name,
                        including_declaration_name: true,
                    }
                } else {
                    ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: to_rename_module_origin,
                        name: to_rename_declaration_name,
                        including_declaration_name: true,
                    }
                }
            } else {
                ElmSymbolToReference::VariableOrVariant {
                    module_origin: to_rename_module_origin,
                    name: to_rename_declaration_name,
                    including_declaration_name: true,
                }
            };
            state_iter_all_modules(state)
                .filter_map(move |project_module| {
                    let mut all_uses_of_at_docs_module_member: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_at_docs_module_member,
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
                        edits: all_uses_of_at_docs_module_member
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
            let elm_declared_symbol_to_rename: ElmSymbolToReference =
                if to_rename_import_expose_name.starts_with(char::is_uppercase) {
                    let to_rename_is_record_type_alias: bool = to_rename_project_module_state
                        .module
                        .syntax
                        .declarations
                        .iter()
                        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                        record_type_alias_name_node.value.as_ref()
                                            == to_rename_import_expose_name
                                    }
                                    _ => false,
                                },
                            )
                        });
                    if to_rename_is_record_type_alias {
                        ElmSymbolToReference::RecordTypeAlias {
                            module_origin: to_rename_import_expose_origin_module,
                            name: to_rename_import_expose_name,
                            including_declaration_name: true,
                        }
                    } else {
                        ElmSymbolToReference::TypeNotRecordAlias {
                            module_origin: to_rename_import_expose_origin_module,
                            name: to_rename_import_expose_name,
                            including_declaration_name: true,
                        }
                    }
                } else {
                    ElmSymbolToReference::VariableOrVariant {
                        module_origin: to_rename_import_expose_origin_module,
                        name: to_rename_import_expose_name,
                        including_declaration_name: true,
                    }
                };
            state_iter_all_modules(state)
                .filter_map(move |project_module| {
                    let mut all_uses_import_exposed_member: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_import_exposed_member,
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
                        edits: all_uses_import_exposed_member
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
        ElmSyntaxSymbol::LetDeclarationName {
            name: to_rename_name,
            start_name_range,
            signature_type: _,
            scope_expression,
        } => {
            let mut all_uses_of_let_declaration_to_rename: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_expression_uses_of_reference_into(
                &mut all_uses_of_let_declaration_to_rename,
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_rename_project_module_state.project,
                    &to_rename_project_module_state.module.syntax,
                ),
                &[ElmLocalBinding {
                    name: to_rename_name,
                    origin: LocalBindingOrigin::LetDeclaredVariable {
                        signature: None, // irrelevant fir finding uses
                        start_name_range: start_name_range,
                    },
                }],
                scope_expression,
                ElmSymbolToReference::LocalBinding {
                    name: to_rename_name,
                    including_let_declaration_name: true,
                },
            );
            vec![lsp_types::TextDocumentEdit {
                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: rename_arguments.text_document_position.text_document.uri,
                    version: None,
                },
                edits: all_uses_of_let_declaration_to_rename
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
                    ElmSymbolToReference::LocalBinding {
                        name: to_rename_name,
                        including_let_declaration_name: true,
                    },
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
                    ElmQualified {
                        qualification: to_rename_qualification,
                        name: to_rename_name,
                    },
                );
                let to_rename_is_record_type_alias: bool = project_state_get_module_with_name(
                    state,
                    to_rename_project_module_state.project,
                    to_rename_module_origin,
                )
                .is_some_and(|(_, to_find_origin_module_state)| {
                    to_find_origin_module_state
                        .syntax
                        .declarations
                        .iter()
                        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                        record_type_alias_name_node.value.as_ref() == to_rename_name
                                    }
                                    _ => false,
                                },
                            )
                        })
                });
                let symbol_to_find: ElmSymbolToReference = if to_rename_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_rename_module_origin,
                        name: to_rename_name,
                        including_declaration_name: true,
                    }
                } else {
                    ElmSymbolToReference::VariableOrVariant {
                        module_origin: to_rename_module_origin,
                        name: to_rename_name,
                        including_declaration_name: true,
                    }
                };
                state_iter_all_modules(state)
                    .filter_map(|project_module| {
                        let mut all_uses_of_renamed_reference: Vec<lsp_types::Range> = Vec::new();
                        elm_syntax_module_uses_of_reference_into(
                            &mut all_uses_of_renamed_reference,
                            state,
                            project_module.project_state,
                            &project_module.module_state.syntax,
                            symbol_to_find,
                        );
                        let elm_module_uri: lsp_types::Url =
                            lsp_types::Url::from_file_path(project_module.module_path).ok()?;
                        Some(lsp_types::TextDocumentEdit {
                            text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                uri: elm_module_uri,
                                version: None,
                            },
                            edits: all_uses_of_renamed_reference
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
                ElmQualified {
                    qualification: to_rename_qualification,
                    name: type_name_to_rename,
                },
            );
            let to_rename_is_record_type_alias: bool = project_state_get_module_with_name(
                state,
                to_rename_project_module_state.project,
                to_rename_module_origin,
            )
            .is_some_and(|(_, to_rename_module_state)| {
                to_rename_module_state
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                    record_type_alias_name_node.value.as_ref()
                                        == type_name_to_rename
                                }
                                _ => false,
                            },
                        )
                    })
            });
            let elm_declared_symbol_to_rename: ElmSymbolToReference =
                if to_rename_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_rename_module_origin,
                        name: type_name_to_rename,
                        including_declaration_name: true,
                    }
                } else {
                    ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: to_rename_module_origin,
                        name: type_name_to_rename,
                        including_declaration_name: true,
                    }
                };
            state_iter_all_modules(state)
                .filter_map(|project_module| {
                    let mut all_uses_of_renamed_type: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_renamed_type,
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
                        edits: all_uses_of_renamed_type
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
fn respond_to_references(
    state: &State,
    references_arguments: lsp_types::ReferenceParams,
) -> Option<Vec<lsp_types::Location>> {
    let to_find_project_module_state = state_get_project_module_by_lsp_url(
        state,
        &references_arguments
            .text_document_position
            .text_document
            .uri,
    )?;
    let symbol_to_find_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &to_find_project_module_state.module.syntax,
            references_arguments.text_document_position.position,
        )?;
    Some(match symbol_to_find_node.value {
        ElmSyntaxSymbol::ImportAlias {
            module_origin: import_alias_to_find_module_origin,
            alias_name: import_alias_to_find,
        } => {
            let mut all_uses_of_found_import_alias: Vec<lsp_types::Range> =
                if references_arguments.context.include_declaration {
                    vec![symbol_to_find_node.range] // the alias on the import itself
                } else {
                    Vec::new()
                };
            let symbol_to_find: ElmSymbolToReference = ElmSymbolToReference::ImportAlias {
                module_origin: import_alias_to_find_module_origin,
                alias_name: import_alias_to_find,
            };
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                to_find_project_module_state.project,
                &to_find_project_module_state.module.syntax,
            );
            let to_find_module_name: &str = to_find_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            for documented_declaration in to_find_project_module_state
                .module
                .syntax
                .declarations
                .iter()
                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
            {
                if let Some(declaration_node) = &documented_declaration.declaration {
                    elm_syntax_declaration_uses_of_reference_into(
                        &mut all_uses_of_found_import_alias,
                        to_find_module_name,
                        &module_origin_lookup,
                        &declaration_node.value,
                        symbol_to_find,
                    );
                }
            }
            all_uses_of_found_import_alias
                .into_iter()
                .map(|use_range_of_found_module| lsp_types::Location {
                    uri: references_arguments
                        .text_document_position
                        .text_document
                        .uri
                        .clone(),
                    range: use_range_of_found_module,
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration,
            name: type_variable_to_find,
        } => {
            let mut all_uses_of_found_type_variable: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_declaration_uses_of_reference_into(
                &mut all_uses_of_found_type_variable,
                to_find_project_module_state
                    .module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|node| node.value.as_ref())
                    .unwrap_or(""),
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_find_project_module_state.project,
                    &to_find_project_module_state.module.syntax,
                ),
                scope_declaration,
                ElmSymbolToReference::TypeVariable(type_variable_to_find),
            );
            all_uses_of_found_type_variable
                .into_iter()
                .map(|use_range_of_found_module| lsp_types::Location {
                    uri: references_arguments
                        .text_document_position
                        .text_document
                        .uri
                        .clone(),
                    range: use_range_of_found_module,
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::ModuleName(module_name_to_find) => to_find_project_module_state
            .project
            .modules
            .iter()
            .filter(|(_, project_module)| {
                if references_arguments.context.include_declaration
                    && let Some(project_module_header) = project_module.syntax.header.as_ref()
                    && let Some(project_module_name_node) =
                        project_module_header.module_name.as_ref()
                {
                    project_module_name_node.value.as_ref() != module_name_to_find
                } else {
                    true
                }
            })
            .flat_map(|(elm_module_file_path, elm_module_state)| {
                let mut all_uses_of_found_module_name: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_module_uses_of_reference_into(
                    &mut all_uses_of_found_module_name,
                    state,
                    to_find_project_module_state.project,
                    &elm_module_state.syntax,
                    ElmSymbolToReference::ModuleName(module_name_to_find),
                );
                lsp_types::Url::from_file_path(elm_module_file_path)
                    .ok()
                    .map(|elm_module_uri| {
                        all_uses_of_found_module_name.into_iter().map(
                            move |use_range_of_found_module| lsp_types::Location {
                                uri: elm_module_uri.clone(),
                                range: use_range_of_found_module,
                            },
                        )
                    })
                    .into_iter()
                    .flatten()
            })
            .collect::<Vec<_>>(),
        ElmSyntaxSymbol::ModuleHeaderExpose {
            name: to_find_name,
            all_exposes: _,
        }
        | ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name: to_find_name,
            module_documentation: _,
        } => {
            let to_find_module_origin: &str = to_find_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            let elm_declared_symbol_to_find: ElmSymbolToReference = if to_find_name
                .starts_with(char::is_uppercase)
            {
                let to_find_is_record_type_alias: bool = to_find_project_module_state
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                } => record_type_alias_name_node.value.as_ref() == to_find_name,
                                _ => false,
                            },
                        )
                    });
                if to_find_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                } else {
                    ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                }
            } else {
                ElmSymbolToReference::VariableOrVariant {
                    module_origin: to_find_module_origin,
                    name: to_find_name,
                    including_declaration_name: references_arguments.context.include_declaration,
                }
            };
            to_find_project_module_state
                .project
                .modules
                .iter()
                .flat_map(move |(project_module_path, project_module_state)| {
                    lsp_types::Url::from_file_path(project_module_path)
                        .ok()
                        .map(|elm_module_uri| {
                            let mut all_uses_of_found_at_docs_module_member: Vec<lsp_types::Range> =
                                Vec::new();
                            elm_syntax_module_uses_of_reference_into(
                                &mut all_uses_of_found_at_docs_module_member,
                                state,
                                to_find_project_module_state.project,
                                &project_module_state.syntax,
                                elm_declared_symbol_to_find,
                            );
                            all_uses_of_found_at_docs_module_member.into_iter().map(
                                move |use_range_of_found_module| lsp_types::Location {
                                    uri: elm_module_uri.clone(),
                                    range: use_range_of_found_module,
                                },
                            )
                        })
                        .into_iter()
                        .flatten()
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::ModuleMemberDeclarationName {
            name: to_find_name,
            documentation: _,
            declaration: _,
        } => {
            let to_find_module_origin: &str = to_find_project_module_state
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            let elm_declared_symbol_to_find: ElmSymbolToReference = if to_find_name
                .starts_with(char::is_uppercase)
            {
                let to_find_is_record_type_alias: bool = to_find_project_module_state
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                } => record_type_alias_name_node.value.as_ref() == to_find_name,
                                _ => false,
                            },
                        )
                    });
                if to_find_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                } else {
                    ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                }
            } else {
                ElmSymbolToReference::VariableOrVariant {
                    module_origin: to_find_module_origin,
                    name: to_find_name,
                    including_declaration_name: references_arguments.context.include_declaration,
                }
            };
            to_find_project_module_state
                .project
                .modules
                .iter()
                .flat_map(move |(project_module_path, project_module_state)| {
                    let mut all_uses_of_found_module_member: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_found_module_member,
                        state,
                        to_find_project_module_state.project,
                        &project_module_state.syntax,
                        elm_declared_symbol_to_find,
                    );
                    lsp_types::Url::from_file_path(project_module_path)
                        .ok()
                        .map(|elm_module_uri| {
                            all_uses_of_found_module_member.into_iter().map(
                                move |use_range_of_found_module| lsp_types::Location {
                                    uri: elm_module_uri.clone(),
                                    range: use_range_of_found_module,
                                },
                            )
                        })
                        .into_iter()
                        .flatten()
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::ImportExpose {
            origin_module: to_find_import_expose_origin_module,
            name: to_find_import_expose_name,
            all_exposes: _,
        } => {
            let elm_declared_symbol_to_find: ElmSymbolToReference = if to_find_import_expose_name
                .starts_with(char::is_uppercase)
            {
                let to_find_is_record_type_alias: bool = to_find_project_module_state
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                    record_type_alias_name_node.value.as_ref()
                                        == to_find_import_expose_name
                                }
                                _ => false,
                            },
                        )
                    });
                if to_find_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_find_import_expose_origin_module,
                        name: to_find_import_expose_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                } else {
                    ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: to_find_import_expose_origin_module,
                        name: to_find_import_expose_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                }
            } else {
                ElmSymbolToReference::VariableOrVariant {
                    module_origin: to_find_import_expose_origin_module,
                    name: to_find_import_expose_name,
                    including_declaration_name: references_arguments.context.include_declaration,
                }
            };
            to_find_project_module_state
                .project
                .modules
                .iter()
                .flat_map(|(project_module_path, project_module_state)| {
                    let mut all_uses_of_found_import_exposed_member: Vec<lsp_types::Range> =
                        Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_found_import_exposed_member,
                        state,
                        to_find_project_module_state.project,
                        &project_module_state.syntax,
                        elm_declared_symbol_to_find,
                    );
                    lsp_types::Url::from_file_path(project_module_path)
                        .ok()
                        .map(|elm_module_uri| {
                            all_uses_of_found_import_exposed_member.into_iter().map(
                                move |use_range_of_found_module| lsp_types::Location {
                                    uri: elm_module_uri.clone(),
                                    range: use_range_of_found_module,
                                },
                            )
                        })
                        .into_iter()
                        .flatten()
                })
                .chain(if references_arguments.context.include_declaration {
                    Some(lsp_types::Location {
                        uri: references_arguments
                            .text_document_position
                            .text_document
                            .uri,
                        range: symbol_to_find_node.range,
                    })
                } else {
                    None
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::LetDeclarationName {
            name: to_find_name,
            start_name_range,
            signature_type: _,
            scope_expression,
        } => {
            let mut all_uses_of_found_let_declaration: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_expression_uses_of_reference_into(
                &mut all_uses_of_found_let_declaration,
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_find_project_module_state.project,
                    &to_find_project_module_state.module.syntax,
                ),
                &[ElmLocalBinding {
                    name: to_find_name,
                    origin: LocalBindingOrigin::LetDeclaredVariable {
                        signature: None, // irrelevant for finding uses
                        start_name_range: start_name_range,
                    },
                }],
                scope_expression,
                ElmSymbolToReference::LocalBinding {
                    name: to_find_name,
                    including_let_declaration_name: references_arguments
                        .context
                        .include_declaration,
                },
            );
            all_uses_of_found_let_declaration
                .into_iter()
                .map(|use_range_of_found_module| lsp_types::Location {
                    uri: references_arguments
                        .text_document_position
                        .text_document
                        .uri
                        .clone(),
                    range: use_range_of_found_module,
                })
                .collect::<Vec<_>>()
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification: to_find_qualification,
            name: to_find_name,
            local_bindings,
        } => {
            if to_find_qualification.is_empty()
                && let Some((to_find_local_binding_origin, local_binding_to_find_scope_expression)) =
                    find_local_binding_scope_expression(&local_bindings, to_find_name)
            {
                let mut all_uses_of_found_local_binding: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_expression_uses_of_reference_into(
                    &mut all_uses_of_found_local_binding,
                    &elm_syntax_module_create_origin_lookup(
                        state,
                        to_find_project_module_state.project,
                        &to_find_project_module_state.module.syntax,
                    ),
                    &[ElmLocalBinding {
                        name: to_find_name,
                        origin: to_find_local_binding_origin,
                    }],
                    local_binding_to_find_scope_expression,
                    ElmSymbolToReference::LocalBinding {
                        name: to_find_name,
                        including_let_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    },
                );
                if references_arguments.context.include_declaration {
                    match to_find_local_binding_origin {
                        LocalBindingOrigin::PatternVariable(range) => {
                            all_uses_of_found_local_binding.push(range);
                        }
                        LocalBindingOrigin::PatternRecordField(range) => {
                            all_uses_of_found_local_binding.push(range);
                        }
                        LocalBindingOrigin::LetDeclaredVariable { .. } => {
                            // already included in scope
                        }
                    }
                }
                all_uses_of_found_local_binding
                    .into_iter()
                    .map(|use_range_of_found_module| lsp_types::Location {
                        uri: references_arguments
                            .text_document_position
                            .text_document
                            .uri
                            .clone(),
                        range: use_range_of_found_module,
                    })
                    .collect::<Vec<_>>()
            } else {
                let to_find_module_origin: &str = look_up_origin_module(
                    &elm_syntax_module_create_origin_lookup(
                        state,
                        to_find_project_module_state.project,
                        &to_find_project_module_state.module.syntax,
                    ),
                    ElmQualified {
                        qualification: to_find_qualification,
                        name: to_find_name,
                    },
                );
                let to_find_is_record_type_alias: bool = project_state_get_module_with_name(
                    state,
                    to_find_project_module_state.project,
                    to_find_module_origin,
                )
                .is_some_and(|(_, to_find_origin_module_state)| {
                    to_find_origin_module_state
                        .syntax
                        .declarations
                        .iter()
                        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                    } => record_type_alias_name_node.value.as_ref() == to_find_name,
                                    _ => false,
                                },
                            )
                        })
                });
                let symbol_to_find: ElmSymbolToReference = if to_find_is_record_type_alias {
                    ElmSymbolToReference::RecordTypeAlias {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                } else {
                    ElmSymbolToReference::VariableOrVariant {
                        module_origin: to_find_module_origin,
                        name: to_find_name,
                        including_declaration_name: references_arguments
                            .context
                            .include_declaration,
                    }
                };
                to_find_project_module_state
                    .project
                    .modules
                    .iter()
                    .flat_map(|(project_module_path, project_module_state)| {
                        let mut all_uses_of_found_reference: Vec<lsp_types::Range> = Vec::new();
                        elm_syntax_module_uses_of_reference_into(
                            &mut all_uses_of_found_reference,
                            state,
                            to_find_project_module_state.project,
                            &project_module_state.syntax,
                            symbol_to_find,
                        );
                        lsp_types::Url::from_file_path(project_module_path)
                            .ok()
                            .map(|elm_module_uri| {
                                all_uses_of_found_reference.into_iter().map(
                                    move |use_range_of_found_module| lsp_types::Location {
                                        uri: elm_module_uri.clone(),
                                        range: use_range_of_found_module,
                                    },
                                )
                            })
                            .into_iter()
                            .flatten()
                    })
                    .collect::<Vec<_>>()
            }
        }
        ElmSyntaxSymbol::Type {
            qualification: to_find_qualification,
            name: type_name_to_find,
        } => {
            let to_find_module_origin: &str = look_up_origin_module(
                &elm_syntax_module_create_origin_lookup(
                    state,
                    to_find_project_module_state.project,
                    &to_find_project_module_state.module.syntax,
                ),
                ElmQualified {
                    qualification: to_find_qualification,
                    name: type_name_to_find,
                },
            );
            let to_find_is_record_type_alias: bool = project_state_get_module_with_name(
                state,
                to_find_project_module_state.project,
                to_find_module_origin,
            )
            .is_some_and(|(_, to_find_module_state)| {
                to_find_module_state
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                    record_type_alias_name_node.value.as_ref() == type_name_to_find
                                }
                                _ => false,
                            },
                        )
                    })
            });
            let elm_declared_symbol_to_find: ElmSymbolToReference = if to_find_is_record_type_alias
            {
                ElmSymbolToReference::RecordTypeAlias {
                    module_origin: to_find_module_origin,
                    name: type_name_to_find,
                    including_declaration_name: references_arguments.context.include_declaration,
                }
            } else {
                ElmSymbolToReference::TypeNotRecordAlias {
                    module_origin: to_find_module_origin,
                    name: type_name_to_find,
                    including_declaration_name: references_arguments.context.include_declaration,
                }
            };
            to_find_project_module_state
                .project
                .modules
                .iter()
                .flat_map(|(project_module_path, project_module_state)| {
                    let mut all_uses_of_found_type: Vec<lsp_types::Range> = Vec::new();
                    elm_syntax_module_uses_of_reference_into(
                        &mut all_uses_of_found_type,
                        state,
                        to_find_project_module_state.project,
                        &project_module_state.syntax,
                        elm_declared_symbol_to_find,
                    );
                    lsp_types::Url::from_file_path(project_module_path)
                        .ok()
                        .map(|elm_module_uri| {
                            all_uses_of_found_type.into_iter().map(
                                move |use_range_of_found_module| lsp_types::Location {
                                    uri: elm_module_uri.clone(),
                                    range: use_range_of_found_module,
                                },
                            )
                        })
                        .into_iter()
                        .flatten()
                })
                .collect::<Vec<_>>()
        }
    })
}

fn respond_to_semantic_tokens_full(
    state: &State,
    semantic_tokens_arguments: &lsp_types::SemanticTokensParams,
) -> Option<lsp_types::SemanticTokensResult> {
    let project_module_state =
        state_get_project_module_by_lsp_url(state, &semantic_tokens_arguments.text_document.uri)?;
    let mut highlighting: Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>> =
        Vec::with_capacity(project_module_state.module.source.len() / 16);
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
                                    delta_line: delta.line,
                                    delta_start: delta.character,
                                    length: segment.range.end.character
                                        - segment.range.start.character,
                                    token_type: semantic_token_type_to_id(
                                        &elm_syntax_highlight_kind_to_lsp_semantic_token_type(
                                            &segment.value,
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

fn semantic_token_type_to_id(semantic_token: &lsp_types::SemanticTokenType) -> u32 {
    token_types
        .iter()
        .enumerate()
        .find_map(|(i, token)| {
            if token == semantic_token {
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
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    start_name_node: ElmSyntaxNode<&str>,
    maybe_documentation: Option<&str>,
    maybe_signature_type: Option<ElmSyntaxNode<&ElmSyntaxType>>,
) -> String {
    let description: String = match maybe_signature_type {
        Some(signature_type_node) => {
            let type_internal_comments =
                elm_syntax_comments_in_range(comments, signature_type_node.range);
            format!(
                "```elm\n{}.{} :{}{}\n```\n",
                module_origin,
                start_name_node.value,
                match elm_syntax_range_line_span(
                    lsp_types::Range {
                        start: start_name_node.range.end,
                        end: signature_type_node.range.start
                    },
                    type_internal_comments
                ) {
                    LineSpan::Single => " ",
                    LineSpan::Multiple => "\n    ",
                },
                &elm_syntax_type_to_string(
                    module_origin_lookup,
                    signature_type_node,
                    4,
                    type_internal_comments
                )
            )
        }
        None => format!(
            "```elm\n{}.{}\n```\n",
            &module_origin, &start_name_node.value
        ),
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
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    maybe_documentation: Option<&str>,
    maybe_type: Option<ElmSyntaxNode<&ElmSyntaxType>>,
) -> String {
    let mut declaration_as_string: String = String::new();
    let maybe_fully_qualified_name: Option<ElmSyntaxNode<String>> = maybe_name
        .map(|name_node| elm_syntax_node_map(name_node, |name| format!("{module_origin}.{name}")));
    elm_syntax_port_declaration_into(
        &mut declaration_as_string,
        comments,
        |qualified| look_up_origin_module(module_origin_lookup, qualified),
        declaration_range,
        maybe_fully_qualified_name
            .as_ref()
            .map(|name_node| elm_syntax_node_as_ref_map(name_node, String::as_str)),
        maybe_type,
    );
    let description: String = format!("```elm\n{}\n```\n", declaration_as_string);
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
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    maybe_documentation: Option<&str>,
    parameters: &[ElmSyntaxNode<Box<str>>],
    maybe_type: Option<ElmSyntaxNode<&ElmSyntaxType>>,
) -> String {
    let mut declaration_as_string: String = String::new();
    let maybe_fully_qualified_name: Option<ElmSyntaxNode<String>> = maybe_name
        .map(|name_node| elm_syntax_node_map(name_node, |name| format!("{module_origin}.{name}")));
    elm_syntax_type_alias_declaration_into(
        &mut declaration_as_string,
        elm_syntax_comments_in_range(comments, declaration_range),
        |qualified| look_up_origin_module(module_origin_lookup, qualified),
        declaration_range,
        maybe_fully_qualified_name
            .as_ref()
            .map(|name_node| elm_syntax_node_as_ref_map(name_node, String::as_str)),
        parameters,
        maybe_type,
    );
    let description = format!("```elm\n{}\n```\n", declaration_as_string);
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(documentation)
        }
    }
}
const list_list_type_info_markdown: &str = "```elm
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
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    maybe_documentation: Option<&str>,
    parameters: &[ElmSyntaxNode<Box<str>>],
    variant0_name: Option<ElmSyntaxNode<&str>>,
    variant0_values: &[ElmSyntaxNode<ElmSyntaxType>],
    variant1_up: &[ElmSyntaxChoiceTypeDeclarationTailingVariant],
) -> String {
    let mut declaration_string: String = String::new();
    let maybe_fully_qualified_name: Option<ElmSyntaxNode<String>> = maybe_name
        .map(|name_node| elm_syntax_node_map(name_node, |name| format!("{module_origin}.{name}")));
    elm_syntax_choice_type_declaration_into(
        &mut declaration_string,
        elm_syntax_comments_in_range(comments, declaration_range),
        |qualified| look_up_origin_module(module_origin_lookup, qualified),
        declaration_range,
        maybe_fully_qualified_name
            .as_ref()
            .map(|name_node| elm_syntax_node_as_ref_map(name_node, String::as_str)),
        parameters,
        variant0_name,
        variant0_values,
        variant1_up,
    );
    let description: String = format!("```elm\n{}\n```\n", declaration_string);
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
    maybe_direction: Option<ElmSyntaxInfixDirection>,
    precedence: Option<i64>,
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
                        match &origin_operator_function_signature.type_ {
                            None => "".to_string(),
                            Some(origin_operator_function_type) => {
                                " :".to_string()
                                    + match elm_syntax_range_line_span(
                                        origin_operator_function_type.range,
                                        &[], // no infix types have comments
                                    ) {
                                        LineSpan::Single => " ",
                                        LineSpan::Multiple => "\n    ",
                                    }
                                    + &elm_syntax_type_to_string(
                                        module_origin_lookup,
                                        elm_syntax_node_as_ref(origin_operator_function_type),
                                        4,
                                        &[], // no infix types have comments
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
    completion_arguments: &lsp_types::CompletionParams,
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
                completion_project_module.project,
                &[],
                module_name,
                completion_project_module
                    .module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|node| node.value.as_ref()),
            ))
        }
        ElmSyntaxSymbol::LetDeclarationName {
            name: _,
            start_name_range: _,
            signature_type: _,
            scope_expression,
        } => {
            match scope_expression.value {
                ElmSyntaxExpression::LetIn {
                    declarations: let_declarations,
                    in_keyword_range: _,
                    result: _,
                } => {
                    // find previous signature
                    let_declarations
                        .iter()
                        .zip(let_declarations.iter().skip(1))
                        .find_map(|(previous_declaration_node, current_declaration_node)| {
                            if let ElmSyntaxLetDeclaration::VariableDeclaration {
                                start_name: current_declaration_start_name_node,
                                signature: None,
                                ..
                            } = &current_declaration_node.value
                                && current_declaration_start_name_node.range
                                    == symbol_to_complete.range
                                && let ElmSyntaxLetDeclaration::VariableDeclaration {
                                    start_name: previous_declaration_start_name_node,
                                    signature:
                                        Some(ElmSyntaxVariableDeclarationSignature {
                                            implementation_name_range: None,
                                            ..
                                        }),
                                    ..
                                } = &previous_declaration_node.value
                            {
                                Some(vec![lsp_types::CompletionItem {
                                    label: previous_declaration_start_name_node.value.to_string(),
                                    kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                    documentation: None,
                                    ..lsp_types::CompletionItem::default()
                                }])
                            } else {
                                None
                            }
                        })
                }
                _ => None,
            }
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
                        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                        .zip(
                            completion_project_module
                                .module
                                .syntax
                                .declarations
                                .iter()
                                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                        Some(ElmSyntaxVariableDeclarationSignature {
                                            implementation_name_range: None,
                                            ..
                                        }),
                                    ..
                                } = &previous_declaration_node.value
                            {
                                Some(vec![lsp_types::CompletionItem {
                                    label: previous_declaration_start_name_node.value.to_string(),
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
                .map(|node| node.value.as_ref())
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
                        } => Some(open_choice_type_name.value.as_ref()),
                        ElmSyntaxExpose::Operator(operator_symbol) => {
                            operator_symbol.as_ref().map(|node| node.value)
                        }
                        ElmSyntaxExpose::Type(name) => Some(name.as_ref()),
                        ElmSyntaxExpose::Variable(name) => Some(name.as_ref()),
                    }?;
                    if expose_name == to_complete_header_expose_name {
                        None
                    } else {
                        Some(expose_name)
                    }
                })
                .collect::<std::collections::HashSet<_>>();
            let mut completion_items: Vec<lsp_types::CompletionItem> =
                Vec::with_capacity(completion_project_module.module.syntax.declarations.len());
            for (origin_module_declaration_node, origin_module_declaration_documentation) in
                completion_project_module
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                    .filter_map(|documented_declaration| {
                        let declaration_node = documented_declaration.declaration.as_ref()?;
                        Some((
                            declaration_node,
                            documented_declaration
                                .documentation
                                .as_ref()
                                .map(|node| node.value.as_ref()),
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
                            && !existing_expose_names.contains(choice_type_name_node.value.as_ref())
                        {
                            let info_markdown: String =
                                present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_origin,
                                    &completion_project_module.module.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        choice_type_name_node,
                                        Box::as_ref,
                                    )),
                                    origin_module_declaration_documentation,
                                    parameters,
                                    variant0_name
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    variant0_values,
                                    variant1_up,
                                );
                            completion_items.push(lsp_types::CompletionItem {
                                label: choice_type_name_node.value.to_string(),
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
                            && !existing_expose_names.contains(name_node.value.as_ref())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            type_.as_ref().map(elm_syntax_node_as_ref),
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
                        type_: maybe_type,
                    } => {
                        if let Some(name_node) = maybe_name
                            && !existing_expose_names.contains(name_node.value.as_ref())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::STRUCT),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            parameters,
                                            maybe_type.as_ref().map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                        }
                    }
                    ElmSyntaxDeclaration::Variable {
                        start_name: start_name_node,
                        signature: maybe_signature,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        if !existing_expose_names.contains(start_name_node.value.as_ref()) {
                            completion_items.push(lsp_types::CompletionItem {
                                label: start_name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            elm_syntax_node_as_ref_map(
                                                start_name_node,
                                                Box::as_ref,
                                            ),
                                            origin_module_declaration_documentation,
                                            maybe_signature
                                                .as_ref()
                                                .and_then(|signature| signature.type_.as_ref())
                                                .map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                        }
                    }
                    ElmSyntaxDeclaration::Operator { .. } => {
                        // no new operators will ever be added
                    }
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
            name: to_complete_header_expose_name,
            module_documentation,
        } => {
            let module_origin: &str = completion_project_module
                .module
                .syntax
                .header
                .as_ref()
                .and_then(|header| header.module_name.as_ref())
                .map(|node| node.value.as_ref())
                .unwrap_or("");
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                completion_project_module.project,
                &completion_project_module.module.syntax,
            );
            let existing_member_names_across_at_docs: std::collections::HashSet<&str> =
                module_documentation
                    .iter()
                    .flat_map(|module_documentation_element| {
                        match &module_documentation_element.value {
                            ElmSyntaxModuleDocumentationElement::Markdown(_) => None,
                            ElmSyntaxModuleDocumentationElement::AtDocs(at_docs_member_names) => {
                                Some(
                                    at_docs_member_names
                                        .iter()
                                        .map(|node| node.value.as_ref())
                                        .filter(|&member_name| {
                                            member_name != to_complete_header_expose_name
                                        }),
                                )
                            }
                        }
                        .into_iter()
                        .flatten()
                    })
                    .collect::<std::collections::HashSet<_>>();
            let mut completion_items: Vec<lsp_types::CompletionItem> = Vec::new();
            for (origin_module_declaration_node, origin_module_declaration_documentation) in
                completion_project_module
                    .module
                    .syntax
                    .declarations
                    .iter()
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                    .filter_map(|documented_declaration| {
                        let declaration_node = documented_declaration.declaration.as_ref()?;
                        Some((
                            declaration_node,
                            documented_declaration
                                .documentation
                                .as_ref()
                                .map(|node| node.value.as_ref()),
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
                            && !existing_member_names_across_at_docs
                                .contains(choice_type_name_node.value.as_ref())
                        {
                            let info_markdown: String =
                                present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_origin,
                                    &completion_project_module.module.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        choice_type_name_node,
                                        Box::as_ref,
                                    )),
                                    origin_module_declaration_documentation,
                                    parameters,
                                    variant0_name
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    variant0_values,
                                    variant1_up,
                                );
                            completion_items.push(lsp_types::CompletionItem {
                                label: choice_type_name_node.value.to_string(),
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
                        }
                    }
                    ElmSyntaxDeclaration::Port {
                        name: maybe_name,
                        colon_key_symbol_range: _,
                        type_,
                    } => {
                        if let Some(name_node) = maybe_name
                            && !existing_member_names_across_at_docs
                                .contains(name_node.value.as_ref())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            type_.as_ref().map(elm_syntax_node_as_ref),
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
                        type_: maybe_type,
                    } => {
                        if let Some(name_node) = maybe_name
                            && !existing_member_names_across_at_docs
                                .contains(name_node.value.as_ref())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::STRUCT),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            parameters,
                                            maybe_type.as_ref().map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                        }
                    }
                    ElmSyntaxDeclaration::Variable {
                        start_name: start_name_node,
                        signature: maybe_signature,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        if !existing_member_names_across_at_docs
                            .contains(start_name_node.value.as_ref())
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: start_name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &completion_project_module.module.syntax.comments,
                                            elm_syntax_node_as_ref_map(
                                                start_name_node,
                                                Box::as_ref,
                                            ),
                                            origin_module_declaration_documentation,
                                            maybe_signature
                                                .as_ref()
                                                .and_then(|signature| signature.type_.as_ref())
                                                .map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
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
                    } => Some(open_choice_type_name.value.as_ref()),
                    ElmSyntaxExpose::Operator(operator_symbol) => {
                        operator_symbol.as_ref().map(|node| node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_ref()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_ref()),
                })
                .collect::<std::collections::HashSet<_>>();
            let import_origin_module_expose_set: ElmExposeSet = elm_syntax_module_header_expose_set(
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
                    .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                                        .map(|node| node.value.as_ref()),
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
                                choice_type_name_node.value.as_ref(),
                            )
                        {
                            let info_markdown: String = format!(
                                "variant in\n{}",
                                present_choice_type_declaration_info_markdown(
                                    &import_module_origin_lookup,
                                    to_complete_module_origin,
                                    &import_origin_module_state.syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(
                                        choice_type_name_node,
                                        Box::as_ref
                                    )),
                                    origin_module_declaration_documentation,
                                    parameters,
                                    variant0_name.as_ref().map(|node| {
                                        elm_syntax_node_as_ref_map(node, Box::as_ref)
                                    }),
                                    variant0_values,
                                    variant1_up,
                                ),
                            );
                            completion_items.extend(
                                variant0_name
                                    .as_ref()
                                    .map(|node| node.value.to_string())
                                    .into_iter()
                                    .chain(variant1_up.iter().filter_map(|variant| {
                                        variant.name.as_ref().map(|node| node.value.to_string())
                                    }))
                                    .map(|variant_name| lsp_types::CompletionItem {
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
                                name_node.value.as_ref(),
                            )
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &import_origin_module_state.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            type_.as_ref().map(elm_syntax_node_as_ref),
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
                        type_: maybe_type,
                    } => {
                        if let Some(name_node) = maybe_name
                            && import_origin_module_declaration_can_still_be_import_expose(
                                name_node.value.as_ref(),
                            )
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::CONSTRUCTOR),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &import_origin_module_state.syntax.comments,
                                            origin_module_declaration_node.range,
                                            Some(elm_syntax_node_as_ref_map(
                                                name_node,
                                                Box::as_ref,
                                            )),
                                            origin_module_declaration_documentation,
                                            parameters,
                                            maybe_type.as_ref().map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
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
                            start_name_node.value.as_ref(),
                        ) {
                            completion_items.push(lsp_types::CompletionItem {
                                label: start_name_node.value.to_string(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &import_module_origin_lookup,
                                            to_complete_module_origin,
                                            &import_origin_module_state.syntax.comments,
                                            elm_syntax_node_as_ref_map(
                                                start_name_node,
                                                Box::as_ref,
                                            ),
                                            origin_module_declaration_documentation,
                                            maybe_signature
                                                .as_ref()
                                                .and_then(|signature| signature.type_.as_ref())
                                                .map(elm_syntax_node_as_ref),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
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
                                        .filter_map(|declaration_or_err| {
                                            declaration_or_err.as_ref().ok()
                                        })
                                        .find_map(|origin_module_potential_function_declaration| {
                                            let origin_module_potential_function_declaration_node =
                                                origin_module_potential_function_declaration
                                                    .declaration
                                                    .as_ref()?;
                                            match &origin_module_potential_function_declaration_node
                                                .value
                                            {
                                                ElmSyntaxDeclaration::Variable {
                                                    start_name: origin_module_declaration_name,
                                                    signature: origin_module_declaration_signature,
                                                    ..
                                                } if origin_module_declaration_name.value
                                                    == origin_module_declaration_function_node
                                                        .value =>
                                                {
                                                    Some((
                                                        origin_module_declaration_signature
                                                            .as_ref(),
                                                        origin_module_potential_function_declaration
                                                            .documentation
                                                            .as_ref()
                                                            .map(|node| node.value.as_ref()),
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
                                            maybe_direction.map(|node| node.value),
                                            maybe_precedence.map(|node| node.value),
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
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
                .map(|node| node.value.as_ref());
            let full_name_to_complete: String = if to_complete_qualification.is_empty() {
                to_complete_name.to_string()
            } else {
                format!("{to_complete_qualification}.{to_complete_name}")
            };
            let to_complete_module_import_alias_origin_lookup: Vec<ElmImportAliasAndModuleOrigin> =
                elm_syntax_imports_create_import_alias_origin_lookup(
                    &completion_project_module.module.syntax.imports,
                );
            let mut completion_items: Vec<lsp_types::CompletionItem> = Vec::new();
            if (to_complete_name.is_empty()) || to_complete_name.starts_with(char::is_uppercase) {
                completion_items.extend(project_module_name_completions_for_except(
                    state,
                    completion_project_module.project,
                    &to_complete_module_import_alias_origin_lookup,
                    &full_name_to_complete,
                    maybe_completion_module_name,
                ));
            }
            if to_complete_qualification.is_empty() {
                let local_binding_completions = local_bindings
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
                                    completion_project_module.project,
                                    &completion_project_module.module.syntax,
                                    local_binding.name,
                                    local_binding.origin,
                                ),
                            },
                        )),
                        ..lsp_types::CompletionItem::default()
                    });
                completion_items.extend(local_binding_completions);
                variable_declaration_completions_into(
                    state,
                    completion_project_module.project,
                    &completion_project_module.module.syntax,
                    &mut completion_items,
                    &ElmExposeSet::All,
                );
                for (import_module_origin, import_expose_set) in completion_project_module
                    .module
                    .syntax
                    .imports
                    .iter()
                    .filter_map(|import_node| {
                        let module_name_node = import_node.value.module_name.as_ref()?;
                        let exposing_node = import_node.value.exposing.as_ref()?;
                        Some((
                            &module_name_node.value,
                            elm_syntax_exposing_to_set(&exposing_node.value),
                        ))
                    })
                {
                    if let Some((_, import_module_state)) = project_state_get_module_with_name(
                        state,
                        completion_project_module.project,
                        import_module_origin,
                    ) {
                        let import_module_expose_set: ElmExposeSet = match import_expose_set {
                            ElmExposeSet::All => elm_syntax_module_header_expose_set(
                                import_module_state.syntax.header.as_ref(),
                            ),
                            ElmExposeSet::Explicit { .. } => import_expose_set,
                        };
                        variable_declaration_completions_into(
                            state,
                            completion_project_module.project,
                            &import_module_state.syntax,
                            &mut completion_items,
                            &import_module_expose_set,
                        );
                    }
                }
            }
            if !to_complete_qualification.is_empty() {
                let to_complete_module_origins: Vec<&str> = look_up_import_alias_module_origins(
                    &to_complete_module_import_alias_origin_lookup,
                    to_complete_qualification,
                )
                .unwrap_or_else(|| vec![to_complete_qualification]);
                for to_complete_module_origin in to_complete_module_origins {
                    if let Some((_, to_complete_origin_module_state)) =
                        project_state_get_module_with_name(
                            state,
                            completion_project_module.project,
                            to_complete_module_origin,
                        )
                    {
                        let origin_module_expose_set: ElmExposeSet =
                            elm_syntax_module_header_expose_set(
                                to_complete_origin_module_state.syntax.header.as_ref(),
                            );
                        variable_declaration_completions_into(
                            state,
                            completion_project_module.project,
                            &to_complete_origin_module_state.syntax,
                            &mut completion_items,
                            &origin_module_expose_set,
                        );
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
                .map(|node| node.value.as_ref());
            let full_name_to_complete: String = if to_complete_qualification.is_empty() {
                to_complete_name.to_string()
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
            if to_complete_qualification.is_empty() {
                type_declaration_completions_into(
                    state,
                    completion_project_module.project,
                    &completion_project_module.module.syntax,
                    &mut completion_items,
                    &ElmExposeSet::All,
                );
                for (import_module_origin, import_expose_set) in completion_project_module
                    .module
                    .syntax
                    .imports
                    .iter()
                    .filter_map(|import_node| {
                        let module_name_node = import_node.value.module_name.as_ref()?;
                        let exposing_node = import_node.value.exposing.as_ref()?;
                        Some((
                            &module_name_node.value,
                            elm_syntax_exposing_to_set(&exposing_node.value),
                        ))
                    })
                {
                    if let Some((_, import_module_state)) = project_state_get_module_with_name(
                        state,
                        completion_project_module.project,
                        import_module_origin,
                    ) {
                        let import_module_expose_set: ElmExposeSet = match import_expose_set {
                            ElmExposeSet::All => elm_syntax_module_header_expose_set(
                                import_module_state.syntax.header.as_ref(),
                            ),
                            ElmExposeSet::Explicit { .. } => import_expose_set,
                        };
                        type_declaration_completions_into(
                            state,
                            completion_project_module.project,
                            &import_module_state.syntax,
                            &mut completion_items,
                            &import_module_expose_set,
                        );
                    }
                }
            }
            if !to_complete_qualification.is_empty() {
                let to_complete_module_origins: Vec<&str> = look_up_import_alias_module_origins(
                    &to_complete_module_import_alias_origin_lookup,
                    to_complete_qualification,
                )
                .unwrap_or_else(|| vec![to_complete_qualification]);
                for to_complete_module_origin in to_complete_module_origins {
                    if let Some((_, origin_module_state)) = project_state_get_module_with_name(
                        state,
                        completion_project_module.project,
                        to_complete_module_origin,
                    ) {
                        let origin_module_expose_set: ElmExposeSet =
                            elm_syntax_module_header_expose_set(
                                origin_module_state.syntax.header.as_ref(),
                            );
                        type_declaration_completions_into(
                            state,
                            completion_project_module.project,
                            &completion_project_module.module.syntax,
                            &mut completion_items,
                            &origin_module_expose_set,
                        );
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

fn variable_declaration_completions_into(
    state: &State,
    project_state: &ProjectState,
    module_syntax: &ElmSyntaxModule,
    completion_items: &mut Vec<lsp_types::CompletionItem>,
    expose_set: &ElmExposeSet,
) {
    let module_name: &str = module_syntax
        .header
        .as_ref()
        .and_then(|header| header.module_name.as_ref())
        .map(|node| node.value.as_ref())
        .unwrap_or("");
    let module_origin_lookup: ModuleOriginLookup =
        elm_syntax_module_create_origin_lookup(state, project_state, module_syntax);
    for (origin_module_declaration_node, origin_module_declaration_documentation) in module_syntax
        .declarations
        .iter()
        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                            .map(|node| node.value.as_ref()),
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
                    && elm_expose_set_contains_choice_type_including_variants(
                        expose_set,
                        &choice_type_name_node.value,
                    )
                {
                    let info_markdown: String = format!(
                        "variant in\n{}",
                        present_choice_type_declaration_info_markdown(
                            &module_origin_lookup,
                            module_name,
                            &module_syntax.comments,
                            origin_module_declaration_node.range,
                            Some(elm_syntax_node_as_ref_map(
                                choice_type_name_node,
                                Box::as_ref
                            )),
                            origin_module_declaration_documentation,
                            parameters,
                            variant0_name
                                .as_ref()
                                .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                            variant0_values,
                            variant1_up,
                        ),
                    );
                    completion_items.extend(
                        variant0_name
                            .as_ref()
                            .map(|node| node.value.to_string())
                            .into_iter()
                            .chain(variant1_up.iter().filter_map(|variant| {
                                variant.name.as_ref().map(|node| node.value.to_string())
                            }))
                            .map(|variant_name: String| lsp_types::CompletionItem {
                                label: variant_name,
                                kind: Some(lsp_types::CompletionItemKind::ENUM_MEMBER),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: info_markdown.clone(),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            }),
                    );
                }
            }
            ElmSyntaxDeclaration::Port {
                name: maybe_name,
                colon_key_symbol_range: _,
                type_,
            } => {
                if let Some(name_node) = maybe_name
                    && elm_expose_set_contains_variable(expose_set, &name_node.value)
                {
                    completion_items.push(lsp_types::CompletionItem {
                        label: name_node.value.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: present_port_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_name,
                                    &module_syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                                    origin_module_declaration_documentation,
                                    type_.as_ref().map(elm_syntax_node_as_ref),
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
                type_: maybe_type,
            } => {
                if let Some(name_node) = maybe_name
                    && elm_expose_set_contains_type_not_including_variants(
                        expose_set,
                        &name_node.value,
                    )
                    && let Some(type_node) = maybe_type
                    && let ElmSyntaxType::Record(_) = type_node.value
                {
                    completion_items.push(lsp_types::CompletionItem {
                        label: name_node.value.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::CONSTRUCTOR),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: format!(
                                    "constructor function for record\n{}",
                                    &present_type_alias_declaration_info_markdown(
                                        &module_origin_lookup,
                                        module_name,
                                        &module_syntax.comments,
                                        origin_module_declaration_node.range,
                                        Some(elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                                        origin_module_declaration_documentation,
                                        parameters,
                                        Some(elm_syntax_node_as_ref(type_node)),
                                    )
                                ),
                            },
                        )),
                        ..lsp_types::CompletionItem::default()
                    });
                }
            }
            ElmSyntaxDeclaration::Variable {
                start_name: start_name_node,
                signature: maybe_signature,
                parameters: _,
                equals_key_symbol_range: _,
                result: _,
            } => {
                if elm_expose_set_contains_variable(expose_set, &start_name_node.value) {
                    completion_items.push(lsp_types::CompletionItem {
                        label: start_name_node.value.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: present_variable_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_name,
                                    &module_syntax.comments,
                                    elm_syntax_node_as_ref_map(start_name_node, Box::as_ref),
                                    origin_module_declaration_documentation,
                                    maybe_signature
                                        .as_ref()
                                        .and_then(|signature| signature.type_.as_ref())
                                        .map(elm_syntax_node_as_ref),
                                ),
                            },
                        )),
                        ..lsp_types::CompletionItem::default()
                    });
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
fn type_declaration_completions_into(
    state: &State,
    project_state: &ProjectState,
    module_syntax: &ElmSyntaxModule,
    completion_items: &mut Vec<lsp_types::CompletionItem>,
    expose_set: &ElmExposeSet,
) {
    let module_name: &str = module_syntax
        .header
        .as_ref()
        .and_then(|header| header.module_name.as_ref())
        .map(|node| node.value.as_ref())
        .unwrap_or("");
    let module_origin_lookup: ModuleOriginLookup =
        elm_syntax_module_create_origin_lookup(state, project_state, module_syntax);
    for (origin_module_declaration_node, origin_module_declaration_documentation) in module_syntax
        .declarations
        .iter()
        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                            .map(|node| node.value.as_ref()),
                    )
                })
        })
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
                    && elm_expose_set_contains_type(expose_set, &name_node.value)
                {
                    completion_items.push(lsp_types::CompletionItem {
                        label: name_node.value.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::ENUM),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_name,
                                    &module_syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                                    origin_module_declaration_documentation,
                                    parameters,
                                    maybe_variant0_name
                                        .as_ref()
                                        .map(|node| elm_syntax_node_as_ref_map(node, Box::as_ref)),
                                    variant0_values,
                                    variant1_up,
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
                    && elm_expose_set_contains_type_not_including_variants(
                        expose_set,
                        &name_node.value,
                    )
                {
                    completion_items.push(lsp_types::CompletionItem {
                        label: name_node.value.to_string(),
                        kind: Some(lsp_types::CompletionItemKind::STRUCT),
                        documentation: Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: present_type_alias_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_name,
                                    &module_syntax.comments,
                                    origin_module_declaration_node.range,
                                    Some(elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                                    origin_module_declaration_documentation,
                                    parameters,
                                    type_.as_ref().map(elm_syntax_node_as_ref),
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

fn respond_to_document_formatting(
    state: &State,
    formatting_arguments: &lsp_types::DocumentFormattingParams,
) -> Option<Vec<lsp_types::TextEdit>> {
    let document_path: std::path::PathBuf =
        formatting_arguments.text_document.uri.to_file_path().ok()?;
    let to_format_project_module = state_get_project_module_by_path(state, &document_path)?;
    let formatted: String = match &state.configured_elm_formatter {
        Some(ConfiguredElmFormatter::Builtin) => {
            elm_syntax_module_format(to_format_project_module.module)
        }
        None => format_using_elm_format(
            "elm-format",
            to_format_project_module.project_path,
            &to_format_project_module.module.source,
        )?,
        Some(ConfiguredElmFormatter::Custom {
            path: configured_elm_format_path,
        }) => format_using_elm_format(
            configured_elm_format_path,
            to_format_project_module.project_path,
            &to_format_project_module.module.source,
        )?,
    };
    // diffing does not seem to be needed here. But maybe it's faster?
    Some(vec![lsp_types::TextEdit {
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: 1_000_000_000, // to_format_project_module.module.source.lines().count() as u32 + 1
                character: 0,
            },
        },
        new_text: formatted,
    }])
}
fn format_using_elm_format(
    configured_elm_format_path: &str,
    project_path: &std::path::Path,
    source: &str,
) -> Option<String> {
    let mut elm_format_cmd: std::process::Command =
        std::process::Command::new(configured_elm_format_path);
    elm_format_cmd
        .args(["--stdin", "--elm-version", "0.19", "--yes"])
        .current_dir(project_path)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    let mut elm_format_process: std::process::Child =
        elm_format_cmd
        .spawn().map_err(|error| {
            eprintln!(
                "running {configured_elm_format_path} failed: {error}. Try installing elm-format via `npm install -g elm-format`."
            );
        }).ok()?;
    {
        // explicit block is necessary to close writing input before blocking for output
        // (otherwise both processes wait, quite the footgun in honestly)
        let mut stdin: std::process::ChildStdin =
            elm_format_process.stdin.take().or_else(|| {
                eprintln!("couldn't open {configured_elm_format_path} stdin");
                let _ = elm_format_process.wait();
                None
            })?;
        std::io::Write::write_all(&mut stdin, source.as_bytes())
            .map_err(|error| {
                eprintln!("couldn't write to {configured_elm_format_path} stdin: {error}");
                let _ = elm_format_process.wait();
            })
            .ok()?;
    }
    let output: std::process::Output = elm_format_process
        .wait_with_output()
        .map_err(|error| {
            eprintln!("couldn't read from {configured_elm_format_path} stdout: {error}");
        })
        .ok()?; // ignore output in case of parse errors
    if !output.stderr.is_empty() {
        // parse error, not worth logging
        return None;
    }
    String::from_utf8(output.stdout)
        .map_err(|error| {
            eprintln!(
                "couldn't read from {configured_elm_format_path} stdout as UTF-8 string: {error}"
            );
        })
        .ok()
}

fn respond_to_document_symbols(
    state: &State,
    document_symbol_arguments: &lsp_types::DocumentSymbolParams,
) -> Option<lsp_types::DocumentSymbolResponse> {
    let document_path: std::path::PathBuf = document_symbol_arguments
        .text_document
        .uri
        .to_file_path()
        .ok()?;
    let project_module = state_get_project_module_by_path(state, &document_path)?;
    Some(lsp_types::DocumentSymbolResponse::Nested(
        project_module
            .module
            .syntax
            .declarations
            .iter()
            .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
            .filter_map(|documented_declaration| documented_declaration.declaration.as_ref())
            .filter_map(|declaration_node| match &declaration_node.value {
                ElmSyntaxDeclaration::ChoiceType {
                    name: maybe_name,
                    parameters: _,
                    equals_key_symbol_range: _,
                    variant0_name,
                    variant0_values,
                    variant1_up,
                } => {
                    let name_node = maybe_name.as_ref()?;
                    Some(lsp_types::DocumentSymbol {
                        name: name_node.value.to_string(),
                        detail: None,
                        kind: lsp_types::SymbolKind::ENUM,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: declaration_node.range,
                        selection_range: name_node.range,
                        children: Some(
                            variant0_name
                                .as_ref()
                                .map(|variant0_name_node| {
                                    (
                                        variant0_name_node,
                                        lsp_types::Range {
                                            start: variant0_name_node.range.start,
                                            end: variant0_values
                                                .last()
                                                .map(|node| node.range.end)
                                                .unwrap_or(variant0_name_node.range.end),
                                        },
                                    )
                                })
                                .into_iter()
                                .chain(variant1_up.iter().filter_map(|variant| {
                                    let variant_name_node = variant.name.as_ref()?;
                                    Some((
                                        variant_name_node,
                                        lsp_types::Range {
                                            start: variant_name_node.range.start,
                                            end: variant
                                                .values
                                                .last()
                                                .map(|node| node.range.end)
                                                .unwrap_or(variant_name_node.range.end),
                                        },
                                    ))
                                }))
                                .map(|(variant_name_node, variant_full_range)| {
                                    lsp_types::DocumentSymbol {
                                        name: variant_name_node.value.to_string(),
                                        detail: None,
                                        kind: lsp_types::SymbolKind::ENUM_MEMBER,
                                        tags: None,
                                        #[allow(deprecated)]
                                        deprecated: None,
                                        range: variant_full_range,
                                        selection_range: variant_name_node.range,
                                        children: None,
                                    }
                                })
                                .collect::<Vec<_>>(),
                        ),
                    })
                }
                ElmSyntaxDeclaration::Operator {
                    operator: maybe_operator,
                    direction: _,
                    precedence: _,
                    equals_key_symbol_range: _,
                    function: _,
                } => {
                    let operator_node = maybe_operator.as_ref()?;
                    Some(lsp_types::DocumentSymbol {
                        name: operator_node.value.to_string(),
                        detail: None,
                        kind: lsp_types::SymbolKind::OPERATOR,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: declaration_node.range,
                        selection_range: operator_node.range,
                        children: None,
                    })
                }
                ElmSyntaxDeclaration::Port {
                    name: maybe_name,
                    colon_key_symbol_range: _,
                    type_: _,
                } => {
                    let name_node = maybe_name.as_ref()?;
                    Some(lsp_types::DocumentSymbol {
                        name: name_node.value.to_string(),
                        detail: None,
                        kind: lsp_types::SymbolKind::FUNCTION,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: declaration_node.range,
                        selection_range: name_node.range,
                        children: None,
                    })
                }
                ElmSyntaxDeclaration::TypeAlias {
                    name: maybe_name,
                    alias_keyword_range: _,
                    parameters: _,
                    equals_key_symbol_range: _,
                    type_: _,
                } => {
                    let name_node = maybe_name.as_ref()?;
                    Some(lsp_types::DocumentSymbol {
                        name: name_node.value.to_string(),
                        detail: None,
                        kind: lsp_types::SymbolKind::STRUCT,
                        tags: None,
                        #[allow(deprecated)]
                        deprecated: None,
                        range: declaration_node.range,
                        selection_range: name_node.range,
                        children: None,
                    })
                }
                ElmSyntaxDeclaration::Variable {
                    start_name: start_name_node,
                    signature: _,
                    parameters: _,
                    equals_key_symbol_range: _,
                    result: _,
                } => Some(lsp_types::DocumentSymbol {
                    name: start_name_node.value.to_string(),
                    detail: None,
                    kind: lsp_types::SymbolKind::FUNCTION,
                    tags: None,
                    #[allow(deprecated)]
                    deprecated: None,
                    range: declaration_node.range,
                    selection_range: start_name_node.range,
                    children: None,
                }),
            })
            .collect::<Vec<_>>(),
    ))
}
fn respond_to_code_action(
    state: &State,
    code_action_arguments: lsp_types::CodeActionParams,
) -> Option<Vec<lsp_types::CodeActionOrCommand>> {
    let document_path: std::path::PathBuf = code_action_arguments
        .text_document
        .uri
        .to_file_path()
        .ok()?;
    let project_module_state = state_get_project_module_by_path(state, &document_path)?;
    let code_action_symbol_node: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_symbol_at_position(
            &project_module_state.module.syntax,
            code_action_arguments.range.start,
        )?;
    match code_action_symbol_node.value {
        ElmSyntaxSymbol::ModuleName(_) => None,
        ElmSyntaxSymbol::ImportAlias { .. } => None,
        ElmSyntaxSymbol::ModuleHeaderExpose { .. } => None,
        ElmSyntaxSymbol::ModuleDocumentationAtDocsMember { .. } => None,
        ElmSyntaxSymbol::ModuleMemberDeclarationName { .. } => None,
        ElmSyntaxSymbol::ImportExpose { .. } => None,
        ElmSyntaxSymbol::LetDeclarationName { .. } => None,
        ElmSyntaxSymbol::TypeVariable { .. } => None,
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            qualification,
            name: _,
            local_bindings: _,
        }
        | ElmSyntaxSymbol::Type {
            qualification,
            name: _,
        } => {
            if implicit_imports_uniquely_qualified.iter().any(
                |&(implicit_import_qualification, _)| {
                    implicit_import_qualification == qualification
                },
            ) {
                return None;
            }
            let already_imported: bool =
                project_module_state
                    .module
                    .syntax
                    .imports
                    .iter()
                    .any(|import_node| {
                        import_node
                            .value
                            .module_name
                            .as_ref()
                            .is_some_and(|name_node| qualification == name_node.value.as_ref())
                            || import_node
                                .value
                                .alias_name
                                .as_ref()
                                .is_some_and(|alias_node| {
                                    qualification == alias_node.value.as_ref()
                                })
                    });
            if already_imported {
                return None;
            }
            Some(vec![if project_state_get_module_with_name(
                state,
                project_module_state.project,
                qualification,
            )
            .is_none()
            {
                lsp_types::CodeActionOrCommand::CodeAction(lsp_types::CodeAction {
                    title: "add missing import".to_string(),
                    kind: Some(lsp_types::CodeActionKind::QUICKFIX),
                    diagnostics: None,
                    edit: None,
                    command: None,
                    is_preferred: None,
                    disabled: Some(lsp_types::CodeActionDisabled {
                        reason: "could not find a module with this name in this project"
                            .to_string(),
                    }),
                    data: None,
                })
            } else {
                let maybe_before_import_insert_position: Option<lsp_types::Position> =
                    project_module_state
                        .module
                        .syntax
                        .imports
                        .last()
                        .map(|node| node.range.end)
                        .or_else(|| {
                            project_module_state
                                .module
                                .syntax
                                .documentation
                                .as_ref()
                                .map(|node| node.range.end)
                        })
                        .or_else(|| {
                            project_module_state
                                .module
                                .syntax
                                .header
                                .as_ref()
                                .map(elm_syntax_module_header_end_position)
                        });
                let import_insert_position = maybe_before_import_insert_position
                    .map(|end| lsp_types::Position {
                        line: end.line + 1,
                        character: 0,
                    })
                    .unwrap_or_else(|| lsp_types::Position {
                        line: 0,
                        character: 0,
                    });
                lsp_types::CodeActionOrCommand::CodeAction(lsp_types::CodeAction {
                    title: "add missing import".to_string(),
                    kind: Some(lsp_types::CodeActionKind::QUICKFIX),
                    diagnostics: None,
                    edit: Some(lsp_types::WorkspaceEdit {
                        changes: None,
                        change_annotations: None,
                        document_changes: Some(lsp_types::DocumentChanges::Edits(vec![
                            lsp_types::TextDocumentEdit {
                                text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                    uri: code_action_arguments.text_document.uri,
                                    version: None,
                                },
                                edits: vec![lsp_types::OneOf::Left(lsp_types::TextEdit {
                                    range: lsp_types::Range {
                                        start: import_insert_position,
                                        end: import_insert_position,
                                    },
                                    new_text: format!("import {qualification}\n"),
                                })],
                            },
                        ])),
                    }),
                    command: None,
                    is_preferred: Some(true),
                    disabled: None,
                    data: None,
                })
            }])
        }
    }
}
/// caveat: for effect modules, this does not respect the { command, subscription }
/// does not matter if you only look at lines as effect modules are currently all single-line
fn elm_syntax_module_header_end_position(
    elm_syntax_module_header: &ElmSyntaxModuleHeader,
) -> lsp_types::Position {
    elm_syntax_module_header
        .exposing
        .as_ref()
        .map(|node| node.range.end)
        .or_else(|| {
            elm_syntax_module_header
                .exposing_keyword_range
                .map(|range| range.end)
        })
        .unwrap_or_else(|| match &elm_syntax_module_header.specific {
            ElmSyntaxModuleHeaderSpecific::Pure {
                module_keyword_range,
            } => elm_syntax_module_header
                .module_name
                .as_ref()
                .map(|node| node.range.end)
                .unwrap_or(module_keyword_range.end),
            ElmSyntaxModuleHeaderSpecific::Port {
                port_keyword_range: _,
                module_keyword_range,
            } => elm_syntax_module_header
                .module_name
                .as_ref()
                .map(|node| node.range.end)
                .unwrap_or(module_keyword_range.end),
            ElmSyntaxModuleHeaderSpecific::Effect {
                effect_keyword_range: _,
                module_keyword_range: _,
                where_keyword_range,
                command: _,
                subscription: _,
            } => where_keyword_range.end,
        })
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
        .unwrap_or_else(|| "".to_string());
    let to_completion_item = |module_path: &std::path::PathBuf,
                              module_name: &str,
                              module_syntax: &ElmSyntaxModule|
     -> Option<lsp_types::CompletionItem> {
        let module_url: lsp_types::Url = lsp_types::Url::from_file_path(module_path).ok()?;
        Some(lsp_types::CompletionItem {
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
                    value: module_syntax
                        .documentation
                        .as_ref()
                        .map(|module_documentation| {
                            elm_syntax_module_documentation_to_markdown(
                                &module_url,
                                module_syntax,
                                &module_documentation.value,
                            )
                        })
                        .unwrap_or_else(|| "_module has no documentation comment_".to_string()),
                },
            )),
            ..lsp_types::CompletionItem::default()
        })
    };
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
                        to_completion_item(
                            &importable_dependency_module_origin.module_path,
                            importable_dependency_module_name_or_alias,
                            &importable_dependency_module_state.syntax,
                        )
                    })
            },
        )
        .chain(completion_project.modules.iter().flat_map(
            |(project_module_path, project_module)| {
                project_module
                    .syntax
                    .header
                    .as_ref()
                    .and_then(|header| header.module_name.as_ref())
                    .map(|project_module_name_node| {
                        let project_module_name: &str = project_module_name_node.value.as_ref();
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
                                    to_completion_item(
                                        project_module_path,
                                        importable_dependency_module_name_or_alias,
                                        &project_module.syntax,
                                    )
                                }
                            })
                    })
                    .into_iter()
                    .flatten()
            },
        ))
        .collect::<Vec<_>>()
}
fn elm_syntax_module_documentation_to_markdown(
    module_url: &lsp_types::Url,
    module_syntax: &ElmSyntaxModule,
    module_documentation_elements: &[ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
) -> String {
    let all_at_docs_module_members: std::collections::HashSet<&str> = module_documentation_elements
        .iter()
        .flat_map(|module_documentation_element_node| {
            match &module_documentation_element_node.value {
                ElmSyntaxModuleDocumentationElement::Markdown(_) => None,
                ElmSyntaxModuleDocumentationElement::AtDocs(expose_group_names) => {
                    Some(expose_group_names.iter().map(|node| {
                        node.value
                            .as_ref()
                            .trim_start_matches('(')
                            .trim_end_matches(')')
                    }))
                }
            }
            .into_iter()
            .flatten()
        })
        .collect::<std::collections::HashSet<_>>();
    let module_member_declaration_names: std::collections::HashMap<&str, lsp_types::Range> =
        if all_at_docs_module_members.is_empty() {
            std::collections::HashMap::new()
        } else {
            module_syntax
                .declarations
                .iter()
                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                .filter_map(|documented| documented.declaration.as_ref())
                .filter_map(|declaration_node| match &declaration_node.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: maybe_declaration_name,
                        ..
                    } => maybe_declaration_name
                        .as_ref()
                        .map(|node| (node.value.as_ref(), node.range)),
                    ElmSyntaxDeclaration::Operator {
                        operator: maybe_operator,
                        ..
                    } => maybe_operator.as_ref().map(|node| (node.value, node.range)),
                    ElmSyntaxDeclaration::Port {
                        name: maybe_declaration_name,
                        ..
                    } => maybe_declaration_name
                        .as_ref()
                        .map(|node| (node.value.as_ref(), node.range)),
                    ElmSyntaxDeclaration::TypeAlias {
                        name: maybe_declaration_name,
                        ..
                    } => maybe_declaration_name
                        .as_ref()
                        .map(|node| (node.value.as_ref(), node.range)),
                    ElmSyntaxDeclaration::Variable {
                        start_name: declaration_start_name_node,
                        ..
                    } => Some((
                        declaration_start_name_node.value.as_ref(),
                        declaration_start_name_node.range,
                    )),
                })
                .filter(|(name, _)| all_at_docs_module_members.contains(name))
                .collect::<std::collections::HashMap<_, _>>()
        };
    let look_up_module_member_declaration_name_range =
        |expose_name: &str| -> Option<lsp_types::Range> {
            module_member_declaration_names
                .get(expose_name.trim_start_matches('(').trim_end_matches(')'))
                .copied()
        };
    let mut result_builder: String = String::new();
    for module_documentation_element_node in module_documentation_elements {
        match &module_documentation_element_node.value {
            ElmSyntaxModuleDocumentationElement::Markdown(markdown_node) => {
                markdown_convert_code_blocks_to_elm_into(&mut result_builder, markdown_node);
            }
            ElmSyntaxModuleDocumentationElement::AtDocs(expose_group_names) => {
                // consider inlining their documentation as well
                result_builder.push_str("_see_ ");
                if let Some((expose_name_node0, expose_name1_up)) = expose_group_names.split_first()
                {
                    match look_up_module_member_declaration_name_range(
                        expose_name_node0.value.as_ref(),
                    ) {
                        None => {
                            result_builder.push_str(&expose_name_node0.value);
                        }
                        Some(module_member0_declaration_name_range) => {
                            name_as_module_module_member_markdown_link_into(
                                &mut result_builder,
                                module_url,
                                module_member0_declaration_name_range,
                                &expose_name_node0.value,
                            );
                        }
                    }
                    for expose_name_node in expose_name1_up {
                        result_builder.push_str(", ");
                        match look_up_module_member_declaration_name_range(
                            expose_name_node.value.as_ref(),
                        ) {
                            None => {
                                result_builder.push_str(&expose_name_node.value);
                            }
                            Some(module_member_declaration_name_range) => {
                                name_as_module_module_member_markdown_link_into(
                                    &mut result_builder,
                                    module_url,
                                    module_member_declaration_name_range,
                                    &expose_name_node.value,
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    result_builder
}
fn name_as_module_module_member_markdown_link_into(
    builder: &mut String,
    module_url: &lsp_types::Url,
    module_member_declaration_name_range: lsp_types::Range,
    module_member_name: &str,
) {
    // I've searched a bunch but couldn't find a standardized way to link
    // to a document symbol. What is done here is only checked to work in vscode-like editors
    let expose_name_normal: &str = module_member_name
        .strip_suffix("(..)")
        .unwrap_or(module_member_name);
    builder.push_str("[`");
    builder.push_str(expose_name_normal);
    builder.push_str("`](");
    builder.push_str(module_url.as_str());
    builder.push_str("#L");
    {
        use std::fmt::Write as _;
        let _ = write!(
            builder,
            "{}",
            // at least in vscode-like editors it's 1-based
            1 + module_member_declaration_name_range.start.line
        );
    }
    builder.push(')');
}
fn documentation_comment_to_markdown(documentation: &str) -> String {
    let markdown_source: &str = documentation.trim();
    let mut builder: String = String::new();
    markdown_convert_code_blocks_to_elm_into(&mut builder, markdown_source);
    builder
}
fn markdown_convert_code_blocks_to_elm_into(builder: &mut String, markdown_source: &str) {
    // because I don't want to introduce a full markdown parser for just this tiny
    // improvement, the code below only approximates where code blocks are.
    let mut with_fenced_code_blocks_converted = String::new();
    markdown_convert_unspecific_fenced_code_blocks_to_elm_into(
        &mut with_fenced_code_blocks_converted,
        markdown_source,
    );
    markdown_convert_indented_code_blocks_to_elm(builder, &with_fenced_code_blocks_converted);
}

/// replace fenced no-language-specified code blocks by `elm...`
fn markdown_convert_unspecific_fenced_code_blocks_to_elm_into(
    result_builder: &mut String,
    markdown_source: &str,
) {
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
}

fn markdown_convert_indented_code_blocks_to_elm(builder: &mut String, markdown_source: &str) {
    let mut current_indent: usize = 0;
    let mut is_in_code_block: bool = false;
    let mut previous_line_was_blank: bool = false;
    for source_line in markdown_source.lines() {
        if source_line.is_empty() {
            builder.push('\n');
            previous_line_was_blank = true;
        } else {
            let current_line_indent: usize = source_line
                .chars()
                .take_while(char::is_ascii_whitespace)
                .count();
            if current_line_indent == source_line.len() {
                // ignore blank line
                builder.push_str(source_line);
                builder.push('\n');
                previous_line_was_blank = true;
            } else {
                if is_in_code_block {
                    if current_line_indent <= current_indent - 1 {
                        is_in_code_block = false;
                        current_indent = current_line_indent;
                        builder.push_str("```\n");
                        builder.push_str(source_line);
                        builder.push('\n');
                    } else {
                        builder.push_str(&source_line[current_indent..]);
                        builder.push('\n');
                    }
                } else if previous_line_was_blank && (current_line_indent >= current_indent + 4) {
                    is_in_code_block = true;
                    current_indent = current_line_indent;
                    builder.push_str("```elm\n");
                    builder.push_str(&source_line[current_line_indent..]);
                    builder.push('\n');
                } else {
                    current_indent = current_line_indent;
                    builder.push_str(source_line);
                    builder.push('\n');
                }
                previous_line_was_blank = false;
            }
        }
    }
    if is_in_code_block {
        builder.push_str("```\n");
    }
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

struct PositionDelta {
    line: u32,
    character: u32,
}
fn lsp_position_positive_delta(
    before: lsp_types::Position,
    after: lsp_types::Position,
) -> Result<PositionDelta, String> {
    match before.line.cmp(&after.line) {
        std::cmp::Ordering::Greater => Err(format!(
            "before line > after line (before: {}, after {})",
            lsp_position_to_string(before),
            lsp_position_to_string(after)
        )),
        std::cmp::Ordering::Equal => {
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
        }
        std::cmp::Ordering::Less => Ok(PositionDelta {
            line: after.line - before.line,
            character: after.character,
        }),
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
    elm_syntax_highlight_kind: &ElmSyntaxHighlightKind,
) -> lsp_types::SemanticTokenType {
    match elm_syntax_highlight_kind {
        ElmSyntaxHighlightKind::KeySymbol => lsp_types::SemanticTokenType::KEYWORD,
        ElmSyntaxHighlightKind::Operator => lsp_types::SemanticTokenType::OPERATOR,
        ElmSyntaxHighlightKind::Field => lsp_types::SemanticTokenType::PROPERTY,
        ElmSyntaxHighlightKind::ModuleNameOrAlias => lsp_types::SemanticTokenType::NAMESPACE,
        ElmSyntaxHighlightKind::Type => lsp_types::SemanticTokenType::TYPE,
        ElmSyntaxHighlightKind::Variable => lsp_types::SemanticTokenType::VARIABLE,
        ElmSyntaxHighlightKind::Variant => lsp_types::SemanticTokenType::ENUM_MEMBER,
        ElmSyntaxHighlightKind::DeclaredVariable => lsp_types::SemanticTokenType::FUNCTION,
        ElmSyntaxHighlightKind::Comment => lsp_types::SemanticTokenType::COMMENT,
        ElmSyntaxHighlightKind::Number => lsp_types::SemanticTokenType::NUMBER,
        ElmSyntaxHighlightKind::String => lsp_types::SemanticTokenType::STRING,
        ElmSyntaxHighlightKind::TypeVariable => lsp_types::SemanticTokenType::TYPE_PARAMETER,
    }
}

fn derive_module_name_from_path(
    source_directories: &[std::path::PathBuf],
    module_path: &std::path::Path,
) -> Option<String> {
    source_directories
        .iter()
        .filter_map(|source_directory_path| module_path.strip_prefix(source_directory_path).ok())
        .filter_map(std::path::Path::to_str)
        .max_by(|a, b| a.len().cmp(&b.len()))
        .map(|path_in_source_directory| {
            path_in_source_directory
                // I'm certain there is a better way to convert path separators independent of OS
                .trim_start_matches(['/', '\\'])
                .trim_end_matches(".elm")
                .replace(['/', '\\'], ".")
        })
}

fn list_elm_files_in_directory_at_paths(
    paths: impl Iterator<Item = std::path::PathBuf>,
) -> Vec<std::path::PathBuf> {
    let mut result: Vec<std::path::PathBuf> = Vec::new();
    for path in paths {
        list_files_passing_test_in_directory_at_path_into(&mut result, path, |file_path| {
            file_path
                .extension()
                .is_some_and(|extension| extension == "elm")
        });
    }
    result
}

fn list_elm_project_directories_in_directory_at_path(
    paths: impl Iterator<Item = std::path::PathBuf>,
) -> Vec<std::path::PathBuf> {
    let mut result: Vec<std::path::PathBuf> = Vec::new();
    for path in paths {
        list_elm_project_directories_in_directory_at_path_into(&mut result, &path);
    }
    result
}

fn list_elm_project_directories_in_directory_at_path_into(
    so_far: &mut Vec<std::path::PathBuf>,
    path: &std::path::PathBuf,
) {
    if !path.is_dir() {
        return;
    }
    if path
        .file_name()
        .is_some_and(|file_name| file_name == "elm-stuff")
    {
        // some elm tools put generated code including elm.json there
        return;
    }
    if let Ok(dir_subs) = std::fs::read_dir(path) {
        for dir_sub in dir_subs.into_iter().filter_map(Result::ok) {
            let dir_sub_path: std::path::PathBuf = dir_sub.path();
            if dir_sub_path.is_file()
                && dir_sub_path
                    .file_name()
                    .is_some_and(|file_name| file_name == "elm.json")
            {
                so_far.push(path.clone());
            }
            list_elm_project_directories_in_directory_at_path_into(so_far, &dir_sub_path);
        }
    }
}

fn list_files_passing_test_in_directory_at_path_into(
    so_far: &mut Vec<std::path::PathBuf>,
    path: std::path::PathBuf,
    should_add_file: fn(&std::path::PathBuf) -> bool,
) {
    if path.is_dir() {
        if let Ok(dir_subs) = std::fs::read_dir(&path) {
            for dir_sub in dir_subs.into_iter().filter_map(Result::ok) {
                list_files_passing_test_in_directory_at_path_into(
                    so_far,
                    dir_sub.path(),
                    should_add_file,
                );
            }
        }
    } else {
        if should_add_file(&path) {
            so_far.push(path);
        }
    }
}

// // // below persistent rust types and conversions to and from temporary elm types
#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxType {
    Unit,
    Variable(Box<str>),
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
        record_variable: Option<ElmSyntaxNode<Box<str>>>,
        bar_key_symbol_range: lsp_types::Range,
        fields: Vec<ElmSyntaxTypeField>,
    },
}
#[derive(Clone, Debug, PartialEq)]
struct ElmQualifiedName {
    qualification: Box<str>,
    name: Box<str>,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxTypeField {
    name: ElmSyntaxNode<Box<str>>,
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
        value: Result<i64, Box<str>>,
    },
    String {
        content: String,
        quoting_style: ElmSyntaxStringQuotingStyle,
    },
    Variable(Box<str>),
    As {
        pattern: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        as_keyword_range: lsp_types::Range,
        variable: Option<ElmSyntaxNode<Box<str>>>,
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
    Record(Vec<ElmSyntaxNode<Box<str>>>),
    Variant {
        reference: ElmSyntaxNode<ElmQualifiedName>,
        values: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ElmSyntaxStringQuotingStyle {
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
    value_type_name: ElmSyntaxNode<Box<str>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxLetDeclaration {
    Destructuring {
        pattern: ElmSyntaxNode<ElmSyntaxPattern>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        expression: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
    VariableDeclaration {
        start_name: ElmSyntaxNode<Box<str>>,
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
    Float(Result<f64, Box<str>>),
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
        base: ElmSyntaxIntBase,
        value: Result<i64, Box<str>>,
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
        field: Option<ElmSyntaxNode<Box<str>>>,
    },
    RecordAccessFunction(Option<ElmSyntaxNode<Box<str>>>),
    RecordUpdate {
        record_variable: Option<ElmSyntaxNode<Box<str>>>,
        bar_key_symbol_range: lsp_types::Range,
        fields: Vec<ElmSyntaxExpressionField>,
    },
    Reference {
        qualification: Box<str>,
        name: Box<str>,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ElmSyntaxIntBase {
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
    name: ElmSyntaxNode<Box<str>>,
    equals_key_symbol_range: Option<lsp_types::Range>,
    value: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExposing {
    All(lsp_types::Range),
    Explicit(Vec<ElmSyntaxNode<ElmSyntaxExpose>>),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExpose {
    ChoiceTypeIncludingVariants {
        name: ElmSyntaxNode<Box<str>>,
        open_range: Option<lsp_types::Range>,
    },
    Operator(Option<ElmSyntaxNode<&'static str>>),
    Type(Box<str>),
    Variable(Box<str>),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxDeclaration {
    ChoiceType {
        name: Option<ElmSyntaxNode<Box<str>>>,
        parameters: Vec<ElmSyntaxNode<Box<str>>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        variant0_name: Option<ElmSyntaxNode<Box<str>>>,
        variant0_values: Vec<ElmSyntaxNode<ElmSyntaxType>>,
        variant1_up: Vec<ElmSyntaxChoiceTypeDeclarationTailingVariant>,
    },
    Operator {
        direction: Option<ElmSyntaxNode<ElmSyntaxInfixDirection>>,
        precedence: Option<ElmSyntaxNode<i64>>,
        operator: Option<ElmSyntaxNode<&'static str>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        function: Option<ElmSyntaxNode<Box<str>>>,
    },
    Port {
        name: Option<ElmSyntaxNode<Box<str>>>,
        colon_key_symbol_range: Option<lsp_types::Range>,
        type_: Option<ElmSyntaxNode<ElmSyntaxType>>,
    },
    TypeAlias {
        alias_keyword_range: lsp_types::Range,
        name: Option<ElmSyntaxNode<Box<str>>>,
        parameters: Vec<ElmSyntaxNode<Box<str>>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        type_: Option<ElmSyntaxNode<ElmSyntaxType>>,
    },
    Variable {
        start_name: ElmSyntaxNode<Box<str>>,
        signature: Option<ElmSyntaxVariableDeclarationSignature>,
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        equals_key_symbol_range: Option<lsp_types::Range>,
        result: Option<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ElmSyntaxInfixDirection {
    Left,
    Non,
    Right,
}
#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxChoiceTypeDeclarationTailingVariant {
    or_key_symbol_range: lsp_types::Range,
    name: Option<ElmSyntaxNode<Box<str>>>,
    values: Vec<ElmSyntaxNode<ElmSyntaxType>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct ElmSyntaxNode<Value> {
    range: lsp_types::Range,
    value: Value,
}

fn elm_syntax_node_as_ref<Value>(elm_syntax_node: &ElmSyntaxNode<Value>) -> ElmSyntaxNode<&Value> {
    ElmSyntaxNode {
        range: elm_syntax_node.range,
        value: &elm_syntax_node.value,
    }
}
fn elm_syntax_node_as_ref_map<'a, A, B>(
    elm_syntax_node: &'a ElmSyntaxNode<A>,
    value_change: impl Fn(&'a A) -> B,
) -> ElmSyntaxNode<B> {
    ElmSyntaxNode {
        range: elm_syntax_node.range,
        value: value_change(&elm_syntax_node.value),
    }
}
fn elm_syntax_node_map<A, B>(
    elm_syntax_node: ElmSyntaxNode<A>,
    value_change: impl Fn(A) -> B,
) -> ElmSyntaxNode<B> {
    ElmSyntaxNode {
        range: elm_syntax_node.range,
        value: value_change(elm_syntax_node.value),
    }
}
fn elm_syntax_node_unbox<Value: ?Sized>(
    elm_syntax_node_box: &ElmSyntaxNode<Box<Value>>,
) -> ElmSyntaxNode<&Value> {
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
    module_name: Option<ElmSyntaxNode<Box<str>>>,
    exposing_keyword_range: Option<lsp_types::Range>,
    exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxModule {
    header: Option<ElmSyntaxModuleHeader>,
    documentation: Option<ElmSyntaxNode<Vec<ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>>>>,
    imports: Vec<ElmSyntaxNode<ElmSyntaxImport>>,
    comments: Vec<ElmSyntaxNode<ElmSyntaxComment>>,
    declarations: Vec<Result<ElmSyntaxDocumentedDeclaration, Box<str>>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxModuleDocumentationElement {
    Markdown(Box<str>),
    AtDocs(Vec<ElmSyntaxNode<Box<str>>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxDocumentedDeclaration {
    documentation: Option<ElmSyntaxNode<Box<str>>>,
    declaration: Option<ElmSyntaxNode<ElmSyntaxDeclaration>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxImport {
    module_name: Option<ElmSyntaxNode<Box<str>>>,
    as_keyword_range: Option<lsp_types::Range>,
    alias_name: Option<ElmSyntaxNode<Box<str>>>,
    exposing_keyword_range: Option<lsp_types::Range>,
    exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>>,
}
#[derive(Clone, Debug)]
enum ElmExposeSet<'a> {
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
) -> ElmExposeSet<'a> {
    match elm_syntax_module_header.and_then(|header| header.exposing.as_ref()) {
        None => ElmExposeSet::All,
        Some(module_header_expose_specific_node) => {
            elm_syntax_exposing_to_set(&module_header_expose_specific_node.value)
        }
    }
}
fn elm_syntax_exposing_to_set<'a>(elm_syntax_exposing: &'a ElmSyntaxExposing) -> ElmExposeSet<'a> {
    match elm_syntax_exposing {
        ElmSyntaxExposing::All(_) => ElmExposeSet::All,
        ElmSyntaxExposing::Explicit(exposes) => {
            let mut operators: Vec<&str> = Vec::new();
            let mut variables: Vec<&str> = Vec::with_capacity(exposes.len());
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
                        operators.push(operator_node.value);
                    }
                    ElmSyntaxExpose::Type(name) => {
                        types.push(name);
                    }
                    ElmSyntaxExpose::Variable(name) => {
                        variables.push(name);
                    }
                }
            }
            ElmExposeSet::Explicit {
                operators: operators,
                variables: variables,
                types: types,
                choice_types_including_variants: choice_types_including_variants,
            }
        }
    }
}
fn elm_expose_set_contains_type(expose_set: &ElmExposeSet, name_to_check: &str) -> bool {
    match expose_set {
        ElmExposeSet::All => true,
        ElmExposeSet::Explicit {
            choice_types_including_variants,
            types,
            operators: _,
            variables: _,
        } => {
            types.contains(&name_to_check)
                || choice_types_including_variants.contains(&name_to_check)
        }
    }
}
fn elm_expose_set_contains_type_not_including_variants(
    expose_set: &ElmExposeSet,
    name_to_check: &str,
) -> bool {
    match expose_set {
        ElmExposeSet::All => true,
        ElmExposeSet::Explicit {
            choice_types_including_variants: _,
            types,
            operators: _,
            variables: _,
        } => types.contains(&name_to_check),
    }
}
fn elm_expose_set_contains_variable(expose_set: &ElmExposeSet, name_to_check: &str) -> bool {
    match expose_set {
        ElmExposeSet::All => true,
        ElmExposeSet::Explicit {
            choice_types_including_variants: _,
            types: _,
            operators: _,
            variables,
        } => variables.contains(&name_to_check),
    }
}
fn elm_expose_set_contains_choice_type_including_variants(
    expose_set: &ElmExposeSet,
    name_to_check: &str,
) -> bool {
    match expose_set {
        ElmExposeSet::All => true,
        ElmExposeSet::Explicit {
            choice_types_including_variants,
            types: _,
            operators: _,
            variables: _,
        } => choice_types_including_variants.contains(&name_to_check),
    }
}
fn elm_expose_set_contains(expose_set: &ElmExposeSet, name_to_check: &str) -> bool {
    match expose_set {
        ElmExposeSet::All => true,
        ElmExposeSet::Explicit {
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

/// Create through `module_origin_lookup_for_implicit_imports` or
/// `elm_syntax_module_create_origin_lookup`
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
        uniquely_qualified: std::collections::HashMap::from(implicit_imports_uniquely_qualified),
        ambiguously_qualified: std::collections::HashMap::new(),
    }
}
const implicit_imports_uniquely_qualified: [(&str, &str); 11] = [
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
];

fn look_up_origin_module<'a>(
    module_origin_lookup: &ModuleOriginLookup<'a>,
    qualified: ElmQualified<'a>,
) -> &'a str {
    match match qualified.qualification {
        "" => module_origin_lookup.unqualified.get(qualified.name),
        qualification_module_or_alias => module_origin_lookup
            .uniquely_qualified
            .get(qualification_module_or_alias),
    } {
        Some(module_origin) => module_origin,
        None => match module_origin_lookup.ambiguously_qualified.get(&qualified) {
            Some(module_origin) => module_origin,
            None => qualified.qualification,
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
            let alias_name_node = import_node.value.alias_name.as_ref()?;
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
        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
        .filter_map(|documented_declaration| documented_declaration.declaration.as_ref())
    {
        match &declaration_node.value {
            ElmSyntaxDeclaration::ChoiceType {
                name: maybe_name,
                parameters: _,
                equals_key_symbol_range: _,
                variant0_name: maybe_variant0_name,
                variant0_values: _,
                variant1_up,
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
                        .insert(operator_node.value, self_module_name);
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
        let allowed_qualification: &str = match &import.alias_name {
            None => import_module_name,
            Some(import_alias_name) => &import_alias_name.value,
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
        let mut insert_import_expose = |import_expose: &'a str| {
            if module_origin_lookup.unqualified.contains_key(import_expose) {
                module_origin_lookup.ambiguously_qualified.insert(
                    ElmQualified {
                        qualification: allowed_qualification,
                        name: import_expose,
                    },
                    import_module_name,
                );
            } else {
                module_origin_lookup
                    .unqualified
                    .insert(import_expose, import_module_name);
            }
        };
        if let Some(import_exposing) = &import.exposing {
            match &import_exposing.value {
                ElmSyntaxExposing::All(_) => {
                    if let Some((_, imported_module_state)) =
                        project_state_get_module_with_name(state, project_state, import_module_name)
                    {
                        for import_exposed_symbol in
                            elm_syntax_module_exposed_symbols(&imported_module_state.syntax)
                        {
                            insert_import_expose(import_exposed_symbol);
                        }
                    }
                }
                ElmSyntaxExposing::Explicit(exposes) => {
                    for expose_node in exposes {
                        match &expose_node.value {
                            ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                                name: choice_type_expose_name,
                                open_range: _,
                            } => {
                                insert_import_expose(&choice_type_expose_name.value);
                                if let Some((_, imported_module_syntax)) =
                                    project_state_get_module_with_name(
                                        state,
                                        project_state,
                                        import_module_name,
                                    )
                                {
                                    'until_origin_choice_type_declaration_found: for documented_declaration in
                                        imported_module_syntax
                                            .syntax
                                            .declarations
                                            .iter()
                                            .filter_map(|declaration_or_err| {
                                                declaration_or_err.as_ref().ok()
                                            })
                                    {
                                        if let Some(declaration_node) =
                                            &documented_declaration.declaration
                                            && let ElmSyntaxDeclaration::ChoiceType {
                                                name: maybe_imported_module_choice_type_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                variant0_name:
                                                    maybe_imported_module_choice_type_variant0_name,
                                                variant0_values: _,
                                                variant1_up: imported_module_choice_type_variant1_up,
                                            } = &declaration_node.value
                                            && Some(choice_type_expose_name.value.as_ref())
                                                == maybe_imported_module_choice_type_name
                                                    .as_ref()
                                                    .map(|node| node.value.as_ref())
                                        {
                                            if let Some(
                                                imported_module_choice_type_variant0_name_node,
                                            ) = maybe_imported_module_choice_type_variant0_name
                                                .as_ref()
                                            {
                                                insert_import_expose(
                                                    &imported_module_choice_type_variant0_name_node
                                                        .value,
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
                                                    insert_import_expose(
                                                            &imported_module_choice_type_variant_name_node.value,
                                                        );
                                                }
                                            }
                                            break 'until_origin_choice_type_declaration_found;
                                        }
                                    }
                                }
                            }
                            ElmSyntaxExpose::Operator(symbol) => {
                                if let Some(operator_symbol_node) = symbol {
                                    insert_import_expose(operator_symbol_node.value);
                                }
                            }
                            ElmSyntaxExpose::Type(name) => {
                                insert_import_expose(name);
                            }
                            ElmSyntaxExpose::Variable(name) => {
                                insert_import_expose(name);
                            }
                        }
                    }
                }
            }
        }
    }
    module_origin_lookup
}

fn elm_syntax_module_exposed_symbols(elm_syntax_module: &ElmSyntaxModule) -> Vec<&str> {
    match elm_syntax_module
        .header
        .as_ref()
        .and_then(|header| header.exposing.as_ref())
        .as_ref()
        .map(|node| &node.value)
    {
        None | Some(ElmSyntaxExposing::All(_)) => {
            let mut exposed_symbols: Vec<&str> =
                Vec::with_capacity(elm_syntax_module.declarations.len());
            for declaration_node in elm_syntax_module
                .declarations
                .iter()
                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
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
                            exposed_symbols.push(exposed_operator_node.value);
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
            exposed_symbols
        }
        Some(ElmSyntaxExposing::Explicit(exposes)) => {
            let mut exposed_symbols: Vec<&str> = Vec::with_capacity(exposes.len());
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: choice_type_expose_name,
                        open_range: _,
                    } => {
                        exposed_symbols.push(&choice_type_expose_name.value);
                        'until_origin_choice_type_declaration_found: for declaration_node in
                            elm_syntax_module
                                .declarations
                                .iter()
                                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                                .filter_map(|documented_declaration| {
                                    documented_declaration.declaration.as_ref()
                                })
                        {
                            if let ElmSyntaxDeclaration::ChoiceType {
                                name: Some(exposed_choice_type_name_node),
                                parameters: _,
                                equals_key_symbol_range: _,
                                variant0_name: maybe_exposed_choice_type_variant0_name,
                                variant0_values: _,
                                variant1_up: exposed_choice_type_variant1_up,
                            } = &declaration_node.value
                                && choice_type_expose_name.value
                                    == exposed_choice_type_name_node.value
                            {
                                if let Some(exposed_choice_type_variant0_name_node) =
                                    maybe_exposed_choice_type_variant0_name
                                {
                                    exposed_symbols
                                        .push(&exposed_choice_type_variant0_name_node.value);
                                }
                                for exposed_choice_type_variant in exposed_choice_type_variant1_up {
                                    if let Some(exposed_choice_type_variant_name_node) =
                                        &exposed_choice_type_variant.name
                                    {
                                        exposed_symbols
                                            .push(&exposed_choice_type_variant_name_node.value);
                                    }
                                }
                                break 'until_origin_choice_type_declaration_found;
                            }
                        }
                    }
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        if let Some(symbol_node) = maybe_symbol {
                            exposed_symbols.push(symbol_node.value);
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
            exposed_symbols
        }
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
enum LineSpan {
    Single,
    Multiple,
}
fn linebreak_indented_into(so_far: &mut String, indent: usize) {
    so_far.push('\n');
    so_far.extend(std::iter::repeat_n(' ', indent));
}
fn space_or_linebreak_indented_into(so_far: &mut String, line_span: LineSpan, indent: usize) {
    match line_span {
        LineSpan::Single => {
            so_far.push(' ');
        }
        LineSpan::Multiple => {
            linebreak_indented_into(so_far, indent);
        }
    }
}

fn elm_syntax_type_to_string(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type: ElmSyntaxNode<&ElmSyntaxType>,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
) -> String {
    let mut builder: String = String::new();
    elm_syntax_type_not_parenthesized_into(
        &mut builder,
        indent,
        |qualified| look_up_origin_module(module_origin_lookup, qualified),
        comments, // pass from parens and slice?
        elm_syntax_type,
    );
    builder
}

fn elm_syntax_comments_in_range(
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    range: lsp_types::Range,
) -> &[ElmSyntaxNode<ElmSyntaxComment>] {
    if comments.is_empty() {
        return &[];
    }
    let comments_in_range_start_index: usize = comments
        .binary_search_by(|comment_node| comment_node.range.start.cmp(&range.start))
        .unwrap_or_else(|i| i);
    let comments_in_range_end_exclusive_index: usize = comments
        .binary_search_by(|comment_node| comment_node.range.start.cmp(&range.end))
        .unwrap_or_else(|i| i);
    &comments[comments_in_range_start_index..comments_in_range_end_exclusive_index]
}
fn elm_syntax_comments_from_position(
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    start_position: lsp_types::Position,
) -> &[ElmSyntaxNode<ElmSyntaxComment>] {
    let comments_in_range_start_index: usize = comments
        .binary_search_by(|comment_node| comment_node.range.start.cmp(&start_position))
        .unwrap_or_else(|i| i);
    &comments[comments_in_range_start_index..]
}

/// same caveat as `elm_syntax_comments_into` apply.
/// use in combination with `elm_syntax_comments_in_range`
fn elm_syntax_comments_then_linebreak_indented_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
) {
    for comment_node_in_range in comments {
        elm_syntax_comment_into(so_far, &comment_node_in_range.value);
        linebreak_indented_into(so_far, indent);
    }
}
/// This one is not 100% the same as elm-format
/// as elm-format has made some very obscure choices, like
///   - {- ...-} can be before other syntax on the same line in some cases (e.g. before field names)
///   - multiple consequent {- ...-} can sometimes be on a single line
///   - {--} however will always force a linebreak
///
/// use in combination with `elm_syntax_comments_in_range`
fn elm_syntax_comments_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
) {
    let mut comments_iterator = comments.iter();
    let Some(first_comment_node) = comments_iterator.next() else {
        return;
    };
    elm_syntax_comment_into(so_far, &first_comment_node.value);
    for comment_node_in_range in comments_iterator {
        linebreak_indented_into(so_far, indent);
        elm_syntax_comment_into(so_far, &comment_node_in_range.value);
    }
}
fn elm_syntax_comment_into(so_far: &mut String, comment: &ElmSyntaxComment) {
    match comment.kind {
        ElmSyntaxCommentKind::UntilLinebreak => {
            so_far.push_str("--");
            so_far.push_str(&comment.content);
        }
        ElmSyntaxCommentKind::Block => {
            so_far.push_str("{-");
            so_far.push_str(&comment.content);
            so_far.push_str("-}");
        }
    }
}

fn elm_syntax_type_to_unparenthesized(
    elm_syntax_type: ElmSyntaxNode<&ElmSyntaxType>,
) -> ElmSyntaxNode<&ElmSyntaxType> {
    match elm_syntax_type.value {
        ElmSyntaxType::Parenthesized(in_parens) => {
            elm_syntax_type_to_unparenthesized(elm_syntax_node_unbox(in_parens))
        }
        _ => elm_syntax_type,
    }
}

fn next_indent(current_indent: usize) -> usize {
    (current_indent + 1).next_multiple_of(4)
}

fn elm_syntax_type_not_parenthesized_into<'a>(
    so_far: &mut String,
    indent: usize,
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    type_node: ElmSyntaxNode<&'a ElmSyntaxType>,
) {
    match type_node.value {
        ElmSyntaxType::Construct {
            reference,
            arguments,
        } => {
            let line_span: LineSpan = elm_syntax_range_line_span(type_node.range, comments);
            let assigned_qualification: &str = assign_qualification(ElmQualified {
                qualification: &reference.value.qualification,
                name: &reference.value.name,
            });
            if !assigned_qualification.is_empty() {
                so_far.push_str(assigned_qualification);
                so_far.push('.');
            }
            so_far.push_str(&reference.value.name);
            let mut previous_syntax_end: lsp_types::Position = reference.range.end;
            for argument_node in arguments {
                space_or_linebreak_indented_into(so_far, line_span, next_indent(indent));
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_syntax_end,
                            end: argument_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_parenthesized_if_space_separated_into(
                    so_far,
                    next_indent(indent),
                    assign_qualification,
                    comments,
                    argument_node.range,
                    elm_syntax_type_to_unparenthesized(elm_syntax_node_as_ref(argument_node)),
                );
                previous_syntax_end = argument_node.range.end;
            }
        }
        ElmSyntaxType::Function {
            input,
            arrow_key_symbol_range: _,
            output: maybe_output,
        } => {
            let input_unparenthesized: ElmSyntaxNode<&ElmSyntaxType> =
                elm_syntax_type_to_unparenthesized(elm_syntax_node_unbox(input));
            match input_unparenthesized.value {
                ElmSyntaxType::Function { .. } => {
                    elm_syntax_type_parenthesized_into(
                        so_far,
                        indent,
                        assign_qualification,
                        comments,
                        input_unparenthesized.range,
                        input_unparenthesized,
                    );
                }
                _ => {
                    elm_syntax_type_not_parenthesized_into(
                        so_far,
                        indent,
                        assign_qualification,
                        comments,
                        elm_syntax_node_unbox(input),
                    );
                }
            }
            space_or_linebreak_indented_into(
                so_far,
                elm_syntax_range_line_span(type_node.range, comments),
                indent,
            );
            let comments_around_arrow: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: input.range.end,
                        end: maybe_output
                            .as_ref()
                            .map(|node| node.range.start)
                            .unwrap_or(type_node.range.end),
                    },
                );
            if let Some(output_node) = maybe_output {
                so_far.push_str("->");
                space_or_linebreak_indented_into(
                    so_far,
                    if comments_around_arrow.is_empty() {
                        elm_syntax_range_line_span(output_node.range, comments)
                    } else {
                        LineSpan::Multiple
                    },
                    next_indent(indent),
                );
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    next_indent(indent),
                    comments_around_arrow,
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    next_indent(indent),
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(output_node),
                );
            } else {
                if !comments_around_arrow.is_empty() {
                    linebreak_indented_into(so_far, indent);
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        indent,
                        comments_around_arrow,
                    );
                }
                so_far.push_str("-> ");
            }
        }
        ElmSyntaxType::Parenthesized(in_parens) => {
            let innermost: ElmSyntaxNode<&ElmSyntaxType> =
                elm_syntax_type_to_unparenthesized(elm_syntax_node_unbox(in_parens));
            let comments_before_innermost: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: type_node.range.start,
                        end: innermost.range.start,
                    },
                );
            let comments_after_innermost: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: innermost.range.end,
                        end: type_node.range.end,
                    },
                );
            if comments_before_innermost.is_empty() && comments_after_innermost.is_empty() {
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent,
                    assign_qualification,
                    comments,
                    innermost,
                );
            } else {
                elm_syntax_type_parenthesized_into(
                    so_far,
                    indent,
                    assign_qualification,
                    comments,
                    type_node.range,
                    innermost,
                );
            }
        }
        ElmSyntaxType::Record(fields) => match fields.split_first() {
            None => {
                let comments_in_curlies: &[ElmSyntaxNode<ElmSyntaxComment>] =
                    elm_syntax_comments_in_range(comments, type_node.range);
                if comments_in_curlies.is_empty() {
                    so_far.push_str("{}");
                } else {
                    so_far.push('{');
                    elm_syntax_comments_into(so_far, indent + 1, comments);
                    linebreak_indented_into(so_far, indent);
                    so_far.push('}');
                }
            }
            Some((field0, field1_up)) => {
                let line_span: LineSpan = elm_syntax_range_line_span(type_node.range, comments);
                so_far.push_str("{ ");
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: type_node.range.start,
                            end: field0.name.range.start,
                        },
                    ),
                );
                let previous_syntax_end: lsp_types::Position = elm_syntax_type_fields_into_string(
                    so_far,
                    indent,
                    assign_qualification,
                    comments,
                    line_span,
                    field0,
                    field1_up,
                );
                space_or_linebreak_indented_into(so_far, line_span, indent);
                let comments_before_closing_curly = elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_syntax_end,
                        end: type_node.range.end,
                    },
                );
                if !comments_before_closing_curly.is_empty() {
                    linebreak_indented_into(so_far, indent);
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        indent,
                        comments_before_closing_curly,
                    );
                }
                so_far.push('}');
            }
        },
        ElmSyntaxType::RecordExtension {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            let line_span: LineSpan = elm_syntax_range_line_span(type_node.range, comments);
            so_far.push_str("{ ");
            let mut previous_syntax_end: lsp_types::Position = type_node.range.start;
            if let Some(record_variable_node) = maybe_record_variable {
                so_far.push_str(&record_variable_node.value);
                previous_syntax_end = record_variable_node.range.end;
            }
            if let Some((field0, field1_up)) = fields.split_first() {
                space_or_linebreak_indented_into(so_far, line_span, indent);
                so_far.push_str("| ");
                previous_syntax_end = elm_syntax_type_fields_into_string(
                    so_far,
                    indent,
                    assign_qualification,
                    comments,
                    line_span,
                    field0,
                    field1_up,
                );
            }
            space_or_linebreak_indented_into(so_far, line_span, indent);
            let comments_before_closing_curly = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: type_node.range.end,
                },
            );
            if !comments_before_closing_curly.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    comments_before_closing_curly,
                );
            }
            so_far.push('}');
        }
        ElmSyntaxType::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            let line_span: LineSpan = elm_syntax_range_line_span(type_node.range, comments);
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: type_node.range.start,
                            end: part0_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: maybe_part0
                                .as_ref()
                                .map(|node| node.range.end)
                                .unwrap_or_else(|| type_node.range.start),
                            end: part1_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: maybe_part0
                                .as_ref()
                                .or(maybe_part1.as_ref())
                                .map(|node| node.range.end)
                                .unwrap_or_else(|| type_node.range.start),
                            end: part2_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(part2_node),
                );
            }
            space_or_linebreak_indented_into(so_far, line_span, indent);
            let comments_before_closing_paren = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: maybe_part0
                        .as_ref()
                        .or(maybe_part1.as_ref())
                        .or(maybe_part2.as_ref())
                        .map(|node| node.range.end)
                        .unwrap_or_else(|| type_node.range.start),
                    end: type_node.range.end,
                },
            );
            if !comments_before_closing_paren.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    comments_before_closing_paren,
                );
            }
            so_far.push(')');
        }
        ElmSyntaxType::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            let line_span: LineSpan = elm_syntax_range_line_span(type_node.range, comments);
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: type_node.range.start,
                            end: part0_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: maybe_part0
                                .as_ref()
                                .map(|node| node.range.end)
                                .unwrap_or_else(|| type_node.range.start),
                            end: part1_node.range.start,
                        },
                    ),
                );
                elm_syntax_type_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    assign_qualification,
                    comments,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            space_or_linebreak_indented_into(so_far, line_span, indent);
            let comments_before_closing_paren = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: maybe_part0
                        .as_ref()
                        .or(maybe_part1.as_ref())
                        .map(|node| node.range.end)
                        .unwrap_or_else(|| type_node.range.start),
                    end: type_node.range.end,
                },
            );
            if !comments_before_closing_paren.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    comments_before_closing_paren,
                );
            }
            so_far.push(')');
        }
        ElmSyntaxType::Unit => {
            let comments_in_unit = elm_syntax_comments_in_range(comments, type_node.range);
            if comments_in_unit.is_empty() {
                so_far.push_str("()");
            } else {
                so_far.push('(');
                elm_syntax_comments_into(so_far, indent + 1, comments_in_unit);
                linebreak_indented_into(so_far, indent);
                so_far.push(')');
            }
        }
        ElmSyntaxType::Variable(name) => {
            so_far.push_str(name);
        }
    }
}
fn elm_syntax_type_parenthesized_into<'a>(
    so_far: &mut String,
    indent: usize,
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    full_range: lsp_types::Range,
    innermost: ElmSyntaxNode<&'a ElmSyntaxType>,
) {
    let comments_before_innermost = elm_syntax_comments_in_range(
        comments,
        lsp_types::Range {
            start: full_range.start,
            end: innermost.range.start,
        },
    );
    let comments_after_innermost = elm_syntax_comments_in_range(
        comments,
        lsp_types::Range {
            start: innermost.range.end,
            end: full_range.end,
        },
    );
    so_far.push('(');
    elm_syntax_comments_then_linebreak_indented_into(so_far, indent + 1, comments_before_innermost);
    elm_syntax_comments_then_linebreak_indented_into(so_far, indent + 1, comments_after_innermost);
    let so_far_length_before_innermost: usize = so_far.len();
    elm_syntax_type_not_parenthesized_into(
        so_far,
        indent + 1,
        assign_qualification,
        comments,
        innermost,
    );
    if so_far[so_far_length_before_innermost..].contains('\n') {
        linebreak_indented_into(so_far, indent);
    }
    so_far.push(')');
}
fn elm_syntax_type_parenthesized_if_space_separated_into<'a>(
    so_far: &mut String,
    indent: usize,
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    full_range: lsp_types::Range,
    unparenthesized: ElmSyntaxNode<&'a ElmSyntaxType>,
) {
    let is_space_separated: bool = match unparenthesized.value {
        ElmSyntaxType::Unit
        | ElmSyntaxType::Variable(_)
        | ElmSyntaxType::Parenthesized(_)
        | ElmSyntaxType::Tuple { .. }
        | ElmSyntaxType::Triple { .. }
        | ElmSyntaxType::Record(_)
        | ElmSyntaxType::RecordExtension { .. } => false,
        ElmSyntaxType::Function { .. } => true,
        ElmSyntaxType::Construct {
            reference: _,
            arguments,
        } => !arguments.is_empty(),
    };
    if is_space_separated {
        elm_syntax_type_parenthesized_into(
            so_far,
            indent,
            assign_qualification,
            comments,
            full_range,
            unparenthesized,
        );
    } else {
        elm_syntax_type_not_parenthesized_into(
            so_far,
            indent,
            assign_qualification,
            comments,
            unparenthesized,
        );
    }
}
/// returns the last syntax end position
fn elm_syntax_type_fields_into_string<'a>(
    so_far: &mut String,
    indent: usize,
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    line_span: LineSpan,
    field0: &'a ElmSyntaxTypeField,
    field1_up: &'a [ElmSyntaxTypeField],
) -> lsp_types::Position {
    so_far.push_str(&field0.name.value);
    let mut previous_syntax_end: lsp_types::Position = field0.name.range.end;
    so_far.push_str(" :");
    if let Some(field0_value_node) = &field0.value {
        let comments_before_field0_value = elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: field0.name.range.end,
                end: field0_value_node.range.start,
            },
        );
        space_or_linebreak_indented_into(
            so_far,
            if comments_before_field0_value.is_empty() {
                elm_syntax_range_line_span(
                    lsp_types::Range {
                        start: field0.name.range.end,
                        end: field0_value_node.range.end,
                    },
                    comments,
                )
            } else {
                LineSpan::Multiple
            },
            next_indent(indent + 2),
        );
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent + 2),
            comments_before_field0_value,
        );
        elm_syntax_type_not_parenthesized_into(
            so_far,
            next_indent(indent + 2),
            assign_qualification,
            comments,
            elm_syntax_node_as_ref(field0_value_node),
        );
        previous_syntax_end = field0_value_node.range.end;
    }
    for field in field1_up {
        if line_span == LineSpan::Multiple {
            linebreak_indented_into(so_far, indent);
        }
        so_far.push_str(", ");
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            indent + 2,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: field.name.range.start,
                },
            ),
        );
        so_far.push_str(&field.name.value);
        previous_syntax_end = field.name.range.end;
        so_far.push_str(" :");
        if let Some(field_value_node) = &field.value {
            let comments_before_field_value = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: field.name.range.end,
                    end: field_value_node.range.start,
                },
            );
            space_or_linebreak_indented_into(
                so_far,
                if comments_before_field_value.is_empty() {
                    elm_syntax_range_line_span(
                        lsp_types::Range {
                            start: field.name.range.end,
                            end: field_value_node.range.end,
                        },
                        comments,
                    )
                } else {
                    LineSpan::Multiple
                },
                next_indent(indent + 2),
            );
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                next_indent(indent + 2),
                comments_before_field_value,
            );
            elm_syntax_type_not_parenthesized_into(
                so_far,
                next_indent(indent + 2),
                assign_qualification,
                comments,
                elm_syntax_node_as_ref(field_value_node),
            );
            previous_syntax_end = field_value_node.range.end;
        }
    }
    previous_syntax_end
}
fn elm_syntax_pattern_not_parenthesized_into(
    so_far: &mut String,
    pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
) {
    match pattern_node.value {
        ElmSyntaxPattern::Unit => {
            so_far.push_str("()");
        }
        ElmSyntaxPattern::Ignored => {
            so_far.push('_');
        }
        ElmSyntaxPattern::Char(maybe_char) => elm_char_into(so_far, *maybe_char),
        ElmSyntaxPattern::Int {
            base,
            value: value_or_err,
        } => {
            elm_int_into(so_far, *base, value_or_err);
        }
        ElmSyntaxPattern::String {
            content,
            quoting_style,
        } => elm_string_into(so_far, *quoting_style, content),
        ElmSyntaxPattern::Variable(name) => {
            so_far.push_str(name);
        }
        ElmSyntaxPattern::As {
            pattern,
            as_keyword_range: _,
            variable: maybe_variable,
        } => {
            elm_syntax_pattern_not_parenthesized_into(so_far, elm_syntax_node_unbox(pattern));
            so_far.push_str(" as ");
            if let Some(variable_node) = maybe_variable {
                so_far.push_str(&variable_node.value);
            }
        }
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_not_parenthesized_into(so_far, elm_syntax_node_unbox(in_parens));
        }
        ElmSyntaxPattern::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            so_far.push_str(" )");
        }
        ElmSyntaxPattern::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            so_far.push_str("( ");
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            so_far.push_str(", ");
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            so_far.push_str(", ");
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_unbox(part2_node),
                );
            }
            so_far.push_str(" )");
        }
        ElmSyntaxPattern::ListCons {
            head: maybe_head,
            cons_key_symbol: _,
            tail: maybe_tail,
        } => {
            if let Some(head_node) = maybe_head {
                elm_syntax_pattern_parenthesized_if_space_separated_into(
                    so_far,
                    elm_syntax_node_unbox(head_node),
                );
            }
            so_far.push_str(" :: ");
            if let Some(tail_node) = maybe_tail {
                match tail_node.value.as_ref() {
                    ElmSyntaxPattern::As { .. } => {
                        elm_syntax_pattern_parenthesized_into(
                            so_far,
                            elm_syntax_node_unbox(tail_node),
                        );
                    }
                    _ => {
                        elm_syntax_pattern_not_parenthesized_into(
                            so_far,
                            elm_syntax_node_unbox(tail_node),
                        );
                    }
                }
            }
        }
        ElmSyntaxPattern::ListExact(elements) => {
            let mut elements_iterator = elements.iter();
            match elements_iterator.next() {
                None => {
                    so_far.push_str("[]");
                }
                Some(element_node0) => {
                    so_far.push_str("[ ");
                    elm_syntax_pattern_not_parenthesized_into(
                        so_far,
                        elm_syntax_node_as_ref(element_node0),
                    );
                    for element_node in elements_iterator {
                        so_far.push_str(", ");
                        elm_syntax_pattern_not_parenthesized_into(
                            so_far,
                            elm_syntax_node_as_ref(element_node),
                        );
                    }
                    so_far.push_str(" ]");
                }
            }
        }
        ElmSyntaxPattern::Record(field_names) => {
            let mut field_names_iterator = field_names.iter();
            match field_names_iterator.next() {
                None => {
                    so_far.push_str("{}");
                }
                Some(field0_name) => {
                    so_far.push_str("{ ");
                    so_far.push_str(&field0_name.value);
                    for field_name in field_names_iterator {
                        so_far.push_str(", ");
                        so_far.push_str(&field_name.value);
                    }
                    so_far.push_str(" }");
                }
            }
        }
        ElmSyntaxPattern::Variant { reference, values } => {
            elm_qualified_name_into(so_far, &reference.value);
            for value_node in values {
                so_far.push(' ');
                elm_syntax_pattern_parenthesized_if_space_separated_into(
                    so_far,
                    elm_syntax_node_as_ref(value_node),
                );
            }
        }
    }
}
fn elm_qualified_name_into(so_far: &mut String, qualified: &ElmQualifiedName) {
    if !qualified.qualification.is_empty() {
        so_far.push_str(&qualified.qualification);
        so_far.push('.');
    }
    so_far.push_str(&qualified.name);
}
fn elm_syntax_pattern_parenthesized_into(
    so_far: &mut String,
    pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
) {
    so_far.push('(');
    elm_syntax_pattern_not_parenthesized_into(so_far, pattern_node);
    so_far.push(')');
}
fn elm_syntax_pattern_parenthesized_if_space_separated_into(
    so_far: &mut String,
    pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
) {
    match pattern_node.value {
        ElmSyntaxPattern::As { .. } | ElmSyntaxPattern::ListCons { .. } => {
            elm_syntax_pattern_parenthesized_into(so_far, pattern_node);
        }
        _ => {
            elm_syntax_pattern_not_parenthesized_into(so_far, pattern_node);
        }
    }
}
fn elm_char_into(so_far: &mut String, maybe_char: Option<char>) {
    match maybe_char {
        None => {
            so_far.push_str("''");
        }
        Some(char) => {
            so_far.push('\'');
            match char {
                '\'' => so_far.push_str("\\'"),
                '\\' => so_far.push_str("\\\\"),
                '\t' => so_far.push_str("\\t"),
                '\n' => so_far.push_str("\\n"),
                '\u{000D}' => so_far.push_str("\\u{000D}"),
                other_character => {
                    if elm_char_needs_unicode_escaping(other_character) {
                        elm_unicode_char_escape_into(so_far, other_character);
                    } else {
                        so_far.push(other_character);
                    }
                }
            }
            so_far.push('\'');
        }
    }
}
fn elm_char_needs_unicode_escaping(char: char) -> bool {
    // I'm aware this isn't the exact criterion that elm-format uses
    // (something something separators, private use, unassigned, ?)
    (char.len_utf16() >= 2) || char.is_control()
}
fn elm_unicode_char_escape_into(so_far: &mut String, char: char) {
    for utf16_code in char.encode_utf16(&mut [0; 2]) {
        use std::fmt::Write as _;
        let _ = write!(so_far, "\\u{{{:04X}}}", utf16_code);
    }
}
fn elm_int_into(so_far: &mut String, base: ElmSyntaxIntBase, value_or_err: &Result<i64, Box<str>>) {
    match value_or_err {
        Err(value_as_string) => match base {
            ElmSyntaxIntBase::IntBase10 => {
                so_far.push_str(value_as_string);
            }
            ElmSyntaxIntBase::IntBase16 => {
                so_far.push_str("0x");
                so_far.push_str(value_as_string);
            }
        },
        &Ok(value) => match base {
            ElmSyntaxIntBase::IntBase10 => {
                use std::fmt::Write as _;
                let _ = write!(so_far, "{}", value);
            }
            ElmSyntaxIntBase::IntBase16 => {
                use std::fmt::Write as _;
                let _ = write!(so_far, "0x{:02x}", value);
                if value <= 0xFF {
                    use std::fmt::Write as _;
                    let _ = write!(so_far, "\\u{{{:02X}}}", value);
                } else if value <= 0xFFFF {
                    use std::fmt::Write as _;
                    let _ = write!(so_far, "\\u{{{:04X}}}", value);
                } else if value <= 0xFFFF_FFFF {
                    use std::fmt::Write as _;
                    let _ = write!(so_far, "\\u{{{:08X}}}", value);
                } else {
                    use std::fmt::Write as _;
                    let _ = write!(so_far, "\\u{{{:016X}}}", value);
                }
            }
        },
    }
}
fn elm_string_into(so_far: &mut String, quoting_style: ElmSyntaxStringQuotingStyle, content: &str) {
    match quoting_style {
        ElmSyntaxStringQuotingStyle::SingleQuoted => {
            so_far.push('"');
            for char in content.chars() {
                match char {
                    '\"' => so_far.push_str("\\\""),
                    '\\' => so_far.push_str("\\\\"),
                    '\t' => so_far.push_str("\\t"),
                    '\n' => so_far.push_str("\\n"),
                    '\u{000D}' => so_far.push_str("\\u{000D}"),
                    other_character => {
                        if elm_char_needs_unicode_escaping(other_character) {
                            elm_unicode_char_escape_into(so_far, other_character);
                        } else {
                            so_far.push(other_character);
                        }
                    }
                }
            }
            so_far.push('"');
        }
        ElmSyntaxStringQuotingStyle::TripleQuoted => {
            so_far.push_str("\"\"\"");
            // because only quotes connected to the ending """ should be escaped to \"
            let mut quote_count_to_insert: usize = 0;
            'pushing_escaped_content: for char in content.chars() {
                if char == '\"' {
                    quote_count_to_insert += 1;
                    continue 'pushing_escaped_content;
                }
                so_far.extend(std::iter::repeat_n('\"', quote_count_to_insert));
                match char {
                    '\\' => so_far.push_str("\\\\"),
                    '\t' => so_far.push_str("\\t"),
                    '\r' => so_far.push('\r'),
                    '\n' => so_far.push('\n'),
                    '\"' => {
                        quote_count_to_insert += 1;
                    }
                    other_character => {
                        if elm_char_needs_unicode_escaping(other_character) {
                            elm_unicode_char_escape_into(so_far, other_character);
                        } else {
                            so_far.push(other_character);
                        }
                    }
                }
            }
            so_far.extend(std::iter::repeat_n("\\\"", quote_count_to_insert));
            so_far.push_str("\"\"\"");
        }
    }
}
fn elm_syntax_expression_not_parenthesized_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    match expression_node.value {
        ElmSyntaxExpression::Unit => {
            let comments_in_unit = elm_syntax_comments_in_range(comments, expression_node.range);
            if comments_in_unit.is_empty() {
                so_far.push_str("()");
            } else {
                so_far.push('(');
                elm_syntax_comments_into(so_far, indent + 1, comments_in_unit);
                linebreak_indented_into(so_far, indent);
                so_far.push(')');
            }
        }
        ElmSyntaxExpression::Call {
            called: called_node,
            argument0: argument0_node,
            argument1_up,
        } => {
            let comments_before_argument0: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: called_node.range.end,
                        end: argument0_node.range.start,
                    },
                );
            let line_span_before_argument0: LineSpan = if comments_before_argument0.is_empty()
                && elm_syntax_expression_line_span(comments, elm_syntax_node_unbox(called_node))
                    == LineSpan::Single
                && elm_syntax_expression_line_span(comments, elm_syntax_node_unbox(argument0_node))
                    == LineSpan::Single
            {
                LineSpan::Single
            } else {
                LineSpan::Multiple
            };
            let full_line_span: LineSpan = match line_span_before_argument0 {
                LineSpan::Multiple => LineSpan::Multiple,
                LineSpan::Single => elm_syntax_expression_line_span(comments, expression_node),
            };
            elm_syntax_expression_parenthesized_if_space_separated_into(
                so_far,
                indent,
                comments,
                elm_syntax_node_unbox(called_node),
            );
            space_or_linebreak_indented_into(
                so_far,
                line_span_before_argument0,
                next_indent(indent),
            );
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                next_indent(indent),
                comments_before_argument0,
            );
            elm_syntax_expression_parenthesized_if_space_separated_into(
                so_far,
                next_indent(indent),
                comments,
                elm_syntax_node_unbox(argument0_node),
            );
            let mut previous_syntax_end: lsp_types::Position = argument0_node.range.end;
            for argument_node in argument1_up.iter().map(elm_syntax_node_as_ref) {
                space_or_linebreak_indented_into(so_far, full_line_span, next_indent(indent));
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_syntax_end,
                            end: argument_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_parenthesized_if_space_separated_into(
                    so_far,
                    next_indent(indent),
                    comments,
                    argument_node,
                );
                previous_syntax_end = argument_node.range.end;
            }
        }
        ElmSyntaxExpression::CaseOf {
            matched: maybe_matched,
            of_keyword_range: maybe_of_keyword_range,
            cases,
        } => {
            so_far.push_str("case");
            let previous_syntax_that_covered_comments_end: lsp_types::Position;
            match maybe_matched {
                None => match maybe_of_keyword_range {
                    None => {
                        so_far.push_str("  ");
                        previous_syntax_that_covered_comments_end = expression_node.range.start;
                    }
                    Some(of_keyword_range) => {
                        let comments_between_case_and_of_keywords: &[ElmSyntaxNode<
                            ElmSyntaxComment,
                        >] = elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: expression_node.range.start,
                                end: of_keyword_range.end,
                            },
                        );
                        if comments_between_case_and_of_keywords.is_empty() {
                            so_far.push_str("  ");
                        } else {
                            linebreak_indented_into(so_far, next_indent(indent));
                            elm_syntax_comments_into(
                                so_far,
                                next_indent(indent),
                                comments_between_case_and_of_keywords,
                            );
                            linebreak_indented_into(so_far, indent);
                        }
                        previous_syntax_that_covered_comments_end = of_keyword_range.end;
                    }
                },
                Some(matched_node) => {
                    let comments_before_matched: &[ElmSyntaxNode<ElmSyntaxComment>] =
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: expression_node.range.start,
                                end: matched_node.range.start,
                            },
                        );
                    let comments_before_of_keyword: &[ElmSyntaxNode<ElmSyntaxComment>] = if cases
                        .is_empty()
                        && let Some(of_keyword_range) = maybe_of_keyword_range
                    {
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: matched_node.range.start,
                                end: of_keyword_range.start,
                            },
                        )
                    } else {
                        &[]
                    };
                    let before_cases_line_span: LineSpan = if comments_before_matched.is_empty()
                        && comments_before_of_keyword.is_empty()
                    {
                        elm_syntax_expression_line_span(
                            comments,
                            elm_syntax_node_unbox(matched_node),
                        )
                    } else {
                        LineSpan::Multiple
                    };
                    space_or_linebreak_indented_into(
                        so_far,
                        before_cases_line_span,
                        next_indent(indent),
                    );
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        next_indent(indent),
                        comments_before_matched,
                    );
                    elm_syntax_expression_not_parenthesized_into(
                        so_far,
                        next_indent(indent),
                        comments,
                        elm_syntax_node_unbox(matched_node),
                    );
                    space_or_linebreak_indented_into(so_far, before_cases_line_span, indent);
                    if let Some(of_keyword_range) = maybe_of_keyword_range
                        && !comments_before_of_keyword.is_empty()
                    {
                        linebreak_indented_into(so_far, indent);
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            next_indent(indent),
                            comments_before_matched,
                        );
                        previous_syntax_that_covered_comments_end = of_keyword_range.end;
                    } else {
                        previous_syntax_that_covered_comments_end = matched_node.range.end;
                    }
                }
            }
            so_far.push_str("of");
            linebreak_indented_into(so_far, next_indent(indent));
            if let Some((case0, case1_up)) = cases.split_first() {
                let mut previous_syntax_end: lsp_types::Position = elm_syntax_case_into(
                    so_far,
                    next_indent(indent),
                    comments,
                    previous_syntax_that_covered_comments_end,
                    case0,
                );
                for case in case1_up {
                    so_far.push('\n');
                    linebreak_indented_into(so_far, next_indent(indent));
                    previous_syntax_end = elm_syntax_case_into(
                        so_far,
                        next_indent(indent),
                        comments,
                        previous_syntax_end,
                        case,
                    );
                }
            }
        }
        ElmSyntaxExpression::Char(maybe_char) => {
            elm_char_into(so_far, *maybe_char);
        }
        ElmSyntaxExpression::Float(value_or_whatever) => match value_or_whatever {
            Err(whatever) => {
                so_far.push_str(whatever);
            }
            Ok(value) => {
                use std::fmt::Write as _;
                let _ = write!(so_far, "{}", *value);
            }
        },
        ElmSyntaxExpression::IfThenElse {
            condition: maybe_condition,
            then_keyword_range: maybe_then_keyword_range,
            on_true: maybe_on_true,
            else_keyword_range: maybe_else_keyword_range,
            on_false: maybe_on_false,
        } => {
            so_far.push_str("if");
            let until_condition: lsp_types::Position = maybe_condition
                .as_ref()
                .map(|node| node.range.start)
                .unwrap_or(expression_node.range.start);
            let before_then_keyword: lsp_types::Position = maybe_then_keyword_range
                .map(|range| range.start)
                .or_else(|| maybe_condition.as_ref().map(|node| node.range.end))
                .unwrap_or(expression_node.range.start);
            let after_on_true: lsp_types::Position = maybe_on_true
                .as_ref()
                .map(|node| node.range.end)
                .unwrap_or(before_then_keyword);
            let comments_before_condition = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: expression_node.range.start,
                    end: until_condition,
                },
            );
            let comments_before_then_keyword: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: expression_node.range.start,
                        end: before_then_keyword,
                    },
                );
            let before_condition_line_span: LineSpan = if comments_before_condition.is_empty()
                && comments_before_then_keyword.is_empty()
            {
                match maybe_condition {
                    None => LineSpan::Single,
                    Some(condition_node) => elm_syntax_expression_line_span(
                        comments,
                        elm_syntax_node_unbox(condition_node),
                    ),
                }
            } else {
                LineSpan::Multiple
            };
            space_or_linebreak_indented_into(
                so_far,
                before_condition_line_span,
                next_indent(indent),
            );
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                next_indent(indent),
                comments_before_condition,
            );
            if let Some(condition_node) = maybe_condition {
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    next_indent(indent),
                    comments,
                    elm_syntax_node_unbox(condition_node),
                );
            }
            space_or_linebreak_indented_into(so_far, before_condition_line_span, indent);
            if !comments_before_then_keyword.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    comments_before_then_keyword,
                );
            }
            so_far.push_str("then");
            linebreak_indented_into(so_far, next_indent(indent));
            if let Some(on_true_node) = maybe_on_true {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: before_then_keyword,
                            end: on_true_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    next_indent(indent),
                    comments,
                    elm_syntax_node_unbox(on_true_node),
                );
                so_far.push('\n');
            }
            linebreak_indented_into(so_far, indent);
            if maybe_on_false.is_none()
                && let Some(else_keyword_range) = maybe_else_keyword_range
            {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: after_on_true,
                            end: else_keyword_range.start,
                        },
                    ),
                );
            }
            so_far.push_str("else");
            match maybe_on_false {
                None => {
                    linebreak_indented_into(so_far, next_indent(indent));
                }
                Some(on_false_node) => {
                    let on_false_innermost: ElmSyntaxNode<&ElmSyntaxExpression> =
                        elm_syntax_expression_to_unparenthesized(elm_syntax_node_unbox(
                            on_false_node,
                        ));
                    let comments_after_on_false_innermost: &[ElmSyntaxNode<ElmSyntaxComment>] =
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: on_false_innermost.range.end,
                                end: on_false_node.range.end,
                            },
                        );
                    if comments_after_on_false_innermost.is_empty()
                        && let ElmSyntaxExpression::IfThenElse { .. } = on_false_innermost.value
                    {
                        let comments_before_on_false_innermost: &[ElmSyntaxNode<
                            ElmSyntaxComment,
                        >] = elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: on_false_node.range.start,
                                end: on_false_innermost.range.start,
                            },
                        );
                        space_or_linebreak_indented_into(
                            so_far,
                            if comments_before_on_false_innermost.is_empty() {
                                LineSpan::Single
                            } else {
                                LineSpan::Multiple
                            },
                            indent,
                        );
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            indent,
                            comments_before_on_false_innermost,
                        );
                        // elm-format here _forces_ the if...then to span
                        // multiple lines which to me seems like a bug
                        elm_syntax_expression_not_parenthesized_into(
                            so_far,
                            indent,
                            comments,
                            on_false_innermost,
                        );
                    } else {
                        linebreak_indented_into(so_far, next_indent(indent));
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            next_indent(indent),
                            elm_syntax_comments_in_range(
                                comments,
                                lsp_types::Range {
                                    start: after_on_true,
                                    end: on_false_node.range.start,
                                },
                            ),
                        );
                        elm_syntax_expression_not_parenthesized_into(
                            so_far,
                            next_indent(indent),
                            comments,
                            elm_syntax_node_unbox(on_false_node),
                        );
                    }
                }
            }
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left: left_node,
            operator: operator_node,
            right: maybe_right,
        } => {
            /* elm-format has some _very_ weird formatting for <| pipelines, e.g.
            this is real formatted code:

            f <|
                g
                    x
                <|
                    h <|
                        i

            like what?! Instead, we just format it like any other operator
            */
            let line_span: LineSpan = elm_syntax_expression_line_span(comments, expression_node);
            elm_syntax_expression_parenthesized_if_not_call_but_space_separated_into(
                so_far,
                indent,
                comments,
                elm_syntax_node_unbox(left_node),
            );
            match maybe_right {
                None => {
                    space_or_linebreak_indented_into(so_far, line_span, next_indent(indent));
                    let comments_before_operator = elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: left_node.range.end,
                            end: operator_node.range.start,
                        },
                    );
                    if !comments_before_operator.is_empty() {
                        linebreak_indented_into(so_far, next_indent(indent));
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            next_indent(indent),
                            comments_before_operator,
                        );
                    }
                    so_far.push_str(operator_node.value);
                }
                Some(right_node) => {
                    space_or_linebreak_indented_into(so_far, line_span, next_indent(indent));
                    so_far.push_str(operator_node.value);
                    so_far.push(' ');
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        next_indent(indent) + operator_node.value.len() + 1,
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: left_node.range.end,
                                end: right_node.range.start,
                            },
                        ),
                    );
                    let mut previous_operator: &str = operator_node.value;
                    let mut next_right_node: &ElmSyntaxNode<Box<ElmSyntaxExpression>> = right_node;
                    'format_infix_operation_chain: loop {
                        match next_right_node.value.as_ref() {
                            ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
                                left: next_right_left_node,
                                operator: next_right_operator_node,
                                right: maybe_next_right_right,
                            } => {
                                elm_syntax_expression_parenthesized_if_not_call_but_space_separated_into(
                                    so_far,
                                    next_indent(indent) + next_right_operator_node.value.len() + 1,
                                    comments,
                                    elm_syntax_node_unbox(next_right_left_node),
                                );
                                space_or_linebreak_indented_into(
                                    so_far,
                                    line_span,
                                    next_indent(indent),
                                );
                                match maybe_next_right_right {
                                    None => {
                                        linebreak_indented_into(so_far, next_indent(indent));
                                        elm_syntax_comments_then_linebreak_indented_into(
                                            so_far,
                                            next_indent(indent),
                                            elm_syntax_comments_in_range(
                                                comments,
                                                lsp_types::Range {
                                                    start: next_right_left_node.range.end,
                                                    end: next_right_operator_node.range.start,
                                                },
                                            ),
                                        );
                                        so_far.push_str(next_right_operator_node.value);
                                        break 'format_infix_operation_chain;
                                    }
                                    Some(right_right_node) => {
                                        so_far.push_str(next_right_operator_node.value);
                                        so_far.push(' ');
                                        elm_syntax_comments_then_linebreak_indented_into(
                                            so_far,
                                            next_indent(indent)
                                                + next_right_operator_node.value.len()
                                                + 1,
                                            elm_syntax_comments_in_range(
                                                comments,
                                                lsp_types::Range {
                                                    start: next_right_left_node.range.end,
                                                    end: right_right_node.range.start,
                                                },
                                            ),
                                        );
                                        previous_operator = next_right_operator_node.value;
                                        next_right_node = right_right_node;
                                    }
                                }
                            }
                            _ => {
                                elm_syntax_expression_parenthesized_if_not_call_but_space_separated_into(
                                    so_far,
                                    next_indent(indent) + previous_operator.len() + 1,
                                    comments,
                                    elm_syntax_node_unbox(next_right_node),
                                );
                                break 'format_infix_operation_chain;
                            }
                        }
                    }
                }
            }
        }
        ElmSyntaxExpression::Integer {
            base,
            value: value_or_err,
        } => {
            elm_int_into(so_far, *base, value_or_err);
        }
        ElmSyntaxExpression::Lambda {
            parameters,
            arrow_key_symbol_range: maybe_arrow_key_symbol_range,
            result: maybe_result,
        } => {
            so_far.push('\\');
            let parameter_comments = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: expression_node.range.start,
                    end: if maybe_result.is_none()
                        && let Some(arrow_key_symbol_range) = maybe_arrow_key_symbol_range
                    {
                        arrow_key_symbol_range.end
                    } else {
                        parameters
                            .last()
                            .map(|node| node.range.end)
                            .unwrap_or(expression_node.range.start)
                    },
                },
            );
            let mut previous_parameter_end: lsp_types::Position = expression_node.range.start;
            if let Some((parameter0_node, parameter1_up)) = parameters.split_first() {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 1,
                    elm_syntax_comments_in_range(
                        parameter_comments,
                        lsp_types::Range {
                            start: previous_parameter_end,
                            end: parameter0_node.range.start,
                        },
                    ),
                );
                elm_syntax_pattern_not_parenthesized_into(
                    so_far,
                    elm_syntax_node_as_ref(parameter0_node),
                );
                let line_span: LineSpan = if parameter_comments.is_empty() {
                    LineSpan::Single
                } else {
                    LineSpan::Multiple
                };
                for parameter_node in parameter1_up {
                    space_or_linebreak_indented_into(so_far, line_span, indent + 1);
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        indent + 1,
                        elm_syntax_comments_in_range(
                            parameter_comments,
                            lsp_types::Range {
                                start: previous_parameter_end,
                                end: parameter_node.range.start,
                            },
                        ),
                    );
                    elm_syntax_pattern_not_parenthesized_into(
                        so_far,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                    previous_parameter_end = parameter_node.range.end;
                }
                space_or_linebreak_indented_into(so_far, line_span, indent);
                previous_parameter_end = parameter0_node.range.end;
            }
            if maybe_result.is_none()
                && let Some(arrow_key_symbol_range) = maybe_arrow_key_symbol_range
                && let comments_before_arrow_key_symbol = elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_parameter_end,
                        end: arrow_key_symbol_range.start,
                    },
                )
                && !comments_before_arrow_key_symbol.is_empty()
            {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    comments_before_arrow_key_symbol,
                );
            }
            so_far.push_str("->");
            space_or_linebreak_indented_into(
                so_far,
                elm_syntax_expression_line_span(comments, expression_node),
                next_indent(indent),
            );
            if let Some(result_node) = maybe_result {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_parameter_end,
                            end: result_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    next_indent(indent),
                    comments,
                    elm_syntax_node_unbox(result_node),
                );
            }
        }
        ElmSyntaxExpression::LetIn {
            declarations,
            in_keyword_range: maybe_in_keyword_range,
            result: maybe_result,
        } => {
            so_far.push_str("let");
            let mut previous_declaration_end: lsp_types::Position = expression_node.range.end;
            match declarations.split_last() {
                None => {
                    linebreak_indented_into(so_far, next_indent(indent));
                }
                Some((last_declaration_node, declarations_before_last)) => {
                    for declaration_node in declarations_before_last {
                        linebreak_indented_into(so_far, next_indent(indent));
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            next_indent(indent),
                            elm_syntax_comments_in_range(
                                comments,
                                lsp_types::Range {
                                    start: previous_declaration_end,
                                    end: declaration_node.range.start,
                                },
                            ),
                        );
                        elm_syntax_let_declaration_into(
                            so_far,
                            next_indent(indent),
                            comments,
                            elm_syntax_node_as_ref(declaration_node),
                        );
                        so_far.push('\n');
                        previous_declaration_end = declaration_node.range.end;
                    }
                    linebreak_indented_into(so_far, next_indent(indent));
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        next_indent(indent),
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: previous_declaration_end,
                                end: last_declaration_node.range.start,
                            },
                        ),
                    );
                    elm_syntax_let_declaration_into(
                        so_far,
                        next_indent(indent),
                        comments,
                        elm_syntax_node_as_ref(last_declaration_node),
                    );
                    previous_declaration_end = last_declaration_node.range.end;
                }
            }
            if let Some(in_keyword_range) = maybe_in_keyword_range {
                elm_syntax_comments_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_declaration_end,
                            end: in_keyword_range.start,
                        },
                    ),
                );
            }
            linebreak_indented_into(so_far, indent);
            so_far.push_str("in");
            linebreak_indented_into(so_far, indent);
            if let Some(result_node) = maybe_result {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: maybe_in_keyword_range
                                .map(|range| range.end)
                                .unwrap_or(previous_declaration_end),
                            end: result_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent,
                    comments,
                    elm_syntax_node_unbox(result_node),
                );
            }
        }
        ElmSyntaxExpression::List(elements) => {
            let comments: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(comments, expression_node.range);
            match elements.split_last() {
                None => {
                    if comments.is_empty() {
                        so_far.push_str("[]");
                    } else {
                        so_far.push('[');
                        elm_syntax_comments_into(so_far, indent + 1, comments);
                        linebreak_indented_into(so_far, indent);
                        so_far.push(']');
                    }
                }
                Some((last_element_node, elements_before_last)) => {
                    so_far.push_str("[ ");
                    let line_span: LineSpan =
                        elm_syntax_expression_line_span(comments, expression_node);
                    let mut previous_element_end: lsp_types::Position = expression_node.range.start;
                    for element_node in elements_before_last {
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            indent,
                            elm_syntax_comments_in_range(
                                comments,
                                lsp_types::Range {
                                    start: previous_element_end,
                                    end: element_node.range.start,
                                },
                            ),
                        );
                        elm_syntax_expression_not_parenthesized_into(
                            so_far,
                            indent + 2,
                            comments,
                            elm_syntax_node_as_ref(element_node),
                        );
                        if line_span == LineSpan::Multiple {
                            linebreak_indented_into(so_far, indent);
                        }
                        so_far.push_str(", ");
                        previous_element_end = element_node.range.end;
                    }
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        indent + 2,
                        elm_syntax_comments_in_range(
                            comments,
                            lsp_types::Range {
                                start: previous_element_end,
                                end: last_element_node.range.start,
                            },
                        ),
                    );
                    elm_syntax_expression_not_parenthesized_into(
                        so_far,
                        indent + 2,
                        comments,
                        elm_syntax_node_as_ref(last_element_node),
                    );
                    space_or_linebreak_indented_into(so_far, line_span, indent);
                    let comments_after_last_element = elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: last_element_node.range.end,
                            end: expression_node.range.end,
                        },
                    );
                    if !comments_after_last_element.is_empty() {
                        linebreak_indented_into(so_far, indent);
                        elm_syntax_comments_then_linebreak_indented_into(
                            so_far,
                            indent + 2,
                            comments_after_last_element,
                        );
                    }
                    so_far.push(']');
                }
            }
        }
        ElmSyntaxExpression::Negation(maybe_in_negation) => {
            so_far.push('-');
            if let Some(in_negation_node) = maybe_in_negation {
                let in_negation_innermost = elm_syntax_expression_to_unparenthesized(
                    elm_syntax_node_unbox(in_negation_node),
                );
                match in_negation_innermost.value {
                    ElmSyntaxExpression::Negation(_) => {
                        // -(-...)
                        elm_syntax_expression_parenthesized_into(
                            so_far,
                            indent + 1,
                            comments,
                            in_negation_node.range,
                            in_negation_innermost,
                        );
                    }
                    _ => {
                        elm_syntax_expression_parenthesized_if_space_separated_into(
                            so_far,
                            indent + 1,
                            comments,
                            elm_syntax_node_unbox(in_negation_node),
                        );
                    }
                }
            }
        }
        ElmSyntaxExpression::OperatorFunction(operator_node) => {
            so_far.push('(');
            so_far.push_str(operator_node.value);
            so_far.push(')');
        }
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            let innermost: ElmSyntaxNode<&ElmSyntaxExpression> =
                elm_syntax_expression_to_unparenthesized(elm_syntax_node_unbox(in_parens));
            let comments_before_innermost = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: expression_node.range.start,
                    end: innermost.range.start,
                },
            );
            let comments_after_innermost = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: innermost.range.end,
                    end: expression_node.range.end,
                },
            );
            if comments_before_innermost.is_empty() && comments_after_innermost.is_empty() {
                elm_syntax_expression_not_parenthesized_into(so_far, indent, comments, innermost);
            } else {
                elm_syntax_expression_parenthesized_into(
                    so_far,
                    indent,
                    comments,
                    in_parens.range,
                    innermost,
                );
            }
        }
        ElmSyntaxExpression::Record(fields) => match fields.split_first() {
            None => {
                let comments_in_curlies: &[ElmSyntaxNode<ElmSyntaxComment>] =
                    elm_syntax_comments_in_range(comments, expression_node.range);
                if comments_in_curlies.is_empty() {
                    so_far.push_str("{}");
                } else {
                    so_far.push('{');
                    elm_syntax_comments_into(so_far, indent + 1, comments);
                    linebreak_indented_into(so_far, indent);
                    so_far.push('}');
                }
            }
            Some((field0, field1_up)) => {
                let line_span: LineSpan =
                    elm_syntax_range_line_span(expression_node.range, comments);
                so_far.push_str("{ ");
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: expression_node.range.start,
                            end: field0.name.range.start,
                        },
                    ),
                );
                let previous_syntax_end: lsp_types::Position =
                    elm_syntax_expression_fields_into_string(
                        so_far, indent, comments, line_span, field0, field1_up,
                    );
                space_or_linebreak_indented_into(so_far, line_span, indent);
                let comments_before_closing_curly = elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_syntax_end,
                        end: expression_node.range.end,
                    },
                );
                if !comments_before_closing_curly.is_empty() {
                    linebreak_indented_into(so_far, indent);
                    elm_syntax_comments_then_linebreak_indented_into(
                        so_far,
                        indent,
                        comments_before_closing_curly,
                    );
                }
                so_far.push('}');
            }
        },
        ElmSyntaxExpression::RecordAccess {
            record,
            field: maybe_field,
        } => {
            elm_syntax_expression_parenthesized_if_space_separated_into(
                so_far,
                indent,
                comments,
                elm_syntax_node_unbox(record),
            );
            so_far.push('.');
            if let Some(field_name_node) = maybe_field {
                so_far.push_str(&field_name_node.value);
            }
        }
        ElmSyntaxExpression::RecordAccessFunction(maybe_field_name) => {
            so_far.push('.');
            if let Some(field_name_node) = maybe_field_name {
                so_far.push_str(&field_name_node.value);
            }
        }
        ElmSyntaxExpression::RecordUpdate {
            record_variable: maybe_record_variable,
            bar_key_symbol_range: _,
            fields,
        } => {
            let line_span: LineSpan = elm_syntax_range_line_span(expression_node.range, comments);
            so_far.push_str("{ ");
            let mut previous_syntax_end: lsp_types::Position = expression_node.range.start;
            if let Some(record_variable_node) = maybe_record_variable {
                so_far.push_str(&record_variable_node.value);
                previous_syntax_end = record_variable_node.range.end;
            }
            if let Some((field0, field1_up)) = fields.split_first() {
                space_or_linebreak_indented_into(so_far, line_span, indent);
                so_far.push_str("| ");
                previous_syntax_end = elm_syntax_expression_fields_into_string(
                    so_far, indent, comments, line_span, field0, field1_up,
                );
            }
            space_or_linebreak_indented_into(so_far, line_span, indent);
            let comments_before_closing_curly = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: expression_node.range.end,
                },
            );
            if !comments_before_closing_curly.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    comments_before_closing_curly,
                );
            }
            so_far.push('}');
        }
        ElmSyntaxExpression::Reference {
            qualification,
            name,
        } => {
            if qualification.is_empty() {
                so_far.push_str(name);
            } else {
                so_far.push_str(qualification);
                so_far.push('.');
                so_far.push_str(name);
            }
        }
        ElmSyntaxExpression::String {
            content,
            quoting_style,
        } => {
            elm_string_into(so_far, *quoting_style, content);
        }
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            so_far.push_str("( ");
            let line_span: LineSpan = elm_syntax_expression_line_span(comments, expression_node);
            let mut previous_part_end: lsp_types::Position = expression_node.range.start;
            if let Some(part_node) = maybe_part0 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_part_end,
                            end: part_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    comments,
                    elm_syntax_node_unbox(part_node),
                );
                previous_part_end = part_node.range.end;
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part_node) = maybe_part1 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_part_end,
                            end: part_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    comments,
                    elm_syntax_node_unbox(part_node),
                );
                previous_part_end = part_node.range.end;
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part_node) = maybe_part2 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_part_end,
                            end: part_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    comments,
                    elm_syntax_node_unbox(part_node),
                );
                previous_part_end = part_node.range.end;
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            let comments_after_parts: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_part_end,
                        end: expression_node.range.end,
                    },
                );
            if !comments_after_parts.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    comments_after_parts,
                );
            }
            so_far.push(')');
        }
        ElmSyntaxExpression::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            so_far.push_str("( ");
            let line_span: LineSpan = elm_syntax_expression_line_span(comments, expression_node);
            let mut previous_part_end: lsp_types::Position = expression_node.range.start;
            if let Some(part_node) = maybe_part0 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_part_end,
                            end: part_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    comments,
                    elm_syntax_node_unbox(part_node),
                );
                previous_part_end = part_node.range.end;
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            so_far.push_str(", ");
            if let Some(part_node) = maybe_part1 {
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent,
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: previous_part_end,
                            end: part_node.range.start,
                        },
                    ),
                );
                elm_syntax_expression_not_parenthesized_into(
                    so_far,
                    indent + 2,
                    comments,
                    elm_syntax_node_unbox(part_node),
                );
                previous_part_end = part_node.range.end;
            }
            if line_span == LineSpan::Multiple {
                linebreak_indented_into(so_far, indent);
            }
            let comments_after_parts: &[ElmSyntaxNode<ElmSyntaxComment>] =
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_part_end,
                        end: expression_node.range.end,
                    },
                );
            if !comments_after_parts.is_empty() {
                linebreak_indented_into(so_far, indent);
                elm_syntax_comments_then_linebreak_indented_into(
                    so_far,
                    indent + 2,
                    comments_after_parts,
                );
            }
            so_far.push(')');
        }
    }
}
/// returns the last syntax end position
fn elm_syntax_case_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    previous_syntax_end: lsp_types::Position,
    case: &ElmSyntaxExpressionCase,
) -> lsp_types::Position {
    let before_case_arrow_key_symbol: lsp_types::Position = case
        .arrow_key_symbol_range
        .map(|range| range.end)
        .unwrap_or(case.pattern.range.end);
    elm_syntax_comments_then_linebreak_indented_into(
        so_far,
        indent,
        elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: previous_syntax_end,
                end: before_case_arrow_key_symbol,
            },
        ),
    );
    elm_syntax_pattern_not_parenthesized_into(so_far, elm_syntax_node_as_ref(&case.pattern));
    so_far.push_str(" ->");
    linebreak_indented_into(so_far, next_indent(indent));
    if let Some(result_node) = &case.result {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent),
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: before_case_arrow_key_symbol,
                    end: result_node.range.end,
                },
            ),
        );
        elm_syntax_expression_not_parenthesized_into(
            so_far,
            next_indent(indent),
            comments,
            elm_syntax_node_as_ref(result_node),
        );
        result_node.range.end
    } else {
        before_case_arrow_key_symbol
    }
}
/// returns the last syntax end position
fn elm_syntax_expression_fields_into_string<'a>(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    line_span: LineSpan,
    field0: &'a ElmSyntaxExpressionField,
    field1_up: &'a [ElmSyntaxExpressionField],
) -> lsp_types::Position {
    so_far.push_str(&field0.name.value);
    let mut previous_syntax_end: lsp_types::Position = field0.name.range.end;
    so_far.push_str(" =");
    if let Some(field0_value_node) = &field0.value {
        let comments_before_field0_value = elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: field0.name.range.end,
                end: field0_value_node.range.start,
            },
        );
        space_or_linebreak_indented_into(
            so_far,
            if comments_before_field0_value.is_empty() {
                elm_syntax_expression_line_span(comments, elm_syntax_node_as_ref(field0_value_node))
            } else {
                LineSpan::Multiple
            },
            next_indent(indent + 2),
        );
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent + 2),
            comments_before_field0_value,
        );
        elm_syntax_expression_not_parenthesized_into(
            so_far,
            next_indent(indent + 2),
            comments,
            elm_syntax_node_as_ref(field0_value_node),
        );
        previous_syntax_end = field0_value_node.range.end;
    }
    for field in field1_up {
        if line_span == LineSpan::Multiple {
            linebreak_indented_into(so_far, indent);
        }
        so_far.push_str(", ");
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            indent + 2,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: field.name.range.start,
                },
            ),
        );
        so_far.push_str(&field.name.value);
        previous_syntax_end = field.name.range.end;
        so_far.push_str(" =");
        if let Some(field_value_node) = &field.value {
            let comments_before_field_value = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: field.name.range.end,
                    end: field_value_node.range.start,
                },
            );
            space_or_linebreak_indented_into(
                so_far,
                if comments_before_field_value.is_empty() {
                    elm_syntax_range_line_span(
                        lsp_types::Range {
                            start: field.name.range.end,
                            end: field_value_node.range.end,
                        },
                        comments,
                    )
                } else {
                    LineSpan::Multiple
                },
                next_indent(indent + 2),
            );
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                next_indent(indent + 2),
                comments_before_field_value,
            );
            elm_syntax_expression_not_parenthesized_into(
                so_far,
                next_indent(indent + 2),
                comments,
                elm_syntax_node_as_ref(field_value_node),
            );
            previous_syntax_end = field_value_node.range.end;
        }
    }
    previous_syntax_end
}
fn elm_syntax_let_declaration_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    let_declaration_node: ElmSyntaxNode<&ElmSyntaxLetDeclaration>,
) {
    match let_declaration_node.value {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern: pattern_node,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            expression: maybe_expression,
        } => {
            elm_syntax_comments_into(
                so_far,
                indent,
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: let_declaration_node.range.start,
                        end: maybe_equals_key_symbol_range
                            .map(|range| range.start)
                            .unwrap_or(pattern_node.range.end),
                    },
                ),
            );
            elm_syntax_pattern_parenthesized_if_space_separated_into(
                so_far,
                elm_syntax_node_as_ref(pattern_node),
            );
            so_far.push_str(" =");
            linebreak_indented_into(so_far, next_indent(indent));
            if let Some(expression_node) = maybe_expression {
                elm_syntax_comments_into(
                    so_far,
                    next_indent(indent),
                    elm_syntax_comments_in_range(
                        comments,
                        lsp_types::Range {
                            start: maybe_equals_key_symbol_range
                                .map(|range| range.end)
                                .unwrap_or(pattern_node.range.end),
                            end: expression_node.range.end,
                        },
                    ),
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
            elm_syntax_variable_declaration_into(
                so_far,
                indent,
                comments,
                elm_syntax_node_as_ref_map(start_name_node, Box::as_ref),
                maybe_signature.as_ref(),
                parameters,
                *maybe_equals_key_symbol_range,
                maybe_result.as_ref().map(elm_syntax_node_as_ref),
            );
        }
    }
}
fn elm_syntax_variable_declaration_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    start_name_node: ElmSyntaxNode<&str>,
    maybe_signature: Option<&ElmSyntaxVariableDeclarationSignature>,
    parameters: &[ElmSyntaxNode<ElmSyntaxPattern>],
    maybe_equals_key_symbol_range: Option<lsp_types::Range>,
    maybe_result: Option<ElmSyntaxNode<&ElmSyntaxExpression>>,
) {
    so_far.push_str(start_name_node.value);
    let mut syntax_before_parameters_end: lsp_types::Position = start_name_node.range.end;
    if let Some(signature) = maybe_signature {
        so_far.push_str(" :");
        if let Some(type_node) = &signature.type_ {
            let comments_before_type = elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: start_name_node.range.end,
                    end: type_node.range.start,
                },
            );
            space_or_linebreak_indented_into(
                so_far,
                if start_name_node.range.end.line == type_node.range.end.line
                    && comments_before_type.is_empty()
                {
                    LineSpan::Single
                } else {
                    LineSpan::Multiple
                },
                next_indent(indent),
            );
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                next_indent(indent),
                comments_before_type,
            );
            elm_syntax_type_not_parenthesized_into(
                so_far,
                next_indent(indent),
                |qualified| qualified.qualification,
                comments,
                elm_syntax_node_as_ref(type_node),
            );
            syntax_before_parameters_end = type_node.range.end;
        }
        linebreak_indented_into(so_far, indent);
        if let Some(implementation_name_range) = signature.implementation_name_range {
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                indent,
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: signature
                            .type_
                            .as_ref()
                            .map(|node| node.range.end)
                            .unwrap_or(signature.colon_key_symbol_range.end),
                        end: implementation_name_range.start,
                    },
                ),
            );
            syntax_before_parameters_end = implementation_name_range.end;
        }
        so_far.push_str(start_name_node.value);
    }
    let parameter_comments = elm_syntax_comments_in_range(
        comments,
        lsp_types::Range {
            start: syntax_before_parameters_end,
            end: if maybe_result.is_none()
                && let Some(equals_key_symbol_range) = maybe_equals_key_symbol_range
            {
                equals_key_symbol_range.end
            } else {
                parameters
                    .last()
                    .map(|node| node.range.end)
                    .unwrap_or(syntax_before_parameters_end)
            },
        },
    );
    let parameters_line_span: LineSpan = if parameter_comments.is_empty() {
        LineSpan::Single
    } else {
        LineSpan::Multiple
    };
    let mut previous_parameter_end: lsp_types::Position = start_name_node.range.start;
    for parameter_node in parameters {
        space_or_linebreak_indented_into(so_far, parameters_line_span, next_indent(indent));
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent),
            elm_syntax_comments_in_range(
                parameter_comments,
                lsp_types::Range {
                    start: previous_parameter_end,
                    end: parameter_node.range.start,
                },
            ),
        );
        elm_syntax_pattern_not_parenthesized_into(so_far, elm_syntax_node_as_ref(parameter_node));
        previous_parameter_end = parameter_node.range.end;
    }
    space_or_linebreak_indented_into(so_far, parameters_line_span, next_indent(indent));
    if maybe_result.is_none()
        && let Some(equals_key_symbol_range) = maybe_equals_key_symbol_range
        && let comments_before_equals_key_symbol = elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: previous_parameter_end,
                end: equals_key_symbol_range.start,
            },
        )
        && !comments_before_equals_key_symbol.is_empty()
    {
        linebreak_indented_into(so_far, indent);
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent),
            comments_before_equals_key_symbol,
        );
    }
    so_far.push('=');
    linebreak_indented_into(so_far, next_indent(indent));
    if let Some(result_node) = maybe_result {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            next_indent(indent),
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_parameter_end,
                    end: result_node.range.start,
                },
            ),
        );
        elm_syntax_expression_not_parenthesized_into(
            so_far,
            next_indent(indent),
            comments,
            result_node,
        );
    }
}
fn elm_syntax_expression_to_unparenthesized(
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) -> ElmSyntaxNode<&ElmSyntaxExpression> {
    match expression_node.value {
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_expression_to_unparenthesized(elm_syntax_node_unbox(in_parens))
        }
        _ => expression_node,
    }
}
fn elm_syntax_range_line_span(
    range: lsp_types::Range,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
) -> LineSpan {
    if elm_syntax_comments_in_range(comments, range).is_empty()
        && range.start.line == range.end.line
    {
        LineSpan::Single
    } else {
        LineSpan::Multiple
    }
}
/// A more accurate (but probably slower) alternative:
/// ```rust
/// let so_far_length_before = so_far.len();
/// ...into(so_far, ...);
/// if so_far[so_far_length_before..].contains('\n') {
///     so_far.insert_str(so_far_length_before, ..linebreak indented..);
/// } else {
///     so_far.insert(so_far_length_before, ' ');
/// }
/// ```
/// with a potential optimization being
fn elm_syntax_expression_line_span(
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) -> LineSpan {
    if elm_syntax_comments_in_range(comments, expression_node.range).is_empty()
        && expression_node.range.start.line == expression_node.range.end.line
        && !elm_syntax_expression_any_sub(expression_node, |sub_node| match sub_node.value {
            ElmSyntaxExpression::CaseOf { .. } => true,
            ElmSyntaxExpression::IfThenElse { .. } => true,
            ElmSyntaxExpression::LetIn { .. } => true,
            ElmSyntaxExpression::String {
                content,
                quoting_style: _,
            } => content.contains("\n"),
            ElmSyntaxExpression::Unit
            | ElmSyntaxExpression::Integer { .. }
            | ElmSyntaxExpression::Float(_)
            | ElmSyntaxExpression::Char(_)
            | ElmSyntaxExpression::Negation(_)
            | ElmSyntaxExpression::Parenthesized(_)
            | ElmSyntaxExpression::List(_)
            | ElmSyntaxExpression::Lambda { .. }
            | ElmSyntaxExpression::InfixOperationIgnoringPrecedence { .. }
            | ElmSyntaxExpression::Record(_)
            | ElmSyntaxExpression::RecordUpdate { .. }
            | ElmSyntaxExpression::RecordAccess { .. }
            | ElmSyntaxExpression::RecordAccessFunction(_)
            | ElmSyntaxExpression::Reference { .. }
            | ElmSyntaxExpression::OperatorFunction(_)
            | ElmSyntaxExpression::Tuple { .. }
            | ElmSyntaxExpression::Triple { .. }
            | ElmSyntaxExpression::Call { .. } => false,
        })
    {
        LineSpan::Single
    } else {
        LineSpan::Multiple
    }
}
fn elm_syntax_expression_parenthesized_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    full_range: lsp_types::Range,
    innermost: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    let comments_before_innermost = elm_syntax_comments_in_range(
        comments,
        lsp_types::Range {
            start: full_range.start,
            end: innermost.range.start,
        },
    );
    let comments_after_innermost = elm_syntax_comments_in_range(
        comments,
        lsp_types::Range {
            start: innermost.range.end,
            end: full_range.end,
        },
    );
    so_far.push('(');
    elm_syntax_comments_then_linebreak_indented_into(so_far, indent + 1, comments_before_innermost);
    elm_syntax_comments_then_linebreak_indented_into(so_far, indent + 1, comments_after_innermost);
    let so_far_length_before_innermost: usize = so_far.len();
    elm_syntax_expression_not_parenthesized_into(so_far, indent + 1, comments, innermost);
    if so_far[so_far_length_before_innermost..].contains('\n') {
        linebreak_indented_into(so_far, indent);
    }
    so_far.push(')');
}
fn elm_syntax_expression_parenthesized_if_space_separated_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    let unparenthesized: ElmSyntaxNode<&ElmSyntaxExpression> =
        elm_syntax_expression_to_unparenthesized(expression_node);
    let is_space_separated: bool = match unparenthesized.value {
        ElmSyntaxExpression::IfThenElse { .. } => true,
        ElmSyntaxExpression::Lambda { .. } => true,
        ElmSyntaxExpression::LetIn { .. } => true,
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence { .. } => true,
        ElmSyntaxExpression::Call { .. } => true,
        ElmSyntaxExpression::CaseOf { .. } => true,
        ElmSyntaxExpression::Unit => false,
        ElmSyntaxExpression::Char(_) => false,
        ElmSyntaxExpression::Float(_) => false,
        ElmSyntaxExpression::Integer { .. } => false,
        ElmSyntaxExpression::List(_) => false,
        ElmSyntaxExpression::Negation(_) => false,
        ElmSyntaxExpression::OperatorFunction(_) => false,
        ElmSyntaxExpression::Parenthesized(_) => false,
        ElmSyntaxExpression::Record(_) => false,
        ElmSyntaxExpression::RecordAccess { .. } => false,
        ElmSyntaxExpression::RecordAccessFunction(_) => false,
        ElmSyntaxExpression::RecordUpdate { .. } => false,
        ElmSyntaxExpression::Reference { .. } => false,
        ElmSyntaxExpression::String { .. } => false,
        ElmSyntaxExpression::Triple { .. } => false,
        ElmSyntaxExpression::Tuple { .. } => false,
    };
    if is_space_separated {
        elm_syntax_expression_parenthesized_into(
            so_far,
            indent,
            comments,
            expression_node.range,
            unparenthesized,
        );
    } else {
        elm_syntax_expression_not_parenthesized_into(so_far, indent, comments, expression_node);
    }
}
fn elm_syntax_expression_parenthesized_if_not_call_but_space_separated_into(
    so_far: &mut String,
    indent: usize,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    let unparenthesized: ElmSyntaxNode<&ElmSyntaxExpression> =
        elm_syntax_expression_to_unparenthesized(expression_node);
    let is_space_separated: bool = match unparenthesized.value {
        ElmSyntaxExpression::IfThenElse { .. } => true,
        ElmSyntaxExpression::Lambda { .. } => true,
        ElmSyntaxExpression::LetIn { .. } => true,
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence { .. } => true,
        ElmSyntaxExpression::CaseOf { .. } => true,
        ElmSyntaxExpression::Call { .. } => false,
        ElmSyntaxExpression::Unit => false,
        ElmSyntaxExpression::Char(_) => false,
        ElmSyntaxExpression::Float(_) => false,
        ElmSyntaxExpression::Integer { .. } => false,
        ElmSyntaxExpression::List(_) => false,
        ElmSyntaxExpression::Negation(_) => false,
        ElmSyntaxExpression::OperatorFunction(_) => false,
        ElmSyntaxExpression::Parenthesized(_) => false,
        ElmSyntaxExpression::Record(_) => false,
        ElmSyntaxExpression::RecordAccess { .. } => false,
        ElmSyntaxExpression::RecordAccessFunction(_) => false,
        ElmSyntaxExpression::RecordUpdate { .. } => false,
        ElmSyntaxExpression::Reference { .. } => false,
        ElmSyntaxExpression::String { .. } => false,
        ElmSyntaxExpression::Triple { .. } => false,
        ElmSyntaxExpression::Tuple { .. } => false,
    };
    if is_space_separated {
        elm_syntax_expression_parenthesized_into(
            so_far,
            indent,
            comments,
            expression_node.range,
            unparenthesized,
        );
    } else {
        elm_syntax_expression_not_parenthesized_into(so_far, indent, comments, expression_node);
    }
}
fn elm_syntax_expression_any_sub(
    expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
    is_needle: impl Fn(ElmSyntaxNode<&ElmSyntaxExpression>) -> bool + Copy,
) -> bool {
    if is_needle(expression_node) {
        return true;
    }
    match expression_node.value {
        ElmSyntaxExpression::Unit => false,
        ElmSyntaxExpression::Call {
            called,
            argument0,
            argument1_up,
        } => {
            elm_syntax_expression_any_sub(elm_syntax_node_unbox(called), is_needle)
                || elm_syntax_expression_any_sub(elm_syntax_node_unbox(argument0), is_needle)
                || argument1_up.iter().any(|argument_node| {
                    elm_syntax_expression_any_sub(elm_syntax_node_as_ref(argument_node), is_needle)
                })
        }
        ElmSyntaxExpression::CaseOf {
            matched: maybe_matched,
            of_keyword_range: _,
            cases,
        } => {
            maybe_matched.as_ref().is_some_and(|matched_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(matched_node), is_needle)
            }) || cases
                .iter()
                .filter_map(|case| case.result.as_ref())
                .any(|case_result_node| {
                    elm_syntax_expression_any_sub(
                        elm_syntax_node_as_ref(case_result_node),
                        is_needle,
                    )
                })
        }
        ElmSyntaxExpression::Char(_) => false,
        ElmSyntaxExpression::Float(_) => false,
        ElmSyntaxExpression::IfThenElse {
            condition: maybe_condition,
            then_keyword_range: _,
            on_true: maybe_on_true,
            else_keyword_range: _,
            on_false: maybe_on_false,
        } => {
            maybe_condition.as_ref().is_some_and(|condition_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(condition_node), is_needle)
            }) || maybe_on_true.as_ref().is_some_and(|on_true_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(on_true_node), is_needle)
            }) || maybe_on_false.as_ref().is_some_and(|on_false_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(on_false_node), is_needle)
            })
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left,
            operator: _,
            right: maybe_right,
        } => {
            elm_syntax_expression_any_sub(elm_syntax_node_unbox(left), is_needle)
                || maybe_right.as_ref().is_some_and(|right_node| {
                    elm_syntax_expression_any_sub(elm_syntax_node_unbox(right_node), is_needle)
                })
        }
        ElmSyntaxExpression::Integer { .. } => false,
        ElmSyntaxExpression::Lambda {
            parameters: _,
            arrow_key_symbol_range: _,
            result: maybe_result,
        } => maybe_result.as_ref().is_some_and(|result_node| {
            elm_syntax_expression_any_sub(elm_syntax_node_unbox(result_node), is_needle)
        }),
        ElmSyntaxExpression::LetIn {
            declarations,
            in_keyword_range: _,
            result: maybe_result,
        } => {
            maybe_result.as_ref().is_some_and(|result_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(result_node), is_needle)
            }) || declarations
                .iter()
                .filter_map(|declaration_node| match &declaration_node.value {
                    ElmSyntaxLetDeclaration::Destructuring {
                        pattern: _,
                        equals_key_symbol_range: _,
                        expression,
                    } => expression.as_ref(),
                    ElmSyntaxLetDeclaration::VariableDeclaration {
                        start_name: _,
                        signature: _,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result,
                    } => result.as_ref(),
                })
                .any(|declaration_expression_node| {
                    elm_syntax_expression_any_sub(
                        elm_syntax_node_as_ref(declaration_expression_node),
                        is_needle,
                    )
                })
        }
        ElmSyntaxExpression::List(elements) => elements.iter().any(|element_node| {
            elm_syntax_expression_any_sub(elm_syntax_node_as_ref(element_node), is_needle)
        }),
        ElmSyntaxExpression::Negation(maybe_in_negation) => {
            maybe_in_negation.as_ref().is_some_and(|in_negation| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(in_negation), is_needle)
            })
        }
        ElmSyntaxExpression::OperatorFunction(_) => false,
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_expression_any_sub(elm_syntax_node_unbox(in_parens), is_needle)
        }
        ElmSyntaxExpression::Record(fields) => fields
            .iter()
            .filter_map(|field| field.value.as_ref())
            .any(|field_value_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_as_ref(field_value_node), is_needle)
            }),
        ElmSyntaxExpression::RecordAccess { record, field: _ } => {
            elm_syntax_expression_any_sub(elm_syntax_node_unbox(record), is_needle)
        }
        ElmSyntaxExpression::RecordAccessFunction(_) => false,
        ElmSyntaxExpression::RecordUpdate {
            record_variable: _,
            bar_key_symbol_range: _,
            fields,
        } => fields
            .iter()
            .filter_map(|field| field.value.as_ref())
            .any(|field_value_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_as_ref(field_value_node), is_needle)
            }),
        ElmSyntaxExpression::Reference { .. } => false,
        ElmSyntaxExpression::String { .. } => false,
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            maybe_part0.as_ref().is_some_and(|part0_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(part0_node), is_needle)
            }) || maybe_part1.as_ref().is_some_and(|part1_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(part1_node), is_needle)
            }) || maybe_part2.as_ref().is_some_and(|part2_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(part2_node), is_needle)
            })
        }
        ElmSyntaxExpression::Tuple {
            part0: maybe_part0,
            part1: maybe_part1,
        } => {
            maybe_part0.as_ref().is_some_and(|part0_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(part0_node), is_needle)
            }) || maybe_part1.as_ref().is_some_and(|part1_node| {
                elm_syntax_expression_any_sub(elm_syntax_node_unbox(part1_node), is_needle)
            })
        }
    }
}
fn elm_syntax_module_format(module_state: &ModuleState) -> String {
    let elm_syntax_module: &ElmSyntaxModule = &module_state.syntax;
    let mut builder: String = String::with_capacity(module_state.source.len());
    let mut previous_syntax_end: lsp_types::Position;
    match &elm_syntax_module.header {
        None => {
            builder.push_str("module  exposing ()");
            previous_syntax_end = lsp_types::Position {
                line: 0,
                character: 0,
            }
        }
        Some(module_header) => {
            previous_syntax_end = elm_syntax_module_header_into(
                &mut builder,
                &elm_syntax_module.comments,
                module_header,
            );
        }
    }
    builder.push_str("\n\n");
    if let Some(module_documentation_node) = &elm_syntax_module.documentation {
        elm_syntax_module_level_comments(
            &mut builder,
            elm_syntax_comments_in_range(
                &elm_syntax_module.comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: module_documentation_node.range.start,
                },
            ),
        );
        elm_syntax_module_documentation_comment_into(
            &mut builder,
            &module_documentation_node.value,
        );
        builder.push_str("\n\n");
        previous_syntax_end = module_documentation_node.range.end;
    }
    if let Some(last_import_node) = elm_syntax_module.imports.last() {
        elm_syntax_module_level_comments(
            &mut builder,
            elm_syntax_comments_in_range(
                &elm_syntax_module.comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: last_import_node.range.end,
                },
            ),
        );
        elm_syntax_imports_then_linebreak_into(&mut builder, &elm_syntax_module.imports);
        previous_syntax_end = last_import_node.range.end;
    } else {
        builder.push('\n');
    }
    for documented_declaration_or_err in &elm_syntax_module.declarations {
        match documented_declaration_or_err {
            Err(whatever) => {
                builder.push_str(whatever);
            }
            Ok(documented_declaration) => {
                builder.push_str("\n\n");
                if let Some(module_documentation_node) = &documented_declaration.documentation {
                    elm_syntax_module_level_comments(
                        &mut builder,
                        elm_syntax_comments_in_range(
                            &elm_syntax_module.comments,
                            lsp_types::Range {
                                start: previous_syntax_end,
                                end: module_documentation_node.range.start,
                            },
                        ),
                    );
                    elm_syntax_documentation_comment_then_linebreak_into(
                        &mut builder,
                        &module_documentation_node.value,
                    );
                    previous_syntax_end = module_documentation_node.range.end;
                }
                if let Some(declaration_node) = &documented_declaration.declaration {
                    elm_syntax_module_level_comments(
                        &mut builder,
                        elm_syntax_comments_in_range(
                            &elm_syntax_module.comments,
                            lsp_types::Range {
                                start: previous_syntax_end,
                                end: declaration_node.range.start,
                            },
                        ),
                    );
                    elm_syntax_declaration_into(
                        &mut builder,
                        &elm_syntax_module.comments,
                        elm_syntax_node_as_ref(declaration_node),
                    );
                    previous_syntax_end = declaration_node.range.end;
                    builder.push('\n');
                }
            }
        }
    }
    let comments_after_declarations: &[ElmSyntaxNode<ElmSyntaxComment>] =
        elm_syntax_comments_from_position(&elm_syntax_module.comments, previous_syntax_end);
    if !comments_after_declarations.is_empty() {
        builder.push_str("\n\n\n");
        elm_syntax_comments_then_linebreak_indented_into(
            &mut builder,
            0,
            comments_after_declarations,
        );
    }
    builder
}
fn elm_syntax_module_level_comments(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
) {
    if !comments.is_empty() {
        so_far.push('\n');
        elm_syntax_comments_then_linebreak_indented_into(so_far, 0, comments);
        so_far.push_str("\n\n");
    }
}
fn elm_syntax_module_documentation_comment_into(
    so_far: &mut String,
    module_documentation_elements: &[ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
) {
    so_far.push_str("{-|");
    for module_documentation_element in module_documentation_elements {
        match &module_documentation_element.value {
            ElmSyntaxModuleDocumentationElement::Markdown(markdown_node) => {
                so_far.push_str(markdown_node);
            }
            ElmSyntaxModuleDocumentationElement::AtDocs(expose_group_names) => {
                so_far.push_str("@docs ");
                if let Some((expose_name0_node, expose_name1_up)) = expose_group_names.split_first()
                {
                    so_far.push_str(
                        expose_name0_node
                            .value
                            .strip_suffix("(..)")
                            .unwrap_or(&expose_name0_node.value),
                    );
                    for expose_name_node in expose_name1_up {
                        so_far.push_str(", ");
                        so_far.push_str(
                            expose_name_node
                                .value
                                .strip_suffix("(..)")
                                .unwrap_or(&expose_name_node.value),
                        );
                    }
                }
            }
        }
    }
    so_far.push_str("-}");
}
fn elm_syntax_documentation_comment_then_linebreak_into(so_far: &mut String, content: &str) {
    so_far.push_str("{-|");
    so_far.push_str(content);
    so_far.push_str("-}\n");
}
/// returns the last syntax end position
fn elm_syntax_module_header_into(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    elm_syntax_module_header: &ElmSyntaxModuleHeader,
) -> lsp_types::Position {
    let before_exposing: lsp_types::Position = elm_syntax_module_header
        .exposing_keyword_range
        .map(|range| range.start)
        .unwrap_or_else(|| match &elm_syntax_module_header.specific {
            ElmSyntaxModuleHeaderSpecific::Pure {
                module_keyword_range,
            } => match &elm_syntax_module_header.module_name {
                Some(module_name_node) => module_name_node.range.end,
                None => module_keyword_range.end,
            },
            ElmSyntaxModuleHeaderSpecific::Port {
                port_keyword_range: _,
                module_keyword_range,
            } => match &elm_syntax_module_header.module_name {
                Some(module_name_node) => module_name_node.range.end,
                None => module_keyword_range.end,
            },
            ElmSyntaxModuleHeaderSpecific::Effect {
                effect_keyword_range: _,
                module_keyword_range: _,
                where_keyword_range,
                command: maybe_command,
                subscription: maybe_subscription,
            } => match (maybe_command, maybe_subscription) {
                (_, Some(subscription_entry)) => subscription_entry.value_type_name.range.end,
                (Some(command_entry), None) => command_entry.value_type_name.range.end,
                (None, None) => where_keyword_range.end,
            },
        });
    elm_syntax_comments_then_linebreak_indented_into(
        so_far,
        0,
        elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: before_exposing,
            },
        ),
    );
    match &elm_syntax_module_header.specific {
        ElmSyntaxModuleHeaderSpecific::Pure {
            module_keyword_range: _,
        } => {
            so_far.push_str("module ");
            if let Some(module_name_node) = &elm_syntax_module_header.module_name {
                so_far.push_str(&module_name_node.value);
            }
        }
        ElmSyntaxModuleHeaderSpecific::Port {
            port_keyword_range: _,
            module_keyword_range: _,
        } => {
            so_far.push_str("port module ");
            if let Some(module_name_node) = &elm_syntax_module_header.module_name {
                so_far.push_str(&module_name_node.value);
            }
        }
        ElmSyntaxModuleHeaderSpecific::Effect {
            effect_keyword_range: _,
            module_keyword_range: _,
            where_keyword_range: _,
            command: maybe_command,
            subscription: maybe_subscription,
        } => {
            so_far.push_str("effect module ");
            if let Some(module_name_node) = &elm_syntax_module_header.module_name {
                so_far.push_str(&module_name_node.value);
            }
            so_far.push_str(" where { ");
            match (maybe_command, maybe_subscription) {
                (None, Some(subscription_entry)) => {
                    so_far.push_str("subscription = ");
                    so_far.push_str(&subscription_entry.value_type_name.value);
                }
                (Some(command_entry), None) => {
                    so_far.push_str("command = ");
                    so_far.push_str(&command_entry.value_type_name.value);
                }
                (Some(command_entry), Some(subscription_entry)) => {
                    so_far.push_str("command = ");
                    so_far.push_str(&command_entry.value_type_name.value);
                    so_far.push_str(", subscription = ");
                    so_far.push_str(&subscription_entry.value_type_name.value);
                }
                (None, None) => {}
            }
            so_far.push_str(" }");
        }
    }
    so_far.push_str(" exposing ");
    match &elm_syntax_module_header.exposing {
        Some(module_header_exposing_node) => {
            // respect @docs grouping like elm-format does?
            elm_syntax_exposing_into(so_far, &module_header_exposing_node.value);
            module_header_exposing_node.range.end
        }
        None => {
            so_far.push_str("()");
            elm_syntax_module_header
                .exposing_keyword_range
                .map(|range| range.end)
                .unwrap_or(before_exposing)
        }
    }
}
fn elm_syntax_exposing_into(so_far: &mut String, elm_syntax_exposing: &ElmSyntaxExposing) {
    match elm_syntax_exposing {
        ElmSyntaxExposing::All(_) => {
            so_far.push_str("(..)");
        }
        ElmSyntaxExposing::Explicit(exposes) => {
            so_far.push('(');
            let mut expose_strings: std::collections::BTreeSet<std::borrow::Cow<str>> =
                std::collections::BTreeSet::new();
            elm_syntax_exposes_into_expose_strings(&mut expose_strings, exposes);
            let mut expose_strings_iterator = expose_strings.into_iter();
            if let Some(expose_string0) = expose_strings_iterator.next() {
                so_far.push_str(&expose_string0);
                for expose_string in expose_strings_iterator {
                    so_far.push_str(", ");
                    so_far.push_str(&expose_string);
                }
            }
            so_far.push(')');
        }
    }
}
fn elm_syntax_imports_then_linebreak_into(
    so_far: &mut String,
    imports: &[ElmSyntaxNode<ElmSyntaxImport>],
) {
    if imports.is_empty() {
        return;
    }
    let mut imports_without_module_name: Vec<&ElmSyntaxNode<ElmSyntaxImport>> = Vec::new();
    let mut imports_with_module_name_merged: std::collections::BTreeMap<
        &str,
        ElmImportOfModuleNameSummary,
    > = std::collections::BTreeMap::new();
    for import_node in imports {
        match &import_node.value.module_name {
            Some(import_module_name_node) => {
                imports_with_module_name_merged
                    .entry(&import_module_name_node.value)
                    .and_modify(|existing_import_with_same_module_name_summary| {
                        elm_syntax_import_merge_into_summary(
                            existing_import_with_same_module_name_summary,
                            &import_node.value,
                        );
                    })
                    .or_insert_with(|| elm_syntax_import_merge_to_summary(&import_node.value));
            }
            None => {
                imports_without_module_name.push(import_node);
            }
        }
    }
    for (import_module_name, import_of_module_name_summary) in imports_with_module_name_merged {
        let mut import_aliases_iterator = import_of_module_name_summary.aliases.into_iter();
        if import_of_module_name_summary.alias_required
            && let Some(alias0) = import_aliases_iterator.next()
        {
            so_far.push_str("import ");
            so_far.push_str(import_module_name);
            so_far.push_str(" as ");
            so_far.push_str(alias0);
        } else {
            so_far.push_str("import ");
            so_far.push_str(import_module_name);
        }
        match import_of_module_name_summary.exposing {
            ElmExposingStrings::All => {
                so_far.push_str(" exposing (..)");
            }
            ElmExposingStrings::Explicit(expose_strings) => {
                if !expose_strings.is_empty() {
                    so_far.push_str(" exposing (");
                    let mut expose_strings_iterator = expose_strings.into_iter();
                    if let Some(expose_string0) = expose_strings_iterator.next() {
                        so_far.push_str(&expose_string0);
                        for expose_string in expose_strings_iterator {
                            so_far.push_str(", ");
                            so_far.push_str(&expose_string);
                        }
                    }
                    so_far.push(')');
                }
            }
        }
        so_far.push('\n');
        for import_alias in import_aliases_iterator {
            so_far.push_str("import ");
            so_far.push_str(import_module_name);
            so_far.push_str(" as ");
            so_far.push_str(import_alias);
            so_far.push('\n');
        }
    }
    for import_without_module_name_node in imports_without_module_name {
        so_far.push_str("import ");
        if let Some(import_alias_name_node) = &import_without_module_name_node.value.alias_name {
            so_far.push_str(" as ");
            so_far.push_str(&import_alias_name_node.value);
        } else if import_without_module_name_node
            .value
            .as_keyword_range
            .is_some()
        {
            so_far.push_str(" as ");
        }
        if import_without_module_name_node
            .value
            .exposing_keyword_range
            .is_some()
            || import_without_module_name_node.value.exposing.is_some()
        {
            so_far.push_str(" exposing ");
            match &import_without_module_name_node.value.exposing {
                None => {
                    so_far.push_str("()");
                }
                Some(import_exposing_node) => {
                    elm_syntax_exposing_into(so_far, &import_exposing_node.value);
                }
            }
        }
        so_far.push('\n');
    }
}
struct ElmImportOfModuleNameSummary<'a> {
    alias_required: bool,
    aliases: std::collections::BTreeSet<&'a str>,
    exposing: ElmExposingStrings<'a>,
}
enum ElmExposingStrings<'a> {
    All,
    Explicit(std::collections::BTreeSet<std::borrow::Cow<'a, str>>),
}
fn elm_syntax_import_merge_to_summary<'a>(
    elm_syntax_import: &'a ElmSyntaxImport,
) -> ElmImportOfModuleNameSummary<'a> {
    ElmImportOfModuleNameSummary {
        alias_required: elm_syntax_import.alias_name.is_some(),
        aliases: match &elm_syntax_import.alias_name {
            None => std::collections::BTreeSet::new(),
            Some(import_alias_name_node) => {
                std::collections::BTreeSet::from([import_alias_name_node.value.as_ref()])
            }
        },
        exposing: match &elm_syntax_import.exposing {
            None => ElmExposingStrings::Explicit(std::collections::BTreeSet::new()),
            Some(import_exposing) => match &import_exposing.value {
                ElmSyntaxExposing::All(_) => ElmExposingStrings::All,
                ElmSyntaxExposing::Explicit(exposes) => {
                    let mut expose_strings: std::collections::BTreeSet<std::borrow::Cow<str>> =
                        std::collections::BTreeSet::new();
                    elm_syntax_exposes_into_expose_strings(&mut expose_strings, exposes);
                    ElmExposingStrings::Explicit(expose_strings)
                }
            },
        },
    }
}
fn elm_syntax_import_merge_into_summary<'a>(
    summary_to_merge_with: &mut ElmImportOfModuleNameSummary<'a>,
    elm_syntax_import: &'a ElmSyntaxImport,
) {
    match &elm_syntax_import.alias_name {
        None => {
            summary_to_merge_with.alias_required = false;
        }
        Some(import_alias_name_node) => {
            summary_to_merge_with
                .aliases
                .insert(import_alias_name_node.value.as_ref());
        }
    }
    match (
        &mut summary_to_merge_with.exposing,
        elm_syntax_import.exposing.as_ref().map(|node| &node.value),
    ) {
        (ElmExposingStrings::All, _) => {}
        (_, None) => {}
        (ElmExposingStrings::Explicit(_), Some(ElmSyntaxExposing::All(_))) => {
            summary_to_merge_with.exposing = ElmExposingStrings::All;
        }
        (
            ElmExposingStrings::Explicit(expose_strings_to_merge_with),
            Some(ElmSyntaxExposing::Explicit(import_exposes)),
        ) => {
            elm_syntax_exposes_into_expose_strings(expose_strings_to_merge_with, import_exposes);
        }
    }
}

fn elm_syntax_exposes_into_expose_strings<'a>(
    expose_strings_so_far: &mut std::collections::BTreeSet<std::borrow::Cow<'a, str>>,
    exposes: &'a [ElmSyntaxNode<ElmSyntaxExpose>],
) {
    for expose_node in exposes {
        match &expose_node.value {
            ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                name: name_node,
                open_range: _,
            } => {
                expose_strings_so_far
                    .insert(std::borrow::Cow::Owned(format!("{}(..)", name_node.value)));
            }
            ElmSyntaxExpose::Operator(None) => {}
            ElmSyntaxExpose::Operator(Some(operator_node)) => {
                expose_strings_so_far.insert(std::borrow::Cow::Owned(format!(
                    "({})",
                    operator_node.value
                )));
            }
            ElmSyntaxExpose::Type(name) => {
                expose_strings_so_far.insert(std::borrow::Cow::Borrowed(name));
            }
            ElmSyntaxExpose::Variable(name) => {
                expose_strings_so_far.insert(std::borrow::Cow::Borrowed(name));
            }
        }
    }
}
fn elm_syntax_declaration_into(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    declaration_node: ElmSyntaxNode<&ElmSyntaxDeclaration>,
) {
    match declaration_node.value {
        ElmSyntaxDeclaration::ChoiceType {
            name: maybe_name,
            parameters,
            equals_key_symbol_range: _,
            variant0_name: maybe_variant0_name,
            variant0_values,
            variant1_up,
        } => {
            elm_syntax_choice_type_declaration_into(
                so_far,
                comments,
                |qualified| qualified.qualification,
                declaration_node.range,
                maybe_name
                    .as_ref()
                    .map(|name_node| elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                parameters,
                maybe_variant0_name
                    .as_ref()
                    .map(|name_node| elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                variant0_values,
                variant1_up,
            );
        }
        ElmSyntaxDeclaration::Operator {
            direction: maybe_infix_direction,
            precedence: maybe_infix_precedence,
            operator: maybe_operator,
            equals_key_symbol_range: _,
            function: maybe_implementation_function_name,
        } => {
            so_far.push_str("infix ");
            if let Some(infix_direction_node) = maybe_infix_direction {
                so_far.push_str(elm_syntax_infix_direction_to_str(
                    infix_direction_node.value,
                ));
            }
            so_far.push(' ');
            if let Some(infix_precedence_node) = maybe_infix_precedence {
                use std::fmt::Write as _;
                let _ = write!(so_far, "{}", infix_precedence_node.value);
            }
            so_far.push_str(" (");
            if let Some(operator_node) = maybe_operator {
                so_far.push_str(operator_node.value);
            }
            so_far.push_str(") = ");
            if let Some(implementation_function_name) = maybe_implementation_function_name {
                so_far.push_str(&implementation_function_name.value);
            }
        }
        ElmSyntaxDeclaration::Port {
            name: maybe_name,
            colon_key_symbol_range: _,
            type_: maybe_type,
        } => {
            elm_syntax_port_declaration_into(
                so_far,
                comments,
                |qualified| qualified.qualification,
                declaration_node.range,
                maybe_name
                    .as_ref()
                    .map(|name_node| elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                maybe_type.as_ref().map(elm_syntax_node_as_ref),
            );
        }
        ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range: _,
            name: maybe_name,
            parameters,
            equals_key_symbol_range: _,
            type_: maybe_type,
        } => {
            elm_syntax_type_alias_declaration_into(
                so_far,
                comments,
                |qualified| qualified.qualification,
                declaration_node.range,
                maybe_name
                    .as_ref()
                    .map(|name_node| elm_syntax_node_as_ref_map(name_node, Box::as_ref)),
                parameters,
                maybe_type.as_ref().map(elm_syntax_node_as_ref),
            );
        }
        ElmSyntaxDeclaration::Variable {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        } => {
            elm_syntax_variable_declaration_into(
                so_far,
                0,
                comments,
                elm_syntax_node_as_ref_map(start_name_node, Box::as_ref),
                maybe_signature.as_ref(),
                parameters,
                *maybe_equals_key_symbol_range,
                maybe_result.as_ref().map(elm_syntax_node_as_ref),
            );
        }
    }
}
fn elm_syntax_port_declaration_into<'a>(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    maybe_type: Option<ElmSyntaxNode<&'a ElmSyntaxType>>,
) {
    let mut previous_syntax_end: lsp_types::Position = declaration_range.start;
    so_far.push_str("port ");
    if let Some(name_node) = maybe_name {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            5,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: declaration_range.start,
                    end: name_node.range.start,
                },
            ),
        );
        so_far.push_str(name_node.value);
        previous_syntax_end = name_node.range.end;
    }
    if let Some(type_node) = maybe_type {
        so_far.push_str(" :");
        let comments_before_type = elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: previous_syntax_end,
                end: type_node.range.start,
            },
        );
        let annotation_line_span: LineSpan = if comments_before_type.is_empty() {
            elm_syntax_range_line_span(type_node.range, comments)
        } else {
            LineSpan::Multiple
        };
        space_or_linebreak_indented_into(so_far, annotation_line_span, 4);
        elm_syntax_comments_then_linebreak_indented_into(so_far, 4, comments_before_type);
        elm_syntax_type_not_parenthesized_into(
            so_far,
            4,
            assign_qualification,
            comments,
            type_node,
        );
    }
}
fn elm_syntax_type_alias_declaration_into<'a>(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    parameters: &[ElmSyntaxNode<Box<str>>],
    maybe_type: Option<ElmSyntaxNode<&'a ElmSyntaxType>>,
) {
    let mut previous_syntax_end: lsp_types::Position = declaration_range.start;
    so_far.push_str("type alias ");
    if let Some(name_node) = maybe_name {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            11,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: declaration_range.start,
                    end: name_node.range.start,
                },
            ),
        );
        so_far.push_str(name_node.value);
        previous_syntax_end = name_node.range.end;
    }
    let comments_before_and_between_parameters = match parameters.last() {
        None => &[],
        Some(last_parameter) => elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: previous_syntax_end,
                end: last_parameter.range.end,
            },
        ),
    };
    for parameter_node in parameters {
        if comments_before_and_between_parameters.is_empty() {
            so_far.push(' ');
        } else {
            linebreak_indented_into(so_far, 12);
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                12,
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_syntax_end,
                        end: parameter_node.range.start,
                    },
                ),
            );
        }
        so_far.push_str(&parameter_node.value);
        previous_syntax_end = parameter_node.range.end;
    }
    if let Some(type_node) = maybe_type {
        space_or_linebreak_indented_into(
            so_far,
            if comments_before_and_between_parameters.is_empty() {
                LineSpan::Single
            } else {
                LineSpan::Multiple
            },
            4,
        );
        so_far.push('=');
        linebreak_indented_into(so_far, 4);
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            4,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: type_node.range.start,
                },
            ),
        );
        elm_syntax_type_not_parenthesized_into(
            so_far,
            4,
            assign_qualification,
            comments,
            type_node,
        );
    }
}
fn elm_syntax_choice_type_declaration_into<'a>(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    declaration_range: lsp_types::Range,
    maybe_name: Option<ElmSyntaxNode<&str>>,
    parameters: &[ElmSyntaxNode<Box<str>>],
    maybe_variant0_name: Option<ElmSyntaxNode<&str>>,
    variant0_values: &'a [ElmSyntaxNode<ElmSyntaxType>],
    variant1_up: &'a [ElmSyntaxChoiceTypeDeclarationTailingVariant],
) {
    let mut previous_syntax_end: lsp_types::Position = declaration_range.start;
    so_far.push_str("type ");
    if let Some(name_node) = maybe_name {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            5,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: declaration_range.start,
                    end: name_node.range.start,
                },
            ),
        );
        so_far.push_str(name_node.value);
        previous_syntax_end = name_node.range.end;
    }
    let comments_before_and_between_parameters = match parameters.last() {
        None => &[],
        Some(last_parameter) => elm_syntax_comments_in_range(
            comments,
            lsp_types::Range {
                start: previous_syntax_end,
                end: last_parameter.range.end,
            },
        ),
    };
    for parameter_node in parameters {
        if comments_before_and_between_parameters.is_empty() {
            so_far.push(' ');
        } else {
            linebreak_indented_into(so_far, 8);
            elm_syntax_comments_then_linebreak_indented_into(
                so_far,
                8,
                elm_syntax_comments_in_range(
                    comments,
                    lsp_types::Range {
                        start: previous_syntax_end,
                        end: parameter_node.range.start,
                    },
                ),
            );
        }
        so_far.push_str(&parameter_node.value);
        previous_syntax_end = parameter_node.range.end;
    }
    linebreak_indented_into(so_far, 4);
    so_far.push_str("= ");
    previous_syntax_end = elm_syntax_choice_type_declaration_variant_into(
        so_far,
        comments,
        assign_qualification,
        previous_syntax_end,
        maybe_variant0_name,
        variant0_values,
    );
    for variant in variant1_up {
        linebreak_indented_into(so_far, 4);
        so_far.push_str("| ");
        previous_syntax_end = elm_syntax_choice_type_declaration_variant_into(
            so_far,
            comments,
            assign_qualification,
            previous_syntax_end,
            variant.name.as_ref().map(|variant_name_node| {
                elm_syntax_node_as_ref_map(variant_name_node, Box::as_ref)
            }),
            &variant.values,
        );
    }
}
fn elm_syntax_choice_type_declaration_variant_into<'a>(
    so_far: &mut String,
    comments: &[ElmSyntaxNode<ElmSyntaxComment>],
    assign_qualification: impl Fn(ElmQualified<'a>) -> &'a str + Copy,
    mut previous_syntax_end: lsp_types::Position,
    maybe_variant_name: Option<ElmSyntaxNode<&str>>,
    variant_values: &'a [ElmSyntaxNode<ElmSyntaxType>],
) -> lsp_types::Position {
    if let Some(variant_name_node) = maybe_variant_name {
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            6,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: variant_name_node.range.start,
                },
            ),
        );
        so_far.push_str(variant_name_node.value);
        previous_syntax_end = variant_name_node.range.end;
    }
    let Some(variant_last_value_node) = variant_values.last() else {
        return previous_syntax_end;
    };
    let line_span: LineSpan = elm_syntax_range_line_span(
        lsp_types::Range {
            start: previous_syntax_end,
            end: variant_last_value_node.range.end,
        },
        comments,
    );
    for value_node in variant_values {
        space_or_linebreak_indented_into(so_far, line_span, 8);
        elm_syntax_comments_then_linebreak_indented_into(
            so_far,
            8,
            elm_syntax_comments_in_range(
                comments,
                lsp_types::Range {
                    start: previous_syntax_end,
                    end: value_node.range.start,
                },
            ),
        );
        elm_syntax_type_parenthesized_if_space_separated_into(
            so_far,
            8,
            assign_qualification,
            comments,
            value_node.range,
            elm_syntax_type_to_unparenthesized(elm_syntax_node_as_ref(value_node)),
        );
        previous_syntax_end = value_node.range.end;
    }
    previous_syntax_end
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
    ModuleDocumentationAtDocsMember {
        name: &'a str,
        module_documentation: &'a [ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
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
    LetDeclarationName {
        name: &'a str,
        start_name_range: lsp_types::Range,
        signature_type: Option<ElmSyntaxNode<&'a ElmSyntaxType>>,
        scope_expression: ElmSyntaxNode<&'a ElmSyntaxExpression>,
    },
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
            elm_syntax_module
                .documentation
                .as_ref()
                .and_then(|module_documentation_node| {
                    elm_syntax_module_documentation_find_symbol_at_position(
                        elm_syntax_node_as_ref_map(module_documentation_node, Vec::as_slice),
                        position,
                    )
                })
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
                .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
                .find_map(|documented_declaration| {
                    let declaration_node = documented_declaration.declaration.as_ref()?;
                    elm_syntax_declaration_find_reference_at_position(
                        elm_syntax_node_as_ref(declaration_node),
                        documented_declaration
                            .documentation
                            .as_ref()
                            .map(|node| node.value.as_ref()),
                        position,
                    )
                })
        })
}
fn elm_syntax_module_documentation_find_symbol_at_position<'a>(
    elm_syntax_module_documentation: ElmSyntaxNode<
        &'a [ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
    >,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    elm_syntax_module_documentation
        .value
        .iter()
        .find_map(|documentation_element_node| {
            if !lsp_range_includes_position(documentation_element_node.range, position) {
                return None;
            }
            match &documentation_element_node.value {
                ElmSyntaxModuleDocumentationElement::Markdown(_) => None,
                ElmSyntaxModuleDocumentationElement::AtDocs(member_names) => {
                    member_names.iter().find_map(|member_name_node| {
                        if lsp_range_includes_position(member_name_node.range, position) {
                            Some(ElmSyntaxNode {
                                range: member_name_node.range,
                                value: ElmSyntaxSymbol::ModuleDocumentationAtDocsMember {
                                    name: member_name_node
                                        .value
                                        .trim_start_matches('(')
                                        .trim_end_matches(')'),
                                    module_documentation: elm_syntax_module_documentation.value,
                                },
                            })
                        } else {
                            None
                        }
                    })
                }
            }
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
        let exposing_node: &ElmSyntaxNode<ElmSyntaxExposing> =
            elm_syntax_module_header.exposing.as_ref()?;
        elm_syntax_module_header_exposing_from_module_find_reference_at_position(
            elm_syntax_node_as_ref(exposing_node),
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
    } else if let Some(import_alias_name_node) = &elm_syntax_import_node.value.alias_name
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
                elm_syntax_import_exposing_from_module_find_reference_at_position(
                    &module_name_node.value,
                    elm_syntax_node_as_ref(exposing),
                    position,
                )
            })
    }
}

fn elm_syntax_module_header_exposing_from_module_find_reference_at_position<'a>(
    elm_syntax_exposing_node: ElmSyntaxNode<&'a ElmSyntaxExposing>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_exposing_node.range, position) {
        return None;
    }
    match elm_syntax_exposing_node.value {
        ElmSyntaxExposing::All(_) => None,
        ElmSyntaxExposing::Explicit(exposes) => exposes.iter().find_map(|expose_node| {
            if lsp_range_includes_position(expose_node.range, position) {
                let expose_name: &str = match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => Some(name.value.as_ref()),
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        maybe_symbol.as_ref().map(|symbol_node| symbol_node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_ref()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_ref()),
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
fn elm_syntax_import_exposing_from_module_find_reference_at_position<'a>(
    import_origin_module: &'a str,
    elm_syntax_exposing_node: ElmSyntaxNode<&'a ElmSyntaxExposing>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_exposing_node.range, position) {
        return None;
    }
    match elm_syntax_exposing_node.value {
        ElmSyntaxExposing::All(_) => None,
        ElmSyntaxExposing::Explicit(exposes) => exposes.iter().find_map(|expose_node| {
            if lsp_range_includes_position(expose_node.range, position) {
                let expose_name: &str = match &expose_node.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => Some(name.value.as_ref()),
                    ElmSyntaxExpose::Operator(maybe_symbol) => {
                        maybe_symbol.as_ref().map(|symbol_node| symbol_node.value)
                    }
                    ElmSyntaxExpose::Type(name) => Some(name.as_ref()),
                    ElmSyntaxExpose::Variable(name) => Some(name.as_ref()),
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
                parameters,
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
                            name: operator_node.value,
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
                        elm_syntax_node_as_ref(field_value_node),
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
                    name: type_variable_value,
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
        signature: Option<&'a ElmSyntaxVariableDeclarationSignature>,
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
                    }
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
                        name: operator.value,
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
                            elm_syntax_expression_node,
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
                .try_fold(local_bindings, |local_bindings, field| match &field.value {
                    Some(field_value_node) => elm_syntax_expression_find_reference_at_position(
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
                .try_fold(local_bindings, |local_bindings, field| match &field.value {
                    Some(field_value_node) => elm_syntax_expression_find_reference_at_position(
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
    scope_expression: ElmSyntaxNode<&'a ElmSyntaxExpression>,
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
                    value: ElmSyntaxSymbol::LetDeclarationName {
                        name: &start_name.value,
                        start_name_range: start_name.range,
                        signature_type: maybe_signature
                            .as_ref()
                            .and_then(|signature| signature.type_.as_ref())
                            .map(elm_syntax_node_as_ref),
                        scope_expression: scope_expression,
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
                        value: ElmSyntaxSymbol::LetDeclarationName {
                            name: &start_name.value,
                            start_name_range: start_name.range,
                            signature_type: signature.type_.as_ref().map(elm_syntax_node_as_ref),
                            scope_expression: scope_expression,
                        },
                        range: implementation_name_range,
                    });
                }
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
enum ElmSymbolToReference<'a> {
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
        including_declaration_name: bool,
    },
    VariableOrVariant {
        module_origin: &'a str,
        name: &'a str,
        including_declaration_name: bool,
    },
    RecordTypeAlias {
        module_origin: &'a str,
        name: &'a str,
        including_declaration_name: bool,
    },
    LocalBinding {
        name: &'a str,
        including_let_declaration_name: bool,
    },
}

fn elm_syntax_module_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    state: &State,
    project_state: &ProjectState,
    elm_syntax_module: &ElmSyntaxModule,
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    let maybe_self_module_name: Option<&ElmSyntaxNode<Box<str>>> = elm_syntax_module
        .header
        .as_ref()
        .and_then(|header| header.module_name.as_ref());
    if let Some(self_module_name_node) = maybe_self_module_name
        && let ElmSymbolToReference::ModuleName(module_name_to_collect_uses_of) =
            symbol_to_collect_uses_of
        && module_name_to_collect_uses_of == self_module_name_node.value.as_ref()
    {
        uses_so_far.push(self_module_name_node.range);
        // a module cannot reference itself within its declarations, imports etc
        return;
    }
    let symbol_to_collect_can_occur_here: bool = match symbol_to_collect_uses_of {
        ElmSymbolToReference::ModuleName(module_origin_to_collect_uses_of)
        | ElmSymbolToReference::RecordTypeAlias {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
            including_declaration_name: _,
        }
        | ElmSymbolToReference::TypeNotRecordAlias {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
            including_declaration_name: _,
        }
        | ElmSymbolToReference::VariableOrVariant {
            module_origin: module_origin_to_collect_uses_of,
            name: _,
            including_declaration_name: _,
        } => {
            Some(module_origin_to_collect_uses_of)
                == maybe_self_module_name
                    .as_ref()
                    .map(|node| node.value.as_ref())
                || elm_syntax_module.imports.iter().any(|import| {
                    import
                        .value
                        .module_name
                        .as_ref()
                        .map(|node| node.value.as_ref())
                        == Some(module_origin_to_collect_uses_of)
                })
        }
        ElmSymbolToReference::ImportAlias { .. } => false,
        ElmSymbolToReference::TypeVariable(_) => false,
        ElmSymbolToReference::LocalBinding { .. } => false,
    };
    if !symbol_to_collect_can_occur_here {
        // if not imported, that module name can never appear, so we can skip a bunch of
        // traversing! (unless implicitly imported, but those modules are never renamed!)
        return;
    }
    let self_module_name: &str = maybe_self_module_name
        .map(|node| node.value.as_ref())
        .unwrap_or("");
    if let Some(module_header) = &elm_syntax_module.header
        && let Some(exposing) = &module_header.exposing
    {
        elm_syntax_exposing_uses_of_reference_into(
            uses_so_far,
            self_module_name,
            &exposing.value,
            symbol_to_collect_uses_of,
        );
    }
    if let Some(module_documentation_node) = &elm_syntax_module.documentation {
        elm_syntax_module_documentation_uses_of_reference_into(
            uses_so_far,
            self_module_name,
            &module_documentation_node.value,
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
    for documented_declaration in elm_syntax_module
        .declarations
        .iter()
        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
    {
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
fn elm_syntax_module_documentation_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    elm_syntax_module_documentation: &[ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    let Some(member_to_collect_uses_of) = (match symbol_to_collect_uses_of {
        ElmSymbolToReference::ModuleName(_) => None,
        ElmSymbolToReference::ImportAlias { .. } => None,
        ElmSymbolToReference::TypeVariable(_) => None,
        ElmSymbolToReference::LocalBinding { .. } => None,
        ElmSymbolToReference::TypeNotRecordAlias {
            module_origin: symbol_module_origin,
            name,
            including_declaration_name: _,
        }
        | ElmSymbolToReference::VariableOrVariant {
            module_origin: symbol_module_origin,
            name,
            including_declaration_name: _,
        }
        | ElmSymbolToReference::RecordTypeAlias {
            module_origin: symbol_module_origin,
            name,
            including_declaration_name: _,
        } => {
            if symbol_module_origin == origin_module {
                Some(name)
            } else {
                None
            }
        }
    }) else {
        return;
    };
    for elm_syntax_module_documentation_element_node in elm_syntax_module_documentation {
        match &elm_syntax_module_documentation_element_node.value {
            ElmSyntaxModuleDocumentationElement::Markdown(_) => {}
            ElmSyntaxModuleDocumentationElement::AtDocs(at_docs_member_names) => {
                for at_docs_member_name_node in at_docs_member_names {
                    if at_docs_member_name_node.value.as_ref() == member_to_collect_uses_of {
                        uses_so_far.push(at_docs_member_name_node.range);
                    }
                }
            }
        }
    }
}
fn elm_syntax_import_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    elm_syntax_import: &ElmSyntaxImport,
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    let Some(import_module_name_node) = &elm_syntax_import.module_name else {
        return;
    };
    if let ElmSymbolToReference::ModuleName(module_name_to_collect_uses_of) =
        symbol_to_collect_uses_of
    {
        if module_name_to_collect_uses_of == import_module_name_node.value.as_ref() {
            uses_so_far.push(import_module_name_node.range);
        }
    } else if let ElmSymbolToReference::ImportAlias {
        module_origin: alias_to_collect_uses_of_origin,
        alias_name: alias_to_collect_uses_of_name,
    } = symbol_to_collect_uses_of
    {
        if alias_to_collect_uses_of_origin == import_module_name_node.value.as_ref()
            && let Some(import_alias_name_node) = &elm_syntax_import.alias_name
            && alias_to_collect_uses_of_name == import_alias_name_node.value.as_ref()
        {
            uses_so_far.push(import_alias_name_node.range);
        }
    } else if let Some(exposing) = &elm_syntax_import.exposing {
        elm_syntax_exposing_uses_of_reference_into(
            uses_so_far,
            &import_module_name_node.value,
            &exposing.value,
            symbol_to_collect_uses_of,
        );
    }
}

fn elm_syntax_exposing_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    elm_syntax_exposing: &ElmSyntaxExposing,
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    match elm_syntax_exposing {
        ElmSyntaxExposing::All(_) => {}
        ElmSyntaxExposing::Explicit(exposes) => {
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name,
                        open_range: _,
                    } => {
                        if let ElmSymbolToReference::TypeNotRecordAlias {
                            name: symbol_name,
                            module_origin: symbol_module_origin,
                            including_declaration_name: _,
                        } = symbol_to_collect_uses_of
                            && symbol_name == name.value.as_ref()
                            && symbol_module_origin == origin_module
                        {
                            uses_so_far.push(name.range);
                        }
                    }
                    ElmSyntaxExpose::Operator(_) => {}
                    ElmSyntaxExpose::Type(name) => {
                        if let ElmSymbolToReference::TypeNotRecordAlias {
                            name: symbol_name,
                            module_origin: symbol_module_origin,
                            including_declaration_name: _,
                        }
                        | ElmSymbolToReference::RecordTypeAlias {
                            name: symbol_name,
                            module_origin: symbol_module_origin,
                            including_declaration_name: _,
                        } = symbol_to_collect_uses_of
                            && symbol_name == name.as_ref()
                            && symbol_module_origin == origin_module
                        {
                            uses_so_far.push(expose.range);
                        }
                    }
                    ElmSyntaxExpose::Variable(name) => {
                        if let ElmSymbolToReference::VariableOrVariant {
                            name: symbol_name,
                            module_origin: symbol_module_origin,
                            including_declaration_name: _,
                        } = symbol_to_collect_uses_of
                            && symbol_name == name.as_ref()
                            && symbol_module_origin == origin_module
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
    symbol_to_collect_uses_of: ElmSymbolToReference,
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
                    == (ElmSymbolToReference::TypeNotRecordAlias {
                        module_origin: origin_module,
                        name: &name_node.value,
                        including_declaration_name: true,
                    })
            {
                uses_so_far.push(name_node.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmSymbolToReference::TypeVariable(&parameter_node.value)
                {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal;
                }
            }
            if let Some(variant0_name_node) = maybe_variant0_name
                && symbol_to_collect_uses_of
                    == (ElmSymbolToReference::VariableOrVariant {
                        name: &variant0_name_node.value,
                        module_origin: origin_module,
                        including_declaration_name: true,
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
                    && (ElmSymbolToReference::VariableOrVariant {
                        name: &variant_name_node.value,
                        module_origin: origin_module,
                        including_declaration_name: true,
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
                    == (ElmSymbolToReference::VariableOrVariant {
                        name: &name_node.value,
                        module_origin: origin_module,
                        including_declaration_name: true,
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
                    == (ElmSymbolToReference::TypeNotRecordAlias {
                        name: &name_node.value,
                        module_origin: origin_module,
                        including_declaration_name: true,
                    }))
                    || (symbol_to_collect_uses_of
                        == (ElmSymbolToReference::RecordTypeAlias {
                            name: &name_node.value,
                            module_origin: origin_module,
                            including_declaration_name: true,
                        })))
            {
                uses_so_far.push(name_node.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmSymbolToReference::TypeVariable(&parameter_node.value)
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
                == (ElmSymbolToReference::VariableOrVariant {
                    name: &start_name_node.value,
                    module_origin: origin_module,
                    including_declaration_name: true,
                })
            {
                uses_so_far.push(start_name_node.range);
            }
            if let Some(signature) = maybe_signature {
                if let Some(implementation_name_range) = signature.implementation_name_range
                    && symbol_to_collect_uses_of
                        == (ElmSymbolToReference::VariableOrVariant {
                            name: &start_name_node.value,
                            module_origin: origin_module,
                            including_declaration_name: true,
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
            let mut parameter_bindings: Vec<ElmLocalBinding> = Vec::new();
            for parameter_node in parameters {
                elm_syntax_pattern_bindings_into(
                    &mut parameter_bindings,
                    elm_syntax_node_as_ref(parameter_node),
                );
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
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    match elm_syntax_type_node.value {
        ElmSyntaxType::Construct {
            reference,
            arguments,
        } => {
            let module_origin: &str = look_up_origin_module(
                module_origin_lookup,
                ElmQualified {
                    qualification: &reference.value.qualification,
                    name: &reference.value.name,
                },
            );
            if let ElmSymbolToReference::TypeNotRecordAlias {
                name: symbol_name,
                module_origin: symbol_module_origin,
                including_declaration_name: _,
            }
            | ElmSymbolToReference::RecordTypeAlias {
                name: symbol_name,
                module_origin: symbol_module_origin,
                including_declaration_name: _,
            } = symbol_to_collect_uses_of
                && symbol_module_origin == module_origin
                && symbol_name == reference.value.name.as_ref()
            {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(
                        reference.range.end,
                        -(reference.value.name.len() as i32),
                    ),
                    end: reference.range.end,
                });
            }
            if (symbol_to_collect_uses_of
                == (ElmSymbolToReference::ImportAlias {
                    module_origin: module_origin,
                    alias_name: &reference.value.qualification,
                }))
                || (symbol_to_collect_uses_of == ElmSymbolToReference::ModuleName(module_origin))
                    && (reference.value.qualification.as_ref() == module_origin)
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
                    == ElmSymbolToReference::TypeVariable(&record_variable_node.value)
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
            if symbol_to_collect_uses_of == ElmSymbolToReference::TypeVariable(variable) {
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
    symbol_to_collect_uses_of: ElmSymbolToReference,
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
                if let ElmSymbolToReference::LocalBinding {
                    name: symbol_name,
                    including_let_declaration_name: _,
                } = symbol_to_collect_uses_of
                    && symbol_name == record_variable_node.value.as_ref()
                {
                    if local_bindings.iter().any(|local_binding| {
                        local_binding.name == record_variable_node.value.as_ref()
                    }) {
                        uses_so_far.push(record_variable_node.range);
                    }
                } else if let ElmSymbolToReference::VariableOrVariant {
                    module_origin: symbol_module_origin,
                    name: symbol_name,
                    including_declaration_name: _,
                } = symbol_to_collect_uses_of
                    && symbol_module_origin
                        == look_up_origin_module(
                            module_origin_lookup,
                            ElmQualified {
                                qualification: "",
                                name: &record_variable_node.value,
                            },
                        )
                    && symbol_name == record_variable_node.value.as_ref()
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
            if let ElmSymbolToReference::LocalBinding {
                name: symbol_name,
                including_let_declaration_name: _,
            } = symbol_to_collect_uses_of
                && symbol_name == name.as_ref()
            {
                if qualification.is_empty()
                    && local_bindings
                        .iter()
                        .any(|local_binding| local_binding.name == name.as_ref())
                {
                    uses_so_far.push(elm_syntax_expression_node.range);
                }
            } else {
                let module_origin: &str = look_up_origin_module(
                    module_origin_lookup,
                    ElmQualified {
                        qualification: qualification,
                        name: name,
                    },
                );
                if let ElmSymbolToReference::VariableOrVariant {
                    module_origin: symbol_module_origin,
                    name: symbol_name,
                    including_declaration_name: _,
                }
                | ElmSymbolToReference::RecordTypeAlias {
                    module_origin: symbol_module_origin,
                    name: symbol_name,
                    including_declaration_name: _,
                } = symbol_to_collect_uses_of
                    && symbol_module_origin == module_origin
                    && symbol_name == name.as_ref()
                {
                    uses_so_far.push(lsp_types::Range {
                        start: lsp_position_add_characters(
                            elm_syntax_expression_node.range.end,
                            -(name.len() as i32),
                        ),
                        end: elm_syntax_expression_node.range.end,
                    });
                } else if (symbol_to_collect_uses_of
                    == (ElmSymbolToReference::ImportAlias {
                        module_origin: module_origin,
                        alias_name: qualification,
                    }))
                    || ((symbol_to_collect_uses_of
                        == ElmSymbolToReference::ModuleName(module_origin))
                        && (qualification.as_ref() == module_origin))
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
    symbol_to_collect_uses_of: ElmSymbolToReference,
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
            if symbol_to_collect_uses_of
                == (ElmSymbolToReference::LocalBinding {
                    name: &start_name_node.value,
                    including_let_declaration_name: true,
                })
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
    symbol_to_collect_uses_of: ElmSymbolToReference,
) {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern: alias_pattern,
            as_keyword_range: _,
            variable: _,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(alias_pattern),
                symbol_to_collect_uses_of,
            );
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
        ElmSyntaxPattern::Record(_) => {}
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
        ElmSyntaxPattern::Variable(_) => {}
        ElmSyntaxPattern::Variant { reference, values } => {
            let module_origin: &str = look_up_origin_module(
                module_origin_lookup,
                ElmQualified {
                    qualification: &reference.value.qualification,
                    name: &reference.value.name,
                },
            );
            if let ElmSymbolToReference::VariableOrVariant {
                module_origin: symbol_module_origin,
                name: symbol_name,
                including_declaration_name: _,
            } = symbol_to_collect_uses_of
                && symbol_module_origin == module_origin
                && symbol_name == reference.value.name.as_ref()
            {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(
                        reference.range.end,
                        -(reference.value.name.len() as i32),
                    ),
                    end: reference.range.end,
                });
            }
            if (symbol_to_collect_uses_of
                == (ElmSymbolToReference::ImportAlias {
                    module_origin: module_origin,
                    alias_name: &reference.value.qualification,
                }))
                || ((symbol_to_collect_uses_of == ElmSymbolToReference::ModuleName(module_origin))
                    && (reference.value.qualification.as_ref() == module_origin))
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
                    signature: signature.as_ref(),
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
    DeclaredVariable,
    Operator,
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
        elm_syntax_highlight_module_documentation_into(
            highlighted_so_far,
            elm_syntax_node_as_ref_map(documentation_node, Vec::as_slice),
        );
    }
    for import_node in elm_syntax_module.imports.iter() {
        elm_syntax_highlight_import_into(highlighted_so_far, elm_syntax_node_as_ref(import_node));
    }
    for documented_declaration in elm_syntax_module
        .declarations
        .iter()
        .filter_map(|declaration_or_err| declaration_or_err.as_ref().ok())
    {
        if let Some(documentation_node) = &documented_declaration.documentation {
            highlighted_so_far.extend(
                elm_syntax_highlight_multi_line(elm_syntax_node_unbox(documentation_node), 3, 2)
                    .map(|range| ElmSyntaxNode {
                        range: range,
                        value: ElmSyntaxHighlightKind::Comment,
                    }),
            );
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
fn elm_syntax_highlight_module_documentation_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_module_documentation_node: ElmSyntaxNode<
        &[ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>],
    >,
) {
    highlighted_so_far.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: elm_syntax_module_documentation_node.range.start,
            end: lsp_position_add_characters(elm_syntax_module_documentation_node.range.start, 3),
        },
        value: ElmSyntaxHighlightKind::Comment,
    });
    for elm_syntax_module_documentation_element_node in elm_syntax_module_documentation_node.value {
        match &elm_syntax_module_documentation_element_node.value {
            ElmSyntaxModuleDocumentationElement::Markdown(markdown) => {
                highlighted_so_far.extend(
                    elm_syntax_highlight_multi_line(
                        ElmSyntaxNode {
                            range: elm_syntax_module_documentation_element_node.range,
                            value: markdown,
                        },
                        0,
                        0,
                    )
                    .map(|range| ElmSyntaxNode {
                        range: range,
                        value: ElmSyntaxHighlightKind::Comment,
                    }),
                );
            }
            ElmSyntaxModuleDocumentationElement::AtDocs(member_names) => {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: elm_syntax_module_documentation_element_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_module_documentation_element_node.range.start,
                            5,
                        ),
                    },
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                for member_name_node in member_names {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: member_name_node.range,
                        value: if member_name_node.value.starts_with(char::is_uppercase) {
                            ElmSyntaxHighlightKind::Type
                        } else if member_name_node.value.starts_with(char::is_lowercase) {
                            ElmSyntaxHighlightKind::DeclaredVariable
                        } else {
                            ElmSyntaxHighlightKind::Operator
                        },
                    });
                }
            }
        }
    }
    highlighted_so_far.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: elm_syntax_module_documentation_node.range.end,
            end: lsp_position_add_characters(elm_syntax_module_documentation_node.range.end, 2),
        },
        value: ElmSyntaxHighlightKind::Comment,
    });
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
            if let Some(exposing_keyword_range) = elm_syntax_module_header.exposing_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: exposing_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(exposing_node) = &elm_syntax_module_header.exposing {
                elm_syntax_highlight_exposing_into(highlighted_so_far, &exposing_node.value);
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
                    value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                    value: ElmSyntaxHighlightKind::DeclaredVariable,
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
            if let Some(exposing_keyword_range) = elm_syntax_module_header.exposing_keyword_range {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: exposing_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
            }
            if let Some(exposing) = &elm_syntax_module_header.exposing {
                elm_syntax_highlight_exposing_into(highlighted_so_far, &exposing.value);
            }
        }
    }
}

fn elm_syntax_highlight_and_place_comment_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_comment_node: ElmSyntaxNode<&ElmSyntaxComment>,
) {
    let insert_index: usize = highlighted_so_far
        .binary_search_by(|token| token.range.start.cmp(&elm_syntax_comment_node.range.start))
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
                elm_syntax_highlight_multi_line(
                    ElmSyntaxNode {
                        range: elm_syntax_comment_node.range,
                        value: &elm_syntax_comment_node.value.content,
                    },
                    2,
                    2,
                )
                .map(|range| ElmSyntaxNode {
                    range: range,
                    value: ElmSyntaxHighlightKind::Comment,
                }),
            );
        }
    }
}
fn elm_syntax_highlight_multi_line(
    elm_syntax_str_node: ElmSyntaxNode<&str>,
    characters_before_content: usize,
    characters_after_content: usize,
) -> impl Iterator<Item = lsp_types::Range> {
    let content_does_not_break_line: bool =
        elm_syntax_str_node.range.start.line == elm_syntax_str_node.range.end.line;
    elm_syntax_str_node
        .value
        .lines()
        .chain(
            // str::lines() eats the last linebreak. Restore it
            if elm_syntax_str_node.value.ends_with("\n") {
                Some("\n")
            } else {
                None
            },
        )
        .enumerate()
        .map(move |(inner_line, inner_line_str)| {
            let line: u32 = elm_syntax_str_node.range.start.line + (inner_line as u32);
            let line_length_utf16: usize = inner_line_str.encode_utf16().count();
            if inner_line == 0 {
                lsp_types::Range {
                    start: elm_syntax_str_node.range.start,
                    end: lsp_position_add_characters(
                        elm_syntax_str_node.range.start,
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
                    end: if line == elm_syntax_str_node.range.end.line {
                        elm_syntax_str_node.range.end
                    } else {
                        lsp_types::Position {
                            line: line,
                            character: (line_length_utf16 + characters_after_content) as u32,
                        }
                    },
                }
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
    if let Some(as_keyword_range) = elm_syntax_import_node.value.as_keyword_range {
        highlighted_so_far.push(ElmSyntaxNode {
            range: as_keyword_range,
            value: ElmSyntaxHighlightKind::KeySymbol,
        });
    }
    if let Some(alias_name_node) = &elm_syntax_import_node.value.alias_name {
        highlighted_so_far.push(ElmSyntaxNode {
            range: alias_name_node.range,
            value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
        });
    }
    if let Some(exposing_keyword_range) = elm_syntax_import_node.value.exposing_keyword_range {
        highlighted_so_far.push(ElmSyntaxNode {
            range: exposing_keyword_range,
            value: ElmSyntaxHighlightKind::KeySymbol,
        });
    }
    if let Some(exposing_node) = &elm_syntax_import_node.value.exposing {
        elm_syntax_highlight_exposing_into(highlighted_so_far, &exposing_node.value);
    }
}

fn elm_syntax_highlight_exposing_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_exposing: &ElmSyntaxExposing,
) {
    match elm_syntax_exposing {
        ElmSyntaxExposing::All(ellipsis_range) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: *ellipsis_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
        }
        ElmSyntaxExposing::Explicit(exposes) => {
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
                                value: ElmSyntaxHighlightKind::Operator,
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
                            value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                        value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                let mut local_bindings: Vec<ElmLocalBinding> = Vec::new();
                for parameter_node in parameters {
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    &local_bindings,
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
                    value: ElmSyntaxHighlightKind::Operator,
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
                    value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                    value: ElmSyntaxHighlightKind::DeclaredVariable,
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
        });
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
                value: ElmSyntaxHighlightKind::Operator,
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
    local_bindings: &[ElmLocalBinding],
    elm_syntax_expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
) {
    match elm_syntax_expression_node.value {
        ElmSyntaxExpression::Call {
            called,
            argument0,
            argument1_up,
        } => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                local_bindings,
                elm_syntax_node_unbox(called),
            );
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                local_bindings,
                elm_syntax_node_unbox(argument0),
            );
            for argument_node in argument1_up {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
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
                    local_bindings,
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
                    let mut local_bindings: Vec<ElmLocalBinding> = local_bindings.to_vec();
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings,
                        elm_syntax_node_as_ref(&case.pattern),
                    );
                    elm_syntax_highlight_expression_into(
                        highlighted_so_far,
                        &local_bindings,
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
                    local_bindings,
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
                    local_bindings,
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
                    local_bindings,
                    elm_syntax_node_unbox(on_false_node),
                );
            }
        }
        ElmSyntaxExpression::InfixOperationIgnoringPrecedence {
            left,
            operator: operator_node,
            right: maybe_right,
        } => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                local_bindings,
                elm_syntax_node_unbox(left),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: operator_node.range,
                value: ElmSyntaxHighlightKind::Operator,
            });
            if let Some(right_node) = maybe_right {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
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
                let mut local_bindings: Vec<ElmLocalBinding> = local_bindings.to_vec();
                for parameter_node in parameters {
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    &local_bindings,
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
            let mut local_bindings: Vec<ElmLocalBinding> = local_bindings.to_vec();
            for let_declaration_node in declarations {
                elm_syntax_let_declaration_introduced_bindings_into(
                    &mut local_bindings,
                    &let_declaration_node.value,
                );
            }
            for let_declaration_node in declarations {
                elm_syntax_highlight_let_declaration_into(
                    highlighted_so_far,
                    &local_bindings,
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
                    &local_bindings,
                    elm_syntax_node_unbox(result_node),
                );
            }
        }
        ElmSyntaxExpression::List(elements) => {
            for element_node in elements {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
                    elm_syntax_node_as_ref(element_node),
                );
            }
        }
        ElmSyntaxExpression::Negation(maybe_in_negation) => {
            if let Some(in_negation_node) = maybe_in_negation {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
                    elm_syntax_node_unbox(in_negation_node),
                );
            }
        }
        ElmSyntaxExpression::OperatorFunction(operator_node) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: operator_node.range,
                value: ElmSyntaxHighlightKind::Operator,
            });
        }
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                local_bindings,
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
                        local_bindings,
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
                local_bindings,
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
                        local_bindings,
                        elm_syntax_node_as_ref(value_node),
                    );
                }
            }
        }
        ElmSyntaxExpression::Reference {
            qualification,
            name,
        } => {
            if qualification.is_empty()
                && let Some(origin_binding) = local_bindings
                    .iter()
                    .find(|bind| bind.name == name.as_ref())
            {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: elm_syntax_expression_node.range,
                    value: match origin_binding.origin {
                        LocalBindingOrigin::PatternVariable(_) => ElmSyntaxHighlightKind::Variable,
                        LocalBindingOrigin::PatternRecordField(_) => {
                            ElmSyntaxHighlightKind::Variable
                        }
                        LocalBindingOrigin::LetDeclaredVariable { .. } => {
                            ElmSyntaxHighlightKind::DeclaredVariable
                        }
                    },
                });
            } else {
                elm_syntax_highlight_qualified_into(
                    highlighted_so_far,
                    ElmSyntaxNode {
                        range: elm_syntax_expression_node.range,
                        value: ElmQualified {
                            qualification: qualification.as_ref(),
                            name: name.as_ref(),
                        },
                    },
                    if name.starts_with(|c: char| c.is_uppercase()) {
                        ElmSyntaxHighlightKind::Variant
                    } else {
                        ElmSyntaxHighlightKind::DeclaredVariable
                    },
                );
            }
        }
        ElmSyntaxExpression::String {
            content,
            quoting_style,
        } => {
            let quote_count: usize = match quoting_style {
                ElmSyntaxStringQuotingStyle::SingleQuoted => 1,
                ElmSyntaxStringQuotingStyle::TripleQuoted => 3,
            };
            highlighted_so_far.extend(
                elm_syntax_highlight_multi_line(
                    ElmSyntaxNode {
                        range: elm_syntax_expression_node.range,
                        value: content,
                    },
                    quote_count,
                    quote_count,
                )
                .map(|range| ElmSyntaxNode {
                    range: range,
                    value: ElmSyntaxHighlightKind::String,
                }),
            );
        }
        ElmSyntaxExpression::Triple {
            part0: maybe_part0,
            part1: maybe_part1,
            part2: maybe_part2,
        } => {
            if let Some(part0_node) = maybe_part0 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
                    elm_syntax_node_unbox(part1_node),
                );
            }
            if let Some(part2_node) = maybe_part2 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
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
                    local_bindings,
                    elm_syntax_node_unbox(part0_node),
                );
            }
            if let Some(part1_node) = maybe_part1 {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    local_bindings,
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
    local_bindings: &[ElmLocalBinding],
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
                    local_bindings,
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
                value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                        value: ElmSyntaxHighlightKind::DeclaredVariable,
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
                let mut local_bindings: Vec<ElmLocalBinding> = local_bindings.to_vec();
                for parameter_node in parameters {
                    elm_syntax_pattern_bindings_into(
                        &mut local_bindings,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    &local_bindings,
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
    content: Box<str>,
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
/// prefer using after `parse_line_break` or similar failed
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
/// prefer using after `parse_line_break` or similar failed
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
fn parse_same_line_while_at_least_one_as_node(
    state: &mut ParseState,
    char_is_valid: impl Fn(char) -> bool + Copy,
) -> Option<ElmSyntaxNode<Box<str>>> {
    let start_position: lsp_types::Position = state.position;
    let start_offset_utf8: usize = state.offset_utf8;
    if !parse_same_line_char_if(state, char_is_valid) {
        return None;
    }
    parse_same_line_while(state, char_is_valid);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: Box::from(&state.source[start_offset_utf8..state.offset_utf8]),
    })
}
fn parse_before_next_linebreak(state: &mut ParseState) {
    parse_same_line_while(state, |c| c != '\r' && c != '\n');
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
    } else if parse_same_line_char_if(state, |c| ('1'..='9').contains(&c)) {
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

fn parse_elm_whitespace_and_comments(state: &mut ParseState) {
    while parse_linebreak(state)
        || parse_same_line_char_if(state, char::is_whitespace)
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
    let content: &str = state.source[state.offset_utf8..]
        .lines()
        .next()
        .unwrap_or("");
    state.offset_utf8 += content.len();
    state.position.character += content.encode_utf16().count() as u32;
    let full_range: lsp_types::Range = lsp_types::Range {
        start: position_before,
        end: state.position,
    };
    state.comments.push(ElmSyntaxNode {
        range: full_range,
        value: ElmSyntaxComment {
            content: Box::from(content),
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
            }
            nesting_level -= 1;
        } else if parse_any_guaranteed_non_linebreak_char(state) {
        } else {
            // end of source
            break 'until_fully_unnested;
        }
    }
    let content_including_closing: &str =
        &state.source[content_start_offset_utf8..state.offset_utf8];
    state.comments.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: ElmSyntaxComment {
            content: Box::from(
                content_including_closing
                    .strip_suffix("-}")
                    .unwrap_or(content_including_closing),
            ),
            kind: ElmSyntaxCommentKind::Block,
        },
    });
    true
}
fn parse_elm_documentation_comment_block_str<'a>(state: &mut ParseState<'a>) -> Option<&'a str> {
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
            }
            nesting_level -= 1;
        } else if parse_any_guaranteed_non_linebreak_char(state) {
        } else {
            // end of source
            break 'until_fully_unnested;
        }
    }
    let content_including_closing: &str =
        &state.source[content_start_offset_utf8..state.offset_utf8];
    Some(
        content_including_closing
            .strip_suffix("-}")
            .unwrap_or(content_including_closing),
    )
}
fn parse_elm_documentation_comment_block_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<Box<str>>> {
    let start_position: lsp_types::Position = state.position;
    let content: &str = parse_elm_documentation_comment_block_str(state)?;
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: Box::from(content),
    })
}
fn parse_elm_syntax_module_documentation_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<Vec<ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>>>> {
    let start_position: lsp_types::Position = state.position;
    let start_offset_utf8: usize = state.offset_utf8;
    let _content: &str = parse_elm_documentation_comment_block_str(state)?;
    let end_position: lsp_types::Position = state.position;
    let end_offset_utf8: usize = state.offset_utf8;
    // reset state to the start of the content
    state.offset_utf8 = start_offset_utf8 + 3;
    state.position = lsp_position_add_characters(start_position, 3);
    let mut parsed_content_elements: Vec<ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>> =
        Vec::new();
    let mut previous_at_docs_end_position: lsp_types::Position = state.position;
    let mut previous_at_docs_end_offset_utf8: usize = state.offset_utf8;
    'parsing_content: while state.offset_utf8 < end_offset_utf8 - 2 {
        let before_potential_at_docs_offset_utf8: usize = state.offset_utf8;
        if let Some(at_docs_key_symbol_range) = parse_symbol_as_range(state, "@docs") {
            parsed_content_elements.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: previous_at_docs_end_position,
                    end: at_docs_key_symbol_range.start,
                },
                value: ElmSyntaxModuleDocumentationElement::Markdown(Box::from(
                    &state.source
                        [previous_at_docs_end_offset_utf8..before_potential_at_docs_offset_utf8],
                )),
            });
            let mut member_names: Vec<ElmSyntaxNode<Box<str>>> = Vec::new();
            'parsing_at_docs_member_names: loop {
                if let Some(expose_name_node) =
                    parse_same_line_while_at_least_one_as_node(state, |c| {
                        c != ',' && (c.is_alphanumeric() || c.is_ascii_punctuation())
                    })
                {
                    member_names.push(expose_name_node);
                } else if (state.source[state.offset_utf8..].starts_with('\n')
                    && !state.source[state.offset_utf8..]
                        .chars()
                        .skip(1)
                        .next()
                        .is_some_and(|c| c.is_ascii_whitespace()))
                    || (state.source[state.offset_utf8..].starts_with("\r\n")
                        && !state.source[state.offset_utf8..]
                            .chars()
                            .skip(2)
                            .next()
                            .is_some_and(|c| c.is_ascii_whitespace()))
                    || (state.offset_utf8 >= end_offset_utf8 - 2)
                    || !(parse_linebreak(state) || parse_any_guaranteed_non_linebreak_char(state))
                {
                    break 'parsing_at_docs_member_names;
                }
            }
            parsed_content_elements.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: at_docs_key_symbol_range.start,
                    end: state.position,
                },
                value: ElmSyntaxModuleDocumentationElement::AtDocs(member_names),
            });
            previous_at_docs_end_position = state.position;
            previous_at_docs_end_offset_utf8 = state.offset_utf8;
        } else {
            if !(parse_linebreak(state) || parse_any_guaranteed_non_linebreak_char(state)) {
                break 'parsing_content;
            }
        }
    }
    parsed_content_elements.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: previous_at_docs_end_position,
            end: state.position,
        },
        value: ElmSyntaxModuleDocumentationElement::Markdown(Box::from(
            &state.source[previous_at_docs_end_offset_utf8..state.offset_utf8],
        )),
    });
    state.position = end_position;
    state.offset_utf8 = end_offset_utf8;
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: parsed_content_elements,
    })
}
fn parse_elm_lowercase_as_box_str(state: &mut ParseState) -> Option<Box<str>> {
    let mut chars_from_offset: std::str::Chars = state.source[state.offset_utf8..].chars();
    if let Some(first_char) = chars_from_offset.next()
        && first_char.is_lowercase()
    {
        let parsed_length: usize = first_char.len_utf8()
            + chars_from_offset
                .take_while(|&c| c.is_alphanumeric() || c == '_')
                .map(char::len_utf8)
                .sum::<usize>();
        let end_offset_utf8: usize = state.offset_utf8 + parsed_length;
        let parsed_str: &str = &state.source[state.offset_utf8..end_offset_utf8];
        state.offset_utf8 = end_offset_utf8;
        state.position.character += parsed_str.encode_utf16().count() as u32;
        Some(Box::from(parsed_str))
    } else {
        None
    }
}
fn parse_elm_lowercase_as_node(state: &mut ParseState) -> Option<ElmSyntaxNode<Box<str>>> {
    let start_position: lsp_types::Position = state.position;
    parse_elm_lowercase_as_box_str(state).map(|name| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: name,
    })
}
fn parse_elm_uppercase(state: &mut ParseState) -> Option<Box<str>> {
    let mut chars_from_offset = state.source[state.offset_utf8..].chars();
    if let Some(first_char) = chars_from_offset.next()
        && first_char.is_uppercase()
    {
        let parsed_length: usize = first_char.len_utf8()
            + chars_from_offset
                .take_while(|&c| c.is_alphanumeric() || c == '_')
                .map(char::len_utf8)
                .sum::<usize>();
        let end_offset_utf8: usize = state.offset_utf8 + parsed_length;
        let parsed_str: &str = &state.source[state.offset_utf8..end_offset_utf8];
        state.offset_utf8 = end_offset_utf8;
        state.position.character += parsed_str.encode_utf16().count() as u32;
        Some(Box::from(parsed_str))
    } else {
        None
    }
}
fn parse_elm_uppercase_node(state: &mut ParseState) -> Option<ElmSyntaxNode<Box<str>>> {
    let start_position: lsp_types::Position = state.position;
    parse_elm_uppercase(state).map(|name| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: name,
    })
}
fn parse_elm_standalone_module_name_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<Box<str>>> {
    // very lenient, even allowing lowercase in most places it's usually forbidden
    // to allow for more convenient autocomplete without pressing shift
    let start_offset_utf8: usize = state.offset_utf8;
    let start_position: lsp_types::Position = state.position;
    if !parse_same_line_char_if(state, char::is_alphabetic) {
        return None;
    }
    parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_' || c == '.');
    let parsed_name_node: ElmSyntaxNode<Box<str>> = ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: Box::from(&state.source[start_offset_utf8..state.offset_utf8]),
    };
    Some(parsed_name_node)
}
fn parse_elm_operator_node(state: &mut ParseState) -> Option<ElmSyntaxNode<&'static str>> {
    // can be optimized by only slicing once for each symbol length
    let start_position: lsp_types::Position = state.position;
    parse_symbol_as_str(state, "</>")
        .or_else(|| parse_symbol_as_str(state, "<?>"))
        .or_else(|| parse_symbol_as_str(state, "=="))
        .or_else(|| parse_symbol_as_str(state, "/="))
        .or_else(|| parse_symbol_as_str(state, "::"))
        .or_else(|| parse_symbol_as_str(state, "++"))
        .or_else(|| parse_symbol_as_str(state, "<|"))
        .or_else(|| parse_symbol_as_str(state, "|>"))
        .or_else(|| parse_symbol_as_str(state, "<<"))
        .or_else(|| parse_symbol_as_str(state, ">>"))
        .or_else(|| parse_symbol_as_str(state, "||"))
        .or_else(|| parse_symbol_as_str(state, "&&"))
        .or_else(|| parse_symbol_as_str(state, "<="))
        .or_else(|| parse_symbol_as_str(state, ">="))
        .or_else(|| parse_symbol_as_str(state, "|="))
        .or_else(|| parse_symbol_as_str(state, "|."))
        .or_else(|| parse_symbol_as_str(state, "//"))
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
    let start_position: lsp_types::Position = state.position;
    parse_symbol_as(state, "</>)", "</>")
        .or_else(|| parse_symbol_as(state, "<?>)", "<?>"))
        .or_else(|| parse_symbol_as(state, "==)", "=="))
        .or_else(|| parse_symbol_as(state, "/=)", "/="))
        .or_else(|| parse_symbol_as(state, "::)", "::"))
        .or_else(|| parse_symbol_as(state, "++)", "++"))
        .or_else(|| parse_symbol_as(state, "<|)", "<|"))
        .or_else(|| parse_symbol_as(state, "|>)", "|>"))
        .or_else(|| parse_symbol_as(state, "<<)", "<<"))
        .or_else(|| parse_symbol_as(state, ">>)", ">>"))
        .or_else(|| parse_symbol_as(state, "||)", "||"))
        .or_else(|| parse_symbol_as(state, "&&)", "&&"))
        .or_else(|| parse_symbol_as(state, "<=)", "<="))
        .or_else(|| parse_symbol_as(state, ">=)", ">="))
        .or_else(|| parse_symbol_as(state, "|=)", "|="))
        .or_else(|| parse_symbol_as(state, "|.)", "|."))
        .or_else(|| parse_symbol_as(state, "//)", "//"))
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
    if state.position.character == 0 {
        return None;
    }
    let start_position: lsp_types::Position = state.position;
    if parse_symbol(state, "(") {
        parse_elm_whitespace_and_comments(state);
        let maybe_operator_symbol: Option<ElmSyntaxNode<&str>> = parse_elm_operator_node(state);
        parse_elm_whitespace_and_comments(state);
        let _: bool = parse_symbol(state, ")");
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_position,
                end: state.position,
            },
            value: ElmSyntaxExpose::Operator(maybe_operator_symbol),
        })
    } else if let Some(variable_name) = parse_elm_lowercase_as_box_str(state) {
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
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, ")");
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
    let import_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "import")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_module_name_node: Option<ElmSyntaxNode<Box<str>>> =
        parse_elm_standalone_module_name_node(state);
    parse_elm_whitespace_and_comments(state);
    let maybe_as_keyword_range: Option<lsp_types::Range> = parse_elm_keyword_as_range(state, "as");
    parse_elm_whitespace_and_comments(state);
    let maybe_alias_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_uppercase_node(state);
    parse_elm_whitespace_and_comments(state);
    let maybe_exposing_keyword_range: Option<lsp_types::Range> =
        parse_elm_keyword_as_range(state, "exposing");
    parse_elm_whitespace_and_comments(state);
    let maybe_exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>> =
        parse_elm_syntax_exposing_node(state);
    let end_position: lsp_types::Position = maybe_exposing
        .as_ref()
        .map(|exposing| exposing.range.end)
        .or_else(|| maybe_exposing_keyword_range.map(|range| range.end))
        .or_else(|| maybe_alias_name.as_ref().map(|node| node.range.end))
        .or_else(|| maybe_as_keyword_range.map(|range| range.end))
        .or_else(|| maybe_module_name_node.as_ref().map(|node| node.range.end))
        .unwrap_or(import_keyword_range.end);
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: import_keyword_range.start,
            end: end_position,
        },
        value: ElmSyntaxImport {
            module_name: maybe_module_name_node,
            as_keyword_range: maybe_as_keyword_range,
            alias_name: maybe_alias_name,
            exposing_keyword_range: maybe_exposing_keyword_range,
            exposing: maybe_exposing,
        },
    })
}
fn parse_elm_syntax_exposing_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxExposing>> {
    let start_position: lsp_types::Position = state.position;
    if !parse_symbol(state, "(") {
        return None;
    }
    parse_elm_whitespace_and_comments(state);
    let exposing: ElmSyntaxExposing = match parse_symbol_as_range(state, "..") {
        Some(all_range) => {
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, ")");
            ElmSyntaxExposing::All(all_range)
        }
        None => {
            let mut expose_nodes: Vec<ElmSyntaxNode<ElmSyntaxExpose>> = Vec::new();
            while let Some(expose_node) = parse_elm_syntax_expose_node(state) {
                expose_nodes.push(expose_node);
                parse_elm_whitespace_and_comments(state);
                while parse_symbol(state, ",") {
                    parse_elm_whitespace_and_comments(state);
                }
            }
            let _: bool = parse_symbol(state, ")");
            ElmSyntaxExposing::Explicit(expose_nodes)
        }
    };
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: exposing,
    })
}

fn parse_elm_syntax_module_header(state: &mut ParseState) -> Option<ElmSyntaxModuleHeader> {
    if let Some(module_keyword_range) = parse_symbol_as_range(state, "module") {
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<Box<str>>> =
            parse_elm_standalone_module_name_node(state);
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing_keyword_range: Option<lsp_types::Range> =
            parse_elm_keyword_as_range(state, "exposing");
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>> =
            parse_elm_syntax_exposing_node(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Pure {
                module_keyword_range: module_keyword_range,
            },
            module_name: maybe_module_name_node,
            exposing_keyword_range: maybe_exposing_keyword_range,
            exposing: maybe_exposing,
        })
    } else if let Some(port_keyword_range) = parse_symbol_as_range(state, "port") {
        parse_elm_whitespace_and_comments(state);
        let module_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "module")?;
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<Box<str>>> =
            parse_elm_standalone_module_name_node(state);
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing_keyword_range: Option<lsp_types::Range> =
            parse_elm_keyword_as_range(state, "exposing");
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>> =
            parse_elm_syntax_exposing_node(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Port {
                port_keyword_range: port_keyword_range,
                module_keyword_range: module_keyword_range,
            },
            module_name: maybe_module_name_node,
            exposing_keyword_range: maybe_exposing_keyword_range,
            exposing: maybe_exposing,
        })
    } else if let Some(effect_keyword_range) = parse_symbol_as_range(state, "effect") {
        parse_elm_whitespace_and_comments(state);
        let module_keyword_range: lsp_types::Range = parse_symbol_as_range(state, "module")?;
        parse_elm_whitespace_and_comments(state);
        let maybe_module_name_node: Option<ElmSyntaxNode<Box<str>>> =
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
            if parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
            maybe_subscription_entry =
                parse_elm_syntax_effect_module_header_where_entry(state, "subscription");
            parse_elm_whitespace_and_comments(state);
            let _: bool = parse_symbol(state, "}");
        } else {
            maybe_command_entry = None;
            maybe_subscription_entry = None;
        }

        parse_elm_whitespace_and_comments(state);
        let maybe_exposing_keyword_range: Option<lsp_types::Range> =
            parse_elm_keyword_as_range(state, "exposing");
        parse_elm_whitespace_and_comments(state);
        let maybe_exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>> =
            parse_elm_syntax_exposing_node(state);
        Some(ElmSyntaxModuleHeader {
            specific: ElmSyntaxModuleHeaderSpecific::Effect {
                effect_keyword_range: effect_keyword_range,
                module_keyword_range,
                where_keyword_range: where_keyword_range,
                command: maybe_command_entry,
                subscription: maybe_subscription_entry,
            },
            module_name: maybe_module_name_node,
            exposing_keyword_range: maybe_exposing_keyword_range,
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
    let type_name_node: ElmSyntaxNode<Box<str>> = parse_elm_uppercase_node(state)?;
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
            if state.position.character > u32::from(state.indent) {
                parse_elm_syntax_type_space_separated_node(state)
            } else {
                None
            };
        Some(ElmSyntaxNode {
            range: lsp_types::Range {
                start: start_type_node.range.start,
                end: match &maybe_output_type {
                    None => arrow_key_symbol_range.end,
                    Some(output_type_node) => output_type_node.range.end,
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
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    parse_elm_syntax_type_construct_node(state).or_else(|| {
        let start_position: lsp_types::Position = state.position;
        parse_symbol_as(state, "()", ElmSyntaxType::Unit)
            .or_else(|| parse_elm_lowercase_as_box_str(state).map(ElmSyntaxType::Variable))
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
    let start_position: lsp_types::Position = state.position;
    parse_elm_syntax_type_not_space_separated(state).map(|type_| ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_position,
            end: state.position,
        },
        value: type_,
    })
}
fn parse_elm_syntax_type_not_space_separated(state: &mut ParseState) -> Option<ElmSyntaxType> {
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    parse_symbol_as(state, "()", ElmSyntaxType::Unit)
        .or_else(|| parse_elm_lowercase_as_box_str(state).map(ElmSyntaxType::Variable))
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
    while parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_start_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_lowercase_as_node(state);
    parse_elm_whitespace_and_comments(state);
    if let Some(bar_key_symbol_range) = parse_symbol_as_range(state, "|") {
        parse_elm_whitespace_and_comments(state);
        let mut fields: Vec<ElmSyntaxTypeField> = Vec::new();
        while let Some(field) = parse_elm_syntax_type_field(state) {
            fields.push(field);
            parse_elm_whitespace_and_comments(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
        }
        let _: bool = parse_symbol(state, "}");
        Some(ElmSyntaxType::RecordExtension {
            record_variable: maybe_start_name,
            bar_key_symbol_range: bar_key_symbol_range,
            fields: fields,
        })
    } else if let Some(field0_name_node) = maybe_start_name {
        let maybe_field0_colon_key_symbol_range: Option<lsp_types::Range> =
            parse_symbol_as_range(state, ":");
        parse_elm_whitespace_and_comments(state);
        let maybe_field0_value: Option<ElmSyntaxNode<ElmSyntaxType>> =
            parse_elm_syntax_type_space_separated_node(state);
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
        let mut fields: Vec<ElmSyntaxTypeField> = vec![ElmSyntaxTypeField {
            name: field0_name_node,
            colon_key_symbol_range: maybe_field0_colon_key_symbol_range,
            value: maybe_field0_value,
        }];
        while let Some(field) = parse_elm_syntax_type_field(state) {
            fields.push(field);
            parse_elm_whitespace_and_comments(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
        }
        let _: bool = parse_symbol(state, "}");
        Some(ElmSyntaxType::Record(fields))
    } else {
        let _: bool = parse_symbol(state, "}");
        Some(ElmSyntaxType::Record(vec![]))
    }
}
fn parse_elm_syntax_type_field(state: &mut ParseState) -> Option<ElmSyntaxTypeField> {
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    let maybe_name: ElmSyntaxNode<Box<str>> = parse_elm_lowercase_as_node(state)?;
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
    parse_elm_whitespace_and_comments(state);
    let mut arguments: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
    let mut construct_end_position: lsp_types::Position = reference_node.range.end;
    while let Some(argument_node) = parse_elm_syntax_type_not_space_separated_node(state) {
        construct_end_position = argument_node.range.end;
        arguments.push(argument_node);
        parse_elm_whitespace_and_comments(state);
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
                            qualification: Box::from(
                                &state.source[start_offset_utf8..(after_last_dot_offset_utf8 - 1)],
                            ),
                            name: Box::from(
                                &state.source[after_last_dot_offset_utf8..state.offset_utf8],
                            ),
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
                        qualification: Box::from(
                            &state.source[start_offset_utf8..(state.offset_utf8 - 1)],
                        ),
                        name: Box::from(""),
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
                qualification: Box::from(""),
                name: Box::from(&state.source[start_offset_utf8..state.offset_utf8]),
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
    if parse_symbol(state, ")") {
        Some(match maybe_in_parens_0 {
            None => ElmSyntaxType::Unit,
            Some(in_parens) => ElmSyntaxType::Parenthesized(elm_syntax_node_box(in_parens)),
        })
    } else {
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
        let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxType>> =
            parse_elm_syntax_type_space_separated_node(state);
        parse_elm_whitespace_and_comments(state);
        if parse_symbol(state, ")") {
            Some(ElmSyntaxType::Tuple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
            })
        } else {
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
            let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxType>> =
                parse_elm_syntax_type_space_separated_node(state);
            parse_elm_whitespace_and_comments(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
            let _: bool = parse_symbol(state, ")");
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
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    parse_elm_syntax_pattern_space_separated_node_starting_at_any_indent(state)
}
fn parse_elm_syntax_pattern_space_separated_node_starting_at_any_indent(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    let start_pattern: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_not_as_or_cons_node(state)?;
    parse_elm_whitespace_and_comments(state);
    let mut consed_left_to_right: Vec<(lsp_types::Range, Option<ElmSyntaxNode<ElmSyntaxPattern>>)> =
        Vec::new();
    while let Some(cons_key_symbol_range) = parse_symbol_as_range(state, "::") {
        parse_elm_whitespace_and_comments(state);
        let maybe_tail_pattern: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
            if state.position.character <= u32::from(state.indent) {
                None
            } else {
                parse_elm_syntax_pattern_not_as_or_cons_node(state)
            };
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
                        start: match &maybe_head {
                            Some(head_node) => head_node.range.start,
                            None => cons_tail_key_symbol_range.start,
                        },
                        end: match &maybe_tail {
                            Some(tail_node) => tail_node.range.end,
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
                end: match &maybe_tail {
                    Some(tail_node) => tail_node.range.end,
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
            let maybe_variable: Option<ElmSyntaxNode<Box<str>>> =
                parse_elm_lowercase_as_node(state);
            Some(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: pattern_with_conses.range.start,
                    end: match &maybe_variable {
                        Some(variable_node) => variable_node.range.end,
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
            .or_else(|| parse_elm_lowercase_as_box_str(state).map(ElmSyntaxPattern::Variable))
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
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    parse_symbol_as(state, "()", ElmSyntaxPattern::Unit)
        .or_else(|| parse_symbol_as(state, "_", ElmSyntaxPattern::Ignored))
        .or_else(|| parse_elm_syntax_pattern_parenthesized_or_tuple_or_triple(state))
        .or_else(|| parse_elm_lowercase_as_box_str(state).map(ElmSyntaxPattern::Variable))
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
    while parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments(state);
    }
    let mut elements: Vec<ElmSyntaxNode<ElmSyntaxPattern>> = Vec::new();
    while let Some(pattern_node) = parse_elm_syntax_pattern_space_separated_node(state) {
        elements.push(pattern_node);
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
    }
    let _: bool = parse_symbol(state, "]");
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
    while parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments(state);
    }
    let mut field_names: Vec<ElmSyntaxNode<Box<str>>> = Vec::new();
    while let Some(field_name_node) = if state.position.character <= u32::from(state.indent) {
        None
    } else {
        parse_elm_lowercase_as_node(state)
    } {
        field_names.push(field_name_node);
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
    }
    let _: bool = parse_symbol(state, "}");
    Some(ElmSyntaxPattern::Record(field_names))
}
fn parse_elm_syntax_pattern_not_space_separated_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxPattern>> {
    let start_position: lsp_types::Position = state.position;
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
    if parse_symbol(state, ")") {
        Some(match maybe_in_parens_0 {
            None => ElmSyntaxPattern::Unit,
            Some(in_parens) => ElmSyntaxPattern::Parenthesized(elm_syntax_node_box(in_parens)),
        })
    } else {
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
        let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
            parse_elm_syntax_pattern_space_separated_node(state);
        parse_elm_whitespace_and_comments(state);
        if parse_symbol(state, ")") {
            Some(ElmSyntaxPattern::Tuple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
            })
        } else {
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
            let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxPattern>> =
                parse_elm_syntax_pattern_space_separated_node(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
            let _: bool = parse_symbol(state, ")");
            Some(ElmSyntaxPattern::Triple {
                part0: maybe_in_parens_0.map(elm_syntax_node_box),
                part1: maybe_part1.map(elm_syntax_node_box),
                part2: maybe_part2.map(elm_syntax_node_box),
            })
        }
    }
}
fn parse_elm_unsigned_integer_base16_as_i64(
    state: &mut ParseState,
) -> Option<Result<i64, Box<str>>> {
    if !parse_symbol(state, "0x") {
        return None;
    }
    let hex_str: &str = parse_same_line_while_as_str(state, |c| c.is_ascii_hexdigit());
    Some(i64::from_str_radix(hex_str, 16).map_err(|_| Box::from(hex_str)))
}
fn parse_elm_unsigned_integer_base10_as_i64(
    state: &mut ParseState,
) -> Option<Result<i64, Box<str>>> {
    let start_offset_utf8: usize = state.offset_utf8;
    if parse_unsigned_integer_base10(state) {
        let decimal_str: &str = &state.source[start_offset_utf8..state.offset_utf8];
        Some(str::parse::<i64>(decimal_str).map_err(|_| Box::from(decimal_str)))
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
                str::parse::<f64>(&full_chomped_str.replace("+", ""))
                    .map_err(|_| Box::from(full_chomped_str))
            } else {
                str::parse::<f64>(full_chomped_str).map_err(|_| Box::from(full_chomped_str))
            },
        )
    } else {
        ElmSyntaxExpression::Integer {
            base: ElmSyntaxIntBase::IntBase10,
            value: str::parse::<i64>(full_chomped_str).map_err(|_| Box::from(full_chomped_str)),
        }
    })
}
fn parse_elm_char(state: &mut ParseState) -> Option<Option<char>> {
    if !parse_symbol(state, "'") {
        return None;
    }
    let result: Option<char> = parse_elm_text_content_char(state);
    let _: bool = parse_symbol(state, "'");
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
            let start_offset_utf8: usize = state.offset_utf8;
            let start_position: lsp_types::Position = state.position;
            let reset_parse_state = |progressed_state: &mut ParseState| {
                progressed_state.offset_utf8 = start_offset_utf8;
                progressed_state.position = start_position;
            };
            if !parse_symbol(state, "\\u{") {
                return None;
            }
            let unicode_hex_start_offset_utf8: usize = state.offset_utf8;
            parse_same_line_while(state, |c| c.is_ascii_hexdigit());
            let unicode_hex_str: &str =
                &state.source[unicode_hex_start_offset_utf8..state.offset_utf8];
            let _: bool = parse_symbol(state, "}");
            let Ok(first_utf16_code) = u16::from_str_radix(unicode_hex_str, 16) else {
                reset_parse_state(state);
                return None;
            };
            match char::from_u32(u32::from(first_utf16_code)) {
                Some(char) => Some(char),
                None => {
                    if !parse_symbol(state, "\\u{") {
                        reset_parse_state(state);
                        return None;
                    }
                    let second_unicode_hex_start_offset_utf8: usize = state.offset_utf8;
                    parse_same_line_while(state, |c| c.is_ascii_hexdigit());
                    let second_unicode_hex_str: &str =
                        &state.source[second_unicode_hex_start_offset_utf8..state.offset_utf8];
                    let _: bool = parse_symbol(state, "}");
                    let Ok(second_utf16_code) = u16::from_str_radix(second_unicode_hex_str, 16)
                    else {
                        reset_parse_state(state);
                        return None;
                    };
                    char::decode_utf16([first_utf16_code, second_utf16_code])
                        .find_map(Result::ok)
                        .or_else(|| {
                            reset_parse_state(state);
                            None
                        })
                }
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
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
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
                            end: match &maybe_right {
                                None => operator_node.range.end,
                                Some(right_node) => right_node.range.end,
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
        if (state.position.character > u32::from(state.indent))
            && let Some(argument0_node) =
                parse_elm_syntax_expression_not_space_separated_node(state)
        {
            let mut argument1_up: Vec<ElmSyntaxNode<ElmSyntaxExpression>> = Vec::new();
            let mut call_end_position: lsp_types::Position = argument0_node.range.end;
            'parsing_argument1_up: loop {
                parse_elm_whitespace_and_comments(state);
                if state.position.character <= u32::from(state.indent) {
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
        let maybe_field_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_lowercase_as_node(state);
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
        .nth(1)
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
    parse_elm_lowercase_as_box_str(state)
        .map(|name| ElmSyntaxExpression::Reference {
            qualification: Box::from(""),
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
                    if let Some(name) = parse_elm_lowercase_as_box_str(state) {
                        return Some(ElmSyntaxExpression::Reference {
                            qualification: Box::from(
                                &state.source[start_offset_utf8..(after_last_dot_offset_utf8 - 1)],
                            ),
                            name: name,
                        });
                    } else if parse_same_line_char_if(state, char::is_uppercase) {
                        parse_same_line_while(state, |c| c.is_alphanumeric() || c == '_');
                        if !parse_symbol(state, ".") {
                            return Some(ElmSyntaxExpression::Reference {
                                qualification: Box::from(
                                    &state.source
                                        [start_offset_utf8..(after_last_dot_offset_utf8 - 1)],
                                ),
                                name: Box::from(
                                    &state.source[after_last_dot_offset_utf8..state.offset_utf8],
                                ),
                            });
                        }
                    } else {
                        // stopping at . and in effect having an empty name is explicitly allowed!
                        return Some(ElmSyntaxExpression::Reference {
                            qualification: Box::from(
                                &state.source[start_offset_utf8..(state.offset_utf8 - 1)],
                            ),
                            name: Box::from(""),
                        });
                    }
                }
            } else {
                Some(ElmSyntaxExpression::Reference {
                    qualification: Box::from(""),
                    name: Box::from(&state.source[start_offset_utf8..state.offset_utf8]),
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
    while parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_start_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_lowercase_as_node(state);
    parse_elm_whitespace_and_comments(state);
    if let Some(bar_key_symbol_range) = parse_symbol_as_range(state, "|") {
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
        let mut fields: Vec<ElmSyntaxExpressionField> = Vec::new();
        while let Some(field) = parse_elm_syntax_expression_field(state) {
            fields.push(field);
            parse_elm_whitespace_and_comments(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
        }
        let _: bool = parse_symbol(state, "}");
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
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
        let mut fields: Vec<ElmSyntaxExpressionField> = vec![ElmSyntaxExpressionField {
            name: field0_name_node,
            equals_key_symbol_range: maybe_field0_equals_key_symbol_range,
            value: maybe_field0_value,
        }];
        while let Some(field) = parse_elm_syntax_expression_field(state) {
            fields.push(field);
            parse_elm_whitespace_and_comments(state);
            while parse_symbol(state, ",") {
                parse_elm_whitespace_and_comments(state);
            }
        }
        let _: bool = parse_symbol(state, "}");
        Some(ElmSyntaxExpression::Record(fields))
    } else {
        let _: bool = parse_symbol(state, "}");
        Some(ElmSyntaxExpression::Record(vec![]))
    }
}
fn parse_elm_syntax_expression_field(state: &mut ParseState) -> Option<ElmSyntaxExpressionField> {
    if state.position.character <= u32::from(state.indent) {
        return None;
    }
    let name_node: ElmSyntaxNode<Box<str>> = parse_elm_lowercase_as_node(state)?;
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
    let if_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "if")?;
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
                        end: match &maybe_on_false {
                            None => else_keyword_range.end,
                            Some(on_false_node) => on_false_node.range.end,
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
                        end: match &maybe_on_true {
                            None => then_keyword_range.end,
                            Some(on_true_node) => on_true_node.range.end,
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
                    end: match &maybe_condition {
                        None => if_keyword_range.end,
                        Some(condition_node) => condition_node.range.end,
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
        // be lenient in allowing , after lambda parameters, even though it's invalid syntax
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
    }
    let maybe_arrow_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "->");
    parse_elm_whitespace_and_comments(state);
    let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        if state.position.character > u32::from(state.indent) {
            parse_elm_syntax_expression_space_separated_node(state)
        } else {
            None
        };
    Some(ElmSyntaxNode {
        range: lsp_types::Range {
            start: backslash_key_symbol_range.start,
            end: match &maybe_result {
                None => syntax_before_result_end_position,
                Some(result_node) => result_node.range.end,
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
    let case_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "case")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_matched: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
        parse_elm_syntax_expression_space_separated_node(state);
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, "of") {
        None => ElmSyntaxNode {
            range: lsp_types::Range {
                start: case_keyword_range.start,
                end: match &maybe_matched {
                    None => case_keyword_range.end,
                    Some(matched_node) => matched_node.range.end,
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
            if state.position.character <= u32::from(state.indent) {
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
                        .unwrap_or(case.pattern.range.end);
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
    if state.position.character < u32::from(state.indent) {
        return None;
    }
    let case_pattern_node: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_space_separated_node_starting_at_any_indent(state)?;
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
    let let_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "let")?;
    parse_elm_whitespace_and_comments(state);
    Some(if state.position.character <= u32::from(state.indent) {
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
        let maybe_in_keyword_range: Option<lsp_types::Range> = 'parsing_declarations: loop {
            if let Some(in_keyword_range) = parse_elm_keyword_as_range(state, "in") {
                break 'parsing_declarations Some(in_keyword_range);
            }
            match parse_elm_syntax_let_declaration(state) {
                None => {
                    break 'parsing_declarations None;
                }
                Some(declaration_node) => {
                    syntax_before_in_key_symbol_end_position = declaration_node.range.end;
                    declarations.push(declaration_node);
                    parse_elm_whitespace_and_comments(state);
                }
            }
        };
        parse_state_pop_indent(state);
        parse_elm_whitespace_and_comments(state);
        let maybe_result: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
            parse_elm_syntax_expression_space_separated_node(state);
        ElmSyntaxNode {
            range: lsp_types::Range {
                start: let_keyword_range.start,
                end: match &maybe_result {
                    None => maybe_in_keyword_range
                        .map(|range| range.end)
                        .unwrap_or(syntax_before_in_key_symbol_end_position),
                    Some(result_node) => result_node.range.end,
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
    if state.position.character < u32::from(state.indent) {
        return None;
    }
    parse_elm_syntax_let_variable_declaration_node(state)
        .or_else(|| parse_elm_syntax_let_destructuring_node(state))
}
fn parse_elm_syntax_let_destructuring_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxLetDeclaration>> {
    let pattern_node: ElmSyntaxNode<ElmSyntaxPattern> =
        parse_elm_syntax_pattern_space_separated_node_starting_at_any_indent(state)?;
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
                    end: match &maybe_expression {
                        None => equals_key_symbol_range.end,
                        Some(expression_node) => expression_node.range.end,
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
fn parse_elm_syntax_let_variable_declaration_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxLetDeclaration>> {
    let start_name_node: ElmSyntaxNode<Box<str>> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, ":") {
        None => parse_elm_syntax_let_variable_declaration_node_after_maybe_signature_and_name(
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
                None => ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: start_name_node.range.start,
                        end: maybe_type
                            .as_ref()
                            .map(|node| node.range.end)
                            .unwrap_or_else(|| colon_key_symbol_range.end),
                    },
                    value: ElmSyntaxLetDeclaration::VariableDeclaration {
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
                },
                Some(implementation_name_range) => {
                    parse_elm_whitespace_and_comments(state);
                    parse_elm_syntax_let_variable_declaration_node_after_maybe_signature_and_name(
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
    })
}
fn parse_elm_syntax_let_variable_declaration_node_after_maybe_signature_and_name(
    state: &mut ParseState,
    start_name_node: ElmSyntaxNode<Box<str>>,
    maybe_signature: Option<ElmSyntaxVariableDeclarationSignature>,
) -> ElmSyntaxNode<ElmSyntaxLetDeclaration> {
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position =
        match &maybe_signature {
            Some(signature) => signature
                .implementation_name_range
                .map(|range| range.end)
                .or_else(|| signature.type_.as_ref().map(|node| node.range.end))
                .unwrap_or(signature.colon_key_symbol_range.end),
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
        if state.position.character <= u32::from(state.indent) {
            None
        } else {
            parse_elm_syntax_expression_space_separated_node(state)
        };
    ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_name_node.range.start,
            end: maybe_result
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or(syntax_before_equals_key_symbol_end_location),
        },
        value: ElmSyntaxLetDeclaration::VariableDeclaration {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters: parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        },
    }
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
    while parse_symbol(state, ",") {
        parse_elm_whitespace_and_comments(state);
    }
    let mut elements: Vec<ElmSyntaxNode<ElmSyntaxExpression>> = Vec::new();
    while let Some(expression_node) = parse_elm_syntax_expression_space_separated_node(state) {
        elements.push(expression_node);
        parse_elm_whitespace_and_comments(state);
        while parse_symbol(state, ",") {
            parse_elm_whitespace_and_comments(state);
        }
    }
    let _: bool = parse_symbol(state, "]");
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
            if parse_symbol(state, ")") {
                match maybe_in_parens_0 {
                    None => ElmSyntaxExpression::Unit,
                    Some(in_parens) => {
                        ElmSyntaxExpression::Parenthesized(elm_syntax_node_box(in_parens))
                    }
                }
            } else {
                while parse_symbol(state, ",") {
                    parse_elm_whitespace_and_comments(state);
                }
                let maybe_part1: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                    parse_elm_syntax_expression_space_separated_node(state);
                parse_elm_whitespace_and_comments(state);
                if parse_symbol(state, ")") {
                    ElmSyntaxExpression::Tuple {
                        part0: maybe_in_parens_0.map(elm_syntax_node_box),
                        part1: maybe_part1.map(elm_syntax_node_box),
                    }
                } else {
                    while parse_symbol(state, ",") {
                        parse_elm_whitespace_and_comments(state);
                    }
                    let maybe_part2: Option<ElmSyntaxNode<ElmSyntaxExpression>> =
                        parse_elm_syntax_expression_space_separated_node(state);
                    while parse_symbol(state, ",") {
                        parse_elm_whitespace_and_comments(state);
                    }
                    let _: bool = parse_symbol(state, ")");
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
        .or_else(|| {
            if state.indent != 0 {
                return None;
            }
            parse_elm_syntax_declaration_variable_node(state)
        })
}
fn parse_elm_syntax_declaration_port_node(
    state: &mut ParseState,
) -> Option<ElmSyntaxNode<ElmSyntaxDeclaration>> {
    let port_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "port")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_lowercase_as_node(state);
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
        .unwrap_or(port_keyword_range.end);
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
    let infix_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "infix")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_direction: Option<ElmSyntaxNode<ElmSyntaxInfixDirection>> =
        parse_elm_syntax_infix_declaration_node(state);
    parse_elm_whitespace_and_comments(state);
    let precedence_start_position: lsp_types::Position = state.position;
    let maybe_precedence: Option<ElmSyntaxNode<i64>> =
        match parse_elm_unsigned_integer_base10_as_i64(state).and_then(Result::ok) {
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
    let maybe_operator_symbol: Option<ElmSyntaxNode<&str>> = parse_elm_operator_node(state);
    parse_elm_whitespace_and_comments(state);
    let _: bool = parse_symbol(state, ")");
    parse_elm_whitespace_and_comments(state);
    let maybe_equals_key_symbol_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "=");
    parse_elm_whitespace_and_comments(state);
    let maybe_function: Option<ElmSyntaxNode<Box<str>>> = parse_elm_lowercase_as_node(state);
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
    let type_keyword_range: lsp_types::Range = parse_elm_keyword_as_range(state, "type")?;
    parse_elm_whitespace_and_comments(state);
    let maybe_alias_keyword_range: Option<lsp_types::Range> = parse_symbol_as_range(state, "alias");
    parse_elm_whitespace_and_comments(state);
    let maybe_name_node: Option<ElmSyntaxNode<Box<str>>> = parse_elm_uppercase_node(state);
    parse_elm_whitespace_and_comments(state);
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position = maybe_name_node
        .as_ref()
        .map(|name_node| name_node.range.end)
        .or_else(|| maybe_alias_keyword_range.map(|range| range.end))
        .unwrap_or(type_keyword_range.end);
    let mut parameters: Vec<ElmSyntaxNode<Box<str>>> = Vec::new();
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
                if state.position.character <= u32::from(state.indent) {
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
            let maybe_variant0_name: Option<ElmSyntaxNode<Box<str>>> =
                parse_elm_uppercase_node(state);
            parse_elm_whitespace_and_comments(state);
            let mut variant0_values: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
            let mut full_end_position: lsp_types::Position = maybe_variant0_name
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or(syntax_before_equals_key_symbol_end_location);
            while let Some(value_node) = parse_elm_syntax_type_not_space_separated_node(state) {
                full_end_position = value_node.range.end;
                variant0_values.push(value_node);
                parse_elm_whitespace_and_comments(state);
            }
            parse_elm_whitespace_and_comments(state);
            let mut variant1_up: Vec<ElmSyntaxChoiceTypeDeclarationTailingVariant> = Vec::new();
            while let Some(variant_node) =
                parse_elm_syntax_choice_type_declaration_trailing_variant_node(state)
            {
                variant1_up.push(variant_node.value);
                full_end_position = variant_node.range.end;
                parse_elm_whitespace_and_comments(state);
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
    while parse_symbol(state, "|") {
        parse_elm_whitespace_and_comments(state);
    }
    let maybe_name: Option<ElmSyntaxNode<Box<str>>> = parse_elm_uppercase_node(state);
    parse_elm_whitespace_and_comments(state);
    let mut values: Vec<ElmSyntaxNode<ElmSyntaxType>> = Vec::new();
    let mut full_end_position: lsp_types::Position = maybe_name
        .as_ref()
        .map(|node| node.range.end)
        .unwrap_or_else(|| or_key_symbol_range.end);
    while let Some(value_node) = parse_elm_syntax_type_not_space_separated_node(state) {
        full_end_position = value_node.range.end;
        values.push(value_node);
        parse_elm_whitespace_and_comments(state);
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
    let start_name_node: ElmSyntaxNode<Box<str>> = parse_elm_lowercase_as_node(state)?;
    parse_elm_whitespace_and_comments(state);
    Some(match parse_symbol_as_range(state, ":") {
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
                None => ElmSyntaxNode {
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
                },
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
    })
}
fn parse_elm_syntax_declaration_variable_node_after_maybe_signature_and_name(
    state: &mut ParseState,
    start_name_node: ElmSyntaxNode<Box<str>>,
    maybe_signature: Option<ElmSyntaxVariableDeclarationSignature>,
) -> ElmSyntaxNode<ElmSyntaxDeclaration> {
    let mut syntax_before_equals_key_symbol_end_location: lsp_types::Position =
        match &maybe_signature {
            Some(signature) => signature
                .implementation_name_range
                .map(|range| range.end)
                .or_else(|| signature.type_.as_ref().map(|node| node.range.end))
                .unwrap_or(signature.colon_key_symbol_range.end),
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
        if state.position.character <= u32::from(state.indent) {
            None
        } else {
            parse_elm_syntax_expression_space_separated_node(state)
        };
    ElmSyntaxNode {
        range: lsp_types::Range {
            start: start_name_node.range.start,
            end: maybe_result
                .as_ref()
                .map(|node| node.range.end)
                .or_else(|| maybe_equals_key_symbol_range.map(|range| range.end))
                .unwrap_or(syntax_before_equals_key_symbol_end_location),
        },
        value: ElmSyntaxDeclaration::Variable {
            start_name: start_name_node,
            signature: maybe_signature,
            parameters: parameters,
            equals_key_symbol_range: maybe_equals_key_symbol_range,
            result: maybe_result,
        },
    }
}
fn parse_elm_syntax_documented_declaration_followed_by_whitespace_and_comments_and_whatever_indented(
    state: &mut ParseState,
) -> Option<ElmSyntaxDocumentedDeclaration> {
    match parse_elm_documentation_comment_block_node(state) {
        None => parse_elm_syntax_declaration_node(state).map(|declaration_node| {
            parse_elm_whitespace_and_comments(state);
            ElmSyntaxDocumentedDeclaration {
                documentation: None,
                declaration: Some(declaration_node),
            }
        }),
        Some(documentation_node) => {
            parse_elm_whitespace_and_comments(state);
            let maybe_declaration: Option<ElmSyntaxNode<ElmSyntaxDeclaration>> =
                parse_elm_syntax_declaration_node(state);
            parse_elm_whitespace_and_comments(state);
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
    let maybe_module_documentation: Option<
        ElmSyntaxNode<Vec<ElmSyntaxNode<ElmSyntaxModuleDocumentationElement>>>,
    > = parse_elm_syntax_module_documentation_node(&mut state);
    parse_elm_whitespace_and_comments(&mut state);
    let mut imports: Vec<ElmSyntaxNode<ElmSyntaxImport>> = Vec::new();
    while let Some(import_node) = parse_elm_syntax_import_node(&mut state) {
        imports.push(import_node);
        parse_elm_whitespace_and_comments(&mut state);
    }
    let mut last_valid_end_offet_utf8: usize = state.offset_utf8;
    let mut last_parsed_was_valid: bool = true;
    let mut declarations: Vec<Result<ElmSyntaxDocumentedDeclaration, Box<str>>> =
        Vec::with_capacity(8);
    'parsing_delarations: loop {
        let offset_utf8_before_parsing_documeted_declaration: usize = state.offset_utf8;
        match parse_elm_syntax_documented_declaration_followed_by_whitespace_and_comments_and_whatever_indented(&mut state) {
            Some(documented_declaration) => {
                if !last_parsed_was_valid {
                    declarations.push(Err(Box::from(&module_source[last_valid_end_offet_utf8..offset_utf8_before_parsing_documeted_declaration])));
                }
                last_parsed_was_valid = true;
                declarations.push(Ok(documented_declaration));
                parse_elm_whitespace_and_comments(&mut state);
                last_valid_end_offet_utf8 = state.offset_utf8;
            }
            None => {
                parse_before_next_linebreak(&mut state);
                if parse_linebreak(&mut state) {
                    last_parsed_was_valid = false;
                } else {
                    break 'parsing_delarations;
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

fn string_replace_lsp_range(
    string: &mut String,
    range: lsp_types::Range,
    range_length: usize,
    replacement: &str,
) {
    let start_line_offset: usize =
        str_offset_after_n_lsp_linebreaks(string, range.start.line as usize);
    let start_offset: usize = start_line_offset
        + str_starting_utf8_length_for_utf16_length(
            &string[start_line_offset..],
            range.start.character as usize,
        );
    let range_length_utf8: usize =
        str_starting_utf8_length_for_utf16_length(&string[start_offset..], range_length);
    string.replace_range(
        start_offset..(start_offset + range_length_utf8),
        replacement,
    );
}
fn str_offset_after_n_lsp_linebreaks(str: &str, linebreak_count_to_skip: usize) -> usize {
    if linebreak_count_to_skip == 0 {
        return 0;
    }
    let mut offset_after_n_linebreaks: usize = 0;
    let mut encountered_linebreaks: usize = 0;
    'finding_after_n_linebreaks_offset: loop {
        if str[offset_after_n_linebreaks..].starts_with("\r\n") {
            encountered_linebreaks += 1;
            offset_after_n_linebreaks += 2;
            if encountered_linebreaks >= linebreak_count_to_skip {
                break 'finding_after_n_linebreaks_offset;
            }
        } else {
            match str[offset_after_n_linebreaks..].chars().next() {
                None => {
                    break 'finding_after_n_linebreaks_offset;
                }
                // see EOL in https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocuments
                Some('\r' | '\n') => {
                    encountered_linebreaks += 1;
                    offset_after_n_linebreaks += 1;
                    if encountered_linebreaks >= linebreak_count_to_skip {
                        break 'finding_after_n_linebreaks_offset;
                    }
                }
                Some(next_char) => {
                    offset_after_n_linebreaks += next_char.len_utf8();
                }
            }
        }
    }
    offset_after_n_linebreaks
}
fn str_starting_utf8_length_for_utf16_length(slice: &str, starting_utf16_length: usize) -> usize {
    let mut utf8_length: usize = 0;
    let mut so_far_length_utf16: usize = 0;
    'traversing_utf16_length: for char in slice.chars() {
        if so_far_length_utf16 >= starting_utf16_length {
            break 'traversing_utf16_length;
        }
        utf8_length += char.len_utf8();
        so_far_length_utf16 += char.len_utf16();
    }
    utf8_length
}
