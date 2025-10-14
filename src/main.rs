#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]

mod elm;

struct State {
    // Currently parsed elm modules and other elm-derived state types are stored using
    // static references. In regular rust code, this is an eventual memory leak. So
    // instead this language server tries to find good points in time to
    // **re-calculate all state** and then throw away all the garbage that was put
    // into the allocator since last cleanup. The effect to the user is very similar
    // to stop-the-world garbage collection (increasing memory usage followed by a
    // major GC pause, repeat) but potentially worse because more computationally
    // expensive (if the state is heavy which ours isn't!). The benefit however is
    // that de-allocating and allocating is very fast. I haven't looked into whether
    // this approach actually has a name; in the meantime I'll call it
    // "clear-reconstruct" allocator
    clear_reconstruct_allocator: &'static bumpalo::Bump,
    last_allocator_clear_time: std::time::Instant,
    elm_jsons_source_directories: std::collections::HashMap<std::path::PathBuf, Vec<std::path::PathBuf>>,
    parsed_modules: std::collections::HashMap<std::path::PathBuf, ModuleState>,
}

#[derive(Clone)]
struct ModuleState {
    source: &'static str,
    syntax: Option<elm::ElmSyntaxModule<'static>>,
}

const token_types: [lsp_types::SemanticTokenType; 11] =
    [
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
    token_types.iter().enumerate().find_map(|(i, token)| if *token == semantic_token {
        Some(i as u32)
    } else {
        None
    }).unwrap_or(0_u32)
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _client_socket) = async_lsp::MainLoop::new_server(|client| {
        let mut router: async_lsp::router::Router<State> = async_lsp::router::Router::new(State {
            clear_reconstruct_allocator: std::boxed::Box::leak(std::boxed::Box::new(bumpalo::Bump::new())),
            last_allocator_clear_time: std::time::Instant::now(),
            elm_jsons_source_directories: std::collections::HashMap::new(),
            parsed_modules: std::collections::HashMap::new(),
        });
        router.request::<lsp_types::request::Initialize, _>(|state, initialize_arguments| {
            let home_directory_path = std::env::home_dir();
            let home_directory_path_str = match home_directory_path {
                Some(ref path) => Some(path.to_string_lossy()),
                None => None,
            };
            let workspace_folder_paths =
                initialize_arguments
                    .workspace_folders
                    .iter()
                    .flatten()
                    .filter_map(|workspace_folder| workspace_folder.uri.to_file_path().ok());
            for workspace_folder_path in workspace_folder_paths {
                match std::fs::read_to_string(std::path::Path::join(&workspace_folder_path, "elm.json")) {
                    Err(_) => { },
                    Ok(elm_json_source) => {
                        let allocator: bumpalo::Bump = bumpalo::Bump::new();
                        let source_directories_to_read_or_error =
                            elm::exports_elm_json_to_project_and_dependency_source_directories(
                                &allocator,
                                elm::GeneratedHomeDirectory { home_directory: match &home_directory_path_str {
                                    Some(path_str) => {
                                        Some(elm::StringString::One(&path_str))
                                    },
                                    None => None,
                                } },
                                elm::string_to_rope(&allocator, elm_json_source),
                            );
                        let workspace_source_directories = match source_directories_to_read_or_error {
                            Err(error) => {
                                eprintln!(
                                    "{error}. Assuming your elm files live at the top level or in any sub-directory"
                                );
                                vec![std::path::Path::join(&workspace_folder_path, "src")]
                            },
                            Ok(source_directories_to_read) => {
                                source_directories_to_read.ref_iter().map(|elm_string| {
                                    std::path::Path::join(&workspace_folder_path, elm_string.to_string())
                                }).collect::<Vec<std::path::PathBuf>>()
                            },
                        };
                        let elm_source_files = workspace_source_directories.iter().filter_map(|workspace_folder_path| {
                            list_elm_files_in_source_directory_at_path(workspace_folder_path).ok()
                        }).flatten();
                        for (file_uri, file_content) in elm_source_files {
                            let file_content_str: &str = state.clear_reconstruct_allocator.alloc(file_content);
                            state.parsed_modules.insert(file_uri, ModuleState {
                                source: file_content_str,
                                syntax: None,
                            });
                        }
                        state
                            .elm_jsons_source_directories
                            .insert(workspace_folder_path, workspace_source_directories);
                    },
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
                                    work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                                        work_done_progress: None,
                                    },
                                    legend: lsp_types::SemanticTokensLegend {
                                        token_modifiers: Vec::new(),
                                        token_types: Vec::from(token_types),
                                    },
                                    range: None,
                                    full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                                },
                            ),
                        ),
                        text_document_sync: Some(
                            lsp_types::TextDocumentSyncCapability::Kind(lsp_types::TextDocumentSyncKind::FULL),
                        ),
                        ..lsp_types::ServerCapabilities::default()
                    },
                    server_info: None,
                })
            }
        }).request::<lsp_types::request::HoverRequest, _>(|state, hover_arguments| {
            let maybe_module_syntax =
                hover_arguments
                    .text_document_position_params
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(|file_path| state.parsed_modules.get_mut(&file_path).and_then(|module_state| {
                        match module_state.syntax {
                            Some(module_syntax) => {
                                Some(module_syntax)
                            },
                            None => {
                                match elm::elm_parser_lenient_run(
                                    elm::elm_parser_lenient_module_(state.clear_reconstruct_allocator),
                                    elm::StringString::One(module_state.source),
                                ) {
                                    None => {
                                        None
                                    },
                                    Some(parsed_elm_module) => {
                                        module_state.syntax = Some(parsed_elm_module);
                                        Some(parsed_elm_module)
                                    },
                                }
                            },
                        }
                    }));
            let hover_location: elm::ElmSyntaxLocation =
                lsp_position_to_elm_syntax_location(hover_arguments.text_document_position_params.position);
            async move {
                match maybe_module_syntax {
                    None => Ok(None),
                    Some(module_syntax) => {
                        if elm::elm_syntax_range_includes_location(
                            hover_location,
                            elm::elm_syntax_node_range(module_syntax.module_definition),
                        ) {
                            Ok(Some(lsp_types::Hover {
                                contents: lsp_types::HoverContents::Scalar(
                                    lsp_types::MarkedString::String(
                                        format!(
                                            "module info: {:?}",
                                            elm::elm_syntax_node_value(module_syntax.module_definition)
                                        ),
                                    ),
                                ),
                                range: None,
                            }))
                        } else {
                            Ok(Some(lsp_types::Hover {
                                contents: lsp_types::HoverContents::Scalar(
                                    lsp_types::MarkedString::String(
                                        format!(
                                            "I am a hover text at offset {}:{}",
                                            hover_arguments.text_document_position_params.position.line,
                                            hover_arguments.text_document_position_params.position.character
                                        ),
                                    ),
                                ),
                                range: None,
                            }))
                        }
                    },
                }
            }
        }).request::<lsp_types::request::GotoDefinition, _>(|_state, goto_definition_arguments| async move {
            Ok(Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                uri: goto_definition_arguments.text_document_position_params.text_document.uri,
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: lsp_types::Position {
                        line: 0,
                        character: 6,
                    },
                },
            })))
        }).request::<lsp_types::request::SemanticTokensFullRequest, _>(|state, semantic_tokens_arguments| {
            let maybe_module_syntax =
                semantic_tokens_arguments
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(
                        |module_to_highlight_file_path| state
                            .parsed_modules
                            .get_mut(&module_to_highlight_file_path)
                            .and_then(|module_state| {
                                match module_state.syntax {
                                    Some(module_syntax) => {
                                        Some(module_syntax)
                                    },
                                    None => {
                                        match elm::elm_parser_lenient_run(
                                            elm::elm_parser_lenient_module_(state.clear_reconstruct_allocator),
                                            elm::StringString::One(module_state.source),
                                        ) {
                                            None => {
                                                None
                                            },
                                            Some(parsed_elm_module) => {
                                                module_state.syntax = Some(parsed_elm_module);
                                                Some(parsed_elm_module)
                                            },
                                        }
                                    },
                                }
                            }),
                    );
            let semantic_tokens = maybe_module_syntax.and_then(|module_syntax| {
                let highlight_allocator = bumpalo::Bump::new();
                Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
                    result_id: None,
                    data: elm::elm_syntax_highlight_for(&highlight_allocator, module_syntax)
                        .into_iter()
                        .scan(elm::ElmSyntaxLocation {
                            line: 1,
                            column: 1,
                        }, |previous_start_location, segment| {
                            let delta = elm::exports_location_delta(*previous_start_location, segment.range.start);
                            let token = lsp_types::SemanticToken {
                                delta_line: delta.line as u32,
                                delta_start: delta.column as u32,
                                length: (segment.range.end.column - segment.range.start.column) as u32,
                                token_type: semantic_token_type_to_id(
                                    elm_syntax_highlight_syntax_kind_to_lsp_semantic_token_type(segment.syntax_kind),
                                ),
                                token_modifiers_bitset: 0_u32,
                            };
                            segment.range.start.clone_into(previous_start_location);
                            Some(token)
                        })
                        .collect::<Vec<lsp_types::SemanticToken>>(),
                }))
            });
            async move {
                Ok(semantic_tokens)
            }
        }).notification::<lsp_types::notification::Initialized>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidOpenTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidChangeTextDocument>(|state, did_change_text_document| {
            let changed_file_paths = did_change_text_document.text_document.uri.to_file_path().ok();
            if let Some(changed_file_path) = changed_file_paths {
                match did_change_text_document.content_changes.into_iter().find_map(|change| {
                    match (change.range, change.range_length) {
                        // marks full new document content
                        (None, None) => Some(change.text),
                        (Some(_), _) | (_, Some(_)) => None,
                    }
                }) {
                    None => {
                        // bug in client. full document should be sent
                    },
                    Some(file_content) => {
                        let file_content_str: &str = state.clear_reconstruct_allocator.alloc(file_content);
                        state.parsed_modules.insert(changed_file_path, ModuleState {
                            source: file_content_str,
                            syntax: None,
                        });
                    },
                }
            }
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidChangeWatchedFiles>(|state, did_change_watched_files| {
            let changed_file_paths =
                did_change_watched_files
                    .changes
                    .into_iter()
                    .filter_map(|file_event| file_event.uri.to_file_path().ok());
            for changed_file_path in changed_file_paths {
                match std::fs::read_to_string(&changed_file_path) {
                    Err(_) => { },
                    Ok(file_content) => {
                        let file_content_str: &str = state.clear_reconstruct_allocator.alloc(file_content);
                        state.parsed_modules.insert(changed_file_path, ModuleState {
                            source: file_content_str,
                            syntax: None,
                        });
                    },
                }
            }

            // regularly clear the allocator and re-allocate sources and re-populate module
            // states. I'm not sure this is generally safe given async is involved but I
            // believe it should be for this language server since _.notification specifically
            // is synchronous and (so far) the rest of the code only uses async with immediate
            // returns.
            if std::time::Instant::now().duration_since(state.last_allocator_clear_time) >= 
                // TODO increase duration once parsing and document sync is more incremental and
                // therefore memory garbage is lower
                std::time::Duration::from_secs(30) {
                let modules_to_reparse = state.parsed_modules.iter().map(|(file_path, module_state)| {
                    (file_path, module_state.source.to_string())
                });

                // no need to state.parsed_modules.clear(); because all entries will be overridden
                // by the reparsed module state
                unsafe {
                    ref_to_mut(state.clear_reconstruct_allocator).reset();
                }
                let mut reparsed_modules = std::collections::HashMap::new();
                for (module_file_path, module_source_to_reparse) in modules_to_reparse {
                    let file_content_str: &str = state.clear_reconstruct_allocator.alloc(module_source_to_reparse);
                    reparsed_modules.insert(module_file_path.clone(), ModuleState {
                        source: file_content_str,
                        syntax: None,
                    });
                }
                state.parsed_modules = reparsed_modules;
                state.last_allocator_clear_time = std::time::Instant::now();
            }
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidSaveTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidCloseTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        });
        tower::ServiceBuilder::new()
            .layer(async_lsp::tracing::TracingLayer::default())
            .layer(async_lsp::server::LifecycleLayer::default())
            .layer(async_lsp::panic::CatchUnwindLayer::default())
            .layer(async_lsp::concurrency::ConcurrencyLayer::default())
            .layer(async_lsp::client_monitor::ClientProcessMonitorLayer::new(client))
            .service(router)
    });
    #[cfg(unix)]
    let (stdin, stdout) =
        (async_lsp::stdio::PipeStdin::lock_tokio().unwrap(), async_lsp::stdio::PipeStdout::lock_tokio().unwrap());
    #[cfg(not(unix))]
    let (stdin, stdout) =
        (
            tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
            tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
        );
    server.run_buffered(stdin, stdout).await.unwrap();
}

fn elm_syntax_highlight_syntax_kind_to_lsp_semantic_token_type(
    elm_syntax_highlight_syntax_kind: elm::ElmSyntaxHighlightSyntaxKind,
) -> lsp_types::SemanticTokenType {
    match elm_syntax_highlight_syntax_kind {
        elm::ElmSyntaxHighlightSyntaxKind::Flow => lsp_types::SemanticTokenType::KEYWORD,
        elm::ElmSyntaxHighlightSyntaxKind::KeySymbol => lsp_types::SemanticTokenType::KEYWORD,
        elm::ElmSyntaxHighlightSyntaxKind::Field => lsp_types::SemanticTokenType::PROPERTY,
        elm::ElmSyntaxHighlightSyntaxKind::ModuleNameOrAlias => lsp_types::SemanticTokenType::NAMESPACE,
        elm::ElmSyntaxHighlightSyntaxKind::Type => lsp_types::SemanticTokenType::TYPE,
        elm::ElmSyntaxHighlightSyntaxKind::Variable => lsp_types::SemanticTokenType::VARIABLE,
        elm::ElmSyntaxHighlightSyntaxKind::Variant => lsp_types::SemanticTokenType::ENUM_MEMBER,
        elm::ElmSyntaxHighlightSyntaxKind::VariableDeclaration => lsp_types::SemanticTokenType::FUNCTION,
        elm::ElmSyntaxHighlightSyntaxKind::Comment => lsp_types::SemanticTokenType::COMMENT,
        elm::ElmSyntaxHighlightSyntaxKind::Number => lsp_types::SemanticTokenType::NUMBER,
        elm::ElmSyntaxHighlightSyntaxKind::String => lsp_types::SemanticTokenType::STRING,
        elm::ElmSyntaxHighlightSyntaxKind::TypeVariable => lsp_types::SemanticTokenType::TYPE_PARAMETER,
    }
}

fn lsp_position_to_elm_syntax_location(lsp_position: lsp_types::Position) -> elm::ElmSyntaxLocation {
    elm::GeneratedColumnLine {
        line: (lsp_position.line + 1) as i64,
        column: (lsp_position.character + 1) as i64,
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
            Option::None => { },
            Option::Some(file_type) => {
                if (file_type == "elm") || (file_type == "elm-testing") {
                    let file_content: String = std::fs::read_to_string(&path)?;
                    so_far.push((path, file_content));
                }
            },
        }
    }
    Ok(())
}

unsafe fn ref_to_mut<T: ?Sized>(val: &T) -> &mut T {
    unsafe {
        (val as *const T as *mut T).as_mut().unwrap_unchecked()
    }
}
