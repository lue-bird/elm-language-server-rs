#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]

mod elm;

struct State {
    projects: std::collections::HashMap<
        /* path to directory containing elm.json */ std::path::PathBuf,
        ProjectState,
    >,
}

struct ProjectState {
    source_directories: Vec<std::path::PathBuf>,
    modules: std::collections::HashMap<std::path::PathBuf, ModuleState>,
    dependency_exposed_module_names: std::collections::HashMap<String, ProjectModuleOrigin>,
}
#[derive(Debug)]
struct ProjectModuleOrigin {
    project_path: std::path::PathBuf,
    module_path: std::path::PathBuf,
}
struct ModuleState {
    syntax: Option<ElmSyntaxModule>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _client_socket) = async_lsp::MainLoop::new_server(|client| {
        let mut router: async_lsp::router::Router<State> = async_lsp::router::Router::new(State {
            projects: std::collections::HashMap::new(),
        });
        router.request::<lsp_types::request::Initialize, _>(|state, initialize_arguments| {
            initialize_state_for_workspace_directories_into(state, initialize_arguments);
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
                                    full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                                },
                            ),
                        ),
                        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                            lsp_types::TextDocumentSyncKind::FULL,
                        )),
                        rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                            prepare_provider: Some(true),
                            work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                        })),
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
                    server_info: None,
                })
            }
        });
        router.request::<lsp_types::request::HoverRequest, _>(
            |state, hover_arguments: lsp_types::HoverParams| {
                let maybe_hover_result = respond_to_hover(state, hover_arguments);
                async move { Ok(maybe_hover_result) }
            },
        );
        router.request::<lsp_types::request::GotoDefinition, _>(
            |state, goto_definition_arguments: lsp_types::GotoDefinitionParams| {
                let response = respond_to_goto_definition(state, goto_definition_arguments);
                async move { Ok(response) }
            },
        );
        router.request::<lsp_types::request::PrepareRenameRequest, _>(
            |state, prepare_rename_arguments: lsp_types::TextDocumentPositionParams| {
                let prepared = respond_to_prepare_rename(state, prepare_rename_arguments);
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
                let maybe_rename_edits = respond_to_rename(state, rename_arguments);
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
                let semantic_tokens =
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
                                changed_file_path,
                                &changed_file_source,
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
                        Err(_) => {}
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
                                            changed_file_path,
                                            &changed_file_source,
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
        router.notification::<lsp_types::notification::DidSaveTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        });
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
    let allocator: bumpalo::Bump = bumpalo::Bump::new();
    let elm_json_path = std::path::Path::join(&project_path, "elm.json");
    let maybe_elm_json = std::fs::read_to_string(&elm_json_path)
        .ok()
        .and_then(|elm_json_source| {
            elm::json_decode_decode_string(
                &allocator,
                elm::elm_project_decoder(&allocator),
                elm::StringString::One(allocator.alloc(elm_json_source)),
            )
            .map_err(|json_decode_error| {
                let mut error_description = String::new();
                elm::json_decode_error_to_string_help(
                    &json_decode_error,
                    String::new(),
                    &mut error_description,
                    0,
                );
                eprintln!("I don't understand this elm.json: {}", error_description)
            })
            .ok()
        });
    if maybe_elm_json.is_none() {
        eprintln!(
            "no valid elm.json found. Now looking for elm module files across the workspace and elm/core 1.0.5"
        );
    }
    let elm_json_source_directories = match maybe_elm_json {
        None => {
            vec![project_path.clone()]
        }
        Some(elm::ElmProjectProject::Application(application_elm_json)) => application_elm_json
            .dirs
            .into_iter()
            .map(|elm_string| std::path::Path::join(&project_path, elm_string.to_string()))
            .collect::<Vec<_>>(),
        Some(elm::ElmProjectProject::Package(_)) => {
            vec![std::path::Path::join(&project_path, "src")]
        }
    };
    let direct_dependency_paths: Vec<std::path::PathBuf> = match maybe_elm_json {
        None => {
            vec!(std::path::Path::join(
                &elm_home_path,
                "0.19.1/packages/elm/core/1.0.5"
            ))
        }
        Some(elm::ElmProjectProject::Application(application_elm_json)) => {
            application_elm_json
                .deps_direct
                .into_iter()
                .map(|(elm::ElmPackageName::Name(package_author, package_name), elm::ElmVersionVersion::Version(package_major_version, package_minor_version, package_patch_version))| {
                    std::path::Path::join(
                        &elm_home_path,
                        format!("0.19.1/packages/{package_author}/{package_name}/{package_major_version}.{package_minor_version}.{package_patch_version}"),
                    )
                })
                .collect::<Vec<_>>()
        }
        Some(elm::ElmProjectProject::Package(package_elm_json)) => {
            package_elm_json.deps
                .into_iter()
                .filter_map(|(elm::ElmPackageName::Name(package_author, package_name), elm::ElmConstraintConstraint::Constraint(elm::ElmVersionVersion::Version(package_major_version, package_minor_version, package_patch_version), lower_bound_operator, _, _))| {
                    match lower_bound_operator {
                        elm::ElmConstraintOp::LessThan => {
                            // I have never seen this used
                            // and it would require some work to implement
                            // so we don't support it
                            None
                        },
                        elm::ElmConstraintOp::LessOrEq => Some(std::path::Path::join(
                            &elm_home_path,
                            format!("0.19.1/packages/{package_author}/{package_name}/{package_major_version}.{package_minor_version}.{package_patch_version}"),
                        ))
                    }
                })
                .collect::<Vec<_>>()
        }
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
        let parse_allocator: bumpalo::Bump = bumpalo::Bump::new();
        module_states.insert(
            module_path,
            ModuleState {
                syntax: elm::elm_parser_lenient_run(
                    elm::elm_parser_lenient_module_(&parse_allocator),
                    elm::StringString::One(&module_source),
                )
                .map(elm_syntax_module_to_persistent),
            },
        );
    }
    match maybe_elm_json {
        None => {}
        Some(elm::ElmProjectProject::Application(_)) => {}
        Some(elm::ElmProjectProject::Package(package_elm_json)) => {
            for exposed_module_name in elm_project_exposed_to_module_names(package_elm_json.exposed)
            {
                let maybe_module_origin_path: Option<&std::path::PathBuf> = module_states
                    .iter()
                    .find_map(|(module_path, module_state)| {
                        module_state.syntax.as_ref().and_then(|module_syntax| {
                            if &module_syntax.header.value.module_name.value == &exposed_module_name
                            {
                                Some(module_path)
                            } else {
                                None
                            }
                        })
                    });
                match maybe_module_origin_path {
                    None => {}
                    Some(module_origin_path) => {
                        dependency_exposed_module_names_so_far.insert(
                            exposed_module_name,
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
        direct_dependency_paths.into_iter(),
    );
    state.projects.insert(
        project_path,
        ProjectState {
            source_directories: elm_json_source_directories,
            modules: module_states,
            dependency_exposed_module_names,
        },
    );
}

fn elm_project_exposed_to_module_names(elm_project_exposed: elm::ElmProjectExposed) -> Vec<String> {
    match elm_project_exposed {
        elm::ElmProjectExposed::ExposedDict(grouped) => grouped
            .into_iter()
            .flat_map(|(_group_name, names)| names.into_iter())
            .map(|elm::ElmModuleName::Name(s)| s.to_string())
            .collect::<Vec<_>>(),
        elm::ElmProjectExposed::ExposedList(names) => names
            .into_iter()
            .map(|elm::ElmModuleName::Name(s)| s.to_string())
            .collect::<Vec<_>>(),
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
                if module_state.syntax.as_ref().is_some_and(|module_syntax| {
                    &module_syntax.header.value.module_name.value == module_name
                }) {
                    Some((module_path, module_state))
                } else {
                    None
                }
            }),
    }
}

struct ProjectModuleState<'a> {
    project: &'a ProjectState,
    module_syntax: &'a ElmSyntaxModule,
}

fn state_get_project_module_by_lsp_url<'a>(
    state: &'a State,
    uri: &lsp_types::Url,
) -> Option<ProjectModuleState<'a>> {
    let file_path = uri.to_file_path().ok()?;
    state.projects.values().find_map(|project_state| {
        let module_state = project_state.modules.get(&file_path)?;
        let module_syntax = module_state.syntax.as_ref()?;
        Some(ProjectModuleState {
            project: project_state,
            module_syntax: module_syntax,
        })
    })
}

fn respond_to_hover(
    state: &State,
    hover_arguments: lsp_types::HoverParams,
) -> Option<lsp_types::Hover> {
    let project_module_state = state_get_project_module_by_lsp_url(
        state,
        &hover_arguments
            .text_document_position_params
            .text_document
            .uri,
    )?;
    let module_syntax = project_module_state.module_syntax;
    let hovered_reference = elm_syntax_module_find_reference_at_position(
        state,
        project_module_state.project,
        module_syntax,
        hover_arguments.text_document_position_params.position,
    )?;
    match hovered_reference.value {
        ElmSyntaxSymbol::TypeVariable { .. } => None,
        ElmSyntaxSymbol::ModuleName(hovered_module_name)
        | ElmSyntaxSymbol::ImportAlias {
            module_origin: hovered_module_name,
            alias_name: _,
        } => {
            let origin_module_syntax = project_state_get_module_with_name(
                state,
                project_module_state.project,
                hovered_module_name,
            )
            .and_then(|(_, m)| m.syntax.as_ref())?;
            let maybe_module_documentation_comment = origin_module_syntax
                .comments
                .iter()
                .find(|comment_node| comment_node.value.starts_with("{-|"));
            // also show list of exports maybe?
            Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                    match maybe_module_documentation_comment {
                        None => "_module has no documentation comment_".to_string(),
                        Some(module_documentation) => {
                            documentation_comment_to_markdown(&module_documentation.value)
                        }
                    },
                )),
                range: Some(hovered_reference.range),
            })
        }
        ElmSyntaxSymbol::LocalBinding {
            scope_expression: _,
            name: hovered_local_binding_name,
            origin: hovered_local_binding_origin,
        } => match hovered_local_binding_origin {
            LocalBindingOrigin::PatternVariable(_) => None,
            LocalBindingOrigin::PatternRecordField(_) => None,
            LocalBindingOrigin::LetDeclaredVariable {
                signature: hovered_local_binding_maybe_signature,
                implementation_name_range: _,
            } => Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                    match hovered_local_binding_maybe_signature {
                        None => format!("```elm\nlet {}\n```\n", hovered_local_binding_name),
                        Some(hovered_local_binding_signature) => {
                            format!(
                                "```elm\nlet {} : {}\n```\n",
                                hovered_local_binding_name,
                                &elm_syntax_type_to_single_line_string(
                                    &elm_syntax_module_create_origin_lookup(
                                        state,
                                        project_module_state.project,
                                        module_syntax
                                    ),
                                    &hovered_local_binding_signature.type_1.value,
                                )
                            )
                        }
                    },
                )),
                range: Some(hovered_reference.range),
            }),
        },
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            module_origin: hovered_module_origin,
            name: hovered_name,
        } => {
            let origin_module_syntax = project_state_get_module_with_name(
                state,
                project_module_state.project,
                hovered_module_origin,
            )
            .and_then(|(_, m)| m.syntax.as_ref())?;
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                project_module_state.project,
                origin_module_syntax,
            );
            origin_module_syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    match &origin_module_declaration.declaration.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0: origin_module_declaration_variant0,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            let any_declared_name_matches_hovered =
                                (&origin_module_declaration_variant0.name.value == hovered_name)
                                    || (origin_module_declaration_variant1_up
                                        .iter()
                                        .any(|variant| &variant.name.value == hovered_name));
                            if any_declared_name_matches_hovered {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: format!(
                                                "variant in\n{}",
                                                &present_choice_type_declaration_info_markdown(
                                                    &module_origin_lookup,
                                                    hovered_module_origin,
                                                    &origin_module_declaration_name.value,
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref(),
                                                    origin_module_declaration_parameters,
                                                    origin_module_declaration_variant0,
                                                    origin_module_declaration_variant1_up,
                                                )
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            direction: origin_module_declaration_direction,
                            operator: origin_module_declaration_operator,
                            function: origin_module_declaration_function,
                            precedence: origin_module_declaration_precedence,
                        } => {
                            if hovered_name == &origin_module_declaration_operator.value {
                                let maybe_origin_operator_function_declaration =
                                    origin_module_syntax.declarations.iter().find_map(
                                        |origin_module_declaration| match &origin_module_declaration
                                            .declaration
                                            .value
                                        {
                                            ElmSyntaxDeclaration::ValueOrFunction {
                                                name: origin_module_declaration_name,
                                                signature: origin_module_declaration_signature,
                                                implementation_name_range: _,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } if origin_module_declaration_name
                                                == &origin_module_declaration_function.value =>
                                            {
                                                Some((
                                                    origin_module_declaration_signature,
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref(),
                                                ))
                                            }
                                            _ => None,
                                        },
                                    );
                                let hover_markdown = present_operator_declaration_info_markdown(
                                    &module_origin_lookup,
                                    hovered_module_origin,
                                    &origin_module_declaration_operator.value,
                                    maybe_origin_operator_function_declaration,
                                    origin_module_declaration.documentation.as_ref(),
                                    origin_module_declaration_precedence.value,
                                    origin_module_declaration_direction.value,
                                );
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: hover_markdown,
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: origin_module_declaration_name,
                            type_,
                        } => {
                            if &origin_module_declaration_name.value == hovered_name {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_port_declaration_info_markdown(
                                                &module_origin_lookup,
                                                &hovered_module_origin,
                                                hovered_name,
                                                origin_module_declaration.documentation.as_ref(),
                                                &type_.value,
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            type_,
                        } => {
                            if &origin_module_declaration_name.value == hovered_name {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: format!(
                                                "constructor function for record\n{}",
                                                &present_type_alias_declaration_info_markdown(
                                                    &module_origin_lookup,
                                                    hovered_module_origin,
                                                    &origin_module_declaration_name.value,
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref(),
                                                    origin_module_declaration_parameters,
                                                    &type_.value
                                                )
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::ValueOrFunction {
                            name: origin_module_declaration_name,
                            signature: origin_module_declaration_maybe_signature,
                            implementation_name_range: _,
                            parameters: _,
                            equals_key_symbol_range: _,
                            result: _,
                        } => {
                            if origin_module_declaration_name == hovered_name {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_variable_declaration_info_markdown(
                                                &module_origin_lookup,
                                                hovered_module_origin,
                                                origin_module_declaration_name,
                                                origin_module_declaration.documentation.as_ref(),
                                                origin_module_declaration_maybe_signature.as_ref(),
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                    }
                })
        }
        ElmSyntaxSymbol::Type {
            module_origin: hovered_module_origin,
            name: hovered_name,
        } => {
            if (hovered_module_origin == "List") && (hovered_name == "List") {
                // module List has no type List.List exposed in an oversight so we make one up.
                // See https://github.com/elm/core/issues/1037
                return Some(lsp_types::Hover {
                    contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: list_list_type_info_markdown.to_string(),
                    }),
                    range: Some(hovered_reference.range),
                });
            }
            let origin_module_syntax = project_state_get_module_with_name(
                state,
                project_module_state.project,
                hovered_module_origin,
            )
            .and_then(|(_, m)| m.syntax.as_ref())?;
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                project_module_state.project,
                origin_module_syntax,
            );
            origin_module_syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    match &origin_module_declaration.declaration.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: origin_module_declaration_parameters,
                            equals_key_symbol_range: _,
                            variant0: origin_module_declaration_variant0,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if &origin_module_declaration_name.value == hovered_name {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_choice_type_declaration_info_markdown(
                                                &module_origin_lookup,
                                                hovered_module_origin,
                                                &origin_module_declaration_name.value,
                                                origin_module_declaration.documentation.as_ref(),
                                                origin_module_declaration_parameters,
                                                origin_module_declaration_variant0,
                                                origin_module_declaration_variant1_up,
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
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
                            if &origin_module_declaration_name.value == hovered_name {
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Markup(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: present_type_alias_declaration_info_markdown(
                                                &module_origin_lookup,
                                                hovered_module_origin,
                                                hovered_name,
                                                origin_module_declaration.documentation.as_ref(),
                                                origin_module_declaration_parameters,
                                                &type_.value,
                                            ),
                                        },
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::ValueOrFunction { .. } => None,
                    }
                })
        }
    }
}

fn respond_to_goto_definition(
    state: &State,
    goto_definition_arguments: lsp_types::GotoDefinitionParams,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let project_module_state = state_get_project_module_by_lsp_url(
        state,
        &goto_definition_arguments
            .text_document_position_params
            .text_document
            .uri,
    )?;
    let module_syntax = project_module_state.module_syntax;
    let goto_reference = elm_syntax_module_find_reference_at_position(
        state,
        project_module_state.project,
        module_syntax,
        goto_definition_arguments
            .text_document_position_params
            .position,
    )?;
    match goto_reference.value {
        ElmSyntaxSymbol::LocalBinding {
            scope_expression: _,
            name: _,
            origin: goto_origin,
        } => match goto_origin {
            LocalBindingOrigin::PatternVariable(range) => Some(
                lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                    uri: goto_definition_arguments
                        .text_document_position_params
                        .text_document
                        .uri,
                    range: range,
                }),
            ),
            LocalBindingOrigin::PatternRecordField(range) => Some(
                lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                    uri: goto_definition_arguments
                        .text_document_position_params
                        .text_document
                        .uri,
                    range: range,
                }),
            ),
            LocalBindingOrigin::LetDeclaredVariable {
                signature: _,
                implementation_name_range,
            } => Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: goto_definition_arguments
                        .text_document_position_params
                        .text_document
                        .uri,
                    range: implementation_name_range,
                },
            )),
        },
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration,
            name: goto_type_variable_name,
        } => {
            match scope_declaration {
                ElmSyntaxDeclaration::ChoiceType {
                    name: _,
                    parameters: origin_type_parameters,
                    equals_key_symbol_range: _,
                    variant0: _,
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
                ElmSyntaxDeclaration::ValueOrFunction { .. } => None,
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
                    project_module_state.project,
                    goto_module_name,
                )?;
            let origin_module_syntax = origin_module_state.syntax.as_ref()?;
            let origin_module_file_url =
                lsp_types::Url::from_file_path(origin_module_file_path).ok()?;
            Some(lsp_types::GotoDefinitionResponse::Scalar(
                lsp_types::Location {
                    uri: origin_module_file_url,
                    range: origin_module_syntax.header.value.module_name.range,
                },
            ))
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            module_origin: goto_module_origin,
            name: goto_name,
        } => {
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_syntax = origin_module_state.syntax.as_ref()?;
            origin_module_syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    match &origin_module_declaration.declaration.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: _,
                            parameters: _,
                            equals_key_symbol_range: _,
                            variant0: origin_module_declaration_variant0,
                            variant1_up: origin_module_declaration_variant1_up,
                        } => {
                            if &origin_module_declaration_variant0.name.value == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_declaration_variant0.name.range,
                                    }))
                            } else {
                                origin_module_declaration_variant1_up
                                    .iter()
                                    .find(|variant| &variant.name.value == goto_name)
                                    .and_then(|variant| {
                                        lsp_types::Url::from_file_path(origin_module_file_path)
                                            .ok()
                                            .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                                uri: origin_module_file_url,
                                                range: variant.name.range,
                                            }))
                                    })
                            }
                        }
                        ElmSyntaxDeclaration::Operator {
                            direction: _,
                            operator: origin_module_declaration_operator,
                            function: origin_module_declaration_function,
                            precedence: _,
                        } => {
                            if goto_name == &origin_module_declaration_operator.value {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_declaration_operator.range,
                                    }))
                            } else if goto_name == &origin_module_declaration_function.value {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_declaration_function.range,
                                    }))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Port {
                            name: origin_module_declaration_name,
                            type_: _,
                        } => {
                            if &origin_module_declaration_name.value == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_declaration_name.range,
                                    }))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: origin_module_declaration_name,
                            parameters: _,
                            equals_key_symbol_range: _,
                            type_: _,
                        } => {
                            // record type alias constructor function
                            if &origin_module_declaration_name.value == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_declaration_name.range,
                                    }))
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::ValueOrFunction {
                            name: origin_module_declaration_name,
                            signature: origin_module_declaration_maybe_signature,
                            implementation_name_range:
                                origin_module_declaration_implementation_name_range,
                            parameters: _,
                            equals_key_symbol_range: _,
                            result: _,
                        } => {
                            if origin_module_declaration_name == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: match origin_module_declaration_maybe_signature {
                                            Some(origin_module_declaration_signature) => {
                                                origin_module_declaration_signature.name.range
                                            }
                                            None => {
                                                *origin_module_declaration_implementation_name_range
                                            }
                                        },
                                    }))
                            } else {
                                None
                            }
                        }
                    }
                })
        }
        ElmSyntaxSymbol::Type {
            module_origin: goto_module_origin,
            name: goto_name,
        } => {
            let (origin_module_file_path, origin_module_state) =
                project_state_get_module_with_name(
                    state,
                    project_module_state.project,
                    goto_module_origin,
                )?;
            let origin_module_syntax = origin_module_state.syntax.as_ref()?;
            origin_module_syntax
                .declarations
                .iter()
                .find_map(|origin_module_declaration| {
                    match &origin_module_declaration.declaration.value {
                        ElmSyntaxDeclaration::ChoiceType {
                            name: origin_module_declaration_name,
                            parameters: _,
                            equals_key_symbol_range: _,
                            variant0: _,
                            variant1_up: _,
                        } => {
                            if &origin_module_declaration_name.value == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| {
                                        lsp_types::GotoDefinitionResponse::Scalar(
                                            lsp_types::Location {
                                                uri: origin_module_file_url,
                                                range: origin_module_declaration_name.range,
                                            },
                                        )
                                    })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::Operator { .. } => None,
                        ElmSyntaxDeclaration::Port { .. } => None,
                        ElmSyntaxDeclaration::TypeAlias {
                            alias_keyword_range: _,
                            name: origin_module_declaration_name,
                            parameters: _,
                            equals_key_symbol_range: _,
                            type_: _,
                        } => {
                            if &origin_module_declaration_name.value == goto_name {
                                lsp_types::Url::from_file_path(origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| {
                                        lsp_types::GotoDefinitionResponse::Scalar(
                                            lsp_types::Location {
                                                uri: origin_module_file_url,
                                                range: origin_module_declaration_name.range,
                                            },
                                        )
                                    })
                            } else {
                                None
                            }
                        }
                        ElmSyntaxDeclaration::ValueOrFunction { .. } => None,
                    }
                })
        }
    }
}

fn respond_to_prepare_rename(
    state: &State,
    prepare_rename_arguments: lsp_types::TextDocumentPositionParams,
) -> Option<Result<lsp_types::PrepareRenameResponse, async_lsp::ResponseError>> {
    let project_module_state =
        state_get_project_module_by_lsp_url(state, &prepare_rename_arguments.text_document.uri)?;
    let prepare_rename_module_syntax = project_module_state.module_syntax;
    let prepare_rename_symbol: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_reference_at_position(
            state,
            project_module_state.project,
            prepare_rename_module_syntax,
            prepare_rename_arguments.position,
        )?;
    Some(match prepare_rename_symbol.value {
        ElmSyntaxSymbol::ImportAlias {
            module_origin: _,
            alias_name: alias_name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol.range,
            placeholder: alias_name.to_string(),
        }),
        ElmSyntaxSymbol::LocalBinding {
            scope_expression: _,
            origin: local_binding_origin,
            name: local_binding_name,
        } => match local_binding_origin {
            LocalBindingOrigin::PatternVariable(_)
            | LocalBindingOrigin::LetDeclaredVariable { .. } => {
                Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                    range: prepare_rename_symbol.range,
                    placeholder: local_binding_name.to_string(),
                })
            }
            LocalBindingOrigin::PatternRecordField(_) => Err(async_lsp::ResponseError::new(
                async_lsp::ErrorCode::REQUEST_FAILED,
                "cannot rename a variable that is bound to a field name",
            )),
        },
        ElmSyntaxSymbol::ModuleName(module_name) => {
            Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                range: prepare_rename_symbol.range,
                placeholder: module_name.to_string(),
            })
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            module_origin: _,
            name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: lsp_types::Range {
                start: lsp_position_add_characters(
                    prepare_rename_symbol.range.end,
                    -(name.len() as i32),
                ),
                end: prepare_rename_symbol.range.end,
            },
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::Type {
            module_origin: _,
            name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: lsp_types::Range {
                start: lsp_position_add_characters(
                    prepare_rename_symbol.range.end,
                    -(name.len() as i32),
                ),
                end: prepare_rename_symbol.range.end,
            },
            placeholder: name.to_string(),
        }),
        ElmSyntaxSymbol::TypeVariable {
            scope_declaration: _,
            name,
        } => Ok(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
            range: prepare_rename_symbol.range,
            placeholder: name.to_string(),
        }),
    })
}

struct ProjectModuleOriginAndState<'a> {
    project_state: &'a ProjectState,
    module_path: &'a std::path::PathBuf,
    module_syntax: &'a ElmSyntaxModule,
}

fn state_iter_all_modules<'a>(
    state: &'a State,
) -> impl Iterator<Item = ProjectModuleOriginAndState<'a>> {
    state.projects.values().flat_map(|project_state| {
        project_state
            .modules
            .iter()
            .filter_map(|(module_path, module_state)| {
                module_state
                    .syntax
                    .as_ref()
                    .map(|module_syntax| ProjectModuleOriginAndState {
                        project_state: project_state,
                        module_path: module_path,
                        module_syntax: module_syntax,
                    })
            })
    })
}

fn respond_to_rename(
    state: &State,
    rename_arguments: lsp_types::RenameParams,
) -> Option<Vec<lsp_types::TextDocumentEdit>> {
    let project_module_state = state_get_project_module_by_lsp_url(
        state,
        &rename_arguments.text_document_position.text_document.uri,
    )?;
    let to_rename_module_syntax = project_module_state.module_syntax;
    let symbol_to_rename: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_reference_at_position(
            state,
            project_module_state.project,
            to_rename_module_syntax,
            rename_arguments.text_document_position.position,
        )?;
    Some(match symbol_to_rename.value {
        ElmSyntaxSymbol::LocalBinding {
            scope_expression: local_binding_to_rename_scope_expression,
            origin: local_binding_to_rename_origin,
            name: local_binding_to_rename,
        } => {
            let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_expression_uses_of_reference_into(
                &mut all_uses_of_renamed_module_name,
                &elm_syntax_module_create_origin_lookup(
                    state,
                    project_module_state.project,
                    to_rename_module_syntax,
                ),
                &[LocalBinding {
                    name: local_binding_to_rename,
                    origin: local_binding_to_rename_origin,
                }],
                local_binding_to_rename_scope_expression,
                ElmDeclaredSymbol::LocalBinding(local_binding_to_rename),
            );
            match local_binding_to_rename_origin {
                LocalBindingOrigin::PatternVariable(range) => {
                    all_uses_of_renamed_module_name.push(range);
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
        ElmSyntaxSymbol::ImportAlias {
            module_origin: import_alias_to_rename_module_origin,
            alias_name: import_alias_to_rename,
        } => {
            let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
            elm_syntax_module_uses_of_reference_into(
                &mut all_uses_of_renamed_module_name,
                state,
                project_module_state.project,
                to_rename_module_syntax,
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
                &to_rename_module_syntax.header.value.module_name.value,
                &elm_syntax_module_create_origin_lookup(
                    state,
                    project_module_state.project,
                    to_rename_module_syntax,
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
                let elm_module_syntax = elm_module_state.syntax.as_ref()?;
                let elm_module_uri = lsp_types::Url::from_file_path(elm_module_file_path).ok()?;
                let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_module_uses_of_reference_into(
                    &mut all_uses_of_renamed_module_name,
                    state,
                    project_module_state.project,
                    elm_module_syntax,
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
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            module_origin: to_rename_module_origin,
            name: to_rename_name,
        } => state_iter_all_modules(state)
            .filter_map(|project_module| {
                let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                elm_syntax_module_uses_of_reference_into(
                    &mut all_uses_of_renamed_module_name,
                    state,
                    project_module.project_state,
                    project_module.module_syntax,
                    ElmDeclaredSymbol::VariableOrVariant {
                        module_origin: to_rename_module_origin,
                        name: to_rename_name,
                    },
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
            .collect::<Vec<_>>(),
        ElmSyntaxSymbol::Type {
            module_origin: to_rename_module_origin,
            name: type_name_to_rename,
        } => {
            let to_rename_is_record_type_alias = project_state_get_module_with_name(
                state,
                project_module_state.project,
                to_rename_module_origin,
            )
            .and_then(|(_, to_rename_module_state)| to_rename_module_state.syntax.as_ref())
            .is_some_and(|to_rename_module_syntax| {
                to_rename_module_syntax
                    .declarations
                    .iter()
                    .any(
                        |documented_declaration| match &documented_declaration.declaration.value {
                            ElmSyntaxDeclaration::TypeAlias {
                                type_:
                                    ElmSyntaxNode {
                                        value: ElmSyntaxType::Record(_),
                                        range: _,
                                    },
                                alias_keyword_range: _,
                                name: record_type_alias,
                                parameters: _,
                                equals_key_symbol_range: _,
                            } => record_type_alias.value == type_name_to_rename,
                            _ => false,
                        },
                    )
            });
            let elm_declared_symbol_to_rename = if to_rename_is_record_type_alias {
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
                        project_module.module_syntax,
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
    elm_syntax_highlight_module_into(&mut highlighting, project_module_state.module_syntax);
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
    maybe_documentation: Option<&ElmSyntaxNode<String>>,
    maybe_signature: Option<
        &elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>,
    >,
) -> String {
    let description = match maybe_signature {
        Some(origin_module_declaration_signature) => format!(
            "```elm\n{}.{} : {}\n```\n",
            module_origin,
            name,
            &elm_syntax_type_to_single_line_string(
                &module_origin_lookup,
                &origin_module_declaration_signature.type_1.value,
            )
        ),
        None => format!("```elm\n{}.{}\n```\n", &module_origin, &name),
    };
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(&documentation.value)
        }
    }
}
fn present_port_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&ElmSyntaxNode<String>>,
    type_: &ElmSyntaxType,
) -> String {
    let description = format!(
        "```elm\nport {}.{} : {}\n```\n",
        module_origin,
        name,
        &elm_syntax_type_to_single_line_string(&module_origin_lookup, type_)
    );
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(&documentation.value)
        }
    }
}
fn present_type_alias_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    name: &str,
    maybe_documentation: Option<&ElmSyntaxNode<String>>,
    parameters: &[ElmSyntaxNode<String>],
    type_: &ElmSyntaxType,
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
        &elm_syntax_type_to_single_line_string(&module_origin_lookup, type_,)
    );
    match maybe_documentation {
        None => description,
        Some(documentation) => {
            description + "-----\n" + &documentation_comment_to_markdown(&documentation.value)
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
    maybe_documentation: Option<&ElmSyntaxNode<String>>,
    parameters: &[ElmSyntaxNode<String>],
    variant0: &elm::GeneratedNameValues<ElmSyntaxNode<String>, Vec<ElmSyntaxNode<ElmSyntaxType>>>,
    variant1_up: &[elm::GeneratedNameOrKeySymbolRangeValues<
        ElmSyntaxNode<String>,
        lsp_types::Range,
        Vec<ElmSyntaxNode<ElmSyntaxType>>,
    >],
) -> String {
    let description = format!(
        "```elm\ntype {}.{}{}\n    = {}{}{}\n```\n",
        module_origin,
        name,
        &parameters
            .iter()
            .fold(String::new(), |so_far, parameter_node| so_far
                + " "
                + &parameter_node.value,),
        &variant0.name.value,
        &variant0
            .values
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
                + &variant.name.value
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
            description + "-----\n" + &documentation_comment_to_markdown(&documentation.value)
        }
    }
}
fn present_operator_declaration_info_markdown(
    module_origin_lookup: &ModuleOriginLookup,
    module_origin: &str,
    operator_symbol: &str,
    maybe_origin_operator_function_declaration: Option<(
        &Option<elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>>,
        Option<&ElmSyntaxNode<String>>,
    )>,
    maybe_documentation: Option<&ElmSyntaxNode<String>>,
    precedence: i64,
    direction: elm::ElmSyntaxInfixDirection,
) -> String {
    match maybe_origin_operator_function_declaration {
        Some((
            origin_operator_function_maybe_signature,
            origin_operator_function_maybe_documentation,
        )) => {
            let description = format!(
                "```elm\ninfix {} {precedence} {module_origin}.({operator_symbol}){}\n```\n",
                elm_syntax_infix_direction_to_str(direction),
                &(match origin_operator_function_maybe_signature {
                    None => "".to_string(),
                    Some(origin_operator_function_signature) => {
                        " : ".to_string()
                            + &elm_syntax_type_to_single_line_string(
                                &module_origin_lookup,
                                &origin_operator_function_signature.type_1.value,
                            )
                    }
                })
            );
            match origin_operator_function_maybe_documentation {
                None => description,
                Some(documentation) => {
                    description
                        + "-----\n"
                        + &documentation_comment_to_markdown(&documentation.value)
                }
            }
        }
        None => {
            let description = format!(
                "```elm\ninfix {} {precedence} {module_origin}.({operator_symbol})\n```\n",
                elm_syntax_infix_direction_to_str(direction)
            );
            match maybe_documentation {
                None => description,
                Some(documentation) => {
                    description
                        + "-----\n"
                        + &documentation_comment_to_markdown(&documentation.value)
                }
            }
        }
    }
}
fn elm_syntax_infix_direction_to_str(direction: elm::ElmSyntaxInfixDirection) -> &'static str {
    match direction {
        elm::ElmSyntaxInfixDirection::Left => "left",
        elm::ElmSyntaxInfixDirection::Non => "non",
        elm::ElmSyntaxInfixDirection::Right => "right",
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
    // consider instead using a customized find that also returns local bindings
    // and the module origin lookup
    let symbol_to_complete: ElmSyntaxNode<ElmSyntaxSymbol> =
        elm_syntax_module_find_reference_at_position(
            state,
            completion_project_module.project,
            completion_project_module.module_syntax,
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
            if module_name
                == &completion_project_module
                    .module_syntax
                    .header
                    .value
                    .module_name
                    .value
            {
                return None;
            }
            Some(
                completion_project_module
                    .project
                    .dependency_exposed_module_names
                    .iter()
                    .filter_map(
                        |(importable_dependency_module_name, importable_dependency_module_origin)| {
                            let importable_dependency_module_syntax =
                                state.projects.get(&importable_dependency_module_origin.project_path)
                                .and_then(|dependency_state| {
                                    dependency_state.modules.get(&importable_dependency_module_origin.module_path)
                                })
                                .and_then(|importable_dependency_module_state| importable_dependency_module_state.syntax.as_ref())?;
                            let maybe_importable_dependency_module_documentation_comment = importable_dependency_module_syntax
                                .comments
                                .iter()
                                .find(|comment_node| comment_node.value.starts_with("{-|"));
                            Some(lsp_types::CompletionItem {
                                label: importable_dependency_module_name.clone(),
                                kind: Some(lsp_types::CompletionItemKind::MODULE),
                                documentation: Some(
                                    lsp_types::Documentation::MarkupContent(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: match maybe_importable_dependency_module_documentation_comment {
                                                None => "_module has no documentation comment_".to_string(),
                                                Some(module_documentation) => {
                                                    documentation_comment_to_markdown(&module_documentation.value)
                                                }
                                            },
                                        },
                                    ),
                                ),
                                ..lsp_types::CompletionItem::default()
                            })
                        },
                    )
                    .chain(
                        completion_project_module
                            .project
                            .modules
                            .iter()
                            .filter_map(|(_, project_module)| {
                                let project_module_syntax = project_module.syntax.as_ref()?;
                                let maybe_project_module_documentation_comment = project_module_syntax
                                    .comments
                                    .iter()
                                    .find(|comment_node| comment_node.value.starts_with("{-|"));
                                Some(lsp_types::CompletionItem {
                                    label: project_module_syntax
                                        .header
                                        .value
                                        .module_name
                                        .value
                                        .clone(),
                                    kind: Some(
                                        lsp_types::CompletionItemKind::MODULE,
                                    ),
                                    documentation: Some(
                                        lsp_types::Documentation::MarkupContent(
                                            lsp_types::MarkupContent {
                                                kind:
                                                    lsp_types::MarkupKind::Markdown,
                                                value:
                                                    match maybe_project_module_documentation_comment {
                                                        None => "_module has no documentation comment_".to_string(),
                                                        Some(module_documentation) => {
                                                            documentation_comment_to_markdown(&module_documentation.value)
                                                        }
                                                    },
                                            },
                                        ),
                                    ),
                                    ..lsp_types::CompletionItem::default()
                                })
                            }),
                    )
                    .collect::<Vec<_>>(),
            )
        }
        ElmSyntaxSymbol::VariableOrVariantOrOperator {
            module_origin,
            name: _,
        } => {
            let module_origin = if module_origin.is_empty() {
                &completion_project_module
                    .module_syntax
                    .header
                    .value
                    .module_name
                    .value
            } else {
                module_origin
            };
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                completion_project_module.project,
                module_origin,
            )?;
            let origin_module_syntax = origin_module_state.syntax.as_ref()?;
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                completion_project_module.project,
                origin_module_syntax,
            );
            let variable_and_open_choice_type_exposes_from_origin_module_or_none_if_all =
                if module_origin
                    == &completion_project_module
                        .module_syntax
                        .header
                        .value
                        .module_name
                        .value
                {
                    None
                } else {
                    match &origin_module_syntax.header.value.exposing.value {
                        ElmSyntaxExposing::All(_) => None,
                        ElmSyntaxExposing::Explicit(exposes) => Some(
                            exposes
                                .iter()
                                .filter_map(|expose_node| match &expose_node.value {
                                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                                        name: open_choice_type_name,
                                        open_range: _,
                                    } => Some(&open_choice_type_name.value),
                                    ElmSyntaxExpose::Operator(operator_symbol) => {
                                        Some(operator_symbol)
                                    }
                                    ElmSyntaxExpose::Type(_) => None,
                                    ElmSyntaxExpose::Variable(name) => Some(name),
                                })
                                .collect::<std::collections::HashSet<_>>(),
                        ),
                    }
                };

            let mut completion_items = Vec::new();
            for origin_module_declaration in origin_module_syntax.declarations.iter() {
                match &origin_module_declaration.declaration.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: choice_type_name,
                        parameters,
                        equals_key_symbol_range: _,
                        variant0,
                        variant1_up,
                    } => {
                        if variable_and_open_choice_type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(&choice_type_name.value))
                        {
                            let info_markdown = format!(
                                "variant in\n{}",
                                present_choice_type_declaration_info_markdown(
                                    &module_origin_lookup,
                                    module_origin,
                                    &choice_type_name.value,
                                    origin_module_declaration.documentation.as_ref(),
                                    parameters,
                                    variant0,
                                    variant1_up,
                                ),
                            );
                            completion_items.extend(
                                std::iter::once(variant0.name.value.clone())
                                    .chain(
                                        variant1_up
                                            .iter()
                                            .map(|variant| variant.name.value.clone()),
                                    )
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
                    ElmSyntaxDeclaration::Port { name, type_ } => {
                        if variable_and_open_choice_type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(&name.value))
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_port_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &name.value,
                                            origin_module_declaration.documentation.as_ref(),
                                            &type_.value,
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            })
                        }
                    }
                    ElmSyntaxDeclaration::TypeAlias {
                        alias_keyword_range: _,
                        name,
                        parameters,
                        equals_key_symbol_range: _,
                        type_,
                    } => {
                        if variable_and_open_choice_type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(&name.value))
                        {
                            if let ElmSyntaxType::Record(_) = type_.value {
                                completion_items.push(lsp_types::CompletionItem {
                                    label: name.value.clone(),
                                    kind: Some(lsp_types::CompletionItemKind::CONSTRUCTOR),
                                    documentation: Some(lsp_types::Documentation::MarkupContent(
                                        lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: format!(
                                                "constructor function for record\n{}",
                                                &present_type_alias_declaration_info_markdown(
                                                    &module_origin_lookup,
                                                    module_origin,
                                                    &name.value,
                                                    origin_module_declaration
                                                        .documentation
                                                        .as_ref(),
                                                    parameters,
                                                    &type_.value,
                                                )
                                            ),
                                        },
                                    )),
                                    ..lsp_types::CompletionItem::default()
                                })
                            }
                        }
                    }
                    ElmSyntaxDeclaration::ValueOrFunction {
                        name,
                        signature: maybe_signature,
                        implementation_name_range: _,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        if variable_and_open_choice_type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(name))
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name.clone(),
                                kind: Some(lsp_types::CompletionItemKind::FUNCTION),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_variable_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            name,
                                            origin_module_declaration.documentation.as_ref(),
                                            maybe_signature.as_ref(),
                                        ),
                                    },
                                )),
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
            Some(completion_items)
        }
        ElmSyntaxSymbol::Type {
            module_origin,
            name: _,
        } => {
            let module_origin = if module_origin.is_empty() {
                &completion_project_module
                    .module_syntax
                    .header
                    .value
                    .module_name
                    .value
            } else {
                module_origin
            };
            let (_, origin_module_state) = project_state_get_module_with_name(
                state,
                completion_project_module.project,
                module_origin,
            )?;
            let origin_module_syntax = origin_module_state.syntax.as_ref()?;
            let module_origin_lookup: ModuleOriginLookup = elm_syntax_module_create_origin_lookup(
                state,
                completion_project_module.project,
                origin_module_syntax,
            );
            let type_exposes_from_origin_module_or_none_if_all = if module_origin
                == &completion_project_module
                    .module_syntax
                    .header
                    .value
                    .module_name
                    .value
            {
                None
            } else {
                match &origin_module_syntax.header.value.exposing.value {
                    ElmSyntaxExposing::All(_) => None,
                    ElmSyntaxExposing::Explicit(exposes) => Some(
                        exposes
                            .iter()
                            .filter_map(|expose_node| match &expose_node.value {
                                ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                                    name: open_choice_type_name,
                                    open_range: _,
                                } => Some(&open_choice_type_name.value),
                                ElmSyntaxExpose::Type(type_name) => Some(type_name),
                                ElmSyntaxExpose::Operator(_) => None,
                                ElmSyntaxExpose::Variable(_) => None,
                            })
                            .collect::<std::collections::HashSet<_>>(),
                    ),
                }
            };

            let mut completion_items = Vec::new();
            for origin_module_declaration in origin_module_syntax.declarations.iter() {
                match &origin_module_declaration.declaration.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name,
                        parameters,
                        equals_key_symbol_range: _,
                        variant0,
                        variant1_up,
                    } => {
                        if type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(&name.value))
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::ENUM),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_choice_type_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &name.value,
                                            origin_module_declaration.documentation.as_ref(),
                                            parameters,
                                            variant0,
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
                        name,
                        parameters,
                        equals_key_symbol_range: _,
                        type_,
                    } => {
                        if type_exposes_from_origin_module_or_none_if_all
                            .as_ref()
                            .is_none_or(|expose| expose.contains(&name.value))
                        {
                            completion_items.push(lsp_types::CompletionItem {
                                label: name.value.clone(),
                                kind: Some(lsp_types::CompletionItemKind::STRUCT),
                                documentation: Some(lsp_types::Documentation::MarkupContent(
                                    lsp_types::MarkupContent {
                                        kind: lsp_types::MarkupKind::Markdown,
                                        value: present_type_alias_declaration_info_markdown(
                                            &module_origin_lookup,
                                            module_origin,
                                            &name.value,
                                            origin_module_declaration.documentation.as_ref(),
                                            parameters,
                                            &type_.value,
                                        ),
                                    },
                                )),
                                ..lsp_types::CompletionItem::default()
                            });
                        }
                    }
                    ElmSyntaxDeclaration::Port { .. } => {}
                    ElmSyntaxDeclaration::ValueOrFunction { .. } => {}
                    ElmSyntaxDeclaration::Operator { .. } => {}
                }
            }
            Some(completion_items)
        }
        ElmSyntaxSymbol::LocalBinding { .. } => {
            // if a local name is already found to be matching,
            // there isn't really a point to suggest further completions
            None
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

fn state_update_source_at_path(
    state: &mut State,
    changed_path: std::path::PathBuf,
    changed_source: &str,
) {
    'updating_module: for project_state in state.projects.values_mut() {
        if project_state.modules.contains_key(&changed_path) {
            let parse_allocator: bumpalo::Bump = bumpalo::Bump::new();
            project_state.modules.insert(
                changed_path,
                ModuleState {
                    syntax: elm::elm_parser_lenient_run(
                        elm::elm_parser_lenient_module_(&parse_allocator),
                        elm::StringString::One(changed_source),
                    )
                    .map(elm_syntax_module_to_persistent),
                },
            );
            break 'updating_module;
        }
    }
}

fn documentation_comment_to_markdown(documentation: &str) -> String {
    let markdown_source = documentation
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
    'converting_fenced: while current_source_index <= (markdown_source.len() - 1) {
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

fn text_grid_location_to_lsp_position(
    text_grid_location: elm::TextGridLocation,
) -> lsp_types::Position {
    lsp_types::Position {
        line: (text_grid_location.line - 1) as u32,
        character: (text_grid_location.column - 1) as u32,
    }
}

fn text_grid_range_to_lsp_range(text_grid_range: elm::TextGridRange) -> lsp_types::Range {
    lsp_types::Range {
        start: text_grid_location_to_lsp_position(text_grid_range.start),
        end: text_grid_location_to_lsp_position(text_grid_range.end),
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
    Construct {
        reference: ElmSyntaxNode<elm::GeneratedNameQualification<String, String>>,
        arguments: Vec<ElmSyntaxNode<ElmSyntaxType>>,
    },
    Function {
        input: ElmSyntaxNode<Box<ElmSyntaxType>>,
        arrow_key_symbol_range: lsp_types::Range,
        output: ElmSyntaxNode<Box<ElmSyntaxType>>,
    },
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxType>>),
    Record(
        Vec<
            elm::GeneratedColonKeySymbolRangeNameValue<
                lsp_types::Range,
                ElmSyntaxNode<String>,
                ElmSyntaxNode<ElmSyntaxType>,
            >,
        >,
    ),
    RecordExtension {
        record_variable: ElmSyntaxNode<String>,
        bar_key_symbol_range: lsp_types::Range,
        field0: elm::GeneratedColonKeySymbolRangeNameValue<
            lsp_types::Range,
            ElmSyntaxNode<String>,
            ElmSyntaxNode<Box<ElmSyntaxType>>,
        >,
        field1_up: Vec<
            elm::GeneratedColonKeySymbolRangeNameValue<
                lsp_types::Range,
                ElmSyntaxNode<String>,
                ElmSyntaxNode<ElmSyntaxType>,
            >,
        >,
    },
    Triple {
        part0: ElmSyntaxNode<Box<ElmSyntaxType>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxType>>,
        part2: ElmSyntaxNode<Box<ElmSyntaxType>>,
    },
    Tuple {
        part0: ElmSyntaxNode<Box<ElmSyntaxType>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxType>>,
    },
    Unit,
    Variable(String),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxPattern {
    As {
        pattern: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        as_keyword_range: lsp_types::Range,
        variable: ElmSyntaxNode<String>,
    },
    Char(char),
    Ignored,
    Int {
        base: elm::ElmSyntaxIntBase,
        value: i64,
    },
    ListCons {
        head: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        cons_key_symbol: lsp_types::Range,
        tail: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
    },
    ListExact(Vec<ElmSyntaxNode<ElmSyntaxPattern>>),
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxPattern>>),
    Record(Vec<ElmSyntaxNode<String>>),
    String {
        content: String,
        quoting_style: elm::ElmSyntaxStringQuotingStyle,
    },
    Triple {
        part0: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        part2: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
    },
    Tuple {
        part0: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxPattern>>,
    },
    Unit,
    Variable(String),
    Variant {
        reference: ElmSyntaxNode<elm::GeneratedNameQualification<String, String>>,
        values: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxModuleHeaderSpecific {
    Effect {
        module_keyword_range: lsp_types::Range,
        where_keyword_range: lsp_types::Range,
        command: Option<ElmSyntaxNode<String>>,
        subscription: Option<ElmSyntaxNode<String>>,
    },
    Port {
        module_keyword_range: lsp_types::Range,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxLetDeclaration {
    Destructuring {
        pattern: ElmSyntaxNode<ElmSyntaxPattern>,
        equals_key_symbol_range: lsp_types::Range,
        expression: ElmSyntaxNode<ElmSyntaxExpression>,
    },
    ValueOrFunctionDeclaration {
        name: String,
        signature:
            Option<elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>>,
        implementation_name_range: lsp_types::Range,
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        equals_key_symbol_range: lsp_types::Range,
        result: ElmSyntaxNode<ElmSyntaxExpression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExpression {
    Call {
        called: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        argument0: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        argument1_up: Vec<ElmSyntaxNode<ElmSyntaxExpression>>,
    },
    CaseOf {
        matched: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        of_keyword_range: lsp_types::Range,
        case0: elm::GeneratedArrowKeySymbolRangePatternResult<
            lsp_types::Range,
            ElmSyntaxNode<ElmSyntaxPattern>,
            ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        >,
        case1_up: Vec<
            elm::GeneratedArrowKeySymbolRangePatternResult<
                lsp_types::Range,
                ElmSyntaxNode<ElmSyntaxPattern>,
                ElmSyntaxNode<ElmSyntaxExpression>,
            >,
        >,
    },
    Char(char),
    Float(f64),
    IfThenElse {
        condition: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        then_keyword_range: lsp_types::Range,
        on_true: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        else_keyword_range: lsp_types::Range,
        on_false: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    InfixOperation {
        left: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        operator: ElmSyntaxNode<String>,
        right: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    Integer {
        value: i64,
        base: elm::ElmSyntaxIntBase,
    },
    Lambda {
        parameter0: ElmSyntaxNode<ElmSyntaxPattern>,
        parameter1_up: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        arrow_key_symbol_range: lsp_types::Range,
        result: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    LetIn {
        declaration0: ElmSyntaxNode<Box<ElmSyntaxLetDeclaration>>,
        declaration1_up: Vec<ElmSyntaxNode<ElmSyntaxLetDeclaration>>,
        in_keyword_range: lsp_types::Range,
        result: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    List(Vec<ElmSyntaxNode<ElmSyntaxExpression>>),
    Negation(ElmSyntaxNode<Box<ElmSyntaxExpression>>),
    OperatorFunction(String),
    Parenthesized(ElmSyntaxNode<Box<ElmSyntaxExpression>>),
    Record(
        Vec<
            elm::GeneratedEqualsKeySymbolRangeNameValue<
                lsp_types::Range,
                ElmSyntaxNode<String>,
                ElmSyntaxNode<ElmSyntaxExpression>,
            >,
        >,
    ),
    RecordAccess {
        record: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        field: ElmSyntaxNode<String>,
    },
    RecordAccessFunction(String),
    RecordUpdate {
        record_variable: ElmSyntaxNode<String>,
        bar_key_symbol_range: lsp_types::Range,
        field0: elm::GeneratedEqualsKeySymbolRangeNameValue<
            lsp_types::Range,
            ElmSyntaxNode<String>,
            ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        >,
        field1_up: Vec<
            elm::GeneratedEqualsKeySymbolRangeNameValue<
                lsp_types::Range,
                ElmSyntaxNode<String>,
                ElmSyntaxNode<ElmSyntaxExpression>,
            >,
        >,
    },
    Reference(elm::GeneratedNameQualification<String, String>),
    String {
        content: String,
        quoting_style: elm::ElmSyntaxStringQuotingStyle,
    },
    Triple {
        part0: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        part2: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    Tuple {
        part0: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        part1: ElmSyntaxNode<Box<ElmSyntaxExpression>>,
    },
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExposing {
    All(lsp_types::Range),
    Explicit(Vec<ElmSyntaxNode<ElmSyntaxExpose>>),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxExpose {
    ChoiceTypeIncludingVariants {
        name: ElmSyntaxNode<String>,
        open_range: lsp_types::Range,
    },
    Operator(String),
    Type(String),
    Variable(String),
}

#[derive(Clone, Debug, PartialEq)]
enum ElmSyntaxDeclaration {
    ChoiceType {
        name: ElmSyntaxNode<String>,
        parameters: Vec<ElmSyntaxNode<String>>,
        equals_key_symbol_range: lsp_types::Range,
        variant0:
            elm::GeneratedNameValues<ElmSyntaxNode<String>, Vec<ElmSyntaxNode<ElmSyntaxType>>>,
        variant1_up: Vec<
            elm::GeneratedNameOrKeySymbolRangeValues<
                ElmSyntaxNode<String>,
                lsp_types::Range,
                Vec<ElmSyntaxNode<ElmSyntaxType>>,
            >,
        >,
    },
    Operator {
        direction: ElmSyntaxNode<elm::ElmSyntaxInfixDirection>,
        operator: ElmSyntaxNode<String>,
        function: ElmSyntaxNode<String>,
        precedence: ElmSyntaxNode<i64>,
    },
    Port {
        name: ElmSyntaxNode<String>,
        type_: ElmSyntaxNode<ElmSyntaxType>,
    },
    TypeAlias {
        alias_keyword_range: lsp_types::Range,
        name: ElmSyntaxNode<String>,
        parameters: Vec<ElmSyntaxNode<String>>,
        equals_key_symbol_range: lsp_types::Range,
        type_: ElmSyntaxNode<ElmSyntaxType>,
    },
    ValueOrFunction {
        name: String,
        signature:
            Option<elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>>,
        implementation_name_range: lsp_types::Range,
        parameters: Vec<ElmSyntaxNode<ElmSyntaxPattern>>,
        equals_key_symbol_range: lsp_types::Range,
        result: ElmSyntaxNode<ElmSyntaxExpression>,
    },
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

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxModuleHeader {
    exposing: ElmSyntaxNode<ElmSyntaxExposing>,
    module_name: ElmSyntaxNode<String>,
    specific: Option<ElmSyntaxModuleHeaderSpecific>,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxModule {
    header: ElmSyntaxNode<ElmSyntaxModuleHeader>,
    imports: Vec<ElmSyntaxNode<ElmSyntaxImport>>,
    comments: Vec<ElmSyntaxNode<String>>,
    declarations: Vec<
        elm::GeneratedDeclarationDocumentation<
            ElmSyntaxNode<ElmSyntaxDeclaration>,
            Option<ElmSyntaxNode<String>>,
        >,
    >,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxImport {
    module_name: ElmSyntaxNode<String>,
    alias: Option<elm::GeneratedAsKeywordRangeName<lsp_types::Range, ElmSyntaxNode<String>>>,
    exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>>,
}

// // if you are looking for conversions from persistent types to elm syntax types,
// // they can be found in previous commits (they have been removed because they aren't used, yet)
// //
fn elm_syntax_module_to_persistent(elm_syntax_module: elm::ElmSyntaxModule) -> ElmSyntaxModule {
    ElmSyntaxModule {
        header: elm_syntax_node_to_persistent(
            elm_syntax_module.header,
            elm_syntax_module_header_to_persistent,
        ),
        imports: elm_syntax_module
            .imports
            .into_iter()
            .map(|import_node| {
                elm_syntax_node_to_persistent(import_node, elm_syntax_import_to_persistent)
            })
            .collect::<Vec<_>>(),
        comments: elm_syntax_module
            .comments
            .into_iter()
            .map(elm_syntax_node_string_to_persistent)
            .collect::<Vec<_>>(),
        declarations: elm_syntax_module
            .declarations
            .into_iter()
            .map(
                |documented_declaration_node| elm::GeneratedDeclarationDocumentation {
                    documentation: documented_declaration_node
                        .documentation
                        .map(elm_syntax_node_string_to_persistent),
                    declaration: elm_syntax_node_to_persistent(
                        documented_declaration_node.declaration,
                        elm_syntax_declaration_to_persistent,
                    ),
                },
            )
            .collect::<Vec<_>>(),
    }
}

fn elm_syntax_module_header_to_persistent(
    elm_syntax_module_header: elm::ElmSyntaxModuleHeader,
) -> ElmSyntaxModuleHeader {
    ElmSyntaxModuleHeader {
        module_name: elm_syntax_node_string_to_persistent(elm_syntax_module_header.module_name),
        exposing: elm_syntax_node_to_persistent(
            elm_syntax_module_header.exposing_,
            elm_syntax_exposing_to_persistent,
        ),
        specific: elm_syntax_module_header
            .specific
            .map(elm_syntax_module_header_specific_to_persistent),
    }
}

fn elm_syntax_module_header_specific_to_persistent(
    elm_syntax_module_header_specific: elm::ElmSyntaxModuleHeaderSpecific,
) -> ElmSyntaxModuleHeaderSpecific {
    match elm_syntax_module_header_specific {
        elm::ElmSyntaxModuleHeaderSpecific::ModuleHeaderSpecificEffect(
            effect_module_header_specific,
        ) => ElmSyntaxModuleHeaderSpecific::Effect {
            module_keyword_range: text_grid_range_to_lsp_range(
                effect_module_header_specific.module_keyword_range,
            ),
            where_keyword_range: text_grid_range_to_lsp_range(
                effect_module_header_specific.where_keyword_range,
            ),
            command: effect_module_header_specific
                .command
                .map(elm_syntax_node_string_to_persistent),
            subscription: effect_module_header_specific
                .subscription
                .map(elm_syntax_node_string_to_persistent),
        },
        elm::ElmSyntaxModuleHeaderSpecific::ModuleHeaderSpecificPort(
            port_module_header_specific,
        ) => ElmSyntaxModuleHeaderSpecific::Port {
            module_keyword_range: text_grid_range_to_lsp_range(
                port_module_header_specific.module_keyword_range,
            ),
        },
    }
}

fn elm_syntax_import_to_persistent(elm_syntax_import: elm::ElmSyntaxImport) -> ElmSyntaxImport {
    ElmSyntaxImport {
        module_name: elm_syntax_node_string_to_persistent(elm_syntax_import.module_name),
        alias: elm_syntax_import
            .alias
            .map(|alias| elm::GeneratedAsKeywordRangeName {
                as_keyword_range: text_grid_range_to_lsp_range(alias.as_keyword_range),
                name: elm_syntax_node_string_to_persistent(alias.name),
            }),
        exposing: elm_syntax_import.exposing_.map(|exposing_node| {
            elm_syntax_node_to_persistent(exposing_node, elm_syntax_exposing_to_persistent)
        }),
    }
}

fn elm_syntax_exposing_to_persistent(
    elm_syntax_exposing: elm::ElmSyntaxExposing,
) -> ElmSyntaxExposing {
    match elm_syntax_exposing {
        elm::ElmSyntaxExposing::ExposingAll(all_range) => {
            ElmSyntaxExposing::All(text_grid_range_to_lsp_range(all_range))
        }
        elm::ElmSyntaxExposing::ExposingExplicit(exposes) => ElmSyntaxExposing::Explicit(
            exposes
                .into_iter()
                .map(|expose_node| {
                    elm_syntax_node_to_persistent(expose_node, elm_syntax_expose_to_persistent)
                })
                .collect::<Vec<_>>(),
        ),
    }
}

fn elm_syntax_expose_to_persistent(elm_syntax_expose: elm::ElmSyntaxExpose) -> ElmSyntaxExpose {
    match elm_syntax_expose {
        elm::ElmSyntaxExpose::ExposeChoiceTypeIncludingVariants(choice_type_expose) => {
            ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                name: elm_syntax_node_string_to_persistent(choice_type_expose.name),
                open_range: text_grid_range_to_lsp_range(choice_type_expose.open_range),
            }
        }
        elm::ElmSyntaxExpose::ExposeOperator(symbol) => {
            ElmSyntaxExpose::Operator(symbol.to_string())
        }
        elm::ElmSyntaxExpose::ExposeTypeName(name) => ElmSyntaxExpose::Type(name.to_string()),
        elm::ElmSyntaxExpose::ExposeVariable(name) => ElmSyntaxExpose::Variable(name.to_string()),
    }
}

fn elm_syntax_declaration_to_persistent(
    elm_syntax_declaration: elm::ElmSyntaxDeclaration,
) -> ElmSyntaxDeclaration {
    match elm_syntax_declaration {
        elm::ElmSyntaxDeclaration::DeclarationChoiceType(choice_type_declaration) => {
            ElmSyntaxDeclaration::ChoiceType {
                name: elm_syntax_node_string_to_persistent(choice_type_declaration.name),
                parameters: choice_type_declaration
                    .parameters
                    .into_iter()
                    .map(elm_syntax_node_string_to_persistent)
                    .collect::<Vec<_>>(),
                equals_key_symbol_range: text_grid_range_to_lsp_range(
                    choice_type_declaration.equals_key_symbol_range,
                ),
                variant0: elm::GeneratedNameValues {
                    name: elm_syntax_node_string_to_persistent(
                        choice_type_declaration.variant0.name,
                    ),
                    values: choice_type_declaration
                        .variant0
                        .values
                        .into_iter()
                        .map(elm_syntax_node_type_to_persistent)
                        .collect::<Vec<_>>(),
                },
                variant1_up: choice_type_declaration
                    .variant1_up
                    .into_iter()
                    .map(|variant| elm::GeneratedNameOrKeySymbolRangeValues {
                        or_key_symbol_range: text_grid_range_to_lsp_range(
                            variant.or_key_symbol_range,
                        ),
                        name: elm_syntax_node_string_to_persistent(variant.name),
                        values: variant
                            .values
                            .into_iter()
                            .map(elm_syntax_node_type_to_persistent)
                            .collect::<Vec<_>>(),
                    })
                    .collect::<Vec<_>>(),
            }
        }
        elm::ElmSyntaxDeclaration::DeclarationOperator(operator_declaration) => {
            ElmSyntaxDeclaration::Operator {
                direction: elm_syntax_node_to_persistent(
                    operator_declaration.direction,
                    |direction| direction,
                ),
                operator: elm_syntax_node_to_persistent(
                    operator_declaration.operator,
                    |operator| operator.to_string(),
                ),
                precedence: elm_syntax_node_to_persistent(
                    operator_declaration.precedence,
                    |precedence| precedence,
                ),
                function: elm_syntax_node_to_persistent(
                    operator_declaration.function,
                    |function| function.to_string(),
                ),
            }
        }
        elm::ElmSyntaxDeclaration::DeclarationPort(port_declaration) => {
            ElmSyntaxDeclaration::Port {
                name: elm_syntax_node_string_to_persistent(port_declaration.name),
                type_: elm_syntax_node_type_to_persistent(port_declaration.type_1),
            }
        }
        elm::ElmSyntaxDeclaration::DeclarationTypeAlias(type_alias_declaration) => {
            ElmSyntaxDeclaration::TypeAlias {
                alias_keyword_range: text_grid_range_to_lsp_range(
                    type_alias_declaration.alias_keyword_range,
                ),
                name: elm_syntax_node_string_to_persistent(type_alias_declaration.name),
                parameters: type_alias_declaration
                    .parameters
                    .into_iter()
                    .map(elm_syntax_node_string_to_persistent)
                    .collect::<Vec<_>>(),
                equals_key_symbol_range: text_grid_range_to_lsp_range(
                    type_alias_declaration.equals_key_symbol_range,
                ),
                type_: elm_syntax_node_type_to_persistent(type_alias_declaration.type_1),
            }
        }
        elm::ElmSyntaxDeclaration::DeclarationValueOrFunction(variable_declaration) => {
            ElmSyntaxDeclaration::ValueOrFunction {
                name: variable_declaration.name.to_string(),
                signature: variable_declaration.signature.map(|signature| {
                    elm::GeneratedNameType0 {
                        name: elm_syntax_node_string_to_persistent(signature.name),
                        type_1: elm_syntax_node_type_to_persistent(signature.type_1),
                    }
                }),
                implementation_name_range: text_grid_range_to_lsp_range(
                    variable_declaration.implementation_name_range,
                ),
                parameters: variable_declaration
                    .parameters
                    .into_iter()
                    .map(elm_syntax_node_pattern_to_persistent)
                    .collect::<Vec<_>>(),
                equals_key_symbol_range: text_grid_range_to_lsp_range(
                    variable_declaration.equals_key_symbol_range,
                ),
                result: elm_syntax_node_expression_to_persistent(variable_declaration.result),
            }
        }
    }
}

fn elm_syntax_type_to_persistent(elm_syntax_type: elm::ElmSyntaxType) -> ElmSyntaxType {
    match elm_syntax_type {
        elm::ElmSyntaxType::TypeConstruct(type_construct) => ElmSyntaxType::Construct {
            reference: elm_syntax_node_to_persistent(type_construct.reference, |reference| {
                elm::GeneratedNameQualification {
                    qualification: reference.qualification.to_string(),
                    name: reference.name.to_string(),
                }
            }),
            arguments: type_construct
                .arguments
                .into_iter()
                .map(elm_syntax_node_type_to_persistent)
                .collect::<Vec<_>>(),
        },
        elm::ElmSyntaxType::TypeFunction(type_function) => ElmSyntaxType::Function {
            input: elm_syntax_node_type_to_persistent_box(type_function.input),
            arrow_key_symbol_range: text_grid_range_to_lsp_range(
                type_function.arrow_key_symbol_range,
            ),
            output: elm_syntax_node_type_to_persistent_box(type_function.output),
        },
        elm::ElmSyntaxType::TypeParenthesized(in_parens) => {
            ElmSyntaxType::Parenthesized(elm_syntax_node_type_to_persistent_box(*in_parens))
        }
        elm::ElmSyntaxType::TypeRecord(fields) => ElmSyntaxType::Record(
            fields
                .into_iter()
                .map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_to_persistent(field.name),
                    colon_key_symbol_range: text_grid_range_to_lsp_range(
                        field.colon_key_symbol_range,
                    ),
                    value: elm_syntax_node_type_to_persistent(field.value),
                })
                .collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxType::TypeRecordExtension(type_record_extension) => {
            ElmSyntaxType::RecordExtension {
                record_variable: elm_syntax_node_string_to_persistent(
                    type_record_extension.record_variable,
                ),
                bar_key_symbol_range: text_grid_range_to_lsp_range(
                    type_record_extension.bar_key_symbol_range,
                ),
                field0: elm::GeneratedColonKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_to_persistent(type_record_extension.field0.name),
                    colon_key_symbol_range: text_grid_range_to_lsp_range(
                        type_record_extension.field0.colon_key_symbol_range,
                    ),
                    value: elm_syntax_node_type_to_persistent_box(
                        type_record_extension.field0.value,
                    ),
                },
                field1_up: type_record_extension
                    .field1_up
                    .into_iter()
                    .map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_to_persistent(field.name),
                        colon_key_symbol_range: text_grid_range_to_lsp_range(
                            field.colon_key_symbol_range,
                        ),
                        value: elm_syntax_node_type_to_persistent(field.value),
                    })
                    .collect::<Vec<_>>(),
            }
        }
        elm::ElmSyntaxType::TypeTriple(parts) => ElmSyntaxType::Triple {
            part0: elm_syntax_node_type_to_persistent_box(parts.part0),
            part1: elm_syntax_node_type_to_persistent_box(parts.part1),
            part2: elm_syntax_node_type_to_persistent_box(parts.part2),
        },
        elm::ElmSyntaxType::TypeTuple(parts) => ElmSyntaxType::Tuple {
            part0: elm_syntax_node_type_to_persistent_box(parts.part0),
            part1: elm_syntax_node_type_to_persistent_box(parts.part1),
        },
        elm::ElmSyntaxType::TypeUnit => ElmSyntaxType::Unit,
        elm::ElmSyntaxType::TypeVariable(name) => ElmSyntaxType::Variable(name.to_string()),
    }
}

fn elm_syntax_node_type_to_persistent(
    elm_syntax_node_type: elm::ElmSyntaxNode<elm::ElmSyntaxType>,
) -> ElmSyntaxNode<ElmSyntaxType> {
    elm_syntax_node_to_persistent(elm_syntax_node_type, elm_syntax_type_to_persistent)
}

fn elm_syntax_node_type_to_persistent_box(
    elm_syntax_node_type: elm::ElmSyntaxNode<elm::ElmSyntaxType>,
) -> ElmSyntaxNode<Box<ElmSyntaxType>> {
    elm_syntax_node_to_persistent(elm_syntax_node_type, |type_| {
        Box::new(elm_syntax_type_to_persistent(type_))
    })
}

fn elm_syntax_pattern_to_persistent(elm_syntax_pattern: elm::ElmSyntaxPattern) -> ElmSyntaxPattern {
    match elm_syntax_pattern {
        elm::ElmSyntaxPattern::PatternAs(as_pattern) => ElmSyntaxPattern::As {
            pattern: elm_syntax_node_pattern_to_persistent_box(as_pattern.pattern),
            as_keyword_range: text_grid_range_to_lsp_range(as_pattern.as_keyword_range),
            variable: elm_syntax_node_string_to_persistent(as_pattern.variable),
        },
        elm::ElmSyntaxPattern::PatternChar(char) => ElmSyntaxPattern::Char(char),
        elm::ElmSyntaxPattern::PatternIgnored => ElmSyntaxPattern::Ignored,
        elm::ElmSyntaxPattern::PatternInt(int_pattern) => ElmSyntaxPattern::Int {
            base: int_pattern.base,
            value: int_pattern.value,
        },
        elm::ElmSyntaxPattern::PatternListCons(list_cons_pattern) => ElmSyntaxPattern::ListCons {
            head: elm_syntax_node_pattern_to_persistent_box(list_cons_pattern.head),
            cons_key_symbol: text_grid_range_to_lsp_range(list_cons_pattern.cons_key_symbol_range),
            tail: elm_syntax_node_pattern_to_persistent_box(list_cons_pattern.tail),
        },
        elm::ElmSyntaxPattern::PatternListExact(elements) => ElmSyntaxPattern::ListExact(
            elements
                .into_iter()
                .map(elm_syntax_node_pattern_to_persistent)
                .collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxPattern::PatternParenthesized(in_parens) => {
            ElmSyntaxPattern::Parenthesized(elm_syntax_node_pattern_to_persistent_box(*in_parens))
        }
        elm::ElmSyntaxPattern::PatternRecord(fields) => ElmSyntaxPattern::Record(
            fields
                .into_iter()
                .map(elm_syntax_node_string_to_persistent)
                .collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxPattern::PatternString(string_pattern) => ElmSyntaxPattern::String {
            quoting_style: string_pattern.quoting_style,
            content: string_pattern.content.to_string(),
        },
        elm::ElmSyntaxPattern::PatternTriple(parts) => ElmSyntaxPattern::Triple {
            part0: elm_syntax_node_pattern_to_persistent_box(parts.part0),
            part1: elm_syntax_node_pattern_to_persistent_box(parts.part1),
            part2: elm_syntax_node_pattern_to_persistent_box(parts.part2),
        },
        elm::ElmSyntaxPattern::PatternTuple(parts) => ElmSyntaxPattern::Tuple {
            part0: elm_syntax_node_pattern_to_persistent_box(parts.part0),
            part1: elm_syntax_node_pattern_to_persistent_box(parts.part1),
        },
        elm::ElmSyntaxPattern::PatternUnit => ElmSyntaxPattern::Unit,
        elm::ElmSyntaxPattern::PatternVariable(name) => {
            ElmSyntaxPattern::Variable(name.to_string())
        }
        elm::ElmSyntaxPattern::PatternVariant(variant_pattern) => ElmSyntaxPattern::Variant {
            reference: elm_syntax_node_to_persistent(variant_pattern.reference, |reference| {
                elm::GeneratedNameQualification {
                    qualification: reference.qualification.to_string(),
                    name: reference.name.to_string(),
                }
            }),
            values: variant_pattern
                .values
                .into_iter()
                .map(elm_syntax_node_pattern_to_persistent)
                .collect::<Vec<_>>(),
        },
    }
}

fn elm_syntax_node_pattern_to_persistent(
    elm_syntax_node_pattern: elm::ElmSyntaxNode<elm::ElmSyntaxPattern>,
) -> ElmSyntaxNode<ElmSyntaxPattern> {
    elm_syntax_node_to_persistent(elm_syntax_node_pattern, elm_syntax_pattern_to_persistent)
}

fn elm_syntax_node_pattern_to_persistent_box(
    elm_syntax_node_pattern: elm::ElmSyntaxNode<elm::ElmSyntaxPattern>,
) -> ElmSyntaxNode<Box<ElmSyntaxPattern>> {
    elm_syntax_node_to_persistent(elm_syntax_node_pattern, |pattern| {
        Box::new(elm_syntax_pattern_to_persistent(pattern))
    })
}

fn elm_syntax_expression_to_persistent(
    elm_syntax_expression: elm::ElmSyntaxExpression,
) -> ElmSyntaxExpression {
    match elm_syntax_expression {
        elm::ElmSyntaxExpression::ExpressionCall(call) => ElmSyntaxExpression::Call {
            called: elm_syntax_node_expression_to_persistent_box(call.called),
            argument0: elm_syntax_node_expression_to_persistent_box(call.argument0),
            argument1_up: call
                .argument1_up
                .into_iter()
                .map(elm_syntax_node_expression_to_persistent)
                .collect::<Vec<_>>(),
        },
        elm::ElmSyntaxExpression::ExpressionCaseOf(case_of) => ElmSyntaxExpression::CaseOf {
            matched: elm_syntax_node_expression_to_persistent_box(case_of.matched),
            of_keyword_range: text_grid_range_to_lsp_range(case_of.of_keyword_range),
            case0: elm::GeneratedArrowKeySymbolRangePatternResult {
                pattern: elm_syntax_node_pattern_to_persistent(case_of.case0.pattern),
                arrow_key_symbol_range: text_grid_range_to_lsp_range(
                    case_of.case0.arrow_key_symbol_range,
                ),
                result: elm_syntax_node_expression_to_persistent_box(case_of.case0.result),
            },
            case1_up: case_of
                .case1_up
                .into_iter()
                .map(|case| elm::GeneratedArrowKeySymbolRangePatternResult {
                    pattern: elm_syntax_node_pattern_to_persistent(case.pattern),
                    arrow_key_symbol_range: text_grid_range_to_lsp_range(
                        case.arrow_key_symbol_range,
                    ),
                    result: elm_syntax_node_expression_to_persistent(case.result),
                })
                .collect::<Vec<_>>(),
        },
        elm::ElmSyntaxExpression::ExpressionChar(char) => ElmSyntaxExpression::Char(char),
        elm::ElmSyntaxExpression::ExpressionFloat(float) => ElmSyntaxExpression::Float(float),
        elm::ElmSyntaxExpression::ExpressionIfThenElse(if_then_else) => {
            ElmSyntaxExpression::IfThenElse {
                condition: elm_syntax_node_expression_to_persistent_box(if_then_else.condition),
                then_keyword_range: text_grid_range_to_lsp_range(if_then_else.then_keyword_range),
                on_true: elm_syntax_node_expression_to_persistent_box(if_then_else.on_true),
                else_keyword_range: text_grid_range_to_lsp_range(if_then_else.else_keyword_range),
                on_false: elm_syntax_node_expression_to_persistent_box(if_then_else.on_false),
            }
        }
        elm::ElmSyntaxExpression::ExpressionInfixOperation(infix_operation) => {
            ElmSyntaxExpression::InfixOperation {
                left: elm_syntax_node_expression_to_persistent_box(infix_operation.left),
                operator: elm_syntax_node_string_to_persistent(infix_operation.operator),
                right: elm_syntax_node_expression_to_persistent_box(infix_operation.right),
            }
        }
        elm::ElmSyntaxExpression::ExpressionInteger(integer_expression) => {
            ElmSyntaxExpression::Integer {
                base: integer_expression.base,
                value: integer_expression.value,
            }
        }
        elm::ElmSyntaxExpression::ExpressionLambda(lambda) => ElmSyntaxExpression::Lambda {
            parameter0: elm_syntax_node_pattern_to_persistent(lambda.parameter0),
            parameter1_up: lambda
                .parameter1_up
                .into_iter()
                .map(elm_syntax_node_pattern_to_persistent)
                .collect::<Vec<_>>(),
            arrow_key_symbol_range: text_grid_range_to_lsp_range(lambda.arrow_key_symbol_range),
            result: elm_syntax_node_expression_to_persistent_box(lambda.result),
        },
        elm::ElmSyntaxExpression::ExpressionLetIn(let_in) => ElmSyntaxExpression::LetIn {
            declaration0: elm_syntax_node_to_persistent(let_in.declaration0, |declaration| {
                Box::new(elm_syntax_let_declaration_to_persistent(declaration))
            }),
            declaration1_up: let_in
                .declaration1_up
                .into_iter()
                .map(|declaration_node| {
                    elm_syntax_node_to_persistent(
                        declaration_node,
                        elm_syntax_let_declaration_to_persistent,
                    )
                })
                .collect::<Vec<_>>(),
            in_keyword_range: text_grid_range_to_lsp_range(let_in.in_keyword_range),
            result: elm_syntax_node_expression_to_persistent_box(let_in.result),
        },
        elm::ElmSyntaxExpression::ExpressionList(elements) => ElmSyntaxExpression::List(
            elements
                .into_iter()
                .map(elm_syntax_node_expression_to_persistent)
                .collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxExpression::ExpressionNegation(in_negation) => ElmSyntaxExpression::Negation(
            elm_syntax_node_expression_to_persistent_box(*in_negation),
        ),
        elm::ElmSyntaxExpression::ExpressionOperatorFunction(symbol) => {
            ElmSyntaxExpression::OperatorFunction(symbol.to_string())
        }
        elm::ElmSyntaxExpression::ExpressionParenthesized(in_parens) => {
            ElmSyntaxExpression::Parenthesized(elm_syntax_node_expression_to_persistent_box(
                *in_parens,
            ))
        }
        elm::ElmSyntaxExpression::ExpressionRecord(fields) => ElmSyntaxExpression::Record(
            fields
                .into_iter()
                .map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_to_persistent(field.name),
                    equals_key_symbol_range: text_grid_range_to_lsp_range(
                        field.equals_key_symbol_range,
                    ),
                    value: elm_syntax_node_expression_to_persistent(field.value),
                })
                .collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxExpression::ExpressionRecordAccess(record_access) => {
            ElmSyntaxExpression::RecordAccess {
                record: elm_syntax_node_expression_to_persistent_box(record_access.record),
                field: elm_syntax_node_string_to_persistent(record_access.field),
            }
        }
        elm::ElmSyntaxExpression::ExpressionRecordAccessFunction(field) => {
            ElmSyntaxExpression::RecordAccessFunction(field.to_string())
        }
        elm::ElmSyntaxExpression::ExpressionRecordUpdate(record_update) => {
            ElmSyntaxExpression::RecordUpdate {
                record_variable: elm_syntax_node_string_to_persistent(
                    record_update.record_variable,
                ),
                bar_key_symbol_range: text_grid_range_to_lsp_range(
                    record_update.bar_key_symbol_range,
                ),
                field0: elm::GeneratedEqualsKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_to_persistent(record_update.field0.name),
                    equals_key_symbol_range: text_grid_range_to_lsp_range(
                        record_update.field0.equals_key_symbol_range,
                    ),
                    value: elm_syntax_node_expression_to_persistent_box(record_update.field0.value),
                },
                field1_up: record_update
                    .field1_up
                    .into_iter()
                    .map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_to_persistent(field.name),
                        equals_key_symbol_range: text_grid_range_to_lsp_range(
                            field.equals_key_symbol_range,
                        ),
                        value: elm_syntax_node_expression_to_persistent(field.value),
                    })
                    .collect::<Vec<_>>(),
            }
        }
        elm::ElmSyntaxExpression::ExpressionReference(reference) => {
            ElmSyntaxExpression::Reference(elm::GeneratedNameQualification {
                qualification: reference.qualification.to_string(),
                name: reference.name.to_string(),
            })
        }
        elm::ElmSyntaxExpression::ExpressionString(string_expression) => {
            ElmSyntaxExpression::String {
                quoting_style: string_expression.quoting_style,
                content: string_expression.content.to_string(),
            }
        }
        elm::ElmSyntaxExpression::ExpressionTriple(parts) => ElmSyntaxExpression::Triple {
            part0: elm_syntax_node_expression_to_persistent_box(parts.part0),
            part1: elm_syntax_node_expression_to_persistent_box(parts.part1),
            part2: elm_syntax_node_expression_to_persistent_box(parts.part2),
        },
        elm::ElmSyntaxExpression::ExpressionTuple(parts) => ElmSyntaxExpression::Tuple {
            part0: elm_syntax_node_expression_to_persistent_box(parts.part0),
            part1: elm_syntax_node_expression_to_persistent_box(parts.part1),
        },
        elm::ElmSyntaxExpression::ExpressionUnit => ElmSyntaxExpression::Unit,
    }
}

fn elm_syntax_node_expression_to_persistent(
    elm_syntax_node_expression: elm::ElmSyntaxNode<elm::ElmSyntaxExpression>,
) -> ElmSyntaxNode<ElmSyntaxExpression> {
    elm_syntax_node_to_persistent(
        elm_syntax_node_expression,
        elm_syntax_expression_to_persistent,
    )
}

fn elm_syntax_node_expression_to_persistent_box(
    elm_syntax_node_expression: elm::ElmSyntaxNode<elm::ElmSyntaxExpression>,
) -> ElmSyntaxNode<Box<ElmSyntaxExpression>> {
    elm_syntax_node_to_persistent(elm_syntax_node_expression, |expression| {
        Box::new(elm_syntax_expression_to_persistent(expression))
    })
}

fn elm_syntax_let_declaration_to_persistent(
    elm_syntax_let_declaration: elm::ElmSyntaxLetDeclaration,
) -> ElmSyntaxLetDeclaration {
    match elm_syntax_let_declaration {
        elm::ElmSyntaxLetDeclaration::LetDestructuring(destructuring) => {
            ElmSyntaxLetDeclaration::Destructuring {
                pattern: elm_syntax_node_pattern_to_persistent(destructuring.pattern),
                equals_key_symbol_range: text_grid_range_to_lsp_range(
                    destructuring.equals_key_symbol_range,
                ),
                expression: elm_syntax_node_expression_to_persistent(destructuring.expression),
            }
        }
        elm::ElmSyntaxLetDeclaration::LetValueOrFunctionDeclaration(variable_declaration) => {
            ElmSyntaxLetDeclaration::ValueOrFunctionDeclaration {
                name: variable_declaration.name.to_string(),
                signature: variable_declaration.signature.map(|signature| {
                    elm::GeneratedNameType0 {
                        name: elm_syntax_node_string_to_persistent(signature.name),
                        type_1: elm_syntax_node_type_to_persistent(signature.type_1),
                    }
                }),
                implementation_name_range: text_grid_range_to_lsp_range(
                    variable_declaration.implementation_name_range,
                ),
                parameters: variable_declaration
                    .parameters
                    .into_iter()
                    .map(elm_syntax_node_pattern_to_persistent)
                    .collect::<Vec<_>>(),
                equals_key_symbol_range: text_grid_range_to_lsp_range(
                    variable_declaration.equals_key_symbol_range,
                ),
                result: elm_syntax_node_expression_to_persistent(variable_declaration.result),
            }
        }
    }
}

fn elm_syntax_node_string_to_persistent(
    elm_syntax_node_string: elm::ElmSyntaxNode<elm::StringString>,
) -> ElmSyntaxNode<String> {
    ElmSyntaxNode {
        range: text_grid_range_to_lsp_range(elm_syntax_node_string.range),
        value: elm_syntax_node_string.value.to_string(),
    }
}

fn elm_syntax_node_to_persistent<Value, ValuePersistent>(
    elm_syntax_node: elm::ElmSyntaxNode<Value>,
    value_to_persistent: fn(Value) -> ValuePersistent,
) -> ElmSyntaxNode<ValuePersistent> {
    ElmSyntaxNode {
        range: text_grid_range_to_lsp_range(elm_syntax_node.range),
        value: value_to_persistent(elm_syntax_node.value),
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

fn elm_syntax_module_create_origin_lookup<'a>(
    state: &'a State,
    project_state: &'a ProjectState,
    elm_syntax_module: &'a ElmSyntaxModule,
) -> ModuleOriginLookup<'a> {
    let mut module_origin_lookup: ModuleOriginLookup = module_origin_lookup_for_implicit_imports();
    for documented_declaration in elm_syntax_module.declarations.iter() {
        match &documented_declaration.declaration.value {
            ElmSyntaxDeclaration::ChoiceType {
                name,
                parameters: _,
                equals_key_symbol_range: _,
                variant0: variant0,
                variant1_up: variant1_up,
            } => {
                module_origin_lookup.unqualified.insert(
                    &name.value,
                    &elm_syntax_module.header.value.module_name.value,
                );
                module_origin_lookup.unqualified.insert(
                    &variant0.name.value,
                    &elm_syntax_module.header.value.module_name.value,
                );
                for variant in variant1_up.iter() {
                    module_origin_lookup.unqualified.insert(
                        &variant.name.value,
                        &elm_syntax_module.header.value.module_name.value,
                    );
                }
            }
            ElmSyntaxDeclaration::Operator {
                direction: _,
                operator,
                function: _,
                precedence: _,
            } => {
                module_origin_lookup.unqualified.insert(
                    &operator.value,
                    &elm_syntax_module.header.value.module_name.value,
                );
            }
            ElmSyntaxDeclaration::Port { name, type_: _ } => {
                module_origin_lookup.unqualified.insert(
                    &name.value,
                    &elm_syntax_module.header.value.module_name.value,
                );
            }
            ElmSyntaxDeclaration::TypeAlias {
                alias_keyword_range: _,
                equals_key_symbol_range: _,
                name,
                parameters: _,
                type_: _,
            } => {
                module_origin_lookup.unqualified.insert(
                    &name.value,
                    &elm_syntax_module.header.value.module_name.value,
                );
            }
            ElmSyntaxDeclaration::ValueOrFunction {
                name,
                signature: _,
                implementation_name_range: _,
                parameters: _,
                equals_key_symbol_range: _,
                result: _,
            } => {
                module_origin_lookup
                    .unqualified
                    .insert(name, &elm_syntax_module.header.value.module_name.value);
            }
        }
    }
    for import_node in &elm_syntax_module.imports {
        let allowed_qualification: &str = match import_node.value.alias {
            None => &import_node.value.module_name.value,
            Some(ref import_alias) => &import_alias.name.value,
        };
        match module_origin_lookup
            .uniquely_qualified
            .remove(allowed_qualification)
        {
            Some(module_origin_for_existing_qualification) => {
                let module_origin_for_existing_qualification_maybe_syntax: Option<
                    &ElmSyntaxModule,
                > = project_state_get_module_with_name(
                    state,
                    project_state,
                    &module_origin_for_existing_qualification,
                )
                .and_then(|(_, imported_module_state)| imported_module_state.syntax.as_ref());
                for imported_module_expose in module_origin_for_existing_qualification_maybe_syntax
                    .map(elm_syntax_module_exposed_symbols)
                    .into_iter()
                    .flatten()
                {
                    module_origin_lookup.ambiguously_qualified.insert(
                        ElmQualified {
                            qualification: allowed_qualification,
                            name: imported_module_expose,
                        },
                        module_origin_for_existing_qualification,
                    );
                }
                let imported_module_maybe_syntax: Option<&ElmSyntaxModule> =
                    project_state_get_module_with_name(
                        state,
                        project_state,
                        &import_node.value.module_name.value,
                    )
                    .and_then(|(_, imported_module_state)| imported_module_state.syntax.as_ref());
                for imported_module_expose in imported_module_maybe_syntax
                    .map(elm_syntax_module_exposed_symbols)
                    .into_iter()
                    .flatten()
                {
                    module_origin_lookup.ambiguously_qualified.insert(
                        ElmQualified {
                            qualification: allowed_qualification,
                            name: imported_module_expose,
                        },
                        &import_node.value.module_name.value,
                    );
                }
            }
            None => {
                module_origin_lookup
                    .uniquely_qualified
                    .insert(allowed_qualification, &import_node.value.module_name.value);
            }
        }
        match import_node.value.exposing {
            None => {}
            Some(ref import_exposing) => match import_exposing.value {
                ElmSyntaxExposing::Explicit(ref exposes) => {
                    for expose_node in exposes {
                        match &expose_node.value {
                            ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                                name: choice_type_expose_name,
                                open_range: _,
                            } => {
                                module_origin_lookup.unqualified.insert(
                                    &choice_type_expose_name.value,
                                    &import_node.value.module_name.value,
                                );
                                let imported_module_maybe_syntax: Option<&ElmSyntaxModule> =
                                    project_state_get_module_with_name(
                                        state,
                                        project_state,
                                        &import_node.value.module_name.value,
                                    )
                                    .and_then(
                                        |(_, imported_module_state)| {
                                            imported_module_state.syntax.as_ref()
                                        },
                                    );
                                match imported_module_maybe_syntax {
                                    None => {}
                                    Some(imported_module_syntax) => {
                                        'until_origin_choice_type_declaration_found: for documented_declaration in
                                            imported_module_syntax.declarations.iter()
                                        {
                                            match &documented_declaration.declaration.value {
                                                ElmSyntaxDeclaration::ChoiceType {
                                                    name: imported_module_choice_type_name,
                                                    parameters: _,
                                                    equals_key_symbol_range: _,
                                                    variant0: imported_module_choice_type_variant0,
                                                    variant1_up:
                                                        imported_module_choice_type_variant1_up,
                                                } => {
                                                    if choice_type_expose_name
                                                        == imported_module_choice_type_name
                                                    {
                                                        module_origin_lookup.unqualified.insert(
                                                            &imported_module_choice_type_variant0
                                                                .name
                                                                .value,
                                                            &import_node.value.module_name.value,
                                                        );
                                                        for imported_module_choice_type_variant in
                                                            imported_module_choice_type_variant1_up
                                                        {
                                                            module_origin_lookup
                                                            .unqualified
                                                            .insert(
                                                                &imported_module_choice_type_variant.name.value,
                                                                &import_node.value.module_name.value,
                                                            );
                                                        }
                                                        break 'until_origin_choice_type_declaration_found;
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                            }
                            ElmSyntaxExpose::Operator(symbol) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(symbol, &import_node.value.module_name.value);
                            }
                            ElmSyntaxExpose::Type(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name, &import_node.value.module_name.value);
                            }
                            ElmSyntaxExpose::Variable(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name, &import_node.value.module_name.value);
                            }
                        }
                    }
                }
                ElmSyntaxExposing::All(_) => {
                    for import_exposed_symbol in
                        elm_syntax_module_exposed_symbols(elm_syntax_module)
                    {
                        module_origin_lookup
                            .unqualified
                            .insert(import_exposed_symbol, &import_node.value.module_name.value);
                    }
                }
            },
        }
    }
    module_origin_lookup
}

fn elm_syntax_module_exposed_symbols<'a>(elm_syntax_module: &'a ElmSyntaxModule) -> Vec<&'a str> {
    let mut exposed_symbols: Vec<&str> = Vec::new();
    match elm_syntax_module.header.value.exposing.value {
        ElmSyntaxExposing::All(_) => {
            for documented_declaration in elm_syntax_module.declarations.iter() {
                match &documented_declaration.declaration.value {
                    ElmSyntaxDeclaration::ChoiceType {
                        name: exposed_choice_type_name,
                        parameters: _,
                        equals_key_symbol_range: _,
                        variant0: exposed_choice_type_variant0,
                        variant1_up: exposed_choice_type_variant1_up,
                    } => {
                        exposed_symbols.push(&exposed_choice_type_name.value);
                        exposed_symbols.push(&exposed_choice_type_variant0.name.value);
                        for exposed_choice_type_variant in exposed_choice_type_variant1_up {
                            exposed_symbols.push(&exposed_choice_type_variant.name.value);
                        }
                    }
                    ElmSyntaxDeclaration::Port {
                        name: exposed_port_name,
                        type_: _,
                    } => {
                        exposed_symbols.push(&exposed_port_name.value);
                    }
                    ElmSyntaxDeclaration::TypeAlias {
                        alias_keyword_range: _,
                        name: exposed_type_alias_name,
                        parameters: _,
                        equals_key_symbol_range: _,
                        type_: _,
                    } => {
                        exposed_symbols.push(&exposed_type_alias_name.value);
                    }
                    ElmSyntaxDeclaration::Operator {
                        direction: _,
                        operator: exposed_operator,
                        function: _,
                        precedence: _,
                    } => {
                        exposed_symbols.push(&exposed_operator.value);
                    }
                    ElmSyntaxDeclaration::ValueOrFunction {
                        name: exposed_variable_name,
                        signature: _,
                        implementation_name_range: _,
                        parameters: _,
                        equals_key_symbol_range: _,
                        result: _,
                    } => {
                        exposed_symbols.push(exposed_variable_name);
                    }
                }
            }
        }
        ElmSyntaxExposing::Explicit(ref exposes) => {
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                        name: choice_type_expose_name,
                        open_range: _,
                    } => {
                        exposed_symbols.push(&choice_type_expose_name.value);
                        'until_origin_choice_type_declaration_found: for declaration in
                            elm_syntax_module.declarations.iter()
                        {
                            match &declaration.declaration.value {
                                ElmSyntaxDeclaration::ChoiceType {
                                    name: exposed_choice_type_name,
                                    parameters: _,
                                    equals_key_symbol_range: _,
                                    variant0: exposed_choice_type_variant0,
                                    variant1_up: exposed_choice_type_variant1_up,
                                } => {
                                    if &choice_type_expose_name.value
                                        == &exposed_choice_type_name.value
                                    {
                                        exposed_symbols
                                            .push(&exposed_choice_type_variant0.name.value);
                                        for exposed_choice_type_variant in
                                            exposed_choice_type_variant1_up
                                        {
                                            exposed_symbols
                                                .push(&exposed_choice_type_variant.name.value);
                                        }
                                        break 'until_origin_choice_type_declaration_found;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    ElmSyntaxExpose::Operator(symbol) => {
                        exposed_symbols.push(symbol);
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
            so_far.push_str(look_up_origin_module(
                module_origin_lookup,
                &reference.value.qualification,
                &reference.value.name,
            ));
            so_far.push('.');
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
            output,
        } => {
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &input.value);
            so_far.push_str(" -> ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &output.value)
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
                    elm_syntax_type_to_single_line_string_into(
                        so_far,
                        module_origin_lookup,
                        &field0.value.value,
                    );
                    for field in fields_iterator {
                        so_far.push_str(", ");
                        so_far.push_str(&field.name.value);
                        so_far.push_str(" : ");
                        elm_syntax_type_to_single_line_string_into(
                            so_far,
                            module_origin_lookup,
                            &field.value.value,
                        );
                    }
                    so_far.push_str(" }")
                }
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable,
            bar_key_symbol_range: _,
            field0,
            field1_up,
        } => {
            so_far.push_str("{ ");
            so_far.push_str(&record_variable.value);
            so_far.push_str(" | ");
            so_far.push_str(&field0.name.value);
            so_far.push_str(" : ");
            elm_syntax_type_to_single_line_string_into(
                so_far,
                module_origin_lookup,
                &field0.value.value,
            );
            for field in field1_up {
                so_far.push_str(", ");
                so_far.push_str(&field.name.value);
                so_far.push_str(" : ");
                elm_syntax_type_to_single_line_string_into(
                    so_far,
                    module_origin_lookup,
                    &field.value.value,
                );
            }
            so_far.push_str(" }")
        }
        ElmSyntaxType::Triple {
            part0,
            part1,
            part2,
        } => {
            so_far.push_str("( ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part0.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part1.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part2.value);
            so_far.push_str(" )");
        }
        ElmSyntaxType::Tuple { part0, part1 } => {
            so_far.push_str("( ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part0.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part1.value);
            so_far.push_str(" )");
        }
        ElmSyntaxType::Unit => so_far.push_str("()"),
        ElmSyntaxType::Variable(name) => so_far.push_str(name),
    }
}

// //
#[derive(Clone, Copy, Debug)]
enum ElmSyntaxSymbol<'a> {
    ModuleName(&'a str),
    ImportAlias {
        module_origin: &'a str,
        alias_name: &'a str,
    },
    VariableOrVariantOrOperator {
        module_origin: &'a str,
        name: &'a str,
    },
    Type {
        module_origin: &'a str,
        name: &'a str,
    },
    TypeVariable {
        scope_declaration: &'a ElmSyntaxDeclaration,
        name: &'a str,
    },
    LocalBinding {
        scope_expression: ElmSyntaxNode<&'a ElmSyntaxExpression>,
        name: &'a str,
        origin: LocalBindingOrigin<'a>,
    },
}

fn elm_syntax_module_find_reference_at_position<'a>(
    state: &'a State,
    project_state: &'a ProjectState,
    elm_syntax_module: &'a ElmSyntaxModule,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if lsp_range_includes_position(elm_syntax_module.header.value.module_name.range, position) {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ModuleName(&elm_syntax_module.header.value.module_name.value),
            range: elm_syntax_module.header.value.module_name.range,
        })
    } else {
        elm_syntax_exposing_from_module_find_reference_at_position(
            elm_syntax_node_as_ref(&elm_syntax_module.header.value.exposing),
            &elm_syntax_module.header.value.module_name.value,
            position,
        )
        .or_else(|| {
            elm_syntax_module.imports.iter().find_map(|import_node| {
                elm_syntax_import_find_reference_at_position(
                    elm_syntax_node_as_ref(import_node),
                    position,
                )
            })
        })
        .or_else(|| {
            let module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(state, project_state, elm_syntax_module);
            elm_syntax_module
                .declarations
                .iter()
                .find_map(move |documented_declaration| {
                    elm_syntax_declaration_find_reference_at_position(
                        &elm_syntax_module.header.value.module_name.value,
                        &module_origin_lookup,
                        elm_syntax_node_as_ref(&documented_declaration.declaration),
                        position,
                    )
                })
        })
    }
}

fn elm_syntax_import_find_reference_at_position<'a>(
    elm_syntax_import_node: ElmSyntaxNode<&'a ElmSyntaxImport>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_import_node.range, position) {
        None
    } else if lsp_range_includes_position(elm_syntax_import_node.value.module_name.range, position)
    {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ModuleName(&elm_syntax_import_node.value.module_name.value),
            range: elm_syntax_import_node.value.module_name.range,
        })
    } else if let Some(ref import_alias) = elm_syntax_import_node.value.alias
        && lsp_range_includes_position(import_alias.name.range, position)
    {
        Some(ElmSyntaxNode {
            value: ElmSyntaxSymbol::ImportAlias {
                module_origin: &elm_syntax_import_node.value.module_name.value,
                alias_name: &import_alias.name.value,
            },
            range: elm_syntax_import_node.value.module_name.range,
        })
    } else {
        elm_syntax_import_node
            .value
            .exposing
            .as_ref()
            .and_then(|exposing_node| {
                elm_syntax_exposing_from_module_find_reference_at_position(
                    elm_syntax_node_as_ref(exposing_node),
                    &elm_syntax_import_node.value.module_name.value,
                    position,
                )
            })
    }
}

fn elm_syntax_exposing_from_module_find_reference_at_position<'a>(
    elm_syntax_exposing_node: ElmSyntaxNode<&'a ElmSyntaxExposing>,
    module_name: &'a str,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_exposing_node.range, position) {
        None
    } else {
        match elm_syntax_exposing_node.value {
            ElmSyntaxExposing::All(_) => None,
            ElmSyntaxExposing::Explicit(exposes) => exposes.iter().find_map(|expose_node| {
                if lsp_range_includes_position(expose_node.range, position) {
                    Some(match &expose_node.value {
                        ElmSyntaxExpose::ChoiceTypeIncludingVariants {
                            name,
                            open_range: _,
                        } => ElmSyntaxNode {
                            value: ElmSyntaxSymbol::Type {
                                module_origin: module_name,
                                name: &name.value,
                            },
                            range: expose_node.range,
                        },
                        ElmSyntaxExpose::Operator(symbol) => ElmSyntaxNode {
                            value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                                module_origin: module_name,
                                name: symbol,
                            },
                            range: lsp_types::Range {
                                start: lsp_position_add_characters(expose_node.range.start, 1),
                                end: lsp_position_add_characters(expose_node.range.end, -1),
                            },
                        },
                        ElmSyntaxExpose::Type(name) => ElmSyntaxNode {
                            value: ElmSyntaxSymbol::Type {
                                module_origin: module_name,
                                name: name,
                            },
                            range: expose_node.range,
                        },
                        ElmSyntaxExpose::Variable(name) => ElmSyntaxNode {
                            value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                                module_origin: module_name,
                                name: name,
                            },
                            range: expose_node.range,
                        },
                    })
                } else {
                    None
                }
            }),
        }
    }
}

fn elm_syntax_declaration_find_reference_at_position<'a>(
    origin_module: &'a str,
    module_origin_lookup: &ModuleOriginLookup<'a>,
    elm_syntax_declaration_node: ElmSyntaxNode<&'a ElmSyntaxDeclaration>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_declaration_node.range, position) {
        None
    } else {
        match elm_syntax_declaration_node.value {
            ElmSyntaxDeclaration::ChoiceType {
                name: name,
                parameters: parameters,
                equals_key_symbol_range: _,
                variant0,
                variant1_up,
            } => {
                if lsp_range_includes_position(name.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::Type {
                            module_origin: origin_module,
                            name: &name.value,
                        },
                        range: name.range,
                    })
                } else if lsp_range_includes_position(variant0.name.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: origin_module,
                            name: &variant0.name.value,
                        },
                        range: variant0.name.range,
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
                            variant0.values.iter().find_map(|variant_value| {
                                elm_syntax_type_find_reference_at_position(
                                    module_origin_lookup,
                                    elm_syntax_declaration_node.value,
                                    elm_syntax_node_as_ref(variant_value),
                                    position,
                                )
                            })
                        })
                        .or_else(|| {
                            variant1_up.iter().find_map(|variant| {
                                if lsp_range_includes_position(variant.name.range, position) {
                                    Some(ElmSyntaxNode {
                                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                                            module_origin: origin_module,
                                            name: &variant.name.value,
                                        },
                                        range: variant.name.range,
                                    })
                                } else {
                                    variant.values.iter().find_map(|variant_value| {
                                        elm_syntax_type_find_reference_at_position(
                                            module_origin_lookup,
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
                operator: operator,
                function,
                precedence: _,
            } => {
                if lsp_range_includes_position(operator.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: origin_module,
                            name: &operator.value,
                        },
                        range: operator.range,
                    })
                } else if lsp_range_includes_position(function.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: origin_module,
                            name: &function.value,
                        },
                        range: function.range,
                    })
                } else {
                    None
                }
            }
            ElmSyntaxDeclaration::Port { name: name, type_ } => {
                if lsp_range_includes_position(name.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: origin_module,
                            name: &name.value,
                        },
                        range: name.range,
                    })
                } else {
                    elm_syntax_type_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_declaration_node.value,
                        elm_syntax_node_as_ref(type_),
                        position,
                    )
                }
            }
            ElmSyntaxDeclaration::TypeAlias {
                alias_keyword_range: _,
                name: name,
                parameters,
                equals_key_symbol_range: _,
                type_,
            } => {
                if lsp_range_includes_position(name.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::Type {
                            module_origin: origin_module,
                            name: &name.value,
                        },
                        range: name.range,
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
                            elm_syntax_type_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_declaration_node.value,
                                elm_syntax_node_as_ref(type_),
                                position,
                            )
                        })
                }
            }
            ElmSyntaxDeclaration::ValueOrFunction {
                name,
                signature: maybe_signature,
                implementation_name_range,
                parameters,
                equals_key_symbol_range: _,
                result,
            } => {
                if lsp_range_includes_position(*implementation_name_range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: origin_module,
                            name: name,
                        },
                        range: *implementation_name_range,
                    })
                } else {
                    maybe_signature
                        .as_ref()
                        .and_then(|signature| {
                            if lsp_range_includes_position(signature.name.range, position) {
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                                        module_origin: origin_module,
                                        name: name,
                                    },
                                    range: signature.name.range,
                                })
                            } else {
                                elm_syntax_type_find_reference_at_position(
                                    module_origin_lookup,
                                    elm_syntax_declaration_node.value,
                                    elm_syntax_node_as_ref(&signature.type_1),
                                    position,
                                )
                            }
                        })
                        .or_else(|| {
                            let mut parameter_introduced_bindings: Vec<LocalBinding> = Vec::new();
                            for parameter_node in parameters {
                                elm_syntax_pattern_bindings_into(
                                    &mut parameter_introduced_bindings,
                                    elm_syntax_node_as_ref(parameter_node),
                                );
                            }
                            elm_syntax_expression_find_reference_at_position(
                                module_origin_lookup,
                                &[(
                                    elm_syntax_node_as_ref(result),
                                    &parameter_introduced_bindings,
                                )],
                                elm_syntax_declaration_node.value,
                                elm_syntax_node_as_ref(result),
                                position,
                            )
                        })
                        .or_else(|| {
                            parameters.iter().find_map(|parameter| {
                                elm_syntax_pattern_find_reference_at_position(
                                    module_origin_lookup,
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
    module_origin_lookup: &ModuleOriginLookup<'a>,
    elm_syntax_pattern_node: ElmSyntaxNode<&'a ElmSyntaxPattern>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern,
            as_keyword_range: _,
            variable: _,
        } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(pattern),
            position,
        ),
        ElmSyntaxPattern::Char(_) => None,
        ElmSyntaxPattern::Ignored => None,
        ElmSyntaxPattern::Int { .. } => None,
        ElmSyntaxPattern::ListCons {
            head,
            cons_key_symbol: _,
            tail,
        } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(head),
            position,
        )
        .or_else(|| {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(tail),
                position,
            )
        }),
        ElmSyntaxPattern::ListExact(elements) => elements.iter().find_map(|element| {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_as_ref(element),
                position,
            )
        }),
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                position,
            )
        }
        ElmSyntaxPattern::Record(_) => None,
        ElmSyntaxPattern::String { .. } => None,
        ElmSyntaxPattern::Triple {
            part0,
            part1,
            part2,
        } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(part0),
            position,
        )
        .or_else(|| {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                position,
            )
        })
        .or_else(|| {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part2),
                position,
            )
        }),
        ElmSyntaxPattern::Tuple { part0, part1 } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(part0),
            position,
        )
        .or_else(|| {
            elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                position,
            )
        }),
        ElmSyntaxPattern::Unit => None,
        ElmSyntaxPattern::Variable(_) => None,
        ElmSyntaxPattern::Variant { reference, values } => {
            if lsp_range_includes_position(reference.range, position) {
                Some(ElmSyntaxNode {
                    value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                        module_origin: look_up_origin_module(
                            module_origin_lookup,
                            &reference.value.qualification,
                            &reference.value.name,
                        ),
                        name: &reference.value.name,
                    },
                    range: reference.range,
                })
            } else {
                values.iter().find_map(|value| {
                    elm_syntax_pattern_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(value),
                        position,
                    )
                })
            }
        }
    }
}

fn elm_syntax_type_find_reference_at_position<'a>(
    module_origin_lookup: &ModuleOriginLookup<'a>,
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
                            module_origin: look_up_origin_module(
                                module_origin_lookup,
                                &reference.value.qualification,
                                &reference.value.name,
                            ),
                            name: &reference.value.name,
                        },
                        range: reference.range,
                    })
                } else {
                    arguments.iter().find_map(|argument| {
                        elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
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
                output,
            } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                scope_declaration,
                elm_syntax_node_unbox(input),
                position,
            )
            .or_else(|| {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    scope_declaration,
                    elm_syntax_node_unbox(output),
                    position,
                )
            }),
            ElmSyntaxType::Parenthesized(in_parens) => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                scope_declaration,
                elm_syntax_node_unbox(in_parens),
                position,
            ),
            ElmSyntaxType::Record(fields) => fields.iter().find_map(|field| {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    scope_declaration,
                    elm_syntax_node_as_ref(&field.value),
                    position,
                )
            }),
            ElmSyntaxType::RecordExtension {
                record_variable: record_type_variable_node,
                bar_key_symbol_range: _,
                field0,
                field1_up,
            } => {
                if lsp_range_includes_position(record_type_variable_node.range, position) {
                    Some(ElmSyntaxNode {
                        range: record_type_variable_node.range,
                        value: ElmSyntaxSymbol::TypeVariable {
                            scope_declaration: scope_declaration,
                            name: &record_type_variable_node.value,
                        },
                    })
                } else {
                    elm_syntax_type_find_reference_at_position(
                        module_origin_lookup,
                        scope_declaration,
                        elm_syntax_node_unbox(&field0.value),
                        position,
                    )
                    .or_else(|| {
                        field1_up.iter().find_map(|field| {
                            elm_syntax_type_find_reference_at_position(
                                module_origin_lookup,
                                scope_declaration,
                                elm_syntax_node_as_ref(&field.value),
                                position,
                            )
                        })
                    })
                }
            }
            ElmSyntaxType::Triple {
                part0,
                part1,
                part2,
            } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                scope_declaration,
                elm_syntax_node_unbox(part0),
                position,
            )
            .or_else(|| {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    scope_declaration,
                    elm_syntax_node_unbox(part1),
                    position,
                )
            })
            .or_else(|| {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    scope_declaration,
                    elm_syntax_node_unbox(part2),
                    position,
                )
            }),
            ElmSyntaxType::Tuple { part0, part1 } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                scope_declaration,
                elm_syntax_node_unbox(part0),
                position,
            )
            .or_else(|| {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    scope_declaration,
                    elm_syntax_node_unbox(part1),
                    position,
                )
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
    PatternVariable(lsp_types::Range),
    PatternRecordField(lsp_types::Range),
    LetDeclaredVariable {
        signature: Option<
            &'a elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>,
        >,
        implementation_name_range: lsp_types::Range,
    },
}
#[derive(Clone, Copy)]
struct LocalBinding<'a> {
    name: &'a str,
    origin: LocalBindingOrigin<'a>,
}

fn elm_syntax_expression_find_reference_at_position<'a>(
    module_origin_lookup: &ModuleOriginLookup<'a>,
    local_bindings: &[(ElmSyntaxNode<&'a ElmSyntaxExpression>, &[LocalBinding<'a>])],
    scope_declaration: &'a ElmSyntaxDeclaration,
    elm_syntax_expression_node: ElmSyntaxNode<&'a ElmSyntaxExpression>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_expression_node.range, position) {
        None
    } else {
        match elm_syntax_expression_node.value {
            ElmSyntaxExpression::Call {
                called,
                argument0,
                argument1_up,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(called),
                position,
            )
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(argument0),
                    position,
                )
            })
            .or_else(|| {
                argument1_up.iter().find_map(|argument| {
                    elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_as_ref(argument),
                        position,
                    )
                })
            }),
            ElmSyntaxExpression::CaseOf {
                matched,
                of_keyword_range: _,
                case0,
                case1_up,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(matched),
                position,
            )
            .or_else(|| {
                elm_syntax_pattern_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&case0.pattern),
                    position,
                )
            })
            .or_else(|| {
                let mut introduced_bindings = Vec::new();
                elm_syntax_pattern_bindings_into(
                    &mut introduced_bindings,
                    elm_syntax_node_as_ref(&case0.pattern),
                );
                let mut local_bindings_including_from_case0_pattern = local_bindings.to_vec();
                local_bindings_including_from_case0_pattern
                    .push((elm_syntax_node_unbox(&case0.result), &introduced_bindings));
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    &local_bindings_including_from_case0_pattern,
                    scope_declaration,
                    elm_syntax_node_unbox(&case0.result),
                    position,
                )
            })
            .or_else(|| {
                case1_up.iter().find_map(|case| {
                    elm_syntax_pattern_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&case.pattern),
                        position,
                    )
                    .or_else(|| {
                        let mut introduced_bindings = Vec::new();
                        elm_syntax_pattern_bindings_into(
                            &mut introduced_bindings,
                            elm_syntax_node_as_ref(&case.pattern),
                        );
                        let mut local_bindings_including_from_case_pattern =
                            local_bindings.to_vec();
                        local_bindings_including_from_case_pattern
                            .push((elm_syntax_node_as_ref(&case.result), &introduced_bindings));
                        elm_syntax_expression_find_reference_at_position(
                            module_origin_lookup,
                            &local_bindings_including_from_case_pattern,
                            scope_declaration,
                            elm_syntax_node_as_ref(&case.result),
                            position,
                        )
                    })
                })
            }),
            ElmSyntaxExpression::Char(_) => None,
            ElmSyntaxExpression::Float(_) => None,
            ElmSyntaxExpression::IfThenElse {
                condition,
                then_keyword_range: _,
                on_true,
                else_keyword_range: _,
                on_false,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(condition),
                position,
            )
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(on_true),
                    position,
                )
            })
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(on_false),
                    position,
                )
            }),
            ElmSyntaxExpression::InfixOperation {
                left,
                operator,
                right,
            } => {
                if lsp_range_includes_position(operator.range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: look_up_origin_module(
                                module_origin_lookup,
                                "",
                                &operator.value,
                            ),
                            name: &operator.value,
                        },
                        range: operator.range,
                    })
                } else {
                    elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_unbox(left),
                        position,
                    )
                    .or_else(|| {
                        elm_syntax_expression_find_reference_at_position(
                            module_origin_lookup,
                            local_bindings,
                            scope_declaration,
                            elm_syntax_node_unbox(right),
                            position,
                        )
                    })
                }
            }
            ElmSyntaxExpression::Integer { .. } => None,
            ElmSyntaxExpression::Lambda {
                arrow_key_symbol_range: _,
                parameter0,
                parameter1_up,
                result,
            } => elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_as_ref(parameter0),
                position,
            )
            .or_else(|| {
                parameter1_up.iter().find_map(|parameter| {
                    elm_syntax_pattern_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(parameter),
                        position,
                    )
                })
            })
            .or_else(|| {
                let mut introduced_bindings = Vec::new();
                elm_syntax_pattern_bindings_into(
                    &mut introduced_bindings,
                    elm_syntax_node_as_ref(parameter0),
                );
                for parameter_node in parameter1_up {
                    elm_syntax_pattern_bindings_into(
                        &mut introduced_bindings,
                        elm_syntax_node_as_ref(parameter_node),
                    );
                }
                let mut local_bindings_including_from_lambda_parameters = local_bindings.to_vec();
                local_bindings_including_from_lambda_parameters
                    .push((elm_syntax_node_unbox(result), &introduced_bindings));
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    &local_bindings_including_from_lambda_parameters,
                    scope_declaration,
                    elm_syntax_node_unbox(result),
                    position,
                )
            }),
            ElmSyntaxExpression::LetIn {
                declaration0,
                declaration1_up,
                in_keyword_range: _,
                result,
            } => {
                let mut introduced_bindings = Vec::new();
                elm_syntax_let_declaration_introduced_bindings_into(
                    &mut introduced_bindings,
                    &declaration0.value,
                );
                for let_declaration_node in declaration1_up {
                    elm_syntax_let_declaration_introduced_bindings_into(
                        &mut introduced_bindings,
                        &let_declaration_node.value,
                    );
                }
                let mut local_bindings_including_let_declaration_introduced =
                    local_bindings.to_vec();
                local_bindings_including_let_declaration_introduced
                    .push((elm_syntax_expression_node, &introduced_bindings));
                elm_syntax_let_declaration_find_reference_at_position(
                    module_origin_lookup,
                    &local_bindings_including_let_declaration_introduced,
                    scope_declaration,
                    elm_syntax_expression_node,
                    elm_syntax_node_unbox(declaration0),
                    position,
                )
                .or_else(|| {
                    declaration1_up.iter().find_map(|declaration| {
                        elm_syntax_let_declaration_find_reference_at_position(
                            module_origin_lookup,
                            &local_bindings_including_let_declaration_introduced,
                            scope_declaration,
                            elm_syntax_expression_node,
                            elm_syntax_node_as_ref(declaration),
                            position,
                        )
                    })
                })
                .or_else(|| {
                    elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        &local_bindings_including_let_declaration_introduced,
                        scope_declaration,
                        elm_syntax_node_unbox(result),
                        position,
                    )
                })
            }
            ElmSyntaxExpression::List(elements) => elements.iter().find_map(|element| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_as_ref(element),
                    position,
                )
            }),
            ElmSyntaxExpression::Negation(in_negation) => {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(in_negation),
                    position,
                )
            }
            ElmSyntaxExpression::OperatorFunction(operator) => Some(ElmSyntaxNode {
                value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                    module_origin: look_up_origin_module(module_origin_lookup, "", operator),
                    name: operator,
                },
                range: elm_syntax_expression_node.range,
            }),
            ElmSyntaxExpression::Parenthesized(in_parens) => {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(in_parens),
                    position,
                )
            }
            ElmSyntaxExpression::Record(fields) => fields.iter().find_map(|field| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_as_ref(&field.value),
                    position,
                )
            }),
            ElmSyntaxExpression::RecordAccess { record, field: _ } => {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(record),
                    position,
                )
            }
            ElmSyntaxExpression::RecordAccessFunction(_) => None,
            ElmSyntaxExpression::RecordUpdate {
                record_variable: record_variable_node,
                bar_key_symbol_range: _,
                field0,
                field1_up,
            } => {
                if lsp_range_includes_position(record_variable_node.range, position) {
                    if let Some((scope_expression, origin)) =
                        local_bindings
                            .iter()
                            .find_map(|(scope_expression, local_bindings)| {
                                local_bindings.iter().find_map(|local_binding| {
                                    if local_binding.name == &record_variable_node.value {
                                        Some((scope_expression, local_binding.origin))
                                    } else {
                                        None
                                    }
                                })
                            })
                    {
                        Some(ElmSyntaxNode {
                            value: ElmSyntaxSymbol::LocalBinding {
                                scope_expression: *scope_expression,
                                origin: origin,
                                name: &record_variable_node.value,
                            },
                            range: record_variable_node.range,
                        })
                    } else {
                        Some(ElmSyntaxNode {
                            value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                                module_origin: look_up_origin_module(
                                    module_origin_lookup,
                                    "",
                                    &record_variable_node.value,
                                ),
                                name: &record_variable_node.value,
                            },
                            range: record_variable_node.range,
                        })
                    }
                } else {
                    elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_unbox(&field0.value),
                        position,
                    )
                    .or_else(|| {
                        field1_up.iter().find_map(|field| {
                            elm_syntax_expression_find_reference_at_position(
                                module_origin_lookup,
                                local_bindings,
                                scope_declaration,
                                elm_syntax_node_as_ref(&field.value),
                                position,
                            )
                        })
                    })
                }
            }
            ElmSyntaxExpression::Reference(reference) => {
                if reference.qualification.is_empty()
                    && let Some((scope_expression, origin)) =
                        local_bindings
                            .iter()
                            .find_map(|(scope_expression, local_bindings)| {
                                local_bindings.iter().find_map(|local_binding| {
                                    if local_binding.name == &reference.name {
                                        Some((scope_expression, local_binding.origin))
                                    } else {
                                        None
                                    }
                                })
                            })
                {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::LocalBinding {
                            scope_expression: *scope_expression,
                            origin: origin,
                            name: &reference.name,
                        },
                        range: elm_syntax_expression_node.range,
                    })
                } else {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::VariableOrVariantOrOperator {
                            module_origin: look_up_origin_module(
                                module_origin_lookup,
                                &reference.qualification,
                                &reference.name,
                            ),
                            name: &reference.name,
                        },
                        range: elm_syntax_expression_node.range,
                    })
                }
            }
            ElmSyntaxExpression::String { .. } => None,
            ElmSyntaxExpression::Triple {
                part0,
                part1,
                part2,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                local_bindings,
                scope_declaration,
                elm_syntax_node_unbox(part0),
                position,
            )
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part1),
                    position,
                )
            })
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part2),
                    position,
                )
            }),
            ElmSyntaxExpression::Tuple { part0, part1 } => {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_unbox(part0),
                    position,
                )
                .or_else(|| {
                    elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        local_bindings,
                        scope_declaration,
                        elm_syntax_node_unbox(part1),
                        position,
                    )
                })
            }
            ElmSyntaxExpression::Unit => None,
        }
    }
}

fn elm_syntax_let_declaration_find_reference_at_position<'a>(
    module_origin_lookup: &ModuleOriginLookup<'a>,
    local_bindings: &[(ElmSyntaxNode<&'a ElmSyntaxExpression>, &[LocalBinding<'a>])],
    scope_declaration: &'a ElmSyntaxDeclaration,
    scope_expression: ElmSyntaxNode<&'a ElmSyntaxExpression>,
    elm_syntax_let_declaration_node: ElmSyntaxNode<&'a ElmSyntaxLetDeclaration>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSyntaxSymbol<'a>>> {
    if !lsp_range_includes_position(elm_syntax_let_declaration_node.range, position) {
        None
    } else {
        match elm_syntax_let_declaration_node.value {
            ElmSyntaxLetDeclaration::Destructuring {
                pattern,
                equals_key_symbol_range: _,
                expression,
            } => elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_as_ref(pattern),
                position,
            )
            .or_else(|| {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    local_bindings,
                    scope_declaration,
                    elm_syntax_node_as_ref(expression),
                    position,
                )
            }),
            ElmSyntaxLetDeclaration::ValueOrFunctionDeclaration {
                name,
                signature: maybe_signature,
                implementation_name_range,
                parameters,
                equals_key_symbol_range: _,
                result,
            } => {
                if lsp_range_includes_position(*implementation_name_range, position) {
                    Some(ElmSyntaxNode {
                        value: ElmSyntaxSymbol::LocalBinding {
                            scope_expression: scope_expression,
                            origin: LocalBindingOrigin::LetDeclaredVariable {
                                signature: maybe_signature.as_ref(),
                                implementation_name_range: *implementation_name_range,
                            },
                            name: name,
                        },
                        range: *implementation_name_range,
                    })
                } else {
                    maybe_signature
                        .as_ref()
                        .and_then(|signature| {
                            if lsp_range_includes_position(signature.name.range, position) {
                                Some(ElmSyntaxNode {
                                    value: ElmSyntaxSymbol::LocalBinding {
                                        scope_expression: scope_expression,
                                        origin: LocalBindingOrigin::LetDeclaredVariable {
                                            signature: maybe_signature.as_ref(),
                                            implementation_name_range: *implementation_name_range,
                                        },
                                        name: name,
                                    },
                                    range: signature.name.range,
                                })
                            } else {
                                elm_syntax_type_find_reference_at_position(
                                    module_origin_lookup,
                                    scope_declaration,
                                    elm_syntax_node_as_ref(&signature.type_1),
                                    position,
                                )
                            }
                        })
                        .or_else(|| {
                            let mut introduced_bindings = Vec::new();
                            for parameter_node in parameters {
                                elm_syntax_pattern_bindings_into(
                                    &mut introduced_bindings,
                                    elm_syntax_node_as_ref(parameter_node),
                                );
                            }
                            let mut local_bindings_including_from_let_function_parameters =
                                local_bindings.to_vec();
                            local_bindings_including_from_let_function_parameters
                                .push((elm_syntax_node_as_ref(result), &introduced_bindings));
                            elm_syntax_expression_find_reference_at_position(
                                module_origin_lookup,
                                &local_bindings_including_from_let_function_parameters,
                                scope_declaration,
                                elm_syntax_node_as_ref(result),
                                position,
                            )
                        })
                        .or_else(|| {
                            parameters.iter().find_map(|parameter| {
                                elm_syntax_pattern_find_reference_at_position(
                                    module_origin_lookup,
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
    if symbol_to_collect_uses_of
        == (ElmDeclaredSymbol::ModuleName(&elm_syntax_module.header.value.module_name.value))
    {
        uses_so_far.push(elm_syntax_module.header.value.module_name.range);
    }
    match symbol_to_collect_uses_of {
        ElmDeclaredSymbol::ModuleName(module_name_to_collect_uses_of)
            if !elm_syntax_module.imports.iter().any(|import| {
                &import.value.module_name.value == module_name_to_collect_uses_of
            }) =>
        {
            // if not imported, that module name can never appear, so we can skip a bunch of
            // traversing! (unless implicitly imported, but those modules are never renamed!)
        }
        _ => {
            elm_syntax_exposing_uses_of_reference_into(
                uses_so_far,
                &elm_syntax_module.header.value.module_name.value,
                &elm_syntax_module.header.value.exposing.value,
                symbol_to_collect_uses_of,
            );
            for import in elm_syntax_module.imports.iter() {
                elm_syntax_import_uses_of_reference_into(
                    uses_so_far,
                    &import.value,
                    symbol_to_collect_uses_of,
                );
            }
            let module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(state, project_state, elm_syntax_module);
            for declaration_node in elm_syntax_module.declarations.iter() {
                elm_syntax_declaration_uses_of_reference_into(
                    uses_so_far,
                    &elm_syntax_module.header.value.module_name.value,
                    &module_origin_lookup,
                    &declaration_node.declaration.value,
                    symbol_to_collect_uses_of,
                );
            }
        }
    }
}

fn elm_syntax_import_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    elm_syntax_import: &ElmSyntaxImport,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    if symbol_to_collect_uses_of
        == ElmDeclaredSymbol::ModuleName(&elm_syntax_import.module_name.value)
    {
        uses_so_far.push(elm_syntax_import.module_name.range);
    }
    match elm_syntax_import.alias {
        None => {}
        Some(elm::GeneratedAsKeywordRangeName {
            as_keyword_range: _,
            name: ref import_alias_name,
        }) => {
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::ImportAlias {
                    module_origin: &elm_syntax_import.module_name.value,
                    alias_name: &import_alias_name.value,
                })
            {
                uses_so_far.push(import_alias_name.range);
            }
        }
    }
    match elm_syntax_import.exposing {
        None => {}
        Some(ref exposing) => {
            elm_syntax_exposing_uses_of_reference_into(
                uses_so_far,
                &elm_syntax_import.module_name.value,
                &exposing.value,
                symbol_to_collect_uses_of,
            );
        }
    }
}

fn elm_syntax_exposing_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    elm_syntax_exposing: &ElmSyntaxExposing,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
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
            name,
            parameters,
            equals_key_symbol_range: _,
            variant0,
            variant1_up,
        } => {
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::TypeNotRecordAlias {
                    module_origin: origin_module,
                    name: &name.value,
                })
            {
                uses_so_far.push(name.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::TypeVariable(&parameter_node.value)
                {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal;
                }
            }
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &variant0.name.value,
                    module_origin: origin_module,
                })
            {
                uses_so_far.push(variant0.name.range);
            }
            for variant0_value in variant0.values.iter() {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(variant0_value),
                    symbol_to_collect_uses_of,
                );
            }
            for variant in variant1_up {
                if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        name: &variant.name.value,
                        module_origin: origin_module,
                    })
                {
                    uses_so_far.push(variant.name.range);
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
        ElmSyntaxDeclaration::Port { name, type_ } => {
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &name.value,
                    module_origin: origin_module,
                })
            {
                uses_so_far.push(name.range);
            }
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(type_),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range: _,
            name,
            parameters,
            equals_key_symbol_range: _,
            type_,
        } => {
            if (symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::TypeNotRecordAlias {
                    name: &name.value,
                    module_origin: origin_module,
                }))
                || (symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::RecordTypeAlias {
                        name: &name.value,
                        module_origin: origin_module,
                    }))
            {
                uses_so_far.push(name.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::TypeVariable(&parameter_node.value)
                {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal;
                }
            }
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(type_),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxDeclaration::ValueOrFunction {
            name,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range: _,
            result,
        } => {
            if symbol_to_collect_uses_of
                == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &name,
                    module_origin: origin_module,
                })
            {
                uses_so_far.push(*implementation_name_range);
            }
            match maybe_signature {
                None => {}
                Some(signature) => {
                    if symbol_to_collect_uses_of
                        == (ElmDeclaredSymbol::VariableOrVariant {
                            name: &signature.name.value,
                            module_origin: origin_module,
                        })
                    {
                        uses_so_far.push(signature.name.range);
                    }
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&signature.type_1),
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
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                &parameter_bindings,
                elm_syntax_node_as_ref(result),
                symbol_to_collect_uses_of,
            );
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
            output,
        } => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(input),
                symbol_to_collect_uses_of,
            );
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(output),
                symbol_to_collect_uses_of,
            );
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
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable,
            bar_key_symbol_range: _,
            field0,
            field1_up,
        } => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(&record_variable.value)
            {
                uses_so_far.push(record_variable.range);
            }
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(&field0.value),
                symbol_to_collect_uses_of,
            );
            for field in field1_up {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxType::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part2),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxType::Tuple { part0, part1 } => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
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
    local_bindings: &[LocalBinding],
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
            matched,
            of_keyword_range: _,
            case0,
            case1_up,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(matched),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(&case0.pattern),
                symbol_to_collect_uses_of,
            );
            {
                let mut local_bindings_including_from_case0_pattern: Vec<LocalBinding> =
                    local_bindings.to_vec();
                elm_syntax_pattern_bindings_into(
                    &mut local_bindings_including_from_case0_pattern,
                    elm_syntax_node_as_ref(&case0.pattern),
                );
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_from_case0_pattern,
                    elm_syntax_node_unbox(&case0.result),
                    symbol_to_collect_uses_of,
                );
            }
            for case in case1_up {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&case.pattern),
                    symbol_to_collect_uses_of,
                );
                let mut local_bindings_including_from_case_pattern: Vec<LocalBinding> =
                    local_bindings.to_vec();
                elm_syntax_pattern_bindings_into(
                    &mut local_bindings_including_from_case_pattern,
                    elm_syntax_node_as_ref(&case.pattern),
                );
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_from_case_pattern,
                    elm_syntax_node_as_ref(&case.result),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Char(_) => {}
        ElmSyntaxExpression::Float(_) => {}
        ElmSyntaxExpression::IfThenElse {
            condition,
            then_keyword_range: _,
            on_true,
            else_keyword_range: _,
            on_false,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(condition),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(on_true),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(on_false),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::InfixOperation {
            left,
            operator: _,
            right,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(left),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(right),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::Integer { .. } => {}
        ElmSyntaxExpression::Lambda {
            parameter0,
            parameter1_up,
            arrow_key_symbol_range: _,
            result,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(parameter0),
                symbol_to_collect_uses_of,
            );
            for parameter_node in parameter1_up {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter_node),
                    symbol_to_collect_uses_of,
                );
            }
            let mut local_bindings_including_from_lambda_parameters = local_bindings.to_vec();
            elm_syntax_pattern_bindings_into(
                &mut local_bindings_including_from_lambda_parameters,
                elm_syntax_node_as_ref(parameter0),
            );
            for parameter_node in parameter1_up {
                elm_syntax_pattern_bindings_into(
                    &mut local_bindings_including_from_lambda_parameters,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                &local_bindings_including_from_lambda_parameters,
                elm_syntax_node_unbox(result),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::LetIn {
            declaration0,
            declaration1_up,
            in_keyword_range: _,
            result,
        } => {
            let mut local_bindings_including_let_declaration_introduced: Vec<LocalBinding> =
                local_bindings.to_vec();
            elm_syntax_let_declaration_introduced_bindings_into(
                &mut local_bindings_including_let_declaration_introduced,
                &declaration0.value,
            );
            for let_declaration_node in declaration1_up {
                elm_syntax_let_declaration_introduced_bindings_into(
                    &mut local_bindings_including_let_declaration_introduced,
                    &let_declaration_node.value,
                );
            }
            elm_syntax_let_declaration_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                &local_bindings_including_let_declaration_introduced,
                &declaration0.value,
                symbol_to_collect_uses_of,
            );
            for let_declaration_node in declaration1_up {
                elm_syntax_let_declaration_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &local_bindings_including_let_declaration_introduced,
                    &let_declaration_node.value,
                    symbol_to_collect_uses_of,
                );
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                &local_bindings_including_let_declaration_introduced,
                elm_syntax_node_unbox(result),
                symbol_to_collect_uses_of,
            );
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
        ElmSyntaxExpression::Negation(in_negation) => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(in_negation),
                symbol_to_collect_uses_of,
            );
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
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
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
            record_variable: record_variable_node,
            bar_key_symbol_range: _,
            field0,
            field1_up,
        } => {
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
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(&field0.value),
                symbol_to_collect_uses_of,
            );
            for field in field1_up {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    local_bindings,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        }
        ElmSyntaxExpression::Reference(reference) => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(&reference.name) {
                if reference.qualification.is_empty()
                    && local_bindings
                        .iter()
                        .any(|local_binding| local_binding.name == &reference.name)
                {
                    uses_so_far.push(elm_syntax_expression_node.range);
                }
            } else {
                let module_origin = look_up_origin_module(
                    module_origin_lookup,
                    &reference.qualification,
                    &reference.name,
                );
                if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::VariableOrVariant {
                        module_origin: module_origin,
                        name: &reference.name,
                    })
                {
                    uses_so_far.push(lsp_types::Range {
                        start: lsp_position_add_characters(
                            elm_syntax_expression_node.range.end,
                            -(reference.name.len() as i32),
                        ),
                        end: elm_syntax_expression_node.range.end,
                    });
                } else if symbol_to_collect_uses_of
                    == (ElmDeclaredSymbol::ImportAlias {
                        module_origin: module_origin,
                        alias_name: &reference.qualification,
                    })
                {
                    uses_so_far.push(lsp_types::Range {
                        start: elm_syntax_expression_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            reference.qualification.len() as i32,
                        ),
                    });
                } else if (symbol_to_collect_uses_of
                    == ElmDeclaredSymbol::ModuleName(module_origin))
                    && (&reference.qualification == module_origin)
                {
                    uses_so_far.push(lsp_types::Range {
                        start: elm_syntax_expression_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            reference.qualification.len() as i32,
                        ),
                    });
                }
            }
        }
        ElmSyntaxExpression::String { .. } => {}
        ElmSyntaxExpression::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(part2),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::Tuple { part0, part1 } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxExpression::Unit => {}
    }
}

fn elm_syntax_let_declaration_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    local_bindings: &[LocalBinding],
    elm_syntax_let_declaration: &ElmSyntaxLetDeclaration,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern,
            equals_key_symbol_range: _,
            expression,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(pattern),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                local_bindings,
                elm_syntax_node_as_ref(expression),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxLetDeclaration::ValueOrFunctionDeclaration {
            name,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range: _,
            result,
        } => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(name) {
                uses_so_far.push(*implementation_name_range);
            }
            match maybe_signature {
                None => {}
                Some(signature) => {
                    if symbol_to_collect_uses_of
                        == ElmDeclaredSymbol::LocalBinding(&signature.name.value)
                    {
                        uses_so_far.push(signature.name.range);
                    }
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&signature.type_1),
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
            let mut local_bindings_including_from_let_function_parameters: Vec<LocalBinding> =
                local_bindings.to_vec();
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
                elm_syntax_node_as_ref(result),
                symbol_to_collect_uses_of,
            );
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
            variable,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(alias_pattern),
                symbol_to_collect_uses_of,
            );
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::LocalBinding(&variable.value) {
                uses_so_far.push(variable.range);
            }
        }
        ElmSyntaxPattern::Char(_) => {}
        ElmSyntaxPattern::Ignored => {}
        ElmSyntaxPattern::Int { .. } => {}
        ElmSyntaxPattern::ListCons {
            head,
            cons_key_symbol: _,
            tail,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(head),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(tail),
                symbol_to_collect_uses_of,
            );
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
            part0,
            part1,
            part2,
        } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part2),
                symbol_to_collect_uses_of,
            );
        }
        ElmSyntaxPattern::Tuple { part0, part1 } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
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
    bindings_so_far: &mut Vec<LocalBinding<'a>>,
    elm_syntax_let_declaration: &'a ElmSyntaxLetDeclaration,
) {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring {
            pattern,
            equals_key_symbol_range: _,
            expression: _,
        } => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_as_ref(pattern));
        }
        ElmSyntaxLetDeclaration::ValueOrFunctionDeclaration {
            name,
            signature,
            implementation_name_range,
            parameters: _,
            equals_key_symbol_range: _,
            result: _,
        } => {
            bindings_so_far.push(LocalBinding {
                name: name,
                origin: LocalBindingOrigin::LetDeclaredVariable {
                    signature: signature.as_ref(),
                    implementation_name_range: *implementation_name_range,
                },
            });
        }
    }
}

fn elm_syntax_pattern_bindings_into<'a>(
    bindings_so_far: &mut Vec<LocalBinding<'a>>,
    elm_syntax_pattern_node: ElmSyntaxNode<&'a ElmSyntaxPattern>,
) {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As {
            pattern: aliased_pattern_node,
            as_keyword_range: _,
            variable,
        } => {
            elm_syntax_pattern_bindings_into(
                bindings_so_far,
                elm_syntax_node_unbox(aliased_pattern_node),
            );
            bindings_so_far.push(LocalBinding {
                origin: LocalBindingOrigin::PatternVariable(variable.range),
                name: &variable.value,
            });
        }
        ElmSyntaxPattern::Char(_) => {}
        ElmSyntaxPattern::Ignored => {}
        ElmSyntaxPattern::Int { .. } => {}
        ElmSyntaxPattern::ListCons {
            head,
            cons_key_symbol: _,
            tail,
        } => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(head));
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(tail));
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
            bindings_so_far.extend(field_names.iter().map(|field_name_node| LocalBinding {
                origin: LocalBindingOrigin::PatternRecordField(field_name_node.range),
                name: &field_name_node.value,
            }));
        }
        ElmSyntaxPattern::String { .. } => {}
        ElmSyntaxPattern::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(part1));
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(part2));
        }

        ElmSyntaxPattern::Tuple { part0, part1 } => {
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_pattern_bindings_into(bindings_so_far, elm_syntax_node_unbox(part1));
        }
        ElmSyntaxPattern::Unit => {}
        ElmSyntaxPattern::Variable(variable) => {
            bindings_so_far.push(LocalBinding {
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
    elm_syntax_highlight_module_header_into(
        highlighted_so_far,
        elm_syntax_node_as_ref(&elm_syntax_module.header),
    );
    for import_node in elm_syntax_module.imports.iter() {
        elm_syntax_highlight_import_into(highlighted_so_far, elm_syntax_node_as_ref(import_node));
    }
    for documented_declaration in elm_syntax_module.declarations.iter() {
        match documented_declaration.documentation {
            None => {}
            Some(ref documentation_node) => {
                elm_syntax_highlight_comment_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(documentation_node),
                );
            }
        }
        elm_syntax_highlight_declaration_into(
            highlighted_so_far,
            elm_syntax_node_as_ref(&documented_declaration.declaration),
        );
    }
    // Inserting many comments in the middle can get expensive (having so many comments to make it matter will be rare).
    // A possible solution (when comment count exceeds other syntax by some factor) is just pushing all comments an sorting the whole thing at once.
    // Feels like overkill, though so I'll hold on on this until issues are opened :)
    for comment_node in elm_syntax_module.comments.iter() {
        elm_syntax_highlight_comment_into(highlighted_so_far, elm_syntax_node_as_ref(comment_node));
    }
}

fn elm_syntax_highlight_module_header_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_module_header_node: ElmSyntaxNode<&ElmSyntaxModuleHeader>,
) {
    match elm_syntax_module_header_node.value.specific {
        None => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_module_header_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_module_header_node.range.start, 6),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_module_header_node.value.module_name.range,
                value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
            });
            elm_syntax_highlight_exposing_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(&elm_syntax_module_header_node.value.exposing),
            );
        }
        Some(ref module_header_specific) => match module_header_specific {
            ElmSyntaxModuleHeaderSpecific::Effect {
                module_keyword_range,
                where_keyword_range,
                command: maybe_command,
                subscription: maybe_subscription,
            } => {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: elm_syntax_module_header_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_module_header_node.range.start,
                            6,
                        ),
                    },
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: *module_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: elm_syntax_module_header_node.value.module_name.range,
                    value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: *where_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                match maybe_command {
                    None => {}
                    Some(command_node) => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: command_node.range,
                            value: ElmSyntaxHighlightKind::VariableDeclaration,
                        });
                    }
                }
                match maybe_subscription {
                    None => {}
                    Some(subscription_node) => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: subscription_node.range,
                            value: ElmSyntaxHighlightKind::VariableDeclaration,
                        });
                    }
                }
                elm_syntax_highlight_exposing_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&elm_syntax_module_header_node.value.exposing),
                );
            }
            ElmSyntaxModuleHeaderSpecific::Port {
                module_keyword_range,
            } => {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: lsp_types::Range {
                        start: elm_syntax_module_header_node.range.start,
                        end: lsp_position_add_characters(
                            elm_syntax_module_header_node.range.start,
                            4,
                        ),
                    },
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: *module_keyword_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: elm_syntax_module_header_node.value.module_name.range,
                    value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
                });
                elm_syntax_highlight_exposing_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&elm_syntax_module_header_node.value.exposing),
                );
            }
        },
    }
}

fn elm_syntax_highlight_comment_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_comment_node: ElmSyntaxNode<&String>,
) {
    let insert_index = highlighted_so_far
        .binary_search_by(|token| {
            lsp_position_compare(token.range.start, elm_syntax_comment_node.range.start)
        })
        .unwrap_or_else(|i| i);
    let tokens_to_insert =
        elm_syntax_comment_node
            .value
            .lines()
            .enumerate()
            .map(|(inner_line, inner_line_str)| {
                let line = elm_syntax_comment_node.range.start.line + (inner_line as u32);
                ElmSyntaxNode {
                    range: if inner_line == 0 {
                        lsp_types::Range {
                            start: elm_syntax_comment_node.range.start,
                            end: lsp_position_add_characters(
                                elm_syntax_comment_node.range.start,
                                inner_line_str.len() as i32,
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
                                    character: inner_line_str.len() as u32,
                                }
                            },
                        }
                    },
                    value: ElmSyntaxHighlightKind::Comment,
                }
            });
    highlighted_so_far.splice(insert_index..insert_index, tokens_to_insert);
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
    highlighted_so_far.push(ElmSyntaxNode {
        range: elm_syntax_import_node.value.module_name.range,
        value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
    });
    match elm_syntax_import_node.value.alias {
        None => {}
        Some(ref alias_node) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: alias_node.as_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: alias_node.name.range,
                value: ElmSyntaxHighlightKind::ModuleNameOrAlias,
            });
        }
    }
    match elm_syntax_import_node.value.exposing {
        None => {}
        Some(ref exposing_node) => {
            elm_syntax_highlight_exposing_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(exposing_node),
            );
        }
    }
}

fn elm_syntax_highlight_exposing_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    elm_syntax_exposing_node: ElmSyntaxNode<&ElmSyntaxExposing>,
) {
    highlighted_so_far.push(ElmSyntaxNode {
        range: lsp_types::Range {
            start: elm_syntax_exposing_node.range.start,
            end: lsp_position_add_characters(elm_syntax_exposing_node.range.start, 8),
        },
        value: ElmSyntaxHighlightKind::KeySymbol,
    });
    match elm_syntax_exposing_node.value {
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
                        open_range,
                    } => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: type_name_node.range,
                            value: ElmSyntaxHighlightKind::Type,
                        });
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: *open_range,
                            value: ElmSyntaxHighlightKind::Variant,
                        });
                    }
                    ElmSyntaxExpose::Operator(_) => {
                        highlighted_so_far.push(ElmSyntaxNode {
                            range: lsp_types::Range {
                                start: lsp_position_add_characters(expose_node.range.start, 1),
                                end: lsp_position_add_characters(expose_node.range.end, -1),
                            },
                            value: ElmSyntaxHighlightKind::KeySymbol,
                        });
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
        ElmSyntaxDeclaration::ValueOrFunction {
            name: _,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range,
            result,
        } => {
            match maybe_signature {
                None => {}
                Some(signature) => {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: signature.name.range,
                        value: ElmSyntaxHighlightKind::VariableDeclaration,
                    });
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(&signature.type_1),
                    );
                }
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *implementation_name_range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
            for parameter_node in parameters {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(result),
            );
        }
        ElmSyntaxDeclaration::ChoiceType {
            name,
            parameters,
            equals_key_symbol_range,
            variant0,
            variant1_up,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: name.range,
                value: ElmSyntaxHighlightKind::Type,
            });
            for parameter_name_node in parameters {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: parameter_name_node.range,
                    value: ElmSyntaxHighlightKind::TypeVariable,
                });
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: variant0.name.range,
                value: ElmSyntaxHighlightKind::Variant,
            });
            for variant0_value_node in variant0.values.iter() {
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
                highlighted_so_far.push(ElmSyntaxNode {
                    range: variant.name.range,
                    value: ElmSyntaxHighlightKind::Variant,
                });
                for variant_value_node in variant.values.iter() {
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(variant_value_node),
                    );
                }
            }
        }
        ElmSyntaxDeclaration::Operator {
            direction,
            operator: operator_node,
            function: function_name_node,
            precedence: precedence_node,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 5),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: direction.range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: precedence_node.range,
                value: ElmSyntaxHighlightKind::Number,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: lsp_position_add_characters(operator_node.range.start, 1),
                    end: lsp_position_add_characters(operator_node.range.end, -1),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: function_name_node.range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
        }
        ElmSyntaxDeclaration::Port {
            name: name_node,
            type_,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_declaration_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_declaration_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: name_node.range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_as_ref(type_));
        }
        ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range,
            name,
            parameters,
            equals_key_symbol_range,
            type_,
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
            highlighted_so_far.push(ElmSyntaxNode {
                range: name.range,
                value: ElmSyntaxHighlightKind::Type,
            });
            for parameter_name_node in parameters {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: parameter_name_node.range,
                    value: ElmSyntaxHighlightKind::TypeVariable,
                });
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_as_ref(type_));
        }
    }
}

fn elm_syntax_highlight_qualified_into(
    highlighted_so_far: &mut Vec<ElmSyntaxNode<ElmSyntaxHighlightKind>>,
    qualified_node: ElmSyntaxNode<&elm::GeneratedNameQualification<String, String>>,
    kind: ElmSyntaxHighlightKind,
) {
    if qualified_node.value.qualification.is_empty() {
        highlighted_so_far.push(ElmSyntaxNode {
            range: qualified_node.range,
            value: kind,
        })
    } else {
        let name_start_position = lsp_position_add_characters(
            qualified_node.range.end,
            -(qualified_node.value.name.len() as i32),
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
            variable: variable_node,
        } => {
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_unbox(alias_pattern_node),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *as_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: variable_node.range,
                value: ElmSyntaxHighlightKind::Variable,
            });
        }
        ElmSyntaxPattern::Char(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: lsp_position_add_characters(elm_syntax_pattern_node.range.start, 1),
                    end: lsp_position_add_characters(elm_syntax_pattern_node.range.end, -1),
                },
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
            head,
            cons_key_symbol,
            tail,
        } => {
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(head));
            highlighted_so_far.push(ElmSyntaxNode {
                range: *cons_key_symbol,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(tail));
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
            quoting_style,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: match quoting_style {
                    elm::ElmSyntaxStringQuotingStyle::StringSingleQuoted => lsp_types::Range {
                        start: lsp_position_add_characters(elm_syntax_pattern_node.range.start, 1),
                        end: lsp_position_add_characters(elm_syntax_pattern_node.range.end, -1),
                    },
                    elm::ElmSyntaxStringQuotingStyle::StringTripleQuoted => lsp_types::Range {
                        start: lsp_position_add_characters(elm_syntax_pattern_node.range.start, 3),
                        end: lsp_position_add_characters(elm_syntax_pattern_node.range.end, -3),
                    },
                },
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxPattern::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(part1));
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(part2));
        }
        ElmSyntaxPattern::Tuple { part0, part1 } => {
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_pattern_into(highlighted_so_far, elm_syntax_node_unbox(part1));
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
        ElmSyntaxPattern::Variant { reference, values } => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(reference),
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
            reference,
            arguments,
        } => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(reference),
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
            output,
        } => {
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(input));
            highlighted_so_far.push(ElmSyntaxNode {
                range: *arrow_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(output));
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
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.colon_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&field.value),
                );
            }
        }
        ElmSyntaxType::RecordExtension {
            record_variable,
            bar_key_symbol_range,
            field0,
            field1_up,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: record_variable.range,
                value: ElmSyntaxHighlightKind::TypeVariable,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: *bar_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: field0.name.range,
                value: ElmSyntaxHighlightKind::Field,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: field0.colon_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_type_into(
                highlighted_so_far,
                elm_syntax_node_unbox(&field0.value),
            );
            for field in field1_up {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.colon_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                elm_syntax_highlight_type_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&field.value),
                );
            }
        }
        ElmSyntaxType::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(part1));
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(part2));
        }
        ElmSyntaxType::Tuple { part0, part1 } => {
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_type_into(highlighted_so_far, elm_syntax_node_unbox(part1));
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
            matched,
            of_keyword_range,
            case0,
            case1_up,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 4),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(matched),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *of_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(&case0.pattern),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: case0.arrow_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(&case0.result),
            );
            for case in case1_up {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&case.pattern),
                );
                highlighted_so_far.push(ElmSyntaxNode {
                    range: case.arrow_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&case.result),
                );
            }
        }
        ElmSyntaxExpression::Char(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: lsp_position_add_characters(elm_syntax_expression_node.range.start, 1),
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.end, -1),
                },
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
            condition,
            then_keyword_range,
            on_true,
            else_keyword_range,
            on_false,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 2),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(condition),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *then_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(on_true),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *else_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(on_false),
            );
        }
        ElmSyntaxExpression::InfixOperation {
            left,
            operator: operator_node,
            right,
        } => {
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(left));
            highlighted_so_far.push(ElmSyntaxNode {
                range: operator_node.range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(right));
        }
        ElmSyntaxExpression::Integer { .. } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: elm_syntax_expression_node.range,
                value: ElmSyntaxHighlightKind::Number,
            });
        }
        ElmSyntaxExpression::Lambda {
            parameter0,
            parameter1_up,
            arrow_key_symbol_range,
            result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 1),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(parameter0),
            );
            for parameter_node in parameter1_up {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *arrow_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(result));
        }
        ElmSyntaxExpression::LetIn {
            declaration0,
            declaration1_up,
            in_keyword_range,
            result,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.start, 3),
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_let_declaration_into(
                highlighted_so_far,
                elm_syntax_node_unbox(declaration0),
            );
            for let_declaration_node in declaration1_up {
                elm_syntax_highlight_let_declaration_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(let_declaration_node),
                );
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *in_keyword_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(result));
        }
        ElmSyntaxExpression::List(elements) => {
            for element_node in elements {
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(element_node),
                );
            }
        }
        ElmSyntaxExpression::Negation(in_negation) => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(in_negation),
            );
        }
        ElmSyntaxExpression::OperatorFunction(_) => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: lsp_position_add_characters(elm_syntax_expression_node.range.start, 1),
                    end: lsp_position_add_characters(elm_syntax_expression_node.range.end, -1),
                },
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
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&field.value),
                );
            }
        }
        ElmSyntaxExpression::RecordAccess {
            record: record_node,
            field: field_name_node,
        } => {
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(record_node),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: lsp_types::Range {
                    start: record_node.range.end,
                    end: field_name_node.range.start,
                },
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: field_name_node.range,
                value: ElmSyntaxHighlightKind::Field,
            });
        }
        ElmSyntaxExpression::RecordAccessFunction(_) => {
            let field_name_start_position =
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
            record_variable,
            bar_key_symbol_range,
            field0,
            field1_up,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: record_variable.range,
                value: ElmSyntaxHighlightKind::Variable,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: *bar_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: field0.name.range,
                value: ElmSyntaxHighlightKind::Field,
            });
            highlighted_so_far.push(ElmSyntaxNode {
                range: field0.equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_unbox(&field0.value),
            );
            for field in field1_up {
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.name.range,
                    value: ElmSyntaxHighlightKind::Field,
                });
                highlighted_so_far.push(ElmSyntaxNode {
                    range: field.equals_key_symbol_range,
                    value: ElmSyntaxHighlightKind::KeySymbol,
                });
                elm_syntax_highlight_expression_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(&field.value),
                );
            }
        }
        ElmSyntaxExpression::Reference(reference) => {
            elm_syntax_highlight_qualified_into(
                highlighted_so_far,
                ElmSyntaxNode {
                    range: elm_syntax_expression_node.range,
                    value: reference,
                },
                if reference
                    .name
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_uppercase())
                {
                    ElmSyntaxHighlightKind::Variant
                } else {
                    ElmSyntaxHighlightKind::Variable
                },
            );
        }
        ElmSyntaxExpression::String {
            content: _,
            quoting_style,
        } => {
            highlighted_so_far.push(ElmSyntaxNode {
                range: match quoting_style {
                    elm::ElmSyntaxStringQuotingStyle::StringSingleQuoted => lsp_types::Range {
                        start: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            1,
                        ),
                        end: lsp_position_add_characters(elm_syntax_expression_node.range.end, -1),
                    },
                    elm::ElmSyntaxStringQuotingStyle::StringTripleQuoted => lsp_types::Range {
                        start: lsp_position_add_characters(
                            elm_syntax_expression_node.range.start,
                            3,
                        ),
                        end: lsp_position_add_characters(elm_syntax_expression_node.range.end, -3),
                    },
                },
                value: ElmSyntaxHighlightKind::String,
            });
        }
        ElmSyntaxExpression::Triple {
            part0,
            part1,
            part2,
        } => {
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(part1));
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(part2));
        }
        ElmSyntaxExpression::Tuple { part0, part1 } => {
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(part0));
            elm_syntax_highlight_expression_into(highlighted_so_far, elm_syntax_node_unbox(part1));
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
            equals_key_symbol_range,
            expression: destructured_expression_node,
        } => {
            elm_syntax_highlight_pattern_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(destructuring_pattern_node),
            );
            highlighted_so_far.push(ElmSyntaxNode {
                range: *equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(destructured_expression_node),
            );
        }
        ElmSyntaxLetDeclaration::ValueOrFunctionDeclaration {
            name: _,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range,
            result,
        } => {
            match maybe_signature {
                None => {}
                Some(signature) => {
                    highlighted_so_far.push(ElmSyntaxNode {
                        range: signature.name.range,
                        value: ElmSyntaxHighlightKind::VariableDeclaration,
                    });
                    elm_syntax_highlight_type_into(
                        highlighted_so_far,
                        elm_syntax_node_as_ref(&signature.type_1),
                    );
                }
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *implementation_name_range,
                value: ElmSyntaxHighlightKind::VariableDeclaration,
            });
            for parameter_node in parameters {
                elm_syntax_highlight_pattern_into(
                    highlighted_so_far,
                    elm_syntax_node_as_ref(parameter_node),
                );
            }
            highlighted_so_far.push(ElmSyntaxNode {
                range: *equals_key_symbol_range,
                value: ElmSyntaxHighlightKind::KeySymbol,
            });
            elm_syntax_highlight_expression_into(
                highlighted_so_far,
                elm_syntax_node_as_ref(result),
            );
        }
    }
}
