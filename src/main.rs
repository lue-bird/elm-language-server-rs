#![allow(non_shorthand_field_patterns)]
#![allow(non_upper_case_globals)]

mod elm;

struct State {
    elm_jsons_source_directories: std::collections::HashMap<std::path::PathBuf, Vec<std::path::PathBuf>>,
    parsed_modules: std::collections::HashMap<std::path::PathBuf, ModuleState>,
}

struct ModuleState {
    source: String,
    syntax: Option<ElmSyntaxModule>,
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
                                elm::GeneratedHomeDirectory { home_directory: match home_directory_path_str {
                                    Some(ref path_str) => {
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
                            let parse_allocator: bumpalo::Bump = bumpalo::Bump::new();
                            state.parsed_modules.insert(file_uri, ModuleState {
                                syntax: elm::elm_parser_lenient_run(
                                    elm::elm_parser_lenient_module_(&parse_allocator),
                                    elm::StringString::One(&file_content),
                                ).map(elm_syntax_module_to_persistent),
                                source: file_content,
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
                        rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                            prepare_provider: Some(true),
                            work_done_progress_options: lsp_types::WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                        })),
                        ..lsp_types::ServerCapabilities::default()
                    },
                    server_info: None,
                })
            }
        }).request::<lsp_types::request::HoverRequest, _>(|state, hover_arguments| {
            let maybe_hover_result =
                hover_arguments
                    .text_document_position_params
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(|hovered_file_path| {
                        let module_syntax =
                            state.parsed_modules.get(&hovered_file_path).and_then(|m| m.syntax.as_ref())?;
                        let hovered_reference =
                            elm_syntax_module_find_reference_at_position(
                                state,
                                module_syntax,
                                hover_arguments.text_document_position_params.position,
                            )?;
                        match &hovered_reference.value {
                            // referencing a module
                            ElmSymbolOwned::ModuleName(hovered_module_name) |
                            ElmSymbolOwned::ImportAlias { module_origin: hovered_module_name, alias_name: _ } => {
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, hovered_module_name)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                Some(lsp_types::Hover {
                                    contents: lsp_types::HoverContents::Scalar(
                                        lsp_types::MarkedString::String(
                                            match origin_module_syntax
                                                .comments
                                                .iter()
                                                .find(|comment_node| comment_node.value.starts_with("{-|")) {
                                                None => 
                                                // show list of exports or something
                                                "_module has no documentation comment_".to_string(),
                                                Some(module_documentation) => module_documentation
                                                    .value
                                                    .trim_start_matches("{-|")
                                                    .trim_end_matches("-}")
                                                    .trim()
                                                    .to_string(),
                                            },
                                        ),
                                    ),
                                    range: Some(hovered_reference.range),
                                })
                            },
                            // referencing a module-declared member
                            ElmSymbolOwned::VariableOrVariantOrOperator { module_origin: hovered_module_origin, name: hovered_name } => {
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, &hovered_module_origin)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                let module_origin_lookup: ModuleOriginLookup =
                                    elm_syntax_module_create_origin_lookup(state, origin_module_syntax);
                                origin_module_syntax
                                    .declarations
                                    .iter()
                                    .find_map(
                                        |origin_module_declaration| match &origin_module_declaration
                                            .declaration
                                            .value {
                                            ElmSyntaxDeclaration
                                            ::ChoiceType {
                                                name: origin_module_declaration_name,
                                                parameters: origin_module_declaration_parameters,
                                                equals_key_symbol_range: _,
                                                variant0: origin_module_declaration_variant0,
                                                variant1_up: origin_module_declaration_variant1_up,
                                            } => if (&origin_module_declaration_variant0
                                                .name
                                                .value ==
                                                hovered_name) ||
                                                (origin_module_declaration_variant1_up
                                                    .iter()
                                                    .any(|variant| &variant.name.value == hovered_name)) {
                                                let description =
                                                    format!(
                                                        "variant in\n```elm\ntype {}.{}{}\n    = {}{}{}\n```\n",
                                                        &hovered_module_origin,
                                                        &origin_module_declaration_name.value,
                                                        &origin_module_declaration_parameters
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, parameter_node| so_far + " " +
                                                                    &parameter_node.value,
                                                            ),
                                                        &origin_module_declaration_variant0.name.value,
                                                        &origin_module_declaration_variant0
                                                            .values
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, value_node| so_far + " " +
                                                                    &elm_syntax_type_to_single_line_string(
                                                                        &module_origin_lookup,
                                                                        &value_node.value,
                                                                    ),
                                                            ),
                                                        &origin_module_declaration_variant1_up
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, variant| so_far + "\n    | " +
                                                                    &variant.name.value +
                                                                    &variant
                                                                        .values
                                                                        .iter()
                                                                        .fold(
                                                                            String::new(),
                                                                            |so_far, value_node| so_far + " " +
                                                                                &elm_syntax_type_to_single_line_string(
                                                                                    &module_origin_lookup,
                                                                                    &value_node.value,
                                                                                ),
                                                                        ),
                                                            ),
                                                    );
                                                Some(lsp_types::Hover {
                                                    contents: lsp_types::HoverContents::Markup(
                                                        lsp_types::MarkupContent {
                                                            kind: lsp_types::MarkupKind::Markdown,
                                                            value: match origin_module_declaration.documentation {
                                                                None => description,
                                                                Some(ref documentation) => description + "-----\n" +
                                                                    &documentation_comment_to_markdown(
                                                                        &documentation.value,
                                                                    ),
                                                            },
                                                        },
                                                    ),
                                                    range: Some(hovered_reference.range),
                                                })
                                            } else {
                                                None
                                            },
                                            ElmSyntaxDeclaration
                                            ::Operator {
                                                direction: origin_module_declaration_direction,
                                                operator: origin_module_declaration_operator,
                                                function: origin_module_declaration_function,
                                                precedence: origin_module_declaration_precedence,
                                            } => if hovered_name ==
                                                &origin_module_declaration_operator.value {
                                                let maybe_origin_operator_function_declaration =
                                                    origin_module_syntax
                                                        .declarations
                                                        .iter()
                                                        .find_map(|origin_module_declaration| {
                                                            match &origin_module_declaration.declaration.value {
                                                                ElmSyntaxDeclaration
                                                                ::ValueOrFunction {
                                                                    name: origin_module_declaration_name,
                                                                    signature: origin_module_declaration_signature,
                                                                    implementation_name_range: _,
                                                                    parameters: _,
                                                                    equals_key_symbol_range: _,
                                                                    result: _,
                                                                } if
                                                                    origin_module_declaration_name ==
                                                                        &origin_module_declaration_function.value => Some(
                                                                    (
                                                                        origin_module_declaration_signature,
                                                                        origin_module_declaration
                                                                            .documentation
                                                                            .as_ref(),
                                                                    ),
                                                                ),
                                                                _ => None,
                                                            }
                                                        });
                                                let hover_markdown = match maybe_origin_operator_function_declaration {
                                                    Some(
                                                        (
                                                            origin_operator_function_maybe_signature,
                                                            origin_operator_function_maybe_documentation,
                                                        ),
                                                    ) => {
                                                        let description =
                                                            "```elm\ninfix ".to_string() +
                                                                match origin_module_declaration_direction.value {
                                                                    elm::ElmSyntaxInfixDirection::Left => "left",
                                                                    elm::ElmSyntaxInfixDirection::Non => "non",
                                                                    elm::ElmSyntaxInfixDirection::Right => "right",
                                                                } + " " +
                                                                &origin_module_declaration_precedence
                                                                    .value
                                                                    .to_string() +
                                                                " " +
                                                                &hovered_module_origin +
                                                                ".(" +
                                                                &origin_module_declaration_operator.value +
                                                                ")" +
                                                                &(match origin_operator_function_maybe_signature {
                                                                    None => "".to_string(),
                                                                    Some(origin_operator_function_signature) => {
                                                                        " : ".to_string() +
                                                                            &elm_syntax_type_to_single_line_string(
                                                                                &module_origin_lookup,
                                                                                &origin_operator_function_signature
                                                                                    .type_1
                                                                                    .value,
                                                                            )
                                                                    },
                                                                }) + "\n```\n";
                                                        match origin_operator_function_maybe_documentation {
                                                            None => description,
                                                            Some(documentation) => description + "-----\n" +
                                                                &documentation_comment_to_markdown(
                                                                    &documentation.value,
                                                                ),
                                                        }
                                                    },
                                                    None => {
                                                        let description =
                                                            "```elm\ninfix ".to_string() +
                                                                match origin_module_declaration_direction.value {
                                                                    elm::ElmSyntaxInfixDirection::Left => "left",
                                                                    elm::ElmSyntaxInfixDirection::Non => "non",
                                                                    elm::ElmSyntaxInfixDirection::Right => "right",
                                                                } + " " +
                                                                &origin_module_declaration_precedence
                                                                    .value
                                                                    .to_string() +
                                                                " " +
                                                                &hovered_module_origin +
                                                                ".(" +
                                                                &origin_module_declaration_operator.value +
                                                                ")" +
                                                                "\n```\n";
                                                        match origin_module_declaration.documentation {
                                                            None => description,
                                                            Some(ref documentation) => description + "-----\n" +
                                                                &documentation_comment_to_markdown(
                                                                    &documentation.value,
                                                                ),
                                                        }
                                                    },
                                                };
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
                                            },
                                            ElmSyntaxDeclaration
                                            ::Port {
                                                name: origin_module_declaration_name,
                                                type_,
                                            } => {
                                                if &origin_module_declaration_name.value == hovered_name {
                                                    let description =
                                                        format!(
                                                            "```elm\nport {}.{} : {}\n```\n",
                                                            &hovered_module_origin,
                                                            &origin_module_declaration_name.value,
                                                            &elm_syntax_type_to_single_line_string(
                                                                &module_origin_lookup,
                                                                &type_.value,
                                                            )
                                                        );
                                                    Some(lsp_types::Hover {
                                                        contents: lsp_types::HoverContents::Markup(
                                                            lsp_types::MarkupContent {
                                                                kind: lsp_types::MarkupKind::Markdown,
                                                                value: match origin_module_declaration.documentation {
                                                                    None => description,
                                                                    Some(ref documentation) => description +
                                                                        "-----\n" +
                                                                        &documentation_comment_to_markdown(
                                                                            &documentation.value,
                                                                        ),
                                                                },
                                                            },
                                                        ),
                                                        range: Some(hovered_reference.range),
                                                    })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration
                                            ::TypeAlias {
                                                alias_keyword_range: _,
                                                name: origin_module_declaration_name,
                                                parameters: origin_module_declaration_parameters,
                                                equals_key_symbol_range: _,
                                                type_,
                                            } => {
                                                if &origin_module_declaration_name.value == hovered_name {
                                                    let description =
                                                        format!(
                                                            "constructor function for record\n```elm\ntype alias {}.{}{} =\n    {}\n```\n",
                                                            &hovered_module_origin,
                                                            &origin_module_declaration_name.value,
                                                            &origin_module_declaration_parameters
                                                                .iter()
                                                                .fold(
                                                                    String::new(),
                                                                    |so_far, parameter_node| so_far + " " +
                                                                        &parameter_node.value,
                                                                ),
                                                            &elm_syntax_type_to_single_line_string(
                                                                &module_origin_lookup,
                                                                &type_.value,
                                                            )
                                                        );
                                                    Some(lsp_types::Hover {
                                                        contents: lsp_types::HoverContents::Markup(
                                                            lsp_types::MarkupContent {
                                                                kind: lsp_types::MarkupKind::Markdown,
                                                                value: match origin_module_declaration.documentation {
                                                                    None => description,
                                                                    Some(ref documentation) => description +
                                                                        "-----\n" +
                                                                        &documentation_comment_to_markdown(
                                                                            &documentation.value,
                                                                        ),
                                                                },
                                                            },
                                                        ),
                                                        range: Some(hovered_reference.range),
                                                    })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration
                                            ::ValueOrFunction {
                                                name: origin_module_declaration_name,
                                                signature: origin_module_declaration_maybe_signature,
                                                implementation_name_range: _,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } => if origin_module_declaration_name ==
                                                hovered_name {
                                                let description = match origin_module_declaration_maybe_signature {
                                                    Some(origin_module_declaration_signature) => format!(
                                                        "```elm\n{}.{} : {}\n```\n",
                                                        &hovered_module_origin,
                                                        &origin_module_declaration_name,
                                                        &elm_syntax_type_to_single_line_string(
                                                            &module_origin_lookup,
                                                            &origin_module_declaration_signature.type_1.value,
                                                        )
                                                    ),
                                                    None => format!(
                                                        "```elm\n{}.{}\n```\n",
                                                        &hovered_module_origin,
                                                        &origin_module_declaration_name
                                                    ),
                                                };
                                                Some(lsp_types::Hover {
                                                    contents: lsp_types::HoverContents::Markup(
                                                        lsp_types::MarkupContent {
                                                            kind: lsp_types::MarkupKind::Markdown,
                                                            value: match origin_module_declaration.documentation {
                                                                None => description,
                                                                Some(ref documentation) => description + "-----\n" +
                                                                    &documentation_comment_to_markdown(
                                                                        &documentation.value,
                                                                    ),
                                                            },
                                                        },
                                                    ),
                                                    range: Some(hovered_reference.range),
                                                })
                                            } else {
                                                None
                                            },
                                        },
                                    )
                            },
                            ElmSymbolOwned::Type { module_origin: hovered_module_origin, name: hovered_name } => {
                                if (hovered_module_origin == "List") && (hovered_name == "List") {
                                    // module List has no type List.List exposed in an oversight so we make one up.
                                    // See https://github.com/elm/core/issues/1037
                                    return Some(lsp_types::Hover {
                                        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                                            kind: lsp_types::MarkupKind::Markdown,
                                            value: "```elm
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
        ".to_string(),
                                        }),
                                        range: Some(hovered_reference.range),
                                    })
                                }
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, &hovered_module_origin)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                let module_origin_lookup: ModuleOriginLookup =
                                    elm_syntax_module_create_origin_lookup(state, origin_module_syntax);
                                origin_module_syntax
                                    .declarations
                                    .iter()
                                    .find_map(
                                        |origin_module_declaration| match &origin_module_declaration
                                            .declaration
                                            .value {
                                            ElmSyntaxDeclaration
                                            ::ChoiceType {
                                                name: origin_module_declaration_name,
                                                parameters: origin_module_declaration_parameters,
                                                equals_key_symbol_range: _,
                                                variant0: origin_module_declaration_variant0,
                                                variant1_up: origin_module_declaration_variant1_up,
                                            } => if &origin_module_declaration_name.value ==
                                                hovered_name {
                                                let description =
                                                    format!(
                                                        "```elm\ntype {}.{}{}\n    = {}{}{}\n```\n",
                                                        &hovered_module_origin,
                                                        &origin_module_declaration_name.value,
                                                        &origin_module_declaration_parameters
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, parameter_node| so_far + " " +
                                                                    &parameter_node.value,
                                                            ),
                                                        &origin_module_declaration_variant0.name.value,
                                                        &origin_module_declaration_variant0
                                                            .values
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, value_node| so_far + " " +
                                                                    &elm_syntax_type_to_single_line_string(
                                                                        &module_origin_lookup,
                                                                        &value_node.value,
                                                                    ),
                                                            ),
                                                        &origin_module_declaration_variant1_up
                                                            .iter()
                                                            .fold(
                                                                String::new(),
                                                                |so_far, variant| so_far + "\n    | " +
                                                                    &variant.name.value +
                                                                    &variant
                                                                        .values
                                                                        .iter()
                                                                        .fold(
                                                                            String::new(),
                                                                            |so_far, value_node| so_far + " " +
                                                                                &elm_syntax_type_to_single_line_string(
                                                                                    &module_origin_lookup,
                                                                                    &value_node.value,
                                                                                ),
                                                                        ),
                                                            ),
                                                    );
                                                Some(lsp_types::Hover {
                                                    contents: lsp_types::HoverContents::Markup(
                                                        lsp_types::MarkupContent {
                                                            kind: lsp_types::MarkupKind::Markdown,
                                                            value: match origin_module_declaration.documentation {
                                                                None => description,
                                                                Some(ref documentation) => description + "-----\n" +
                                                                    &documentation_comment_to_markdown(
                                                                        &documentation.value,
                                                                    ),
                                                            },
                                                        },
                                                    ),
                                                    range: Some(hovered_reference.range),
                                                })
                                            } else {
                                                None
                                            },
                                            ElmSyntaxDeclaration::Operator { .. } => None,
                                            ElmSyntaxDeclaration::Port { .. } => None,
                                            ElmSyntaxDeclaration
                                            ::TypeAlias {
                                                alias_keyword_range: _,
                                                name: origin_module_declaration_name,
                                                parameters: origin_module_declaration_parameters,
                                                equals_key_symbol_range: _,
                                                type_,
                                            } => {
                                                if &origin_module_declaration_name.value == hovered_name {
                                                    let description =
                                                        format!(
                                                            "```elm\ntype alias {}.{}{} =\n    {}\n```\n",
                                                            &hovered_module_origin,
                                                            &origin_module_declaration_name.value,
                                                            &origin_module_declaration_parameters
                                                                .iter()
                                                                .fold(
                                                                    String::new(),
                                                                    |so_far, parameter_node| so_far + " " +
                                                                        &parameter_node.value,
                                                                ),
                                                            &elm_syntax_type_to_single_line_string(
                                                                &module_origin_lookup,
                                                                &type_.value,
                                                            )
                                                        );
                                                    Some(lsp_types::Hover {
                                                        contents: lsp_types::HoverContents::Markup(
                                                            lsp_types::MarkupContent {
                                                                kind: lsp_types::MarkupKind::Markdown,
                                                                value: match origin_module_declaration.documentation {
                                                                    None => description,
                                                                    Some(ref documentation) => description +
                                                                        "-----\n" +
                                                                        &documentation_comment_to_markdown(
                                                                            &documentation.value,
                                                                        ),
                                                                },
                                                            },
                                                        ),
                                                        range: Some(hovered_reference.range),
                                                    })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration::ValueOrFunction { .. } => None,
                                        },
                                    )
                            },
                        }
                    });
            async move {
                Ok(maybe_hover_result)
            }
        }).request::<lsp_types::request::GotoDefinition, _>(|state, goto_definition_arguments| {
            let maybe_declaration_range: Option<lsp_types::Location> =
                goto_definition_arguments
                    .text_document_position_params
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(|goto_file_path| {
                        let module_syntax =
                            state.parsed_modules.get(&goto_file_path).and_then(|m| m.syntax.as_ref())?;
                        let goto_reference =
                            elm_syntax_module_find_reference_at_position(
                                state,
                                module_syntax,
                                goto_definition_arguments.text_document_position_params.position,
                            )?;
                        match &goto_reference.value {
                            ElmSymbolOwned::ModuleName(goto_module_name) |
                            ElmSymbolOwned::ImportAlias { module_origin: goto_module_name, alias_name: _ } => {
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, goto_module_name)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                    .ok()
                                    .map(|origin_module_file_url| lsp_types::Location {
                                        uri: origin_module_file_url,
                                        range: origin_module_syntax.header.value.module_name.range,
                                    })
                            },
                            ElmSymbolOwned
                            ::VariableOrVariantOrOperator {
                                module_origin: goto_module_origin,
                                name: goto_name,
                            } => {
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, &goto_module_origin)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                origin_module_syntax
                                    .declarations
                                    .iter()
                                    .find_map(
                                        |origin_module_declaration| match &origin_module_declaration
                                            .declaration
                                            .value {
                                            ElmSyntaxDeclaration
                                            ::ChoiceType {
                                                name: _,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                variant0: origin_module_declaration_variant0,
                                                variant1_up: origin_module_declaration_variant1_up,
                                            } => if &origin_module_declaration_variant0
                                                .name
                                                .value ==
                                                goto_name {
                                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                                    .ok()
                                                    .map(|origin_module_file_url| lsp_types::Location {
                                                        uri: origin_module_file_url,
                                                        range: origin_module_declaration_variant0.name.range,
                                                    })
                                            } else {
                                                origin_module_declaration_variant1_up
                                                    .iter()
                                                    .find(|variant| &variant.name.value == goto_name)
                                                    .and_then(
                                                        |variant| lsp_types::Url::from_file_path(
                                                            &origin_module_file_path,
                                                        )
                                                            .ok()
                                                            .map(|origin_module_file_url| lsp_types::Location {
                                                                uri: origin_module_file_url,
                                                                range: variant.name.range,
                                                            }),
                                                    )
                                            },
                                            ElmSyntaxDeclaration
                                            ::Operator {
                                                direction: _,
                                                operator: origin_module_declaration_operator,
                                                function: origin_module_declaration_function,
                                                precedence: _,
                                            } => if goto_name ==
                                                &origin_module_declaration_operator.value {
                                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                                    .ok()
                                                    .map(|origin_module_file_url| lsp_types::Location {
                                                        uri: origin_module_file_url,
                                                        range: origin_module_declaration_operator.range,
                                                    })
                                            } else if goto_name == &origin_module_declaration_function.value {
                                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                                    .ok()
                                                    .map(|origin_module_file_url| lsp_types::Location {
                                                        uri: origin_module_file_url,
                                                        range: origin_module_declaration_function.range,
                                                    })
                                            } else {
                                                None
                                            },
                                            ElmSyntaxDeclaration
                                            ::Port {
                                                name: origin_module_declaration_name,
                                                type_: _,
                                            } => {
                                                if &origin_module_declaration_name.value == goto_name {
                                                    lsp_types::Url::from_file_path(&origin_module_file_path)
                                                        .ok()
                                                        .map(|origin_module_file_url| lsp_types::Location {
                                                            uri: origin_module_file_url,
                                                            range: origin_module_declaration_name.range,
                                                        })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration
                                            ::TypeAlias {
                                                alias_keyword_range: _,
                                                name: origin_module_declaration_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                type_: _,
                                            } => {
                                                // record type alias constructor function
                                                if &origin_module_declaration_name.value == goto_name {
                                                    lsp_types::Url::from_file_path(&origin_module_file_path)
                                                        .ok()
                                                        .map(|origin_module_file_url| lsp_types::Location {
                                                            uri: origin_module_file_url,
                                                            range: origin_module_declaration_name.range,
                                                        })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration
                                            ::ValueOrFunction {
                                                name: origin_module_declaration_name,
                                                signature: origin_module_declaration_maybe_signature,
                                                implementation_name_range: origin_module_declaration_implementation_name_range,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } => if origin_module_declaration_name ==
                                                goto_name {
                                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                                    .ok()
                                                    .map(|origin_module_file_url| lsp_types::Location {
                                                        uri: origin_module_file_url,
                                                        range: match origin_module_declaration_maybe_signature {
                                                            Some(
                                                                origin_module_declaration_signature,
                                                            ) => origin_module_declaration_signature
                                                                .name
                                                                .range,
                                                            None => *origin_module_declaration_implementation_name_range,
                                                        },
                                                    })
                                            } else {
                                                None
                                            },
                                        },
                                    )
                            },
                            ElmSymbolOwned::Type { module_origin: goto_module_origin, name: goto_name } => {
                                let origin_module_file_path =
                                    state_file_path_for_module_name(state, &goto_module_origin)?;
                                let origin_module_syntax =
                                    state
                                        .parsed_modules
                                        .get(&origin_module_file_path)
                                        .and_then(|m| m.syntax.as_ref())?;
                                origin_module_syntax
                                    .declarations
                                    .iter()
                                    .find_map(
                                        |origin_module_declaration| match &origin_module_declaration
                                            .declaration
                                            .value {
                                            ElmSyntaxDeclaration
                                            ::ChoiceType {
                                                name: origin_module_declaration_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                variant0: _,
                                                variant1_up: _,
                                            } => if &origin_module_declaration_name.value ==
                                                goto_name {
                                                lsp_types::Url::from_file_path(&origin_module_file_path)
                                                    .ok()
                                                    .map(|origin_module_file_url| lsp_types::Location {
                                                        uri: origin_module_file_url,
                                                        range: origin_module_declaration_name.range,
                                                    })
                                            } else {
                                                None
                                            },
                                            ElmSyntaxDeclaration::Operator { .. } => None,
                                            ElmSyntaxDeclaration::Port { .. } => None,
                                            ElmSyntaxDeclaration
                                            ::TypeAlias {
                                                alias_keyword_range: _,
                                                name: origin_module_declaration_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                type_: _,
                                            } => {
                                                if &origin_module_declaration_name.value == goto_name {
                                                    lsp_types::Url::from_file_path(&origin_module_file_path)
                                                        .ok()
                                                        .map(|origin_module_file_url| lsp_types::Location {
                                                            uri: origin_module_file_url,
                                                            range: origin_module_declaration_name.range,
                                                        })
                                                } else {
                                                    None
                                                }
                                            },
                                            ElmSyntaxDeclaration::ValueOrFunction { .. } => None,
                                        },
                                    )
                            },
                        }
                    });
            async move {
                Ok(maybe_declaration_range.map(|location| lsp_types::GotoDefinitionResponse::Scalar(location)))
            }
        }).request::<lsp_types::request::PrepareRenameRequest, _>(|state, prepare_rename_arguments| {
            let prepared =
                prepare_rename_arguments.text_document.uri.to_file_path().ok().and_then(|prepare_rename_file_path| {
                    let prepare_rename_module_syntax =
                        state.parsed_modules.get(&prepare_rename_file_path).and_then(|m| m.syntax.as_ref())?;
                    let prepare_rename_symbol: ElmSyntaxNode<ElmSymbolOwned> =
                        elm_syntax_module_find_reference_at_position(
                            state,
                            prepare_rename_module_syntax,
                            prepare_rename_arguments.position,
                        )?;
                    let placeholder = match &prepare_rename_symbol.value {
                        ElmSymbolOwned::ImportAlias { module_origin: _, alias_name: alias_name } => {
                            alias_name.clone()
                        },
                        ElmSymbolOwned::ModuleName(module_name) => {
                            module_name.clone()
                        },
                        ElmSymbolOwned::VariableOrVariantOrOperator { module_origin: _, name } => {
                            name.clone()
                        },
                        ElmSymbolOwned::Type { module_origin: _, name } => {
                            name.clone()
                        },
                    };
                    Some(lsp_types::PrepareRenameResponse::RangeWithPlaceholder {
                        range: lsp_types::Range {
                            start: 
                            // if qualified, only the actual, unqualified name is relevant
                            lsp_position_add_characters(prepare_rename_symbol.range.end, -(placeholder.len() as i32)),
                            end: prepare_rename_symbol.range.end,
                        },
                        placeholder: placeholder,
                    })
                });
            async move {
                Ok(prepared)
            }
        }).request::<lsp_types::request::Shutdown, _>(|_state, ()| {
            // ?
            async {
                Ok(())
            }
        }).request::<lsp_types::request::Rename, _>(|state, rename_arguments| {
            let maybe_rename_edits =
                rename_arguments
                    .text_document_position
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(|to_rename_file_path| {
                        let to_rename_module_syntax =
                            state.parsed_modules.get(&to_rename_file_path).and_then(|m| m.syntax.as_ref())?;
                        let symbol_to_rename: ElmSyntaxNode<ElmSymbolOwned> =
                            elm_syntax_module_find_reference_at_position(
                                state,
                                to_rename_module_syntax,
                                rename_arguments.text_document_position.position,
                            )?;
                        Some(match &symbol_to_rename.value {
                            ElmSymbolOwned
                            ::ImportAlias {
                                module_origin: import_alias_to_rename_module_origin,
                                alias_name: import_alias_to_rename,
                            } => {
                                let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                                elm_syntax_module_uses_of_reference_into(
                                    &mut all_uses_of_renamed_module_name,
                                    state,
                                    to_rename_module_syntax,
                                    ElmDeclaredSymbol::ImportAlias {
                                        module_origin: import_alias_to_rename_module_origin,
                                        alias_name: import_alias_to_rename,
                                    },
                                );
                                vec!(lsp_types::TextDocumentEdit {
                                    text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                        uri: rename_arguments.text_document_position.text_document.uri,
                                        version: None,
                                    },
                                    edits: all_uses_of_renamed_module_name
                                        .into_iter()
                                        .map(
                                            |use_range_of_renamed_module| lsp_types::OneOf::Left(lsp_types::TextEdit {
                                                range: use_range_of_renamed_module,
                                                new_text: rename_arguments.new_name.clone(),
                                            }),
                                        )
                                        .collect::<Vec<_>>(),
                                })
                            },
                            ElmSymbolOwned::ModuleName(module_name_to_rename) => {
                                state.parsed_modules.iter().filter_map(|(elm_module_file_path, elm_module_state)| {
                                    let elm_module_syntax = elm_module_state.syntax.as_ref()?;
                                    let elm_module_uri = lsp_types::Url::from_file_path(elm_module_file_path).ok()?;
                                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                                    elm_syntax_module_uses_of_reference_into(
                                        &mut all_uses_of_renamed_module_name,
                                        state,
                                        elm_module_syntax,
                                        ElmDeclaredSymbol::ModuleName(module_name_to_rename),
                                    );

                                    // should this also rename the actual module origin file?
                                    Some(lsp_types::TextDocumentEdit {
                                        text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                            uri: elm_module_uri,
                                            version: None,
                                        },
                                        edits: all_uses_of_renamed_module_name
                                            .into_iter()
                                            .map(
                                                |use_range_of_renamed_module| lsp_types::OneOf::Left(
                                                    lsp_types::TextEdit {
                                                        range: use_range_of_renamed_module,
                                                        new_text: rename_arguments.new_name.clone(),
                                                    },
                                                ),
                                            )
                                            .collect::<Vec<_>>(),
                                    })
                                }).collect::<Vec<_>>()
                            },
                            ElmSymbolOwned
                            ::VariableOrVariantOrOperator {
                                module_origin: to_rename_module_origin,
                                name: to_rename_name,
                            } => {
                                state.parsed_modules.iter().filter_map(|(elm_module_file_path, elm_module_syntax)| {
                                    let elm_module_syntax = elm_module_syntax.syntax.as_ref()?;
                                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                                    elm_syntax_module_uses_of_reference_into(
                                        &mut all_uses_of_renamed_module_name,
                                        state,
                                        elm_module_syntax,
                                        ElmDeclaredSymbol::VariableOrVariant {
                                            module_origin: to_rename_module_origin,
                                            name: to_rename_name,
                                        },
                                    );
                                    lsp_types::Url::from_file_path(elm_module_file_path)
                                        .ok()
                                        .map(|elm_module_uri| lsp_types::TextDocumentEdit {
                                            text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                                uri: elm_module_uri,
                                                version: None,
                                            },
                                            edits: all_uses_of_renamed_module_name
                                                .into_iter()
                                                .map(
                                                    |use_range_of_renamed_module| lsp_types::OneOf::Left(
                                                        lsp_types::TextEdit {
                                                            range: use_range_of_renamed_module,
                                                            new_text: rename_arguments.new_name.clone(),
                                                        },
                                                    ),
                                                )
                                                .collect::<Vec<_>>(),
                                        })
                                }).collect::<Vec<_>>()
                            },
                            ElmSymbolOwned::Type { module_origin: to_rename_module_origin, name: to_rename_name } => {
                                state.parsed_modules.iter().filter_map(|(elm_module_file_path, elm_module_syntax)| {
                                    let elm_module_syntax = elm_module_syntax.syntax.as_ref()?;
                                    let to_rename_is_record_type_alias =
                                        elm_module_syntax
                                            .declarations
                                            .iter()
                                            .any(|declaration| match &declaration.declaration.value {
                                                ElmSyntaxDeclaration
                                                ::TypeAlias {
                                                    type_: ElmSyntaxNode {
                                                        value: ElmSyntaxType::Record(_),
                                                        range: _,
                                                    },
                                                    alias_keyword_range: _,
                                                    name: _,
                                                    parameters: _,
                                                    equals_key_symbol_range: _,
                                                } => true,
                                                _ => false,
                                            });
                                    let mut all_uses_of_renamed_module_name: Vec<lsp_types::Range> = Vec::new();
                                    elm_syntax_module_uses_of_reference_into(
                                        &mut all_uses_of_renamed_module_name,
                                        state,
                                        elm_module_syntax,
                                        if to_rename_is_record_type_alias {
                                            ElmDeclaredSymbol::RecordTypeAlias {
                                                module_origin: to_rename_module_origin,
                                                name: to_rename_name,
                                            }
                                        } else {
                                            ElmDeclaredSymbol::TypeNotRecordAlias {
                                                module_origin: to_rename_module_origin,
                                                name: to_rename_name,
                                            }
                                        },
                                    );
                                    lsp_types::Url::from_file_path(elm_module_file_path)
                                        .ok()
                                        .map(|elm_module_uri| lsp_types::TextDocumentEdit {
                                            text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                                                uri: elm_module_uri,
                                                version: None,
                                            },
                                            edits: all_uses_of_renamed_module_name
                                                .into_iter()
                                                .map(
                                                    |use_range_of_renamed_module| lsp_types::OneOf::Left(
                                                        lsp_types::TextEdit {
                                                            range: use_range_of_renamed_module,
                                                            new_text: rename_arguments.new_name.clone(),
                                                        },
                                                    ),
                                                )
                                                .collect::<Vec<_>>(),
                                        })
                                }).collect::<Vec<_>>()
                            },
                        })
                    });
            async move {
                Ok(maybe_rename_edits.map(|rename_edits| lsp_types::WorkspaceEdit {
                    changes: None,
                    document_changes: Some(lsp_types::DocumentChanges::Edits(rename_edits)),
                    change_annotations: None,
                }))
            }
        }).request::<lsp_types::request::SemanticTokensFullRequest, _>(|state, semantic_tokens_arguments| {
            let semantic_tokens =
                semantic_tokens_arguments
                    .text_document
                    .uri
                    .to_file_path()
                    .ok()
                    .and_then(|file_path| state.parsed_modules.get(&file_path).and_then(|m| m.syntax.as_ref()))
                    .and_then(|module_syntax| {
                        let highlight_allocator = bumpalo::Bump::new();
                        Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
                            result_id: None,
                            data: elm::elm_syntax_highlight_for(
                                &highlight_allocator,
                                elm_syntax_module_from_persistent(&highlight_allocator, &module_syntax),
                            )
                                .into_iter()
                                .scan(elm::TextGridLocation {
                                    line: 1,
                                    column: 1,
                                }, |previous_start_location, segment| {
                                    let delta =
                                        elm::exports_location_delta(*previous_start_location, segment.range.start);
                                    let token = lsp_types::SemanticToken {
                                        delta_line: delta.line as u32,
                                        delta_start: delta.column as u32,
                                        length: (segment.range.end.column - segment.range.start.column) as u32,
                                        token_type: semantic_token_type_to_id(
                                            elm_syntax_highlight_syntax_kind_to_lsp_semantic_token_type(
                                                segment.syntax_kind,
                                            ),
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
                        let parse_allocator: bumpalo::Bump = bumpalo::Bump::new();
                        state.parsed_modules.insert(changed_file_path, ModuleState {
                            syntax: elm::elm_parser_lenient_run(
                                elm::elm_parser_lenient_module_(&parse_allocator),
                                elm::StringString::One(&file_content),
                            ).map(elm_syntax_module_to_persistent),
                            source: file_content,
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
                        let parse_allocator: bumpalo::Bump = bumpalo::Bump::new();
                        state.parsed_modules.insert(changed_file_path, ModuleState {
                            syntax: elm::elm_parser_lenient_run(
                                elm::elm_parser_lenient_module_(&parse_allocator),
                                elm::StringString::One(&file_content),
                            ).map(elm_syntax_module_to_persistent),
                            source: file_content,
                        });
                    },
                }
            }
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidSaveTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::DidCloseTextDocument>(|_state, _| {
            std::ops::ControlFlow::Continue(())
        }).notification::<lsp_types::notification::Exit>(|_state, _| {
            // ?
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

fn documentation_comment_to_markdown(documentation: &str) -> String {
    let markdown_source = documentation.trim_start_matches("{-|").trim_end_matches("-}").trim();

    // because I don't want to introduce a full markdown parser for just this tiny
    // improvement, the code below only approximates where code blocks are.
    let with_fenced_blocks_converted: String = markdown_convert_unspecific_fenced_code_blocks_to_elm(markdown_source);
    markdown_convert_indented_code_blocks_to_elm(&with_fenced_blocks_converted)
}

/// replace fenced no-language-specified code blocks by `elm...`
fn markdown_convert_unspecific_fenced_code_blocks_to_elm(markdown_source: &str) -> String {
    let mut result_builder: String = String::new();
    let mut current_source_index: usize = 0;
    'converting_fenced: while current_source_index <= (markdown_source.len() - 1) {
        match markdown_source[current_source_index..].find("```").map(|i| i + current_source_index) {
            None => {
                result_builder.push_str(&markdown_source[current_source_index..]);
                break 'converting_fenced
            },
            Some(index_at_opening_fence) => {
                let index_after_opening_fence = index_at_opening_fence + 3;
                match markdown_source[index_after_opening_fence..]
                    .find("```")
                    .map(|i| i + index_after_opening_fence) {
                    None => {
                        result_builder.push_str(&markdown_source[current_source_index..]);
                        break 'converting_fenced
                    },
                    Some(index_at_closing_fence) => {
                        match markdown_source[index_after_opening_fence..].chars().next() {
                            // fenced block without a specific language
                            Some('\n') => {
                                result_builder.push_str(
                                    &markdown_source[current_source_index .. index_at_opening_fence],
                                );
                                result_builder.push_str("```elm");
                                result_builder.push_str(
                                    &markdown_source[index_after_opening_fence .. index_at_closing_fence],
                                );
                                result_builder.push_str("```");
                                current_source_index = index_at_closing_fence + 3;
                            },
                            // fenced block with a specific language
                            _ => {
                                result_builder.push_str(
                                    &markdown_source[current_source_index .. (index_at_closing_fence + 3)],
                                );
                                current_source_index = index_at_closing_fence + 3;
                            },
                        }
                    },
                }
            },
        }
    }
    result_builder
}

fn markdown_convert_indented_code_blocks_to_elm(markdown_source: &str) -> String {
    let mut result_builder: String = String::new();
    let mut current_indent: usize = 0;
    let mut is_in_code_block = false;
    for source_line in markdown_source.lines() {
        let current_line_indent = source_line.chars().take_while(|c| c.is_ascii_whitespace()).count();
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

fn state_file_path_for_module_name(state: &State, module_name: &str) -> Option<std::path::PathBuf> {
    // can be incorrect if multiple packages internal module names and or the
    // project's module names overlap. To prevent that, it would help immensely if
    // State was instead grouped by project, so { projects : Map<Path, { elm_json,
    // modules: Map<Path, Module_State> }> }
    let possible_file_paths: Vec<std::path::PathBuf> =
        state
            .elm_jsons_source_directories
            .iter()
            .flat_map(
                |(elm_json_parent_path, source_directories)| source_directories
                    .iter()
                    .flat_map(
                        |source_directory_path| [
                            std::path::Path::new(elm_json_parent_path)
                                .join(source_directory_path)
                                .join(module_name.replace(".", "/"))
                                .with_extension("elm"),
                            std::path::Path::new(elm_json_parent_path)
                                .join(source_directory_path)
                                .join(module_name.replace(".", "/"))
                                .with_extension("elm-testing"),
                        ],
                    ),
            )
            .collect::<Vec<_>>();
    state
        .parsed_modules
        .keys()
        .find(|file_path| possible_file_paths.iter().any(|possibility| &possibility == file_path))
        .map(|path| path.clone())
}

fn lsp_range_includes_position(range: lsp_types::Range, position: lsp_types::Position) -> bool {
    // position >= range.start
    ((position.line > range.start.line) ||
        ((position.line == range.start.line) && (position.character >= range.start.character))) &&
        // position <= range.end
        ((position.line < range.end.line) ||
            ((position.line == range.end.line) && (position.character <= range.end.character)))
}

fn lsp_position_add_characters(position: lsp_types::Position, additional_character_count: i32) -> lsp_types::Position {
    lsp_types::Position {
        line: position.line,
        character: (position.character as i32 + additional_character_count) as u32,
    }
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

fn lsp_position_to_text_grid_location(lsp_position: lsp_types::Position) -> elm::TextGridLocation {
    elm::GeneratedColumnLine {
        line: (lsp_position.line + 1) as i64,
        column: (lsp_position.character + 1) as i64,
    }
}

fn lsp_range_to_text_grid_range(lsp_range: lsp_types::Range) -> elm::TextGridRange {
    elm::GeneratedEndStart {
        start: lsp_position_to_text_grid_location(lsp_range.start),
        end: lsp_position_to_text_grid_location(lsp_range.end),
    }
}

fn text_grid_location_to_lsp_position(text_grid_location: elm::TextGridLocation) -> lsp_types::Position {
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

// // // below persistent rust types and conversions to and from temporary elm
// types
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
            elm
            ::GeneratedColonKeySymbolRangeNameValue<
                lsp_types::Range,
                ElmSyntaxNode<String>,
                ElmSyntaxNode<ElmSyntaxType>,
            >,
        >,
    ),
    RecordExtension {
        record_variable: ElmSyntaxNode<String>,
        bar_key_symbol_range: lsp_types::Range,
        field0: elm
        ::GeneratedColonKeySymbolRangeNameValue<
            lsp_types::Range,
            ElmSyntaxNode<String>,
            ElmSyntaxNode<Box<ElmSyntaxType>>,
        >,
        field1_up: Vec<
            elm
            ::GeneratedColonKeySymbolRangeNameValue<
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
        signature: Option<elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>>,
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
        case0: elm
        ::GeneratedArrowKeySymbolRangePatternResult<
            lsp_types::Range,
            ElmSyntaxNode<ElmSyntaxPattern>,
            ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        >,
        case1_up: Vec<
            elm
            ::GeneratedArrowKeySymbolRangePatternResult<
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
            elm
            ::GeneratedEqualsKeySymbolRangeNameValue<
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
        field0: elm
        ::GeneratedEqualsKeySymbolRangeNameValue<
            lsp_types::Range,
            ElmSyntaxNode<String>,
            ElmSyntaxNode<Box<ElmSyntaxExpression>>,
        >,
        field1_up: Vec<
            elm
            ::GeneratedEqualsKeySymbolRangeNameValue<
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
        variant0: elm::GeneratedNameValues<ElmSyntaxNode<String>, Vec<ElmSyntaxNode<ElmSyntaxType>>>,
        variant1_up: Vec<
            elm
            ::GeneratedNameOrKeySymbolRangeValues<
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
        signature: Option<elm::GeneratedNameType0<ElmSyntaxNode<String>, ElmSyntaxNode<ElmSyntaxType>>>,
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

fn elm_syntax_node_as_ref<'a, Value>(elm_syntax_node: &'a ElmSyntaxNode<Value>) -> ElmSyntaxNode<&'a Value> {
    ElmSyntaxNode {
        range: elm_syntax_node.range,
        value: &elm_syntax_node.value,
    }
}

fn elm_syntax_node_unbox<
    'a,
    Value,
>(elm_syntax_node_box: &'a ElmSyntaxNode<Box<Value>>) -> ElmSyntaxNode<&'a Value> {
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
        elm::GeneratedDeclarationDocumentation<ElmSyntaxNode<ElmSyntaxDeclaration>, Option<ElmSyntaxNode<String>>>,
    >,
}

#[derive(Clone, Debug, PartialEq)]
struct ElmSyntaxImport {
    module_name: ElmSyntaxNode<String>,
    alias: Option<elm::GeneratedAsKeywordRangeName<lsp_types::Range, ElmSyntaxNode<String>>>,
    exposing: Option<ElmSyntaxNode<ElmSyntaxExposing>>,
}

fn elm_syntax_node_from_persistent<
    Value,
    ValuePersistent,
>(
    elm_syntax_node: ElmSyntaxNode<ValuePersistent>,
    value_from_persistent: impl Fn(ValuePersistent) -> Value,
) -> elm::ElmSyntaxNode<Value> {
    elm::GeneratedRangeValue {
        range: lsp_range_to_text_grid_range(elm_syntax_node.range),
        value: value_from_persistent(elm_syntax_node.value),
    }
}

fn elm_syntax_node_from_persistent_ref<
    'a,
    Value,
    ValuePersistent,
>(
    elm_syntax_node: &'a ElmSyntaxNode<ValuePersistent>,
    value_from_persistent: impl Fn(&'a ValuePersistent) -> Value,
) -> elm::ElmSyntaxNode<Value> {
    elm::GeneratedRangeValue {
        range: lsp_range_to_text_grid_range(elm_syntax_node.range),
        value: value_from_persistent(&elm_syntax_node.value),
    }
}

fn elm_syntax_node_string_from_persistent<
    'a,
>(elm_syntax_node: &'a ElmSyntaxNode<String>) -> elm::ElmSyntaxNode<elm::StringString<'a>> {
    elm_syntax_node_from_persistent_ref(elm_syntax_node, |value| elm::StringString::One(value))
}

fn elm_syntax_module_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_module_persistent: &'a ElmSyntaxModule) -> elm::ElmSyntaxModule<'a> {
    elm::GeneratedCommentsDeclarationsHeaderImports {
        header: elm_syntax_node_from_persistent_ref(
            &elm_syntax_module_persistent.header,
            |header| elm_syntax_module_header_from_persistent(allocator, header),
        ),
        imports: elm::double_ended_iterator_to_list(
            allocator,
            elm_syntax_module_persistent
                .imports
                .iter()
                .map(
                    |import_node| elm_syntax_node_from_persistent_ref(
                        import_node,
                        |import| elm_syntax_import_from_persistent(allocator, import),
                    ),
                ),
        ),
        comments: elm::double_ended_iterator_to_list(
            allocator,
            elm_syntax_module_persistent
                .comments
                .iter()
                .map(|comment_node| elm_syntax_node_string_from_persistent(comment_node)),
        ),
        declarations: elm::double_ended_iterator_to_list(
            allocator,
            elm_syntax_module_persistent
                .declarations
                .iter()
                .map(|documented_declaration_node| elm::GeneratedDeclarationDocumentation {
                    documentation: documented_declaration_node
                        .documentation
                        .as_ref()
                        .map(|documentation_node| elm_syntax_node_string_from_persistent(documentation_node)),
                    declaration: elm_syntax_node_from_persistent_ref(
                        &documented_declaration_node.declaration,
                        |declaration| elm_syntax_declaration_from_persistent(allocator, declaration),
                    ),
                }),
        ),
    }
}

fn elm_syntax_import_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_import: &'a ElmSyntaxImport) -> elm::ElmSyntaxImport<'a> {
    elm::GeneratedAliasExposing0ModuleName {
        module_name: elm_syntax_node_string_from_persistent(&elm_syntax_import.module_name),
        alias: elm_syntax_import.alias.as_ref().map(|alias| elm::GeneratedAsKeywordRangeName {
            as_keyword_range: lsp_range_to_text_grid_range(alias.as_keyword_range),
            name: elm_syntax_node_string_from_persistent(&alias.name),
        }),
        exposing_: elm_syntax_import
            .exposing
            .as_ref()
            .map(
                |exposing_node| elm_syntax_node_from_persistent_ref(
                    exposing_node,
                    |exposing| elm_syntax_exposing_from_persistent(allocator, exposing),
                ),
            ),
    }
}

fn elm_syntax_declaration_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_declaration_persistent: &'a ElmSyntaxDeclaration,
) -> elm::ElmSyntaxDeclaration<'a> {
    match elm_syntax_declaration_persistent {
        ElmSyntaxDeclaration::ChoiceType { name, parameters, equals_key_symbol_range, variant0, variant1_up } => elm
        ::ElmSyntaxDeclaration
        ::DeclarationChoiceType(
            elm::GeneratedEqualsKeySymbolRangeNameParametersVariant0Variant1Up {
                name: elm_syntax_node_string_from_persistent(name),
                parameters: elm::double_ended_iterator_to_list(
                    allocator,
                    parameters.iter().map(elm_syntax_node_string_from_persistent),
                ),
                equals_key_symbol_range: lsp_range_to_text_grid_range(*equals_key_symbol_range),
                variant0: elm::GeneratedNameValues {
                    name: elm_syntax_node_string_from_persistent(&variant0.name),
                    values: elm::double_ended_iterator_to_list(
                        allocator,
                        variant0
                            .values
                            .iter()
                            .map(|variant_value| elm_syntax_node_type_from_persistent(allocator, variant_value)),
                    ),
                },
                variant1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    variant1_up.iter().map(|variant| elm::GeneratedNameOrKeySymbolRangeValues {
                        or_key_symbol_range: lsp_range_to_text_grid_range(variant.or_key_symbol_range),
                        name: elm_syntax_node_string_from_persistent(&variant.name),
                        values: elm::double_ended_iterator_to_list(
                            allocator,
                            variant
                                .values
                                .iter()
                                .map(|variant_value| elm_syntax_node_type_from_persistent(allocator, variant_value)),
                        ),
                    }),
                ),
            },
        ),
        ElmSyntaxDeclaration::Operator { precedence, operator, function, direction } => elm
        ::ElmSyntaxDeclaration
        ::DeclarationOperator(
            elm::GeneratedDirectionFunctionOperatorPrecedence {
                precedence: elm_syntax_node_from_persistent(*precedence, |precedence| precedence),
                direction: elm_syntax_node_from_persistent(*direction, |direction| direction),
                operator: elm_syntax_node_string_from_persistent(operator),
                function: elm_syntax_node_string_from_persistent(function),
            },
        ),
        ElmSyntaxDeclaration::Port { name, type_ } => elm::ElmSyntaxDeclaration::DeclarationPort(
            elm::GeneratedNameType0 {
                name: elm_syntax_node_string_from_persistent(name),
                type_1: elm_syntax_node_type_from_persistent(allocator, type_),
            },
        ),
        ElmSyntaxDeclaration
        ::TypeAlias {
            alias_keyword_range,
            name,
            parameters,
            equals_key_symbol_range,
            type_,
        } => elm
        ::ElmSyntaxDeclaration
        ::DeclarationTypeAlias(
            elm::GeneratedAliasKeywordRangeEqualsKeySymbolRangeNameParametersType0 {
                name: elm_syntax_node_string_from_persistent(name),
                alias_keyword_range: lsp_range_to_text_grid_range(*alias_keyword_range),
                parameters: elm::double_ended_iterator_to_list(
                    allocator,
                    parameters.iter().map(|parameter_node| elm_syntax_node_string_from_persistent(parameter_node)),
                ),
                equals_key_symbol_range: lsp_range_to_text_grid_range(*equals_key_symbol_range),
                type_1: elm_syntax_node_type_from_persistent(allocator, type_),
            },
        ),
        ElmSyntaxDeclaration
        ::ValueOrFunction {
            name,
            signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range,
            result,
        } => elm
        ::ElmSyntaxDeclaration
        ::DeclarationValueOrFunction(
            elm::GeneratedEqualsKeySymbolRangeImplementationNameRangeNameParametersResultSignature {
                name: elm::StringString::One(name),
                signature: signature.as_ref().map(|signature| elm::GeneratedNameType0 {
                    name: elm_syntax_node_string_from_persistent(&signature.name),
                    type_1: elm_syntax_node_type_from_persistent(allocator, &signature.type_1),
                }),
                implementation_name_range: lsp_range_to_text_grid_range(*implementation_name_range),
                parameters: elm::double_ended_iterator_to_list(
                    allocator,
                    parameters
                        .iter()
                        .map(|parameter_node| elm_syntax_node_pattern_from_persistent(allocator, parameter_node)),
                ),
                equals_key_symbol_range: lsp_range_to_text_grid_range(*equals_key_symbol_range),
                result: elm_syntax_node_expression_from_persistent(allocator, result),
            },
        ),
    }
}

fn elm_syntax_node_type_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_type_node: &'a ElmSyntaxNode<ElmSyntaxType>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxType<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_type_node,
        |type_| elm_syntax_type_from_persistent(allocator, type_),
    )
}

fn elm_syntax_node_type_from_persistent_box<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_type_node: &'a ElmSyntaxNode<Box<ElmSyntaxType>>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxType<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_type_node,
        |type_| elm_syntax_type_from_persistent(allocator, type_),
    )
}

fn elm_syntax_node_pattern_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_pattern_node: &'a ElmSyntaxNode<ElmSyntaxPattern>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxPattern<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_pattern_node,
        |pattern| elm_syntax_pattern_from_persistent(allocator, pattern),
    )
}

fn elm_syntax_node_pattern_from_persistent_box<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_pattern_node: &'a ElmSyntaxNode<Box<ElmSyntaxPattern>>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxPattern<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_pattern_node,
        |pattern| elm_syntax_pattern_from_persistent(allocator, pattern),
    )
}

fn elm_syntax_node_expression_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_expression_node: &'a ElmSyntaxNode<ElmSyntaxExpression>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxExpression<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_expression_node,
        |pattern| elm_syntax_expression_from_persistent(allocator, pattern),
    )
}

fn elm_syntax_node_expression_from_persistent_box<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_expression_node: &'a ElmSyntaxNode<Box<ElmSyntaxExpression>>,
) -> elm::ElmSyntaxNode<elm::ElmSyntaxExpression<'a>> {
    elm_syntax_node_from_persistent_ref(
        elm_syntax_expression_node,
        |pattern| elm_syntax_expression_from_persistent(allocator, pattern),
    )
}

fn elm_syntax_pattern_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_pattern: &'a ElmSyntaxPattern) -> elm::ElmSyntaxPattern<'a> {
    match elm_syntax_pattern {
        ElmSyntaxPattern::As { pattern, as_keyword_range, variable } => elm::ElmSyntaxPattern::PatternAs(
            allocator.alloc(elm::GeneratedAsKeywordRangePatternVariable {
                pattern: elm_syntax_node_pattern_from_persistent_box(allocator, pattern),
                as_keyword_range: lsp_range_to_text_grid_range(*as_keyword_range),
                variable: elm_syntax_node_string_from_persistent(variable),
            }),
        ),
        ElmSyntaxPattern::Char(char) => elm::ElmSyntaxPattern::PatternChar(*char),
        ElmSyntaxPattern::Ignored => elm::ElmSyntaxPattern::PatternIgnored,
        ElmSyntaxPattern::Int { base, value } => elm::ElmSyntaxPattern::PatternInt(elm::GeneratedBaseValue {
            value: *value,
            base: *base,
        }),
        ElmSyntaxPattern::ListCons { cons_key_symbol, head, tail } => elm::ElmSyntaxPattern::PatternListCons(
            allocator.alloc(elm::GeneratedConsKeySymbolRangeHeadTail {
                head: elm_syntax_node_pattern_from_persistent_box(allocator, head),
                cons_key_symbol_range: lsp_range_to_text_grid_range(*cons_key_symbol),
                tail: elm_syntax_node_pattern_from_persistent_box(allocator, tail),
            }),
        ),
        ElmSyntaxPattern::ListExact(elements) => elm::ElmSyntaxPattern::PatternListExact(
            allocator.alloc(
                elm::double_ended_iterator_to_list(
                    allocator,
                    elements.iter().map(|element| elm_syntax_node_pattern_from_persistent(allocator, element)),
                ),
            ),
        ),
        ElmSyntaxPattern::Parenthesized(in_parens) => elm::ElmSyntaxPattern::PatternParenthesized(
            allocator.alloc(elm_syntax_node_pattern_from_persistent_box(allocator, in_parens)),
        ),
        ElmSyntaxPattern::Record(fields) => elm::ElmSyntaxPattern::PatternRecord(
            elm::double_ended_iterator_to_list(
                allocator,
                fields.iter().map(|field| elm_syntax_node_string_from_persistent(field)),
            ),
        ),
        ElmSyntaxPattern::String { content, quoting_style } => elm::ElmSyntaxPattern::PatternString(
            elm::GeneratedContentQuotingStyle {
                quoting_style: *quoting_style,
                content: elm::StringString::One(content),
            },
        ),
        ElmSyntaxPattern::Triple { part0, part1, part2 } => elm::ElmSyntaxPattern::PatternTriple(
            allocator.alloc(elm::GeneratedPart0Part1Part2 {
                part0: elm_syntax_node_pattern_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_pattern_from_persistent_box(allocator, part1),
                part2: elm_syntax_node_pattern_from_persistent_box(allocator, part2),
            }),
        ),
        ElmSyntaxPattern::Tuple { part0, part1 } => elm::ElmSyntaxPattern::PatternTuple(
            allocator.alloc(elm::GeneratedPart0Part1 {
                part0: elm_syntax_node_pattern_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_pattern_from_persistent_box(allocator, part1),
            }),
        ),
        ElmSyntaxPattern::Unit => elm::ElmSyntaxPattern::PatternUnit,
        ElmSyntaxPattern::Variable(name) => elm::ElmSyntaxPattern::PatternVariable(elm::StringString::One(name)),
        ElmSyntaxPattern::Variant { reference: reference_node, values } => elm::ElmSyntaxPattern::PatternVariant(
            allocator.alloc(elm::GeneratedReferenceValues {
                reference: elm_syntax_node_from_persistent_ref(
                    reference_node,
                    |reference| elm::GeneratedNameQualification {
                        qualification: elm::StringString::One(&reference.qualification),
                        name: elm::StringString::One(&reference.name),
                    },
                ),
                values: elm::double_ended_iterator_to_list(
                    allocator,
                    values.iter().map(|value| elm_syntax_node_pattern_from_persistent(allocator, value)),
                ),
            }),
        ),
    }
}

fn elm_syntax_expression_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_expression: &'a ElmSyntaxExpression) -> elm::ElmSyntaxExpression<'a> {
    match elm_syntax_expression {
        ElmSyntaxExpression::Call { called, argument0, argument1_up } => elm::ElmSyntaxExpression::ExpressionCall(
            allocator.alloc(elm::GeneratedArgument0Argument1UpCalled {
                called: elm_syntax_node_expression_from_persistent_box(allocator, called),
                argument0: elm_syntax_node_expression_from_persistent_box(allocator, argument0),
                argument1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    argument1_up
                        .iter()
                        .map(|argument| elm_syntax_node_expression_from_persistent(allocator, argument)),
                ),
            }),
        ),
        ElmSyntaxExpression::CaseOf { matched, of_keyword_range, case0, case1_up } => elm
        ::ElmSyntaxExpression
        ::ExpressionCaseOf(
            allocator.alloc(elm::GeneratedCase0Case1UpMatchedOfKeywordRange {
                matched: elm_syntax_node_expression_from_persistent_box(allocator, matched),
                of_keyword_range: lsp_range_to_text_grid_range(*of_keyword_range),
                case0: elm::GeneratedArrowKeySymbolRangePatternResult {
                    pattern: elm_syntax_node_pattern_from_persistent(allocator, &case0.pattern),
                    arrow_key_symbol_range: lsp_range_to_text_grid_range(case0.arrow_key_symbol_range),
                    result: elm_syntax_node_expression_from_persistent_box(allocator, &case0.result),
                },
                case1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    case1_up.iter().map(|case| elm::GeneratedArrowKeySymbolRangePatternResult {
                        pattern: elm_syntax_node_pattern_from_persistent(allocator, &case.pattern),
                        arrow_key_symbol_range: lsp_range_to_text_grid_range(case.arrow_key_symbol_range),
                        result: elm_syntax_node_expression_from_persistent(allocator, &case.result),
                    }),
                ),
            }),
        ),
        ElmSyntaxExpression::Char(char) => elm::ElmSyntaxExpression::ExpressionChar(*char),
        ElmSyntaxExpression::Float(float) => elm::ElmSyntaxExpression::ExpressionFloat(*float),
        ElmSyntaxExpression::IfThenElse { condition, then_keyword_range, on_true, else_keyword_range, on_false } => elm
        ::ElmSyntaxExpression
        ::ExpressionIfThenElse(
            allocator.alloc(elm::GeneratedConditionElseKeywordRangeOnFalseOnTrueThenKeywordRange {
                condition: elm_syntax_node_expression_from_persistent_box(allocator, condition),
                then_keyword_range: lsp_range_to_text_grid_range(*then_keyword_range),
                on_true: elm_syntax_node_expression_from_persistent_box(allocator, on_true),
                else_keyword_range: lsp_range_to_text_grid_range(*else_keyword_range),
                on_false: elm_syntax_node_expression_from_persistent_box(allocator, on_false),
            }),
        ),
        ElmSyntaxExpression::InfixOperation { left, operator, right } => elm
        ::ElmSyntaxExpression
        ::ExpressionInfixOperation(
            allocator.alloc(elm::GeneratedLeftOperatorRight {
                left: elm_syntax_node_expression_from_persistent_box(allocator, left),
                operator: elm_syntax_node_string_from_persistent(operator),
                right: elm_syntax_node_expression_from_persistent_box(allocator, right),
            }),
        ),
        ElmSyntaxExpression::Integer { value, base } => elm::ElmSyntaxExpression::ExpressionInteger(
            elm::GeneratedBaseValue {
                base: *base,
                value: *value,
            },
        ),
        ElmSyntaxExpression::Lambda { arrow_key_symbol_range, parameter0, parameter1_up, result } => elm
        ::ElmSyntaxExpression
        ::ExpressionLambda(
            allocator.alloc(elm::GeneratedArrowKeySymbolRangeParameter0Parameter1UpResult {
                parameter0: elm_syntax_node_pattern_from_persistent(allocator, parameter0),
                parameter1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    parameter1_up
                        .iter()
                        .map(|parameter| elm_syntax_node_pattern_from_persistent(allocator, parameter)),
                ),
                arrow_key_symbol_range: lsp_range_to_text_grid_range(*arrow_key_symbol_range),
                result: elm_syntax_node_expression_from_persistent_box(allocator, result),
            }),
        ),
        ElmSyntaxExpression
        ::LetIn {
            declaration0: declaration0_node,
            declaration1_up,
            in_keyword_range,
            result,
        } => elm
        ::ElmSyntaxExpression
        ::ExpressionLetIn(
            allocator.alloc(elm::GeneratedDeclaration0Declaration1UpInKeywordRangeResult {
                declaration0: elm_syntax_node_from_persistent_ref(
                    declaration0_node,
                    |declaration| elm_syntax_let_declaration_from_persistent(allocator, declaration),
                ),
                declaration1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    declaration1_up
                        .iter()
                        .map(
                            |declaration_node| elm_syntax_node_from_persistent_ref(
                                declaration_node,
                                |declaration| elm_syntax_let_declaration_from_persistent(allocator, declaration),
                            ),
                        ),
                ),
                in_keyword_range: lsp_range_to_text_grid_range(*in_keyword_range),
                result: elm_syntax_node_expression_from_persistent_box(allocator, result),
            }),
        ),
        ElmSyntaxExpression::List(elements) => elm::ElmSyntaxExpression::ExpressionList(
            allocator.alloc(
                elm::double_ended_iterator_to_list(
                    allocator,
                    elements.iter().map(|element| elm_syntax_node_expression_from_persistent(allocator, element)),
                ),
            ),
        ),
        ElmSyntaxExpression::Negation(in_negation) => elm::ElmSyntaxExpression::ExpressionNegation(
            allocator.alloc(elm_syntax_node_expression_from_persistent_box(allocator, in_negation)),
        ),
        ElmSyntaxExpression::OperatorFunction(operator) => elm::ElmSyntaxExpression::ExpressionOperatorFunction(
            elm::StringString::One(operator),
        ),
        ElmSyntaxExpression::Parenthesized(in_parens) => elm::ElmSyntaxExpression::ExpressionParenthesized(
            allocator.alloc(elm_syntax_node_expression_from_persistent_box(allocator, in_parens)),
        ),
        ElmSyntaxExpression::Record(fields) => elm::ElmSyntaxExpression::ExpressionRecord(
            allocator.alloc(
                elm::double_ended_iterator_to_list(
                    allocator,
                    fields.iter().map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_from_persistent(&field.name),
                        equals_key_symbol_range: lsp_range_to_text_grid_range(field.equals_key_symbol_range),
                        value: elm_syntax_node_expression_from_persistent(allocator, &field.value),
                    }),
                ),
            ),
        ),
        ElmSyntaxExpression::RecordAccess { record, field } => elm::ElmSyntaxExpression::ExpressionRecordAccess(
            allocator.alloc(elm::GeneratedFieldRecord {
                record: elm_syntax_node_expression_from_persistent_box(allocator, record),
                field: elm_syntax_node_string_from_persistent(field),
            }),
        ),
        ElmSyntaxExpression::RecordAccessFunction(field) => elm::ElmSyntaxExpression::ExpressionRecordAccessFunction(
            elm::StringString::One(field),
        ),
        ElmSyntaxExpression::RecordUpdate { record_variable, bar_key_symbol_range, field0, field1_up } => elm
        ::ElmSyntaxExpression
        ::ExpressionRecordUpdate(
            allocator.alloc(elm::GeneratedBarKeySymbolRangeField0Field1UpRecordVariable {
                record_variable: elm_syntax_node_string_from_persistent(record_variable),
                bar_key_symbol_range: lsp_range_to_text_grid_range(*bar_key_symbol_range),
                field0: elm::GeneratedEqualsKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_from_persistent(&field0.name),
                    equals_key_symbol_range: lsp_range_to_text_grid_range(field0.equals_key_symbol_range),
                    value: elm_syntax_node_expression_from_persistent_box(allocator, &field0.value),
                },
                field1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    field1_up.iter().map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_from_persistent(&field.name),
                        equals_key_symbol_range: lsp_range_to_text_grid_range(field.equals_key_symbol_range),
                        value: elm_syntax_node_expression_from_persistent(allocator, &field.value),
                    }),
                ),
            }),
        ),
        ElmSyntaxExpression::Reference(reference) => elm::ElmSyntaxExpression::ExpressionReference(
            elm::GeneratedNameQualification {
                qualification: elm::StringString::One(&reference.qualification),
                name: elm::StringString::One(&reference.name),
            },
        ),
        ElmSyntaxExpression::String { content, quoting_style } => elm::ElmSyntaxExpression::ExpressionString(
            elm::GeneratedContentQuotingStyle {
                content: elm::StringString::One(content),
                quoting_style: *quoting_style,
            },
        ),
        ElmSyntaxExpression::Triple { part0, part1, part2 } => elm::ElmSyntaxExpression::ExpressionTriple(
            allocator.alloc(elm::GeneratedPart0Part1Part2 {
                part0: elm_syntax_node_expression_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_expression_from_persistent_box(allocator, part1),
                part2: elm_syntax_node_expression_from_persistent_box(allocator, part2),
            }),
        ),
        ElmSyntaxExpression::Tuple { part0, part1 } => elm::ElmSyntaxExpression::ExpressionTuple(
            allocator.alloc(elm::GeneratedPart0Part1 {
                part0: elm_syntax_node_expression_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_expression_from_persistent_box(allocator, part1),
            }),
        ),
        ElmSyntaxExpression::Unit => elm::ElmSyntaxExpression::ExpressionUnit,
    }
}

fn elm_syntax_let_declaration_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_let_declaration: &'a ElmSyntaxLetDeclaration,
) -> elm::ElmSyntaxLetDeclaration<'a> {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring { pattern, equals_key_symbol_range, expression } => elm
        ::ElmSyntaxLetDeclaration
        ::LetDestructuring(
            allocator.alloc(elm::GeneratedEqualsKeySymbolRangeExpressionPattern {
                pattern: elm_syntax_node_pattern_from_persistent(allocator, pattern),
                equals_key_symbol_range: lsp_range_to_text_grid_range(*equals_key_symbol_range),
                expression: elm_syntax_node_expression_from_persistent(allocator, expression),
            }),
        ),
        ElmSyntaxLetDeclaration
        ::ValueOrFunctionDeclaration {
            name,
            signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range,
            result,
        } => elm
        ::ElmSyntaxLetDeclaration
        ::LetValueOrFunctionDeclaration(
            allocator.alloc(elm::GeneratedEqualsKeySymbolRangeImplementationNameRangeNameParametersResultSignature {
                name: elm::StringString::One(name),
                signature: signature.as_ref().map(|signature| elm::GeneratedNameType0 {
                    name: elm_syntax_node_string_from_persistent(&signature.name),
                    type_1: elm_syntax_node_type_from_persistent(allocator, &signature.type_1),
                }),
                implementation_name_range: lsp_range_to_text_grid_range(*implementation_name_range),
                parameters: elm::double_ended_iterator_to_list(
                    allocator,
                    parameters
                        .iter()
                        .map(|parameter_node| elm_syntax_node_pattern_from_persistent(allocator, parameter_node)),
                ),
                equals_key_symbol_range: lsp_range_to_text_grid_range(*equals_key_symbol_range),
                result: elm_syntax_node_expression_from_persistent(allocator, result),
            }),
        ),
    }
}

fn elm_syntax_type_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_type: &'a ElmSyntaxType) -> elm::ElmSyntaxType<'a> {
    match elm_syntax_type {
        ElmSyntaxType::Construct { reference: reference_node, arguments } => elm::ElmSyntaxType::TypeConstruct(
            allocator.alloc(elm::GeneratedArgumentsReference {
                reference: elm_syntax_node_from_persistent_ref(
                    reference_node,
                    |reference| elm::GeneratedNameQualification {
                        qualification: elm::StringString::One(&reference.qualification),
                        name: elm::StringString::One(&reference.name),
                    },
                ),
                arguments: elm::double_ended_iterator_to_list(
                    allocator,
                    arguments
                        .iter()
                        .map(|argument_node| elm_syntax_node_type_from_persistent(allocator, argument_node)),
                ),
            }),
        ),
        ElmSyntaxType::Function { input, arrow_key_symbol_range, output } => elm::ElmSyntaxType::TypeFunction(
            allocator.alloc(elm::GeneratedArrowKeySymbolRangeInputOutput {
                input: elm_syntax_node_type_from_persistent_box(allocator, input),
                arrow_key_symbol_range: (lsp_range_to_text_grid_range(*arrow_key_symbol_range)),
                output: elm_syntax_node_type_from_persistent_box(allocator, output),
            }),
        ),
        ElmSyntaxType::Parenthesized(in_parens) => elm::ElmSyntaxType::TypeParenthesized(
            allocator.alloc(elm_syntax_node_type_from_persistent_box(allocator, in_parens)),
        ),
        ElmSyntaxType::Record(fields) => elm::ElmSyntaxType::TypeRecord(
            allocator.alloc(
                elm::double_ended_iterator_to_list(
                    allocator,
                    fields.iter().map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_from_persistent(&field.name),
                        colon_key_symbol_range: lsp_range_to_text_grid_range(field.colon_key_symbol_range),
                        value: elm_syntax_node_type_from_persistent(allocator, &field.value),
                    }),
                ),
            ),
        ),
        ElmSyntaxType::RecordExtension { record_variable, bar_key_symbol_range, field0, field1_up } => elm
        ::ElmSyntaxType
        ::TypeRecordExtension(
            allocator.alloc(elm::GeneratedBarKeySymbolRangeField0Field1UpRecordVariable {
                record_variable: elm_syntax_node_string_from_persistent(record_variable),
                bar_key_symbol_range: lsp_range_to_text_grid_range(*bar_key_symbol_range),
                field0: elm::GeneratedColonKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_from_persistent(&field0.name),
                    colon_key_symbol_range: lsp_range_to_text_grid_range(field0.colon_key_symbol_range),
                    value: elm_syntax_node_type_from_persistent_box(allocator, &field0.value),
                },
                field1_up: elm::double_ended_iterator_to_list(
                    allocator,
                    field1_up.iter().map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                        name: elm_syntax_node_string_from_persistent(&field.name),
                        colon_key_symbol_range: lsp_range_to_text_grid_range(field.colon_key_symbol_range),
                        value: elm_syntax_node_type_from_persistent(allocator, &field.value),
                    }),
                ),
            }),
        ),
        ElmSyntaxType::Triple { part0, part1, part2 } => elm::ElmSyntaxType::TypeTriple(
            allocator.alloc(elm::GeneratedPart0Part1Part2 {
                part0: elm_syntax_node_type_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_type_from_persistent_box(allocator, part1),
                part2: elm_syntax_node_type_from_persistent_box(allocator, part2),
            }),
        ),
        ElmSyntaxType::Tuple { part0, part1 } => elm::ElmSyntaxType::TypeTuple(
            allocator.alloc(elm::GeneratedPart0Part1 {
                part0: elm_syntax_node_type_from_persistent_box(allocator, part0),
                part1: elm_syntax_node_type_from_persistent_box(allocator, part1),
            }),
        ),
        ElmSyntaxType::Unit => elm::ElmSyntaxType::TypeUnit,
        ElmSyntaxType::Variable(name) => elm::ElmSyntaxType::TypeVariable(elm::StringString::One(name.as_ref())),
    }
}

fn elm_syntax_module_header_from_persistent<
    'a,
>(
    allocator: &'a bumpalo::Bump,
    elm_syntax_module_header_persistent: &'a ElmSyntaxModuleHeader,
) -> elm::ElmSyntaxModuleHeader<'a> {
    elm::GeneratedExposing0ModuleNameSpecific {
        module_name: elm_syntax_node_string_from_persistent(&elm_syntax_module_header_persistent.module_name),
        exposing_: elm_syntax_node_from_persistent_ref(
            &elm_syntax_module_header_persistent.exposing,
            |exposing| elm_syntax_exposing_from_persistent(allocator, exposing),
        ),
        specific: elm_syntax_module_header_persistent
            .specific
            .as_ref()
            .map(|ref specific| elm_syntax_module_header_specific_from_persistent(specific)),
    }
}

fn elm_syntax_exposing_from_persistent<
    'a,
>(allocator: &'a bumpalo::Bump, elm_syntax_exposing_persistent: &'a ElmSyntaxExposing) -> elm::ElmSyntaxExposing<'a> {
    match elm_syntax_exposing_persistent {
        ElmSyntaxExposing::All(range) => elm::ElmSyntaxExposing::ExposingAll(lsp_range_to_text_grid_range(*range)),
        ElmSyntaxExposing::Explicit(exposes) => elm::ElmSyntaxExposing::ExposingExplicit(
            elm::double_ended_iterator_to_list(
                allocator,
                exposes
                    .iter()
                    .map(
                        |expose_node| elm_syntax_node_from_persistent_ref(
                            expose_node,
                            |expose| elm_syntax_expose_from_persistent(expose),
                        ),
                    ),
            ),
        ),
    }
}

fn elm_syntax_expose_from_persistent<
    'a,
>(elm_syntax_expose_persistent: &'a ElmSyntaxExpose) -> elm::ElmSyntaxExpose<'a> {
    match elm_syntax_expose_persistent {
        ElmSyntaxExpose::ChoiceTypeIncludingVariants { name, open_range } => elm
        ::ElmSyntaxExpose
        ::ExposeChoiceTypeIncludingVariants(
            elm::GeneratedNameOpenRange {
                name: elm_syntax_node_string_from_persistent(name),
                open_range: lsp_range_to_text_grid_range(*open_range),
            },
        ),
        ElmSyntaxExpose::Operator(symbol) => elm::ElmSyntaxExpose::ExposeOperator(elm::StringString::One(symbol)),
        ElmSyntaxExpose::Type(name) => elm::ElmSyntaxExpose::ExposeTypeName(elm::StringString::One(name)),
        ElmSyntaxExpose::Variable(name) => elm::ElmSyntaxExpose::ExposeVariable(elm::StringString::One(name)),
    }
}

fn elm_syntax_module_header_specific_from_persistent<
    'a,
>(
    elm_syntax_module_header_specific_persistent: &'a ElmSyntaxModuleHeaderSpecific,
) -> elm::ElmSyntaxModuleHeaderSpecific<'a> {
    match elm_syntax_module_header_specific_persistent {
        ElmSyntaxModuleHeaderSpecific::Effect { module_keyword_range, command, subscription } => elm
        ::ElmSyntaxModuleHeaderSpecific
        ::ModuleHeaderSpecificEffect(
            elm::GeneratedCommandModuleKeywordRangeSubscription {
                module_keyword_range: lsp_range_to_text_grid_range(*module_keyword_range),
                command: command.as_ref().map(|name_node| elm_syntax_node_string_from_persistent(name_node)),
                subscription: subscription
                    .as_ref()
                    .map(|name_node| elm_syntax_node_string_from_persistent(name_node)),
            },
        ),
        ElmSyntaxModuleHeaderSpecific::Port { module_keyword_range } => elm
        ::ElmSyntaxModuleHeaderSpecific
        ::ModuleHeaderSpecificPort(
            elm::GeneratedModuleKeywordRange {
                module_keyword_range: lsp_range_to_text_grid_range(*module_keyword_range),
            },
        ),
    }
}

// //
fn elm_syntax_module_to_persistent(elm_syntax_module: elm::ElmSyntaxModule) -> ElmSyntaxModule {
    ElmSyntaxModule {
        header: elm_syntax_node_to_persistent(elm_syntax_module.header, elm_syntax_module_header_to_persistent),
        imports: elm_syntax_module
            .imports
            .into_iter()
            .map(
                |
                    import_node:
                        elm
                        ::GeneratedRangeValue<
                            elm
                            ::GeneratedEndStart<
                                elm::GeneratedColumnLine<i64, i64>,
                                elm::GeneratedColumnLine<i64, i64>,
                            >,
                            elm
                            ::GeneratedAliasExposing0ModuleName<
                                Option<
                                    elm
                                    ::GeneratedAsKeywordRangeName<
                                        elm
                                        ::GeneratedEndStart<
                                            elm::GeneratedColumnLine<i64, i64>,
                                            elm::GeneratedColumnLine<i64, i64>,
                                        >,
                                        elm
                                        ::GeneratedRangeValue<
                                            elm
                                            ::GeneratedEndStart<
                                                elm::GeneratedColumnLine<i64, i64>,
                                                elm::GeneratedColumnLine<i64, i64>,
                                            >,
                                            elm::StringString<'_>,
                                        >,
                                    >,
                                >,
                                Option<
                                    elm
                                    ::GeneratedRangeValue<
                                        elm
                                        ::GeneratedEndStart<
                                            elm::GeneratedColumnLine<i64, i64>,
                                            elm::GeneratedColumnLine<i64, i64>,
                                        >,
                                        elm::ElmSyntaxExposing<'_>,
                                    >,
                                >,
                                elm
                                ::GeneratedRangeValue<
                                    elm
                                    ::GeneratedEndStart<
                                        elm::GeneratedColumnLine<i64, i64>,
                                        elm::GeneratedColumnLine<i64, i64>,
                                    >,
                                    elm::StringString<'_>,
                                >,
                            >,
                        >,
                | elm_syntax_node_to_persistent(
                    import_node,
                    elm_syntax_import_to_persistent,
                ),
            )
            .collect::<Vec<_>>(),
        comments: elm_syntax_module
            .comments
            .into_iter()
            .map(elm_syntax_node_string_to_persistent)
            .collect::<Vec<_>>(),
        declarations: elm_syntax_module
            .declarations
            .into_iter()
            .map(|documented_declaration_node| elm::GeneratedDeclarationDocumentation {
                documentation: documented_declaration_node.documentation.map(elm_syntax_node_string_to_persistent),
                declaration: elm_syntax_node_to_persistent(
                    documented_declaration_node.declaration,
                    elm_syntax_declaration_to_persistent,
                ),
            })
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
        specific: elm_syntax_module_header.specific.map(elm_syntax_module_header_specific_to_persistent),
    }
}

fn elm_syntax_module_header_specific_to_persistent(
    elm_syntax_module_header_specific: elm::ElmSyntaxModuleHeaderSpecific,
) -> ElmSyntaxModuleHeaderSpecific {
    match elm_syntax_module_header_specific {
        elm::ElmSyntaxModuleHeaderSpecific::ModuleHeaderSpecificEffect(
            effect_module_header_specific,
        ) => ElmSyntaxModuleHeaderSpecific
        ::Effect {
            module_keyword_range: text_grid_range_to_lsp_range(effect_module_header_specific.module_keyword_range),
            command: effect_module_header_specific.command.map(elm_syntax_node_string_to_persistent),
            subscription: effect_module_header_specific.subscription.map(elm_syntax_node_string_to_persistent),
        },
        elm::ElmSyntaxModuleHeaderSpecific::ModuleHeaderSpecificPort(
            port_module_header_specific,
        ) => ElmSyntaxModuleHeaderSpecific
        ::Port {
            module_keyword_range: text_grid_range_to_lsp_range(port_module_header_specific.module_keyword_range),
        },
    }
}

fn elm_syntax_import_to_persistent(elm_syntax_import: elm::ElmSyntaxImport) -> ElmSyntaxImport {
    ElmSyntaxImport {
        module_name: elm_syntax_node_string_to_persistent(elm_syntax_import.module_name),
        alias: elm_syntax_import.alias.map(|alias| elm::GeneratedAsKeywordRangeName {
            as_keyword_range: text_grid_range_to_lsp_range(alias.as_keyword_range),
            name: elm_syntax_node_string_to_persistent(alias.name),
        }),
        exposing: elm_syntax_import
            .exposing_
            .map(|exposing_node| elm_syntax_node_to_persistent(exposing_node, elm_syntax_exposing_to_persistent)),
    }
}

fn elm_syntax_exposing_to_persistent(elm_syntax_exposing: elm::ElmSyntaxExposing) -> ElmSyntaxExposing {
    match elm_syntax_exposing {
        elm::ElmSyntaxExposing::ExposingAll(all_range) => ElmSyntaxExposing::All(
            text_grid_range_to_lsp_range(all_range),
        ),
        elm::ElmSyntaxExposing::ExposingExplicit(exposes) => ElmSyntaxExposing::Explicit(
            exposes
                .into_iter()
                .map(|expose_node| elm_syntax_node_to_persistent(expose_node, elm_syntax_expose_to_persistent))
                .collect::<Vec<_>>(),
        ),
    }
}

fn elm_syntax_expose_to_persistent(elm_syntax_expose: elm::ElmSyntaxExpose) -> ElmSyntaxExpose {
    match elm_syntax_expose {
        elm::ElmSyntaxExpose::ExposeChoiceTypeIncludingVariants(choice_type_expose) => ElmSyntaxExpose
        ::ChoiceTypeIncludingVariants {
            name: elm_syntax_node_string_to_persistent(choice_type_expose.name),
            open_range: text_grid_range_to_lsp_range(choice_type_expose.open_range),
        },
        elm::ElmSyntaxExpose::ExposeOperator(symbol) => ElmSyntaxExpose::Operator(symbol.to_string()),
        elm::ElmSyntaxExpose::ExposeTypeName(name) => ElmSyntaxExpose::Type(name.to_string()),
        elm::ElmSyntaxExpose::ExposeVariable(name) => ElmSyntaxExpose::Variable(name.to_string()),
    }
}

fn elm_syntax_declaration_to_persistent(elm_syntax_declaration: elm::ElmSyntaxDeclaration) -> ElmSyntaxDeclaration {
    match elm_syntax_declaration {
        elm::ElmSyntaxDeclaration::DeclarationChoiceType(choice_type_declaration) => ElmSyntaxDeclaration
        ::ChoiceType {
            name: elm_syntax_node_string_to_persistent(choice_type_declaration.name),
            parameters: choice_type_declaration
                .parameters
                .into_iter()
                .map(elm_syntax_node_string_to_persistent)
                .collect::<Vec<_>>(),
            equals_key_symbol_range: text_grid_range_to_lsp_range(choice_type_declaration.equals_key_symbol_range),
            variant0: elm::GeneratedNameValues {
                name: elm_syntax_node_string_to_persistent(choice_type_declaration.variant0.name),
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
                    or_key_symbol_range: text_grid_range_to_lsp_range(variant.or_key_symbol_range),
                    name: elm_syntax_node_string_to_persistent(variant.name),
                    values: variant.values.into_iter().map(elm_syntax_node_type_to_persistent).collect::<Vec<_>>(),
                })
                .collect::<Vec<_>>(),
        },
        elm::ElmSyntaxDeclaration::DeclarationOperator(operator_declaration) => ElmSyntaxDeclaration::Operator {
            direction: elm_syntax_node_to_persistent(operator_declaration.direction, |direction| direction),
            operator: elm_syntax_node_to_persistent(operator_declaration.operator, |operator| operator.to_string()),
            precedence: elm_syntax_node_to_persistent(operator_declaration.precedence, |precedence| precedence),
            function: elm_syntax_node_to_persistent(operator_declaration.function, |function| function.to_string()),
        },
        elm::ElmSyntaxDeclaration::DeclarationPort(port_declaration) => ElmSyntaxDeclaration::Port {
            name: elm_syntax_node_string_to_persistent(port_declaration.name),
            type_: elm_syntax_node_type_to_persistent(port_declaration.type_1),
        },
        elm::ElmSyntaxDeclaration::DeclarationTypeAlias(type_alias_declaration) => ElmSyntaxDeclaration::TypeAlias {
            alias_keyword_range: text_grid_range_to_lsp_range(type_alias_declaration.alias_keyword_range),
            name: elm_syntax_node_string_to_persistent(type_alias_declaration.name),
            parameters: type_alias_declaration
                .parameters
                .into_iter()
                .map(elm_syntax_node_string_to_persistent)
                .collect::<Vec<_>>(),
            equals_key_symbol_range: text_grid_range_to_lsp_range(type_alias_declaration.equals_key_symbol_range),
            type_: elm_syntax_node_type_to_persistent(type_alias_declaration.type_1),
        },
        elm::ElmSyntaxDeclaration::DeclarationValueOrFunction(variable_declaration) => ElmSyntaxDeclaration
        ::ValueOrFunction {
            name: variable_declaration.name.to_string(),
            signature: variable_declaration.signature.map(|signature| elm::GeneratedNameType0 {
                name: elm_syntax_node_string_to_persistent(signature.name),
                type_1: elm_syntax_node_type_to_persistent(signature.type_1),
            }),
            implementation_name_range: text_grid_range_to_lsp_range(variable_declaration.implementation_name_range),
            parameters: variable_declaration
                .parameters
                .into_iter()
                .map(elm_syntax_node_pattern_to_persistent)
                .collect::<Vec<_>>(),
            equals_key_symbol_range: text_grid_range_to_lsp_range(variable_declaration.equals_key_symbol_range),
            result: elm_syntax_node_expression_to_persistent(variable_declaration.result),
        },
    }
}

fn elm_syntax_type_to_persistent(elm_syntax_type: elm::ElmSyntaxType) -> ElmSyntaxType {
    match elm_syntax_type {
        elm::ElmSyntaxType::TypeConstruct(type_construct) => ElmSyntaxType::Construct {
            reference: elm_syntax_node_to_persistent(
                type_construct.reference,
                |reference| elm::GeneratedNameQualification {
                    qualification: reference.qualification.to_string(),
                    name: reference.name.to_string(),
                },
            ),
            arguments: type_construct
                .arguments
                .into_iter()
                .map(elm_syntax_node_type_to_persistent)
                .collect::<Vec<_>>(),
        },
        elm::ElmSyntaxType::TypeFunction(type_function) => ElmSyntaxType::Function {
            input: elm_syntax_node_type_to_persistent_box(type_function.input),
            arrow_key_symbol_range: text_grid_range_to_lsp_range(type_function.arrow_key_symbol_range),
            output: elm_syntax_node_type_to_persistent_box(type_function.output),
        },
        elm::ElmSyntaxType::TypeParenthesized(in_parens) => ElmSyntaxType::Parenthesized(
            elm_syntax_node_type_to_persistent_box(*in_parens),
        ),
        elm::ElmSyntaxType::TypeRecord(fields) => ElmSyntaxType::Record(
            fields.into_iter().map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                name: elm_syntax_node_string_to_persistent(field.name),
                colon_key_symbol_range: text_grid_range_to_lsp_range(field.colon_key_symbol_range),
                value: elm_syntax_node_type_to_persistent(field.value),
            }).collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxType::TypeRecordExtension(type_record_extension) => ElmSyntaxType::RecordExtension {
            record_variable: elm_syntax_node_string_to_persistent(type_record_extension.record_variable),
            bar_key_symbol_range: text_grid_range_to_lsp_range(type_record_extension.bar_key_symbol_range),
            field0: elm::GeneratedColonKeySymbolRangeNameValue {
                name: elm_syntax_node_string_to_persistent(type_record_extension.field0.name),
                colon_key_symbol_range: text_grid_range_to_lsp_range(
                    type_record_extension.field0.colon_key_symbol_range,
                ),
                value: elm_syntax_node_type_to_persistent_box(type_record_extension.field0.value),
            },
            field1_up: type_record_extension
                .field1_up
                .into_iter()
                .map(|field| elm::GeneratedColonKeySymbolRangeNameValue {
                    name: elm_syntax_node_string_to_persistent(field.name),
                    colon_key_symbol_range: text_grid_range_to_lsp_range(field.colon_key_symbol_range),
                    value: elm_syntax_node_type_to_persistent(field.value),
                })
                .collect::<Vec<_>>(),
        },
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
    elm_syntax_node_to_persistent(elm_syntax_node_type, |type_| Box::new(elm_syntax_type_to_persistent(type_)))
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
            elements.into_iter().map(elm_syntax_node_pattern_to_persistent).collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxPattern::PatternParenthesized(in_parens) => ElmSyntaxPattern::Parenthesized(
            elm_syntax_node_pattern_to_persistent_box(*in_parens),
        ),
        elm::ElmSyntaxPattern::PatternRecord(fields) => ElmSyntaxPattern::Record(
            fields.into_iter().map(elm_syntax_node_string_to_persistent).collect::<Vec<_>>(),
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
        elm::ElmSyntaxPattern::PatternVariable(name) => ElmSyntaxPattern::Variable(name.to_string()),
        elm::ElmSyntaxPattern::PatternVariant(variant_pattern) => ElmSyntaxPattern::Variant {
            reference: elm_syntax_node_to_persistent(
                variant_pattern.reference,
                |reference| elm::GeneratedNameQualification {
                    qualification: reference.qualification.to_string(),
                    name: reference.name.to_string(),
                },
            ),
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
    elm_syntax_node_to_persistent(
        elm_syntax_node_pattern,
        |pattern| Box::new(elm_syntax_pattern_to_persistent(pattern)),
    )
}

fn elm_syntax_expression_to_persistent(elm_syntax_expression: elm::ElmSyntaxExpression) -> ElmSyntaxExpression {
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
                arrow_key_symbol_range: text_grid_range_to_lsp_range(case_of.case0.arrow_key_symbol_range),
                result: elm_syntax_node_expression_to_persistent_box(case_of.case0.result),
            },
            case1_up: case_of.case1_up.into_iter().map(|case| elm::GeneratedArrowKeySymbolRangePatternResult {
                pattern: elm_syntax_node_pattern_to_persistent(case.pattern),
                arrow_key_symbol_range: text_grid_range_to_lsp_range(case.arrow_key_symbol_range),
                result: elm_syntax_node_expression_to_persistent(case.result),
            }).collect::<Vec<_>>(),
        },
        elm::ElmSyntaxExpression::ExpressionChar(char) => ElmSyntaxExpression::Char(char),
        elm::ElmSyntaxExpression::ExpressionFloat(float) => ElmSyntaxExpression::Float(float),
        elm::ElmSyntaxExpression::ExpressionIfThenElse(if_then_else) => ElmSyntaxExpression::IfThenElse {
            condition: elm_syntax_node_expression_to_persistent_box(if_then_else.condition),
            then_keyword_range: text_grid_range_to_lsp_range(if_then_else.then_keyword_range),
            on_true: elm_syntax_node_expression_to_persistent_box(if_then_else.on_true),
            else_keyword_range: text_grid_range_to_lsp_range(if_then_else.else_keyword_range),
            on_false: elm_syntax_node_expression_to_persistent_box(if_then_else.on_false),
        },
        elm::ElmSyntaxExpression::ExpressionInfixOperation(infix_operation) => ElmSyntaxExpression::InfixOperation {
            left: elm_syntax_node_expression_to_persistent_box(infix_operation.left),
            operator: elm_syntax_node_string_to_persistent(infix_operation.operator),
            right: elm_syntax_node_expression_to_persistent_box(infix_operation.right),
        },
        elm::ElmSyntaxExpression::ExpressionInteger(integer_expression) => ElmSyntaxExpression::Integer {
            base: integer_expression.base,
            value: integer_expression.value,
        },
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
            declaration0: elm_syntax_node_to_persistent(
                let_in.declaration0,
                |declaration| Box::new(elm_syntax_let_declaration_to_persistent(declaration)),
            ),
            declaration1_up: let_in
                .declaration1_up
                .into_iter()
                .map(
                    |declaration_node| elm_syntax_node_to_persistent(
                        declaration_node,
                        elm_syntax_let_declaration_to_persistent,
                    ),
                )
                .collect::<Vec<_>>(),
            in_keyword_range: text_grid_range_to_lsp_range(let_in.in_keyword_range),
            result: elm_syntax_node_expression_to_persistent_box(let_in.result),
        },
        elm::ElmSyntaxExpression::ExpressionList(elements) => ElmSyntaxExpression::List(
            elements.into_iter().map(elm_syntax_node_expression_to_persistent).collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxExpression::ExpressionNegation(in_negation) => ElmSyntaxExpression::Negation(
            elm_syntax_node_expression_to_persistent_box(*in_negation),
        ),
        elm::ElmSyntaxExpression::ExpressionOperatorFunction(symbol) => ElmSyntaxExpression::OperatorFunction(
            symbol.to_string(),
        ),
        elm::ElmSyntaxExpression::ExpressionParenthesized(in_parens) => ElmSyntaxExpression::Parenthesized(
            elm_syntax_node_expression_to_persistent_box(*in_parens),
        ),
        elm::ElmSyntaxExpression::ExpressionRecord(fields) => ElmSyntaxExpression::Record(
            fields.into_iter().map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                name: elm_syntax_node_string_to_persistent(field.name),
                equals_key_symbol_range: text_grid_range_to_lsp_range(field.equals_key_symbol_range),
                value: elm_syntax_node_expression_to_persistent(field.value),
            }).collect::<Vec<_>>(),
        ),
        elm::ElmSyntaxExpression::ExpressionRecordAccess(record_access) => ElmSyntaxExpression::RecordAccess {
            record: elm_syntax_node_expression_to_persistent_box(record_access.record),
            field: elm_syntax_node_string_to_persistent(record_access.field),
        },
        elm::ElmSyntaxExpression::ExpressionRecordAccessFunction(field) => ElmSyntaxExpression::RecordAccessFunction(
            field.to_string(),
        ),
        elm::ElmSyntaxExpression::ExpressionRecordUpdate(record_update) => ElmSyntaxExpression::RecordUpdate {
            record_variable: elm_syntax_node_string_to_persistent(record_update.record_variable),
            bar_key_symbol_range: text_grid_range_to_lsp_range(record_update.bar_key_symbol_range),
            field0: elm::GeneratedEqualsKeySymbolRangeNameValue {
                name: elm_syntax_node_string_to_persistent(record_update.field0.name),
                equals_key_symbol_range: text_grid_range_to_lsp_range(record_update.field0.equals_key_symbol_range),
                value: elm_syntax_node_expression_to_persistent_box(record_update.field0.value),
            },
            field1_up: record_update.field1_up.into_iter().map(|field| elm::GeneratedEqualsKeySymbolRangeNameValue {
                name: elm_syntax_node_string_to_persistent(field.name),
                equals_key_symbol_range: text_grid_range_to_lsp_range(field.equals_key_symbol_range),
                value: elm_syntax_node_expression_to_persistent(field.value),
            }).collect::<Vec<_>>(),
        },
        elm::ElmSyntaxExpression::ExpressionReference(reference) => ElmSyntaxExpression::Reference(
            elm::GeneratedNameQualification {
                qualification: reference.qualification.to_string(),
                name: reference.name.to_string(),
            },
        ),
        elm::ElmSyntaxExpression::ExpressionString(string_expression) => ElmSyntaxExpression::String {
            quoting_style: string_expression.quoting_style,
            content: string_expression.content.to_string(),
        },
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
    elm_syntax_node_to_persistent(elm_syntax_node_expression, elm_syntax_expression_to_persistent)
}

fn elm_syntax_node_expression_to_persistent_box(
    elm_syntax_node_expression: elm::ElmSyntaxNode<elm::ElmSyntaxExpression>,
) -> ElmSyntaxNode<Box<ElmSyntaxExpression>> {
    elm_syntax_node_to_persistent(
        elm_syntax_node_expression,
        |expression| Box::new(elm_syntax_expression_to_persistent(expression)),
    )
}

fn elm_syntax_let_declaration_to_persistent(
    elm_syntax_let_declaration: elm::ElmSyntaxLetDeclaration,
) -> ElmSyntaxLetDeclaration {
    match elm_syntax_let_declaration {
        elm::ElmSyntaxLetDeclaration::LetDestructuring(destructuring) => ElmSyntaxLetDeclaration::Destructuring {
            pattern: elm_syntax_node_pattern_to_persistent(destructuring.pattern),
            equals_key_symbol_range: text_grid_range_to_lsp_range(destructuring.equals_key_symbol_range),
            expression: elm_syntax_node_expression_to_persistent(destructuring.expression),
        },
        elm::ElmSyntaxLetDeclaration::LetValueOrFunctionDeclaration(variable_declaration) => ElmSyntaxLetDeclaration
        ::ValueOrFunctionDeclaration {
            name: variable_declaration.name.to_string(),
            signature: variable_declaration.signature.map(|signature| elm::GeneratedNameType0 {
                name: elm_syntax_node_string_to_persistent(signature.name),
                type_1: elm_syntax_node_type_to_persistent(signature.type_1),
            }),
            implementation_name_range: text_grid_range_to_lsp_range(variable_declaration.implementation_name_range),
            parameters: variable_declaration
                .parameters
                .into_iter()
                .map(elm_syntax_node_pattern_to_persistent)
                .collect::<Vec<_>>(),
            equals_key_symbol_range: text_grid_range_to_lsp_range(variable_declaration.equals_key_symbol_range),
            result: elm_syntax_node_expression_to_persistent(variable_declaration.result),
        },
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

fn elm_syntax_node_to_persistent<
    Value,
    ValuePersistent,
>(
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
struct ModuleOriginLookup {
    unqualified: std::collections::HashMap<String, String>,
    qualified: std::collections::HashMap<String, String>,
}

fn module_origin_lookup_for_implicit_imports() -> ModuleOriginLookup {
    // https://github.com/elm/core?tab=readme-ov-file#default-imports
    ModuleOriginLookup {
        unqualified: std::collections::HashMap::from(
            [
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
            ].map(|(from, to)| (from.to_string(), to.to_string())),
        ),
        qualified: std::collections::HashMap::from(
            [
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
            ].map(|(from, to)| (from.to_string(), to.to_string())),
        ),
    }
}

fn look_up_origin_module<
    'a,
>(module_origin_lookup: &'a ModuleOriginLookup, qualification: &'a str, name: &'a str) -> &'a str {
    match match qualification {
        "" => module_origin_lookup.unqualified.get(name),
        qualification_module_or_alias => module_origin_lookup.qualified.get(qualification_module_or_alias),
    } {
        Some(s) => s.as_str(),
        None => qualification,
    }
}

fn elm_syntax_module_create_origin_lookup(state: &State, elm_syntax_module: &ElmSyntaxModule) -> ModuleOriginLookup {
    let mut module_origin_lookup: ModuleOriginLookup = module_origin_lookup_for_implicit_imports();
    for documented_declaration in elm_syntax_module.declarations.iter() {
        match &documented_declaration.declaration.value {
            ElmSyntaxDeclaration
            ::ChoiceType {
                name,
                parameters: _,
                equals_key_symbol_range: _,
                variant0: variant0,
                variant1_up: variant1_up,
            } => {
                module_origin_lookup
                    .unqualified
                    .insert(name.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
                module_origin_lookup
                    .unqualified
                    .insert(variant0.name.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
                for variant in variant1_up.iter() {
                    module_origin_lookup
                        .unqualified
                        .insert(variant.name.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
                }
            },
            ElmSyntaxDeclaration::Operator { direction: _, operator, function: _, precedence: _ } => {
                module_origin_lookup
                    .unqualified
                    .insert(operator.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
            },
            ElmSyntaxDeclaration::Port { name, type_: _ } => {
                module_origin_lookup
                    .unqualified
                    .insert(name.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
            },
            ElmSyntaxDeclaration
            ::TypeAlias {
                alias_keyword_range: _,
                equals_key_symbol_range: _,
                name,
                parameters: _,
                type_: _,
            } => {
                module_origin_lookup
                    .unqualified
                    .insert(name.value.clone(), elm_syntax_module.header.value.module_name.value.clone());
            },
            ElmSyntaxDeclaration
            ::ValueOrFunction {
                name,
                signature: _,
                implementation_name_range: _,
                parameters: _,
                equals_key_symbol_range: _,
                result: _,
            } => {
                module_origin_lookup
                    .unqualified
                    .insert(name.clone(), elm_syntax_module.header.value.module_name.value.clone());
            },
        }
    }
    for import_node in &elm_syntax_module.imports {
        let allowed_qualification: &String = match import_node.value.alias {
            None => &import_node.value.module_name.value,
            Some(ref import_alias) => &import_alias.name.value,
        };
        module_origin_lookup
            .qualified
            .insert(allowed_qualification.clone(), import_node.value.module_name.value.clone());
        match &import_node.value.exposing {
            None => { },
            Some(import_exposing) => match import_exposing.value {
                ElmSyntaxExposing::Explicit(ref exposes) => {
                    for expose_node in exposes {
                        match &expose_node.value {
                            ElmSyntaxExpose
                            ::ChoiceTypeIncludingVariants {
                                name: choice_type_expose_name,
                                open_range: _,
                            } => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(
                                        choice_type_expose_name.value.clone(),
                                        import_node.value.module_name.value.clone(),
                                    );
                                let imported_module_maybe_syntax: Option<&ElmSyntaxModule> =
                                    match state_file_path_for_module_name(
                                        state,
                                        &import_node.value.module_name.value,
                                    ) {
                                        None => None,
                                        Some(imported_module_file_path) => state
                                            .parsed_modules
                                            .get(&imported_module_file_path)
                                            .and_then(|m| m.syntax.as_ref()),
                                    };
                                match imported_module_maybe_syntax {
                                    None => { },
                                    Some(imported_module_syntax) => {
                                        'until_origin_choice_type_declaration_found: for declaration in imported_module_syntax
                                            .declarations
                                            .iter() {
                                            match &declaration.declaration.value {
                                                ElmSyntaxDeclaration
                                                ::ChoiceType {
                                                    name: imported_module_choice_type_name,
                                                    parameters: _,
                                                    equals_key_symbol_range: _,
                                                    variant0: imported_module_choice_type_variant0,
                                                    variant1_up: imported_module_choice_type_variant1_up,
                                                } => {
                                                    if choice_type_expose_name == imported_module_choice_type_name {
                                                        module_origin_lookup
                                                            .unqualified
                                                            .insert(
                                                                imported_module_choice_type_variant0.name.value.clone(),
                                                                import_node.value.module_name.value.clone(),
                                                            );
                                                        for imported_module_choice_type_variant in imported_module_choice_type_variant1_up {
                                                            module_origin_lookup
                                                                .unqualified
                                                                .insert(
                                                                    imported_module_choice_type_variant
                                                                        .name
                                                                        .value
                                                                        .clone(),
                                                                    import_node.value.module_name.value.clone(),
                                                                );
                                                        }
                                                        break 'until_origin_choice_type_declaration_found
                                                    }
                                                },
                                                _ => { },
                                            }
                                        }
                                    },
                                }
                            },
                            ElmSyntaxExpose::Operator(symbol) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(symbol.clone(), import_node.value.module_name.value.clone());
                            },
                            ElmSyntaxExpose::Type(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name.clone(), import_node.value.module_name.value.clone());
                            },
                            ElmSyntaxExpose::Variable(name) => {
                                module_origin_lookup
                                    .unqualified
                                    .insert(name.clone(), import_node.value.module_name.value.clone());
                            },
                        }
                    }
                },
                ElmSyntaxExposing::All(_) => {
                    match elm_syntax_module.header.value.exposing.value {
                        ElmSyntaxExposing::All(_) => {
                            let imported_module_maybe_syntax: Option<&ElmSyntaxModule> =
                                match state_file_path_for_module_name(state, &import_node.value.module_name.value) {
                                    None => None,
                                    Some(imported_module_file_path) => state
                                        .parsed_modules
                                        .get(&imported_module_file_path)
                                        .and_then(|m| m.syntax.as_ref()),
                                };
                            match imported_module_maybe_syntax {
                                None => { },
                                Some(imported_module_syntax) => {
                                    for declaration in imported_module_syntax.declarations.iter() {
                                        match &declaration.declaration.value {
                                            ElmSyntaxDeclaration
                                            ::ChoiceType {
                                                name: imported_module_choice_type_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                variant0: imported_module_choice_type_variant0,
                                                variant1_up: imported_module_choice_type_variant1_up,
                                            } => {
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_choice_type_name.value.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_choice_type_variant0.name.value.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                                for imported_module_choice_type_variant in imported_module_choice_type_variant1_up {
                                                    module_origin_lookup
                                                        .unqualified
                                                        .insert(
                                                            imported_module_choice_type_variant.name.value.clone(),
                                                            import_node.value.module_name.value.clone(),
                                                        );
                                                }
                                            },
                                            ElmSyntaxDeclaration
                                            ::Port {
                                                name: imported_module_port_name,
                                                type_: _,
                                            } => {
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_port_name.value.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                            },
                                            ElmSyntaxDeclaration
                                            ::TypeAlias {
                                                alias_keyword_range: _,
                                                name: imported_module_type_alias_name,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                type_: _,
                                            } => {
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_type_alias_name.value.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                            },
                                            ElmSyntaxDeclaration
                                            ::Operator {
                                                direction: _,
                                                operator: imported_module_operator,
                                                function: _,
                                                precedence: _,
                                            } => {
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_operator.value.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                            },
                                            ElmSyntaxDeclaration
                                            ::ValueOrFunction {
                                                name: imported_module_variable_name,
                                                signature: _,
                                                implementation_name_range: _,
                                                parameters: _,
                                                equals_key_symbol_range: _,
                                                result: _,
                                            } => {
                                                module_origin_lookup
                                                    .unqualified
                                                    .insert(
                                                        imported_module_variable_name.clone(),
                                                        import_node.value.module_name.value.clone(),
                                                    );
                                            },
                                        }
                                    }
                                },
                            }
                        },
                        ElmSyntaxExposing::Explicit(ref exposes) => {
                            for expose in exposes {
                                match &expose.value {
                                    ElmSyntaxExpose
                                    ::ChoiceTypeIncludingVariants {
                                        name: choice_type_expose_name,
                                        open_range: _,
                                    } => {
                                        module_origin_lookup
                                            .unqualified
                                            .insert(
                                                choice_type_expose_name.value.clone(),
                                                import_node.value.module_name.value.clone(),
                                            );
                                        let imported_module_maybe_syntax: Option<&ElmSyntaxModule> =
                                            match state_file_path_for_module_name(
                                                state,
                                                &import_node.value.module_name.value,
                                            ) {
                                                None => None,
                                                Some(imported_module_file_path) => state
                                                    .parsed_modules
                                                    .get(&imported_module_file_path)
                                                    .and_then(|m| m.syntax.as_ref()),
                                            };
                                        match imported_module_maybe_syntax {
                                            None => { },
                                            Some(imported_module_syntax) => {
                                                'until_origin_choice_type_declaration_found: for declaration in imported_module_syntax
                                                    .declarations
                                                    .iter() {
                                                    match &declaration.declaration.value {
                                                        ElmSyntaxDeclaration
                                                        ::ChoiceType {
                                                            name: imported_module_choice_type_name,
                                                            parameters: _,
                                                            equals_key_symbol_range: _,
                                                            variant0: imported_module_choice_type_variant0,
                                                            variant1_up: imported_module_choice_type_variant1_up,
                                                        } => {
                                                            if choice_type_expose_name ==
                                                                imported_module_choice_type_name {
                                                                module_origin_lookup
                                                                    .unqualified
                                                                    .insert(
                                                                        imported_module_choice_type_variant0
                                                                            .name
                                                                            .value
                                                                            .clone(),
                                                                        import_node.value.module_name.value.clone(),
                                                                    );
                                                                for imported_module_choice_type_variant in imported_module_choice_type_variant1_up {
                                                                    module_origin_lookup
                                                                        .unqualified
                                                                        .insert(
                                                                            imported_module_choice_type_variant
                                                                                .name
                                                                                .value
                                                                                .clone(),
                                                                            import_node
                                                                                .value
                                                                                .module_name
                                                                                .value
                                                                                .clone(),
                                                                        );
                                                                }
                                                                break 'until_origin_choice_type_declaration_found
                                                            }
                                                        },
                                                        _ => { },
                                                    }
                                                }
                                            },
                                        }
                                    },
                                    ElmSyntaxExpose::Operator(symbol) => {
                                        module_origin_lookup
                                            .unqualified
                                            .insert(symbol.clone(), import_node.value.module_name.value.clone());
                                    },
                                    ElmSyntaxExpose::Type(name) => {
                                        module_origin_lookup
                                            .unqualified
                                            .insert(name.clone(), import_node.value.module_name.value.clone());
                                    },
                                    ElmSyntaxExpose::Variable(name) => {
                                        module_origin_lookup
                                            .unqualified
                                            .insert(name.clone(), import_node.value.module_name.value.clone());
                                    },
                                }
                            }
                        },
                    }
                },
            },
        }
    }
    module_origin_lookup
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
        ElmSyntaxType::Construct { reference, arguments } => {
            so_far.push_str(
                look_up_origin_module(module_origin_lookup, &reference.value.qualification, &reference.value.name),
            );
            so_far.push('.');
            so_far.push_str(&reference.value.name);
            for argument in arguments {
                so_far.push(' ');
                elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &argument.value);
            }
        },
        ElmSyntaxType::Function { input, arrow_key_symbol_range: _, output } => {
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &input.value);
            so_far.push_str(" -> ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &output.value)
        },
        ElmSyntaxType::Parenthesized(in_parens) => {
            so_far.push('(');
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &in_parens.value);
            so_far.push(')');
        },
        ElmSyntaxType::Record(fields) => {
            let mut fields_iterator = fields.iter();
            match fields_iterator.next() {
                None => {
                    so_far.push_str("{}")
                },
                Some(field0) => {
                    so_far.push_str("{ ");
                    so_far.push_str(&field0.name.value);
                    so_far.push_str(" : ");
                    elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &field0.value.value);
                    for field in fields_iterator {
                        so_far.push_str(", ");
                        so_far.push_str(&field.name.value);
                        so_far.push_str(" : ");
                        elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &field.value.value);
                    }
                    so_far.push_str(" }")
                },
            }
        },
        ElmSyntaxType::RecordExtension { record_variable, bar_key_symbol_range: _, field0, field1_up } => {
            so_far.push_str("{ ");
            so_far.push_str(&record_variable.value);
            so_far.push_str(" | ");
            so_far.push_str(&field0.name.value);
            so_far.push_str(" : ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &field0.value.value);
            for field in field1_up {
                so_far.push_str(", ");
                so_far.push_str(&field.name.value);
                so_far.push_str(" : ");
                elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &field.value.value);
            }
            so_far.push_str(" }")
        },
        ElmSyntaxType::Triple { part0, part1, part2 } => {
            so_far.push_str("( ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part0.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part1.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part2.value);
            so_far.push_str(" )");
        },
        ElmSyntaxType::Tuple { part0, part1 } => {
            so_far.push_str("( ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part0.value);
            so_far.push_str(", ");
            elm_syntax_type_to_single_line_string_into(so_far, module_origin_lookup, &part1.value);
            so_far.push_str(" )");
        },
        ElmSyntaxType::Unit => {
            so_far.push_str("()")
        },
        ElmSyntaxType::Variable(name) => {
            so_far.push_str(name)
        },
    }
}

// //
#[derive(Clone, Debug)]
enum ElmSymbolOwned {
    ModuleName(String),
    ImportAlias {
        module_origin: String,
        alias_name: String,
    },
    VariableOrVariantOrOperator {
        module_origin: String,
        name: String,
    },
    Type {
        module_origin: String,
        name: String,
    },
    // TODO add TypeVariable { scope_declaration : &ElmSyntaxDeclaration, name :
    // String }, TODO add , TODO add local references,
}

fn elm_syntax_module_find_reference_at_position(
    state: &State,
    elm_syntax_module: &ElmSyntaxModule,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if lsp_range_includes_position(elm_syntax_module.header.value.module_name.range, position) {
        Some(ElmSyntaxNode {
            value: ElmSymbolOwned::ModuleName(elm_syntax_module.header.value.module_name.value.clone()),
            range: elm_syntax_module.header.value.module_name.range,
        })
    } else {
        elm_syntax_exposing_from_module_find_reference_at_position(
            elm_syntax_node_as_ref(&elm_syntax_module.header.value.exposing),
            &elm_syntax_module.header.value.module_name.value,
            position,
        )
            .or_else(
                || elm_syntax_module
                    .imports
                    .iter()
                    .find_map(
                        |import_node| elm_syntax_import_find_reference_at_position(
                            elm_syntax_node_as_ref(import_node),
                            position,
                        ),
                    ),
            )
            .or_else(|| {
                let module_origin_lookup: ModuleOriginLookup =
                    elm_syntax_module_create_origin_lookup(state, elm_syntax_module);
                elm_syntax_module
                    .declarations
                    .iter()
                    .find_map(
                        |documented_declaration| elm_syntax_declaration_find_reference_at_position(
                            &elm_syntax_module.header.value.module_name.value,
                            &module_origin_lookup,
                            elm_syntax_node_as_ref(&documented_declaration.declaration),
                            position,
                        ),
                    )
            })
    }
}

fn elm_syntax_import_find_reference_at_position(
    elm_syntax_import_node: ElmSyntaxNode<&ElmSyntaxImport>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_import_node.range, position) {
        None
    } else if lsp_range_includes_position(elm_syntax_import_node.value.module_name.range, position) {
        Some(ElmSyntaxNode {
            value: ElmSymbolOwned::ModuleName(elm_syntax_import_node.value.module_name.value.clone()),
            range: elm_syntax_import_node.value.module_name.range,
        })
    } else if let Some(ref import_alias) = elm_syntax_import_node.value.alias &&
        lsp_range_includes_position(import_alias.name.range, position) {
        Some(ElmSyntaxNode {
            value: ElmSymbolOwned::ImportAlias {
                module_origin: elm_syntax_import_node.value.module_name.value.clone(),
                alias_name: import_alias.name.value.clone(),
            },
            range: elm_syntax_import_node.value.module_name.range,
        })
    } else {
        elm_syntax_import_node
            .value
            .exposing
            .as_ref()
            .and_then(
                |exposing_node| elm_syntax_exposing_from_module_find_reference_at_position(
                    elm_syntax_node_as_ref(exposing_node),
                    &elm_syntax_import_node.value.module_name.value,
                    position,
                ),
            )
    }
}

fn elm_syntax_exposing_from_module_find_reference_at_position(
    elm_syntax_exposing_node: ElmSyntaxNode<&ElmSyntaxExposing>,
    module_name: &str,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_exposing_node.range, position) {
        None
    } else {
        match &elm_syntax_exposing_node.value {
            ElmSyntaxExposing::All(_) => None,
            ElmSyntaxExposing::Explicit(exposes) => exposes
                .iter()
                .find_map(|expose_node| if lsp_range_includes_position(expose_node.range, position) {
                    Some(match &expose_node.value {
                        ElmSyntaxExpose::ChoiceTypeIncludingVariants { name, open_range: _ } => ElmSyntaxNode {
                            value: ElmSymbolOwned::Type {
                                module_origin: module_name.to_string(),
                                name: name.value.clone(),
                            },
                            range: expose_node.range,
                        },
                        ElmSyntaxExpose::Operator(symbol) => ElmSyntaxNode {
                            value: ElmSymbolOwned::VariableOrVariantOrOperator {
                                module_origin: module_name.to_string(),
                                name: symbol.clone(),
                            },
                            range: lsp_types::Range {
                                start: lsp_position_add_characters(expose_node.range.start, 1),
                                end: lsp_position_add_characters(expose_node.range.end, -1),
                            },
                        },
                        ElmSyntaxExpose::Type(name) => ElmSyntaxNode {
                            value: ElmSymbolOwned::Type {
                                module_origin: module_name.to_string(),
                                name: name.clone(),
                            },
                            range: expose_node.range,
                        },
                        ElmSyntaxExpose::Variable(name) => ElmSyntaxNode {
                            value: ElmSymbolOwned::VariableOrVariantOrOperator {
                                module_origin: module_name.to_string(),
                                name: name.clone(),
                            },
                            range: expose_node.range,
                        },
                    })
                } else {
                    None
                }),
        }
    }
}

fn elm_syntax_declaration_find_reference_at_position(
    origin_module: &str,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_declaration_node: ElmSyntaxNode<&ElmSyntaxDeclaration>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_declaration_node.range, position) {
        None
    } else {
        match elm_syntax_declaration_node.value {
            ElmSyntaxDeclaration
            ::ChoiceType {
                name: name,
                parameters: _,
                equals_key_symbol_range: _,
                variant0,
                variant1_up,
            } => if lsp_range_includes_position(
                name.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::Type {
                        module_origin: origin_module.to_string(),
                        name: name.value.clone(),
                    },
                    range: name.range,
                })
            } else if lsp_range_includes_position(variant0.name.range, position) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: origin_module.to_string(),
                        name: variant0.name.value.clone(),
                    },
                    range: variant0.name.range,
                })
            } else {
                variant0
                    .values
                    .iter()
                    .find_map(
                        |variant_value| elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(variant_value),
                            position,
                        ),
                    )
                    .or_else(
                        || variant1_up
                            .iter()
                            .find_map(|variant| if lsp_range_includes_position(variant.name.range, position) {
                                Some(ElmSyntaxNode {
                                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                                        module_origin: origin_module.to_string(),
                                        name: variant.name.value.clone(),
                                    },
                                    range: variant.name.range,
                                })
                            } else {
                                variant
                                    .values
                                    .iter()
                                    .find_map(
                                        |variant_value| elm_syntax_type_find_reference_at_position(
                                            module_origin_lookup,
                                            elm_syntax_node_as_ref(variant_value),
                                            position,
                                        ),
                                    )
                            }),
                    )
            },
            ElmSyntaxDeclaration
            ::Operator {
                direction: _,
                operator: operator,
                function,
                precedence: _,
            } => if lsp_range_includes_position(
                operator.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: origin_module.to_string(),
                        name: operator.value.clone(),
                    },
                    range: operator.range,
                })
            } else if lsp_range_includes_position(function.range, position) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: origin_module.to_string(),
                        name: function.value.clone(),
                    },
                    range: function.range,
                })
            } else {
                None
            },
            ElmSyntaxDeclaration::Port { name: name, type_ } => if lsp_range_includes_position(name.range, position) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: origin_module.to_string(),
                        name: name.value.clone(),
                    },
                    range: name.range,
                })
            } else {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_as_ref(type_),
                    position,
                )
            },
            ElmSyntaxDeclaration
            ::TypeAlias {
                alias_keyword_range: _,
                name: name,
                parameters: _,
                equals_key_symbol_range: _,
                type_,
            } => if lsp_range_includes_position(
                name.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::Type {
                        module_origin: origin_module.to_string(),
                        name: name.value.clone(),
                    },
                    range: name.range,
                })
            } else {
                elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_as_ref(type_),
                    position,
                )
            },
            ElmSyntaxDeclaration
            ::ValueOrFunction {
                name,
                signature: maybe_signature,
                implementation_name_range,
                parameters,
                equals_key_symbol_range: _,
                result,
            } => if lsp_range_includes_position(
                *implementation_name_range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: origin_module.to_string(),
                        name: name.clone(),
                    },
                    range: *implementation_name_range,
                })
            } else {
                maybe_signature
                    .as_ref()
                    .and_then(|signature| if lsp_range_includes_position(signature.name.range, position) {
                        Some(ElmSyntaxNode {
                            value: ElmSymbolOwned::VariableOrVariantOrOperator {
                                module_origin: origin_module.to_string(),
                                name: name.clone(),
                            },
                            range: signature.name.range,
                        })
                    } else {
                        elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(&signature.type_1),
                            position,
                        )
                    })
                    .or_else(
                        || elm_syntax_expression_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(result),
                            position,
                        ),
                    )
                    .or_else(
                        || parameters
                            .iter()
                            .find_map(
                                |parameter| elm_syntax_pattern_find_reference_at_position(
                                    module_origin_lookup,
                                    elm_syntax_node_as_ref(parameter),
                                    position,
                                ),
                            ),
                    )
            },
        }
    }
}

fn elm_syntax_pattern_find_reference_at_position(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    match elm_syntax_pattern_node.value {
        ElmSyntaxPattern
        ::As {
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
        ElmSyntaxPattern::ListCons { head, cons_key_symbol: _, tail } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(head),
            position,
        ).or_else(
            || elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(tail),
                position,
            ),
        ),
        ElmSyntaxPattern::ListExact(elements) => elements
            .iter()
            .find_map(
                |element| elm_syntax_pattern_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_as_ref(element),
                    position,
                ),
            ),
        ElmSyntaxPattern::Parenthesized(in_parens) => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(in_parens),
            position,
        ),
        ElmSyntaxPattern::Record(_) => None,
        ElmSyntaxPattern::String { .. } => None,
        ElmSyntaxPattern::Triple { part0, part1, part2 } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(part0),
            position,
        )
            .or_else(
                || elm_syntax_pattern_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1),
                    position,
                ),
            )
            .or_else(
                || elm_syntax_pattern_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(part2),
                    position,
                ),
            ),
        ElmSyntaxPattern::Tuple { part0, part1 } => elm_syntax_pattern_find_reference_at_position(
            module_origin_lookup,
            elm_syntax_node_unbox(part0),
            position,
        ).or_else(
            || elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                position,
            ),
        ),
        ElmSyntaxPattern::Unit => None,
        ElmSyntaxPattern::Variable(_) => None,
        ElmSyntaxPattern::Variant { reference, values } => if lsp_range_includes_position(reference.range, position) {
            Some(ElmSyntaxNode {
                value: ElmSymbolOwned::VariableOrVariantOrOperator {
                    module_origin: look_up_origin_module(
                        module_origin_lookup,
                        &reference.value.qualification,
                        &reference.value.name,
                    ).to_string(),
                    name: reference.value.name.clone(),
                },
                range: reference.range,
            })
        } else {
            values
                .iter()
                .find_map(
                    |value| elm_syntax_pattern_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(value),
                        position,
                    ),
                )
        },
    }
}

fn elm_syntax_type_find_reference_at_position(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type_node: ElmSyntaxNode<&ElmSyntaxType>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_type_node.range, position) {
        None
    } else {
        match &elm_syntax_type_node.value {
            ElmSyntaxType::Construct { reference, arguments } => if lsp_range_includes_position(
                reference.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::Type {
                        module_origin: look_up_origin_module(
                            module_origin_lookup,
                            &reference.value.qualification,
                            &reference.value.name,
                        ).to_string(),
                        name: reference.value.name.clone(),
                    },
                    range: reference.range,
                })
            } else {
                arguments
                    .iter()
                    .find_map(
                        |argument| elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(argument),
                            position,
                        ),
                    )
            },
            ElmSyntaxType
            ::Function {
                input,
                arrow_key_symbol_range: _,
                output,
            } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(input),
                position,
            ).or_else(
                || elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(output),
                    position,
                ),
            ),
            ElmSyntaxType::Parenthesized(in_parens) => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                position,
            ),
            ElmSyntaxType::Record(fields) => fields
                .iter()
                .find_map(
                    |field| elm_syntax_type_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&field.value),
                        position,
                    ),
                ),
            ElmSyntaxType
            ::RecordExtension {
                record_variable: _,
                bar_key_symbol_range: _,
                field0,
                field1_up,
            } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(&field0.value),
                position,
            ).or_else(
                || field1_up
                    .iter()
                    .find_map(
                        |field| elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(&field.value),
                            position,
                        ),
                    ),
            ),
            ElmSyntaxType::Triple { part0, part1, part2 } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                position,
            )
                .or_else(
                    || elm_syntax_type_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(part1),
                        position,
                    ),
                )
                .or_else(
                    || elm_syntax_type_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(part2),
                        position,
                    ),
                ),
            ElmSyntaxType::Tuple { part0, part1 } => elm_syntax_type_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                position,
            ).or_else(
                || elm_syntax_type_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1),
                    position,
                ),
            ),
            ElmSyntaxType::Unit => None,
            ElmSyntaxType::Variable(_) => None,
        }
    }
}

fn elm_syntax_expression_find_reference_at_position(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_expression_node.range, position) {
        None
    } else {
        match elm_syntax_expression_node.value {
            ElmSyntaxExpression
            ::Call {
                called,
                argument0,
                argument1_up,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(called),
                position,
            )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(argument0),
                        position,
                    ),
                )
                .or_else(
                    || argument1_up
                        .iter()
                        .find_map(
                            |argument| elm_syntax_expression_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_node_as_ref(argument),
                                position,
                            ),
                        ),
                ),
            ElmSyntaxExpression
            ::CaseOf {
                matched,
                of_keyword_range: _,
                case0,
                case1_up,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(matched),
                position,
            )
                .or_else(
                    || elm_syntax_pattern_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&case0.pattern),
                        position,
                    ),
                )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(&case0.result),
                        position,
                    ),
                )
                .or_else(
                    || case1_up
                        .iter()
                        .find_map(
                            |case| elm_syntax_pattern_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_node_as_ref(&case.pattern),
                                position,
                            ).or_else(
                                || elm_syntax_expression_find_reference_at_position(
                                    module_origin_lookup,
                                    elm_syntax_node_as_ref(&case.result),
                                    position,
                                ),
                            ),
                        ),
                ),
            ElmSyntaxExpression::Char(_) => None,
            ElmSyntaxExpression::Float(_) => None,
            ElmSyntaxExpression
            ::IfThenElse {
                condition,
                then_keyword_range: _,
                on_true,
                else_keyword_range: _,
                on_false,
            } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(condition),
                position,
            )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(on_true),
                        position,
                    ),
                )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(on_false),
                        position,
                    ),
                ),
            ElmSyntaxExpression::InfixOperation { left, operator, right } => if lsp_range_includes_position(
                operator.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: look_up_origin_module(module_origin_lookup, "", &operator.value).to_string(),
                        name: operator.value.clone(),
                    },
                    range: operator.range,
                })
            } else {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(left),
                    position,
                ).or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(right),
                        position,
                    ),
                )
            },
            ElmSyntaxExpression::Integer { .. } => None,
            ElmSyntaxExpression
            ::Lambda {
                arrow_key_symbol_range: _,
                parameter0,
                parameter1_up,
                result,
            } => elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_as_ref(parameter0),
                position,
            )
                .or_else(
                    || parameter1_up
                        .iter()
                        .find_map(
                            |parameter| elm_syntax_pattern_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_node_as_ref(parameter),
                                position,
                            ),
                        ),
                )
                .or_else(
                    // TODO add let-declaration-introduced bindings
                    || elm_syntax_expression_find_reference_at_position(module_origin_lookup, elm_syntax_node_unbox(result), position),
                ),
            ElmSyntaxExpression
            ::LetIn {
                declaration0,
                declaration1_up,
                in_keyword_range: _,
                result,
            } => elm_syntax_let_declaration_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(declaration0),
                position,
            )
                .or_else(
                    || declaration1_up
                        .iter()
                        .find_map(
                            |declaration| elm_syntax_let_declaration_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_node_as_ref(declaration),
                                position,
                            ),
                        ),
                )
                .or_else(
                    // TODO add let-declaration-introduced bindings
                    || elm_syntax_expression_find_reference_at_position(module_origin_lookup, elm_syntax_node_unbox(result), position),
                ),
            ElmSyntaxExpression::List(elements) => elements
                .iter()
                .find_map(
                    |element| elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(element),
                        position,
                    ),
                ),
            ElmSyntaxExpression::Negation(in_negation) => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(in_negation),
                position,
            ),
            ElmSyntaxExpression::OperatorFunction(operator) => Some(ElmSyntaxNode {
                value: ElmSymbolOwned::VariableOrVariantOrOperator {
                    module_origin: look_up_origin_module(module_origin_lookup, "", operator).to_string(),
                    name: operator.clone(),
                },
                range: elm_syntax_expression_node.range,
            }),
            ElmSyntaxExpression::Parenthesized(in_parens) => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                position,
            ),
            ElmSyntaxExpression::Record(fields) => fields
                .iter()
                .find_map(
                    |field| elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&field.value),
                        position,
                    ),
                ),
            ElmSyntaxExpression::RecordAccess { record, field: _ } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(record),
                position,
            ),
            ElmSyntaxExpression::RecordAccessFunction(_) => None,
            ElmSyntaxExpression
            ::RecordUpdate {
                record_variable,
                bar_key_symbol_range: _,
                field0,
                field1_up,
            } => if lsp_range_includes_position(
                record_variable.range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: look_up_origin_module(
                            module_origin_lookup,
                            "",
                            &record_variable.value,
                        ).to_string(),
                        name: record_variable.value.clone(),
                    },
                    range: elm_syntax_expression_node.range,
                })
            } else {
                elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(&field0.value),
                    position,
                ).or_else(
                    || field1_up
                        .iter()
                        .find_map(
                            |field| elm_syntax_expression_find_reference_at_position(
                                module_origin_lookup,
                                elm_syntax_node_as_ref(&field.value),
                                position,
                            ),
                        ),
                )
            },
            ElmSyntaxExpression::Reference(reference) => Some(ElmSyntaxNode {
                value: ElmSymbolOwned::VariableOrVariantOrOperator {
                    module_origin: look_up_origin_module(
                        module_origin_lookup,
                        &reference.qualification,
                        &reference.name,
                    ).to_string(),
                    name: reference.name.clone(),
                },
                range: elm_syntax_expression_node.range,
            }),
            ElmSyntaxExpression::String { .. } => None,
            ElmSyntaxExpression::Triple { part0, part1, part2 } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                position,
            )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(part1),
                        position,
                    ),
                )
                .or_else(
                    || elm_syntax_expression_find_reference_at_position(
                        module_origin_lookup,
                        elm_syntax_node_unbox(part2),
                        position,
                    ),
                ),
            ElmSyntaxExpression::Tuple { part0, part1 } => elm_syntax_expression_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                position,
            ).or_else(
                || elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_unbox(part1),
                    position,
                ),
            ),
            ElmSyntaxExpression::Unit => None,
        }
    }
}

fn elm_syntax_let_declaration_find_reference_at_position(
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_let_declaration_node: ElmSyntaxNode<&ElmSyntaxLetDeclaration>,
    position: lsp_types::Position,
) -> Option<ElmSyntaxNode<ElmSymbolOwned>> {
    if !lsp_range_includes_position(elm_syntax_let_declaration_node.range, position) {
        None
    } else {
        match elm_syntax_let_declaration_node.value {
            ElmSyntaxLetDeclaration
            ::Destructuring {
                pattern,
                equals_key_symbol_range: _,
                expression,
            } => elm_syntax_pattern_find_reference_at_position(
                module_origin_lookup,
                elm_syntax_node_as_ref(pattern),
                position,
            ).or_else(
                || elm_syntax_expression_find_reference_at_position(
                    module_origin_lookup,
                    elm_syntax_node_as_ref(expression),
                    position,
                ),
            ),
            ElmSyntaxLetDeclaration
            ::ValueOrFunctionDeclaration {
                name,
                signature: maybe_signature,
                implementation_name_range,
                parameters,
                equals_key_symbol_range: _,
                result,
            } => if lsp_range_includes_position(
                *implementation_name_range,
                position,
            ) {
                Some(ElmSyntaxNode {
                    value: ElmSymbolOwned::VariableOrVariantOrOperator {
                        module_origin: "".to_string(),
                        name: name.clone(),
                    },
                    range: *implementation_name_range,
                })
            } else {
                maybe_signature
                    .as_ref()
                    .and_then(|signature| if lsp_range_includes_position(signature.name.range, position) {
                        Some(ElmSyntaxNode {
                            value: ElmSymbolOwned::VariableOrVariantOrOperator {
                                module_origin: "".to_string(),
                                name: name.clone(),
                            },
                            range: signature.name.range,
                        })
                    } else {
                        elm_syntax_type_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(&signature.type_1),
                            position,
                        )
                    })
                    .or_else(
                        || elm_syntax_expression_find_reference_at_position(
                            module_origin_lookup,
                            elm_syntax_node_as_ref(result),
                            position,
                        ),
                    )
                    .or_else(
                        || parameters
                            .iter()
                            .find_map(
                                |parameter| elm_syntax_pattern_find_reference_at_position(
                                    module_origin_lookup,
                                    elm_syntax_node_as_ref(parameter),
                                    position,
                                ),
                            ),
                    )
            },
        }
    }
}

// //
#[derive(Clone, Copy, PartialEq, Eq)]
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
}

fn elm_syntax_module_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    state: &State,
    elm_syntax_module: &ElmSyntaxModule,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    if symbol_to_collect_uses_of ==
        (ElmDeclaredSymbol::ModuleName(&elm_syntax_module.header.value.module_name.value)) {
        uses_so_far.push(elm_syntax_module.header.value.module_name.range);
    }
    match symbol_to_collect_uses_of {
        ElmDeclaredSymbol::ModuleName(module_name_to_collect_uses_of) if
            !elm_syntax_module
                .imports
                .iter()
                .any(|import| &import.value.module_name.value == module_name_to_collect_uses_of) => {
            // if not imported, that module name can never appear, so we can skip a bunch of
            // traversing! (unless implicitly imported, but those modules are never renamed!)
        },
        _ => {
            elm_syntax_exposing_uses_of_reference_into(
                uses_so_far,
                &elm_syntax_module.header.value.module_name.value,
                &elm_syntax_module.header.value.exposing.value,
                symbol_to_collect_uses_of,
            );
            for import in elm_syntax_module.imports.iter() {
                elm_syntax_import_uses_of_reference_into(uses_so_far, &import.value, symbol_to_collect_uses_of);
            }
            let module_origin_lookup: ModuleOriginLookup =
                elm_syntax_module_create_origin_lookup(state, elm_syntax_module);
            for declaration_node in elm_syntax_module.declarations.iter() {
                elm_syntax_declaration_uses_of_reference_into(
                    uses_so_far,
                    &elm_syntax_module.header.value.module_name.value,
                    &module_origin_lookup,
                    &declaration_node.declaration.value,
                    symbol_to_collect_uses_of,
                );
            }
        },
    }
}

fn elm_syntax_import_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    elm_syntax_import: &ElmSyntaxImport,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    if symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(&elm_syntax_import.module_name.value) {
        uses_so_far.push(elm_syntax_import.module_name.range);
    }
    match elm_syntax_import.alias {
        None => { },
        Some(elm::GeneratedAsKeywordRangeName { as_keyword_range: _, name: ref import_alias_name }) => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::ImportAlias {
                module_origin: &elm_syntax_import.module_name.value,
                alias_name: &import_alias_name.value,
            }) {
                uses_so_far.push(import_alias_name.range);
            }
        },
    }
    match elm_syntax_import.exposing {
        None => { },
        Some(ref exposing) => {
            elm_syntax_exposing_uses_of_reference_into(
                uses_so_far,
                &elm_syntax_import.module_name.value,
                &exposing.value,
                symbol_to_collect_uses_of,
            );
        },
    }
}

fn elm_syntax_exposing_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    origin_module: &str,
    elm_syntax_exposing: &ElmSyntaxExposing,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_exposing {
        ElmSyntaxExposing::All(_) => { },
        ElmSyntaxExposing::Explicit(exposes) => {
            for expose in exposes {
                match &expose.value {
                    ElmSyntaxExpose::ChoiceTypeIncludingVariants { name, open_range: _ } => {
                        if symbol_to_collect_uses_of == (ElmDeclaredSymbol::TypeNotRecordAlias {
                            name: &name.value,
                            module_origin: origin_module,
                        }) {
                            uses_so_far.push(name.range);
                        }
                    },
                    ElmSyntaxExpose::Operator(_) => { },
                    ElmSyntaxExpose::Type(name) => {
                        if (symbol_to_collect_uses_of == (ElmDeclaredSymbol::TypeNotRecordAlias {
                            name: name,
                            module_origin: origin_module,
                        })) || (symbol_to_collect_uses_of == (ElmDeclaredSymbol::RecordTypeAlias {
                            name: name,
                            module_origin: origin_module,
                        })) {
                            uses_so_far.push(expose.range);
                        }
                    },
                    ElmSyntaxExpose::Variable(name) => {
                        if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                            name: name,
                            module_origin: origin_module,
                        }) {
                            uses_so_far.push(expose.range);
                        }
                    },
                }
            }
        },
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
        ElmSyntaxDeclaration::ChoiceType { name, parameters, equals_key_symbol_range: _, variant0, variant1_up } => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::TypeNotRecordAlias {
                name: &name.value,
                module_origin: origin_module,
            }) {
                uses_so_far.push(name.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(&parameter_node.value) {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal
                }
            }
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: &variant0.name.value,
                module_origin: origin_module,
            }) {
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
                if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &variant.name.value,
                    module_origin: origin_module,
                }) {
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
        },
        ElmSyntaxDeclaration::Operator { .. } => { },
        ElmSyntaxDeclaration::Port { name, type_ } => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: &name.value,
                module_origin: origin_module,
            }) {
                uses_so_far.push(name.range);
            }
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(type_),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxDeclaration
        ::TypeAlias {
            alias_keyword_range: _,
            name,
            parameters,
            equals_key_symbol_range: _,
            type_,
        } => {
            if (symbol_to_collect_uses_of == (ElmDeclaredSymbol::TypeNotRecordAlias {
                name: &name.value,
                module_origin: origin_module,
            })) || (symbol_to_collect_uses_of == (ElmDeclaredSymbol::RecordTypeAlias {
                name: &name.value,
                module_origin: origin_module,
            })) {
                uses_so_far.push(name.range);
            }
            'parameter_traversal: for parameter_node in parameters {
                if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(&parameter_node.value) {
                    uses_so_far.push(parameter_node.range);
                    break 'parameter_traversal
                }
            }
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(type_),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxDeclaration
        ::ValueOrFunction {
            name,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range: _,
            result,
        } => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: &name,
                module_origin: origin_module,
            }) {
                uses_so_far.push(*implementation_name_range);
            }
            match maybe_signature {
                None => { },
                Some(signature) => {
                    if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                        name: &signature.name.value,
                        module_origin: origin_module,
                    }) {
                        uses_so_far.push(signature.name.range);
                    }
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&signature.type_1),
                        symbol_to_collect_uses_of,
                    );
                },
            }
            for parameter in parameters {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter),
                    symbol_to_collect_uses_of,
                );
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(result),
                symbol_to_collect_uses_of,
            );
        },
    }
}

fn elm_syntax_type_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_type_node: ElmSyntaxNode<&ElmSyntaxType>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_type_node.value {
        ElmSyntaxType::Construct { reference, arguments } => {
            let module_origin =
                look_up_origin_module(module_origin_lookup, &reference.value.qualification, &reference.value.name);
            if (symbol_to_collect_uses_of == (ElmDeclaredSymbol::TypeNotRecordAlias {
                module_origin: module_origin,
                name: &reference.value.name,
            })) || (symbol_to_collect_uses_of == (ElmDeclaredSymbol::RecordTypeAlias {
                module_origin: module_origin,
                name: &reference.value.name,
            })) {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(reference.range.end, -(reference.value.name.len() as i32)),
                    end: reference.range.end,
                });
            }
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::ImportAlias {
                module_origin: module_origin,
                alias_name: &reference.value.qualification,
            }) {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            } else if (symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(module_origin)) &&
                (&reference.value.qualification == module_origin) {
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
        },
        ElmSyntaxType::Function { input, arrow_key_symbol_range: _, output } => {
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
        },
        ElmSyntaxType::Parenthesized(in_parens) => {
            elm_syntax_type_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxType::Record(fields) => {
            for field in fields {
                elm_syntax_type_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxType::RecordExtension { record_variable, bar_key_symbol_range: _, field0, field1_up } => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(&record_variable.value) {
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
        },
        ElmSyntaxType::Triple { part0, part1, part2 } => {
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
        },
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
        },
        ElmSyntaxType::Unit => { },
        ElmSyntaxType::Variable(variable) => {
            if symbol_to_collect_uses_of == ElmDeclaredSymbol::TypeVariable(variable) {
                uses_so_far.push(elm_syntax_type_node.range);
            }
        },
    }
}

fn elm_syntax_expression_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    // TODO add local variables from pattern and lets
    elm_syntax_expression_node: ElmSyntaxNode<&ElmSyntaxExpression>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_expression_node.value {
        ElmSyntaxExpression::Call { called, argument0, argument1_up } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(called),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(argument0),
                symbol_to_collect_uses_of,
            );
            for argument_node in argument1_up {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(argument_node),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxExpression::CaseOf { matched, of_keyword_range: _, case0, case1_up } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(matched),
                symbol_to_collect_uses_of,
            );
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(&case0.pattern),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(&case0.result),
                symbol_to_collect_uses_of,
            );
            for case in case1_up {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&case.pattern),
                    symbol_to_collect_uses_of,
                );
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&case.result),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxExpression::Char(_) => { },
        ElmSyntaxExpression::Float(_) => { },
        ElmSyntaxExpression
        ::IfThenElse {
            condition,
            then_keyword_range: _,
            on_true,
            else_keyword_range: _,
            on_false,
        } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(condition),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(on_true),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(on_false),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::InfixOperation { left, operator: _, right } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(left),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(right),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::Integer { .. } => { },
        ElmSyntaxExpression::Lambda { parameter0, parameter1_up, arrow_key_symbol_range: _, result } => {
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
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(result),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::LetIn { declaration0, declaration1_up, in_keyword_range: _, result } => {
            elm_syntax_let_declaration_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                &declaration0.value,
                symbol_to_collect_uses_of,
            );
            for let_declaration_node in declaration1_up {
                elm_syntax_let_declaration_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    &let_declaration_node.value,
                    symbol_to_collect_uses_of,
                );
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(result),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::List(elements) => {
            for element_node in elements {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(element_node),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxExpression::Negation(in_negation) => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_negation),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::OperatorFunction(_) => { },
        ElmSyntaxExpression::Parenthesized(in_parens) => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::Record(fields) => {
            for field in fields {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxExpression::RecordAccess { record, field: _ } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(record),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::RecordAccessFunction(_) => { },
        ElmSyntaxExpression
        ::RecordUpdate {
            record_variable: record_variable_node,
            bar_key_symbol_range: _,
            field0,
            field1_up,
        } => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                module_origin: look_up_origin_module(module_origin_lookup, "", &record_variable_node.value),
                name: &record_variable_node.value,
            }) {
                uses_so_far.push(record_variable_node.range);
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(&field0.value),
                symbol_to_collect_uses_of,
            );
            for field in field1_up {
                elm_syntax_expression_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(&field.value),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxExpression::Reference(reference) => {
            let module_origin =
                look_up_origin_module(module_origin_lookup, &reference.qualification, &reference.name);
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                module_origin: module_origin,
                name: &reference.name,
            }) {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(
                        elm_syntax_expression_node.range.end,
                        -(reference.name.len() as i32),
                    ),
                    end: elm_syntax_expression_node.range.end,
                });
            } else if symbol_to_collect_uses_of == (ElmDeclaredSymbol::ImportAlias {
                module_origin: module_origin,
                alias_name: &reference.qualification,
            }) {
                uses_so_far.push(lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(
                        elm_syntax_expression_node.range.start,
                        reference.qualification.len() as i32,
                    ),
                });
            } else if (symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(module_origin)) &&
                (&reference.qualification == module_origin) {
                uses_so_far.push(lsp_types::Range {
                    start: elm_syntax_expression_node.range.start,
                    end: lsp_position_add_characters(
                        elm_syntax_expression_node.range.start,
                        reference.qualification.len() as i32,
                    ),
                });
            }
        },
        ElmSyntaxExpression::String { .. } => { },
        ElmSyntaxExpression::Triple { part0, part1, part2 } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part2),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::Tuple { part0, part1 } => {
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part0),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(part1),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxExpression::Unit => { },
    }
}

fn elm_syntax_let_declaration_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_let_declaration: &ElmSyntaxLetDeclaration,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match elm_syntax_let_declaration {
        ElmSyntaxLetDeclaration::Destructuring { pattern, equals_key_symbol_range: _, expression } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(pattern),
                symbol_to_collect_uses_of,
            );
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(expression),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxLetDeclaration
        ::ValueOrFunctionDeclaration {
            name,
            signature: maybe_signature,
            implementation_name_range,
            parameters,
            equals_key_symbol_range: _,
            result,
        } => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: name,
                module_origin: "",
            }) {
                uses_so_far.push(*implementation_name_range);
            }
            match maybe_signature {
                None => { },
                Some(signature) => {
                    if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                        name: &signature.name.value,
                        module_origin: "",
                    }) {
                        uses_so_far.push(signature.name.range);
                    }
                    elm_syntax_type_uses_of_reference_into(
                        uses_so_far,
                        module_origin_lookup,
                        elm_syntax_node_as_ref(&signature.type_1),
                        symbol_to_collect_uses_of,
                    );
                },
            }
            for parameter in parameters {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(parameter),
                    symbol_to_collect_uses_of,
                );
            }
            elm_syntax_expression_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_as_ref(result),
                symbol_to_collect_uses_of,
            );
        },
    }
}

fn elm_syntax_pattern_uses_of_reference_into(
    uses_so_far: &mut Vec<lsp_types::Range>,
    module_origin_lookup: &ModuleOriginLookup,
    elm_syntax_pattern_node: ElmSyntaxNode<&ElmSyntaxPattern>,
    symbol_to_collect_uses_of: ElmDeclaredSymbol,
) {
    match &elm_syntax_pattern_node.value {
        ElmSyntaxPattern::As { pattern: alias_pattern, as_keyword_range: _, variable } => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(alias_pattern),
                symbol_to_collect_uses_of,
            );
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: &variable.value,
                module_origin: "",
            }) {
                uses_so_far.push(variable.range);
            }
        },
        ElmSyntaxPattern::Char(_) => { },
        ElmSyntaxPattern::Ignored => { },
        ElmSyntaxPattern::Int { .. } => { },
        ElmSyntaxPattern::ListCons { head, cons_key_symbol: _, tail } => {
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
        },
        ElmSyntaxPattern::ListExact(elements) => {
            for element in elements {
                elm_syntax_pattern_uses_of_reference_into(
                    uses_so_far,
                    module_origin_lookup,
                    elm_syntax_node_as_ref(element),
                    symbol_to_collect_uses_of,
                );
            }
        },
        ElmSyntaxPattern::Parenthesized(in_parens) => {
            elm_syntax_pattern_uses_of_reference_into(
                uses_so_far,
                module_origin_lookup,
                elm_syntax_node_unbox(in_parens),
                symbol_to_collect_uses_of,
            );
        },
        ElmSyntaxPattern::Record(field_names) => {
            for field_name_node in field_names {
                if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                    name: &field_name_node.value,
                    module_origin: "",
                }) {
                    uses_so_far.push(field_name_node.range);
                }
            }
        },
        ElmSyntaxPattern::String { .. } => { },
        ElmSyntaxPattern::Triple { part0, part1, part2 } => {
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
        },
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
        },
        ElmSyntaxPattern::Unit => { },
        ElmSyntaxPattern::Variable(variable) => {
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                name: variable,
                module_origin: "",
            }) {
                uses_so_far.push(elm_syntax_pattern_node.range);
            }
        },
        ElmSyntaxPattern::Variant { reference, values } => {
            let module_origin =
                look_up_origin_module(module_origin_lookup, &reference.value.qualification, &reference.value.name);
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::VariableOrVariant {
                module_origin: module_origin,
                name: &reference.value.name,
            }) {
                uses_so_far.push(lsp_types::Range {
                    start: lsp_position_add_characters(reference.range.end, -(reference.value.name.len() as i32)),
                    end: reference.range.end,
                });
            }
            if symbol_to_collect_uses_of == (ElmDeclaredSymbol::ImportAlias {
                module_origin: module_origin,
                alias_name: &reference.value.qualification,
            }) {
                uses_so_far.push(lsp_types::Range {
                    start: reference.range.start,
                    end: lsp_position_add_characters(
                        reference.range.start,
                        reference.value.qualification.len() as i32,
                    ),
                });
            } else if (symbol_to_collect_uses_of == ElmDeclaredSymbol::ModuleName(module_origin)) &&
                (&reference.value.qualification == module_origin) {
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
        },
    }
}
