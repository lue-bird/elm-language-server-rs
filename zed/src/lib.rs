struct ElmLanguageServerRsZedExtension();

impl zed_extension_api::Extension for ElmLanguageServerRsZedExtension {
    fn new() -> Self {
        ElmLanguageServerRsZedExtension()
    }
    fn language_server_command(
        &mut self,
        _: &zed_extension_api::LanguageServerId,
        worktree: &zed_extension_api::Worktree,
    ) -> zed_extension_api::Result<zed_extension_api::Command> {
        if let Some(path) = worktree.which("elm-language-server-rs") {
            Ok(zed_extension_api::Command::new(path))
        } else {
            Err("executable elm-language-server-rs not found in the PATH environment".into())
        }
    }
}

zed_extension_api::register_extension!(ElmLanguageServerRsZedExtension);
