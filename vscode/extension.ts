import * as vscode from "vscode";
import {
  LanguageClientOptions,
} from "vscode-languageclient";
import {
  LanguageClient,
  ServerOptions,
} from "vscode-languageclient/node";
import * as child_process from "node:child_process";

let client: LanguageClient | null = null;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const languageServerExecutableName: string =
    // when debugging, switch out for
    // "/???/elm-language-server-rs/target/debug/elm-language-server-rs"
    "elm-language-server-rs";
  context.subscriptions.push(vscode.commands.registerCommand("elm.commands.restart", async () => {
    if (client !== null) {
      await client.stop();
      await client.start();
    }
  }));

  const serverOptions: ServerOptions = async () => {
    return child_process.spawn(languageServerExecutableName)
  };
  const clientOptions: LanguageClientOptions = {
    diagnosticCollectionName: "elm",
    documentSelector: [
      {
        scheme: "file",
        language: "elm",
      },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.elm"),
    },
    initializationOptions: getSettings(vscode.workspace.getConfiguration().get<IClientSettings>("elm-language-server-rs")),
  };
  client = new LanguageClient(
    "elm-language-server-rs",
    "elm",
    serverOptions,
    clientOptions,
  );
  await client.start();
}
function getSettings(config: IClientSettings | undefined): object {
  return config
    ? {
      elmPath: config.elmPath,
      elmFormatPath: config.elmFormatPath,
      elmTestPath: config.elmTestPath,
    }
    : {};
}
export interface IClientSettings {
  elmFormatPath: string;
  elmPath: string;
  elmTestPath: string;
}

export function deactivate(): Thenable<void> | undefined {
  if (client !== null) {
    return client.stop()
  }
  return undefined;
}
