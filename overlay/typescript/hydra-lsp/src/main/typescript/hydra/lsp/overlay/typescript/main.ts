// Hydra language server entry point (#836): attaches the transport-independent core in server.ts to
// vscode-languageserver over stdio. Kept deliberately thin; run as `node main.js --stdio`.

import {
  createConnection,
  ProposedFeatures,
} from "vscode-languageserver/node.js";
import { exitCode, handlers, initializeResult, newState } from "./server.js";

const connection = createConnection(ProposedFeatures.all);
const state = newState();

connection.onInitialize(() => initializeResult() as any);

for (const [method, handler] of handlers) {
  if (method === "shutdown") {
    connection.onShutdown(() => { handler(state, undefined); });
  } else if (method === "textDocument/formatting") {
    connection.onRequest(method, (params: unknown) => handler(state, params).result);
  } else {
    connection.onNotification(method, (params: unknown) => {
      for (const n of handler(state, params).notifications) connection.sendNotification(n.method, n.params);
    });
  }
}

connection.onExit(() => process.exit(exitCode(state)));
connection.listen();
