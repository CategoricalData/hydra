// Hydra VS Code extension host (#806).
//
// Scope today is intentionally minimal: syntax highlighting is delivered by the
// TextMate grammar (syntaxes/hydra.tmLanguage.json) and editing behavior by
// language-configuration.json — neither needs this host. This file adds only the
// Unicode input mechanism (#806 scope item 3): a Lean/Agda-style abbreviation,
// where typing a backslash-prefixed name (e.g. `\lambda`, `\to`) and accepting the
// completion inserts the corresponding Unicode operator (λ, →).
//
// Language intelligence (diagnostics, go-to-definition, hover, completion of names
// in scope) is explicitly OUT OF SCOPE here and belongs to the Hydra language
// server (#808). This activation path is where that server client would later be
// started; it is kept deliberately small so #808 can bolt on without restructuring.

import * as vscode from 'vscode';

// The abbreviation table. Keys are typed WITHOUT the leading backslash; the trigger
// character ('\\') is handled by the completion provider. Kept in sync with the
// snippet prefixes in snippets/hydra.json and with syntax.md §2.3's operator table.
const ABBREVIATIONS: ReadonlyArray<[string, string, string]> = [
  // name,        unicode, human description
  ['lambda', 'λ', 'term lambda binder'],
  ['l', 'λ', 'term lambda binder (short)'],
  ['Lambda', 'Λ', 'type-lambda / System F binder'],
  ['L', 'Λ', 'type-lambda binder (short)'],
  ['forall', '∀', 'type-scheme binder'],
  ['all', '∀', 'type-scheme binder (short)'],
  ['to', '→', 'function / kind arrow'],
  ['r', '→', 'function arrow (short)'],
  ['Rightarrow', '⇒', 'type-scheme constraint arrow'],
  ['R', '⇒', 'constraint arrow (short)'],
  ['langle', '⟨', 'type application, open'],
  ['<', '⟨', 'type application, open (short)'],
  ['rangle', '⟩', 'type application, close'],
  ['>', '⟩', 'type application, close (short)'],
  ['neg', '¬', 'boolean negation (RESERVED #811)'],
];

export function activate(context: vscode.ExtensionContext): void {
  const provider = vscode.languages.registerCompletionItemProvider(
    { language: 'hydra' },
    {
      provideCompletionItems(document, position) {
        // Find a backslash before the cursor on the current line; offer the
        // abbreviations whose name extends what the user has typed after it.
        const line = document.lineAt(position.line).text;
        const prefix = line.slice(0, position.character);
        const match = /\\([A-Za-z<>]*)$/.exec(prefix);
        if (!match) {
          return undefined;
        }
        const typed = match[1];
        const start = position.translate(0, -(typed.length + 1)); // include the backslash
        const replaceRange = new vscode.Range(start, position);

        const items: vscode.CompletionItem[] = [];
        for (const [name, unicode, description] of ABBREVIATIONS) {
          if (name.startsWith(typed) || typed.length === 0) {
            const item = new vscode.CompletionItem(
              '\\' + name,
              vscode.CompletionItemKind.Text,
            );
            item.detail = `${unicode}  —  ${description}`;
            item.insertText = unicode;
            item.filterText = '\\' + name;
            item.range = replaceRange;
            // Sort exact single-glyph shortcuts first.
            item.sortText = name.length <= 2 ? '0' + name : '1' + name;
            items.push(item);
          }
        }
        return items;
      },
    },
    '\\', // trigger completion as soon as a backslash is typed
  );

  context.subscriptions.push(provider);
}

export function deactivate(): void {
  // Nothing to tear down yet. The #808 language-server client would be stopped here.
}
