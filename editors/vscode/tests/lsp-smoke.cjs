// Scripted LSP smoke test for the bundled Hydra language server (#836): node tests/lsp-smoke.cjs [out/server.cjs]
const { spawn } = require("child_process");
const p = spawn("node", [process.argv[2] || "out/server.cjs", "--stdio"]);
let buf = Buffer.alloc(0); const got = [];
p.stdout.on("data", d => { buf = Buffer.concat([buf, d]);
  for (;;) { const i = buf.indexOf("\r\n\r\n"); if (i < 0) break;
    const m = /Content-Length: (\d+)/.exec(buf.slice(0, i).toString()); const n = +m[1];
    if (buf.length < i + 4 + n) break;
    got.push(JSON.parse(buf.slice(i + 4, i + 4 + n).toString())); buf = buf.slice(i + 4 + n); } });
const send = o => { const s = JSON.stringify(o); p.stdin.write(`Content-Length: ${Buffer.byteLength(s)}\r\n\r\n${s}`); };
const uri = "file:///t.hydra";
const text = "## doc\nmodule test.m\n\nimport hydra.core.packaging\n\n## seg\ntype T = union{\n\n  ## a\n  x: string}\n";
send({jsonrpc:"2.0",id:1,method:"initialize",params:{processId:null,rootUri:null,capabilities:{}}});
send({jsonrpc:"2.0",method:"initialized",params:{}});
send({jsonrpc:"2.0",method:"textDocument/didOpen",params:{textDocument:{uri,languageId:"hydra",version:1,text}}});
send({jsonrpc:"2.0",id:2,method:"textDocument/formatting",params:{textDocument:{uri},options:{tabSize:2,insertSpaces:true}}});
send({jsonrpc:"2.0",id:3,method:"shutdown"});
send({jsonrpc:"2.0",method:"exit"});
// Assertions: lifecycle per spec, advertised capabilities exactly full-sync + formatting, a formatting edit, clean exit.
const timer = setTimeout(() => { console.error("FAIL: timeout"); p.kill(); process.exit(1); }, 10000);
p.on("exit", code => {
  clearTimeout(timer);
  const byId = id => got.find(g => g.id === id);
  const ok = [
    ["exit code 0", code === 0],
    ["capabilities", JSON.stringify(byId(1)?.result?.capabilities) === '{"textDocumentSync":1,"documentFormattingProvider":true}'],
    ["didOpen publishes diagnostics", got.some(g => g.method === "textDocument/publishDiagnostics")],
    ["formatting returns one whole-document edit", byId(2)?.result?.length === 1 && byId(2).result[0].range.start.line === 0],
    ["shutdown result null", byId(3)?.result === null],
  ];
  for (const [name, pass] of ok) console.log((pass ? "ok   " : "FAIL ") + name);
  process.exit(ok.every(([, pass]) => pass) ? 0 : 1);
});
